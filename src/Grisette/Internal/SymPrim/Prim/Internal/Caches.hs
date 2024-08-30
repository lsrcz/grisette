{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-cse #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Prim.Internal.Caches
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.Prim.Internal.Caches
  ( Cache (..),
    CacheState (..),
    Id,
    Interned (..),
    typeMemoizedCache,
    intern,
    shallowCollectGarbageTermCache,
    setupPeriodicTermCacheGC,
    haveCache,
    threadCacheSize,
    Digest,
  )
where

import Control.Concurrent
  ( ThreadId,
    forkIO,
    mkWeakThreadId,
    myThreadId,
    threadDelay,
  )
import Control.Monad (filterM, forever, replicateM)
import qualified Data.Array as A
import Data.Atomics (atomicModifyIORefCAS, atomicModifyIORefCAS_)
import Data.Data (Proxy (Proxy), TypeRep, Typeable, typeRep)
import qualified Data.HashMap.Strict as M
import qualified Data.HashTable.IO as HT
import Data.Hashable (Hashable)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Vector.Hashtables as VHT
import qualified Data.Vector.Mutable as BV
import Data.Word (Word32)
import GHC.Base (Any)
import GHC.IO (unsafePerformIO)
import Grisette.Internal.SymPrim.Prim.Internal.Utils
  ( WeakThreadId,
    WeakThreadIdRef,
    myWeakThreadId,
    weakThreadId,
    weakThreadRefAlive,
  )
import Unsafe.Coerce (unsafeCoerce)

type Id = Word32

type Digest = Word32

newtype Cache t
  = Cache {getCache :: A.Array Int (CacheState t)}

type HashTable k v = VHT.Dictionary (VHT.PrimState IO) BV.MVector k BV.MVector v

data CacheState t
  = CacheState
  { currentThread :: HashTable (Description t) t,
    otherThread :: HT.BasicHashTable (WeakThreadId, Id) t
  }

cacheStateSize :: CacheState t -> IO Int
cacheStateSize (CacheState s _) = VHT.size s

cacheSize :: Cache t -> IO Int
cacheSize (Cache a) = sum <$> mapM cacheStateSize (A.elems a)

class (Hashable (Description t)) => Interned t where
  data Description t
  type Uninterned t
  describe :: Uninterned t -> Description t
  identify :: WeakThreadId -> Digest -> Id -> Uninterned t -> t
  threadId :: t -> WeakThreadId
  descriptionDigest :: Description t -> Digest

{-# NOINLINE termCacheCell #-}
termCacheCell ::
  IORef
    ( M.HashMap
        WeakThreadId
        ( WeakThreadIdRef,
          IORef (M.HashMap TypeRep (Cache Any))
        )
    )
termCacheCell = unsafePerformIO $ newIORef M.empty

threadCacheSize :: WeakThreadId -> IO Int
threadCacheSize tid = do
  caches <- readIORef termCacheCell
  case M.lookup tid caches of
    Just (_, cref) -> do
      cache <- readIORef cref
      sum <$> mapM cacheSize (M.elems cache)
    Nothing -> return 0

shallowCollectGarbageTermCache :: IO ()
shallowCollectGarbageTermCache = do
  cache <- readIORef termCacheCell
  finishedOrDied <-
    fmap (fmap fst)
      $ filterM
        ( \(_, (v, _)) ->
            not <$> weakThreadRefAlive v
        )
      $ M.toList cache
  -- M.traverseWithKey
  --   ( \k _ -> do
  --       s <- threadCacheSize k
  --       putStr $ show (k, s)
  --   )
  --   cache
  -- putStrLn ""
  atomicModifyIORefCAS_ termCacheCell $ \m -> foldr M.delete m finishedOrDied

setupPeriodicTermCacheGC :: Int -> IO ()
setupPeriodicTermCacheGC n = do
  _ <- forkIO $ forever $ do
    threadDelay n
    shallowCollectGarbageTermCache
  return ()

cacheWidth :: Word32
cacheWidth = 10
{-# INLINE cacheWidth #-}

mkCache :: forall t. (Interned t) => IO (Cache t)
mkCache = result
  where
    element = CacheState <$> VHT.initialize 0 <*> HT.new
    result = do
      elements <- replicateM (fromIntegral cacheWidth) element
      return $ Cache $ A.listArray (0, fromIntegral cacheWidth - 1) elements

-- | Internal cache for memoization of term construction. Different types have
-- different caches and they may share names, ids, or representations, but they
-- are not the same term.
typeMemoizedCache ::
  forall a. (Interned a, Typeable a) => ThreadId -> IO (Cache a)
typeMemoizedCache tid = do
  caches <- readIORef termCacheCell
  let wtid = weakThreadId tid
  case M.lookup wtid caches of
    Just (_, cref) -> do
      cache <- readIORef cref
      case M.lookup (typeRep (Proxy @a)) cache of
        Just d -> return $ unsafeCoerce d
        Nothing -> do
          r1 <- mkCache
          writeIORef cref $!
            M.insert (typeRep (Proxy @a)) (unsafeCoerce r1) cache
          return r1
    Nothing -> do
      r1 <- mkCache
      wtidRef <- mkWeakThreadId tid
      r <- newIORef $ M.singleton (typeRep (Proxy @a)) (unsafeCoerce r1)
      atomicModifyIORefCAS termCacheCell $
        \m -> (M.insert wtid (wtidRef, r) m, r1)

haveCache :: IO Bool
haveCache = do
  caches <- readIORef termCacheCell
  tid <- myWeakThreadId
  return $ M.member tid caches

intern :: forall t. (Interned t, Typeable t) => Uninterned t -> IO t
intern !bt = do
  tid <- myThreadId
  cache <- typeMemoizedCache tid
  let !dt = describe bt :: Description t
      !hdt = descriptionDigest dt
      !r = hdt `mod` cacheWidth
      CacheState s _ = getCache cache A.! (fromIntegral r)
  VHT.lookup s dt >>= \case
    Nothing -> do
      i <- VHT.size s
      let !newId = cacheWidth * fromIntegral i + r
          !t = identify (weakThreadId tid) hdt newId bt
      VHT.insert s dt t
      return t
    Just t -> return t
{-# NOINLINE intern #-}
