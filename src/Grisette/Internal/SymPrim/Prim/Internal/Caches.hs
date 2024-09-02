{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
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
    -- shallowCollectGarbageTermCache,
    -- setupPeriodicTermCacheGC,
    haveCache,
    threadCacheSize,
    dumpThreadCache,
    threadCacheLiveSize,
    Digest,
  )
where

import Control.Concurrent
  ( MVar,
    ThreadId,
    mkWeakThreadId,
    myThreadId,
    newMVar,
    putMVar,
    takeMVar,
  )
import Control.Monad (replicateM)
import qualified Data.Array as A
import Data.Atomics (atomicModifyIORefCAS, atomicModifyIORefCAS_)
import Data.Data (Proxy (Proxy), TypeRep, Typeable, typeRep)
import qualified Data.HashMap.Strict as M
import qualified Data.HashTable.IO as HT
import Data.Hashable (Hashable)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import Data.Word (Word32)
import GHC.Base (Any)
import GHC.IO (unsafePerformIO)
import GHC.Weak (Weak, deRefWeak)
import Grisette.Internal.SymPrim.Prim.Internal.Utils
  ( WeakThreadId,
    WeakThreadIdRef,
    myWeakThreadId,
    weakThreadId,
  )
import System.Mem.Weak (addFinalizer, mkWeakPtr)
import Unsafe.Coerce (unsafeCoerce)

type Id = Word32

type Digest = Word32

newtype Cache t
  = Cache {getCache :: A.Array Int (CacheState t)}

type HashTable k v = HT.BasicHashTable k v

data CacheState t where
  CacheState ::
    (Interned t) =>
    { nextId :: IORef Id,
      sem :: MVar (),
      currentThread :: HashTable (Description t) (Id, Weak t),
      otherThread :: HT.BasicHashTable (WeakThreadId, Id) t
    } ->
    CacheState t

cacheStateSize :: CacheState t -> IO Int
cacheStateSize (CacheState _ _ s _) = length <$> HT.toList s

cacheStateLiveSize :: CacheState t -> IO Int
cacheStateLiveSize (CacheState _ sem s _) = do
  takeMVar sem
  v <- fmap snd <$> HT.toList s
  r <-
    sum
      <$> mapM
        ( \(_, x) -> do
            x <- deRefWeak x
            if isJust x then return 1 else return 0
        )
        v
  putMVar sem ()
  return r

dumpCacheState :: CacheState t -> IO ()
dumpCacheState (CacheState _ sem s _) = do
  takeMVar sem
  v <- HT.toList s
  mapM_
    ( \(k, (i, v)) -> do
        v1 <- deRefWeak v
        case v1 of
          Nothing -> print (k, i, "dead")
          Just r -> print (k, i, r)
    )
    v
  putMVar sem ()

dumpCache :: Cache t -> IO ()
dumpCache (Cache a) = mapM_ dumpCacheState (A.elems a)

cacheSize :: Cache t -> IO Int
cacheSize (Cache a) = sum <$> mapM cacheStateSize (A.elems a)

cacheLiveSize :: Cache t -> IO Int
cacheLiveSize (Cache a) = sum <$> mapM cacheStateLiveSize (A.elems a)

class
  ( Show (Description t),
    Hashable (Description t),
    Typeable t,
    Show t
  ) =>
  Interned t
  where
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

threadCacheLiveSize :: WeakThreadId -> IO Int
threadCacheLiveSize tid = do
  caches <- readIORef termCacheCell
  case M.lookup tid caches of
    Just (_, cref) -> do
      cache <- readIORef cref
      sum <$> mapM cacheLiveSize (M.elems cache)
    Nothing -> return 0

dumpThreadCache :: WeakThreadId -> IO ()
dumpThreadCache tid = do
  caches <- readIORef termCacheCell
  case M.lookup tid caches of
    Just (_, cref) -> do
      cache <- readIORef cref
      mapM_ dumpCache (M.elems cache)
    Nothing -> return ()

{-
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
  M.traverseWithKey
    ( \k _ -> do
        s <- threadCacheSize k
        sl <- threadCacheLiveSize k
        s1 <- atomically $ threadCacheGCSetSize k
        putStr $ show (k, s, sl, s1)
    )
    cache
  putStrLn ""
  print finishedOrDied
  -- atomicModifyIORefCAS_ termCacheCell $ \m -> foldr M.delete m finishedOrDied
  performMajorGC

setupPeriodicTermCacheGC :: Int -> IO ()
setupPeriodicTermCacheGC n = do
  _ <- forkIO $ forever $ do
    threadDelay n
    shallowCollectGarbageTermCache
  return ()
  -}

cacheWidth :: Word32
cacheWidth = 10
{-# INLINE cacheWidth #-}

mkCache :: forall t. (Interned t) => IO (Cache t)
mkCache = result
  where
    element =
      CacheState
        <$> newIORef 0
        <*> newMVar ()
        <*> HT.new
        <*> HT.new
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
      addFinalizer tid $ atomicModifyIORefCAS_ termCacheCell $ M.delete wtid
      r <- newIORef $ M.singleton (typeRep (Proxy @a)) (unsafeCoerce r1)
      atomicModifyIORefCAS termCacheCell $
        \m -> (M.insert wtid (wtidRef, r) m, r1)

haveCache :: IO Bool
haveCache = do
  caches <- readIORef termCacheCell
  tid <- myWeakThreadId
  return $ M.member tid caches

reclaimTerm ::
  forall t. (Interned t) => WeakThreadId -> Int -> Description t -> IO ()
reclaimTerm id grp dt = do
  caches <- readIORef termCacheCell
  case M.lookup id caches of
    Just (_, cref) -> do
      cache <- readIORef cref
      case M.lookup (typeRep (Proxy @t)) cache of
        Just c -> do
          let Cache a = unsafeCoerce c :: Cache t
          let CacheState _ sem s _ = a A.! grp
          takeMVar sem
          result <- HT.lookup s dt
          case result of
            Nothing -> return ()
            Just (_, wr) -> do
              t <- deRefWeak wr
              case t of
                Nothing -> HT.delete s dt
                Just _ -> return ()
          putMVar sem ()
        Nothing -> return ()
    Nothing -> return ()

intern :: forall t. (Interned t, Typeable t) => Uninterned t -> IO t
intern !bt = do
  tid <- myThreadId
  let wtid = weakThreadId tid
  cache <- typeMemoizedCache tid
  let !dt = describe bt :: Description t
      !hdt = descriptionDigest dt
      !r = hdt `mod` cacheWidth
      CacheState nextBaseId sem s _ = getCache cache A.! (fromIntegral r)
  takeMVar sem
  HT.lookup s dt >>= \case
    Nothing -> do
      i <- readIORef nextBaseId
      writeIORef nextBaseId (i + 1)
      let !newId = cacheWidth * i + r
          !t = identify (weakThreadId tid) hdt newId bt
      weakRef <- mkWeakPtr t (Just $ reclaimTerm wtid (fromIntegral r) dt)
      HT.insert s dt (newId, weakRef)
      putMVar sem ()
      return t
    Just (oldId, t) -> do
      t1 <- deRefWeak t
      case t1 of
        Nothing -> do
          let !term = identify (weakThreadId tid) hdt oldId bt
          weakRef <-
            mkWeakPtr term (Just $ reclaimTerm wtid (fromIntegral r) dt)
          HT.insert s dt (oldId, weakRef)
          putMVar sem ()
          return term
        Just t1 -> do
          putMVar sem ()
          return t1
{-# NOINLINE intern #-}
