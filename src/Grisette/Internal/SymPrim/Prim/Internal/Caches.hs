{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  ( WeakThreadId,
    myWeakThreadId,
    Cache (..),
    CacheState (..),
    Id,
    Interned (..),
    typeMemoizedCache,
    intern,
    shallowCollectGarbageTermCache,
    setupPeriodicTermCacheGC,
    haveCache,
  )
where

#if MIN_VERSION_base(4, 19, 0)
import GHC.Conc.Sync (fromThreadId)
#endif

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
import qualified Data.HashTable.ST.Basic as HTST
import Data.Hashable (Hashable (hash, hashWithSalt))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word64)
import GHC.Base (Any)
import GHC.Conc
  ( ThreadStatus (ThreadDied, ThreadFinished),
    threadStatus,
  )
import GHC.IO (stToIO, unsafePerformIO)
import GHC.Weak (Weak, deRefWeak)
import Unsafe.Coerce (unsafeCoerce)

type Id = Int

#if MIN_VERSION_base(4, 19, 0)
data WeakThreadId = WeakThreadId Word64 (Weak ThreadId)
myWeakThreadId :: IO WeakThreadId
myWeakThreadId = do
  tid <- myThreadId
  wk <- mkWeakThreadId tid
  return $ WeakThreadId (fromThreadId tid) wk
#else
data WeakThreadId = WeakThreadId String (Weak ThreadId)
myWeakThreadId :: IO WeakThreadId
myWeakThreadId = do
  tid <- myThreadId
  wk <- mkWeakThreadId tid
  return $ WeakThreadId (show tid) wk
#endif

weakThreadAlive :: WeakThreadId -> IO Bool
weakThreadAlive (WeakThreadId _ wtid) = do
  tid <- deRefWeak wtid
  case tid of
    Nothing -> return False
    Just tid -> do
      st <- threadStatus tid
      return $ st `notElem` [ThreadFinished, ThreadDied]

instance Show WeakThreadId where
  show (WeakThreadId s _) = show s

instance Eq WeakThreadId where
  (WeakThreadId s1 _) == (WeakThreadId s2 _) = s1 == s2

instance Hashable WeakThreadId where
  hashWithSalt s (WeakThreadId i _) = s `hashWithSalt` i

data Cache t
  = Cache {cacheTid :: WeakThreadId, getCache :: A.Array Int (CacheState t)}

data CacheState t
  = CacheState
  { currentThread :: HT.BasicHashTable (Description t) t,
    otherThread :: HT.BasicHashTable (WeakThreadId, Id) t
  }

class (Hashable (Description t)) => Interned t where
  data Description t
  type Uninterned t
  describe :: Uninterned t -> Description t
  identify :: WeakThreadId -> Int -> Id -> Uninterned t -> t
  threadId :: t -> WeakThreadId
  -- cacheWidth :: p t -> Int

{-# NOINLINE termCacheCell #-}
termCacheCell :: IORef (M.HashMap WeakThreadId (IORef (M.HashMap TypeRep Any)))
termCacheCell = unsafePerformIO $ newIORef M.empty

shallowCollectGarbageTermCache :: IO ()
shallowCollectGarbageTermCache = do
  cache <- readIORef termCacheCell
  finishedOrDied <- filterM (fmap not . weakThreadAlive) $ M.keys cache
  atomicModifyIORefCAS_ termCacheCell $ \m -> foldr M.delete m finishedOrDied

setupPeriodicTermCacheGC :: Int -> IO ()
setupPeriodicTermCacheGC n = do
  _ <- forkIO $ forever $ do
    threadDelay n
    shallowCollectGarbageTermCache
  return ()

cacheWidth :: Int
cacheWidth = 100
{-# INLINE cacheWidth #-}

mkCache :: forall t. (Interned t) => WeakThreadId -> IO (Cache t)
mkCache tid = result
  where
    element = CacheState <$> HT.new <*> HT.new
    result = do
      elements <- replicateM cacheWidth element
      return $ Cache tid $ A.listArray (0, cacheWidth - 1) elements

-- | Internal cache for memoization of term construction. Different types have
-- different caches and they may share names, ids, or representations, but they
-- are not the same term.
typeMemoizedCache :: 
 forall a. (Interned a, Typeable a) => WeakThreadId -> IO (Cache a)
typeMemoizedCache tid = do
  caches <- readIORef termCacheCell
  case M.lookup tid caches of
    Just cref -> do
      cache <- readIORef cref
      case M.lookup (typeRep (Proxy @a)) cache of
        Just d -> return $ unsafeCoerce d
        Nothing -> do
          r1 <- mkCache tid
          writeIORef cref $!
            M.insert (typeRep (Proxy @a)) (unsafeCoerce r1) cache
          return r1
    Nothing -> do
      r1 <- mkCache tid
      r <- newIORef $ M.singleton (typeRep (Proxy @a)) (unsafeCoerce r1)
      atomicModifyIORefCAS termCacheCell $ \m -> (M.insert tid r m, r1)

haveCache :: IO Bool
haveCache = do
  caches <- readIORef termCacheCell
  tid <- myWeakThreadId
  return $ M.member tid caches

intern :: forall t. (Interned t, Typeable t) => Uninterned t -> IO t
intern !bt = do
  tid <- myWeakThreadId
  cache <- typeMemoizedCache tid
  let !dt = describe bt :: Description t
      !hdt = hash dt
      !r = hdt `mod` cacheWidth
      CacheState s _ = getCache cache A.! r
  HT.lookup s dt >>= \case
    Nothing -> do
      i <- stToIO $ HTST.size s
      let !newId = cacheWidth * i + r
          !t = identify tid hdt newId bt
      HT.insert s dt t
      return t
    Just t -> return t
{-# NOINLINE intern #-}
