{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-cse #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.Prim.InternedTerm.Caches
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.Prim.InternedTerm.Caches (typeMemoizedCache) where

import Control.Concurrent
import Data.Data
import qualified Data.HashMap.Strict as M
import Data.IORef
import Data.Interned
import GHC.Base (Any)
import GHC.IO
import Unsafe.Coerce

mkOnceIO :: IO a -> IO (IO a)
mkOnceIO io = do
  mv <- newEmptyMVar
  demand <- newEmptyMVar
  forkIO (takeMVar demand >> io >>= putMVar mv)
  return (tryPutMVar demand () >> readMVar mv)

termCacheCell :: IO (IORef (M.HashMap TypeRep Any))
termCacheCell = unsafePerformIO $ mkOnceIO $ newIORef M.empty
{-# NOINLINE termCacheCell #-}

typeMemoizedCache :: forall a. (Interned a, Typeable a) => Cache a
typeMemoizedCache = unsafeDupablePerformIO $ do
  c <- termCacheCell
  atomicModifyIORef' c $ \m ->
    case M.lookup (typeRep (Proxy @a)) m of
      Just d -> (m, unsafeCoerce d)
      Nothing -> (M.insert (typeRep (Proxy @a)) (unsafeCoerce r1) m, r1)
        where
          r1 :: Cache a
          !r1 = mkCache
          {-# NOINLINE r1 #-}
