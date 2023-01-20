{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.Prim.InternedTerm.Caches
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.Prim.InternedTerm.Caches (typeMemoizedCache) where

import Control.Once
import Data.Data
import qualified Data.HashMap.Strict as M
import Data.IORef
import Data.Interned
import GHC.Base (Any)
import GHC.IO
import Unsafe.Coerce

termCacheCell :: IO (IORef (M.HashMap TypeRep Any))
termCacheCell = unsafeDupablePerformIO $ once $ newIORef M.empty

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
