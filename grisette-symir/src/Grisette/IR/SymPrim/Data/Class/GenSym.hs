{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.Class.GenSym
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.Class.GenSym
  ( chooseFresh,
    chooseSimpleFresh,
    chooseUnionFresh,
    choose,
    chooseSimple,
    chooseUnion,
  )
where

import Data.Proxy
import GHC.Generics
import Grisette.Core.Data.Class.GenSym
import Grisette.IR.SymPrim.Control.Monad.Union
import Grisette.IR.SymPrim.Data.Class.Mergeable
import Grisette.IR.SymPrim.Data.Class.SimpleMergeable
import Grisette.IR.SymPrim.Data.SymPrim

-- | 'gchooseFresh' specialized with 'SymBool'
chooseFresh :: (Mergeable a, MonadFresh m, MonadUnion u) => [a] -> m (u a)
chooseFresh = gchooseFresh
{-# INLINE chooseFresh #-}

-- | 'gchoose' specialized with 'SymBool'
choose :: (Mergeable a, MonadUnion u) => [a] -> FreshIdent -> u a
choose = gchoose
{-# INLINE choose #-}

-- | 'gchooseSimpleFresh' specialized with 'SymBool'
chooseSimpleFresh :: (SimpleMergeable a, MonadFresh m) => [a] -> m a
chooseSimpleFresh = gchooseSimpleFresh (Proxy @SymBool)
{-# INLINE chooseSimpleFresh #-}

-- | 'gchooseSimple' specialized with 'SymBool'
chooseSimple :: (SimpleMergeable a) => [a] -> FreshIdent -> a
chooseSimple = gchooseSimple (Proxy @SymBool)
{-# INLINE chooseSimple #-}

-- | 'gchooseUnionFresh' specialized with 'SymBool'
chooseUnionFresh :: (Mergeable a, MonadFresh m, MonadUnion u) => [u a] -> m (u a)
chooseUnionFresh = gchooseUnionFresh
{-# INLINE chooseUnionFresh #-}

-- | 'gchooseUnion' specialized with 'SymBool'
chooseUnion :: (Mergeable a, MonadUnion u) => [u a] -> FreshIdent -> u a
chooseUnion = gchooseUnion
{-# INLINE chooseUnion #-}
