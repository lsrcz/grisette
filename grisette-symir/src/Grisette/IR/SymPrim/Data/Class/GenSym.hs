{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

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

chooseFresh :: (Mergeable a, MonadFresh m, MonadUnion u) => [a] -> m (u a)
chooseFresh = gchooseFresh
{-# INLINE chooseFresh #-}

choose :: (Mergeable a, MonadUnion u) => [a] -> FreshIdent -> u a
choose = gchoose
{-# INLINE choose #-}

chooseSimpleFresh :: (SimpleMergeable a, MonadFresh m) => [a] -> m a
chooseSimpleFresh = gchooseSimpleFresh (Proxy @SymBool)
{-# INLINE chooseSimpleFresh #-}

chooseSimple :: (SimpleMergeable a) => [a] -> FreshIdent -> a
chooseSimple = gchooseSimple (Proxy @SymBool)
{-# INLINE chooseSimple #-}

chooseUnionFresh :: (Mergeable a, MonadFresh m, MonadUnion u) => [u a] -> m (u a)
chooseUnionFresh = gchooseUnionFresh
{-# INLINE chooseUnionFresh #-}

chooseUnion :: (Mergeable a, MonadUnion u) => [u a] -> FreshIdent -> u a
chooseUnion = gchooseUnion
{-# INLINE chooseUnion #-}
