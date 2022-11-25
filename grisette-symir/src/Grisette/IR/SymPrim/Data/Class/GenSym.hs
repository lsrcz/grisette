{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.IR.SymPrim.Data.Class.GenSym
  ( GenSym,
    fresh,
    genSym,
    chooseFresh,
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

type GenSym spec a = GGenSym SymBool spec a

fresh :: (GenSym spec a, MonadFresh m, MonadUnion u) => spec -> m (u a)
fresh = gfresh
{-# INLINE fresh #-}

genSym :: (GenSym spec a, MonadUnion u) => spec -> FreshIdent -> u a
genSym = ggenSym
{-# INLINE genSym #-}

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
