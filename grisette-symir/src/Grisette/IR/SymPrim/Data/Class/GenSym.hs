{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.IR.SymPrim.Data.Class.GenSym
  ( GenSym,
    genSymFresh,
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

genSymFresh :: (GenSym spec a, MonadGenSymFresh m, MonadUnion u) => spec -> m (u a)
genSymFresh = ggenSymFresh
{-# INLINE genSymFresh #-}

genSym :: (GenSym spec a, MonadUnion u) => spec -> GenSymIdent -> u a
genSym = ggenSym
{-# INLINE genSym #-}

chooseFresh :: (Mergeable a, MonadGenSymFresh m, MonadUnion u) => [a] -> m (u a)
chooseFresh = gchooseFresh
{-# INLINE chooseFresh #-}

choose :: (Mergeable a, MonadUnion u) => [a] -> GenSymIdent -> u a
choose = gchoose
{-# INLINE choose #-}

chooseSimpleFresh :: (SimpleMergeable a, MonadGenSymFresh m) => [a] -> m a
chooseSimpleFresh = gchooseSimpleFresh (Proxy @SymBool)
{-# INLINE chooseSimpleFresh #-}

chooseSimple :: (SimpleMergeable a) => [a] -> GenSymIdent -> a
chooseSimple = gchooseSimple (Proxy @SymBool)
{-# INLINE chooseSimple #-}

chooseUnionFresh :: (Mergeable a, MonadGenSymFresh m, MonadUnion u) => [u a] -> m (u a)
chooseUnionFresh = gchooseUnionFresh
{-# INLINE chooseUnionFresh #-}

chooseUnion :: (Mergeable a, MonadUnion u) => [u a] -> GenSymIdent -> u a
chooseUnion = gchooseUnion
{-# INLINE chooseUnion #-}
