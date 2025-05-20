{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Unified.UnifiedData
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Unified.UnifiedData
  ( UnifiedDataBase,
    GetData,
    BaseMonad,
    wrapData,
    extractData,
    UnifiedData,
    AllUnifiedData,
    symCompare,
    liftSymCompare,
    symCompare1,
    liftSymCompare2,
    symCompare2,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad.Identity (Identity (Identity, runIdentity))
import Data.Bytes.Serial (Serial)
import Data.Functor.Classes (Ord1 (liftCompare), Ord2 (liftCompare2), compare1, compare2)
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Grisette.Internal.Core.Data.Class.AsKey (KeyEq, KeyHashable)
import Grisette.Internal.Core.Data.Class.ITEOp (ITEOp)
import Grisette.Internal.Core.Data.Class.LogicalOp (LogicalOp)
import Grisette.Internal.Core.Data.Class.UnionView (UnionView)
import Grisette.Internal.Internal.Decl.Core.Control.Monad.Union
  ( Union,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.EvalSym
  ( EvalSym,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.ExtractSym
  ( ExtractSym,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.Mergeable
  ( Mergeable,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.PPrint
  ( PPrint,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.SubstSym
  ( SubstSym,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.SymEq
  ( SymEq,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.SymOrd
  ( SymOrd,
  )
import qualified Grisette.Internal.Internal.Decl.Core.Data.Class.SymOrd as SymOrd
import Grisette.Internal.Internal.Decl.Core.Data.Class.ToCon
  ( ToCon,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.ToSym
  ( ToSym,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.TryMerge
  ( TryMerge,
    mrgSingle,
  )
import Grisette.Internal.Internal.Decl.SymPrim.AllSyms (AllSyms)
import Grisette.Internal.Internal.Decl.Unified.Class.UnifiedITEOp
  ( UnifiedITEOp,
  )
import Grisette.Internal.Internal.Decl.Unified.Class.UnifiedSimpleMergeable
  ( UnifiedBranching (withBaseBranching),
    UnifiedSimpleMergeable,
    UnifiedSimpleMergeable1,
  )
import Grisette.Internal.Internal.Decl.Unified.Class.UnifiedSymEq
  ( UnifiedSymEq,
  )
import Grisette.Internal.Internal.Decl.Unified.Class.UnifiedSymOrd
  ( UnifiedSymOrd (withBaseSymOrd),
    UnifiedSymOrd1 (withBaseSymOrd1),
    UnifiedSymOrd2 (withBaseSymOrd2),
  )
import Grisette.Internal.Internal.Impl.Unified.Class.UnifiedITEOp ()
import Grisette.Internal.Internal.Impl.Unified.Class.UnifiedSimpleMergeable
  ( liftUnion,
  )
import Grisette.Internal.Internal.Impl.Unified.Class.UnifiedSymEq ()
import Grisette.Internal.Internal.Impl.Unified.Class.UnifiedSymOrd ()
import Grisette.Internal.Unified.Class.UnionViewMode (UnionViewMode)
import Grisette.Internal.Unified.EvalModeTag (EvalModeTag (C, S))
import Grisette.Internal.Unified.Util (DecideEvalMode, withMode)
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift)

-- | A type family that specifies the base monad for the evaluation mode.
--
-- Resolves to 'Identity' for `C` mode, and 'Union' for `S` mode.
type BaseMonad mode = GetData mode

class
  ( UnifiedSimpleMergeable1 mode (GetData mode),
    UnifiedBranching mode (GetData mode),
    UnionView (GetData mode),
    UnionViewMode mode (GetData mode),
    Monad (GetData mode),
    TryMerge (GetData mode)
  ) =>
  UnifiedDataBase mode
  where
  -- | Get a unified data type. Resolves to 'Identity' in 'C' mode, and 'Union'
  -- in 'S' mode.
  type GetData mode = (r :: Type -> Type) | r -> mode

class
  ( u ~ GetData mode v,
    (Mergeable v) => Mergeable u,
    (AllSyms v) => AllSyms u,
    (Eq v) => Eq u,
    (EvalSym v) => EvalSym u,
    (ExtractSym v) => ExtractSym u,
    (ITEOp v, Mergeable v) => ITEOp u,
    (PPrint v) => PPrint u,
    (Eq v) => KeyEq u,
    (Eq v, Hashable v) => KeyHashable u,
    (Lift v) => Lift u,
    (LogicalOp v, Mergeable v) => LogicalOp u,
    (NFData v) => NFData u,
    (Num v, Mergeable v) => Num u,
    (SymEq v) => SymEq u,
    (Show v) => Show u,
    (SymOrd v) => SymOrd u,
    (SubstSym v) => SubstSym u,
    (Serial v, Mergeable v) => Serial u,
    (UnifiedITEOp mode v, Mergeable v) => UnifiedITEOp mode u,
    (Mergeable v) => UnifiedSimpleMergeable mode u,
    (UnifiedSymEq mode v) => UnifiedSymEq mode u,
    (UnifiedSymOrd mode v) => UnifiedSymOrd mode u,
    forall a. (ToSym a v) => ToSym (Identity a) u,
    forall a. (ToSym v a) => ToSym u (Union a),
    forall a. (ToCon v a) => ToCon u (Identity a),
    forall a. (ToCon a v) => ToCon (Union a) u,
    UnifiedDataBase mode
  ) =>
  UnifiedDataImpl (mode :: EvalModeTag) v u
    | u -> mode v
  where
  -- type GetData mode = (r :: Type -> Type) | r -> mode

  -- | Wraps a value into the unified data type.
  wrapData :: (Mergeable v) => v -> u

  -- | Extracts a value from the unified data type.
  extractData :: (Mergeable v, Monad m, UnifiedBranching mode m) => u -> m v

instance UnifiedDataBase 'C where
  type GetData 'C = Identity

instance UnifiedDataImpl 'C v (Identity v) where
  wrapData = Identity
  extractData ::
    forall m. (Mergeable v, Monad m, UnifiedBranching C m) => Identity v -> m v
  extractData = withBaseBranching @'C @m $ return . runIdentity

instance UnifiedDataBase 'S where
  type GetData 'S = Union

instance UnifiedDataImpl 'S v (Union v) where
  wrapData = mrgSingle
  extractData ::
    forall m. (Mergeable v, Monad m, UnifiedBranching S m) => Union v -> m v
  extractData = liftUnion

-- | This class is needed as constraint in user code prior to GHC 9.2.1.
-- See the notes in 'Grisette.Internal.Unified.IsMode.IsMode'.
class (UnifiedDataImpl mode v (GetData mode v)) => UnifiedData mode v

instance (UnifiedDataImpl bool v (GetData bool v)) => UnifiedData bool v

class
  (UnifiedSimpleMergeable 'S (GetData 'S v)) =>
  UnifiedDataSimpleMergeable v

instance (Mergeable v) => UnifiedDataSimpleMergeable v

-- | Evaluation mode with unified data types.
class
  ( forall v. UnifiedData bool v,
    forall v. (Mergeable v) => UnifiedDataSimpleMergeable v
  ) =>
  AllUnifiedData bool

instance
  ( forall v. UnifiedData bool v,
    forall v. (Mergeable v) => UnifiedDataSimpleMergeable v
  ) =>
  AllUnifiedData bool

-- TODO: temporary instance for dependencies

-- | Unified `Grisette.Internal.Core.Data.Class.SymOrd.symCompare`.
symCompare ::
  forall mode a.
  (DecideEvalMode mode, UnifiedSymOrd mode a) =>
  a ->
  a ->
  GetData mode Ordering
symCompare x y =
  withMode @mode
    (withBaseSymOrd @mode @a $ return $ compare x y)
    ( withBaseSymOrd @mode @a $
        SymOrd.symCompare x y
    )
{-# INLINE symCompare #-}

-- | Unified `Grisette.Internal.Core.Data.Class.SymOrd.liftSymCompare`.
liftSymCompare ::
  forall mode f a b.
  (DecideEvalMode mode, UnifiedSymOrd1 mode f) =>
  (a -> b -> GetData mode Ordering) ->
  f a ->
  f b ->
  GetData mode Ordering
liftSymCompare f a b =
  withMode @mode
    ( withBaseSymOrd1 @mode @f $
        return $
          liftCompare (\x y -> runIdentity $ f x y) a b
    )
    ( withBaseSymOrd1 @mode @f $
        SymOrd.liftSymCompare f a b
    )
{-# INLINE liftSymCompare #-}

-- | Unified `Grisette.Internal.Core.Data.Class.SymOrd.symCompare1`.
symCompare1 ::
  forall mode f a.
  (DecideEvalMode mode, UnifiedSymOrd mode a, UnifiedSymOrd1 mode f) =>
  f a ->
  f a ->
  GetData mode Ordering
symCompare1 a b =
  withMode @mode
    (withBaseSymOrd1 @mode @f $ withBaseSymOrd @mode @a $ return $ compare1 a b)
    ( withBaseSymOrd1 @mode @f $
        withBaseSymOrd @mode @a $
          SymOrd.symCompare1 a b
    )
{-# INLINE symCompare1 #-}

-- | Unified `Grisette.Internal.Core.Data.Class.SymOrd.liftSymCompare2`.
liftSymCompare2 ::
  forall mode f a b c d.
  (DecideEvalMode mode, UnifiedSymOrd2 mode f) =>
  (a -> b -> GetData mode Ordering) ->
  (c -> d -> GetData mode Ordering) ->
  f a c ->
  f b d ->
  GetData mode Ordering
liftSymCompare2 f g a b =
  withMode @mode
    ( withBaseSymOrd2 @mode @f $
        return $
          liftCompare2
            (\x y -> runIdentity $ f x y)
            (\x y -> runIdentity $ g x y)
            a
            b
    )
    ( withBaseSymOrd2 @mode @f $
        SymOrd.liftSymCompare2 f g a b
    )
{-# INLINE liftSymCompare2 #-}

-- | Unified `Grisette.Internal.Core.Data.Class.SymOrd.symCompare2`.
symCompare2 ::
  forall mode f a b.
  ( DecideEvalMode mode,
    UnifiedSymOrd mode a,
    UnifiedSymOrd mode b,
    UnifiedSymOrd2 mode f
  ) =>
  f a b ->
  f a b ->
  GetData mode Ordering
symCompare2 a b =
  withMode @mode
    ( withBaseSymOrd2 @mode @f $
        withBaseSymOrd @mode @a $
          withBaseSymOrd @mode @b $
            return $
              compare2 a b
    )
    ( withBaseSymOrd2 @mode @f $
        withBaseSymOrd @mode @a $
          withBaseSymOrd @mode @b $
            SymOrd.symCompare2 a b
    )
{-# INLINE symCompare2 #-}
