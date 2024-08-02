{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.SimpleMergeable
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.SimpleMergeable
  ( -- * Simple mergeable types
    SimpleMergeable (..),
    SimpleMergeable1 (..),
    mrgIte1,
    SimpleMergeable2 (..),
    mrgIte2,

    -- * Generic 'SimpleMergeable'
    SimpleMergeableArgs (..),
    GSimpleMergeable (..),
    genericMrgIte,
    genericLiftMrgIte,

    -- * Symbolic branching
    SymBranching (..),
    mrgIf,
    mergeWithStrategy,
    merge,
  )
where

import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Identity
  ( Identity (Identity),
    IdentityT (IdentityT),
  )
import qualified Control.Monad.RWS.Lazy as RWSLazy
import qualified Control.Monad.RWS.Strict as RWSStrict
import Control.Monad.Reader (ReaderT (ReaderT))
import qualified Control.Monad.State.Lazy as StateLazy
import qualified Control.Monad.State.Strict as StateStrict
import Control.Monad.Trans.Cont (ContT (ContT))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import Data.Functor.Compose (Compose (Compose))
import Data.Functor.Const (Const)
import Data.Functor.Product (Product)
import Data.Kind (Type)
import Data.Monoid (Alt, Ap, Endo (Endo))
import qualified Data.Monoid as Monoid
import Data.Ord (Down)
import GHC.Generics
  ( Generic (Rep, from, to),
    Generic1 (Rep1, from1, to1),
    K1 (K1),
    M1 (M1),
    Par1 (Par1),
    Rec1 (Rec1),
    U1,
    V1,
    (:.:) (Comp1),
    type (:*:) ((:*:)),
  )
import GHC.TypeNats (KnownNat, type (<=))
import Generics.Deriving (Default (Default), Default1 (Default1))
import Grisette.Internal.Core.Control.Exception (AssertionError)
import Grisette.Internal.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Internal.Core.Data.Class.Mergeable
  ( GMergeable,
    Mergeable (rootStrategy),
    Mergeable1 (liftRootStrategy),
    Mergeable2 (liftRootStrategy2),
    Mergeable3 (liftRootStrategy3),
    MergingStrategy (SimpleStrategy),
  )
import Grisette.Internal.Core.Data.Class.TryMerge
  ( TryMerge (tryMergeWithStrategy),
  )
import Grisette.Internal.SymPrim.BV (BitwidthMismatch)
import Grisette.Internal.SymPrim.FP (BitCastNaNError, ValidFP)
import Grisette.Internal.SymPrim.GeneralFun (type (-->))
import Grisette.Internal.SymPrim.Prim.Term
  ( LinkedRep,
    SupportedPrim,
  )
import Grisette.Internal.SymPrim.SymAlgReal (SymAlgReal)
import Grisette.Internal.SymPrim.SymBV
  ( SymIntN,
    SymWordN,
  )
import Grisette.Internal.SymPrim.SymBool (SymBool)
import Grisette.Internal.SymPrim.SymFP (SymFP, SymFPRoundingMode)
import Grisette.Internal.SymPrim.SymGeneralFun (type (-~>))
import Grisette.Internal.SymPrim.SymInteger (SymInteger)
import Grisette.Internal.SymPrim.SymTabularFun (type (=~>))
import Grisette.Internal.SymPrim.TabularFun (type (=->))
import Grisette.Internal.TH.DeriveBuiltin (deriveBuiltins)
import Grisette.Internal.TH.DeriveInstanceProvider
  ( Strategy (ViaDefault, ViaDefault1),
  )
import Grisette.Internal.Utils.Derive (Arity0, Arity1)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
-- >>> import Control.Monad.Identity

-- | This class indicates that a type has a simple root merge strategy.
--
-- __Note:__ This type class can be derived for algebraic data types.
-- You may need the @DerivingVia@ and @DerivingStrategies@ extensions.
--
-- > data X = ...
-- >   deriving Generic
-- >   deriving (Mergeable, SimpleMergeable) via (Default X)
class (Mergeable a) => SimpleMergeable a where
  -- | Performs if-then-else with the simple root merge strategy.
  --
  -- >>> mrgIte "a" "b" "c" :: SymInteger
  -- (ite a b c)
  mrgIte :: SymBool -> a -> a -> a

-- | Lifting of the 'SimpleMergeable' class to unary type constructors.
class
  (Mergeable1 u, forall a. (SimpleMergeable a) => (SimpleMergeable (u a))) =>
  SimpleMergeable1 u
  where
  -- | Lift 'mrgIte' through the type constructor.
  --
  -- >>> liftMrgIte mrgIte "a" (Identity "b") (Identity "c") :: Identity SymInteger
  -- Identity (ite a b c)
  liftMrgIte :: (SymBool -> a -> a -> a) -> SymBool -> u a -> u a -> u a

-- | Lift the standard 'mrgIte' function through the type constructor.
--
-- >>> mrgIte1 "a" (Identity "b") (Identity "c") :: Identity SymInteger
-- Identity (ite a b c)
mrgIte1 ::
  (SimpleMergeable1 u, SimpleMergeable a) => SymBool -> u a -> u a -> u a
mrgIte1 = liftMrgIte mrgIte
{-# INLINE mrgIte1 #-}

-- | Lifting of the 'SimpleMergeable' class to binary type constructors.
class
  (Mergeable2 u, forall a. (SimpleMergeable a) => SimpleMergeable1 (u a)) =>
  SimpleMergeable2 u
  where
  -- | Lift 'mrgIte' through the type constructor.
  --
  -- >>> liftMrgIte2 mrgIte mrgIte "a" ("b", "c") ("d", "e") :: (SymInteger, SymBool)
  -- ((ite a b d),(ite a c e))
  liftMrgIte2 ::
    (SymBool -> a -> a -> a) ->
    (SymBool -> b -> b -> b) ->
    SymBool ->
    u a b ->
    u a b ->
    u a b

-- | Lift the standard 'mrgIte' function through the type constructor.
--
-- >>> mrgIte2 "a" ("b", "c") ("d", "e") :: (SymInteger, SymBool)
-- ((ite a b d),(ite a c e))
mrgIte2 ::
  (SimpleMergeable2 u, SimpleMergeable a, SimpleMergeable b) =>
  SymBool ->
  u a b ->
  u a b ->
  u a b
mrgIte2 = liftMrgIte2 mrgIte mrgIte
{-# INLINE mrgIte2 #-}

-- | The arguments to the generic simple merging function.
data family SimpleMergeableArgs arity a :: Type

data instance SimpleMergeableArgs Arity0 _ = SimpleMergeableArgs0

newtype instance SimpleMergeableArgs Arity1 a
  = SimpleMergeableArgs1 (SymBool -> a -> a -> a)

-- | Generic 'SimpleMergeable' class.
class GSimpleMergeable arity f where
  gmrgIte :: SimpleMergeableArgs arity a -> SymBool -> f a -> f a -> f a

instance GSimpleMergeable arity V1 where
  gmrgIte _ _ t _ = t
  {-# INLINE gmrgIte #-}

instance (GSimpleMergeable arity U1) where
  gmrgIte _ _ t _ = t
  {-# INLINE gmrgIte #-}

instance
  (GSimpleMergeable arity a, GSimpleMergeable arity b) =>
  (GSimpleMergeable arity (a :*: b))
  where
  gmrgIte args cond (a1 :*: a2) (b1 :*: b2) =
    gmrgIte args cond a1 b1 :*: gmrgIte args cond a2 b2
  {-# INLINE gmrgIte #-}

instance (GSimpleMergeable arity a) => (GSimpleMergeable arity (M1 i c a)) where
  gmrgIte args cond (M1 a) (M1 b) = M1 $ gmrgIte args cond a b
  {-# INLINE gmrgIte #-}

instance (SimpleMergeable c) => (GSimpleMergeable arity (K1 i c)) where
  gmrgIte _ cond (K1 a) (K1 b) = K1 $ mrgIte cond a b
  {-# INLINE gmrgIte #-}

instance GSimpleMergeable Arity1 Par1 where
  gmrgIte (SimpleMergeableArgs1 f) cond (Par1 l) (Par1 r) = Par1 $ f cond l r
  {-# INLINE gmrgIte #-}

instance (SimpleMergeable1 f) => GSimpleMergeable Arity1 (Rec1 f) where
  gmrgIte (SimpleMergeableArgs1 f) cond (Rec1 l) (Rec1 r) =
    Rec1 $ liftMrgIte f cond l r
  {-# INLINE gmrgIte #-}

instance
  (SimpleMergeable1 f, GSimpleMergeable Arity1 g) =>
  GSimpleMergeable Arity1 (f :.: g)
  where
  gmrgIte targs cond (Comp1 l) (Comp1 r) =
    Comp1 $ liftMrgIte (gmrgIte targs) cond l r
  {-# INLINE gmrgIte #-}

instance
  (Generic a, GSimpleMergeable Arity0 (Rep a), GMergeable Arity0 (Rep a)) =>
  SimpleMergeable (Default a)
  where
  mrgIte cond (Default a) (Default b) =
    Default $ genericMrgIte cond a b
  {-# INLINE mrgIte #-}

-- | Generic 'mrgIte' function.
genericMrgIte ::
  (Generic a, GSimpleMergeable Arity0 (Rep a)) =>
  SymBool ->
  a ->
  a ->
  a
genericMrgIte cond a b =
  to $ gmrgIte SimpleMergeableArgs0 cond (from a) (from b)
{-# INLINE genericMrgIte #-}

instance
  ( Generic1 f,
    GSimpleMergeable Arity1 (Rep1 f),
    GMergeable Arity1 (Rep1 f),
    SimpleMergeable a
  ) =>
  SimpleMergeable (Default1 f a)
  where
  mrgIte = mrgIte1
  {-# INLINE mrgIte #-}

instance
  (Generic1 f, GSimpleMergeable Arity1 (Rep1 f), GMergeable Arity1 (Rep1 f)) =>
  SimpleMergeable1 (Default1 f)
  where
  liftMrgIte f c (Default1 l) (Default1 r) =
    Default1 $ genericLiftMrgIte f c l r
  {-# INLINE liftMrgIte #-}

-- | Generic 'liftMrgIte' function.
genericLiftMrgIte ::
  (Generic1 f, GSimpleMergeable Arity1 (Rep1 f)) =>
  (SymBool -> a -> a -> a) ->
  SymBool ->
  f a ->
  f a ->
  f a
genericLiftMrgIte f c l r =
  to1 $ gmrgIte (SimpleMergeableArgs1 f) c (from1 l) (from1 r)
{-# INLINE genericLiftMrgIte #-}

-- | Special case of the 'Mergeable1' and 'SimpleMergeable1' class for type
-- constructors that are 'SimpleMergeable' when applied to any 'Mergeable'
-- types.
--
-- This type class is used to generalize the 'mrgIf' function to other
-- containers, for example, monad transformer transformed Unions.
class
  ( SimpleMergeable1 u,
    forall a. (Mergeable a) => SimpleMergeable (u a),
    TryMerge u
  ) =>
  SymBranching (u :: Type -> Type)
  where
  -- | Symbolic @if@ control flow with the result merged with some merge
  -- strategy.
  --
  -- >>> mrgIfWithStrategy rootStrategy "a" (mrgSingle "b") (return "c") :: Union SymInteger
  -- {(ite a b c)}
  --
  -- __Note:__ Be careful to call this directly in your code.
  -- The supplied merge strategy should be consistent with the type's root merge
  -- strategy, or some internal invariants would be broken and the program can
  -- crash.
  --
  -- This function is to be called when the 'Mergeable' constraint can not be
  -- resolved, e.g., the merge strategy for the contained type is given with
  -- 'Mergeable1'. In other cases, 'mrgIf' is usually a better alternative.
  mrgIfWithStrategy :: MergingStrategy a -> SymBool -> u a -> u a -> u a

  -- | Symbolic @if@ control flow with the result.
  --
  -- This function does not need a merging strategy, and it will merge the
  -- result only if any of the branches is merged.
  mrgIfPropagatedStrategy :: SymBool -> u a -> u a -> u a

-- | Try to merge the container with a given merge strategy.
mergeWithStrategy :: (SymBranching m) => MergingStrategy a -> m a -> m a
mergeWithStrategy = tryMergeWithStrategy
{-# INLINE mergeWithStrategy #-}

-- | Try to merge the container with the root strategy.
merge :: (SymBranching m, Mergeable a) => m a -> m a
merge = mergeWithStrategy rootStrategy
{-# INLINE merge #-}

-- | Symbolic @if@ control flow with the result merged with the type's root
-- merge strategy.
--
-- Equivalent to @'mrgIfWithStrategy' 'rootStrategy'@.
--
-- >>> mrgIf "a" (return "b") (return "c") :: Union SymInteger
-- {(ite a b c)}
mrgIf :: (SymBranching u, Mergeable a) => SymBool -> u a -> u a -> u a
mrgIf = mrgIfWithStrategy rootStrategy
{-# INLINE mrgIf #-}

deriveBuiltins
  (ViaDefault ''SimpleMergeable)
  [''SimpleMergeable]
  [ ''(),
    ''(,),
    ''(,,),
    ''(,,,),
    ''(,,,,),
    ''(,,,,,),
    ''(,,,,,,),
    ''(,,,,,,,),
    ''(,,,,,,,,),
    ''(,,,,,,,,,),
    ''(,,,,,,,,,,),
    ''(,,,,,,,,,,,),
    ''(,,,,,,,,,,,,),
    ''(,,,,,,,,,,,,,),
    ''(,,,,,,,,,,,,,,),
    ''AssertionError,
    ''BitwidthMismatch,
    ''BitCastNaNError,
    ''Identity,
    ''Monoid.Dual,
    ''Monoid.Sum,
    ''Monoid.Product,
    ''Down
  ]

deriveBuiltins
  (ViaDefault1 ''SimpleMergeable1)
  [''SimpleMergeable, ''SimpleMergeable1]
  [ ''(,),
    ''(,,),
    ''(,,,),
    ''(,,,,),
    ''(,,,,,),
    ''(,,,,,,),
    ''(,,,,,,,),
    ''(,,,,,,,,),
    ''(,,,,,,,,,),
    ''(,,,,,,,,,,),
    ''(,,,,,,,,,,,),
    ''(,,,,,,,,,,,,),
    ''(,,,,,,,,,,,,,),
    ''(,,,,,,,,,,,,,,),
    ''Identity,
    ''Monoid.Dual,
    ''Monoid.Sum,
    ''Monoid.Product,
    ''Down
  ]

instance SimpleMergeable2 (,) where
  liftMrgIte2 ma mb cond (a1, b1) (a2, b2) = (ma cond a1 a2, mb cond b1 b2)
  {-# INLINE liftMrgIte2 #-}

instance (SimpleMergeable a) => SimpleMergeable2 ((,,) a) where
  liftMrgIte2 mb mc cond (a1, b1, c1) (a2, b2, c2) =
    (mrgIte cond a1 a2, mb cond b1 b2, mc cond c1 c2)
  {-# INLINE liftMrgIte2 #-}

instance
  (SimpleMergeable a, SimpleMergeable b) =>
  SimpleMergeable2 ((,,,) a b)
  where
  liftMrgIte2 mc md cond (a1, b1, c1, d1) (a2, b2, c2, d2) =
    (mrgIte cond a1 a2, mrgIte cond b1 b2, mc cond c1 c2, md cond d1 d2)
  {-# INLINE liftMrgIte2 #-}

instance (SimpleMergeable b) => SimpleMergeable (a -> b) where
  mrgIte = mrgIte1
  {-# INLINE mrgIte #-}

instance SimpleMergeable1 ((->) a) where
  liftMrgIte ms cond t f v = ms cond (t v) (f v)
  {-# INLINE liftMrgIte #-}

instance SimpleMergeable2 (->) where
  liftMrgIte2 _ ms cond t f v = ms cond (t v) (f v)
  {-# INLINE liftMrgIte2 #-}

-- MaybeT
instance (SymBranching m, Mergeable a) => SimpleMergeable (MaybeT m a) where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance (SymBranching m) => SimpleMergeable1 (MaybeT m) where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance (SymBranching m) => SymBranching (MaybeT m) where
  mrgIfWithStrategy strategy cond (MaybeT l) (MaybeT r) =
    MaybeT $ mrgIfWithStrategy (liftRootStrategy strategy) cond l r
  {-# INLINE mrgIfWithStrategy #-}
  mrgIfPropagatedStrategy cond (MaybeT l) (MaybeT r) =
    MaybeT $ mrgIfPropagatedStrategy cond l r
  {-# INLINE mrgIfPropagatedStrategy #-}

-- ExceptT
instance
  (SymBranching m, Mergeable e, Mergeable a) =>
  SimpleMergeable (ExceptT e m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (SymBranching m, Mergeable e) =>
  SimpleMergeable1 (ExceptT e m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (SymBranching m, Mergeable e) =>
  SymBranching (ExceptT e m)
  where
  mrgIfWithStrategy s cond (ExceptT t) (ExceptT f) =
    ExceptT $ mrgIfWithStrategy (liftRootStrategy s) cond t f
  {-# INLINE mrgIfWithStrategy #-}
  mrgIfPropagatedStrategy cond (ExceptT t) (ExceptT f) =
    ExceptT $ mrgIfPropagatedStrategy cond t f
  {-# INLINE mrgIfPropagatedStrategy #-}

-- StateT
instance
  (Mergeable s, Mergeable a, SymBranching m) =>
  SimpleMergeable (StateLazy.StateT s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (Mergeable s, SymBranching m) =>
  SimpleMergeable1 (StateLazy.StateT s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (Mergeable s, SymBranching m) =>
  SymBranching (StateLazy.StateT s m)
  where
  mrgIfWithStrategy s cond (StateLazy.StateT t) (StateLazy.StateT f) =
    StateLazy.StateT $ \v ->
      mrgIfWithStrategy
        (liftRootStrategy2 s rootStrategy)
        cond
        (t v)
        (f v)
  {-# INLINE mrgIfWithStrategy #-}
  mrgIfPropagatedStrategy cond (StateLazy.StateT t) (StateLazy.StateT f) =
    StateLazy.StateT $ \v -> mrgIfPropagatedStrategy cond (t v) (f v)
  {-# INLINE mrgIfPropagatedStrategy #-}

instance
  (Mergeable s, Mergeable a, SymBranching m) =>
  SimpleMergeable (StateStrict.StateT s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (Mergeable s, SymBranching m) =>
  SimpleMergeable1 (StateStrict.StateT s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (Mergeable s, SymBranching m) =>
  SymBranching (StateStrict.StateT s m)
  where
  mrgIfWithStrategy s cond (StateStrict.StateT t) (StateStrict.StateT f) =
    StateStrict.StateT $
      \v ->
        mrgIfWithStrategy (liftRootStrategy2 s rootStrategy) cond (t v) (f v)
  {-# INLINE mrgIfWithStrategy #-}
  mrgIfPropagatedStrategy cond (StateStrict.StateT t) (StateStrict.StateT f) =
    StateStrict.StateT $ \v -> mrgIfPropagatedStrategy cond (t v) (f v)
  {-# INLINE mrgIfPropagatedStrategy #-}

-- WriterT
instance
  (Mergeable s, Mergeable a, SymBranching m, Monoid s) =>
  SimpleMergeable (WriterLazy.WriterT s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (Mergeable s, SymBranching m, Monoid s) =>
  SimpleMergeable1 (WriterLazy.WriterT s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (Mergeable s, SymBranching m, Monoid s) =>
  SymBranching (WriterLazy.WriterT s m)
  where
  mrgIfWithStrategy s cond (WriterLazy.WriterT t) (WriterLazy.WriterT f) =
    WriterLazy.WriterT $
      mrgIfWithStrategy (liftRootStrategy2 s rootStrategy) cond t f
  {-# INLINE mrgIfWithStrategy #-}
  mrgIfPropagatedStrategy cond (WriterLazy.WriterT t) (WriterLazy.WriterT f) =
    WriterLazy.WriterT $ mrgIfPropagatedStrategy cond t f
  {-# INLINE mrgIfPropagatedStrategy #-}

instance
  (Mergeable s, Mergeable a, SymBranching m, Monoid s) =>
  SimpleMergeable (WriterStrict.WriterT s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (Mergeable s, SymBranching m, Monoid s) =>
  SimpleMergeable1 (WriterStrict.WriterT s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (Mergeable s, SymBranching m, Monoid s) =>
  SymBranching (WriterStrict.WriterT s m)
  where
  mrgIfWithStrategy s cond (WriterStrict.WriterT t) (WriterStrict.WriterT f) =
    WriterStrict.WriterT $
      mrgIfWithStrategy (liftRootStrategy2 s rootStrategy) cond t f
  {-# INLINE mrgIfWithStrategy #-}
  mrgIfPropagatedStrategy
    cond
    (WriterStrict.WriterT t)
    (WriterStrict.WriterT f) =
      WriterStrict.WriterT $ mrgIfPropagatedStrategy cond t f
  {-# INLINE mrgIfPropagatedStrategy #-}

-- ReaderT
instance
  (Mergeable a, SymBranching m) =>
  SimpleMergeable (ReaderT s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (SymBranching m) =>
  SimpleMergeable1 (ReaderT s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (SymBranching m) =>
  SymBranching (ReaderT s m)
  where
  mrgIfWithStrategy s cond (ReaderT t) (ReaderT f) =
    ReaderT $ \v -> mrgIfWithStrategy s cond (t v) (f v)
  {-# INLINE mrgIfWithStrategy #-}
  mrgIfPropagatedStrategy cond (ReaderT t) (ReaderT f) =
    ReaderT $ \v -> mrgIfPropagatedStrategy cond (t v) (f v)
  {-# INLINE mrgIfPropagatedStrategy #-}

-- IdentityT
instance
  (SymBranching m, Mergeable a) =>
  SimpleMergeable (IdentityT m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance (SymBranching m) => SimpleMergeable1 (IdentityT m) where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance (SymBranching m) => SymBranching (IdentityT m) where
  mrgIfWithStrategy s cond (IdentityT l) (IdentityT r) =
    IdentityT $ mrgIfWithStrategy s cond l r
  {-# INLINE mrgIfWithStrategy #-}
  mrgIfPropagatedStrategy cond (IdentityT l) (IdentityT r) =
    IdentityT $ mrgIfPropagatedStrategy cond l r
  {-# INLINE mrgIfPropagatedStrategy #-}

-- ContT
instance (SymBranching m, Mergeable r) => SimpleMergeable (ContT r m a) where
  mrgIte cond (ContT l) (ContT r) = ContT $ \c -> mrgIf cond (l c) (r c)
  {-# INLINE mrgIte #-}

instance (SymBranching m, Mergeable r) => SimpleMergeable1 (ContT r m) where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance (SymBranching m, Mergeable r) => SymBranching (ContT r m) where
  mrgIfWithStrategy _ cond (ContT l) (ContT r) =
    ContT $ \c -> mrgIf cond (l c) (r c)
  {-# INLINE mrgIfWithStrategy #-}
  mrgIfPropagatedStrategy cond (ContT l) (ContT r) =
    ContT $ \c -> mrgIfPropagatedStrategy cond (l c) (r c)
  {-# INLINE mrgIfPropagatedStrategy #-}

-- RWST
instance
  (Mergeable s, Mergeable w, Monoid w, Mergeable a, SymBranching m) =>
  SimpleMergeable (RWSLazy.RWST r w s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (Mergeable s, Mergeable w, Monoid w, SymBranching m) =>
  SimpleMergeable1 (RWSLazy.RWST r w s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (Mergeable s, Mergeable w, Monoid w, SymBranching m) =>
  SymBranching (RWSLazy.RWST r w s m)
  where
  mrgIfWithStrategy ms cond (RWSLazy.RWST t) (RWSLazy.RWST f) =
    RWSLazy.RWST $ \r s ->
      mrgIfWithStrategy
        (liftRootStrategy3 ms rootStrategy rootStrategy)
        cond
        (t r s)
        (f r s)
  {-# INLINE mrgIfWithStrategy #-}
  mrgIfPropagatedStrategy cond (RWSLazy.RWST t) (RWSLazy.RWST f) =
    RWSLazy.RWST $ \r s -> mrgIfPropagatedStrategy cond (t r s) (f r s)
  {-# INLINE mrgIfPropagatedStrategy #-}

instance
  (Mergeable s, Mergeable w, Monoid w, Mergeable a, SymBranching m) =>
  SimpleMergeable (RWSStrict.RWST r w s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (Mergeable s, Mergeable w, Monoid w, SymBranching m) =>
  SimpleMergeable1 (RWSStrict.RWST r w s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (Mergeable s, Mergeable w, Monoid w, SymBranching m) =>
  SymBranching (RWSStrict.RWST r w s m)
  where
  mrgIfWithStrategy ms cond (RWSStrict.RWST t) (RWSStrict.RWST f) =
    RWSStrict.RWST $ \r s ->
      mrgIfWithStrategy
        (liftRootStrategy3 ms rootStrategy rootStrategy)
        cond
        (t r s)
        (f r s)
  {-# INLINE mrgIfWithStrategy #-}
  mrgIfPropagatedStrategy cond (RWSStrict.RWST t) (RWSStrict.RWST f) =
    RWSStrict.RWST $ \r s -> mrgIfPropagatedStrategy cond (t r s) (f r s)
  {-# INLINE mrgIfPropagatedStrategy #-}

-- Product
deriving via
  (Default (Product l r a))
  instance
    (SimpleMergeable (l a), SimpleMergeable (r a)) =>
    SimpleMergeable (Product l r a)

deriving via
  (Default1 (Product l r))
  instance
    (SimpleMergeable1 l, SimpleMergeable1 r) => SimpleMergeable1 (Product l r)

-- Compose
deriving via
  (Default (Compose f g a))
  instance
    (SimpleMergeable (f (g a))) =>
    SimpleMergeable (Compose f g a)

instance
  (SimpleMergeable1 f, SimpleMergeable1 g) =>
  SimpleMergeable1 (Compose f g)
  where
  liftMrgIte m cond (Compose l) (Compose r) =
    Compose $ liftMrgIte (liftMrgIte m) cond l r

-- Const
deriving via
  (Default (Const a b))
  instance
    (SimpleMergeable a) => SimpleMergeable (Const a b)

deriving via
  (Default1 (Const a))
  instance
    (SimpleMergeable a) => SimpleMergeable1 (Const a)

-- Alt
deriving via
  (Default (Alt f a))
  instance
    (SimpleMergeable (f a)) => SimpleMergeable (Alt f a)

deriving via
  (Default1 (Alt f))
  instance
    (SimpleMergeable1 f) => SimpleMergeable1 (Alt f)

-- Ap
deriving via
  (Default (Ap f a))
  instance
    (SimpleMergeable (f a)) => SimpleMergeable (Ap f a)

deriving via
  (Default1 (Ap f))
  instance
    (SimpleMergeable1 f) => SimpleMergeable1 (Ap f)

-- Endo
instance (SimpleMergeable a) => SimpleMergeable (Endo a) where
  mrgIte = mrgIte1
  {-# INLINE mrgIte #-}

instance SimpleMergeable1 Endo where
  liftMrgIte m cond (Endo l) (Endo r) = Endo $ liftMrgIte m cond l r
  {-# INLINE liftMrgIte #-}

-- Generic
deriving via (Default (U1 p)) instance SimpleMergeable (U1 p)

deriving via (Default (V1 p)) instance SimpleMergeable (V1 p)

deriving via
  (Default (K1 i c p))
  instance
    (SimpleMergeable c) => SimpleMergeable (K1 i c p)

deriving via
  (Default (M1 i c f p))
  instance
    (SimpleMergeable (f p)) => SimpleMergeable (M1 i c f p)

deriving via
  (Default ((f :*: g) p))
  instance
    (SimpleMergeable (f p), SimpleMergeable (g p)) =>
    SimpleMergeable ((f :*: g) p)

deriving via
  (Default (Par1 p))
  instance
    (SimpleMergeable p) => SimpleMergeable (Par1 p)

deriving via
  (Default (Rec1 f p))
  instance
    (SimpleMergeable (f p)) => SimpleMergeable (Rec1 f p)

deriving via
  (Default ((f :.: g) p))
  instance
    (SimpleMergeable (f (g p))) => SimpleMergeable ((f :.: g) p)

#define SIMPLE_MERGEABLE_SIMPLE(symtype) \
instance SimpleMergeable symtype where \
  mrgIte = symIte; \
  {-# INLINE mrgIte #-}

#define SIMPLE_MERGEABLE_BV(symtype) \
instance (KnownNat n, 1 <= n) => SimpleMergeable (symtype n) where \
  mrgIte = symIte; \
  {-# INLINE mrgIte #-}

#define SIMPLE_MERGEABLE_FUN(cop, op) \
instance (SupportedPrim (cop ca cb), LinkedRep ca sa, LinkedRep cb sb) => \
  SimpleMergeable (op sa sb) where \
  mrgIte = symIte; \
  {-# INLINE mrgIte #-}

#if 1
SIMPLE_MERGEABLE_SIMPLE(SymBool)
SIMPLE_MERGEABLE_SIMPLE(SymInteger)
SIMPLE_MERGEABLE_SIMPLE(SymFPRoundingMode)
SIMPLE_MERGEABLE_SIMPLE(SymAlgReal)
SIMPLE_MERGEABLE_BV(SymIntN)
SIMPLE_MERGEABLE_BV(SymWordN)
SIMPLE_MERGEABLE_FUN((=->), (=~>))
SIMPLE_MERGEABLE_FUN((-->), (-~>))
#endif

instance (ValidFP eb sb) => SimpleMergeable (SymFP eb sb) where
  mrgIte = symIte
  {-# INLINE mrgIte #-}
