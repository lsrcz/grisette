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
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Internal.Decl.Core.Data.Class.SimpleMergeable
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Decl.Core.Data.Class.SimpleMergeable
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

import Data.Kind (Type)
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
import Generics.Deriving (Default (Default), Default1 (Default1))
import Grisette.Internal.Core.Data.Class.AsKey (AsKey (AsKey), AsKey1 (AsKey1))
import Grisette.Internal.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Internal.Internal.Decl.Core.Data.Class.Mergeable
  ( GMergeable,
    Mergeable (rootStrategy),
    Mergeable1,
    Mergeable2,
    MergingStrategy,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.TryMerge
  ( TryMerge (tryMergeWithStrategy),
  )
import Grisette.Internal.SymPrim.Prim.Internal.Term (SupportedPrim (pevalITETerm))
import Grisette.Internal.SymPrim.SymBool (SymBool (SymBool))
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

instance SimpleMergeable SymBool where
  mrgIte (SymBool c) (SymBool t) (SymBool f) = SymBool $ pevalITETerm c t f
  {-# INLINE mrgIte #-}

instance {-# OVERLAPPABLE #-} (SimpleMergeable a) => ITEOp a where
  symIte = mrgIte
  {-# INLINE symIte #-}

instance (SimpleMergeable a) => SimpleMergeable (AsKey a) where
  mrgIte c (AsKey t) (AsKey f) = AsKey $ mrgIte c t f
  {-# INLINE mrgIte #-}

instance (SimpleMergeable (f a)) => SimpleMergeable (AsKey1 f a) where
  mrgIte c (AsKey1 t) (AsKey1 f) = AsKey1 $ mrgIte c t f
  {-# INLINE mrgIte #-}

instance
  (SimpleMergeable1 f) =>
  SimpleMergeable1 (AsKey1 f)
  where
  liftMrgIte m c (AsKey1 t) (AsKey1 f) = AsKey1 $ liftMrgIte m c t f
  {-# INLINE liftMrgIte #-}

instance (SymBranching f) => SymBranching (AsKey1 f) where
  mrgIfWithStrategy strategy cond (AsKey1 t) (AsKey1 f) =
    AsKey1 $ mrgIfWithStrategy strategy cond t f
  mrgIfPropagatedStrategy cond (AsKey1 t) (AsKey1 f) =
    AsKey1 $ mrgIfPropagatedStrategy cond t f
  {-# INLINE mrgIfWithStrategy #-}
  {-# INLINE mrgIfPropagatedStrategy #-}
