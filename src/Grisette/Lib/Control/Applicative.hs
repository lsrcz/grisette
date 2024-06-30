-- |
-- Module      :   Grisette.Lib.Control.Applicative
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Lib.Control.Applicative
  ( -- * Applicative Functors
    mrgPureWithStrategy,
    mrgPure,
    (.<*>),
    mrgLiftA2,
    (.*>),
    (.<*),

    -- * Alternatives
    mrgEmpty,
    (.<|>),
    mrgSome,
    mrgMany,

    -- * Utility functions
    (.<$>),
    (.<$),
    (.<**>),
    mrgLiftA,
    mrgLiftA3,
    mrgOptional,
    mrgAsum,
  )
where

import Control.Applicative (Alternative)
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable, MergingStrategy)
import Grisette.Internal.Core.Data.Class.TryMerge (TryMerge)
import Grisette.Lib.Data.Functor ((.<$), (.<$>))
import qualified Grisette.Unified.Lib.Control.Applicative as Unified

-- | Alias for 'Grisette.Core.mrgSingleWithStrategy'.
mrgPureWithStrategy ::
  (TryMerge m, Applicative m) => MergingStrategy a -> a -> m a
mrgPureWithStrategy = Unified.mrgPureWithStrategy
{-# INLINE mrgPureWithStrategy #-}

-- | Alias for 'Grisette.Core.mrgSingle'.
mrgPure :: (TryMerge m, Applicative m, Mergeable a) => a -> m a
mrgPure = Unified.mrgPure
{-# INLINE mrgPure #-}

infixl 4 .<*>

-- | '<*>' with 'MergingStrategy' knowledge propagation.
(.<*>) ::
  (Applicative f, TryMerge f, Mergeable a, Mergeable b) =>
  f (a -> b) ->
  f a ->
  f b
(.<*>) = (Unified..<*>)
{-# INLINE (.<*>) #-}

-- | 'liftA2' with 'MergingStrategy' knowledge propagation.
mrgLiftA2 ::
  (Applicative f, TryMerge f, Mergeable a, Mergeable b, Mergeable c) =>
  (a -> b -> c) ->
  f a ->
  f b ->
  f c
mrgLiftA2 = Unified.mrgLiftA2
{-# INLINE mrgLiftA2 #-}

infixl 4 .*>

-- | '*>' with 'MergingStrategy' knowledge propagation.
(.*>) ::
  (Applicative f, TryMerge f, Mergeable a, Mergeable b) => f a -> f b -> f b
(.*>) = (Unified..*>)
{-# INLINE (.*>) #-}

infixl 4 .<*

-- | '<*' with 'MergingStrategy' knowledge propagation.
(.<*) ::
  (Applicative f, TryMerge f, Mergeable a, Mergeable b) => f a -> f b -> f a
(.<*) = (Unified..<*)
{-# INLINE (.<*) #-}

-- | 'Control.Applicative.empty' with 'MergingStrategy' knowledge propagation.
mrgEmpty :: (Alternative f, TryMerge f, Mergeable a) => f a
mrgEmpty = Unified.mrgEmpty
{-# INLINE mrgEmpty #-}

infixl 3 .<|>

-- | 'Control.Applicative.<|>' with 'MergingStrategy' knowledge propagation.
(.<|>) :: (Alternative f, TryMerge f, Mergeable a) => f a -> f a -> f a
(.<|>) = (Unified..<|>)
{-# INLINE (.<|>) #-}

-- | 'Control.Applicative.some' with 'MergingStrategy' knowledge propagation.
mrgSome :: (Alternative f, TryMerge f, Mergeable a) => f a -> f [a]
mrgSome = Unified.mrgSome
{-# INLINE mrgSome #-}

-- | 'Control.Applicative.many' with 'MergingStrategy' knowledge propagation.
mrgMany :: (Alternative f, TryMerge f, Mergeable a) => f a -> f [a]
mrgMany = Unified.mrgMany
{-# INLINE mrgMany #-}

infixl 4 .<**>

-- | 'Control.Applicative.<**>' with 'MergingStrategy' knowledge propagation.
(.<**>) ::
  (Applicative f, TryMerge f, Mergeable a, Mergeable b) =>
  f a ->
  f (a -> b) ->
  f b
(.<**>) = (Unified..<**>)
{-# INLINE (.<**>) #-}

-- | 'Control.Applicative.liftA' with 'MergingStrategy' knowledge propagation.
mrgLiftA ::
  (Applicative f, TryMerge f, Mergeable a, Mergeable b) =>
  (a -> b) ->
  f a ->
  f b
mrgLiftA = Unified.mrgLiftA
{-# INLINE mrgLiftA #-}

-- | 'Control.Applicative.liftA3' with 'MergingStrategy' knowledge propagation.
mrgLiftA3 ::
  ( Applicative f,
    TryMerge f,
    Mergeable a,
    Mergeable b,
    Mergeable c,
    Mergeable d
  ) =>
  (a -> b -> c -> d) ->
  f a ->
  f b ->
  f c ->
  f d
mrgLiftA3 = Unified.mrgLiftA3
{-# INLINE mrgLiftA3 #-}

-- | 'Control.Applicative.optional' with 'MergingStrategy' knowledge
-- propagation.
mrgOptional ::
  (Alternative f, TryMerge f, Mergeable a) =>
  f a ->
  f (Maybe a)
mrgOptional = Unified.mrgOptional
{-# INLINE mrgOptional #-}

-- | 'Control.Applicative.asum' with 'MergingStrategy' knowledge propagation.
mrgAsum ::
  (Alternative f, TryMerge f, Mergeable a, Foldable t) => t (f a) -> f a
mrgAsum = Unified.mrgAsum
{-# INLINE mrgAsum #-}
