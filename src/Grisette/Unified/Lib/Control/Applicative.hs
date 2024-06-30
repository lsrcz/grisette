-- |
-- Module      :   Grisette.Unified.Lib.Control.Applicative
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Unified.Lib.Control.Applicative
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

import Control.Applicative (Alternative (empty, (<|>)), (<**>))
import Data.Functor (void)
import Grisette.Internal.Core.Data.Class.Mergeable
  ( Mergeable (rootStrategy),
    MergingStrategy,
  )
import Grisette.Internal.Core.Data.Class.TryMerge
  ( TryMerge,
    mrgSingleWithStrategy,
    tryMerge,
  )
import Grisette.Lib.Data.Functor ((.<$), (.<$>))

-- | Alias for 'mrgSingleWithStrategy'.
mrgPureWithStrategy ::
  (TryMerge m, Applicative m) => MergingStrategy a -> a -> m a
mrgPureWithStrategy = mrgSingleWithStrategy
{-# INLINE mrgPureWithStrategy #-}

-- | Alias for 'Grisette.Core.mrgSingle'.
mrgPure :: (TryMerge m, Applicative m, Mergeable a) => a -> m a
mrgPure = mrgPureWithStrategy rootStrategy
{-# INLINE mrgPure #-}

infixl 4 .<*>

-- | '<*>' with 'MergingStrategy' knowledge propagation.
(.<*>) ::
  (Applicative f, TryMerge f, Mergeable a, Mergeable b) =>
  f (a -> b) ->
  f a ->
  f b
f .<*> a = tryMerge $ tryMerge f <*> tryMerge a
{-# INLINE (.<*>) #-}

-- | 'liftA2' with 'MergingStrategy' knowledge propagation.
mrgLiftA2 ::
  (Applicative f, TryMerge f, Mergeable a, Mergeable b, Mergeable c) =>
  (a -> b -> c) ->
  f a ->
  f b ->
  f c
mrgLiftA2 f a b = f .<$> a .<*> tryMerge b
{-# INLINE mrgLiftA2 #-}

infixl 4 .*>

-- | '*>' with 'MergingStrategy' knowledge propagation.
(.*>) ::
  (Applicative f, TryMerge f, Mergeable a, Mergeable b) => f a -> f b -> f b
a .*> b = tryMerge $ tryMerge (void a) *> tryMerge b
{-# INLINE (.*>) #-}

infixl 4 .<*

-- | '<*' with 'MergingStrategy' knowledge propagation.
(.<*) ::
  (Applicative f, TryMerge f, Mergeable a, Mergeable b) => f a -> f b -> f a
a .<* b = tryMerge $ tryMerge a <* tryMerge (void b)
{-# INLINE (.<*) #-}

-- | 'empty' with 'MergingStrategy' knowledge propagation.
mrgEmpty :: (Alternative f, TryMerge f, Mergeable a) => f a
mrgEmpty = tryMerge empty
{-# INLINE mrgEmpty #-}

infixl 3 .<|>

-- | '<|>' with 'MergingStrategy' knowledge propagation.
(.<|>) :: (Alternative f, TryMerge f, Mergeable a) => f a -> f a -> f a
a .<|> b = tryMerge $ tryMerge a <|> tryMerge b
{-# INLINE (.<|>) #-}

-- | 'Control.Applicative.some' with 'MergingStrategy' knowledge propagation.
mrgSome :: (Alternative f, TryMerge f, Mergeable a) => f a -> f [a]
mrgSome v = some_v
  where
    many_v = some_v .<|> pure []
    some_v = mrgLiftA2 (:) v many_v
{-# INLINE mrgSome #-}

-- | 'Control.Applicative.many' with 'MergingStrategy' knowledge propagation.
mrgMany :: (Alternative f, TryMerge f, Mergeable a) => f a -> f [a]
mrgMany v = many_v
  where
    many_v = some_v .<|> pure []
    some_v = mrgLiftA2 (:) v many_v
{-# INLINE mrgMany #-}

infixl 4 .<**>

-- | 'Control.Applicative.<**>' with 'MergingStrategy' knowledge propagation.
(.<**>) ::
  (Applicative f, TryMerge f, Mergeable a, Mergeable b) =>
  f a ->
  f (a -> b) ->
  f b
a .<**> f = tryMerge $ tryMerge a <**> tryMerge f
{-# INLINE (.<**>) #-}

-- | 'Control.Applicative.liftA' with 'MergingStrategy' knowledge propagation.
mrgLiftA ::
  (Applicative f, TryMerge f, Mergeable a, Mergeable b) =>
  (a -> b) ->
  f a ->
  f b
mrgLiftA f a = mrgPure f .<*> a
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
mrgLiftA3 f a b c = mrgPure f .<*> a .<*> b .<*> c
{-# INLINE mrgLiftA3 #-}

-- | 'Control.Applicative.optional' with 'MergingStrategy' knowledge propagation.
mrgOptional ::
  (Alternative f, TryMerge f, Mergeable a) =>
  f a ->
  f (Maybe a)
mrgOptional v = Just .<$> v .<|> pure Nothing
{-# INLINE mrgOptional #-}

-- | 'Control.Applicative.asum' with 'MergingStrategy' knowledge propagation.
mrgAsum ::
  (Alternative f, TryMerge f, Mergeable a, Foldable t) => t (f a) -> f a
mrgAsum = foldr (.<|>) mrgEmpty
{-# INLINE mrgAsum #-}
