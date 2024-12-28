{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}

-- |
-- Module      :   Grisette.Internal.Internal.Decl.Core.Data.Class.TryMerge
-- Copyright   :   (c) Sirui Lu 2023-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Decl.Core.Data.Class.TryMerge
  ( TryMerge (..),
    tryMerge,
    MonadTryMerge,
    mrgSingle,
    mrgSingleWithStrategy,
    mrgToSym,
    toUnionSym,
  )
where

import Grisette.Internal.Core.Data.Class.ToSym (ToSym (toSym))
import Grisette.Internal.Internal.Decl.Core.Data.Class.Mergeable
  ( Mergeable (rootStrategy),
    MergingStrategy,
  )

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim

-- | A class for containers that may or may not be merged.
--
-- If the container is capable of multi-path execution, then the
-- `tryMergeWithStrategy` function should merge the paths according to the
-- supplied strategy.
--
-- If the container is not capable of multi-path execution, then the
-- `tryMergeWithStrategy` function should be equivalent to `id`.
--
-- Note that this will not necessarily do a recursive merge for the elements.
class TryMerge m where
  tryMergeWithStrategy :: MergingStrategy a -> m a -> m a

-- | Try to merge the container with the root strategy.
tryMerge :: (TryMerge m, Mergeable a) => m a -> m a
tryMerge = tryMergeWithStrategy rootStrategy
{-# INLINE tryMerge #-}

-- | Wrap a value in the applicative functor and capture the 'Mergeable'
-- knowledge.
--
-- >>> mrgSingleWithStrategy rootStrategy "a" :: Union SymInteger
-- {a}
--
-- __Note:__ Be careful to call this directly from your code.
-- The supplied merge strategy should be consistent with the type's root merge
-- strategy, or some internal invariants would be broken and the program can
-- crash.
--
-- This function is to be called when the 'Mergeable' constraint can not be
-- resolved, e.g., the merge strategy for the contained type is given with
-- 'Mergeable1'. In other cases, 'Grisette.Lib.Control.Applicative.mrgPure'
-- is usually a better alternative.
mrgSingleWithStrategy ::
  (TryMerge m, Applicative m) =>
  MergingStrategy a ->
  a ->
  m a
mrgSingleWithStrategy strategy = tryMergeWithStrategy strategy . pure
{-# INLINE mrgSingleWithStrategy #-}

-- | Wrap a value in the applicative functor and propagate the type's root merge
-- strategy.
--
-- Equivalent to @'mrgSingleWithStrategy' 'rootStrategy'@.
--
-- >>> mrgSingle "a" :: Union SymInteger
-- {a}
mrgSingle :: (TryMerge m, Applicative m, Mergeable a) => a -> m a
mrgSingle = mrgSingleWithStrategy rootStrategy
{-# INLINE mrgSingle #-}

-- | Alias for a monad type that has 'TryMerge'.
type MonadTryMerge f = (TryMerge f, Monad f)

-- | Convert a value to symbolic value and wrap it with a mergeable container.
--
-- This is a synonym for 'toUnionSym'.
mrgToSym ::
  (ToSym a b, Mergeable b, TryMerge m, Applicative m) =>
  a ->
  m b
mrgToSym = toUnionSym
{-# INLINE mrgToSym #-}

-- | Convert a value to symbolic value and wrap it with a mergeable container.
--
-- This is a synonym for 'toUnionSym'.
toUnionSym ::
  (ToSym a b, Mergeable b, TryMerge m, Applicative m) =>
  a ->
  m b
toUnionSym = tryMerge . pure . toSym
{-# INLINE toUnionSym #-}
