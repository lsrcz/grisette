{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.Lib.Control.Monad
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Lib.Control.Monad
  ( -- * mrg* variants for operations in "Control.Monad"
    mrgReturnWithStrategy,
    mrgBindWithStrategy,
    mrgReturn,
    (.>>=),
    (.>>),
    mrgFoldM,
    mrgMzero,
    mrgMplus,
    mrgFmap,
  )
where

import Control.Monad (MonadPlus (mplus, mzero))
import Grisette.Core.Control.Monad.Union (MonadUnion)
import Grisette.Core.Data.Class.Mergeable
  ( Mergeable,
    MergingStrategy,
  )
import Grisette.Core.Data.Class.SimpleMergeable
  ( UnionLike (mergeWithStrategy),
    merge,
  )
import Grisette.Lib.Data.Foldable (mrgFoldlM)

-- | 'return' with 'MergingStrategy' knowledge propagation.
mrgReturnWithStrategy :: (MonadUnion u) => MergingStrategy a -> a -> u a
mrgReturnWithStrategy s = mergeWithStrategy s . return
{-# INLINE mrgReturnWithStrategy #-}

-- | '>>=' with 'MergingStrategy' knowledge propagation.
mrgBindWithStrategy :: (MonadUnion u) => MergingStrategy b -> u a -> (a -> u b) -> u b
mrgBindWithStrategy s a f = mergeWithStrategy s $ a >>= f
{-# INLINE mrgBindWithStrategy #-}

-- | 'return' with 'MergingStrategy' knowledge propagation.
mrgReturn :: (MonadUnion u, Mergeable a) => a -> u a
mrgReturn = merge . return
{-# INLINE mrgReturn #-}

-- | '>>=' with 'MergingStrategy' knowledge propagation.
(.>>=) :: (MonadUnion u, Mergeable b) => u a -> (a -> u b) -> u b
a .>>= f = merge $ a >>= f
{-# INLINE (.>>=) #-}

-- | 'foldM' with 'MergingStrategy' knowledge propagation.
mrgFoldM :: (MonadUnion m, Mergeable b, Foldable t) => (b -> a -> m b) -> b -> t a -> m b
mrgFoldM = mrgFoldlM
{-# INLINE mrgFoldM #-}

-- | '>>' with 'MergingStrategy' knowledge propagation.
--
-- This is usually more efficient than calling the original '>>' and merge the results.
(.>>) :: forall m a b. (MonadUnion m, Mergeable b) => m a -> m b -> m b
a .>> f = merge $ mrgFmap (const ()) a >> f
{-# INLINE (.>>) #-}

-- | 'mzero' with 'MergingStrategy' knowledge propagation.
mrgMzero :: forall m a. (MonadUnion m, Mergeable a, MonadPlus m) => m a
mrgMzero = merge mzero
{-# INLINE mrgMzero #-}

-- | 'mplus' with 'MergingStrategy' knowledge propagation.
mrgMplus :: forall m a. (MonadUnion m, Mergeable a, MonadPlus m) => m a -> m a -> m a
mrgMplus a b = merge $ mplus a b
{-# INLINE mrgMplus #-}

-- | 'fmap' with 'MergingStrategy' knowledge propagation.
mrgFmap :: (MonadUnion f, Mergeable b, Functor f) => (a -> b) -> f a -> f b
mrgFmap f a = merge $ fmap f a
{-# INLINE mrgFmap #-}
