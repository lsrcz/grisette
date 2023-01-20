{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.Lib.Control.Monad
-- Copyright   :   (c) Sirui Lu 2021-2022
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
    (>>=~),
    (>>~),
    mrgFoldM,
    mrgMzero,
    mrgMplus,
    mrgFmap,
  )
where

import Control.Monad
import Grisette.Core.Control.Monad.Union
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Lib.Data.Foldable

-- | 'return' with 'GMergingStrategy' knowledge propagation.
mrgReturnWithStrategy :: (MonadUnion u) => MergingStrategy a -> a -> u a
mrgReturnWithStrategy s = mergeWithStrategy s . return
{-# INLINE mrgReturnWithStrategy #-}

-- | '>>=' with 'GMergingStrategy' knowledge propagation.
mrgBindWithStrategy :: (MonadUnion u) => MergingStrategy b -> u a -> (a -> u b) -> u b
mrgBindWithStrategy s a f = mergeWithStrategy s $ a >>= f
{-# INLINE mrgBindWithStrategy #-}

-- | 'return' with 'GMergingStrategy' knowledge propagation.
mrgReturn :: (MonadUnion u, Mergeable a) => a -> u a
mrgReturn = merge . return
{-# INLINE mrgReturn #-}

-- | '>>=' with 'GMergingStrategy' knowledge propagation.
(>>=~) :: (MonadUnion u, Mergeable b) => u a -> (a -> u b) -> u b
a >>=~ f = merge $ a >>= f
{-# INLINE (>>=~) #-}

-- | 'foldM' with 'GMergingStrategy' knowledge propagation.
mrgFoldM :: (MonadUnion m, Mergeable b, Foldable t) => (b -> a -> m b) -> b -> t a -> m b
mrgFoldM = mrgFoldlM
{-# INLINE mrgFoldM #-}

-- | '>>' with 'GMergingStrategy' knowledge propagation.
--
-- This is usually more efficient than calling the original '>>' and merge the results.
(>>~) :: forall m a b. (MonadUnion m, Mergeable b) => m a -> m b -> m b
a >>~ f = merge $ mrgFmap (const ()) a >> f
{-# INLINE (>>~) #-}

-- | 'mzero' with 'GMergingStrategy' knowledge propagation.
mrgMzero :: forall m a. (MonadUnion m, Mergeable a, MonadPlus m) => m a
mrgMzero = merge mzero
{-# INLINE mrgMzero #-}

-- | 'mplus' with 'GMergingStrategy' knowledge propagation.
mrgMplus :: forall m a. (MonadUnion m, Mergeable a, MonadPlus m) => m a -> m a -> m a
mrgMplus a b = merge $ mplus a b
{-# INLINE mrgMplus #-}

-- | 'fmap' with 'GMergingStrategy' knowledge propagation.
mrgFmap :: (MonadUnion f, Mergeable b, Functor f) => (a -> b) -> f a -> f b
mrgFmap f a = merge $ fmap f a
{-# INLINE mrgFmap #-}