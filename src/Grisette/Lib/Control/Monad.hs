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
  )
where

import Control.Monad (MonadPlus (mplus, mzero))
import Grisette.Core.Data.Class.Mergeable
  ( Mergeable,
    MergingStrategy,
  )
import Grisette.Core.Data.Class.TryMerge
  ( MonadTryMerge,
    TryMerge (tryMergeWithStrategy),
    tryMerge,
  )
import Grisette.Lib.Data.Foldable (mrgFoldlM)
import Grisette.Lib.Data.Functor (mrgFmap)

-- | 'return' with 'MergingStrategy' knowledge propagation.
mrgReturnWithStrategy :: (MonadTryMerge u) => MergingStrategy a -> a -> u a
mrgReturnWithStrategy s = tryMergeWithStrategy s . return
{-# INLINE mrgReturnWithStrategy #-}

-- | '>>=' with 'MergingStrategy' knowledge propagation.
mrgBindWithStrategy :: (MonadTryMerge u) => MergingStrategy b -> u a -> (a -> u b) -> u b
mrgBindWithStrategy s a f = tryMergeWithStrategy s $ a >>= f
{-# INLINE mrgBindWithStrategy #-}

-- | 'return' with 'MergingStrategy' knowledge propagation.
mrgReturn :: (MonadTryMerge u, Mergeable a) => a -> u a
mrgReturn = tryMerge . return
{-# INLINE mrgReturn #-}

-- | '>>=' with 'MergingStrategy' knowledge propagation.
(.>>=) :: (MonadTryMerge u, Mergeable b) => u a -> (a -> u b) -> u b
a .>>= f = tryMerge $ a >>= f
{-# INLINE (.>>=) #-}

-- | 'foldM' with 'MergingStrategy' knowledge propagation.
mrgFoldM :: (MonadTryMerge m, Mergeable b, Foldable t) => (b -> a -> m b) -> b -> t a -> m b
mrgFoldM = mrgFoldlM
{-# INLINE mrgFoldM #-}

-- | '>>' with 'MergingStrategy' knowledge propagation.
--
-- This is usually more efficient than calling the original '>>' and merge the results.
(.>>) :: forall m a b. (MonadTryMerge m, Mergeable b) => m a -> m b -> m b
a .>> f = tryMerge $ mrgFmap (const ()) a >> f
{-# INLINE (.>>) #-}

-- | 'mzero' with 'MergingStrategy' knowledge propagation.
mrgMzero :: forall m a. (MonadTryMerge m, Mergeable a, MonadPlus m) => m a
mrgMzero = tryMerge mzero
{-# INLINE mrgMzero #-}

-- | 'mplus' with 'MergingStrategy' knowledge propagation.
mrgMplus :: forall m a. (MonadTryMerge m, Mergeable a, MonadPlus m) => m a -> m a -> m a
mrgMplus a b = tryMerge $ mplus a b
{-# INLINE mrgMplus #-}
