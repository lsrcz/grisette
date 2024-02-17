{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.Lib.Control.Foldable
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Lib.Data.Foldable
  ( -- * mrg* variants for operations in "Data.Foldable"
    mrgFoldlM,
    mrgFoldrM,
    mrgTraverse_,
    mrgFor_,
    mrgMapM_,
    mrgForM_,
    mrgSequence_,
    mrgMsum,
  )
where

import Control.Monad (MonadPlus)
import Grisette.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Core.Data.Class.TryMerge
  ( MonadTryMerge,
    TryMerge,
    mrgPure,
    tryMerge,
  )
import {-# SOURCE #-} Grisette.Lib.Control.Monad
  ( mrgMplus,
    mrgMzero,
    mrgReturn,
  )

-- | 'Data.Foldable.foldlM' with 'MergingStrategy' knowledge propagation.
mrgFoldlM :: (MonadTryMerge m, Mergeable b, Foldable t) => (b -> a -> m b) -> b -> t a -> m b
mrgFoldlM f z0 xs = foldr c mrgReturn xs z0
  where
    c x k z = tryMerge (f z x) >>= k
{-# INLINE mrgFoldlM #-}

-- | 'Data.Foldable.foldrM' with 'MergingStrategy' knowledge propagation.
mrgFoldrM :: (MonadTryMerge m, Mergeable b, Foldable t) => (a -> b -> m b) -> b -> t a -> m b
mrgFoldrM f z0 xs = foldl c mrgReturn xs z0
  where
    c k x z = tryMerge (f x z) >>= k
{-# INLINE mrgFoldrM #-}

-- | 'Data.Foldable.traverse_' with 'MergingStrategy' knowledge propagation.
mrgTraverse_ :: (Applicative m, TryMerge m, Foldable t) => (a -> m b) -> t a -> m ()
mrgTraverse_ f = foldr c (mrgPure ())
  where
    c x k = f x *> k
{-# INLINE mrgTraverse_ #-}

-- | 'Data.Foldable.for_' with 'MergingStrategy' knowledge propagation.
mrgFor_ :: (Applicative m, TryMerge m, Foldable t) => t a -> (a -> m b) -> m ()
mrgFor_ = flip mrgTraverse_
{-# INLINE mrgFor_ #-}

-- | 'Data.Foldable.mapM_' with 'MergingStrategy' knowledge propagation.
mrgMapM_ :: (MonadTryMerge m, Foldable t) => (a -> m b) -> t a -> m ()
mrgMapM_ = mrgTraverse_
{-# INLINE mrgMapM_ #-}

-- | 'Data.Foldable.forM_' with 'MergingStrategy' knowledge propagation.
mrgForM_ :: (MonadTryMerge m, Foldable t) => t a -> (a -> m b) -> m ()
mrgForM_ = flip mrgMapM_
{-# INLINE mrgForM_ #-}

-- | 'Data.Foldable.sequence_' with 'MergingStrategy' knowledge propagation.
mrgSequence_ :: (Foldable t, MonadTryMerge m) => t (m a) -> m ()
mrgSequence_ = foldr c (mrgReturn ())
  where
    c m k = m >> k
{-# INLINE mrgSequence_ #-}

-- | 'Data.Foldable.msum' with 'MergingStrategy' knowledge propagation.
mrgMsum :: forall m a t. (MonadTryMerge m, Mergeable a, MonadPlus m, Foldable t) => t (m a) -> m a
mrgMsum = foldr mrgMplus mrgMzero
{-# INLINE mrgMsum #-}
