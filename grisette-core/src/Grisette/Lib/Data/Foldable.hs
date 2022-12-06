{-# LANGUAGE RankNTypes #-}

module Grisette.Lib.Data.Foldable
  ( mrgFoldlM,
    mrgFoldrM,
    mrgTraverse_,
    mrgFor_,
    mrgMapM_,
    mrgForM_,
    mrgSequence_,
    mrgMsum,
  )
where

import Control.Monad
import Grisette.Core.Control.Monad.Union
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Data.Class.SimpleMergeable
import {-# SOURCE #-} Grisette.Lib.Control.Monad

-- | 'foldlM' with 'GMergingStrategy' knowledge propagation.
mrgFoldlM :: (GMonadUnion bool m, GMergeable bool b, Foldable t) => (b -> a -> m b) -> b -> t a -> m b
mrgFoldlM f z0 xs = foldr c mrgReturn xs z0
  where
    c x k z = merge (f z x) >>= k
{-# INLINE mrgFoldlM #-}

-- | 'foldrM' with 'GMergingStrategy' knowledge propagation.
mrgFoldrM :: (GMonadUnion bool m, GMergeable bool b, Foldable t) => (a -> b -> m b) -> b -> t a -> m b
mrgFoldrM f z0 xs = foldl c mrgReturn xs z0
  where
    c k x z = merge (f x z) >>= k
{-# INLINE mrgFoldrM #-}

-- | 'traverse_' with 'GMergingStrategy' knowledge propagation.
mrgTraverse_ :: (SymBoolOp bool, GMonadUnion bool m, Foldable t) => (a -> m b) -> t a -> m ()
mrgTraverse_ f = foldr c (mrgReturn ())
  where
    c x k = f x >> k
{-# INLINE mrgTraverse_ #-}

-- | 'for_' with 'GMergingStrategy' knowledge propagation.
mrgFor_ :: (SymBoolOp bool, GMonadUnion bool m, Foldable t) => t a -> (a -> m b) -> m ()
mrgFor_ = flip mrgTraverse_
{-# INLINE mrgFor_ #-}

-- | 'mapM_' with 'GMergingStrategy' knowledge propagation.
mrgMapM_ :: (SymBoolOp bool, GMonadUnion bool m, Foldable t) => (a -> m b) -> t a -> m ()
mrgMapM_ = mrgTraverse_
{-# INLINE mrgMapM_ #-}

-- | 'forM_' with 'GMergingStrategy' knowledge propagation.
mrgForM_ :: (SymBoolOp bool, GMonadUnion bool m, Foldable t) => t a -> (a -> m b) -> m ()
mrgForM_ = flip mrgMapM_
{-# INLINE mrgForM_ #-}

-- | 'sequence_' with 'GMergingStrategy' knowledge propagation.
mrgSequence_ :: (SymBoolOp bool, Foldable t, GMonadUnion bool m) => t (m a) -> m ()
mrgSequence_ = foldr c (mrgReturn ())
  where
    c m k = m >> k
{-# INLINE mrgSequence_ #-}

-- | 'msum' with 'GMergingStrategy' knowledge propagation.
mrgMsum :: forall bool m a t. (GMonadUnion bool m, GMergeable bool a, MonadPlus m, Foldable t) => t (m a) -> m a
mrgMsum = foldr mrgMplus mrgMzero
{-# INLINE mrgMsum #-}
