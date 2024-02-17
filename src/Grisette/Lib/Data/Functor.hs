module Grisette.Lib.Data.Functor (mrgFmap) where

import Grisette.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Core.Data.Class.TryMerge (TryMerge, tryMerge)

-- | 'fmap' with 'MergingStrategy' knowledge propagation.
mrgFmap :: (TryMerge f, Mergeable b, Functor f) => (a -> b) -> f a -> f b
mrgFmap f a = tryMerge $ fmap f a
{-# INLINE mrgFmap #-}
