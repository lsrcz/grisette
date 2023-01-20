{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}

module Grisette.Lib.Control.Monad where

import Control.Monad
import Grisette.Core.Control.Monad.Union
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Mergeable

mrgReturnWithStrategy :: (MonadUnion u) => MergingStrategy a -> a -> u a

-- | '>>=' with 'Mergeable' knowledge propagation.
mrgBindWithStrategy :: (MonadUnion u) => MergingStrategy b -> u a -> (a -> u b) -> u b

-- | 'return' with 'Mergeable' knowledge propagation.
mrgReturn :: (MonadUnion u, Mergeable a) => a -> u a

-- | '>>=' with 'Mergeable' knowledge propagation.
(>>=~) :: (MonadUnion u, Mergeable b) => u a -> (a -> u b) -> u b
mrgFoldM :: (MonadUnion m, Mergeable b, Foldable t) => (b -> a -> m b) -> b -> t a -> m b

-- | '>>' with 'Mergeable' knowledge propagation.
--
-- This is usually more efficient than calling the original '>>' and merge the results.
(>>~) :: forall m a b. (MonadUnion m, Mergeable b) => m a -> m b -> m b

-- | 'mzero' with 'Mergeable' knowledge propagation.
mrgMzero :: forall m a. (MonadUnion m, Mergeable a, MonadPlus m) => m a

-- | 'mplus' with 'Mergeable' knowledge propagation.
mrgMplus :: forall m a. (MonadUnion m, Mergeable a, MonadPlus m) => m a -> m a -> m a

-- | 'fmap' with 'Mergeable' knowledge propagation.
mrgFmap :: (MonadUnion f, Mergeable b, Functor f) => (a -> b) -> f a -> f b
