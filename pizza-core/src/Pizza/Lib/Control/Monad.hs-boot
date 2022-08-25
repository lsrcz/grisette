{-# LANGUAGE RankNTypes #-}

module Pizza.Lib.Control.Monad where

import Control.Monad
import Pizza.Core.Control.Monad.Union
import Pizza.Core.Data.Class.Bool
import Pizza.Core.Data.Class.Mergeable

mrgReturnWithStrategy :: (MonadUnion bool u) => MergingStrategy bool a -> a -> u a

-- | '>>=' with 'Mergeable' knowledge propagation.
mrgBindWithStrategy :: (MonadUnion bool u) => MergingStrategy bool b -> u a -> (a -> u b) -> u b

-- | 'return' with 'Mergeable' knowledge propagation.
mrgReturn :: (MonadUnion bool u, Mergeable bool a) => a -> u a

-- | '>>=' with 'Mergeable' knowledge propagation.
(>>=~) :: (MonadUnion bool u, Mergeable bool b) => u a -> (a -> u b) -> u b
mrgFoldM :: (MonadUnion bool m, Mergeable bool b, Foldable t) => (b -> a -> m b) -> b -> t a -> m b

-- | '>>' with 'Mergeable' knowledge propagation.
--
-- This is usually more efficient than calling the original '>>' and merge the results.
(>>~) :: forall bool m a b. (SymBoolOp bool, MonadUnion bool m, Mergeable bool b) => m a -> m b -> m b

-- | 'mzero' with 'Mergeable' knowledge propagation.
mrgMzero :: forall bool m a. (MonadUnion bool m, Mergeable bool a, MonadPlus m) => m a

-- | 'mplus' with 'Mergeable' knowledge propagation.
mrgMplus :: forall bool m a. (MonadUnion bool m, Mergeable bool a, MonadPlus m) => m a -> m a -> m a

-- | 'fmap' with 'Mergeable' knowledge propagation.
mrgFmap :: (MonadUnion bool f, Mergeable bool b, Functor f) => (a -> b) -> f a -> f b
