{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}

module Grisette.Lib.Control.Monad
  ( mrgReturnWithStrategy,
    mrgBindWithStrategy,
    mrgReturn,
    (>>=~),
    mrgFoldM,
    (>>~),
    mrgMzero,
    mrgMplus,
    mrgFmap,
  )
where

import Control.Monad (MonadPlus)
import Grisette.Core.Control.Monad.Union (MonadUnion)
import Grisette.Core.Data.Class.Mergeable
  ( Mergeable,
    MergingStrategy,
  )

mrgReturnWithStrategy :: (MonadUnion u) => MergingStrategy a -> a -> u a
mrgBindWithStrategy :: (MonadUnion u) => MergingStrategy b -> u a -> (a -> u b) -> u b
mrgReturn :: (MonadUnion u, Mergeable a) => a -> u a
(>>=~) :: (MonadUnion u, Mergeable b) => u a -> (a -> u b) -> u b
mrgFoldM :: (MonadUnion m, Mergeable b, Foldable t) => (b -> a -> m b) -> b -> t a -> m b
(>>~) :: forall m a b. (MonadUnion m, Mergeable b) => m a -> m b -> m b
mrgMzero :: forall m a. (MonadUnion m, Mergeable a, MonadPlus m) => m a
mrgMplus :: forall m a. (MonadUnion m, Mergeable a, MonadPlus m) => m a -> m a -> m a
mrgFmap :: (MonadUnion f, Mergeable b, Functor f) => (a -> b) -> f a -> f b
