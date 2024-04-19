{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}

module Grisette.Lib.Control.Monad
  ( mrgReturnWithStrategy,
    mrgBindWithStrategy,
    mrgReturn,
    (.>>=),
    mrgFoldM,
    (.>>),
    mrgMzero,
    mrgMplus,
  )
where

import Control.Monad (MonadPlus)
import Grisette.Internal.Core.Data.Class.Mergeable
  ( Mergeable,
    MergingStrategy,
  )
import Grisette.Internal.Core.Data.Class.TryMerge (MonadTryMerge)

mrgReturnWithStrategy :: (MonadTryMerge u) => MergingStrategy a -> a -> u a
mrgBindWithStrategy :: (MonadTryMerge u) => MergingStrategy a -> MergingStrategy b -> u a -> (a -> u b) -> u b
mrgReturn :: (MonadTryMerge u, Mergeable a) => a -> u a
(.>>=) :: (MonadTryMerge u, Mergeable a, Mergeable b) => u a -> (a -> u b) -> u b
mrgFoldM :: (MonadTryMerge m, Mergeable b, Foldable t) => (b -> a -> m b) -> b -> t a -> m b
(.>>) :: (MonadTryMerge m, Mergeable a, Mergeable b) => m a -> m b -> m b
mrgMzero :: forall m a. (MonadTryMerge m, Mergeable a, MonadPlus m) => m a
mrgMplus :: forall m a. (MonadTryMerge m, Mergeable a, MonadPlus m) => m a -> m a -> m a
