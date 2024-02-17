{-# LANGUAGE ConstraintKinds #-}

module Grisette.Core.Data.Class.TryMerge
  ( TryMerge (..),
    mrgPureWithStrategy,
    mrgPure,
    tryMerge,
    MonadTryMerge,
    mrgSingle,
    mrgSingleWithStrategy,
  )
where

import Control.Monad.Cont (ContT (ContT))
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Identity
  ( IdentityT (IdentityT),
  )
import qualified Control.Monad.RWS.Lazy as RWSLazy
import qualified Control.Monad.RWS.Strict as RWSStrict
import Control.Monad.Reader (ReaderT (ReaderT))
import qualified Control.Monad.State.Lazy as StateLazy
import qualified Control.Monad.State.Strict as StateStrict
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import Grisette.Core.Data.Class.Mergeable
  ( Mergeable (rootStrategy),
    Mergeable1 (liftRootStrategy),
    Mergeable2 (liftRootStrategy2),
    Mergeable3 (liftRootStrategy3),
    MergingStrategy,
  )

-- | A class for containers that may or may not be merged.
--
-- If the container is capable of multi-path execution, then the
-- `tryMergeWithStrategy` function should merge the paths according to the
-- supplied strategy.
--
-- If the container is not capable of multi-path execution, then the
-- `tryMergeWithStrategy` function should be equivalent to `id`.
--
-- Note that this will not necessarily do a recursive merge for the elements.
class TryMerge m where
  tryMergeWithStrategy :: MergingStrategy a -> m a -> m a

-- | Try to merge the container with the root strategy.
tryMerge :: (TryMerge m, Mergeable a) => m a -> m a
tryMerge = tryMergeWithStrategy rootStrategy
{-# INLINE tryMerge #-}

-- | Alias for 'mrgPureWithStrategy'.
mrgSingleWithStrategy ::
  (TryMerge m, Applicative m) =>
  MergingStrategy a ->
  a ->
  m a
mrgSingleWithStrategy = mrgPureWithStrategy
{-# INLINE mrgSingleWithStrategy #-}

-- | Alias for 'mrgPure'.
mrgSingle :: (TryMerge m, Applicative m, Mergeable a) => a -> m a
mrgSingle = mrgPure
{-# INLINE mrgSingle #-}

-- | Wrap a value in the applicative functor and capture the 'Mergeable'
-- knowledge.
--
-- >>> mrgPureWithStrategy rootStrategy "a" :: UnionM SymInteger
-- {a}
--
-- __Note:__ Be careful to call this directly from your code.
-- The supplied merge strategy should be consistent with the type's root merge
-- strategy, or some internal invariants would be broken and the program can
-- crash.
--
-- This function is to be called when the 'Mergeable' constraint can not be
-- resolved, e.g., the merge strategy for the contained type is given with
-- 'Mergeable1'. In other cases, 'mrgPure' is usually a better alternative.
mrgPureWithStrategy ::
  (TryMerge m, Applicative m) => MergingStrategy a -> a -> m a
mrgPureWithStrategy strategy = tryMergeWithStrategy strategy . pure
{-# INLINE mrgPureWithStrategy #-}

-- | Wrap a value in the applicative functor and propagate the type's root merge
-- strategy.
--
-- Equivalent to @'mrgPureWithStrategy' 'rootStrategy'@.
--
-- >>> mrgSingle "a" :: UnionM SymInteger
-- {a}
mrgPure :: (TryMerge m, Applicative m, Mergeable a) => a -> m a
mrgPure = mrgPureWithStrategy rootStrategy
{-# INLINE mrgPure #-}

instance (TryMerge m) => TryMerge (MaybeT m) where
  tryMergeWithStrategy strategy (MaybeT ma) =
    MaybeT $ tryMergeWithStrategy (liftRootStrategy strategy) ma
  {-# INLINE tryMergeWithStrategy #-}

instance (Mergeable e, TryMerge m) => TryMerge (ExceptT e m) where
  tryMergeWithStrategy strategy (ExceptT ma) =
    ExceptT $ tryMergeWithStrategy (liftRootStrategy strategy) ma
  {-# INLINE tryMergeWithStrategy #-}

instance (TryMerge m) => TryMerge (ReaderT r m) where
  tryMergeWithStrategy strategy (ReaderT f) = ReaderT $ \v -> tryMergeWithStrategy strategy $ f v
  {-# INLINE tryMergeWithStrategy #-}

instance
  (Mergeable s, TryMerge m) =>
  TryMerge (StateLazy.StateT s m)
  where
  tryMergeWithStrategy strategy (StateLazy.StateT f) =
    StateLazy.StateT $
      \s -> tryMergeWithStrategy (liftRootStrategy2 strategy rootStrategy) (f s)
  {-# INLINE tryMergeWithStrategy #-}

instance
  (Mergeable s, TryMerge m) =>
  TryMerge (StateStrict.StateT s m)
  where
  tryMergeWithStrategy strategy (StateStrict.StateT f) =
    StateStrict.StateT $
      \s -> tryMergeWithStrategy (liftRootStrategy2 strategy rootStrategy) (f s)
  {-# INLINE tryMergeWithStrategy #-}

instance
  (Monoid w, Mergeable w, TryMerge m) =>
  TryMerge (WriterLazy.WriterT w m)
  where
  tryMergeWithStrategy strategy (WriterLazy.WriterT f) =
    WriterLazy.WriterT $
      tryMergeWithStrategy (liftRootStrategy2 strategy rootStrategy) f
  {-# INLINE tryMergeWithStrategy #-}

instance
  (Monoid w, Mergeable w, TryMerge m) =>
  TryMerge (WriterStrict.WriterT w m)
  where
  tryMergeWithStrategy strategy (WriterStrict.WriterT f) =
    WriterStrict.WriterT $
      tryMergeWithStrategy (liftRootStrategy2 strategy rootStrategy) f
  {-# INLINE tryMergeWithStrategy #-}

instance
  (Monoid w, Mergeable w, Mergeable s, TryMerge m) =>
  TryMerge (RWSStrict.RWST r w s m)
  where
  tryMergeWithStrategy strategy (RWSStrict.RWST f) =
    RWSStrict.RWST $
      \r s ->
        tryMergeWithStrategy
          (liftRootStrategy3 strategy rootStrategy rootStrategy)
          (f r s)
  {-# INLINE tryMergeWithStrategy #-}

instance
  (Monoid w, Mergeable w, Mergeable s, TryMerge m) =>
  TryMerge (RWSLazy.RWST r w s m)
  where
  tryMergeWithStrategy strategy (RWSLazy.RWST f) =
    RWSLazy.RWST $
      \r s ->
        tryMergeWithStrategy
          (liftRootStrategy3 strategy rootStrategy rootStrategy)
          (f r s)
  {-# INLINE tryMergeWithStrategy #-}

instance (TryMerge m) => TryMerge (IdentityT m) where
  tryMergeWithStrategy strategy (IdentityT ma) = IdentityT $ tryMergeWithStrategy strategy ma
  {-# INLINE tryMergeWithStrategy #-}

instance (TryMerge m, Mergeable r) => TryMerge (ContT r m) where
  tryMergeWithStrategy _ (ContT ma) = ContT $ \c -> tryMergeWithStrategy rootStrategy (ma c)
  {-# INLINE tryMergeWithStrategy #-}

-- | Alias for a monad type that has 'TryMerge'.
type MonadTryMerge f = (TryMerge f, Monad f)
