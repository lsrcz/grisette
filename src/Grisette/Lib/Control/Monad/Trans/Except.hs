module Grisette.Lib.Control.Monad.Trans.Except
  ( mrgExcept,
    mrgRunExceptT,
    mrgWithExceptT,
    mrgThrowE,
    mrgCatchE,
  )
where

import Control.Monad.Trans.Except
  ( ExceptT,
    catchE,
    except,
    runExceptT,
    throwE,
    withExceptT,
  )
import Grisette.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Core.Data.Class.TryMerge (MonadTryMerge, tryMerge)

-- | 'Control.Monad.Trans.Except.except' with 'MergingStrategy' knowledge
-- propagation.
mrgExcept ::
  (MonadTryMerge m, Mergeable e, Mergeable a) => Either e a -> ExceptT e m a
mrgExcept = tryMerge . except
{-# INLINE mrgExcept #-}

-- | 'Control.Monad.Trans.Except.runExceptT' with 'MergingStrategy' knowledge
-- propagation.
mrgRunExceptT ::
  (MonadTryMerge m, Mergeable e, Mergeable a) => ExceptT e m a -> m (Either e a)
mrgRunExceptT = tryMerge . runExceptT
{-# INLINE mrgRunExceptT #-}

-- | 'Control.Monad.Trans.Except.withExceptT' with 'MergingStrategy' knowledge
-- propagation.
mrgWithExceptT ::
  (MonadTryMerge m, Mergeable a, Mergeable e, Mergeable e') =>
  (e -> e') ->
  ExceptT e m a ->
  ExceptT e' m a
mrgWithExceptT f e = tryMerge $ withExceptT f (tryMerge e)
{-# INLINE mrgWithExceptT #-}

-- | 'Control.Monad.Trans.Except.throwE' with 'MergingStrategy' knowledge
-- propagation.
mrgThrowE :: (MonadTryMerge m, Mergeable e, Mergeable a) => e -> ExceptT e m a
mrgThrowE = tryMerge . throwE
{-# INLINE mrgThrowE #-}

-- | 'Control.Monad.Trans.Except.catchE' with 'MergingStrategy' knowledge
-- propagation.
mrgCatchE ::
  (MonadTryMerge m, Mergeable e, Mergeable a) =>
  ExceptT e m a ->
  (e -> ExceptT e m a) ->
  ExceptT e m a
mrgCatchE value handler =
  tryMerge $ catchE (tryMerge value) (tryMerge . handler)
{-# INLINE mrgCatchE #-}
