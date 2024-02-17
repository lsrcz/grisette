{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.Lib.Control.Monad.Trans.State.Lazy
-- Copyright   :   (c) Sirui Lu 2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Lib.Control.Monad.Trans.State.Lazy
  ( -- * mrg* variants for operations in "Control.Monad.Trans.State.Lazy"
    mrgState,
    mrgRunStateT,
    mrgEvalStateT,
    mrgExecStateT,
    mrgMapStateT,
    mrgWithStateT,
    mrgGet,
    mrgPut,
    mrgModify,
    mrgModify',
    mrgGets,
  )
where

import Control.Monad.Trans.State.Lazy
  ( StateT (StateT),
    runStateT,
  )
import Grisette.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Core.Data.Class.TryMerge (TryMerge, tryMerge)
import Grisette.Lib.Control.Monad (mrgReturn)

-- | 'Control.Monad.Trans.State.Lazy.state' with 'MergingStrategy' knowledge
-- propagation.
mrgState ::
  (Monad m, TryMerge m, Mergeable s, Mergeable a) =>
  (s -> (a, s)) ->
  StateT s m a
mrgState f = StateT (mrgReturn . f)
{-# INLINE mrgState #-}

-- | 'Control.Monad.Trans.State.Lazy.runStateT' with 'MergingStrategy' knowledge
-- propagation.
mrgRunStateT ::
  (Monad m, TryMerge m, Mergeable s, Mergeable a) =>
  StateT s m a ->
  s ->
  m (a, s)
mrgRunStateT m s = runStateT m s >>= mrgReturn
{-# INLINE mrgRunStateT #-}

-- | 'Control.Monad.Trans.State.Lazy.evalStateT' with 'MergingStrategy'
-- knowledge propagation.
mrgEvalStateT ::
  (Monad m, TryMerge m, Mergeable a) =>
  StateT s m a ->
  s ->
  m a
mrgEvalStateT m s = do
  ~(a, _) <- runStateT m s
  mrgReturn a
{-# INLINE mrgEvalStateT #-}

-- | 'Control.Monad.Trans.State.Lazy.execStateT' with 'MergingStrategy'
-- knowledge propagation.
mrgExecStateT ::
  (Monad m, TryMerge m, Mergeable s) =>
  StateT s m a ->
  s ->
  m s
mrgExecStateT m s = do
  ~(_, s') <- runStateT m s
  mrgReturn s'
{-# INLINE mrgExecStateT #-}

-- | 'Control.Monad.Trans.State.Lazy.mapStateT' with 'MergingStrategy' knowledge
-- propagation.
mrgMapStateT ::
  (TryMerge n, Mergeable b, Mergeable s) =>
  (m (a, s) -> n (b, s)) ->
  StateT s m a ->
  StateT s n b
mrgMapStateT f m = StateT $ tryMerge . f . runStateT m
{-# INLINE mrgMapStateT #-}

-- | 'Control.Monad.Trans.State.Lazy.withStateT' with 'MergingStrategy'
-- knowledge propagation.
mrgWithStateT ::
  (TryMerge m, Mergeable s, Mergeable a) =>
  (s -> s) ->
  StateT s m a ->
  StateT s m a
mrgWithStateT f m = StateT $ tryMerge . runStateT m . f
{-# INLINE mrgWithStateT #-}

-- | 'Control.Monad.Trans.State.Lazy.get' with 'MergingStrategy' knowledge
-- propagation.
mrgGet :: (Monad m, TryMerge m, Mergeable s) => StateT s m s
mrgGet = mrgState (\s -> (s, s))
{-# INLINE mrgGet #-}

-- | 'Control.Monad.Trans.State.Lazy.put' with 'MergingStrategy' knowledge
-- propagation.
mrgPut :: (Monad m, TryMerge m, Mergeable s) => s -> StateT s m ()
mrgPut s = mrgState (const ((), s))
{-# INLINE mrgPut #-}

-- | 'Control.Monad.Trans.State.Lazy.modify' with 'MergingStrategy' knowledge
-- propagation.
mrgModify :: (Monad m, TryMerge m, Mergeable s) => (s -> s) -> StateT s m ()
mrgModify f = mrgState (\s -> ((), f s))
{-# INLINE mrgModify #-}

-- | 'Control.Monad.Trans.State.Lazy.modify'' with 'MergingStrategy' knowledge
-- propagation.
mrgModify' :: (Monad m, TryMerge m, Mergeable s) => (s -> s) -> StateT s m ()
mrgModify' f = do
  s <- mrgGet
  mrgPut $! f s
{-# INLINE mrgModify' #-}

-- | 'Control.Monad.Trans.State.Lazy.gets' with 'MergingStrategy' knowledge
-- propagation.
mrgGets ::
  (Monad m, TryMerge m, Mergeable s, Mergeable a) =>
  (s -> a) ->
  StateT s m a
mrgGets f = mrgState $ \s -> (f s, s)
{-# INLINE mrgGets #-}
