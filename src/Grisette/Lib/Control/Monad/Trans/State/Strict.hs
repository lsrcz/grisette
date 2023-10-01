{-# LANGUAGE Safe #-}

-- |
-- Module      :   Grisette.Lib.Control.Monad.Trans.State.Strict
-- Copyright   :   (c) Sirui Lu 2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Lib.Control.Monad.Trans.State.Strict
  ( -- * mrg* variants for operations in "Control.Monad.Trans.State.Strict"
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

import Control.Monad.Trans.State.Strict
  ( StateT (StateT),
    runStateT,
  )
import Grisette.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Core.Data.Class.SimpleMergeable (UnionLike, merge)
import Grisette.Lib.Control.Monad (mrgReturn)

-- | 'Control.Monad.Trans.State.Strict.state' with 'MergingStrategy' knowledge
-- propagation.
mrgState ::
  (Monad m, UnionLike m, Mergeable s, Mergeable a) =>
  (s -> (a, s)) ->
  StateT s m a
mrgState f = StateT (mrgReturn . f)
{-# INLINE mrgState #-}

-- | 'Control.Monad.Trans.State.Strict.runStateT' with 'MergingStrategy'
-- knowledge propagation.
mrgRunStateT ::
  (Monad m, UnionLike m, Mergeable s, Mergeable a) =>
  StateT s m a ->
  s ->
  m (a, s)
mrgRunStateT m s = runStateT m s >>= mrgReturn
{-# INLINE mrgRunStateT #-}

-- | 'Control.Monad.Trans.State.Strict.evalStateT' with 'MergingStrategy'
-- knowledge propagation.
mrgEvalStateT ::
  (Monad m, UnionLike m, Mergeable a) =>
  StateT s m a ->
  s ->
  m a
mrgEvalStateT m s = do
  (a, _) <- runStateT m s
  mrgReturn a
{-# INLINE mrgEvalStateT #-}

-- | 'Control.Monad.Trans.State.Strict.execStateT' with 'MergingStrategy'
-- knowledge propagation.
mrgExecStateT ::
  (Monad m, UnionLike m, Mergeable s) =>
  StateT s m a ->
  s ->
  m s
mrgExecStateT m s = do
  (_, s') <- runStateT m s
  mrgReturn s'
{-# INLINE mrgExecStateT #-}

-- | 'Control.Monad.Trans.State.Strict.mapStateT' with 'MergingStrategy'
-- knowledge propagation.
mrgMapStateT ::
  (UnionLike n, Mergeable b, Mergeable s) =>
  (m (a, s) -> n (b, s)) ->
  StateT s m a ->
  StateT s n b
mrgMapStateT f m = StateT $ merge . f . runStateT m
{-# INLINE mrgMapStateT #-}

-- | 'Control.Monad.Trans.State.Strict.withStateT' with 'MergingStrategy'
-- knowledge propagation.
mrgWithStateT ::
  (UnionLike m, Mergeable s, Mergeable a) =>
  (s -> s) ->
  StateT s m a ->
  StateT s m a
mrgWithStateT f m = StateT $ merge . runStateT m . f
{-# INLINE mrgWithStateT #-}

-- | 'Control.Monad.Trans.State.Strict.get' with 'MergingStrategy' knowledge
-- propagation.
mrgGet :: (Monad m, UnionLike m, Mergeable s) => StateT s m s
mrgGet = mrgState (\s -> (s, s))
{-# INLINE mrgGet #-}

-- | 'Control.Monad.Trans.State.Strict.put' with 'MergingStrategy' knowledge
-- propagation.
mrgPut :: (Monad m, UnionLike m, Mergeable s) => s -> StateT s m ()
mrgPut s = mrgState (const ((), s))
{-# INLINE mrgPut #-}

-- | 'Control.Monad.Trans.State.Strict.modify' with 'MergingStrategy' knowledge
-- propagation.
mrgModify :: (Monad m, UnionLike m, Mergeable s) => (s -> s) -> StateT s m ()
mrgModify f = mrgState (\s -> ((), f s))
{-# INLINE mrgModify #-}

-- | 'Control.Monad.Trans.State.Strict.modify'' with 'MergingStrategy' knowledge
-- propagation.
mrgModify' :: (Monad m, UnionLike m, Mergeable s) => (s -> s) -> StateT s m ()
mrgModify' f = do
  s <- mrgGet
  mrgPut $! f s
{-# INLINE mrgModify' #-}

-- | 'Control.Monad.Trans.State.Strict.gets' with 'MergingStrategy' knowledge
-- propagation.
mrgGets ::
  (Monad m, UnionLike m, Mergeable s, Mergeable a) =>
  (s -> a) ->
  StateT s m a
mrgGets f = mrgState $ \s -> (f s, s)
{-# INLINE mrgGets #-}
