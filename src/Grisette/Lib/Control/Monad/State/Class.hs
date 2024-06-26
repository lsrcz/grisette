{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.Lib.Control.Monad.State.Class
-- Copyright   :   (c) Sirui Lu 2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Lib.Control.Monad.State.Class
  ( -- * mrg* variants for operations in "Control.Monad.State.Class"
    mrgGet,
    mrgPut,
    mrgState,
    mrgModify,
    mrgModify',
    mrgGets,
  )
where

import Control.Monad.State.Class (MonadState (get, put))
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.TryMerge (TryMerge, tryMerge)
import Grisette.Lib.Control.Monad (mrgReturn)

-- | 'Control.Monad.State.Class.get' with 'Grisette.Core.MergingStrategy'
-- knowledge propagation.
mrgGet :: (MonadState s m, TryMerge m, Mergeable s) => m s
mrgGet = tryMerge get
{-# INLINE mrgGet #-}

-- | 'Control.Monad.State.Class.put' with 'Grisette.Core.MergingStrategy'
-- knowledge propagation.
mrgPut :: (MonadState s m, TryMerge m) => s -> m ()
mrgPut = tryMerge . put
{-# INLINE mrgPut #-}

-- | 'Control.Monad.State.Class.state' with 'Grisette.Core.MergingStrategy'
-- knowledge propagation.
mrgState ::
  (MonadState s m, TryMerge m, Mergeable s, Mergeable a) =>
  (s -> (a, s)) ->
  m a
mrgState f = tryMerge $ do
  s <- mrgGet
  let ~(a, s') = f s
  mrgPut s'
  mrgReturn a

-- | 'Control.Monad.State.Class.modify' with 'Grisette.Core.MergingStrategy'
-- knowledge propagation.
mrgModify :: (MonadState s m, TryMerge m, Mergeable s) => (s -> s) -> m ()
mrgModify f = mrgState (\s -> ((), f s))
{-# INLINE mrgModify #-}

-- | 'Control.Monad.State.Class.modify'' with 'Grisette.Core.MergingStrategy'
-- knowledge propagation.
mrgModify' :: (MonadState s m, TryMerge m, Mergeable s) => (s -> s) -> m ()
mrgModify' f = do
  s' <- mrgGet
  mrgPut $! f s'
{-# INLINE mrgModify' #-}

-- | 'Control.Monad.State.Class.gets' with 'Grisette.Core.MergingStrategy'
-- knowledge propagation.
mrgGets ::
  (MonadState s m, TryMerge m, Mergeable s, Mergeable a) =>
  (s -> a) ->
  m a
mrgGets f = do
  s <- mrgGet
  mrgReturn $ f s
{-# INLINE mrgGets #-}
