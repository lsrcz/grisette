{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.Lib.Control.Monad.Except
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Lib.Control.Monad.Except
  ( -- * mrg* variants for operations in "Control.Monad.Except"
    mrgThrowError,
    mrgCatchError,
    mrgLiftEither,
    mrgTryError,
    mrgWithError,
    mrgHandleError,
    mrgMapError,
    mrgModifyError,
  )
where

import Control.Monad.Except (ExceptT, MonadError (catchError, throwError))
import Grisette.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Core.Data.Class.TryMerge (TryMerge, tryMerge)
import Grisette.Lib.Control.Monad (mrgReturn)
import Grisette.Lib.Control.Monad.Trans.Except (mrgRunExceptT)
import Grisette.Lib.Data.Functor (mrgFmap)

-- | 'Control.Monad.Except.throwError' with 'MergingStrategy' knowledge
-- propagation.
mrgThrowError :: (MonadError e m, TryMerge m, Mergeable a) => e -> m a
mrgThrowError = tryMerge . throwError
{-# INLINE mrgThrowError #-}

-- | 'Control.Monad.Except.catchError' with 'MergingStrategy' knowledge
-- propagation.
mrgCatchError ::
  (MonadError e m, TryMerge m, Mergeable a) =>
  m a ->
  (e -> m a) ->
  m a
mrgCatchError v handler = tryMerge $ v `catchError` (tryMerge . handler)
{-# INLINE mrgCatchError #-}

-- | 'Control.Monad.Except.liftEither' with 'MergingStrategy' knowledge
-- propagation.
mrgLiftEither ::
  (MonadError e m, TryMerge m, Mergeable a, Mergeable e) => Either e a -> m a
mrgLiftEither = either mrgThrowError mrgReturn
{-# INLINE mrgLiftEither #-}

-- | 'Control.Monad.Except.tryError' with 'MergingStrategy' knowledge
-- propagation.
mrgTryError ::
  (MonadError e m, TryMerge m, Mergeable a, Mergeable e) =>
  m a ->
  m (Either e a)
mrgTryError action = (mrgFmap Right action) `mrgCatchError` (mrgReturn . Left)
{-# INLINE mrgTryError #-}

-- | 'Control.Monad.Except.withError' with 'MergingStrategy' knowledge
-- propagation.
mrgWithError ::
  (MonadError e m, TryMerge m, Mergeable a, Mergeable e) =>
  (e -> e) ->
  m a ->
  m a
mrgWithError f action =
  tryMerge $ mrgTryError action >>= either (mrgThrowError . f) mrgReturn
{-# INLINE mrgWithError #-}

-- | 'Control.Monad.Except.handleError' with 'MergingStrategy' knowledge
-- propagation.
mrgHandleError ::
  (MonadError e m, TryMerge m, Mergeable a, Mergeable e) =>
  (e -> m a) ->
  m a ->
  m a
mrgHandleError = flip mrgCatchError
{-# INLINE mrgHandleError #-}

-- | 'Control.Monad.Except.mapError' with 'MergingStrategy' knowledge
-- propagation.
mrgMapError ::
  ( MonadError e m,
    TryMerge m,
    MonadError e' n,
    TryMerge n,
    Mergeable a,
    Mergeable b,
    Mergeable e,
    Mergeable e'
  ) =>
  (m (Either e a) -> n (Either e' b)) ->
  m a ->
  n b
mrgMapError f action = tryMerge (f (mrgTryError action)) >>= mrgLiftEither
{-# INLINE mrgMapError #-}

-- | 'Control.Monad.Except.modifyError' with 'MergingStrategy' knowledge
-- propagation.
mrgModifyError ::
  ( MonadError e' m,
    TryMerge m,
    Mergeable a,
    Mergeable e,
    Mergeable e
  ) =>
  (e -> e') ->
  ExceptT e m a ->
  m a
mrgModifyError f m =
  tryMerge $ mrgRunExceptT m >>= either (mrgThrowError . f) mrgReturn
{-# INLINE mrgModifyError #-}
