{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.Lib.Control.Monad.Except
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Lib.Control.Monad.Except
  ( -- * mrg* variants for operations in "Control.Monad.Except"
    mrgThrowError,
    mrgCatchError,
  )
where

import Control.Monad.Except (MonadError (catchError, throwError))
import Grisette.Core.Control.Monad.Union (MonadUnion)
import Grisette.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Core.Data.Class.SimpleMergeable (merge)

-- | 'throwError' with 'MergingStrategy' knowledge propagation.
mrgThrowError :: (MonadError e m, MonadUnion m, Mergeable a) => e -> m a
mrgThrowError = merge . throwError
{-# INLINE mrgThrowError #-}

-- | 'catchError' with 'MergingStrategy' knowledge propagation.
mrgCatchError ::
  (MonadError e m, MonadUnion m, Mergeable a) =>
  m a ->
  (e -> m a) ->
  m a
mrgCatchError v handler = merge $ v `catchError` (merge . handler)
{-# INLINE mrgCatchError #-}
