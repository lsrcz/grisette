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
import Grisette.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Core.Data.Class.TryMerge (TryMerge, tryMerge)

-- | 'throwError' with 'MergingStrategy' knowledge propagation.
mrgThrowError :: (MonadError e m, TryMerge m, Mergeable a) => e -> m a
mrgThrowError = tryMerge . throwError
{-# INLINE mrgThrowError #-}

-- | 'catchError' with 'MergingStrategy' knowledge propagation.
mrgCatchError ::
  (MonadError e m, TryMerge m, Mergeable a) =>
  m a ->
  (e -> m a) ->
  m a
mrgCatchError v handler = tryMerge $ v `catchError` (tryMerge . handler)
{-# INLINE mrgCatchError #-}
