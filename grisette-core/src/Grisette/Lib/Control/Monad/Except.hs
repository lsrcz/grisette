{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.Lib.Control.Monad.Except
-- Copyright   :   (c) Sirui Lu 2021-2022
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

import Control.Monad.Except
import Grisette.Core.Control.Monad.Union
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Data.Class.SimpleMergeable

-- | 'throwError' with 'GMergingStrategy' knowledge propagation.
mrgThrowError :: (SymBoolOp bool, MonadError e m, GMonadUnion bool m, GMergeable bool a) => e -> m a
mrgThrowError = merge . throwError
{-# INLINE mrgThrowError #-}

-- | 'catchError' with 'GMergingStrategy' knowledge propagation.
mrgCatchError ::
  (SymBoolOp bool, MonadError e m, GMonadUnion bool m, GMergeable bool a) =>
  m a ->
  (e -> m a) ->
  m a
mrgCatchError v handler = merge $ v `catchError` (merge . handler)
{-# INLINE mrgCatchError #-}
