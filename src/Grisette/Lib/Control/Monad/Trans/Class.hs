{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.Lib.Control.Monad.Trans.Class
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Lib.Control.Monad.Trans.Class
  ( -- * mrg* variants for operations in "Control.Monad.Trans.Class"
    mrgLift,
  )
where

import Control.Monad.Trans (MonadTrans (lift))
import Grisette.Core.Control.Monad.Union (MonadUnion)
import Grisette.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Core.Data.Class.SimpleMergeable (merge)

-- | 'lift' with 'MergingStrategy' knowledge propagation.
mrgLift ::
  forall t m a.
  (MonadUnion (t m), MonadTrans t, Monad m, Mergeable a) =>
  m a ->
  t m a
mrgLift v = merge $ lift v
{-# INLINE mrgLift #-}
