{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.Lib.Control.Monad.Trans
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Lib.Control.Monad.Trans
  ( -- * mrg* variants for operations in "Control.Monad.Trans"
    mrgLift,
  )
where

import Control.Monad.Trans
import Grisette.Core.Control.Monad.Union
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Data.Class.SimpleMergeable

-- | 'lift' with 'GMergingStrategy' knowledge propagation.
mrgLift ::
  forall bool t m a.
  (GMonadUnion bool (t m), MonadTrans t, Monad m, GMergeable bool a) =>
  m a ->
  t m a
mrgLift v = merge $ lift v
{-# INLINE mrgLift #-}
