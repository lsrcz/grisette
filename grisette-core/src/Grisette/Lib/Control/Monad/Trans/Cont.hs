{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.Lib.Control.Monad.Trans.Cont
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only

module Grisette.Lib.Control.Monad.Trans.Cont
  ( mrgRunContT,
    mrgEvalContT,
    mrgResetT,
  )
where

import Control.Monad.Cont
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Lib.Control.Monad

-- | 'Control.Monad.Cont.runContT' with 'GMergingStrategy' knowledge propagation
mrgRunContT :: (SymBoolOp bool, GUnionLike bool m, GMergeable bool r) => ContT r m a -> (a -> m r) -> m r
mrgRunContT c = merge . runContT c
{-# INLINE mrgRunContT #-}

-- | 'Control.Monad.Cont.evalContT' with 'GMergingStrategy' knowledge propagation
mrgEvalContT :: (SymBoolOp bool, GUnionLike bool m, GMergeable bool r, Monad m) => ContT r m r -> m r
mrgEvalContT c = runContT c mrgReturn
{-# INLINE mrgEvalContT #-}

-- | 'Control.Monad.Cont.resetT' with 'GMergingStrategy' knowledge propagation
mrgResetT :: (SymBoolOp bool, GUnionLike bool m, GMergeable bool r, Monad m) => Monad m => ContT r m r -> ContT r' m r
mrgResetT = lift . mrgEvalContT
{-# INLINE mrgResetT #-}
