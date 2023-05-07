{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.Lib.Control.Monad.Trans.Cont
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Lib.Control.Monad.Trans.Cont
  ( -- * mrg* variants for operations in "Control.Monad.Trans.Cont"
    mrgRunContT,
    mrgEvalContT,
    mrgResetT,
  )
where

import Control.Monad.Cont
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Lib.Control.Monad

-- | 'Control.Monad.Cont.runContT' with 'MergingStrategy' knowledge propagation
mrgRunContT :: (UnionLike m, Mergeable r) => ContT r m a -> (a -> m r) -> m r
mrgRunContT c = merge . runContT c
{-# INLINE mrgRunContT #-}

-- | 'Control.Monad.Cont.evalContT' with 'MergingStrategy' knowledge propagation
mrgEvalContT :: (UnionLike m, Mergeable r, Monad m) => ContT r m r -> m r
mrgEvalContT c = runContT c mrgReturn
{-# INLINE mrgEvalContT #-}

-- | 'Control.Monad.Cont.resetT' with 'MergingStrategy' knowledge propagation
mrgResetT :: (UnionLike m, Mergeable r, Monad m) => (Monad m) => ContT r m r -> ContT r' m r
mrgResetT = lift . mrgEvalContT
{-# INLINE mrgResetT #-}
