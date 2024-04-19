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

import Control.Monad.Cont (ContT (runContT))
import Control.Monad.Trans.Class (lift)
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.TryMerge
  ( TryMerge,
    tryMerge,
  )
import Grisette.Lib.Control.Monad (mrgReturn)

-- | 'Control.Monad.Cont.runContT' with 'MergingStrategy' knowledge propagation
mrgRunContT :: (TryMerge m, Mergeable r) => ContT r m a -> (a -> m r) -> m r
mrgRunContT c = tryMerge . runContT c
{-# INLINE mrgRunContT #-}

-- | 'Control.Monad.Cont.evalContT' with 'MergingStrategy' knowledge propagation
mrgEvalContT :: (TryMerge m, Mergeable r, Monad m) => ContT r m r -> m r
mrgEvalContT c = runContT c mrgReturn
{-# INLINE mrgEvalContT #-}

-- | 'Control.Monad.Cont.resetT' with 'MergingStrategy' knowledge propagation
mrgResetT ::
  (TryMerge m, Mergeable r, Monad m) =>
  ContT r m r ->
  ContT r' m r
mrgResetT = lift . mrgEvalContT
{-# INLINE mrgResetT #-}
