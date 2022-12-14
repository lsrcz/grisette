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

mrgRunContT :: (SymBoolOp bool, GUnionLike bool m, GMergeable bool r) => ContT r m a -> (a -> m r) -> m r
mrgRunContT c = merge . runContT c
{-# INLINE mrgRunContT #-}

mrgEvalContT :: (SymBoolOp bool, GUnionLike bool m, GMergeable bool r, Monad m) => ContT r m r -> m r
mrgEvalContT c = runContT c mrgReturn
{-# INLINE mrgEvalContT #-}

mrgResetT :: (SymBoolOp bool, GUnionLike bool m, GMergeable bool r, Monad m) => Monad m => ContT r m r -> ContT r' m r
mrgResetT = lift . mrgEvalContT
{-# INLINE mrgResetT #-}
