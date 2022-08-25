module Pizza.Lib.Control.Monad.Trans.Cont
  ( mrgRunContT,
    mrgEvalContT,
    mrgResetT,
  )
where

import Control.Monad.Cont
import Pizza.Core.Data.Class.Bool
import Pizza.Core.Data.Class.Mergeable
import Pizza.Core.Data.Class.SimpleMergeable
import Pizza.Lib.Control.Monad

mrgRunContT :: (SymBoolOp bool, UnionLike bool m, Mergeable bool r) => ContT r m a -> (a -> m r) -> m r
mrgRunContT c = merge . runContT c
{-# INLINE mrgRunContT #-}

mrgEvalContT :: (SymBoolOp bool, UnionLike bool m, Mergeable bool r, Monad m) => ContT r m r -> m r
mrgEvalContT c = runContT c mrgReturn
{-# INLINE mrgEvalContT #-}

mrgResetT :: (SymBoolOp bool, UnionLike bool m, Mergeable bool r, Monad m) => Monad m => ContT r m r -> ContT r' m r
mrgResetT = lift . mrgEvalContT
{-# INLINE mrgResetT #-}
