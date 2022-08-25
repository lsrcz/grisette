{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pizza.Lib.Control.Monad.Trans
  ( mrgLift,
  )
where

import Control.Monad.Trans
import Pizza.Core.Control.Monad.Union
import Pizza.Core.Data.Class.Mergeable
import Pizza.Core.Data.Class.SimpleMergeable

-- | 'lift' with 'Mergeable' knowledge propagation.
mrgLift ::
  forall bool t m a.
  (MonadUnion bool (t m), MonadTrans t, Monad m, Mergeable bool a) =>
  m a ->
  t m a
mrgLift v = merge $ lift v
{-# INLINE mrgLift #-}
