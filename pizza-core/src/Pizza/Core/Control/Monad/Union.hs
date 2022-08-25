{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Pizza.Core.Control.Monad.Union
  ( MonadUnion,
  )
where

import Pizza.Core.Data.Class.SimpleMergeable

-- $setup
-- >>> import Pizza.Core
-- >>> import Pizza.IR.SymPrim

-- | Class for monads that support union-like operations and 'Mergeable' knowledge propagation.
type MonadUnion bool u = (UnionLike bool u, Monad u)
