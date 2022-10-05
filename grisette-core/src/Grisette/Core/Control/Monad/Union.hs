{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Core.Control.Monad.Union
  ( MonadUnion,
  )
where

import Grisette.Core.Data.Class.SimpleMergeable

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim

-- | Class for monads that support union-like operations and 'Mergeable' knowledge propagation.
type MonadUnion bool u = (UnionLike bool u, Monad u)
