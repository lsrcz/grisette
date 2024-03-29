{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Core.Control.Monad.Union
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Control.Monad.Union
  ( -- * MonadUnion
    MonadUnion,
  )
where

import Grisette.Core.Data.Class.SimpleMergeable (UnionMergeable1)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim

-- | Class for monads that support union-like operations and
-- 'Grisette.Core.Data.Class.Mergeable' knowledge propagation.
type MonadUnion u = (UnionMergeable1 u, Monad u)
