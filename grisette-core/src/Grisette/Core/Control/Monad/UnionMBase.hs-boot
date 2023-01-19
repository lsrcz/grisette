{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grisette.Core.Control.Monad.UnionMBase (UnionMBase (..)) where

import Data.IORef
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Core.Data.UnionBase

data UnionMBase bool a where
  -- | 'UnionMBase' with no 'Mergeable' knowledge.
  UAny ::
    -- | (Possibly) cached merging result.
    IORef (Either (UnionBase bool a) (UnionMBase bool a)) ->
    -- | Original 'UnionBase'.
    UnionBase bool a ->
    UnionMBase bool a
  -- | 'UnionMBase' with 'Mergeable' knowledge.
  UMrg ::
    -- | Cached merging strategy.
    GMergingStrategy bool a ->
    -- | Merged UnionBase
    UnionBase bool a ->
    UnionMBase bool a

instance SymBoolOp bool => GUnionLike bool (UnionMBase bool)

instance (SymBoolOp bool) => Functor (UnionMBase bool)

instance (SymBoolOp bool) => Applicative (UnionMBase bool)

instance (SymBoolOp bool) => Monad (UnionMBase bool)
