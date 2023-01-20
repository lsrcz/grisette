{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grisette.Core.Control.Monad.UnionM (UnionM (..)) where

import Data.IORef
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Core.Data.Union

data UnionM a where
  -- | 'UnionM' with no 'Mergeable' knowledge.
  UAny ::
    -- | (Possibly) cached merging result.
    IORef (Either (Union a) (UnionM a)) ->
    -- | Original 'Union'.
    Union a ->
    UnionM a
  -- | 'UnionM' with 'Mergeable' knowledge.
  UMrg ::
    -- | Cached merging strategy.
    MergingStrategy a ->
    -- | Merged Union
    Union a ->
    UnionM a

instance UnionLike UnionM

instance Functor UnionM

instance Applicative UnionM

instance Monad UnionM