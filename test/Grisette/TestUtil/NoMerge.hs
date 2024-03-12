{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Grisette.TestUtil.NoMerge (NoMerge (..), oneNotMerged, noMergeNotMerged) where

import GHC.Generics (Generic)
import Grisette.Core.Control.Monad.UnionM (UnionM, mergePropagatedIf)
import Grisette.Core.Data.Class.Mergeable
  ( Mergeable (rootStrategy),
    MergingStrategy (NoStrategy),
  )

data NoMerge = NoMerge
  deriving (Show, Eq, Generic)

instance Mergeable NoMerge where
  rootStrategy = NoStrategy

oneNotMerged :: UnionM Int
oneNotMerged = mergePropagatedIf "a" (return 1) (return 1)

noMergeNotMerged :: UnionM NoMerge
noMergeNotMerged = mergePropagatedIf "a" (return NoMerge) (return NoMerge)