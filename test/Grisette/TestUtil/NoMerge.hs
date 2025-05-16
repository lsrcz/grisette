{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Grisette.TestUtil.NoMerge
  ( NoMerge (..),
    oneNotMerged,
    noMergeNotMerged,
  )
where

import GHC.Generics (Generic)
import Grisette
  ( AsKey1,
    Mergeable (rootStrategy),
    MergingStrategy (NoStrategy),
    SymBranching (mrgIfPropagatedStrategy),
    Union,
  )

data NoMerge = NoMerge
  deriving (Show, Eq, Generic)

instance Mergeable NoMerge where
  rootStrategy = NoStrategy

oneNotMerged :: AsKey1 Union Int
oneNotMerged = mrgIfPropagatedStrategy "a" (return 1) (return 1)

noMergeNotMerged :: AsKey1 Union NoMerge
noMergeNotMerged = mrgIfPropagatedStrategy "a" (return NoMerge) (return NoMerge)
