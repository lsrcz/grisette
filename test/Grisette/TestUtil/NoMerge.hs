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
  ( Mergeable (rootStrategy),
    MergingStrategy (NoStrategy),
    SymBranching (mrgIfPropagatedStrategy),
    Union,
  )

data NoMerge = NoMerge
  deriving (Show, Eq, Generic)

instance Mergeable NoMerge where
  rootStrategy = NoStrategy

oneNotMerged :: Union Int
oneNotMerged = mrgIfPropagatedStrategy "a" (return 1) (return 1)

noMergeNotMerged :: Union NoMerge
noMergeNotMerged = mrgIfPropagatedStrategy "a" (return NoMerge) (return NoMerge)
