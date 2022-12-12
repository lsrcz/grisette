{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :   Grisette.TestUtils.Mergeable
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.TestUtils.Mergeable where

import GHC.Stack
import Grisette.Core.Data.Class.Mergeable
import Grisette.TestUtils.Assertions
import Grisette.TestUtils.SBool

testMergeableSimpleEquivClass ::
  (HasCallStack, GMergeable SBool x, Show x, Eq x) => x -> [DynamicSortedIdx] -> [(SBool, x, x, x)] -> Assertion
testMergeableSimpleEquivClass x idxs cases = do
  let (idxsT, s) = gresolveStrategy @SBool gmergingStrategy x
  case s of
    SimpleStrategy m -> do
      idxsT @=? idxs
      go cases
      where
        go [] = return ()
        go ((c, t, f, r) : xs) = do
          fst (gresolveStrategy @SBool gmergingStrategy t) @=? idxs
          fst (gresolveStrategy @SBool gmergingStrategy f) @=? idxs
          fst (gresolveStrategy @SBool gmergingStrategy r) @=? idxs
          m c t f @=? r
          go xs
    _ -> assertFailure $ "Bad strategy type for " ++ show x

testMergeableSimpleEquivClass' ::
  (HasCallStack, GMergeable SBool x, Show y, Eq y) => (x -> y) -> x -> [DynamicSortedIdx] -> [(SBool, x, x, x)] -> Assertion
testMergeableSimpleEquivClass' vis x idxs cases = do
  let (idxsT, s) = gresolveStrategy @SBool gmergingStrategy x
  case s of
    SimpleStrategy m -> do
      idxsT @=? idxs
      go cases
      where
        go [] = return ()
        go ((c, t, f, r) : xs) = do
          fst (gresolveStrategy @SBool gmergingStrategy t) @=? idxs
          fst (gresolveStrategy @SBool gmergingStrategy f) @=? idxs
          fst (gresolveStrategy @SBool gmergingStrategy r) @=? idxs
          vis (m c t f) @=? vis r
          go xs
    _ -> assertFailure $ "Bad strategy type for " ++ show (vis x)
