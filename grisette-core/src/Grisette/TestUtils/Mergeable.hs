{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.TestUtils.Mergeable where

import GHC.Stack
import Grisette.Core.Data.Class.Mergeable
import Grisette.TestUtils.Assertions
import Grisette.TestUtils.SBool

testMergeableSimpleEquivClass ::
  (HasCallStack, GMergeable SBool x, Show x, Eq x) => x -> [DynamicSortedIdx] -> [(SBool, x, x, x)] -> Assertion
testMergeableSimpleEquivClass x idxs cases = do
  let (idxsT, s) = gresolveStrategy @SBool grootStrategy x
  case s of
    SimpleStrategy m -> do
      idxsT @=? idxs
      go cases
      where
        go [] = return ()
        go ((c, t, f, r) : xs) = do
          fst (gresolveStrategy @SBool grootStrategy t) @=? idxs
          fst (gresolveStrategy @SBool grootStrategy f) @=? idxs
          fst (gresolveStrategy @SBool grootStrategy r) @=? idxs
          m c t f @=? r
          go xs
    _ -> assertFailure $ "Bad strategy type for " ++ show x

testMergeableSimpleEquivClass' ::
  (HasCallStack, GMergeable SBool x, Show y, Eq y) => (x -> y) -> x -> [DynamicSortedIdx] -> [(SBool, x, x, x)] -> Assertion
testMergeableSimpleEquivClass' vis x idxs cases = do
  let (idxsT, s) = gresolveStrategy @SBool grootStrategy x
  case s of
    SimpleStrategy m -> do
      idxsT @=? idxs
      go cases
      where
        go [] = return ()
        go ((c, t, f, r) : xs) = do
          fst (gresolveStrategy @SBool grootStrategy t) @=? idxs
          fst (gresolveStrategy @SBool grootStrategy f) @=? idxs
          fst (gresolveStrategy @SBool grootStrategy r) @=? idxs
          vis (m c t f) @=? vis r
          go xs
    _ -> assertFailure $ "Bad strategy type for " ++ show (vis x)
