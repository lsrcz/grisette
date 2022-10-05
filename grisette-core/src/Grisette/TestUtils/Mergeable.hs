{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.TestUtils.Mergeable where

import GHC.Stack
import Grisette.Core.Data.Class.Mergeable
import Grisette.TestUtils.Assertions
import Grisette.TestUtils.SBool

testMergeableSimpleEquivClass ::
  (HasCallStack, Mergeable SBool x, Show x, Eq x) => x -> [DynamicSortedIdx] -> [(SBool, x, x, x)] -> Assertion
testMergeableSimpleEquivClass x idxs cases = do
  let (idxsT, s) = resolveStrategy @SBool mergingStrategy x
  case s of
    SimpleStrategy m -> do
      idxsT @=? idxs
      go cases
      where
        go [] = return ()
        go ((c, t, f, r) : xs) = do
          fst (resolveStrategy @SBool mergingStrategy t) @=? idxs
          fst (resolveStrategy @SBool mergingStrategy f) @=? idxs
          fst (resolveStrategy @SBool mergingStrategy r) @=? idxs
          m c t f @=? r
          go xs
    _ -> assertFailure $ "Bad strategy type for " ++ show x

testMergeableSimpleEquivClass' ::
  (HasCallStack, Mergeable SBool x, Show y, Eq y) => (x -> y) -> x -> [DynamicSortedIdx] -> [(SBool, x, x, x)] -> Assertion
testMergeableSimpleEquivClass' vis x idxs cases = do
  let (idxsT, s) = resolveStrategy @SBool mergingStrategy x
  case s of
    SimpleStrategy m -> do
      idxsT @=? idxs
      go cases
      where
        go [] = return ()
        go ((c, t, f, r) : xs) = do
          fst (resolveStrategy @SBool mergingStrategy t) @=? idxs
          fst (resolveStrategy @SBool mergingStrategy f) @=? idxs
          fst (resolveStrategy @SBool mergingStrategy r) @=? idxs
          vis (m c t f) @=? vis r
          go xs
    _ -> assertFailure $ "Bad strategy type for " ++ show (vis x)
