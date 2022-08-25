{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Pizza.TestUtils.Mergeable where

import Pizza.Core.Data.Class.Mergeable
import Pizza.TestUtils.SBool
import Test.Hspec

testMergeableSimpleEquivClass ::
  (HasCallStack, Mergeable SBool x, Show x, Eq x) => x -> [DynamicSortedIdx] -> [(SBool, x, x, x)] -> Expectation
testMergeableSimpleEquivClass x idxs cases = do
  let (idxsT, s) = resolveStrategy @SBool mergingStrategy x
  case s of
    SimpleStrategy m -> do
      idxsT `shouldBe` idxs
      go cases
      where
        go [] = return ()
        go ((c, t, f, r) : xs) = do
          fst (resolveStrategy @SBool mergingStrategy t) `shouldBe` idxs
          fst (resolveStrategy @SBool mergingStrategy f) `shouldBe` idxs
          fst (resolveStrategy @SBool mergingStrategy r) `shouldBe` idxs
          m c t f `shouldBe` r
          go xs
    _ -> expectationFailure $ "Bad strategy type for " ++ show x

testMergeableSimpleEquivClass' ::
  (HasCallStack, Mergeable SBool x, Show y, Eq y) => (x -> y) -> x -> [DynamicSortedIdx] -> [(SBool, x, x, x)] -> Expectation
testMergeableSimpleEquivClass' vis x idxs cases = do
  let (idxsT, s) = resolveStrategy @SBool mergingStrategy x
  case s of
    SimpleStrategy m -> do
      idxsT `shouldBe` idxs
      go cases
      where
        go [] = return ()
        go ((c, t, f, r) : xs) = do
          fst (resolveStrategy @SBool mergingStrategy t) `shouldBe` idxs
          fst (resolveStrategy @SBool mergingStrategy f) `shouldBe` idxs
          fst (resolveStrategy @SBool mergingStrategy r) `shouldBe` idxs
          vis (m c t f) `shouldBe` vis r
          go xs
    _ -> expectationFailure $ "Bad strategy type for " ++ show (vis x)
