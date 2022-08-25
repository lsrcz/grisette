{-# LANGUAGE FlexibleContexts #-}

module Pizza.TestUtils.SEq where

import Pizza.Core.Data.Class.Bool
import Pizza.TestUtils.SBool
import Test.Hspec

concreteSEqOkProp :: (HasCallStack, SEq SBool a, Eq a) => (a, a) -> Expectation
concreteSEqOkProp (i, j) = do
  i ==~ j `shouldBe` CBool (i == j)
  i /=~ j `shouldBe` CBool (i /= j)
