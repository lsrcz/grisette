{-# LANGUAGE FlexibleContexts #-}

module Pizza.TestUtils.SEq where

import Pizza.Core.Data.Class.Bool
import Pizza.TestUtils.SBool
import Test.Tasty.HUnit

concreteSEqOkProp :: (HasCallStack, SEq SBool a, Eq a) => (a, a) -> Assertion
concreteSEqOkProp (i, j) = do
  i ==~ j @=? CBool (i == j)
  i /=~ j @=? CBool (i /= j)
