{-# LANGUAGE FlexibleContexts #-}

module Pizza.TestUtils.SEq where

import GHC.Stack
import Pizza.Core.Data.Class.Bool
import Pizza.TestUtils.Assertions
import Pizza.TestUtils.SBool

concreteSEqOkProp :: (HasCallStack, SEq SBool a, Eq a) => (a, a) -> Assertion
concreteSEqOkProp (i, j) = do
  i ==~ j @=? CBool (i == j)
  i /=~ j @=? CBool (i /= j)
