{-# LANGUAGE FlexibleContexts #-}

module Grisette.TestUtils.SEq where

import GHC.Stack
import Grisette.Core.Data.Class.Bool
import Grisette.TestUtils.Assertions
import Grisette.TestUtils.SBool

concreteSEqOkProp :: (HasCallStack, SEq SBool a, Eq a) => (a, a) -> Assertion
concreteSEqOkProp (i, j) = do
  i ==~ j @=? CBool (i == j)
  i /=~ j @=? CBool (i /= j)
