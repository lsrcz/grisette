{-# LANGUAGE FlexibleContexts #-}

module Grisette.TestUtils.SEq where

import GHC.Stack
import Grisette.Core.Data.Class.Bool
import Grisette.TestUtils.Assertions
import Grisette.TestUtils.SBool

concreteSEqOkProp :: (HasCallStack, GSEq SBool a, Eq a) => (a, a) -> Assertion
concreteSEqOkProp (i, j) = do
  i `gsymeq` j @=? CBool (i == j)
  i `gsymne` j @=? CBool (i /= j)
