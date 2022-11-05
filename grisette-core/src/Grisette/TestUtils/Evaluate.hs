{-# LANGUAGE FlexibleContexts #-}

module Grisette.TestUtils.Evaluate (concreteGEvaluateSymOkProp) where

import qualified Data.HashMap.Strict as M
import GHC.Stack
import Grisette.Core.Data.Class.Evaluate
import Grisette.TestUtils.Assertions
import Grisette.TestUtils.SBool

concreteGEvaluateSymOkProp :: (HasCallStack, GEvaluateSym (M.HashMap Symbol Bool) a, Show a, Eq a) => a -> Assertion
concreteGEvaluateSymOkProp x = gevaluateSym True (M.empty :: M.HashMap Symbol Bool) x @=? x
