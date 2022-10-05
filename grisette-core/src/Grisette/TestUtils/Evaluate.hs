{-# LANGUAGE FlexibleContexts #-}

module Grisette.TestUtils.Evaluate (concreteEvaluateSymOkProp) where

import qualified Data.HashMap.Strict as M
import GHC.Stack
import Grisette.Core.Data.Class.Evaluate
import Grisette.TestUtils.Assertions
import Grisette.TestUtils.SBool

concreteEvaluateSymOkProp :: (HasCallStack, EvaluateSym (M.HashMap Symbol Bool) a, Show a, Eq a) => a -> Assertion
concreteEvaluateSymOkProp x = evaluateSym True (M.empty :: M.HashMap Symbol Bool) x @=? x
