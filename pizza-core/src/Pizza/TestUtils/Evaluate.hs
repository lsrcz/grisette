{-# LANGUAGE FlexibleContexts #-}

module Pizza.TestUtils.Evaluate (concreteEvaluateSymOkProp) where

import qualified Data.HashMap.Strict as M
import Pizza.Core.Data.Class.Evaluate
import Pizza.TestUtils.SBool
import Test.Tasty.HUnit

concreteEvaluateSymOkProp :: (HasCallStack, EvaluateSym (M.HashMap Symbol Bool) a, Show a, Eq a) => a -> Assertion
concreteEvaluateSymOkProp x = evaluateSym True (M.empty :: M.HashMap Symbol Bool) x @=? x
