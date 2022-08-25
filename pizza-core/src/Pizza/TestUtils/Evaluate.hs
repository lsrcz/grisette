{-# LANGUAGE FlexibleContexts #-}

module Pizza.TestUtils.Evaluate (concreteEvaluateSymOkProp) where

import qualified Data.HashMap.Strict as M
import Pizza.Core.Data.Class.Evaluate
import Pizza.TestUtils.SBool
import Test.Hspec

concreteEvaluateSymOkProp :: (HasCallStack, EvaluateSym (M.HashMap Symbol Bool) a, Show a, Eq a) => a -> Expectation
concreteEvaluateSymOkProp x = evaluateSym True (M.empty :: M.HashMap Symbol Bool) x `shouldBe` x
