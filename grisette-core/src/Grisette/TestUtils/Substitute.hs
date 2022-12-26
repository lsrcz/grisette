{-# LANGUAGE FlexibleContexts #-}

module Grisette.TestUtils.Substitute (concreteGSubstituteSymOkProp) where

import qualified Data.HashMap.Strict as M
import GHC.Stack
import Grisette.Core.Data.Class.Substitute
import Grisette.TestUtils.Assertions
import Grisette.TestUtils.SBool

concreteGSubstituteSymOkProp :: (HasCallStack, GSubstituteSym TSymbol TSBool a, Show a, Eq a) => a -> Assertion
concreteGSubstituteSymOkProp x = gsubstituteSym (TSymbol $ SSymbol "a") (TSBool $ SSBool "b") x @=? x
