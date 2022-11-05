{-# LANGUAGE FlexibleContexts #-}

module Grisette.TestUtils.ExtractSymbolics where

import qualified Data.HashSet as S
import GHC.Stack
import Grisette.Core.Data.Class.ExtractSymbolics
import Grisette.TestUtils.Assertions
import Grisette.TestUtils.SBool

concreteGExtractSymbolicsOkProp :: (HasCallStack, GExtractSymbolics (S.HashSet Symbol) a) => (a, a) -> Assertion
concreteGExtractSymbolicsOkProp x = gextractSymbolics x @=? (S.empty :: S.HashSet Symbol)
