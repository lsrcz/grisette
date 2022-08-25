{-# LANGUAGE FlexibleContexts #-}

module Pizza.TestUtils.ExtractSymbolics where

import qualified Data.HashSet as S
import Pizza.Core.Data.Class.ExtractSymbolics
import Pizza.TestUtils.SBool
import Test.Hspec

concreteExtractSymbolicsOkProp :: (HasCallStack, ExtractSymbolics (S.HashSet Symbol) a) => (a, a) -> Expectation
concreteExtractSymbolicsOkProp x = extractSymbolics x `shouldBe` (S.empty :: S.HashSet Symbol)
