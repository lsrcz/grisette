{-# LANGUAGE ScopedTypeVariables #-}

module Pizza.IR.SymPrim.Data.Prim.IntegerSpec where

import Pizza.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Pizza.IR.SymPrim.Data.Prim.InternedTerm.Term
import Pizza.IR.SymPrim.Data.Prim.PartialEval.Integer
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "DivI" $ do
    describe "DivI construction" $ do
      prop "DivI on concrete" $ \(i :: Integer, j :: Integer) -> do
        if j /= 0
          then pevalDivIntegerTerm (concTerm i) (concTerm j) `shouldBe` concTerm (i `div` j)
          else
            pevalDivIntegerTerm (concTerm i) (concTerm j)
              `shouldBe` divIntegerTerm (concTerm i) (concTerm j)
      it "divide by 1" $ do
        pevalDivIntegerTerm (ssymbTerm "a" :: Term Integer) (concTerm 1) `shouldBe` ssymbTerm "a"
      it "DivI symbolic" $ do
        pevalDivIntegerTerm (ssymbTerm "a" :: Term Integer) (ssymbTerm "b")
          `shouldBe` divIntegerTerm (ssymbTerm "a" :: Term Integer) (ssymbTerm "b" :: Term Integer)
  describe "ModI" $ do
    describe "ModI construction" $ do
      prop "ModI on concrete" $ \(i :: Integer, j :: Integer) -> do
        if j /= 0
          then pevalModIntegerTerm (concTerm i) (concTerm j) `shouldBe` concTerm (i `mod` j)
          else
            pevalModIntegerTerm (concTerm i) (concTerm j)
              `shouldBe` modIntegerTerm (concTerm i) (concTerm j)
      it "mod by 1" $ do
        pevalModIntegerTerm (ssymbTerm "a" :: Term Integer) (concTerm 1) `shouldBe` concTerm 0
      it "mod by -1" $ do
        pevalModIntegerTerm (ssymbTerm "a" :: Term Integer) (concTerm $ -1) `shouldBe` concTerm 0
      it "ModI symbolic" $ do
        pevalModIntegerTerm (ssymbTerm "a" :: Term Integer) (ssymbTerm "b")
          `shouldBe` modIntegerTerm (ssymbTerm "a" :: Term Integer) (ssymbTerm "b" :: Term Integer)
