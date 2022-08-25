{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Pizza.IR.SymPrim.Data.Prim.BVSpec where

import Data.Proxy
import Pizza.IR.SymPrim.Data.BV
import Pizza.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Pizza.IR.SymPrim.Data.Prim.InternedTerm.Term
import Pizza.IR.SymPrim.Data.Prim.PartialEval.BV
import Test.Hspec

spec :: Spec
spec = do
  describe "pevalBVSelectTerm" $ do
    describe "pevalBVSelectTerm construction" $ do
      it "pevalBVSelectTerm on concrete" $ do
        pevalBVSelectTerm
          (Proxy @0)
          (Proxy @1)
          (concTerm 6 :: Term (WordN 4))
          `shouldBe` concTerm 0
        pevalBVSelectTerm
          (Proxy @1)
          (Proxy @1)
          (concTerm 6 :: Term (WordN 4))
          `shouldBe` concTerm 1
        pevalBVSelectTerm
          (Proxy @2)
          (Proxy @1)
          (concTerm 6 :: Term (WordN 4))
          `shouldBe` concTerm 1
        pevalBVSelectTerm
          (Proxy @3)
          (Proxy @1)
          (concTerm 6 :: Term (WordN 4))
          `shouldBe` concTerm 0
        pevalBVSelectTerm
          (Proxy @0)
          (Proxy @2)
          (concTerm 6 :: Term (WordN 4))
          `shouldBe` concTerm 2
        pevalBVSelectTerm
          (Proxy @1)
          (Proxy @2)
          (concTerm 6 :: Term (WordN 4))
          `shouldBe` concTerm 3
        pevalBVSelectTerm
          (Proxy @2)
          (Proxy @2)
          (concTerm 6 :: Term (WordN 4))
          `shouldBe` concTerm 1
        pevalBVSelectTerm
          (Proxy @0)
          (Proxy @3)
          (concTerm 6 :: Term (WordN 4))
          `shouldBe` concTerm 6
        pevalBVSelectTerm
          (Proxy @1)
          (Proxy @3)
          (concTerm 6 :: Term (WordN 4))
          `shouldBe` concTerm 3
        pevalBVSelectTerm
          (Proxy @0)
          (Proxy @4)
          (concTerm 6 :: Term (WordN 4))
          `shouldBe` concTerm 6
      it "pevalBVSelectTerm on symbolic" $ do
        pevalBVSelectTerm
          (Proxy @2)
          (Proxy @1)
          (ssymbTerm "a" :: Term (WordN 4))
          `shouldBe` bvselectTerm (Proxy @2) (Proxy @1) (ssymbTerm "a" :: Term (WordN 4))
  describe "Extension" $ do
    describe "Extension construction" $ do
      it "Extension on concrete" $ do
        pevalBVExtendTerm True (Proxy @6) (concTerm 15 :: Term (WordN 4))
          `shouldBe` (concTerm 63 :: Term (WordN 6))
        pevalBVExtendTerm False (Proxy @6) (concTerm 15 :: Term (WordN 4))
          `shouldBe` (concTerm 15 :: Term (WordN 6))
        pevalBVExtendTerm True (Proxy @6) (concTerm 15 :: Term (IntN 4))
          `shouldBe` (concTerm 63 :: Term (IntN 6))
        pevalBVExtendTerm False (Proxy @6) (concTerm 15 :: Term (IntN 4))
          `shouldBe` (concTerm 15 :: Term (IntN 6))
      it "Extension on symbolic" $ do
        pevalBVExtendTerm True (Proxy @6) (ssymbTerm "a" :: Term (WordN 4))
          `shouldBe` bvextendTerm True (Proxy @6) (ssymbTerm "a" :: Term (WordN 4))
        pevalBVExtendTerm False (Proxy @6) (ssymbTerm "a" :: Term (WordN 4))
          `shouldBe` bvextendTerm False (Proxy @6) (ssymbTerm "a" :: Term (WordN 4))
  describe "Concat" $ do
    describe "Concat construction" $ do
      it "Concat on concrete" $ do
        pevalBVConcatTerm (concTerm 3 :: Term (WordN 4)) (concTerm 5 :: Term (WordN 3))
          `shouldBe` concTerm 29
        pevalBVConcatTerm (concTerm 3 :: Term (IntN 4)) (concTerm 5 :: Term (IntN 3))
          `shouldBe` concTerm 29
      it "Concat on symbolic" $ do
        pevalBVConcatTerm (ssymbTerm "a" :: Term (WordN 4)) (ssymbTerm "b" :: Term (WordN 3))
          `shouldBe` bvconcatTerm
            (ssymbTerm "a" :: Term (WordN 4))
            (ssymbTerm "b" :: Term (WordN 3))
