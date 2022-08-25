{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pizza.IR.SymPrim.Data.Prim.BitsSpec where

import Pizza.IR.SymPrim.Data.BV
import Pizza.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Pizza.IR.SymPrim.Data.Prim.InternedTerm.Term
import Pizza.IR.SymPrim.Data.Prim.PartialEval.Bits
import Test.Hspec

spec :: Spec
spec = do
  describe "AndBits" $ do
    describe "AndBits construction" $ do
      it "AndBits on both concrete" $ do
        pevalAndBitsTerm
          (concTerm 3 :: Term (WordN 4))
          (concTerm 5)
          `shouldBe` concTerm 1
      it "AndBits with zeroBits" $ do
        pevalAndBitsTerm
          (concTerm 0 :: Term (WordN 4))
          (ssymbTerm "a")
          `shouldBe` concTerm 0
        pevalAndBitsTerm
          (ssymbTerm "a")
          (concTerm 0 :: Term (WordN 4))
          `shouldBe` concTerm 0
      it "AndBits with all one bits" $ do
        pevalAndBitsTerm
          (concTerm 15 :: Term (WordN 4))
          (ssymbTerm "a")
          `shouldBe` ssymbTerm "a"
        pevalAndBitsTerm
          (ssymbTerm "a")
          (concTerm 15 :: Term (WordN 4))
          `shouldBe` ssymbTerm "a"
      it "AndBits symbolic" $ do
        pevalAndBitsTerm
          (ssymbTerm "a" :: Term (WordN 4))
          (ssymbTerm "b")
          `shouldBe` andBitsTerm
            (ssymbTerm "a" :: Term (WordN 4))
            (ssymbTerm "b" :: Term (WordN 4))
  describe "OrBits" $ do
    describe "OrBits construction" $ do
      it "OrBits on both concrete" $ do
        pevalOrBitsTerm
          (concTerm 3 :: Term (WordN 4))
          (concTerm 5)
          `shouldBe` concTerm 7
      it "OrBits with zeroBits" $ do
        pevalOrBitsTerm
          (concTerm 0 :: Term (WordN 4))
          (ssymbTerm "a")
          `shouldBe` ssymbTerm "a"
        pevalOrBitsTerm
          (ssymbTerm "a")
          (concTerm 0 :: Term (WordN 4))
          `shouldBe` ssymbTerm "a"
      it "OrBits with all one bits" $ do
        pevalOrBitsTerm
          (concTerm 15 :: Term (WordN 4))
          (ssymbTerm "a")
          `shouldBe` concTerm 15
        pevalOrBitsTerm
          (ssymbTerm "a")
          (concTerm 15 :: Term (WordN 4))
          `shouldBe` concTerm 15
      it "OrBits symbolic" $ do
        pevalOrBitsTerm
          (ssymbTerm "a" :: Term (WordN 4))
          (ssymbTerm "b")
          `shouldBe` orBitsTerm
            (ssymbTerm "a" :: Term (WordN 4))
            (ssymbTerm "b" :: Term (WordN 4))
  describe "XorBits" $ do
    describe "XorBits construction" $ do
      it "XorBits on both concrete" $ do
        pevalXorBitsTerm
          (concTerm 3 :: Term (WordN 4))
          (concTerm 5)
          `shouldBe` concTerm 6
      it "XorBits with zeroBits" $ do
        pevalXorBitsTerm
          (concTerm 0 :: Term (WordN 4))
          (ssymbTerm "a")
          `shouldBe` ssymbTerm "a"
        pevalXorBitsTerm
          (ssymbTerm "a")
          (concTerm 0 :: Term (WordN 4))
          `shouldBe` ssymbTerm "a"
      it "XorBits with all one bits" $ do
        pevalXorBitsTerm
          (concTerm 15 :: Term (WordN 4))
          (ssymbTerm "a")
          `shouldBe` pevalComplementBitsTerm (ssymbTerm "a")
        pevalXorBitsTerm
          (ssymbTerm "a")
          (concTerm 15 :: Term (WordN 4))
          `shouldBe` pevalComplementBitsTerm (ssymbTerm "a")
      it "XorBits with single complement" $ do
        pevalXorBitsTerm
          (pevalComplementBitsTerm $ ssymbTerm "a" :: Term (WordN 4))
          (ssymbTerm "b")
          `shouldBe` pevalComplementBitsTerm (pevalXorBitsTerm (ssymbTerm "a") (ssymbTerm "b"))
        pevalXorBitsTerm
          (ssymbTerm "a" :: Term (WordN 4))
          (pevalComplementBitsTerm $ ssymbTerm "b")
          `shouldBe` pevalComplementBitsTerm (pevalXorBitsTerm (ssymbTerm "a") (ssymbTerm "b"))
      it "XorBits with both complement" $ do
        pevalXorBitsTerm
          (pevalComplementBitsTerm $ ssymbTerm "a" :: Term (WordN 4))
          (pevalComplementBitsTerm $ ssymbTerm "b")
          `shouldBe` pevalXorBitsTerm (ssymbTerm "a") (ssymbTerm "b")
      it "XorBits symbolic" $ do
        pevalXorBitsTerm
          (ssymbTerm "a" :: Term (WordN 4))
          (ssymbTerm "b")
          `shouldBe` xorBitsTerm
            (ssymbTerm "a" :: Term (WordN 4))
            (ssymbTerm "b" :: Term (WordN 4))
  describe "ComplementBits" $ do
    describe "ComplementBits construction" $ do
      it "ComplementBits on concrete" $ do
        pevalComplementBitsTerm (concTerm 5 :: Term (WordN 4)) `shouldBe` concTerm 10
      it "ComplementBits on complement" $ do
        pevalComplementBitsTerm (pevalComplementBitsTerm (ssymbTerm "a") :: Term (WordN 4)) `shouldBe` ssymbTerm "a"
      it "ComplementBits on symbolic" $ do
        pevalComplementBitsTerm (ssymbTerm "a" :: Term (WordN 4))
          `shouldBe` complementBitsTerm (ssymbTerm "a" :: Term (WordN 4))
  describe "ShiftBits" $ do
    describe "ShiftBits construction" $ do
      it "ShiftBits on concrete" $ do
        pevalShiftBitsTerm (concTerm 15 :: Term (WordN 4)) (-5) `shouldBe` concTerm 0
        pevalShiftBitsTerm (concTerm 15 :: Term (WordN 4)) (-4) `shouldBe` concTerm 0
        pevalShiftBitsTerm (concTerm 15 :: Term (WordN 4)) (-3) `shouldBe` concTerm 1
        pevalShiftBitsTerm (concTerm 15 :: Term (WordN 4)) (-2) `shouldBe` concTerm 3
        pevalShiftBitsTerm (concTerm 15 :: Term (WordN 4)) (-1) `shouldBe` concTerm 7
        pevalShiftBitsTerm (concTerm 15 :: Term (WordN 4)) 0 `shouldBe` concTerm 15
        pevalShiftBitsTerm (concTerm 15 :: Term (WordN 4)) 1 `shouldBe` concTerm 14
        pevalShiftBitsTerm (concTerm 15 :: Term (WordN 4)) 2 `shouldBe` concTerm 12
        pevalShiftBitsTerm (concTerm 15 :: Term (WordN 4)) 3 `shouldBe` concTerm 8
        pevalShiftBitsTerm (concTerm 15 :: Term (WordN 4)) 4 `shouldBe` concTerm 0
        pevalShiftBitsTerm (concTerm 15 :: Term (WordN 4)) 5 `shouldBe` concTerm 0

        pevalShiftBitsTerm (concTerm 15 :: Term (IntN 4)) (-5) `shouldBe` concTerm 15
        pevalShiftBitsTerm (concTerm 15 :: Term (IntN 4)) (-4) `shouldBe` concTerm 15
        pevalShiftBitsTerm (concTerm 15 :: Term (IntN 4)) (-3) `shouldBe` concTerm 15
        pevalShiftBitsTerm (concTerm 15 :: Term (IntN 4)) (-2) `shouldBe` concTerm 15
        pevalShiftBitsTerm (concTerm 15 :: Term (IntN 4)) (-1) `shouldBe` concTerm 15
        pevalShiftBitsTerm (concTerm 15 :: Term (IntN 4)) 0 `shouldBe` concTerm 15
        pevalShiftBitsTerm (concTerm 15 :: Term (IntN 4)) 1 `shouldBe` concTerm 14
        pevalShiftBitsTerm (concTerm 15 :: Term (IntN 4)) 2 `shouldBe` concTerm 12
        pevalShiftBitsTerm (concTerm 15 :: Term (IntN 4)) 3 `shouldBe` concTerm 8
        pevalShiftBitsTerm (concTerm 15 :: Term (IntN 4)) 4 `shouldBe` concTerm 0
        pevalShiftBitsTerm (concTerm 15 :: Term (IntN 4)) 5 `shouldBe` concTerm 0
      it "ShiftBits 0" $ do
        pevalShiftBitsTerm (ssymbTerm "a" :: Term (WordN 4)) 0 `shouldBe` ssymbTerm "a"
        pevalShiftBitsTerm (ssymbTerm "a" :: Term (IntN 4)) 0 `shouldBe` ssymbTerm "a"
      it "ShiftBits left bitsize" $ do
        pevalShiftBitsTerm (ssymbTerm "a" :: Term (WordN 4)) 4 `shouldBe` concTerm 0
        pevalShiftBitsTerm (ssymbTerm "a" :: Term (IntN 4)) 4 `shouldBe` concTerm 0
        pevalShiftBitsTerm (ssymbTerm "a" :: Term (WordN 4)) 5 `shouldBe` concTerm 0
        pevalShiftBitsTerm (ssymbTerm "a" :: Term (IntN 4)) 5 `shouldBe` concTerm 0
      it "ShiftBits same direction twice" $ do
        pevalShiftBitsTerm (pevalShiftBitsTerm (ssymbTerm "a" :: Term (WordN 4)) 1) 2
          `shouldBe` pevalShiftBitsTerm (ssymbTerm "a" :: Term (WordN 4)) 3
        pevalShiftBitsTerm (pevalShiftBitsTerm (ssymbTerm "a" :: Term (WordN 4)) (-1)) (-2)
          `shouldBe` pevalShiftBitsTerm (ssymbTerm "a" :: Term (WordN 4)) (-3)
      it "ShiftBits symbolic" $ do
        pevalShiftBitsTerm (ssymbTerm "a" :: Term (WordN 4)) 2
          `shouldBe` shiftBitsTerm (ssymbTerm "a" :: Term (WordN 4)) 2
  describe "Rotate" $ do
    describe "Rotate construction" $ do
      it "Rotate on concrete" $ do
        pevalRotateBitsTerm (concTerm 3 :: Term (WordN 4)) (-4) `shouldBe` concTerm 3
        pevalRotateBitsTerm (concTerm 3 :: Term (WordN 4)) (-3) `shouldBe` concTerm 6
        pevalRotateBitsTerm (concTerm 3 :: Term (WordN 4)) (-2) `shouldBe` concTerm 12
        pevalRotateBitsTerm (concTerm 3 :: Term (WordN 4)) (-1) `shouldBe` concTerm 9
        pevalRotateBitsTerm (concTerm 3 :: Term (WordN 4)) 0 `shouldBe` concTerm 3
        pevalRotateBitsTerm (concTerm 3 :: Term (WordN 4)) 1 `shouldBe` concTerm 6
        pevalRotateBitsTerm (concTerm 3 :: Term (WordN 4)) 2 `shouldBe` concTerm 12
        pevalRotateBitsTerm (concTerm 3 :: Term (WordN 4)) 3 `shouldBe` concTerm 9
        pevalRotateBitsTerm (concTerm 3 :: Term (WordN 4)) 4 `shouldBe` concTerm 3
      it "Rotate 0" $ do
        pevalRotateBitsTerm (ssymbTerm "a" :: Term (WordN 4)) 0 `shouldBe` ssymbTerm "a"
      it "Rotate extra bits" $ do
        pevalRotateBitsTerm (ssymbTerm "a" :: Term (WordN 4)) 4 `shouldBe` ssymbTerm "a"
        pevalRotateBitsTerm (ssymbTerm "a" :: Term (WordN 4)) 5
          `shouldBe` pevalRotateBitsTerm (ssymbTerm "a") 1
        pevalRotateBitsTerm (ssymbTerm "a" :: Term (WordN 4)) (-1)
          `shouldBe` pevalRotateBitsTerm (ssymbTerm "a") 3
      it "Rotate twice" $ do
        pevalRotateBitsTerm (pevalRotateBitsTerm (ssymbTerm "a" :: Term (WordN 4)) 1) 2
          `shouldBe` pevalRotateBitsTerm (ssymbTerm "a") 3
      it "Rotate symbolic" $ do
        pevalRotateBitsTerm (ssymbTerm "a" :: Term (WordN 4)) 2
          `shouldBe` rotateBitsTerm (ssymbTerm "a" :: Term (WordN 4)) 2
