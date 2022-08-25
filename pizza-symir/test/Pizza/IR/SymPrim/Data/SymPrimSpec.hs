{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Pizza.IR.SymPrim.Data.SymPrimSpec where

import Control.Monad.Except
import Data.Bits
import qualified Data.HashSet as S
import Data.Int
import Data.Proxy
import Data.Word
import Pizza.Core.Data.Class.BitVector
import Pizza.Core.Data.Class.Bool
import Pizza.Core.Data.Class.Evaluate
import Pizza.Core.Data.Class.ExtractSymbolics
import Pizza.Core.Data.Class.Function
import Pizza.Core.Data.Class.GenSym
import Pizza.Core.Data.Class.Integer
import Pizza.Core.Data.Class.Mergeable
import Pizza.Core.Data.Class.PrimWrapper
import Pizza.Core.Data.Class.SOrd
import Pizza.Core.Data.Class.SimpleMergeable
import Pizza.Core.Data.Class.ToCon
import Pizza.Core.Data.Class.ToSym
import Pizza.IR.SymPrim.Control.Monad.UnionM
import Pizza.IR.SymPrim.Data.BV
import Pizza.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Pizza.IR.SymPrim.Data.Prim.InternedTerm.Term
import qualified Pizza.IR.SymPrim.Data.Prim.Model as Model
import Pizza.IR.SymPrim.Data.Prim.PartialEval.BV
import Pizza.IR.SymPrim.Data.Prim.PartialEval.Bits
import Pizza.IR.SymPrim.Data.Prim.PartialEval.Bool
import Pizza.IR.SymPrim.Data.Prim.PartialEval.Integer
import Pizza.IR.SymPrim.Data.Prim.PartialEval.Num
import Pizza.IR.SymPrim.Data.Prim.PartialEval.TabularFunc
import Pizza.IR.SymPrim.Data.SymPrim
import Pizza.IR.SymPrim.Data.TabularFunc
import Test.Hspec
import Test.Hspec.QuickCheck
import Type.Reflection

spec :: Spec
spec = do
  describe "PrimWrapper for SymPrim" $ do
    it "conc should work" $ do
      (conc 1 :: Sym Integer) `shouldBe` Sym (concTerm 1)
    it "ssymb should work" $ do
      (ssymb "a" :: Sym Integer) `shouldBe` Sym (ssymbTerm "a")
    it "isymb should work" $ do
      (isymb "a" 1 :: Sym Integer) `shouldBe` Sym (isymbTerm "a" 1)
    it "concView should work" $ do
      concView (conc 1 :: Sym Integer) `shouldBe` Just 1
      concView (ssymb "a" :: Sym Integer) `shouldBe` Nothing
      case conc 1 :: Sym Integer of
        Conc 1 -> return ()
        _ -> expectationFailure "Bad match"
      case ssymb "a" :: Sym Integer of
        Conc _ -> expectationFailure "Bad match"
        _ -> return ()
  describe "ITEOp for SymPrim" $ do
    it "ites should work" $
      ites (ssymb "a" :: Sym Bool) (ssymb "b" :: Sym Integer) (ssymb "c")
        `shouldBe` Sym (pevalITETerm (ssymbTerm "a") (ssymbTerm "b") (ssymbTerm "c"))
  describe "Mergeable for SymPrim" $ do
    it "Mergeable should work" $ do
      let SimpleStrategy s = mergingStrategy :: MergingStrategy (Sym Bool) (Sym Integer)
      s (ssymb "a") (ssymb "b") (ssymb "c")
        `shouldBe` ites (ssymb "a" :: Sym Bool) (ssymb "b" :: Sym Integer) (ssymb "c")
  describe "SimpleMergeable for SymPrim" $ do
    it "Mergeable should work" $ do
      mrgIte (ssymb "a" :: Sym Bool) (ssymb "b") (ssymb "c")
        `shouldBe` ites (ssymb "a" :: Sym Bool) (ssymb "b" :: Sym Integer) (ssymb "c")
  describe "IsString for SymPrim" $ do
    it "string literal should work" $ do
      ("a" :: Sym Bool) `shouldBe` Sym (ssymbTerm "a")
  describe "ToSym for SymPrim" $ do
    it "ToSym from self" $ do
      toSym (ssymb "a" :: Sym Bool) `shouldBe` (ssymb "a" :: Sym Bool)
    it "ToSym from concrete" $ do
      toSym True `shouldBe` (conc True :: Sym Bool)
  describe "ToCon for SymPrim" $ do
    it "ToCon to self" $ do
      toCon (ssymb "a" :: Sym Bool) `shouldBe` (Nothing :: Maybe Bool)
    it "ToCon to concrete" $ do
      toCon True `shouldBe` Just True
  describe "Evaluate for SymPrim" $ do
    it "evaluate for SymPrim should work" $ do
      let m1 = Model.empty
      let m2 = Model.insert m1 (TermSymbol (typeRep @Integer) (SimpleSymbol "a")) (1 :: Integer)
      let m3 = Model.insert m2 (TermSymbol (typeRep @Bool) (SimpleSymbol "b")) True
      evaluateSym False m3 (ites ("c" :: Sym Bool) "a" ("a" + "a" :: Sym Integer))
        `shouldBe` ites ("c" :: Sym Bool) 1 2
      evaluateSym True m3 (ites ("c" :: Sym Bool) "a" ("a" + "a" :: Sym Integer)) `shouldBe` 2
  describe "ExtractSymbolics" $ do
    it "extractSymbolics for SymPrim should work" $ do
      extractSymbolics (ites ("c" :: Sym Bool) ("a" :: Sym Integer) ("b" :: Sym Integer))
        `shouldBe` S.fromList
          [ TermSymbol (typeRep @Bool) (SimpleSymbol "c"),
            TermSymbol (typeRep @Integer) (SimpleSymbol "a"),
            TermSymbol (typeRep @Integer) (SimpleSymbol "b")
          ]
  describe "GenSym" $ do
    it "GenSym for SymPrim should work" $ do
      (genSym () "a" :: UnionM (Sym Bool)) `shouldBe` mrgSingle (isymb "a" 0)
      (genSymSimple () "a" :: Sym Bool) `shouldBe` isymb "a" 0
      (genSym (ssymb "a" :: Sym Bool) "a" :: UnionM (Sym Bool)) `shouldBe` mrgSingle (isymb "a" 0)
      (genSymSimple (ssymb "a" :: Sym Bool) "a" :: Sym Bool) `shouldBe` isymb "a" 0
      (genSym () (nameWithInfo "a" True) :: UnionM (Sym Bool)) `shouldBe` mrgSingle (iinfosymb "a" 0 True)
      (genSymSimple () (nameWithInfo "a" True) :: Sym Bool) `shouldBe` iinfosymb "a" 0 True
  describe "SEq" $ do
    it "SEq for SymPrim should work" $ do
      (ssymb "a" :: Sym Bool) ==~ ssymb "b" `shouldBe` Sym (pevalEqvTerm (ssymbTerm "a" :: Term Bool) (ssymbTerm "b"))
      (ssymb "a" :: Sym Bool) /=~ ssymb "b" `shouldBe` Sym (pevalNotTerm $ pevalEqvTerm (ssymbTerm "a" :: Term Bool) (ssymbTerm "b"))
  describe "Sym Bool" $ do
    describe "LogicalOp for Sym Bool" $ do
      it "||~ for SymPrim should work" $ do
        ssymb "a" ||~ ssymb "b" `shouldBe` Sym (pevalOrTerm (ssymbTerm "a") (ssymbTerm "b"))
      it "&&~ for SymPrim should work" $ do
        ssymb "a" &&~ ssymb "b" `shouldBe` Sym (pevalAndTerm (ssymbTerm "a") (ssymbTerm "b"))
      it "nots for SymPrim should work" $ do
        nots (ssymb "a") `shouldBe` Sym (pevalNotTerm (ssymbTerm "a"))
      it "xors for SymPrim should work" $ do
        xors (ssymb "a") (ssymb "b") `shouldBe` Sym (pevalXorTerm (ssymbTerm "a") (ssymbTerm "b"))
      it "implies for SymPrim should work" $ do
        implies (ssymb "a") (ssymb "b") `shouldBe` Sym (pevalImplyTerm (ssymbTerm "a") (ssymbTerm "b"))
  describe "Sym Integer" $ do
    describe "Num for Sym Integer" $ do
      it "fromInteger should work" $ do
        (1 :: Sym Integer) `shouldBe` Sym (concTerm 1)
      it "(+) for SymPrim should work" $ do
        (ssymb "a" :: Sym Integer) + ssymb "b" `shouldBe` Sym (pevalAddNumTerm (ssymbTerm "a") (ssymbTerm "b"))
      it "(-) for SymPrim should work" $ do
        (ssymb "a" :: Sym Integer) - ssymb "b" `shouldBe` Sym (pevalMinusNumTerm (ssymbTerm "a") (ssymbTerm "b"))
      it "(*) for SymPrim should work" $ do
        (ssymb "a" :: Sym Integer) * ssymb "b" `shouldBe` Sym (pevalTimesNumTerm (ssymbTerm "a") (ssymbTerm "b"))
      it "negate for SymPrim should work" $ do
        negate (ssymb "a" :: Sym Integer) `shouldBe` Sym (pevalUMinusNumTerm (ssymbTerm "a"))
      it "abs for SymPrim should work" $ do
        abs (ssymb "a" :: Sym Integer) `shouldBe` Sym (pevalAbsNumTerm (ssymbTerm "a"))
      it "signum for SymPrim should work" $ do
        signum (ssymb "a" :: Sym Integer) `shouldBe` Sym (pevalSignumNumTerm (ssymbTerm "a"))
    describe "SignedDivMod for Sym Integer" $ do
      prop "divs should work on concrete" $ \(i :: Integer, j :: Integer) ->
        divs (conc i :: Sym Integer) (conc j)
          `shouldBe` if j == 0
            then merge $ throwError () :: ExceptT () UnionM SymInteger
            else mrgSingle $ conc $ i `div` j
      it "divs should work when divided by zero" $ do
        divs (ssymb "a" :: Sym Integer) (conc 0)
          `shouldBe` (merge $ throwError () :: ExceptT () UnionM SymInteger)
      it "divs should work on symbolic" $ do
        divs (ssymb "a" :: Sym Integer) (ssymb "b")
          `shouldBe` ( mrgIf
                         ((ssymb "b" :: Sym Integer) ==~ conc (0 :: Integer) :: SymBool)
                         (throwError ())
                         (mrgSingle $ Sym $ pevalDivIntegerTerm (ssymbTerm "a") (ssymbTerm "b")) ::
                         ExceptT () UnionM SymInteger
                     )
      prop "mods should work on concrete" $ \(i :: Integer, j :: Integer) ->
        mods (conc i :: Sym Integer) (conc j)
          `shouldBe` if j == 0
            then merge $ throwError () :: ExceptT () UnionM SymInteger
            else mrgSingle $ conc $ i `mod` j
      it "mods should work when divided by zero" $ do
        mods (ssymb "a" :: Sym Integer) (conc 0)
          `shouldBe` (merge $ throwError () :: ExceptT () UnionM SymInteger)
      it "mods should work on symbolic" $ do
        mods (ssymb "a" :: Sym Integer) (ssymb "b")
          `shouldBe` ( mrgIf
                         ((ssymb "b" :: Sym Integer) ==~ conc (0 :: Integer) :: SymBool)
                         (throwError ())
                         (mrgSingle $ Sym $ pevalModIntegerTerm (ssymbTerm "a") (ssymbTerm "b")) ::
                         ExceptT () UnionM SymInteger
                     )
    describe "SOrd for Sym Integer" $ do
      prop "SOrd should work on concrete" $ \(i :: Integer, j :: Integer) -> do
        (conc i :: Sym Integer) <=~ conc j `shouldBe` (conc (i <= j) :: SymBool)
        (conc i :: Sym Integer) <~ conc j `shouldBe` (conc (i < j) :: SymBool)
        (conc i :: Sym Integer) >=~ conc j `shouldBe` (conc (i >= j) :: SymBool)
        (conc i :: Sym Integer) >~ conc j `shouldBe` (conc (i > j) :: SymBool)
        (conc i :: Sym Integer)
          `symCompare` conc j
          `shouldBe` (i `symCompare` j :: UnionM Ordering)
      it "SOrd should work on symbolic" $ do
        let a :: Sym Integer = ssymb "a"
        let b :: Sym Integer = ssymb "b"
        let at :: Term Integer = ssymbTerm "a"
        let bt :: Term Integer = ssymbTerm "b"
        a <=~ b `shouldBe` Sym (pevalLeNumTerm at bt)
        a <~ b `shouldBe` Sym (pevalLtNumTerm at bt)
        a >=~ b `shouldBe` Sym (pevalGeNumTerm at bt)
        a >~ b `shouldBe` Sym (pevalGtNumTerm at bt)
        (a `symCompare` ssymb "b" :: UnionM Ordering)
          `shouldBe` mrgIf (a <~ b) (mrgSingle LT) (mrgIf (a ==~ b) (mrgSingle EQ) (mrgSingle GT))
  describe "Sym BV" $ do
    let au :: Sym (WordN 4) = ssymb "a"
    let bu :: Sym (WordN 4) = ssymb "b"
    let as :: Sym (IntN 4) = ssymb "a"
    let bs :: Sym (IntN 4) = ssymb "b"
    let aut :: Term (WordN 4) = ssymbTerm "a"
    let but :: Term (WordN 4) = ssymbTerm "b"
    let ast :: Term (IntN 4) = ssymbTerm "a"
    let bst :: Term (IntN 4) = ssymbTerm "b"
    describe "Num for Sym BV" $ do
      it "fromInteger should work" $ do
        (1 :: Sym (WordN 4)) `shouldBe` Sym (concTerm 1)
        (1 :: Sym (IntN 4)) `shouldBe` Sym (concTerm 1)
      it "(+) for SymPrim should work" $ do
        au + bu `shouldBe` Sym (pevalAddNumTerm aut but)
        as + bs `shouldBe` Sym (pevalAddNumTerm ast bst)
      it "(-) for SymPrim should work" $ do
        au - bu `shouldBe` Sym (pevalMinusNumTerm aut but)
        as - bs `shouldBe` Sym (pevalMinusNumTerm ast bst)
      it "(*) for SymPrim should work" $ do
        au * bu `shouldBe` Sym (pevalTimesNumTerm aut but)
        as * bs `shouldBe` Sym (pevalTimesNumTerm ast bst)
      it "negate for SymPrim should work" $ do
        negate au `shouldBe` Sym (pevalUMinusNumTerm aut)
        negate as `shouldBe` Sym (pevalUMinusNumTerm ast)
      it "abs for SymPrim should work" $ do
        abs au `shouldBe` Sym (pevalAbsNumTerm aut)
        abs as `shouldBe` Sym (pevalAbsNumTerm ast)
      it "signum for SymPrim should work" $ do
        signum au `shouldBe` Sym (pevalSignumNumTerm aut)
        signum as `shouldBe` Sym (pevalSignumNumTerm ast)
    describe "SOrd for Sym BV" $ do
      prop "SOrd should work on concrete" $ \(i :: Integer, j :: Integer) -> do
        let iu :: WordN 4 = fromInteger i
        let ju :: WordN 4 = fromInteger j
        let is :: IntN 4 = fromInteger i
        let js :: IntN 4 = fromInteger j
        let normalizeu k = k - k `div` 16 * 16
        let normalizes k = if normalizeu k >= 8 then normalizeu k - 16 else normalizeu k
        (conc iu :: Sym (WordN 4)) <=~ conc ju `shouldBe` (conc (normalizeu i <= normalizeu j) :: SymBool)
        (conc iu :: Sym (WordN 4)) <~ conc ju `shouldBe` (conc (normalizeu i < normalizeu j) :: SymBool)
        (conc iu :: Sym (WordN 4)) >=~ conc ju `shouldBe` (conc (normalizeu i >= normalizeu j) :: SymBool)
        (conc iu :: Sym (WordN 4)) >~ conc ju `shouldBe` (conc (normalizeu i > normalizeu j) :: SymBool)
        (conc iu :: Sym (WordN 4))
          `symCompare` conc ju
          `shouldBe` (normalizeu i `symCompare` normalizeu j :: UnionM Ordering)
        (conc is :: Sym (IntN 4)) <=~ conc js `shouldBe` (conc (normalizes i <= normalizes j) :: SymBool)
        (conc is :: Sym (IntN 4)) <~ conc js `shouldBe` (conc (normalizes i < normalizes j) :: SymBool)
        (conc is :: Sym (IntN 4)) >=~ conc js `shouldBe` (conc (normalizes i >= normalizes j) :: SymBool)
        (conc is :: Sym (IntN 4)) >~ conc js `shouldBe` (conc (normalizes i > normalizes j) :: SymBool)
        (conc is :: Sym (IntN 4))
          `symCompare` conc js
          `shouldBe` (normalizes i `symCompare` normalizes j :: UnionM Ordering)
      it "SOrd should work on symbolic" $ do
        au <=~ bu `shouldBe` Sym (pevalLeNumTerm aut but)
        au <~ bu `shouldBe` Sym (pevalLtNumTerm aut but)
        au >=~ bu `shouldBe` Sym (pevalGeNumTerm aut but)
        au >~ bu `shouldBe` Sym (pevalGtNumTerm aut but)
        (au `symCompare` bu :: UnionM Ordering)
          `shouldBe` mrgIf (au <~ bu) (mrgSingle LT) (mrgIf (au ==~ bu) (mrgSingle EQ) (mrgSingle GT))

        as <=~ bs `shouldBe` Sym (pevalLeNumTerm ast bst)
        as <~ bs `shouldBe` Sym (pevalLtNumTerm ast bst)
        as >=~ bs `shouldBe` Sym (pevalGeNumTerm ast bst)
        as >~ bs `shouldBe` Sym (pevalGtNumTerm ast bst)
        (as `symCompare` bs :: UnionM Ordering)
          `shouldBe` mrgIf (as <~ bs) (mrgSingle LT) (mrgIf (as ==~ bs) (mrgSingle EQ) (mrgSingle GT))
    describe "Bits for Sym BV" $ do
      it ".&. for SymPrim should work" $ do
        au .&. bu `shouldBe` Sym (pevalAndBitsTerm aut but)
        as .&. bs `shouldBe` Sym (pevalAndBitsTerm ast bst)
      it ".|. for SymPrim should work" $ do
        au .|. bu `shouldBe` Sym (pevalOrBitsTerm aut but)
        as .|. bs `shouldBe` Sym (pevalOrBitsTerm ast bst)
      it "xor for SymPrim should work" $ do
        au `xor` bu `shouldBe` Sym (pevalXorBitsTerm aut but)
        as `xor` bs `shouldBe` Sym (pevalXorBitsTerm ast bst)
      it "complement for SymPrim should work" $ do
        complement au `shouldBe` Sym (pevalComplementBitsTerm aut)
        complement as `shouldBe` Sym (pevalComplementBitsTerm ast)
      it "shift for SymPrim should work" $ do
        shift au 1 `shouldBe` Sym (pevalShiftBitsTerm aut 1)
        shift as 1 `shouldBe` Sym (pevalShiftBitsTerm ast 1)
      it "rotate for SymPrim should work" $ do
        rotate au 1 `shouldBe` Sym (pevalRotateBitsTerm aut 1)
        rotate as 1 `shouldBe` Sym (pevalRotateBitsTerm ast 1)
      it "bitSize for SymPrim should work" $ do
        bitSizeMaybe au `shouldBe` Just 4
        bitSizeMaybe as `shouldBe` Just 4
      it "isSigned for SymPrim should work" $ do
        isSigned au `shouldBe` False
        isSigned as `shouldBe` True
      it "testBit for SymPrim would only work on concrete ones" $ do
        testBit (Conc 3 :: Sym (WordN 4)) 1 `shouldBe` True
        testBit (Conc 3 :: Sym (WordN 4)) 2 `shouldBe` False
        testBit (Conc 3 :: Sym (IntN 4)) 1 `shouldBe` True
        testBit (Conc 3 :: Sym (IntN 4)) 2 `shouldBe` False
      it "bit for SymPrim would work" $ do
        bit 1 `shouldBe` (Conc 2 :: Sym (WordN 4))
        bit 1 `shouldBe` (Conc 2 :: Sym (IntN 4))
      it "popCount for SymPrim would only work on concrete ones" $ do
        popCount (Conc 3 :: Sym (WordN 4)) `shouldBe` 2
        popCount (Conc 3 :: Sym (WordN 4)) `shouldBe` 2
        popCount (Conc 3 :: Sym (IntN 4)) `shouldBe` 2
        popCount (Conc 3 :: Sym (IntN 4)) `shouldBe` 2
    describe "BVConcat for Sym BV" $ do
      it "bvconcat for SymPrim" $ do
        bvconcat
          (ssymb "a" :: Sym (WordN 4))
          (ssymb "b" :: Sym (WordN 3))
          `shouldBe` Sym
            ( pevalBVConcatTerm
                (ssymbTerm "a" :: Term (WordN 4))
                (ssymbTerm "b" :: Term (WordN 3))
            )
    describe "bvextend for Sym BV" $ do
      it "bvzeroExtend for SymPrim" $ do
        bvzeroExtend (Proxy @6) au `shouldBe` Sym (pevalBVExtendTerm False (Proxy @6) aut)
        bvzeroExtend (Proxy @6) as `shouldBe` Sym (pevalBVExtendTerm False (Proxy @6) ast)
      it "bvsignExtend for SymPrim" $ do
        bvsignExtend (Proxy @6) au `shouldBe` Sym (pevalBVExtendTerm True (Proxy @6) aut)
        bvsignExtend (Proxy @6) as `shouldBe` Sym (pevalBVExtendTerm True (Proxy @6) ast)
      it "bvextend for SymPrim" $ do
        bvextend (Proxy @6) au `shouldBe` Sym (pevalBVExtendTerm False (Proxy @6) aut)
        bvextend (Proxy @6) as `shouldBe` Sym (pevalBVExtendTerm True (Proxy @6) ast)
    describe "bvselect for Sym BV" $ do
      it "bvselect for SymPrim" $ do
        bvselect (Proxy @2) (Proxy @1) au
          `shouldBe` Sym (pevalBVSelectTerm (Proxy @2) (Proxy @1) aut)
        bvselect (Proxy @2) (Proxy @1) as
          `shouldBe` Sym (pevalBVSelectTerm (Proxy @2) (Proxy @1) ast)
    describe "conversion between Int8 and Sym BV" $ do
      it "toSym" $ do
        toSym (0 :: Int8) `shouldBe` (conc 0 :: SymIntN 8)
        toSym (-127 :: Int8) `shouldBe` (conc $ -127 :: SymIntN 8)
        toSym (-128 :: Int8) `shouldBe` (conc $ -128 :: SymIntN 8)
        toSym (127 :: Int8) `shouldBe` (conc 127 :: SymIntN 8)
      it "toCon" $ do
        toCon (conc 0 :: SymIntN 8) `shouldBe` Just (0 :: Int8)
        toCon (conc $ -127 :: SymIntN 8) `shouldBe` Just (-127 :: Int8)
        toCon (conc $ -128 :: SymIntN 8) `shouldBe` Just (-128 :: Int8)
        toCon (conc 127 :: SymIntN 8) `shouldBe` Just (127 :: Int8)
    describe "conversion between Word8 and Sym BV" $ do
      it "toSym" $ do
        toSym (0 :: Word8) `shouldBe` (conc 0 :: SymWordN 8)
        toSym (1 :: Word8) `shouldBe` (conc 1 :: SymWordN 8)
        toSym (255 :: Word8) `shouldBe` (conc 255 :: SymWordN 8)
      it "toCon" $ do
        toCon (conc 0 :: SymWordN 8) `shouldBe` Just (0 :: Word8)
        toCon (conc 1 :: SymWordN 8) `shouldBe` Just (1 :: Word8)
        toCon (conc 255 :: SymWordN 8) `shouldBe` Just (255 :: Word8)
  describe "TabularFunc" $ do
    it "apply" $ do
      (ssymb "a" :: Integer =~> Integer)
        # ssymb "b"
        `shouldBe` Sym (pevalTabularFuncApplyTerm (ssymbTerm "a" :: Term (Integer =-> Integer)) (ssymbTerm "b"))
  describe "Symbolic size" $ do
    it "symSize" $ do
      symSize (ssymb "a" :: Sym Integer) `shouldBe` 1
      symSize (conc 1 :: Sym Integer) `shouldBe` 1
      symSize (conc 1 + ssymb "a" :: Sym Integer) `shouldBe` 3
      symSize (ssymb "a" + ssymb "a" :: Sym Integer) `shouldBe` 2
      symSize (-(ssymb "a") :: Sym Integer) `shouldBe` 2
      symSize (ites (ssymb "a" :: Sym Bool) (ssymb "b") (ssymb "c") :: Sym Integer) `shouldBe` 4
    it "symsSize" $ do
      symsSize [ssymb "a" :: Sym Integer, ssymb "a" + ssymb "a"] `shouldBe` 2
