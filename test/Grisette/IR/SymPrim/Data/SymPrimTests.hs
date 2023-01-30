{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Grisette.IR.SymPrim.Data.SymPrimTests where

import Control.Monad.Except
import Data.Bits
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.Int
import Data.Proxy
import Data.Word
import Grisette.Core.Control.Monad.UnionM
import Grisette.Core.Data.Class.BitVector
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Evaluate
import Grisette.Core.Data.Class.ExtractSymbolics
import Grisette.Core.Data.Class.Function
import Grisette.Core.Data.Class.GenSym
import Grisette.Core.Data.Class.Integer
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Data.Class.ModelOps
import Grisette.Core.Data.Class.SOrd
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Core.Data.Class.Solvable
import Grisette.Core.Data.Class.ToCon
import Grisette.Core.Data.Class.ToSym
import Grisette.IR.SymPrim.Data.BV
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Grisette.IR.SymPrim.Data.Prim.Model
import Grisette.IR.SymPrim.Data.Prim.ModelValue
import Grisette.IR.SymPrim.Data.Prim.PartialEval.BV
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bits
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bool
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Integer
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Num
import Grisette.IR.SymPrim.Data.Prim.PartialEval.TabularFun
import Grisette.IR.SymPrim.Data.SymPrim
import Grisette.IR.SymPrim.Data.TabularFun
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding ((.&.))
import Type.Reflection hiding (Con)

symPrimTests :: TestTree
symPrimTests =
  testGroup
    "SymPrimTests"
    [ testGroup
        "General SymPrim"
        [ testGroup
            "Solvable"
            [ testCase "con" $ do
                (con 1 :: SymInteger) @=? SymInteger (conTerm 1),
              testCase "ssym" $ do
                (ssym "a" :: SymInteger) @=? SymInteger (ssymTerm "a"),
              testCase "isym" $ do
                (isym "a" 1 :: SymInteger) @=? SymInteger (isymTerm "a" 1),
              testCase "conView" $ do
                conView (con 1 :: SymInteger) @=? Just 1
                conView (ssym "a" :: SymInteger) @=? Nothing
                case con 1 :: SymInteger of
                  Con 1 -> return ()
                  _ -> assertFailure "Bad match"
                case ssym "a" :: SymInteger of
                  Con _ -> assertFailure "Bad match"
                  _ -> return ()
            ],
          testGroup
            "ITEOp"
            [ testCase "ites" $
                ites (ssym "a" :: SymBool) (ssym "b" :: SymInteger) (ssym "c")
                  @=? SymInteger (pevalITETerm (ssymTerm "a") (ssymTerm "b") (ssymTerm "c"))
            ],
          testCase "Mergeable" $ do
            let SimpleStrategy s = rootStrategy :: MergingStrategy (SymInteger)
            s (ssym "a") (ssym "b") (ssym "c")
              @=? ites (ssym "a" :: SymBool) (ssym "b" :: SymInteger) (ssym "c"),
          testCase "SimpleMergeable" $ do
            mrgIte (ssym "a" :: SymBool) (ssym "b") (ssym "c")
              @=? ites (ssym "a" :: SymBool) (ssym "b" :: SymInteger) (ssym "c"),
          testCase "IsString" $ do
            ("a" :: SymBool) @=? SymBool (ssymTerm "a"),
          testGroup
            "ToSym"
            [ testCase "From self" $ do
                toSym (ssym "a" :: SymBool) @=? (ssym "a" :: SymBool),
              testCase "From concrete" $ do
                toSym True @=? (con True :: SymBool)
            ],
          testGroup
            "ToCon"
            [ testCase "To self" $ do
                toCon (ssym "a" :: SymBool) @=? (Nothing :: Maybe Bool),
              testCase "To concrete" $ do
                toCon True @=? Just True
            ],
          testCase "EvaluateSym" $ do
            let m1 = emptyModel :: Model
            let m2 = insertValue (SimpleSymbol "a") (1 :: Integer) m1
            let m3 = insertValue (SimpleSymbol "b") True m2
            evaluateSym False m3 (ites ("c" :: SymBool) "a" ("a" + "a" :: SymInteger))
              @=? ites ("c" :: SymBool) 1 2
            evaluateSym True m3 (ites ("c" :: SymBool) "a" ("a" + "a" :: SymInteger)) @=? 2,
          testCase "ExtractSymbolics" $ do
            extractSymbolics (ites ("c" :: SymBool) ("a" :: SymInteger) ("b" :: SymInteger))
              @=? SymbolSet
                ( S.fromList
                    [ someTypedSymbol (SimpleSymbol "c" :: TypedSymbol Bool),
                      someTypedSymbol (SimpleSymbol "a" :: TypedSymbol Integer),
                      someTypedSymbol (SimpleSymbol "b" :: TypedSymbol Integer)
                    ]
                ),
          testCase "GenSym" $ do
            (genSym () "a" :: UnionM (SymBool)) @=? mrgSingle (isym "a" 0)
            (genSymSimple () "a" :: SymBool) @=? isym "a" 0
            (genSym (ssym "a" :: SymBool) "a" :: UnionM (SymBool)) @=? mrgSingle (isym "a" 0)
            (genSymSimple (ssym "a" :: SymBool) "a" :: SymBool) @=? isym "a" 0
            (genSym () (nameWithInfo "a" True) :: UnionM (SymBool)) @=? mrgSingle (iinfosym "a" 0 True)
            (genSymSimple () (nameWithInfo "a" True) :: SymBool) @=? iinfosym "a" 0 True,
          testCase "SEq" $ do
            (ssym "a" :: SymBool) ==~ ssym "b" @=? SymBool (pevalEqvTerm (ssymTerm "a" :: Term Bool) (ssymTerm "b"))
            (ssym "a" :: SymBool) /=~ ssym "b" @=? SymBool (pevalNotTerm $ pevalEqvTerm (ssymTerm "a" :: Term Bool) (ssymTerm "b"))
        ],
      testGroup
        "SymBool"
        [ testGroup
            "LogicalOp"
            [ testCase "||~" $ do
                ssym "a" ||~ ssym "b" @=? SymBool (pevalOrTerm (ssymTerm "a") (ssymTerm "b")),
              testCase "&&~" $ do
                ssym "a" &&~ ssym "b" @=? SymBool (pevalAndTerm (ssymTerm "a") (ssymTerm "b")),
              testCase "nots" $ do
                nots (ssym "a") @=? SymBool (pevalNotTerm (ssymTerm "a")),
              testCase "xors" $ do
                xors (ssym "a") (ssym "b") @=? SymBool (pevalXorTerm (ssymTerm "a") (ssymTerm "b")),
              testCase "implies" $ do
                implies (ssym "a") (ssym "b") @=? SymBool (pevalImplyTerm (ssymTerm "a") (ssymTerm "b"))
            ]
        ],
      testGroup
        "SymInteger"
        [ testGroup
            "Num"
            [ testCase "fromInteger" $ do
                (1 :: SymInteger) @=? SymInteger (conTerm 1),
              testCase "(+)" $ do
                (ssym "a" :: SymInteger) + ssym "b" @=? SymInteger (pevalAddNumTerm (ssymTerm "a") (ssymTerm "b")),
              testCase "(-)" $ do
                (ssym "a" :: SymInteger) - ssym "b" @=? SymInteger (pevalMinusNumTerm (ssymTerm "a") (ssymTerm "b")),
              testCase "(*)" $ do
                (ssym "a" :: SymInteger) * ssym "b" @=? SymInteger (pevalTimesNumTerm (ssymTerm "a") (ssymTerm "b")),
              testCase "negate" $ do
                negate (ssym "a" :: SymInteger) @=? SymInteger (pevalUMinusNumTerm (ssymTerm "a")),
              testCase "abs" $ do
                abs (ssym "a" :: SymInteger) @=? SymInteger (pevalAbsNumTerm (ssymTerm "a")),
              testCase "signum" $ do
                signum (ssym "a" :: SymInteger) @=? SymInteger (pevalSignumNumTerm (ssymTerm "a"))
            ],
          testGroup
            "SignedDivMod"
            [ testProperty "divs on concrete" $ \(i :: Integer, j :: Integer) ->
                ioProperty $
                  divs (con i :: SymInteger) (con j)
                    @=? if j == 0
                      then merge $ throwError () :: ExceptT () UnionM SymInteger
                      else mrgSingle $ con $ i `div` j,
              testCase "divs when divided by zero" $ do
                divs (ssym "a" :: SymInteger) (con 0)
                  @=? (merge $ throwError () :: ExceptT () UnionM SymInteger),
              testCase "divs on symbolic" $ do
                divs (ssym "a" :: SymInteger) (ssym "b")
                  @=? ( mrgIf
                          ((ssym "b" :: SymInteger) ==~ con (0 :: Integer) :: SymBool)
                          (throwError ())
                          (mrgSingle $ SymInteger $ pevalDivIntegerTerm (ssymTerm "a") (ssymTerm "b")) ::
                          ExceptT () UnionM SymInteger
                      ),
              testProperty "mods on concrete" $ \(i :: Integer, j :: Integer) ->
                ioProperty $
                  mods (con i :: SymInteger) (con j)
                    @=? if j == 0
                      then merge $ throwError () :: ExceptT () UnionM SymInteger
                      else mrgSingle $ con $ i `mod` j,
              testCase "mods when divided by zero" $ do
                mods (ssym "a" :: SymInteger) (con 0)
                  @=? (merge $ throwError () :: ExceptT () UnionM SymInteger),
              testCase "mods on symbolic" $ do
                mods (ssym "a" :: SymInteger) (ssym "b")
                  @=? ( mrgIf
                          ((ssym "b" :: SymInteger) ==~ con (0 :: Integer) :: SymBool)
                          (throwError ())
                          (mrgSingle $ SymInteger $ pevalModIntegerTerm (ssymTerm "a") (ssymTerm "b")) ::
                          ExceptT () UnionM SymInteger
                      )
            ],
          testGroup
            "SOrd"
            [ testProperty "SOrd on concrete" $ \(i :: Integer, j :: Integer) -> ioProperty $ do
                (con i :: SymInteger) <=~ con j @=? (con (i <= j) :: SymBool)
                (con i :: SymInteger) <~ con j @=? (con (i < j) :: SymBool)
                (con i :: SymInteger) >=~ con j @=? (con (i >= j) :: SymBool)
                (con i :: SymInteger) >~ con j @=? (con (i > j) :: SymBool)
                (con i :: SymInteger)
                  `symCompare` con j
                  @=? (i `symCompare` j :: UnionM Ordering),
              testCase "SOrd on symbolic" $ do
                let a :: SymInteger = ssym "a"
                let b :: SymInteger = ssym "b"
                let at :: Term Integer = ssymTerm "a"
                let bt :: Term Integer = ssymTerm "b"
                a <=~ b @=? SymBool (pevalLeNumTerm at bt)
                a <~ b @=? SymBool (pevalLtNumTerm at bt)
                a >=~ b @=? SymBool (pevalGeNumTerm at bt)
                a >~ b @=? SymBool (pevalGtNumTerm at bt)
                (a `symCompare` ssym "b" :: UnionM Ordering)
                  @=? mrgIf (a <~ b) (mrgSingle LT) (mrgIf (a ==~ b) (mrgSingle EQ) (mrgSingle GT))
            ]
        ],
      let au :: SymWordN 4 = ssym "a"
          bu :: SymWordN 4 = ssym "b"
          as :: SymIntN 4 = ssym "a"
          bs :: SymIntN 4 = ssym "b"
          aut :: Term (WordN 4) = ssymTerm "a"
          but :: Term (WordN 4) = ssymTerm "b"
          ast :: Term (IntN 4) = ssymTerm "a"
          bst :: Term (IntN 4) = ssymTerm "b"
       in testGroup
            "Sym BV"
            [ testGroup
                "Num"
                [ testCase "fromInteger" $ do
                    (1 :: SymWordN 4) @=? SymWordN (conTerm 1)
                    (1 :: SymIntN 4) @=? SymIntN (conTerm 1),
                  testCase "(+)" $ do
                    au + bu @=? SymWordN (pevalAddNumTerm aut but)
                    as + bs @=? SymIntN (pevalAddNumTerm ast bst),
                  testCase "(-)" $ do
                    au - bu @=? SymWordN (pevalMinusNumTerm aut but)
                    as - bs @=? SymIntN (pevalMinusNumTerm ast bst),
                  testCase "(*)" $ do
                    au * bu @=? SymWordN (pevalTimesNumTerm aut but)
                    as * bs @=? SymIntN (pevalTimesNumTerm ast bst),
                  testCase "negate" $ do
                    negate au @=? SymWordN (pevalUMinusNumTerm aut)
                    negate as @=? SymIntN (pevalUMinusNumTerm ast),
                  testCase "abs" $ do
                    abs au @=? SymWordN (pevalAbsNumTerm aut)
                    abs as @=? SymIntN (pevalAbsNumTerm ast),
                  testCase "signum" $ do
                    signum au @=? SymWordN (pevalSignumNumTerm aut)
                    signum as @=? SymIntN (pevalSignumNumTerm ast)
                ],
              testGroup
                "SOrd"
                [ testProperty "SOrd on concrete" $ \(i :: Integer, j :: Integer) -> ioProperty $ do
                    let iu :: WordN 4 = fromInteger i
                    let ju :: WordN 4 = fromInteger j
                    let is :: IntN 4 = fromInteger i
                    let js :: IntN 4 = fromInteger j
                    let normalizeu k = k - k `div` 16 * 16
                    let normalizes k = if normalizeu k >= 8 then normalizeu k - 16 else normalizeu k
                    (con iu :: SymWordN 4) <=~ con ju @=? (con (normalizeu i <= normalizeu j) :: SymBool)
                    (con iu :: SymWordN 4) <~ con ju @=? (con (normalizeu i < normalizeu j) :: SymBool)
                    (con iu :: SymWordN 4) >=~ con ju @=? (con (normalizeu i >= normalizeu j) :: SymBool)
                    (con iu :: SymWordN 4) >~ con ju @=? (con (normalizeu i > normalizeu j) :: SymBool)
                    (con iu :: SymWordN 4)
                      `symCompare` con ju
                      @=? (normalizeu i `symCompare` normalizeu j :: UnionM Ordering)
                    (con is :: SymIntN 4) <=~ con js @=? (con (normalizes i <= normalizes j) :: SymBool)
                    (con is :: SymIntN 4) <~ con js @=? (con (normalizes i < normalizes j) :: SymBool)
                    (con is :: SymIntN 4) >=~ con js @=? (con (normalizes i >= normalizes j) :: SymBool)
                    (con is :: SymIntN 4) >~ con js @=? (con (normalizes i > normalizes j) :: SymBool)
                    (con is :: SymIntN 4)
                      `symCompare` con js
                      @=? (normalizes i `symCompare` normalizes j :: UnionM Ordering),
                  testCase "SOrd on symbolic" $ do
                    au <=~ bu @=? SymBool (pevalLeNumTerm aut but)
                    au <~ bu @=? SymBool (pevalLtNumTerm aut but)
                    au >=~ bu @=? SymBool (pevalGeNumTerm aut but)
                    au >~ bu @=? SymBool (pevalGtNumTerm aut but)
                    (au `symCompare` bu :: UnionM Ordering)
                      @=? mrgIf (au <~ bu) (mrgSingle LT) (mrgIf (au ==~ bu) (mrgSingle EQ) (mrgSingle GT))

                    as <=~ bs @=? SymBool (pevalLeNumTerm ast bst)
                    as <~ bs @=? SymBool (pevalLtNumTerm ast bst)
                    as >=~ bs @=? SymBool (pevalGeNumTerm ast bst)
                    as >~ bs @=? SymBool (pevalGtNumTerm ast bst)
                    (as `symCompare` bs :: UnionM Ordering)
                      @=? mrgIf (as <~ bs) (mrgSingle LT) (mrgIf (as ==~ bs) (mrgSingle EQ) (mrgSingle GT))
                ],
              testGroup
                "Bits"
                [ testCase ".&." $ do
                    au .&. bu @=? SymWordN (pevalAndBitsTerm aut but)
                    as .&. bs @=? SymIntN (pevalAndBitsTerm ast bst),
                  testCase ".|." $ do
                    au .|. bu @=? SymWordN (pevalOrBitsTerm aut but)
                    as .|. bs @=? SymIntN (pevalOrBitsTerm ast bst),
                  testCase "xor" $ do
                    au `xor` bu @=? SymWordN (pevalXorBitsTerm aut but)
                    as `xor` bs @=? SymIntN (pevalXorBitsTerm ast bst),
                  testCase "complement" $ do
                    complement au @=? SymWordN (pevalComplementBitsTerm aut)
                    complement as @=? SymIntN (pevalComplementBitsTerm ast),
                  testCase "shift" $ do
                    shift au 1 @=? SymWordN (pevalShiftBitsTerm aut 1)
                    shift as 1 @=? SymIntN (pevalShiftBitsTerm ast 1),
                  testCase "rotate" $ do
                    rotate au 1 @=? SymWordN (pevalRotateBitsTerm aut 1)
                    rotate as 1 @=? SymIntN (pevalRotateBitsTerm ast 1),
                  testCase "bitSize" $ do
                    bitSizeMaybe au @=? Just 4
                    bitSizeMaybe as @=? Just 4,
                  testCase "isSigned" $ do
                    isSigned au @=? False
                    isSigned as @=? True,
                  testCase "testBit would only work on concrete ones" $ do
                    testBit (con 3 :: SymWordN 4) 1 @=? True
                    testBit (con 3 :: SymWordN 4) 2 @=? False
                    testBit (con 3 :: SymIntN 4) 1 @=? True
                    testBit (con 3 :: SymIntN 4) 2 @=? False,
                  testCase "bit would work" $ do
                    bit 1 @=? (con 2 :: SymWordN 4)
                    bit 1 @=? (con 2 :: SymIntN 4),
                  testCase "popCount would only work on concrete ones" $ do
                    popCount (con 3 :: SymWordN 4) @=? 2
                    popCount (con 3 :: SymWordN 4) @=? 2
                    popCount (con 3 :: SymIntN 4) @=? 2
                    popCount (con 3 :: SymIntN 4) @=? 2
                ],
              testGroup
                "concatSizedBV"
                [ testCase "concatSizedBV" $ do
                    concatSizedBV
                      (ssym "a" :: SymWordN 4)
                      (ssym "b" :: SymWordN 3)
                      @=? SymWordN
                        ( pevalBVConcatTerm
                            (ssymTerm "a" :: Term (WordN 4))
                            (ssymTerm "b" :: Term (WordN 3))
                        )
                ],
              testGroup
                "extSizedBV for Sym BV"
                [ testCase "zextSizedBV" $ do
                    zextSizedBV (Proxy @6) au @=? SymWordN (pevalBVExtendTerm False (Proxy @6) aut)
                    zextSizedBV (Proxy @6) as @=? SymIntN (pevalBVExtendTerm False (Proxy @6) ast),
                  testCase "sextSizedBV" $ do
                    sextSizedBV (Proxy @6) au @=? SymWordN (pevalBVExtendTerm True (Proxy @6) aut)
                    sextSizedBV (Proxy @6) as @=? SymIntN (pevalBVExtendTerm True (Proxy @6) ast),
                  testCase "extSizedBV" $ do
                    extSizedBV (Proxy @6) au @=? SymWordN (pevalBVExtendTerm False (Proxy @6) aut)
                    extSizedBV (Proxy @6) as @=? SymIntN (pevalBVExtendTerm True (Proxy @6) ast)
                ],
              testGroup
                "selectSizedBV for Sym BV"
                [ testCase "selectSizedBV" $ do
                    selectSizedBV (Proxy @2) (Proxy @1) au
                      @=? SymWordN (pevalBVSelectTerm (Proxy @2) (Proxy @1) aut)
                    selectSizedBV (Proxy @2) (Proxy @1) as
                      @=? SymIntN (pevalBVSelectTerm (Proxy @2) (Proxy @1) ast)
                ],
              testGroup
                "conversion between Int8 and Sym BV"
                [ testCase "toSym" $ do
                    toSym (0 :: Int8) @=? (con 0 :: SymIntN 8)
                    toSym (-127 :: Int8) @=? (con $ -127 :: SymIntN 8)
                    toSym (-128 :: Int8) @=? (con $ -128 :: SymIntN 8)
                    toSym (127 :: Int8) @=? (con 127 :: SymIntN 8),
                  testCase "toCon" $ do
                    toCon (con 0 :: SymIntN 8) @=? Just (0 :: Int8)
                    toCon (con $ -127 :: SymIntN 8) @=? Just (-127 :: Int8)
                    toCon (con $ -128 :: SymIntN 8) @=? Just (-128 :: Int8)
                    toCon (con 127 :: SymIntN 8) @=? Just (127 :: Int8)
                ],
              testGroup
                "conversion between Word8 and Sym BV"
                [ testCase "toSym" $ do
                    toSym (0 :: Word8) @=? (con 0 :: SymWordN 8)
                    toSym (1 :: Word8) @=? (con 1 :: SymWordN 8)
                    toSym (255 :: Word8) @=? (con 255 :: SymWordN 8),
                  testCase "toCon" $ do
                    toCon (con 0 :: SymWordN 8) @=? Just (0 :: Word8)
                    toCon (con 1 :: SymWordN 8) @=? Just (1 :: Word8)
                    toCon (con 255 :: SymWordN 8) @=? Just (255 :: Word8)
                ]
            ],
      testGroup
        "TabularFun"
        [ testCase "apply" $ do
            (ssym "a" :: Integer =~> Integer)
              # ssym "b"
              @=? SymInteger (pevalTabularFunApplyTerm (ssymTerm "a" :: Term (Integer =-> Integer)) (ssymTerm "b"))
        ],
      testGroup
        "GeneralFun"
        [ testCase "evaluate" $ do
            evaluateSym
              False
              (buildModel ("a" := (1 :: Integer), "b" := (2 :: Integer)))
              (con ("a" --> "a" + "b") :: Integer -~> Integer)
              @=? (con ("a" --> "a" + 2) :: Integer -~> Integer)
            evaluateSym
              False
              (buildModel ("a" := (1 :: Integer), "b" := (2 :: Integer), "c" := (3 :: Integer)))
              (con ("a" --> (con $ "b" --> "a" + "b" + "c")) :: Integer -~> Integer --> Integer)
              @=? (con ("a" --> (con $ "b" --> "a" + "b" + 3) :: Integer --> Integer --> Integer))
        ],
      testGroup
        "Symbolic size"
        [ testCase "symSize" $ do
            symSize (ssym "a" :: SymInteger) @=? 1
            symSize (con 1 :: SymInteger) @=? 1
            symSize (con 1 + ssym "a" :: SymInteger) @=? 3
            symSize (ssym "a" + ssym "a" :: SymInteger) @=? 2
            symSize (-(ssym "a") :: SymInteger) @=? 2
            symSize (ites (ssym "a" :: SymBool) (ssym "b") (ssym "c") :: SymInteger) @=? 4,
          testCase "symsSize" $ do
            symsSize [ssym "a" :: SymInteger, ssym "a" + ssym "a"] @=? 2
        ],
      let asymbol :: TypedSymbol Integer = "a"
          bsymbol :: TypedSymbol Bool = "b"
          csymbol :: TypedSymbol Integer = "c"
          dsymbol :: TypedSymbol Bool = "d"
          esymbol :: TypedSymbol (WordN 4) = "e"
          fsymbol :: TypedSymbol (IntN 4) = "f"
          gsymbol :: TypedSymbol (WordN 16) = "g"
          hsymbol :: TypedSymbol (IntN 16) = "h"
          a :: SymInteger = ssym "a"
          b :: SymBool = "b"
          c :: SymInteger = "c"
          d :: SymBool = "d"
          e :: SymWordN 4 = "e"
          f :: SymIntN 4 = "f"
          g :: SymWordN 16 = "g"
          h :: SymIntN 16 = "h"
          va :: Integer = 1
          vc :: Integer = 2
          ve :: WordN 4 = 3
          vf :: IntN 4 = 4
          vg :: WordN 16 = 5
          vh :: IntN 16 = 6
       in testCase
            "construting Model from ModelSymPair"
            $ do
              buildModel ("a" := va) @=? Model (M.singleton (someTypedSymbol asymbol) (toModelValue va))
              buildModel ("a" := va, "b" := True)
                @=? Model
                  ( M.fromList
                      [ (someTypedSymbol asymbol, toModelValue va),
                        (someTypedSymbol bsymbol, toModelValue True)
                      ]
                  )
              buildModel
                ( "a" := va,
                  "b" := True,
                  "c" := vc
                )
                @=? Model
                  ( M.fromList
                      [ (someTypedSymbol asymbol, toModelValue va),
                        (someTypedSymbol bsymbol, toModelValue True),
                        (someTypedSymbol csymbol, toModelValue vc)
                      ]
                  )
              buildModel
                ( "a" := va,
                  "b" := True,
                  "c" := vc,
                  "d" := False
                )
                @=? Model
                  ( M.fromList
                      [ (someTypedSymbol asymbol, toModelValue va),
                        (someTypedSymbol bsymbol, toModelValue True),
                        (someTypedSymbol csymbol, toModelValue vc),
                        (someTypedSymbol dsymbol, toModelValue False)
                      ]
                  )
              buildModel
                ( "a" := va,
                  "b" := True,
                  "c" := vc,
                  "d" := False,
                  "e" := ve
                )
                @=? Model
                  ( M.fromList
                      [ (someTypedSymbol asymbol, toModelValue va),
                        (someTypedSymbol bsymbol, toModelValue True),
                        (someTypedSymbol csymbol, toModelValue vc),
                        (someTypedSymbol dsymbol, toModelValue False),
                        (someTypedSymbol esymbol, toModelValue ve)
                      ]
                  )
              buildModel
                ( "a" := va,
                  "b" := True,
                  "c" := vc,
                  "d" := False,
                  "e" := ve,
                  "f" := vf
                )
                @=? Model
                  ( M.fromList
                      [ (someTypedSymbol asymbol, toModelValue va),
                        (someTypedSymbol bsymbol, toModelValue True),
                        (someTypedSymbol csymbol, toModelValue vc),
                        (someTypedSymbol dsymbol, toModelValue False),
                        (someTypedSymbol esymbol, toModelValue ve),
                        (someTypedSymbol fsymbol, toModelValue vf)
                      ]
                  )
              buildModel
                ( "a" := va,
                  "b" := True,
                  "c" := vc,
                  "d" := False,
                  "e" := ve,
                  "f" := vf,
                  "g" := vg
                )
                @=? Model
                  ( M.fromList
                      [ (someTypedSymbol asymbol, toModelValue va),
                        (someTypedSymbol bsymbol, toModelValue True),
                        (someTypedSymbol csymbol, toModelValue vc),
                        (someTypedSymbol dsymbol, toModelValue False),
                        (someTypedSymbol esymbol, toModelValue ve),
                        (someTypedSymbol fsymbol, toModelValue vf),
                        (someTypedSymbol gsymbol, toModelValue vg)
                      ]
                  )
              buildModel
                ( "a" := va,
                  "b" := True,
                  "c" := vc,
                  "d" := False,
                  "e" := ve,
                  "f" := vf,
                  "g" := vg,
                  "h" := vh
                )
                @=? Model
                  ( M.fromList
                      [ (someTypedSymbol asymbol, toModelValue va),
                        (someTypedSymbol bsymbol, toModelValue True),
                        (someTypedSymbol csymbol, toModelValue vc),
                        (someTypedSymbol dsymbol, toModelValue False),
                        (someTypedSymbol esymbol, toModelValue ve),
                        (someTypedSymbol fsymbol, toModelValue vf),
                        (someTypedSymbol gsymbol, toModelValue vg),
                        (someTypedSymbol hsymbol, toModelValue vh)
                      ]
                  )
    ]
