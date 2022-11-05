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
import Grisette.Core.Data.Class.BitVector
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Evaluate
import Grisette.Core.Data.Class.ExtractSymbolics
import Grisette.Core.Data.Class.Function
import Grisette.Core.Data.Class.GenSym
import Grisette.Core.Data.Class.Integer
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Data.Class.ModelOps
import Grisette.Core.Data.Class.PrimWrapper
import Grisette.Core.Data.Class.SOrd
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Core.Data.Class.ToCon
import Grisette.Core.Data.Class.ToSym
import Grisette.IR.SymPrim.Control.Monad.UnionM
import Grisette.IR.SymPrim.Data.BV
import Grisette.IR.SymPrim.Data.Class.Evaluate
import Grisette.IR.SymPrim.Data.Class.SEq
import Grisette.IR.SymPrim.Data.Class.SOrd
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Grisette.IR.SymPrim.Data.Prim.Model
import Grisette.IR.SymPrim.Data.Prim.ModelValue
import Grisette.IR.SymPrim.Data.Prim.PartialEval.BV
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bits
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bool
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Integer
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Num
import Grisette.IR.SymPrim.Data.Prim.PartialEval.TabularFunc
import Grisette.IR.SymPrim.Data.SymPrim
import Grisette.IR.SymPrim.Data.TabularFunc
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding ((.&.))
import Type.Reflection

symPrimTests :: TestTree
symPrimTests =
  testGroup
    "SymPrimTests"
    [ testGroup
        "General SymPrim"
        [ testGroup
            "PrimWrapper"
            [ testCase "conc" $ do
                (conc 1 :: Sym Integer) @=? Sym (concTerm 1),
              testCase "ssymb" $ do
                (ssymb "a" :: Sym Integer) @=? Sym (ssymbTerm "a"),
              testCase "isymb" $ do
                (isymb "a" 1 :: Sym Integer) @=? Sym (isymbTerm "a" 1),
              testCase "concView" $ do
                concView (conc 1 :: Sym Integer) @=? Just 1
                concView (ssymb "a" :: Sym Integer) @=? Nothing
                case conc 1 :: Sym Integer of
                  Conc 1 -> return ()
                  _ -> assertFailure "Bad match"
                case ssymb "a" :: Sym Integer of
                  Conc _ -> assertFailure "Bad match"
                  _ -> return ()
            ],
          testGroup
            "ITEOp"
            [ testCase "ites" $
                ites (ssymb "a" :: Sym Bool) (ssymb "b" :: Sym Integer) (ssymb "c")
                  @=? Sym (pevalITETerm (ssymbTerm "a") (ssymbTerm "b") (ssymbTerm "c"))
            ],
          testCase "Mergeable" $ do
            let SimpleStrategy s = mergingStrategy :: MergingStrategy (Sym Bool) (Sym Integer)
            s (ssymb "a") (ssymb "b") (ssymb "c")
              @=? ites (ssymb "a" :: Sym Bool) (ssymb "b" :: Sym Integer) (ssymb "c"),
          testCase "SimpleMergeable" $ do
            mrgIte (ssymb "a" :: Sym Bool) (ssymb "b") (ssymb "c")
              @=? ites (ssymb "a" :: Sym Bool) (ssymb "b" :: Sym Integer) (ssymb "c"),
          testCase "IsString" $ do
            ("a" :: Sym Bool) @=? Sym (ssymbTerm "a"),
          testGroup
            "ToSym"
            [ testCase "From self" $ do
                toSym (ssymb "a" :: Sym Bool) @=? (ssymb "a" :: Sym Bool),
              testCase "From concrete" $ do
                toSym True @=? (conc True :: Sym Bool)
            ],
          testGroup
            "ToCon"
            [ testCase "To self" $ do
                toCon (ssymb "a" :: Sym Bool) @=? (Nothing :: Maybe Bool),
              testCase "To concrete" $ do
                toCon True @=? Just True
            ],
          testCase "EvaluateSym" $ do
            let m1 = emptyModel :: Model
            let m2 = insertValue (SimpleSymbol "a") (1 :: Integer) m1
            let m3 = insertValue (SimpleSymbol "b") True m2
            evaluateSym False m3 (ites ("c" :: Sym Bool) "a" ("a" + "a" :: Sym Integer))
              @=? ites ("c" :: Sym Bool) 1 2
            evaluateSym True m3 (ites ("c" :: Sym Bool) "a" ("a" + "a" :: Sym Integer)) @=? 2,
          testCase "ExtractSymbolics" $ do
            extractSymbolics (ites ("c" :: Sym Bool) ("a" :: Sym Integer) ("b" :: Sym Integer))
              @=? SymbolSet
                ( S.fromList
                    [ someTypedSymbol (SimpleSymbol "c" :: TypedSymbol Bool),
                      someTypedSymbol (SimpleSymbol "a" :: TypedSymbol Integer),
                      someTypedSymbol (SimpleSymbol "b" :: TypedSymbol Integer)
                    ]
                ),
          testCase "GenSym" $ do
            (genSym () "a" :: UnionM (Sym Bool)) @=? mrgSingle (isymb "a" 0)
            (genSymSimple () "a" :: Sym Bool) @=? isymb "a" 0
            (genSym (ssymb "a" :: Sym Bool) "a" :: UnionM (Sym Bool)) @=? mrgSingle (isymb "a" 0)
            (genSymSimple (ssymb "a" :: Sym Bool) "a" :: Sym Bool) @=? isymb "a" 0
            (genSym () (nameWithInfo "a" True) :: UnionM (Sym Bool)) @=? mrgSingle (iinfosymb "a" 0 True)
            (genSymSimple () (nameWithInfo "a" True) :: Sym Bool) @=? iinfosymb "a" 0 True,
          testCase "SEq" $ do
            (ssymb "a" :: Sym Bool) ==~ ssymb "b" @=? Sym (pevalEqvTerm (ssymbTerm "a" :: Term Bool) (ssymbTerm "b"))
            (ssymb "a" :: Sym Bool) /=~ ssymb "b" @=? Sym (pevalNotTerm $ pevalEqvTerm (ssymbTerm "a" :: Term Bool) (ssymbTerm "b"))
        ],
      testGroup
        "Sym Bool"
        [ testGroup
            "LogicalOp"
            [ testCase "||~" $ do
                ssymb "a" ||~ ssymb "b" @=? Sym (pevalOrTerm (ssymbTerm "a") (ssymbTerm "b")),
              testCase "&&~" $ do
                ssymb "a" &&~ ssymb "b" @=? Sym (pevalAndTerm (ssymbTerm "a") (ssymbTerm "b")),
              testCase "nots" $ do
                nots (ssymb "a") @=? Sym (pevalNotTerm (ssymbTerm "a")),
              testCase "xors" $ do
                xors (ssymb "a") (ssymb "b") @=? Sym (pevalXorTerm (ssymbTerm "a") (ssymbTerm "b")),
              testCase "implies" $ do
                implies (ssymb "a") (ssymb "b") @=? Sym (pevalImplyTerm (ssymbTerm "a") (ssymbTerm "b"))
            ]
        ],
      testGroup
        "Sym Integer"
        [ testGroup
            "Num"
            [ testCase "fromInteger" $ do
                (1 :: Sym Integer) @=? Sym (concTerm 1),
              testCase "(+)" $ do
                (ssymb "a" :: Sym Integer) + ssymb "b" @=? Sym (pevalAddNumTerm (ssymbTerm "a") (ssymbTerm "b")),
              testCase "(-)" $ do
                (ssymb "a" :: Sym Integer) - ssymb "b" @=? Sym (pevalMinusNumTerm (ssymbTerm "a") (ssymbTerm "b")),
              testCase "(*)" $ do
                (ssymb "a" :: Sym Integer) * ssymb "b" @=? Sym (pevalTimesNumTerm (ssymbTerm "a") (ssymbTerm "b")),
              testCase "negate" $ do
                negate (ssymb "a" :: Sym Integer) @=? Sym (pevalUMinusNumTerm (ssymbTerm "a")),
              testCase "abs" $ do
                abs (ssymb "a" :: Sym Integer) @=? Sym (pevalAbsNumTerm (ssymbTerm "a")),
              testCase "signum" $ do
                signum (ssymb "a" :: Sym Integer) @=? Sym (pevalSignumNumTerm (ssymbTerm "a"))
            ],
          testGroup
            "SignedDivMod"
            [ testProperty "divs on concrete" $ \(i :: Integer, j :: Integer) ->
                ioProperty $
                  divs (conc i :: Sym Integer) (conc j)
                    @=? if j == 0
                      then merge $ throwError () :: ExceptT () UnionM SymInteger
                      else mrgSingle $ conc $ i `div` j,
              testCase "divs when divided by zero" $ do
                divs (ssymb "a" :: Sym Integer) (conc 0)
                  @=? (merge $ throwError () :: ExceptT () UnionM SymInteger),
              testCase "divs on symbolic" $ do
                divs (ssymb "a" :: Sym Integer) (ssymb "b")
                  @=? ( mrgIf
                          ((ssymb "b" :: Sym Integer) ==~ conc (0 :: Integer) :: SymBool)
                          (throwError ())
                          (mrgSingle $ Sym $ pevalDivIntegerTerm (ssymbTerm "a") (ssymbTerm "b")) ::
                          ExceptT () UnionM SymInteger
                      ),
              testProperty "mods on concrete" $ \(i :: Integer, j :: Integer) ->
                ioProperty $
                  mods (conc i :: Sym Integer) (conc j)
                    @=? if j == 0
                      then merge $ throwError () :: ExceptT () UnionM SymInteger
                      else mrgSingle $ conc $ i `mod` j,
              testCase "mods when divided by zero" $ do
                mods (ssymb "a" :: Sym Integer) (conc 0)
                  @=? (merge $ throwError () :: ExceptT () UnionM SymInteger),
              testCase "mods on symbolic" $ do
                mods (ssymb "a" :: Sym Integer) (ssymb "b")
                  @=? ( mrgIf
                          ((ssymb "b" :: Sym Integer) ==~ conc (0 :: Integer) :: SymBool)
                          (throwError ())
                          (mrgSingle $ Sym $ pevalModIntegerTerm (ssymbTerm "a") (ssymbTerm "b")) ::
                          ExceptT () UnionM SymInteger
                      )
            ],
          testGroup
            "SOrd"
            [ testProperty "SOrd on concrete" $ \(i :: Integer, j :: Integer) -> ioProperty $ do
                (conc i :: Sym Integer) <=~ conc j @=? (conc (i <= j) :: SymBool)
                (conc i :: Sym Integer) <~ conc j @=? (conc (i < j) :: SymBool)
                (conc i :: Sym Integer) >=~ conc j @=? (conc (i >= j) :: SymBool)
                (conc i :: Sym Integer) >~ conc j @=? (conc (i > j) :: SymBool)
                (conc i :: Sym Integer)
                  `symCompare` conc j
                  @=? (i `symCompare` j :: UnionM Ordering),
              testCase "SOrd on symbolic" $ do
                let a :: Sym Integer = ssymb "a"
                let b :: Sym Integer = ssymb "b"
                let at :: Term Integer = ssymbTerm "a"
                let bt :: Term Integer = ssymbTerm "b"
                a <=~ b @=? Sym (pevalLeNumTerm at bt)
                a <~ b @=? Sym (pevalLtNumTerm at bt)
                a >=~ b @=? Sym (pevalGeNumTerm at bt)
                a >~ b @=? Sym (pevalGtNumTerm at bt)
                (a `symCompare` ssymb "b" :: UnionM Ordering)
                  @=? mrgIf (a <~ b) (mrgSingle LT) (mrgIf (a ==~ b) (mrgSingle EQ) (mrgSingle GT))
            ]
        ],
      let au :: Sym (WordN 4) = ssymb "a"
          bu :: Sym (WordN 4) = ssymb "b"
          as :: Sym (IntN 4) = ssymb "a"
          bs :: Sym (IntN 4) = ssymb "b"
          aut :: Term (WordN 4) = ssymbTerm "a"
          but :: Term (WordN 4) = ssymbTerm "b"
          ast :: Term (IntN 4) = ssymbTerm "a"
          bst :: Term (IntN 4) = ssymbTerm "b"
       in testGroup
            "Sym BV"
            [ testGroup
                "Num"
                [ testCase "fromInteger" $ do
                    (1 :: Sym (WordN 4)) @=? Sym (concTerm 1)
                    (1 :: Sym (IntN 4)) @=? Sym (concTerm 1),
                  testCase "(+)" $ do
                    au + bu @=? Sym (pevalAddNumTerm aut but)
                    as + bs @=? Sym (pevalAddNumTerm ast bst),
                  testCase "(-)" $ do
                    au - bu @=? Sym (pevalMinusNumTerm aut but)
                    as - bs @=? Sym (pevalMinusNumTerm ast bst),
                  testCase "(*)" $ do
                    au * bu @=? Sym (pevalTimesNumTerm aut but)
                    as * bs @=? Sym (pevalTimesNumTerm ast bst),
                  testCase "negate" $ do
                    negate au @=? Sym (pevalUMinusNumTerm aut)
                    negate as @=? Sym (pevalUMinusNumTerm ast),
                  testCase "abs" $ do
                    abs au @=? Sym (pevalAbsNumTerm aut)
                    abs as @=? Sym (pevalAbsNumTerm ast),
                  testCase "signum" $ do
                    signum au @=? Sym (pevalSignumNumTerm aut)
                    signum as @=? Sym (pevalSignumNumTerm ast)
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
                    (conc iu :: Sym (WordN 4)) <=~ conc ju @=? (conc (normalizeu i <= normalizeu j) :: SymBool)
                    (conc iu :: Sym (WordN 4)) <~ conc ju @=? (conc (normalizeu i < normalizeu j) :: SymBool)
                    (conc iu :: Sym (WordN 4)) >=~ conc ju @=? (conc (normalizeu i >= normalizeu j) :: SymBool)
                    (conc iu :: Sym (WordN 4)) >~ conc ju @=? (conc (normalizeu i > normalizeu j) :: SymBool)
                    (conc iu :: Sym (WordN 4))
                      `symCompare` conc ju
                      @=? (normalizeu i `symCompare` normalizeu j :: UnionM Ordering)
                    (conc is :: Sym (IntN 4)) <=~ conc js @=? (conc (normalizes i <= normalizes j) :: SymBool)
                    (conc is :: Sym (IntN 4)) <~ conc js @=? (conc (normalizes i < normalizes j) :: SymBool)
                    (conc is :: Sym (IntN 4)) >=~ conc js @=? (conc (normalizes i >= normalizes j) :: SymBool)
                    (conc is :: Sym (IntN 4)) >~ conc js @=? (conc (normalizes i > normalizes j) :: SymBool)
                    (conc is :: Sym (IntN 4))
                      `symCompare` conc js
                      @=? (normalizes i `symCompare` normalizes j :: UnionM Ordering),
                  testCase "SOrd on symbolic" $ do
                    au <=~ bu @=? Sym (pevalLeNumTerm aut but)
                    au <~ bu @=? Sym (pevalLtNumTerm aut but)
                    au >=~ bu @=? Sym (pevalGeNumTerm aut but)
                    au >~ bu @=? Sym (pevalGtNumTerm aut but)
                    (au `symCompare` bu :: UnionM Ordering)
                      @=? mrgIf (au <~ bu) (mrgSingle LT) (mrgIf (au ==~ bu) (mrgSingle EQ) (mrgSingle GT))

                    as <=~ bs @=? Sym (pevalLeNumTerm ast bst)
                    as <~ bs @=? Sym (pevalLtNumTerm ast bst)
                    as >=~ bs @=? Sym (pevalGeNumTerm ast bst)
                    as >~ bs @=? Sym (pevalGtNumTerm ast bst)
                    (as `symCompare` bs :: UnionM Ordering)
                      @=? mrgIf (as <~ bs) (mrgSingle LT) (mrgIf (as ==~ bs) (mrgSingle EQ) (mrgSingle GT))
                ],
              testGroup
                "Bits"
                [ testCase ".&." $ do
                    au .&. bu @=? Sym (pevalAndBitsTerm aut but)
                    as .&. bs @=? Sym (pevalAndBitsTerm ast bst),
                  testCase ".|." $ do
                    au .|. bu @=? Sym (pevalOrBitsTerm aut but)
                    as .|. bs @=? Sym (pevalOrBitsTerm ast bst),
                  testCase "xor" $ do
                    au `xor` bu @=? Sym (pevalXorBitsTerm aut but)
                    as `xor` bs @=? Sym (pevalXorBitsTerm ast bst),
                  testCase "complement" $ do
                    complement au @=? Sym (pevalComplementBitsTerm aut)
                    complement as @=? Sym (pevalComplementBitsTerm ast),
                  testCase "shift" $ do
                    shift au 1 @=? Sym (pevalShiftBitsTerm aut 1)
                    shift as 1 @=? Sym (pevalShiftBitsTerm ast 1),
                  testCase "rotate" $ do
                    rotate au 1 @=? Sym (pevalRotateBitsTerm aut 1)
                    rotate as 1 @=? Sym (pevalRotateBitsTerm ast 1),
                  testCase "bitSize" $ do
                    bitSizeMaybe au @=? Just 4
                    bitSizeMaybe as @=? Just 4,
                  testCase "isSigned" $ do
                    isSigned au @=? False
                    isSigned as @=? True,
                  testCase "testBit would only work on concrete ones" $ do
                    testBit (Conc 3 :: Sym (WordN 4)) 1 @=? True
                    testBit (Conc 3 :: Sym (WordN 4)) 2 @=? False
                    testBit (Conc 3 :: Sym (IntN 4)) 1 @=? True
                    testBit (Conc 3 :: Sym (IntN 4)) 2 @=? False,
                  testCase "bit would work" $ do
                    bit 1 @=? (Conc 2 :: Sym (WordN 4))
                    bit 1 @=? (Conc 2 :: Sym (IntN 4)),
                  testCase "popCount would only work on concrete ones" $ do
                    popCount (Conc 3 :: Sym (WordN 4)) @=? 2
                    popCount (Conc 3 :: Sym (WordN 4)) @=? 2
                    popCount (Conc 3 :: Sym (IntN 4)) @=? 2
                    popCount (Conc 3 :: Sym (IntN 4)) @=? 2
                ],
              testGroup
                "BVConcat"
                [ testCase "bvconcat" $ do
                    bvconcat
                      (ssymb "a" :: Sym (WordN 4))
                      (ssymb "b" :: Sym (WordN 3))
                      @=? Sym
                        ( pevalBVConcatTerm
                            (ssymbTerm "a" :: Term (WordN 4))
                            (ssymbTerm "b" :: Term (WordN 3))
                        )
                ],
              testGroup
                "bvextend for Sym BV"
                [ testCase "bvzeroExtend" $ do
                    bvzeroExtend (Proxy @6) au @=? Sym (pevalBVExtendTerm False (Proxy @6) aut)
                    bvzeroExtend (Proxy @6) as @=? Sym (pevalBVExtendTerm False (Proxy @6) ast),
                  testCase "bvsignExtend" $ do
                    bvsignExtend (Proxy @6) au @=? Sym (pevalBVExtendTerm True (Proxy @6) aut)
                    bvsignExtend (Proxy @6) as @=? Sym (pevalBVExtendTerm True (Proxy @6) ast),
                  testCase "bvextend" $ do
                    bvextend (Proxy @6) au @=? Sym (pevalBVExtendTerm False (Proxy @6) aut)
                    bvextend (Proxy @6) as @=? Sym (pevalBVExtendTerm True (Proxy @6) ast)
                ],
              testGroup
                "bvselect for Sym BV"
                [ testCase "bvselect" $ do
                    bvselect (Proxy @2) (Proxy @1) au
                      @=? Sym (pevalBVSelectTerm (Proxy @2) (Proxy @1) aut)
                    bvselect (Proxy @2) (Proxy @1) as
                      @=? Sym (pevalBVSelectTerm (Proxy @2) (Proxy @1) ast)
                ],
              testGroup
                "conversion between Int8 and Sym BV"
                [ testCase "toSym" $ do
                    toSym (0 :: Int8) @=? (conc 0 :: SymIntN 8)
                    toSym (-127 :: Int8) @=? (conc $ -127 :: SymIntN 8)
                    toSym (-128 :: Int8) @=? (conc $ -128 :: SymIntN 8)
                    toSym (127 :: Int8) @=? (conc 127 :: SymIntN 8),
                  testCase "toCon" $ do
                    toCon (conc 0 :: SymIntN 8) @=? Just (0 :: Int8)
                    toCon (conc $ -127 :: SymIntN 8) @=? Just (-127 :: Int8)
                    toCon (conc $ -128 :: SymIntN 8) @=? Just (-128 :: Int8)
                    toCon (conc 127 :: SymIntN 8) @=? Just (127 :: Int8)
                ],
              testGroup
                "conversion between Word8 and Sym BV"
                [ testCase "toSym" $ do
                    toSym (0 :: Word8) @=? (conc 0 :: SymWordN 8)
                    toSym (1 :: Word8) @=? (conc 1 :: SymWordN 8)
                    toSym (255 :: Word8) @=? (conc 255 :: SymWordN 8),
                  testCase "toCon" $ do
                    toCon (conc 0 :: SymWordN 8) @=? Just (0 :: Word8)
                    toCon (conc 1 :: SymWordN 8) @=? Just (1 :: Word8)
                    toCon (conc 255 :: SymWordN 8) @=? Just (255 :: Word8)
                ]
            ],
      testGroup
        "TabularFunc"
        [ testCase "apply" $ do
            (ssymb "a" :: Integer =~> Integer)
              # ssymb "b"
              @=? Sym (pevalTabularFuncApplyTerm (ssymbTerm "a" :: Term (Integer =-> Integer)) (ssymbTerm "b"))
        ],
      testGroup
        "Symbolic size"
        [ testCase "symSize" $ do
            symSize (ssymb "a" :: Sym Integer) @=? 1
            symSize (conc 1 :: Sym Integer) @=? 1
            symSize (conc 1 + ssymb "a" :: Sym Integer) @=? 3
            symSize (ssymb "a" + ssymb "a" :: Sym Integer) @=? 2
            symSize (-(ssymb "a") :: Sym Integer) @=? 2
            symSize (ites (ssymb "a" :: Sym Bool) (ssymb "b") (ssymb "c") :: Sym Integer) @=? 4,
          testCase "symsSize" $ do
            symsSize [ssymb "a" :: Sym Integer, ssymb "a" + ssymb "a"] @=? 2
        ],
      let asymbol :: TypedSymbol Integer = "a"
          bsymbol :: TypedSymbol Bool = "b"
          csymbol :: TypedSymbol Integer = "c"
          dsymbol :: TypedSymbol Bool = "d"
          esymbol :: TypedSymbol (WordN 4) = "e"
          fsymbol :: TypedSymbol (IntN 4) = "f"
          gsymbol :: TypedSymbol (WordN 16) = "g"
          hsymbol :: TypedSymbol (IntN 16) = "h"
          a :: Sym Integer = ssymb "a"
          b :: Sym Bool = "b"
          c :: Sym Integer = "c"
          d :: Sym Bool = "d"
          e :: Sym (WordN 4) = "e"
          f :: Sym (IntN 4) = "f"
          g :: Sym (WordN 16) = "g"
          h :: Sym (IntN 16) = "h"
       in testCase
            "construting Model from ModelSymPair"
            $ do
              buildModel (a := 1) @=? Model (M.singleton (someTypedSymbol asymbol) (toModelValue (1 :: Integer)))
              buildModel (a := 1, b := True)
                @=? Model
                  ( M.fromList
                      [ (someTypedSymbol asymbol, toModelValue (1 :: Integer)),
                        (someTypedSymbol bsymbol, toModelValue True)
                      ]
                  )
              buildModel
                ( a := 1,
                  b := True,
                  c := 2
                )
                @=? Model
                  ( M.fromList
                      [ (someTypedSymbol asymbol, toModelValue (1 :: Integer)),
                        (someTypedSymbol bsymbol, toModelValue True),
                        (someTypedSymbol csymbol, toModelValue (2 :: Integer))
                      ]
                  )
              buildModel
                ( a := 1,
                  b := True,
                  c := 2,
                  d := False
                )
                @=? Model
                  ( M.fromList
                      [ (someTypedSymbol asymbol, toModelValue (1 :: Integer)),
                        (someTypedSymbol bsymbol, toModelValue True),
                        (someTypedSymbol csymbol, toModelValue (2 :: Integer)),
                        (someTypedSymbol dsymbol, toModelValue False)
                      ]
                  )
              buildModel
                ( a := 1,
                  b := True,
                  c := 2,
                  d := False,
                  e := 3
                )
                @=? Model
                  ( M.fromList
                      [ (someTypedSymbol asymbol, toModelValue (1 :: Integer)),
                        (someTypedSymbol bsymbol, toModelValue True),
                        (someTypedSymbol csymbol, toModelValue (2 :: Integer)),
                        (someTypedSymbol dsymbol, toModelValue False),
                        (someTypedSymbol esymbol, toModelValue (3 :: WordN 4))
                      ]
                  )
              buildModel
                ( a := 1,
                  b := True,
                  c := 2,
                  d := False,
                  e := 3,
                  f := 4
                )
                @=? Model
                  ( M.fromList
                      [ (someTypedSymbol asymbol, toModelValue (1 :: Integer)),
                        (someTypedSymbol bsymbol, toModelValue True),
                        (someTypedSymbol csymbol, toModelValue (2 :: Integer)),
                        (someTypedSymbol dsymbol, toModelValue False),
                        (someTypedSymbol esymbol, toModelValue (3 :: WordN 4)),
                        (someTypedSymbol fsymbol, toModelValue (4 :: IntN 4))
                      ]
                  )
              buildModel
                ( a := 1,
                  b := True,
                  c := 2,
                  d := False,
                  e := 3,
                  f := 4,
                  g := 5
                )
                @=? Model
                  ( M.fromList
                      [ (someTypedSymbol asymbol, toModelValue (1 :: Integer)),
                        (someTypedSymbol bsymbol, toModelValue True),
                        (someTypedSymbol csymbol, toModelValue (2 :: Integer)),
                        (someTypedSymbol dsymbol, toModelValue False),
                        (someTypedSymbol esymbol, toModelValue (3 :: WordN 4)),
                        (someTypedSymbol fsymbol, toModelValue (4 :: IntN 4)),
                        (someTypedSymbol gsymbol, toModelValue (5 :: WordN 16))
                      ]
                  )
              buildModel
                ( a := 1,
                  b := True,
                  c := 2,
                  d := False,
                  e := 3,
                  f := 4,
                  g := 5,
                  h := 6
                )
                @=? Model
                  ( M.fromList
                      [ (someTypedSymbol asymbol, toModelValue (1 :: Integer)),
                        (someTypedSymbol bsymbol, toModelValue True),
                        (someTypedSymbol csymbol, toModelValue (2 :: Integer)),
                        (someTypedSymbol dsymbol, toModelValue False),
                        (someTypedSymbol esymbol, toModelValue (3 :: WordN 4)),
                        (someTypedSymbol fsymbol, toModelValue (4 :: IntN 4)),
                        (someTypedSymbol gsymbol, toModelValue (5 :: WordN 16)),
                        (someTypedSymbol hsymbol, toModelValue (6 :: IntN 16))
                      ]
                  )
    ]
