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
import Grisette.Core.Data.Class.SOrd
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Core.Data.Class.Solvable
import Grisette.Core.Data.Class.ToCon
import Grisette.Core.Data.Class.ToSym
import Grisette.IR.SymPrim.Control.Monad.UnionM
import Grisette.IR.SymPrim.Data.BV
import Grisette.IR.SymPrim.Data.Class.Evaluate
import Grisette.IR.SymPrim.Data.Class.ExtractSymbolics
import Grisette.IR.SymPrim.Data.Class.GenSym
import Grisette.IR.SymPrim.Data.Class.Mergeable
import Grisette.IR.SymPrim.Data.Class.SEq
import Grisette.IR.SymPrim.Data.Class.SOrd
import Grisette.IR.SymPrim.Data.Class.SimpleMergeable
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
                (con 1 :: Sym Integer) @=? Sym (conTerm 1),
              testCase "ssym" $ do
                (ssym "a" :: Sym Integer) @=? Sym (ssymTerm "a"),
              testCase "isym" $ do
                (isym "a" 1 :: Sym Integer) @=? Sym (isymTerm "a" 1),
              testCase "conView" $ do
                conView (con 1 :: Sym Integer) @=? Just 1
                conView (ssym "a" :: Sym Integer) @=? Nothing
                case con 1 :: Sym Integer of
                  Con 1 -> return ()
                  _ -> assertFailure "Bad match"
                case ssym "a" :: Sym Integer of
                  Con _ -> assertFailure "Bad match"
                  _ -> return ()
            ],
          testGroup
            "ITEOp"
            [ testCase "ites" $
                ites (ssym "a" :: Sym Bool) (ssym "b" :: Sym Integer) (ssym "c")
                  @=? Sym (pevalITETerm (ssymTerm "a") (ssymTerm "b") (ssymTerm "c"))
            ],
          testCase "Mergeable" $ do
            let SimpleStrategy s = rootStrategy :: MergingStrategy (Sym Integer)
            s (ssym "a") (ssym "b") (ssym "c")
              @=? ites (ssym "a" :: Sym Bool) (ssym "b" :: Sym Integer) (ssym "c"),
          testCase "SimpleMergeable" $ do
            mrgIte (ssym "a" :: Sym Bool) (ssym "b") (ssym "c")
              @=? ites (ssym "a" :: Sym Bool) (ssym "b" :: Sym Integer) (ssym "c"),
          testCase "IsString" $ do
            ("a" :: Sym Bool) @=? Sym (ssymTerm "a"),
          testGroup
            "ToSym"
            [ testCase "From self" $ do
                toSym (ssym "a" :: Sym Bool) @=? (ssym "a" :: Sym Bool),
              testCase "From concrete" $ do
                toSym True @=? (con True :: Sym Bool)
            ],
          testGroup
            "ToCon"
            [ testCase "To self" $ do
                toCon (ssym "a" :: Sym Bool) @=? (Nothing :: Maybe Bool),
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
            (genSym () "a" :: UnionM (Sym Bool)) @=? mrgSingle (isym "a" 0)
            (genSymSimple () "a" :: Sym Bool) @=? isym "a" 0
            (genSym (ssym "a" :: Sym Bool) "a" :: UnionM (Sym Bool)) @=? mrgSingle (isym "a" 0)
            (genSymSimple (ssym "a" :: Sym Bool) "a" :: Sym Bool) @=? isym "a" 0
            (genSym () (nameWithInfo "a" True) :: UnionM (Sym Bool)) @=? mrgSingle (iinfosym "a" 0 True)
            (genSymSimple () (nameWithInfo "a" True) :: Sym Bool) @=? iinfosym "a" 0 True,
          testCase "SEq" $ do
            (ssym "a" :: Sym Bool) ==~ ssym "b" @=? Sym (pevalEqvTerm (ssymTerm "a" :: Term Bool) (ssymTerm "b"))
            (ssym "a" :: Sym Bool) /=~ ssym "b" @=? Sym (pevalNotTerm $ pevalEqvTerm (ssymTerm "a" :: Term Bool) (ssymTerm "b"))
        ],
      testGroup
        "Sym Bool"
        [ testGroup
            "LogicalOp"
            [ testCase "||~" $ do
                ssym "a" ||~ ssym "b" @=? Sym (pevalOrTerm (ssymTerm "a") (ssymTerm "b")),
              testCase "&&~" $ do
                ssym "a" &&~ ssym "b" @=? Sym (pevalAndTerm (ssymTerm "a") (ssymTerm "b")),
              testCase "nots" $ do
                nots (ssym "a") @=? Sym (pevalNotTerm (ssymTerm "a")),
              testCase "xors" $ do
                xors (ssym "a") (ssym "b") @=? Sym (pevalXorTerm (ssymTerm "a") (ssymTerm "b")),
              testCase "implies" $ do
                implies (ssym "a") (ssym "b") @=? Sym (pevalImplyTerm (ssymTerm "a") (ssymTerm "b"))
            ]
        ],
      testGroup
        "Sym Integer"
        [ testGroup
            "Num"
            [ testCase "fromInteger" $ do
                (1 :: Sym Integer) @=? Sym (conTerm 1),
              testCase "(+)" $ do
                (ssym "a" :: Sym Integer) + ssym "b" @=? Sym (pevalAddNumTerm (ssymTerm "a") (ssymTerm "b")),
              testCase "(-)" $ do
                (ssym "a" :: Sym Integer) - ssym "b" @=? Sym (pevalMinusNumTerm (ssymTerm "a") (ssymTerm "b")),
              testCase "(*)" $ do
                (ssym "a" :: Sym Integer) * ssym "b" @=? Sym (pevalTimesNumTerm (ssymTerm "a") (ssymTerm "b")),
              testCase "negate" $ do
                negate (ssym "a" :: Sym Integer) @=? Sym (pevalUMinusNumTerm (ssymTerm "a")),
              testCase "abs" $ do
                abs (ssym "a" :: Sym Integer) @=? Sym (pevalAbsNumTerm (ssymTerm "a")),
              testCase "signum" $ do
                signum (ssym "a" :: Sym Integer) @=? Sym (pevalSignumNumTerm (ssymTerm "a"))
            ],
          testGroup
            "SignedDivMod"
            [ testProperty "divs on concrete" $ \(i :: Integer, j :: Integer) ->
                ioProperty $
                  divs (con i :: Sym Integer) (con j)
                    @=? if j == 0
                      then merge $ throwError () :: ExceptT () UnionM SymInteger
                      else mrgSingle $ con $ i `div` j,
              testCase "divs when divided by zero" $ do
                divs (ssym "a" :: Sym Integer) (con 0)
                  @=? (merge $ throwError () :: ExceptT () UnionM SymInteger),
              testCase "divs on symbolic" $ do
                divs (ssym "a" :: Sym Integer) (ssym "b")
                  @=? ( mrgIf
                          ((ssym "b" :: Sym Integer) ==~ con (0 :: Integer) :: SymBool)
                          (throwError ())
                          (mrgSingle $ Sym $ pevalDivIntegerTerm (ssymTerm "a") (ssymTerm "b")) ::
                          ExceptT () UnionM SymInteger
                      ),
              testProperty "mods on concrete" $ \(i :: Integer, j :: Integer) ->
                ioProperty $
                  mods (con i :: Sym Integer) (con j)
                    @=? if j == 0
                      then merge $ throwError () :: ExceptT () UnionM SymInteger
                      else mrgSingle $ con $ i `mod` j,
              testCase "mods when divided by zero" $ do
                mods (ssym "a" :: Sym Integer) (con 0)
                  @=? (merge $ throwError () :: ExceptT () UnionM SymInteger),
              testCase "mods on symbolic" $ do
                mods (ssym "a" :: Sym Integer) (ssym "b")
                  @=? ( mrgIf
                          ((ssym "b" :: Sym Integer) ==~ con (0 :: Integer) :: SymBool)
                          (throwError ())
                          (mrgSingle $ Sym $ pevalModIntegerTerm (ssymTerm "a") (ssymTerm "b")) ::
                          ExceptT () UnionM SymInteger
                      )
            ],
          testGroup
            "SOrd"
            [ testProperty "SOrd on concrete" $ \(i :: Integer, j :: Integer) -> ioProperty $ do
                (con i :: Sym Integer) <=~ con j @=? (con (i <= j) :: SymBool)
                (con i :: Sym Integer) <~ con j @=? (con (i < j) :: SymBool)
                (con i :: Sym Integer) >=~ con j @=? (con (i >= j) :: SymBool)
                (con i :: Sym Integer) >~ con j @=? (con (i > j) :: SymBool)
                (con i :: Sym Integer)
                  `symCompare` con j
                  @=? (i `symCompare` j :: UnionM Ordering),
              testCase "SOrd on symbolic" $ do
                let a :: Sym Integer = ssym "a"
                let b :: Sym Integer = ssym "b"
                let at :: Term Integer = ssymTerm "a"
                let bt :: Term Integer = ssymTerm "b"
                a <=~ b @=? Sym (pevalLeNumTerm at bt)
                a <~ b @=? Sym (pevalLtNumTerm at bt)
                a >=~ b @=? Sym (pevalGeNumTerm at bt)
                a >~ b @=? Sym (pevalGtNumTerm at bt)
                (a `symCompare` ssym "b" :: UnionM Ordering)
                  @=? mrgIf (a <~ b) (mrgSingle LT) (mrgIf (a ==~ b) (mrgSingle EQ) (mrgSingle GT))
            ]
        ],
      let au :: Sym (WordN 4) = ssym "a"
          bu :: Sym (WordN 4) = ssym "b"
          as :: Sym (IntN 4) = ssym "a"
          bs :: Sym (IntN 4) = ssym "b"
          aut :: Term (WordN 4) = ssymTerm "a"
          but :: Term (WordN 4) = ssymTerm "b"
          ast :: Term (IntN 4) = ssymTerm "a"
          bst :: Term (IntN 4) = ssymTerm "b"
       in testGroup
            "Sym BV"
            [ testGroup
                "Num"
                [ testCase "fromInteger" $ do
                    (1 :: Sym (WordN 4)) @=? Sym (conTerm 1)
                    (1 :: Sym (IntN 4)) @=? Sym (conTerm 1),
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
                    (con iu :: Sym (WordN 4)) <=~ con ju @=? (con (normalizeu i <= normalizeu j) :: SymBool)
                    (con iu :: Sym (WordN 4)) <~ con ju @=? (con (normalizeu i < normalizeu j) :: SymBool)
                    (con iu :: Sym (WordN 4)) >=~ con ju @=? (con (normalizeu i >= normalizeu j) :: SymBool)
                    (con iu :: Sym (WordN 4)) >~ con ju @=? (con (normalizeu i > normalizeu j) :: SymBool)
                    (con iu :: Sym (WordN 4))
                      `symCompare` con ju
                      @=? (normalizeu i `symCompare` normalizeu j :: UnionM Ordering)
                    (con is :: Sym (IntN 4)) <=~ con js @=? (con (normalizes i <= normalizes j) :: SymBool)
                    (con is :: Sym (IntN 4)) <~ con js @=? (con (normalizes i < normalizes j) :: SymBool)
                    (con is :: Sym (IntN 4)) >=~ con js @=? (con (normalizes i >= normalizes j) :: SymBool)
                    (con is :: Sym (IntN 4)) >~ con js @=? (con (normalizes i > normalizes j) :: SymBool)
                    (con is :: Sym (IntN 4))
                      `symCompare` con js
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
                    testBit (con 3 :: Sym (WordN 4)) 1 @=? True
                    testBit (con 3 :: Sym (WordN 4)) 2 @=? False
                    testBit (con 3 :: Sym (IntN 4)) 1 @=? True
                    testBit (con 3 :: Sym (IntN 4)) 2 @=? False,
                  testCase "bit would work" $ do
                    bit 1 @=? (con 2 :: Sym (WordN 4))
                    bit 1 @=? (con 2 :: Sym (IntN 4)),
                  testCase "popCount would only work on concrete ones" $ do
                    popCount (con 3 :: Sym (WordN 4)) @=? 2
                    popCount (con 3 :: Sym (WordN 4)) @=? 2
                    popCount (con 3 :: Sym (IntN 4)) @=? 2
                    popCount (con 3 :: Sym (IntN 4)) @=? 2
                ],
              testGroup
                "BVConcat"
                [ testCase "bvconcat" $ do
                    bvconcat
                      (ssym "a" :: Sym (WordN 4))
                      (ssym "b" :: Sym (WordN 3))
                      @=? Sym
                        ( pevalBVConcatTerm
                            (ssymTerm "a" :: Term (WordN 4))
                            (ssymTerm "b" :: Term (WordN 3))
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
              @=? Sym (pevalTabularFunApplyTerm (ssymTerm "a" :: Term (Integer =-> Integer)) (ssymTerm "b"))
        ],
      testGroup
        "Symbolic size"
        [ testCase "symSize" $ do
            symSize (ssym "a" :: Sym Integer) @=? 1
            symSize (con 1 :: Sym Integer) @=? 1
            symSize (con 1 + ssym "a" :: Sym Integer) @=? 3
            symSize (ssym "a" + ssym "a" :: Sym Integer) @=? 2
            symSize (-(ssym "a") :: Sym Integer) @=? 2
            symSize (ites (ssym "a" :: Sym Bool) (ssym "b") (ssym "c") :: Sym Integer) @=? 4,
          testCase "symsSize" $ do
            symsSize [ssym "a" :: Sym Integer, ssym "a" + ssym "a"] @=? 2
        ],
      let asymbol :: TypedSymbol Integer = "a"
          bsymbol :: TypedSymbol Bool = "b"
          csymbol :: TypedSymbol Integer = "c"
          dsymbol :: TypedSymbol Bool = "d"
          esymbol :: TypedSymbol (WordN 4) = "e"
          fsymbol :: TypedSymbol (IntN 4) = "f"
          gsymbol :: TypedSymbol (WordN 16) = "g"
          hsymbol :: TypedSymbol (IntN 16) = "h"
          a :: Sym Integer = ssym "a"
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
