{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.SymPrim.Prim.SerializationTests (serializationTests) where

import Data.Data (Proxy (Proxy))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Serialize (Serialize, decode, encode)
import Grisette
  ( AlgReal,
    FPRoundingMode (RNE),
    IEEEFPConstants (fpNaN),
    Solvable (con),
    SymbolKind (ConstantKind),
    TypedSymbol,
    (-->),
    type (-->),
    type (=->) (TabularFun),
  )
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP (FP)
import Grisette.Internal.SymPrim.Prim.Term
  ( FPBinaryOp (FPMinimum),
    FPRoundingBinaryOp (FPAdd),
    FPRoundingUnaryOp (FPSqrt),
    FPTrait (FPIsNaN, FPIsNegativeInfinite),
    FPUnaryOp (FPAbs),
    FloatingUnaryOp (FloatingSin),
    Term,
    absNumTerm,
    addNumTerm,
    andBitsTerm,
    andTerm,
    applyTerm,
    bitCastOrTerm,
    bitCastTerm,
    bvConcatTerm,
    bvSelectTerm,
    bvsignExtendTerm,
    bvzeroExtendTerm,
    complementBitsTerm,
    conTerm,
    distinctTerm,
    divIntegralTerm,
    eqTerm,
    existsTerm,
    fdivTerm,
    floatingUnaryTerm,
    forallTerm,
    fpBinaryTerm,
    fpFMATerm,
    fpRoundingBinaryTerm,
    fpRoundingUnaryTerm,
    fpTraitTerm,
    fpUnaryTerm,
    fromFPOrTerm,
    fromIntegralTerm,
    iteTerm,
    leOrdTerm,
    ltOrdTerm,
    modIntegralTerm,
    negNumTerm,
    notTerm,
    orBitsTerm,
    orTerm,
    pevalFPTraitTerm,
    powerTerm,
    quotIntegralTerm,
    recipTerm,
    remIntegralTerm,
    rotateLeftTerm,
    rotateRightTerm,
    shiftLeftTerm,
    shiftRightTerm,
    signumNumTerm,
    ssymTerm,
    toFPTerm,
    xorBitsTerm,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

assertSerialization :: (Eq a, Show a, Serialize a) => a -> IO ()
assertSerialization x = decode (encode x) @?= Right x

serializationTests :: Test
serializationTests =
  testGroup
    "Serialization"
    [ testGroup
        "ConTerm"
        [ testCase "Bool" $ do
            assertSerialization $ conTerm True,
          testCase "Integer" $ do
            assertSerialization $ conTerm (1 :: Integer),
          testCase "IntN" $ do
            assertSerialization $ conTerm (42 :: IntN 8),
          testCase "WordN" $ do
            assertSerialization $ conTerm (42 :: WordN 8),
          testCase "FP" $ do
            assertSerialization $ conTerm (42 :: FP 8 24)
            let termNaN = conTerm (fpNaN :: FP 8 24)
            let decodedTerm =
                  decode (encode termNaN) ::
                    Either String (Term (FP 8 24))
            pevalFPTraitTerm FPIsNaN <$> decodedTerm @?= Right (conTerm True),
          testCase "FPRoundingMode" $ do
            assertSerialization $ conTerm RNE,
          testCase "AlgReal" $ do
            assertSerialization $ conTerm (1 / 8 :: AlgReal),
          testCase "TabularFun" $ do
            assertSerialization $ conTerm t1
            assertSerialization $ conTerm t2
            assertSerialization $ conTerm t3
            assertSerialization $ conTerm t4
            assertSerialization $ conTerm t5
            assertSerialization $ conTerm t6
            assertSerialization $ conTerm t7,
          testCase "GeneralFun" $ do
            assertSerialization $ conTerm g1
            assertSerialization $ conTerm g2
            assertSerialization $ conTerm g3
            assertSerialization $ conTerm g4
            assertSerialization $ conTerm g5
            assertSerialization $ conTerm g6
            assertSerialization $ conTerm g7
        ],
      testCase "SymTerm" $
        assertSerialization (ssymTerm "a" :: Term Bool),
      testCase "ForallTerm" $
        assertSerialization $
          forallTerm
            ("a" :: TypedSymbol 'ConstantKind Integer)
            (ssymTerm "b"),
      testCase "ExistsTerm" $
        assertSerialization $
          existsTerm
            ("a" :: TypedSymbol 'ConstantKind Integer)
            (ssymTerm "b"),
      testCase "NotTerm" $ assertSerialization $ notTerm (ssymTerm "a"),
      testCase "AndTerm" $
        assertSerialization $
          andTerm (ssymTerm "a") (ssymTerm "b"),
      testCase "OrTerm" $
        assertSerialization $
          orTerm (ssymTerm "a") (ssymTerm "b"),
      testCase "EqTerm" $
        assertSerialization $
          eqTerm (ssymTerm "a" :: Term Integer) (ssymTerm "b"),
      testCase "DistinctTerm" $
        assertSerialization $
          distinctTerm $
            (ssymTerm "a" :: Term Integer) :| [ssymTerm "b", ssymTerm "c"],
      testCase "ITETerm" $
        assertSerialization $
          iteTerm (ssymTerm "a") (ssymTerm "b" :: Term Integer) (ssymTerm "c"),
      testCase "AddNumTerm" $
        assertSerialization $
          addNumTerm (ssymTerm "a" :: Term Integer) (ssymTerm "b"),
      testCase "NegNumTerm" $
        assertSerialization $
          negNumTerm (ssymTerm "a" :: Term Integer),
      testCase "MulNumTerm" $
        assertSerialization $
          addNumTerm (ssymTerm "a" :: Term Integer) (ssymTerm "b"),
      testCase "AbsNumTerm" $
        assertSerialization $
          absNumTerm (ssymTerm "a" :: Term Integer),
      testCase "SignumNumTerm" $
        assertSerialization $
          signumNumTerm (ssymTerm "a" :: Term Integer),
      testCase "ltOrdTerm" $
        assertSerialization $
          ltOrdTerm (ssymTerm "a" :: Term Integer) (ssymTerm "b"),
      testCase "leOrdTerm" $
        assertSerialization $
          leOrdTerm (ssymTerm "a" :: Term Integer) (ssymTerm "b"),
      testCase "andBitsTerm" $
        assertSerialization $
          andBitsTerm (ssymTerm "a" :: Term (WordN 8)) (ssymTerm "b"),
      testCase "orBitsTerm" $
        assertSerialization $
          orBitsTerm (ssymTerm "a" :: Term (WordN 8)) (ssymTerm "b"),
      testCase "xorBitsTerm" $
        assertSerialization $
          xorBitsTerm (ssymTerm "a" :: Term (WordN 8)) (ssymTerm "b"),
      testCase "complementBitsTerm" $
        assertSerialization $
          complementBitsTerm (ssymTerm "a" :: Term (WordN 8)),
      testCase "shiftLeftTerm" $
        assertSerialization $
          shiftLeftTerm (ssymTerm "a" :: Term (WordN 8)) (ssymTerm "b"),
      testCase "shiftRightTerm" $
        assertSerialization $
          shiftRightTerm (ssymTerm "a" :: Term (WordN 8)) (ssymTerm "b"),
      testCase "rotateLeftTerm" $
        assertSerialization $
          rotateLeftTerm (ssymTerm "a" :: Term (WordN 8)) (ssymTerm "b"),
      testCase "rotateRightTerm" $
        assertSerialization $
          rotateRightTerm (ssymTerm "a" :: Term (WordN 8)) (ssymTerm "b"),
      testCase "bitCastTerm" $ do
        assertSerialization
          (bitCastTerm (ssymTerm "a" :: Term Bool) :: Term (WordN 1))
        assertSerialization
          (bitCastTerm (ssymTerm "a" :: Term Bool) :: Term (IntN 1))
        assertSerialization
          (bitCastTerm (ssymTerm "a" :: Term (WordN 1)) :: Term Bool)
        assertSerialization
          (bitCastTerm (ssymTerm "a" :: Term (IntN 1)) :: Term Bool)
        assertSerialization
          (bitCastTerm (ssymTerm "a" :: Term (WordN 8)) :: Term (FP 2 6))
        assertSerialization
          (bitCastTerm (ssymTerm "a" :: Term (WordN 8)) :: Term (IntN 8))
        assertSerialization
          (bitCastTerm (ssymTerm "a" :: Term (IntN 8)) :: Term (FP 2 6)),
      testCase "bitCastOrTerm" $ do
        assertSerialization
          ( bitCastOrTerm
              (ssymTerm "d" :: Term (WordN 8))
              (ssymTerm "a" :: Term (FP 2 6))
          )
        assertSerialization
          ( bitCastOrTerm
              (ssymTerm "d" :: Term (IntN 8))
              (ssymTerm "a" :: Term (FP 2 6))
          ),
      testCase "bvConcatTerm" $
        assertSerialization $
          bvConcatTerm
            (ssymTerm "d" :: Term (WordN 8))
            (ssymTerm "a" :: Term (WordN 8)),
      testCase "bvSelectTerm" $
        assertSerialization $
          bvSelectTerm
            (Proxy @2)
            (Proxy @3)
            (ssymTerm "b" :: Term (WordN 8)),
      testCase "bvExtendTerm" $ do
        assertSerialization $
          bvsignExtendTerm
            (Proxy @16)
            (ssymTerm "b" :: Term (WordN 8))
        assertSerialization $
          bvzeroExtendTerm
            (Proxy @16)
            (ssymTerm "b" :: Term (WordN 8)),
      testCase "applyTerm" $ do
        assertSerialization $
          applyTerm (ssymTerm "a" :: Term (Integer =-> Integer)) (ssymTerm "b")
        assertSerialization $
          applyTerm (ssymTerm "a" :: Term (Integer --> Integer)) (ssymTerm "b"),
      testCase "divIntegralTerm" $
        assertSerialization $
          divIntegralTerm (ssymTerm "a" :: Term Integer) (ssymTerm "b"),
      testCase "modIntegralTerm" $
        assertSerialization $
          modIntegralTerm (ssymTerm "a" :: Term Integer) (ssymTerm "b"),
      testCase "quotIntegralTerm" $
        assertSerialization $
          quotIntegralTerm (ssymTerm "a" :: Term Integer) (ssymTerm "b"),
      testCase "remIntegralTerm" $
        assertSerialization $
          remIntegralTerm (ssymTerm "a" :: Term Integer) (ssymTerm "b"),
      testCase "fpTraitTerm" $
        assertSerialization $
          fpTraitTerm FPIsNegativeInfinite (ssymTerm "a" :: Term (FP 2 6)),
      testCase "fdivTerm" $
        assertSerialization $
          fdivTerm (ssymTerm "a" :: Term (FP 2 6)) (ssymTerm "b"),
      testCase "recipTerm" $
        assertSerialization $
          recipTerm (ssymTerm "a" :: Term (FP 2 6)),
      testCase "floatingUnaryTerm" $
        assertSerialization $
          floatingUnaryTerm FloatingSin (ssymTerm "a" :: Term (FP 2 6)),
      testCase "powerTerm" $
        assertSerialization $
          powerTerm (ssymTerm "a" :: Term (FP 2 6)) (ssymTerm "b"),
      testCase "fpUnaryTerm" $
        assertSerialization $
          fpUnaryTerm FPAbs (ssymTerm "a" :: Term (FP 2 6)),
      testCase "fpBinaryTerm" $
        assertSerialization $
          fpBinaryTerm FPMinimum (ssymTerm "a" :: Term (FP 2 6)) (ssymTerm "b"),
      testCase "fpRoundingUnaryTerm" $
        assertSerialization $
          fpRoundingUnaryTerm
            FPSqrt
            (ssymTerm "a")
            (ssymTerm "b" :: Term (FP 2 6)),
      testCase "fpRoundingBinaryTerm" $
        assertSerialization $
          fpRoundingBinaryTerm
            FPAdd
            (ssymTerm "a")
            (ssymTerm "b" :: Term (FP 2 6))
            (ssymTerm "c"),
      testCase "fpFMATerm" $
        assertSerialization $
          fpFMATerm
            (ssymTerm "a")
            (ssymTerm "b" :: Term (FP 2 6))
            (ssymTerm "c")
            (ssymTerm "d"),
      testCase "fromIntegralTerm" $ do
        assertSerialization
          (fromIntegralTerm (ssymTerm "a" :: Term Integer) :: Term Integer)
        assertSerialization
          (fromIntegralTerm (ssymTerm "a" :: Term Integer) :: Term AlgReal)
        assertSerialization
          (fromIntegralTerm (ssymTerm "a" :: Term Integer) :: Term (WordN 8))
        assertSerialization
          (fromIntegralTerm (ssymTerm "a" :: Term Integer) :: Term (IntN 8))
        assertSerialization
          (fromIntegralTerm (ssymTerm "a" :: Term Integer) :: Term (FP 8 24))
        assertSerialization
          (fromIntegralTerm (ssymTerm "a" :: Term (WordN 8)) :: Term Integer)
        assertSerialization
          (fromIntegralTerm (ssymTerm "a" :: Term (WordN 8)) :: Term AlgReal)
        assertSerialization
          (fromIntegralTerm (ssymTerm "a" :: Term (WordN 8)) :: Term (WordN 8))
        assertSerialization
          (fromIntegralTerm (ssymTerm "a" :: Term (WordN 8)) :: Term (IntN 8))
        assertSerialization
          (fromIntegralTerm (ssymTerm "a" :: Term (WordN 8)) :: Term (FP 8 24))
        assertSerialization
          (fromIntegralTerm (ssymTerm "a" :: Term (IntN 8)) :: Term Integer)
        assertSerialization
          (fromIntegralTerm (ssymTerm "a" :: Term (IntN 8)) :: Term AlgReal)
        assertSerialization
          (fromIntegralTerm (ssymTerm "a" :: Term (IntN 8)) :: Term (WordN 8))
        assertSerialization
          (fromIntegralTerm (ssymTerm "a" :: Term (IntN 8)) :: Term (IntN 8))
        assertSerialization
          (fromIntegralTerm (ssymTerm "a" :: Term (IntN 8)) :: Term (FP 8 24)),
      testCase "fromFPOrTerm" $ do
        assertSerialization
          ( fromFPOrTerm
              (ssymTerm "a" :: Term Integer)
              (ssymTerm "r")
              (ssymTerm "b" :: Term (FP 8 24))
          )
        assertSerialization
          ( fromFPOrTerm
              (ssymTerm "a" :: Term AlgReal)
              (ssymTerm "r")
              (ssymTerm "b" :: Term (FP 8 24))
          )
        assertSerialization
          ( fromFPOrTerm
              (ssymTerm "a" :: Term (WordN 8))
              (ssymTerm "r")
              (ssymTerm "b" :: Term (FP 8 24))
          )
        assertSerialization
          ( fromFPOrTerm
              (ssymTerm "a" :: Term (IntN 8))
              (ssymTerm "r")
              (ssymTerm "b" :: Term (FP 8 24))
          )
        assertSerialization
          ( fromFPOrTerm
              (ssymTerm "a" :: Term (FP 11 53))
              (ssymTerm "r")
              (ssymTerm "b" :: Term (FP 8 24))
          ),
      testCase "toFPTerm" $ do
        assertSerialization
          ( toFPTerm
              (ssymTerm "r")
              (ssymTerm "a" :: Term Integer) ::
              Term (FP 8 24)
          )
        assertSerialization
          ( toFPTerm
              (ssymTerm "r")
              (ssymTerm "a" :: Term AlgReal) ::
              Term (FP 8 24)
          )
        assertSerialization
          ( toFPTerm
              (ssymTerm "r")
              (ssymTerm "a" :: Term (WordN 8)) ::
              Term (FP 8 24)
          )
        assertSerialization
          ( toFPTerm
              (ssymTerm "r")
              (ssymTerm "a" :: Term (IntN 8)) ::
              Term (FP 8 24)
          )
        assertSerialization
          ( toFPTerm
              (ssymTerm "r")
              (ssymTerm "a" :: Term (FP 11 53)) ::
              Term (FP 8 24)
          )
    ]

t1 :: Integer =-> Integer
t1 = TabularFun [(1, 2), (2, 3)] 4

t1' :: Integer =-> Integer
t1' = TabularFun [(1, 2), (22, 3)] 4

t2 :: Integer =-> Integer =-> Integer
t2 = TabularFun [(1, t1)] t1'

t2' :: Integer =-> Integer =-> Integer
t2' = TabularFun [(2, t1)] t1'

t3 :: Integer =-> Integer =-> Integer =-> Integer
t3 = TabularFun [(2, t2)] t2'

t3' :: Integer =-> Integer =-> Integer =-> Integer
t3' = TabularFun [(3, t2)] t2'

t4 :: Integer =-> Integer =-> Integer =-> Integer =-> Integer
t4 = TabularFun [(3, t3)] t3'

t4' :: Integer =-> Integer =-> Integer =-> Integer =-> Integer
t4' = TabularFun [(4, t3)] t3'

t5 :: Integer =-> Integer =-> Integer =-> Integer =-> Integer =-> Integer
t5 = TabularFun [(4, t4)] t4'

t5' :: Integer =-> Integer =-> Integer =-> Integer =-> Integer =-> Integer
t5' = TabularFun [(5, t4)] t4'

t6 ::
  Integer
    =-> Integer
    =-> Integer
    =-> Integer
    =-> Integer
    =-> Integer
    =-> Integer
t6 = TabularFun [(5, t5)] t5'

t6' ::
  Integer
    =-> Integer
    =-> Integer
    =-> Integer
    =-> Integer
    =-> Integer
    =-> Integer
t6' = TabularFun [(6, t5)] t5'

t7 ::
  Integer
    =-> Integer
    =-> Integer
    =-> Integer
    =-> Integer
    =-> Integer
    =-> Integer
    =-> Integer
t7 = TabularFun [(6, t6)] t6'

g1 :: Integer --> Integer
g1 = "a" --> "a" + "b" + "c" + "d" + "e" + "f" + "g"

g2 :: Integer --> Integer --> Integer
g2 = "b" --> con g1

g3 :: Integer --> Integer --> Integer --> Integer
g3 = "c" --> con g2

g4 :: Integer --> Integer --> Integer --> Integer --> Integer
g4 = "d" --> con g3

g5 :: Integer --> Integer --> Integer --> Integer --> Integer --> Integer
g5 = "e" --> con g4

g6 ::
  Integer
    --> Integer
    --> Integer
    --> Integer
    --> Integer
    --> Integer
    --> Integer
g6 = "f" --> con g5

g7 ::
  Integer
    --> Integer
    --> Integer
    --> Integer
    --> Integer
    --> Integer
    --> Integer
    --> Integer
g7 = "g" --> con g6
