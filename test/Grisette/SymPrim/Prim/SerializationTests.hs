{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.SymPrim.Prim.SerializationTests (serializationTests) where

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
  ( FPTrait (FPIsNaN),
    Term,
    absNumTerm,
    addNumTerm,
    andTerm,
    conTerm,
    eqTerm,
    existsTerm,
    forallTerm,
    iteTerm,
    negNumTerm,
    notTerm,
    orTerm,
    pevalFPTraitTerm,
    signumNumTerm,
    ssymTerm,
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
          signumNumTerm (ssymTerm "a" :: Term Integer)
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