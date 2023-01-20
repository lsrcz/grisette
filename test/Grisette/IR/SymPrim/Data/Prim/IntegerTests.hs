{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.IR.SymPrim.Data.Prim.IntegerTests where

import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Integer
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

integerTests :: TestTree
integerTests =
  testGroup
    "IntegerTests"
    [ testGroup
        "DivI"
        [ testProperty "On concrete" $
            ioProperty . \(i :: Integer, j :: Integer) -> do
              if j /= 0
                then pevalDivIntegerTerm (conTerm i) (conTerm j) @=? conTerm (i `div` j)
                else
                  pevalDivIntegerTerm (conTerm i) (conTerm j)
                    @=? divIntegerTerm (conTerm i) (conTerm j),
          testCase "divide by 1" $ do
            pevalDivIntegerTerm (ssymTerm "a" :: Term Integer) (conTerm 1) @=? ssymTerm "a",
          testCase "On symbolic" $ do
            pevalDivIntegerTerm (ssymTerm "a" :: Term Integer) (ssymTerm "b")
              @=? divIntegerTerm (ssymTerm "a" :: Term Integer) (ssymTerm "b" :: Term Integer)
        ],
      testGroup
        "ModI"
        [ testProperty "On concrete" $
            ioProperty . \(i :: Integer, j :: Integer) -> do
              if j /= 0
                then pevalModIntegerTerm (conTerm i) (conTerm j) @=? conTerm (i `mod` j)
                else
                  pevalModIntegerTerm (conTerm i) (conTerm j)
                    @=? modIntegerTerm (conTerm i) (conTerm j),
          testCase "mod by 1" $ do
            pevalModIntegerTerm (ssymTerm "a" :: Term Integer) (conTerm 1) @=? conTerm 0,
          testCase "mod by -1" $ do
            pevalModIntegerTerm (ssymTerm "a" :: Term Integer) (conTerm $ -1) @=? conTerm 0,
          testCase "On symbolic" $ do
            pevalModIntegerTerm (ssymTerm "a" :: Term Integer) (ssymTerm "b")
              @=? modIntegerTerm (ssymTerm "a" :: Term Integer) (ssymTerm "b" :: Term Integer)
        ]
    ]
