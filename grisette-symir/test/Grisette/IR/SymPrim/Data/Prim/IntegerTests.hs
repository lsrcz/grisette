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
                then pevalDivIntegerTerm (concTerm i) (concTerm j) @=? concTerm (i `div` j)
                else
                  pevalDivIntegerTerm (concTerm i) (concTerm j)
                    @=? divIntegerTerm (concTerm i) (concTerm j),
          testCase "divide by 1" $ do
            pevalDivIntegerTerm (ssymbTerm "a" :: Term Integer) (concTerm 1) @=? ssymbTerm "a",
          testCase "On symbolic" $ do
            pevalDivIntegerTerm (ssymbTerm "a" :: Term Integer) (ssymbTerm "b")
              @=? divIntegerTerm (ssymbTerm "a" :: Term Integer) (ssymbTerm "b" :: Term Integer)
        ],
      testGroup
        "ModI"
        [ testProperty "On concrete" $
            ioProperty . \(i :: Integer, j :: Integer) -> do
              if j /= 0
                then pevalModIntegerTerm (concTerm i) (concTerm j) @=? concTerm (i `mod` j)
                else
                  pevalModIntegerTerm (concTerm i) (concTerm j)
                    @=? modIntegerTerm (concTerm i) (concTerm j),
          testCase "mod by 1" $ do
            pevalModIntegerTerm (ssymbTerm "a" :: Term Integer) (concTerm 1) @=? concTerm 0,
          testCase "mod by -1" $ do
            pevalModIntegerTerm (ssymbTerm "a" :: Term Integer) (concTerm $ -1) @=? concTerm 0,
          testCase "On symbolic" $ do
            pevalModIntegerTerm (ssymbTerm "a" :: Term Integer) (ssymbTerm "b")
              @=? modIntegerTerm (ssymbTerm "a" :: Term Integer) (ssymbTerm "b" :: Term Integer)
        ]
    ]
