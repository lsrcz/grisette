{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.IR.SymPrim.Data.Prim.NumTests where

import Grisette.IR.SymPrim.Data.BV
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bool
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Num
import Test.Tasty
import Test.Tasty.HUnit

numTests :: TestTree
numTests =
  testGroup
    "NumTests"
    [ testGroup
        "Add"
        [ testCase "On concrete" $ do
            pevalAddNumTerm (concTerm 1 :: Term Integer) (concTerm 2) @=? concTerm 3
            pevalAddNumTerm (concTerm 1 :: Term (WordN 3)) (concTerm 2) @=? concTerm 3
            pevalAddNumTerm (concTerm 1 :: Term (IntN 3)) (concTerm 2) @=? concTerm 3,
          testCase "On left 0" $ do
            pevalAddNumTerm (concTerm 0 :: Term Integer) (ssymbTerm "a") @=? ssymbTerm "a",
          testCase "On right 0" $ do
            pevalAddNumTerm (ssymbTerm "a") (concTerm 0 :: Term Integer) @=? ssymbTerm "a",
          testCase "On left concrete" $ do
            pevalAddNumTerm (concTerm 1 :: Term Integer) (ssymbTerm "a")
              @=? addNumTerm (concTerm 1 :: Term Integer) (ssymbTerm "a" :: Term Integer),
          testCase "On right concrete" $ do
            pevalAddNumTerm (ssymbTerm "a") (concTerm 1 :: Term Integer)
              @=? addNumTerm (concTerm 1 :: Term Integer) (ssymbTerm "a" :: Term Integer),
          testCase "On no concrete" $ do
            pevalAddNumTerm (ssymbTerm "a") (ssymbTerm "b" :: Term Integer)
              @=? addNumTerm (ssymbTerm "a" :: Term Integer) (ssymbTerm "b" :: Term Integer),
          testCase "On left concrete and right add concrete value" $ do
            pevalAddNumTerm (concTerm 1 :: Term Integer) (pevalAddNumTerm (concTerm 2 :: Term Integer) (ssymbTerm "a"))
              @=? pevalAddNumTerm (concTerm 3 :: Term Integer) (ssymbTerm "a"),
          testCase "On right concrete and left add concrete value" $ do
            pevalAddNumTerm (pevalAddNumTerm (concTerm 2 :: Term Integer) (ssymbTerm "a")) (concTerm 1 :: Term Integer)
              @=? pevalAddNumTerm (concTerm 3 :: Term Integer) (ssymbTerm "a"),
          testCase "On left add concrete" $ do
            pevalAddNumTerm (pevalAddNumTerm (concTerm 2 :: Term Integer) (ssymbTerm "a")) (ssymbTerm "b")
              @=? pevalAddNumTerm (concTerm 2 :: Term Integer) (pevalAddNumTerm (ssymbTerm "a") (ssymbTerm "b")),
          testCase "On right add concrete" $ do
            pevalAddNumTerm (ssymbTerm "b") (pevalAddNumTerm (concTerm 2 :: Term Integer) (ssymbTerm "a"))
              @=? pevalAddNumTerm (concTerm 2 :: Term Integer) (pevalAddNumTerm (ssymbTerm "b") (ssymbTerm "a")),
          testCase "On both uminus" $ do
            pevalAddNumTerm (pevalUMinusNumTerm $ ssymbTerm "a" :: Term Integer) (pevalUMinusNumTerm $ ssymbTerm "b")
              @=? pevalUMinusNumTerm (pevalAddNumTerm (ssymbTerm "a") (ssymbTerm "b")),
          testCase "On both times the same concrete" $ do
            pevalAddNumTerm
              (pevalTimesNumTerm (concTerm 3) (ssymbTerm "a") :: Term Integer)
              (pevalTimesNumTerm (concTerm 3) (ssymbTerm "b"))
              @=? pevalTimesNumTerm (concTerm 3) (pevalAddNumTerm (ssymbTerm "a") (ssymbTerm "b")),
          testCase "On both times the same symbolic" $ do
            pevalAddNumTerm
              (pevalTimesNumTerm (concTerm 3) (ssymbTerm "a") :: Term Integer)
              (pevalTimesNumTerm (concTerm 3) (ssymbTerm "a"))
              @=? pevalTimesNumTerm (concTerm 6) (ssymbTerm "a")
            pevalAddNumTerm
              (pevalTimesNumTerm (concTerm 3) (ssymbTerm "a") :: Term Integer)
              (pevalTimesNumTerm (concTerm 4) (ssymbTerm "a"))
              @=? pevalTimesNumTerm (concTerm 7) (ssymbTerm "a"),
          testCase "Unfold 1" $ do
            pevalAddNumTerm
              (concTerm 3)
              (pevalITETerm (ssymbTerm "a") (concTerm 1 :: Term Integer) (ssymbTerm "a"))
              @=? pevalITETerm (ssymbTerm "a") (concTerm 4) (pevalAddNumTerm (concTerm 3) (ssymbTerm "a"))
            pevalAddNumTerm
              (pevalITETerm (ssymbTerm "a") (concTerm 1 :: Term Integer) (ssymbTerm "a"))
              (concTerm 3)
              @=? pevalITETerm (ssymbTerm "a") (concTerm 4) (pevalAddNumTerm (ssymbTerm "a") (concTerm 3))
        ],
      testGroup
        "minus"
        [ testCase "minus num should be delegated to add and uminus" $ do
            pevalMinusNumTerm (ssymbTerm "a" :: Term Integer) (ssymbTerm "b")
              @=? pevalAddNumTerm (ssymbTerm "a") (pevalUMinusNumTerm $ ssymbTerm "b")
        ],
      testGroup
        "UMinus"
        [ testCase "On concrete" $ do
            pevalUMinusNumTerm (concTerm 1 :: Term Integer) @=? concTerm (-1)
            pevalUMinusNumTerm (concTerm 1 :: Term (WordN 3)) @=? concTerm (-1),
          testCase "On UMinus" $ do
            pevalUMinusNumTerm (pevalUMinusNumTerm (ssymbTerm "a" :: Term Integer)) @=? ssymbTerm "a",
          testCase "On Add concrete" $ do
            pevalUMinusNumTerm (pevalAddNumTerm (concTerm 1) (ssymbTerm "a" :: Term Integer))
              @=? pevalAddNumTerm (concTerm $ -1) (pevalUMinusNumTerm $ ssymbTerm "a"),
          testCase "On Add uminus" $ do
            pevalUMinusNumTerm (pevalAddNumTerm (pevalUMinusNumTerm $ ssymbTerm "a") (ssymbTerm "b" :: Term Integer))
              @=? pevalAddNumTerm (ssymbTerm "a") (pevalUMinusNumTerm $ ssymbTerm "b")
            pevalUMinusNumTerm (pevalAddNumTerm (ssymbTerm "a") (pevalUMinusNumTerm $ ssymbTerm "b" :: Term Integer))
              @=? pevalAddNumTerm (pevalUMinusNumTerm $ ssymbTerm "a") (ssymbTerm "b"),
          testCase "On Times concrete" $ do
            pevalUMinusNumTerm (pevalTimesNumTerm (concTerm 3) (ssymbTerm "a" :: Term Integer))
              @=? pevalTimesNumTerm (concTerm $ -3) (ssymbTerm "a"),
          testCase "On symbolic" $ do
            pevalUMinusNumTerm (ssymbTerm "a" :: Term Integer)
              @=? uminusNumTerm (ssymbTerm "a")
        ],
      testGroup
        "Times"
        [ testCase "On both concrete" $ do
            pevalTimesNumTerm (concTerm 3 :: Term Integer) (concTerm 5)
              @=? concTerm 15,
          testCase "On left 0" $ do
            pevalTimesNumTerm (concTerm 0 :: Term Integer) (ssymbTerm "a")
              @=? concTerm 0,
          testCase "On right 0" $ do
            pevalTimesNumTerm (ssymbTerm "a") (concTerm 0 :: Term Integer)
              @=? concTerm 0,
          testCase "On left 1" $ do
            pevalTimesNumTerm (concTerm 1 :: Term Integer) (ssymbTerm "a")
              @=? ssymbTerm "a",
          testCase "On right 1" $ do
            pevalTimesNumTerm (ssymbTerm "a") (concTerm 1 :: Term Integer)
              @=? ssymbTerm "a",
          testCase "On left -1" $ do
            pevalTimesNumTerm (concTerm $ -1 :: Term Integer) (ssymbTerm "a")
              @=? pevalUMinusNumTerm (ssymbTerm "a"),
          testCase "On right -1" $ do
            pevalTimesNumTerm (ssymbTerm "a") (concTerm $ -1 :: Term Integer)
              @=? pevalUMinusNumTerm (ssymbTerm "a"),
          testCase "On left concrete and right times concrete symbolics" $ do
            pevalTimesNumTerm (concTerm 3) (pevalTimesNumTerm (concTerm 5 :: Term Integer) (ssymbTerm "a"))
              @=? pevalTimesNumTerm (concTerm 15) (ssymbTerm "a"),
          testCase "On right concrete and left times concrete symbolics" $ do
            pevalTimesNumTerm (pevalTimesNumTerm (concTerm 5 :: Term Integer) (ssymbTerm "a")) (concTerm 3)
              @=? pevalTimesNumTerm (concTerm 15) (ssymbTerm "a"),
          testCase "On left concrete and right add concrete symbolics" $ do
            pevalTimesNumTerm (concTerm 3) (pevalAddNumTerm (concTerm 5 :: Term Integer) (ssymbTerm "a"))
              @=? pevalAddNumTerm (concTerm 15) (pevalTimesNumTerm (concTerm 3) (ssymbTerm "a")),
          testCase "On right concrete and left add concrete symbolics" $ do
            pevalTimesNumTerm (pevalAddNumTerm (concTerm 5 :: Term Integer) (ssymbTerm "a")) (concTerm 3)
              @=? pevalAddNumTerm (concTerm 15) (pevalTimesNumTerm (concTerm 3) (ssymbTerm "a")),
          testCase "On left concrete and right uminus" $ do
            pevalTimesNumTerm (concTerm 3 :: Term Integer) (pevalUMinusNumTerm (ssymbTerm "a"))
              @=? pevalTimesNumTerm (concTerm $ -3) (ssymbTerm "a"),
          testCase "On left times concrete symbolics" $ do
            pevalTimesNumTerm (pevalTimesNumTerm (concTerm 3 :: Term Integer) (ssymbTerm "a")) (ssymbTerm "b")
              @=? pevalTimesNumTerm (concTerm 3) (pevalTimesNumTerm (ssymbTerm "a") (ssymbTerm "b")),
          testCase "On right times concrete symbolics" $ do
            pevalTimesNumTerm (ssymbTerm "b") (pevalTimesNumTerm (concTerm 3 :: Term Integer) (ssymbTerm "a"))
              @=? pevalTimesNumTerm (concTerm 3) (pevalTimesNumTerm (ssymbTerm "b") (ssymbTerm "a")),
          testCase "On left uminus" $ do
            pevalTimesNumTerm (pevalUMinusNumTerm $ ssymbTerm "a") (ssymbTerm "b" :: Term Integer)
              @=? pevalUMinusNumTerm (pevalTimesNumTerm (ssymbTerm "a") (ssymbTerm "b")),
          testCase "On right uminus" $ do
            pevalTimesNumTerm (ssymbTerm "a") (pevalUMinusNumTerm $ ssymbTerm "b" :: Term Integer)
              @=? pevalUMinusNumTerm (pevalTimesNumTerm (ssymbTerm "a") (ssymbTerm "b")),
          testCase "On right concrete and left uminus" $ do
            pevalTimesNumTerm (pevalUMinusNumTerm (ssymbTerm "a")) (concTerm 3 :: Term Integer)
              @=? pevalTimesNumTerm (concTerm $ -3) (ssymbTerm "a"),
          testCase "On left concrete" $ do
            pevalTimesNumTerm (concTerm 3 :: Term Integer) (ssymbTerm "a")
              @=? timesNumTerm
                (concTerm 3 :: Term Integer)
                (ssymbTerm "a" :: Term Integer),
          testCase "On right concrete" $ do
            pevalTimesNumTerm (ssymbTerm "a") (concTerm 3 :: Term Integer)
              @=? timesNumTerm
                (concTerm 3 :: Term Integer)
                (ssymbTerm "a" :: Term Integer),
          testCase "On no concrete" $ do
            pevalTimesNumTerm (ssymbTerm "a") (ssymbTerm "b" :: Term Integer)
              @=? timesNumTerm (ssymbTerm "a" :: Term Integer) (ssymbTerm "b" :: Term Integer),
          testCase "Unfold 1" $ do
            pevalTimesNumTerm
              (concTerm 3)
              (pevalITETerm (ssymbTerm "a") (concTerm 5 :: Term Integer) (ssymbTerm "a"))
              @=? pevalITETerm (ssymbTerm "a") (concTerm 15) (pevalTimesNumTerm (concTerm 3) (ssymbTerm "a"))
            pevalTimesNumTerm
              (pevalITETerm (ssymbTerm "a") (concTerm 5 :: Term Integer) (ssymbTerm "a"))
              (concTerm 3)
              @=? pevalITETerm (ssymbTerm "a") (concTerm 15) (pevalTimesNumTerm (ssymbTerm "a") (concTerm 3))
        ],
      testGroup
        "Abs"
        [ testCase "On concrete" $ do
            pevalAbsNumTerm (concTerm 10 :: Term Integer) @=? concTerm 10
            pevalAbsNumTerm (concTerm $ -10 :: Term Integer) @=? concTerm 10,
          testCase "On UMinus" $ do
            pevalAbsNumTerm (pevalUMinusNumTerm $ ssymbTerm "a" :: Term Integer) @=? pevalAbsNumTerm (ssymbTerm "a"),
          testCase "On Abs" $ do
            pevalAbsNumTerm (pevalAbsNumTerm $ ssymbTerm "a" :: Term Integer) @=? pevalAbsNumTerm (ssymbTerm "a"),
          testCase "On Times Integer" $ do
            pevalAbsNumTerm (pevalTimesNumTerm (ssymbTerm "a") (ssymbTerm "b") :: Term Integer)
              @=? pevalTimesNumTerm (pevalAbsNumTerm (ssymbTerm "a")) (pevalAbsNumTerm (ssymbTerm "b")),
          testCase "On Times BV" $ do
            pevalAbsNumTerm (pevalTimesNumTerm (ssymbTerm "a") (ssymbTerm "b") :: Term (IntN 5))
              @=? absNumTerm (pevalTimesNumTerm (ssymbTerm "a") (ssymbTerm "b") :: Term (IntN 5))
            pevalAbsNumTerm (pevalTimesNumTerm (ssymbTerm "a") (ssymbTerm "b") :: Term (WordN 5))
              @=? absNumTerm (pevalTimesNumTerm (ssymbTerm "a") (ssymbTerm "b") :: Term (WordN 5)),
          testCase "On symbolic" $ do
            pevalAbsNumTerm (ssymbTerm "a" :: Term Integer)
              @=? absNumTerm (ssymbTerm "a")
        ],
      testGroup
        "Signum"
        [ testCase "On concrete" $ do
            pevalSignumNumTerm (concTerm 10 :: Term Integer) @=? concTerm 1
            pevalSignumNumTerm (concTerm 0 :: Term Integer) @=? concTerm 0
            pevalSignumNumTerm (concTerm $ -10 :: Term Integer) @=? concTerm (-1),
          testCase "On UMinus Integer" $ do
            pevalSignumNumTerm (pevalUMinusNumTerm $ ssymbTerm "a" :: Term Integer)
              @=? pevalUMinusNumTerm (pevalSignumNumTerm $ ssymbTerm "a"),
          testCase "On UMinus BV" $ do
            pevalSignumNumTerm (pevalUMinusNumTerm $ ssymbTerm "a" :: Term (IntN 5))
              @=? signumNumTerm (pevalUMinusNumTerm $ ssymbTerm "a" :: Term (IntN 5))
            pevalSignumNumTerm (pevalUMinusNumTerm $ ssymbTerm "a" :: Term (WordN 5))
              @=? signumNumTerm (pevalUMinusNumTerm $ ssymbTerm "a" :: Term (WordN 5)),
          testCase "On Times Integer" $ do
            pevalSignumNumTerm (pevalTimesNumTerm (ssymbTerm "a") (ssymbTerm "b") :: Term Integer)
              @=? pevalTimesNumTerm (pevalSignumNumTerm $ ssymbTerm "a") (pevalSignumNumTerm $ ssymbTerm "b"),
          testCase "On Times BV" $ do
            pevalSignumNumTerm (pevalTimesNumTerm (ssymbTerm "a") (ssymbTerm "b") :: Term (IntN 5))
              @=? signumNumTerm (pevalTimesNumTerm (ssymbTerm "a") (ssymbTerm "b") :: Term (IntN 5))
            pevalSignumNumTerm (pevalTimesNumTerm (ssymbTerm "a") (ssymbTerm "b") :: Term (WordN 5))
              @=? signumNumTerm (pevalTimesNumTerm (ssymbTerm "a") (ssymbTerm "b") :: Term (WordN 5)),
          testCase "On symbolics" $ do
            pevalSignumNumTerm (ssymbTerm "a" :: Term Integer)
              @=? signumNumTerm (ssymbTerm "a")
        ],
      let concSignedBV :: Integer -> Term (IntN 5) = concTerm . fromInteger
          concUnsignedBV :: Integer -> Term (WordN 5) = concTerm . fromInteger
       in testGroup
            "Lt"
            [ testCase "On both concrete" $ do
                pevalLtNumTerm (concTerm 1 :: Term Integer) (concTerm 2) @=? concTerm True
                pevalLtNumTerm (concTerm 2 :: Term Integer) (concTerm 2) @=? concTerm False
                pevalLtNumTerm (concTerm 3 :: Term Integer) (concTerm 2) @=? concTerm False
                pevalLtNumTerm (concTerm 1 :: Term (IntN 2)) (concTerm 0) @=? concTerm False
                pevalLtNumTerm (concTerm 2 :: Term (IntN 2)) (concTerm 0) @=? concTerm True
                pevalLtNumTerm (concTerm 3 :: Term (IntN 2)) (concTerm 0) @=? concTerm True
                pevalLtNumTerm (concTerm 1 :: Term (WordN 2)) (concTerm 2) @=? concTerm True
                pevalLtNumTerm (concTerm 2 :: Term (WordN 2)) (concTerm 2) @=? concTerm False
                pevalLtNumTerm (concTerm 3 :: Term (WordN 2)) (concTerm 2) @=? concTerm False,
              testCase "On left constant and right add concrete Integers" $ do
                pevalLtNumTerm (concTerm 1 :: Term Integer) (pevalAddNumTerm (concTerm 2) (ssymbTerm "a"))
                  @=? pevalLtNumTerm (concTerm $ -1 :: Term Integer) (ssymbTerm "a"),
              testCase "On right constant left add concrete Integers" $ do
                pevalLtNumTerm (pevalAddNumTerm (concTerm 2) (ssymbTerm "a")) (concTerm 1 :: Term Integer)
                  @=? pevalLtNumTerm (concTerm 1 :: Term Integer) (pevalUMinusNumTerm $ ssymbTerm "a"),
              testCase "On right constant Integers" $ do
                pevalLtNumTerm (ssymbTerm "a") (concTerm 1 :: Term Integer)
                  @=? pevalLtNumTerm (concTerm $ -1 :: Term Integer) (pevalUMinusNumTerm $ ssymbTerm "a"),
              testCase "On right constant and left uminus Integers" $ do
                pevalLtNumTerm (pevalUMinusNumTerm $ ssymbTerm "a") (concTerm 1 :: Term Integer)
                  @=? pevalLtNumTerm (concTerm $ -1 :: Term Integer) (ssymbTerm "a"),
              testCase "On left add concrete Integers" $ do
                pevalLtNumTerm (pevalAddNumTerm (concTerm 2) (ssymbTerm "a")) (ssymbTerm "b" :: Term Integer)
                  @=? pevalLtNumTerm (concTerm 2 :: Term Integer) (pevalAddNumTerm (ssymbTerm "b") (pevalUMinusNumTerm $ ssymbTerm "a")),
              testCase "On right add concrete Integers" $ do
                pevalLtNumTerm (ssymbTerm "b" :: Term Integer) (pevalAddNumTerm (concTerm 2) (ssymbTerm "a"))
                  @=? pevalLtNumTerm (concTerm $ -2 :: Term Integer) (pevalAddNumTerm (ssymbTerm "a") (pevalUMinusNumTerm $ ssymbTerm "b")),
              testCase "On left constant and right add concrete BVs should not be simplified" $ do
                pevalLtNumTerm (concSignedBV 1) (pevalAddNumTerm (concTerm 2) (ssymbTerm "a"))
                  @=? ltNumTerm (concSignedBV 1) (pevalAddNumTerm (concSignedBV 2) (ssymbTerm "a"))
                pevalLtNumTerm (concUnsignedBV 1) (pevalAddNumTerm (concTerm 2) (ssymbTerm "a"))
                  @=? ltNumTerm (concUnsignedBV 1) (pevalAddNumTerm (concUnsignedBV 2) (ssymbTerm "a")),
              testCase "On right constant and left add concrete BVs should not be simplified" $ do
                pevalLtNumTerm (pevalAddNumTerm (concSignedBV 2) (ssymbTerm "a")) (concTerm 1)
                  @=? ltNumTerm (pevalAddNumTerm (concSignedBV 2) (ssymbTerm "a")) (concSignedBV 1)
                pevalLtNumTerm (pevalAddNumTerm (concUnsignedBV 2) (ssymbTerm "a")) (concTerm 1)
                  @=? ltNumTerm (pevalAddNumTerm (concUnsignedBV 2) (ssymbTerm "a")) (concUnsignedBV 1),
              testCase "On right constant BVs should not be simplified" $ do
                pevalLtNumTerm (ssymbTerm "a") (concSignedBV 1)
                  @=? ltNumTerm (ssymbTerm "a" :: Term (IntN 5)) (concSignedBV 1)
                pevalLtNumTerm (ssymbTerm "a") (concUnsignedBV 1)
                  @=? ltNumTerm (ssymbTerm "a" :: Term (WordN 5)) (concUnsignedBV 1),
              testCase "On right constant and left uminus BVs should not be simplified" $ do
                pevalLtNumTerm (pevalUMinusNumTerm $ ssymbTerm "a") (concSignedBV 1)
                  @=? ltNumTerm (pevalUMinusNumTerm $ ssymbTerm "a" :: Term (IntN 5)) (concSignedBV 1)
                pevalLtNumTerm (pevalUMinusNumTerm $ ssymbTerm "a") (concUnsignedBV 1)
                  @=? ltNumTerm (pevalUMinusNumTerm $ ssymbTerm "a" :: Term (WordN 5)) (concUnsignedBV 1),
              testCase "On left add concrete BVs should not be simplified" $ do
                pevalLtNumTerm (pevalAddNumTerm (concSignedBV 2) (ssymbTerm "a")) (ssymbTerm "b")
                  @=? ltNumTerm (pevalAddNumTerm (concSignedBV 2) (ssymbTerm "a")) (ssymbTerm "b" :: Term (IntN 5))
                pevalLtNumTerm (pevalAddNumTerm (concUnsignedBV 2) (ssymbTerm "a")) (ssymbTerm "b")
                  @=? ltNumTerm (pevalAddNumTerm (concUnsignedBV 2) (ssymbTerm "a")) (ssymbTerm "b" :: Term (WordN 5)),
              testCase "On right add concrete BVs should not be simplified" $ do
                pevalLtNumTerm (ssymbTerm "b") (pevalAddNumTerm (concSignedBV 2) (ssymbTerm "a"))
                  @=? ltNumTerm
                    (ssymbTerm "b" :: Term (IntN 5))
                    (pevalAddNumTerm (concSignedBV 2) (ssymbTerm "a"))
                pevalLtNumTerm (ssymbTerm "b") (pevalAddNumTerm (concUnsignedBV 2) (ssymbTerm "a"))
                  @=? ltNumTerm
                    (ssymbTerm "b" :: Term (WordN 5))
                    (pevalAddNumTerm (concUnsignedBV 2) (ssymbTerm "a")),
              testCase "On symbolic" $ do
                pevalLtNumTerm (ssymbTerm "a" :: Term Integer) (ssymbTerm "b")
                  @=? ltNumTerm (ssymbTerm "a" :: Term Integer) (ssymbTerm "b" :: Term Integer)
            ],
      let concSignedBV :: Integer -> Term (IntN 5) = concTerm . fromInteger
          concUnsignedBV :: Integer -> Term (WordN 5) = concTerm . fromInteger
       in testGroup
            "Le"
            [ testCase "On both concrete" $ do
                pevalLeNumTerm (concTerm 1 :: Term Integer) (concTerm 2) @=? concTerm True
                pevalLeNumTerm (concTerm 2 :: Term Integer) (concTerm 2) @=? concTerm True
                pevalLeNumTerm (concTerm 3 :: Term Integer) (concTerm 2) @=? concTerm False
                pevalLeNumTerm (concTerm 0 :: Term (IntN 2)) (concTerm 0) @=? concTerm True
                pevalLeNumTerm (concTerm 1 :: Term (IntN 2)) (concTerm 0) @=? concTerm False
                pevalLeNumTerm (concTerm 2 :: Term (IntN 2)) (concTerm 0) @=? concTerm True
                pevalLeNumTerm (concTerm 3 :: Term (IntN 2)) (concTerm 0) @=? concTerm True
                pevalLeNumTerm (concTerm 1 :: Term (WordN 2)) (concTerm 2) @=? concTerm True
                pevalLeNumTerm (concTerm 2 :: Term (WordN 2)) (concTerm 2) @=? concTerm True
                pevalLeNumTerm (concTerm 3 :: Term (WordN 2)) (concTerm 2) @=? concTerm False,
              testCase "On left constant and right add concrete Integers" $ do
                pevalLeNumTerm (concTerm 1 :: Term Integer) (pevalAddNumTerm (concTerm 2) (ssymbTerm "a"))
                  @=? pevalLeNumTerm (concTerm $ -1 :: Term Integer) (ssymbTerm "a"),
              testCase "On right constant and left add concrete Integers" $ do
                pevalLeNumTerm (pevalAddNumTerm (concTerm 2) (ssymbTerm "a")) (concTerm 1 :: Term Integer)
                  @=? pevalLeNumTerm (concTerm 1 :: Term Integer) (pevalUMinusNumTerm $ ssymbTerm "a"),
              testCase "On right constant Integers" $ do
                pevalLeNumTerm (ssymbTerm "a") (concTerm 1 :: Term Integer)
                  @=? pevalLeNumTerm (concTerm $ -1 :: Term Integer) (pevalUMinusNumTerm $ ssymbTerm "a"),
              testCase "On right constant left uminus Integers" $ do
                pevalLeNumTerm (pevalUMinusNumTerm $ ssymbTerm "a") (concTerm 1 :: Term Integer)
                  @=? pevalLeNumTerm (concTerm $ -1 :: Term Integer) (ssymbTerm "a"),
              testCase "On left add concrete Integers" $ do
                pevalLeNumTerm (pevalAddNumTerm (concTerm 2) (ssymbTerm "a")) (ssymbTerm "b" :: Term Integer)
                  @=? pevalLeNumTerm (concTerm 2 :: Term Integer) (pevalAddNumTerm (ssymbTerm "b") (pevalUMinusNumTerm $ ssymbTerm "a")),
              testCase "On right add concrete Integers" $ do
                pevalLeNumTerm (ssymbTerm "b" :: Term Integer) (pevalAddNumTerm (concTerm 2) (ssymbTerm "a"))
                  @=? pevalLeNumTerm (concTerm $ -2 :: Term Integer) (pevalAddNumTerm (ssymbTerm "a") (pevalUMinusNumTerm $ ssymbTerm "b")),
              testCase "On left constant and right add concrete BVs should not be simplified" $ do
                pevalLeNumTerm (concSignedBV 1) (pevalAddNumTerm (concTerm 2) (ssymbTerm "a"))
                  @=? leNumTerm (concSignedBV 1) (pevalAddNumTerm (concSignedBV 2) (ssymbTerm "a"))
                pevalLeNumTerm (concUnsignedBV 1) (pevalAddNumTerm (concTerm 2) (ssymbTerm "a"))
                  @=? leNumTerm (concUnsignedBV 1) (pevalAddNumTerm (concUnsignedBV 2) (ssymbTerm "a")),
              testCase "On right constant and left add concrete BVs should not be simplified" $ do
                pevalLeNumTerm (pevalAddNumTerm (concSignedBV 2) (ssymbTerm "a")) (concTerm 1)
                  @=? leNumTerm (pevalAddNumTerm (concSignedBV 2) (ssymbTerm "a")) (concSignedBV 1)
                pevalLeNumTerm (pevalAddNumTerm (concUnsignedBV 2) (ssymbTerm "a")) (concTerm 1)
                  @=? leNumTerm (pevalAddNumTerm (concUnsignedBV 2) (ssymbTerm "a")) (concUnsignedBV 1),
              testCase "On right constant BVs should not be simplified" $ do
                pevalLeNumTerm (ssymbTerm "a") (concSignedBV 1)
                  @=? leNumTerm (ssymbTerm "a" :: Term (IntN 5)) (concSignedBV 1)
                pevalLeNumTerm (ssymbTerm "a") (concUnsignedBV 1)
                  @=? leNumTerm (ssymbTerm "a" :: Term (WordN 5)) (concUnsignedBV 1),
              testCase "On right constant and left uminus BVs should not be simplified" $ do
                pevalLeNumTerm (pevalUMinusNumTerm $ ssymbTerm "a") (concSignedBV 1)
                  @=? leNumTerm (pevalUMinusNumTerm $ ssymbTerm "a" :: Term (IntN 5)) (concSignedBV 1)
                pevalLeNumTerm (pevalUMinusNumTerm $ ssymbTerm "a") (concUnsignedBV 1)
                  @=? leNumTerm (pevalUMinusNumTerm $ ssymbTerm "a" :: Term (WordN 5)) (concUnsignedBV 1),
              testCase "On left add concrete BVs should not be simplified" $ do
                pevalLeNumTerm (pevalAddNumTerm (concSignedBV 2) (ssymbTerm "a")) (ssymbTerm "b")
                  @=? leNumTerm (pevalAddNumTerm (concSignedBV 2) (ssymbTerm "a")) (ssymbTerm "b" :: Term (IntN 5))
                pevalLeNumTerm (pevalAddNumTerm (concUnsignedBV 2) (ssymbTerm "a")) (ssymbTerm "b")
                  @=? leNumTerm (pevalAddNumTerm (concUnsignedBV 2) (ssymbTerm "a")) (ssymbTerm "b" :: Term (WordN 5)),
              testCase "Lt on right add concrete BVs should not be simplified" $ do
                pevalLeNumTerm (ssymbTerm "b") (pevalAddNumTerm (concSignedBV 2) (ssymbTerm "a"))
                  @=? leNumTerm
                    (ssymbTerm "b" :: Term (IntN 5))
                    (pevalAddNumTerm (concSignedBV 2) (ssymbTerm "a"))
                pevalLeNumTerm (ssymbTerm "b") (pevalAddNumTerm (concUnsignedBV 2) (ssymbTerm "a"))
                  @=? leNumTerm
                    (ssymbTerm "b" :: Term (WordN 5))
                    (pevalAddNumTerm (concUnsignedBV 2) (ssymbTerm "a")),
              testCase "On symbolic" $ do
                pevalLeNumTerm (ssymbTerm "a" :: Term Integer) (ssymbTerm "b")
                  @=? leNumTerm (ssymbTerm "a" :: Term Integer) (ssymbTerm "b" :: Term Integer)
            ],
      testCase "Gt should be delegated to Lt" $
        pevalGtNumTerm (ssymbTerm "a" :: Term Integer) (ssymbTerm "b")
          @=? pevalLtNumTerm (ssymbTerm "b" :: Term Integer) (ssymbTerm "a"),
      testCase "Ge should be delegated to Le" $ do
        pevalGeNumTerm (ssymbTerm "a" :: Term Integer) (ssymbTerm "b")
          @=? pevalLeNumTerm (ssymbTerm "b" :: Term Integer) (ssymbTerm "a")
    ]
