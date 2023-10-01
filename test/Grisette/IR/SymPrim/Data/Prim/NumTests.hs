{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.IR.SymPrim.Data.Prim.NumTests (numTests) where

import Grisette.Core.Data.BV (IntN, WordN)
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
  ( absNumTerm,
    addNumTerm,
    conTerm,
    leNumTerm,
    ltNumTerm,
    signumNumTerm,
    ssymTerm,
    timesNumTerm,
    uminusNumTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term (Term)
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bool
  ( pevalITETerm,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Num
  ( pevalAbsNumTerm,
    pevalAddNumTerm,
    pevalGeNumTerm,
    pevalGtNumTerm,
    pevalLeNumTerm,
    pevalLtNumTerm,
    pevalMinusNumTerm,
    pevalSignumNumTerm,
    pevalTimesNumTerm,
    pevalUMinusNumTerm,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@=?))

numTests :: Test
numTests =
  testGroup
    "Num"
    [ testGroup
        "Add"
        [ testCase "On concrete" $ do
            pevalAddNumTerm (conTerm 1 :: Term Integer) (conTerm 2) @=? conTerm 3
            pevalAddNumTerm (conTerm 1 :: Term (WordN 3)) (conTerm 2) @=? conTerm 3
            pevalAddNumTerm (conTerm 1 :: Term (IntN 3)) (conTerm 2) @=? conTerm 3,
          testCase "On left 0" $ do
            pevalAddNumTerm (conTerm 0 :: Term Integer) (ssymTerm "a") @=? ssymTerm "a",
          testCase "On right 0" $ do
            pevalAddNumTerm (ssymTerm "a") (conTerm 0 :: Term Integer) @=? ssymTerm "a",
          testCase "On left concrete" $ do
            pevalAddNumTerm (conTerm 1 :: Term Integer) (ssymTerm "a")
              @=? addNumTerm (conTerm 1 :: Term Integer) (ssymTerm "a" :: Term Integer),
          testCase "On right concrete" $ do
            pevalAddNumTerm (ssymTerm "a") (conTerm 1 :: Term Integer)
              @=? addNumTerm (conTerm 1 :: Term Integer) (ssymTerm "a" :: Term Integer),
          testCase "On no concrete" $ do
            pevalAddNumTerm (ssymTerm "a") (ssymTerm "b" :: Term Integer)
              @=? addNumTerm (ssymTerm "a" :: Term Integer) (ssymTerm "b" :: Term Integer),
          testCase "On left concrete and right add concrete value" $ do
            pevalAddNumTerm (conTerm 1 :: Term Integer) (pevalAddNumTerm (conTerm 2 :: Term Integer) (ssymTerm "a"))
              @=? pevalAddNumTerm (conTerm 3 :: Term Integer) (ssymTerm "a"),
          testCase "On right concrete and left add concrete value" $ do
            pevalAddNumTerm (pevalAddNumTerm (conTerm 2 :: Term Integer) (ssymTerm "a")) (conTerm 1 :: Term Integer)
              @=? pevalAddNumTerm (conTerm 3 :: Term Integer) (ssymTerm "a"),
          testCase "On left add concrete" $ do
            pevalAddNumTerm (pevalAddNumTerm (conTerm 2 :: Term Integer) (ssymTerm "a")) (ssymTerm "b")
              @=? pevalAddNumTerm (conTerm 2 :: Term Integer) (pevalAddNumTerm (ssymTerm "a") (ssymTerm "b")),
          testCase "On right add concrete" $ do
            pevalAddNumTerm (ssymTerm "b") (pevalAddNumTerm (conTerm 2 :: Term Integer) (ssymTerm "a"))
              @=? pevalAddNumTerm (conTerm 2 :: Term Integer) (pevalAddNumTerm (ssymTerm "b") (ssymTerm "a")),
          testCase "On both uminus" $ do
            pevalAddNumTerm (pevalUMinusNumTerm $ ssymTerm "a" :: Term Integer) (pevalUMinusNumTerm $ ssymTerm "b")
              @=? pevalUMinusNumTerm (pevalAddNumTerm (ssymTerm "a") (ssymTerm "b")),
          testCase "On both times the same concrete" $ do
            pevalAddNumTerm
              (pevalTimesNumTerm (conTerm 3) (ssymTerm "a") :: Term Integer)
              (pevalTimesNumTerm (conTerm 3) (ssymTerm "b"))
              @=? pevalTimesNumTerm (conTerm 3) (pevalAddNumTerm (ssymTerm "a") (ssymTerm "b")),
          testCase "On both times the same symbolic" $ do
            pevalAddNumTerm
              (pevalTimesNumTerm (conTerm 3) (ssymTerm "a") :: Term Integer)
              (pevalTimesNumTerm (conTerm 3) (ssymTerm "a"))
              @=? pevalTimesNumTerm (conTerm 6) (ssymTerm "a")
            pevalAddNumTerm
              (pevalTimesNumTerm (conTerm 3) (ssymTerm "a") :: Term Integer)
              (pevalTimesNumTerm (conTerm 4) (ssymTerm "a"))
              @=? pevalTimesNumTerm (conTerm 7) (ssymTerm "a"),
          testCase "Unfold 1" $ do
            pevalAddNumTerm
              (conTerm 3)
              (pevalITETerm (ssymTerm "a") (conTerm 1 :: Term Integer) (ssymTerm "a"))
              @=? pevalITETerm (ssymTerm "a") (conTerm 4) (pevalAddNumTerm (conTerm 3) (ssymTerm "a"))
            pevalAddNumTerm
              (pevalITETerm (ssymTerm "a") (conTerm 1 :: Term Integer) (ssymTerm "a"))
              (conTerm 3)
              @=? pevalITETerm (ssymTerm "a") (conTerm 4) (pevalAddNumTerm (ssymTerm "a") (conTerm 3))
        ],
      testGroup
        "minus"
        [ testCase "minus num should be delegated to add and uminus" $ do
            pevalMinusNumTerm (ssymTerm "a" :: Term Integer) (ssymTerm "b")
              @=? pevalAddNumTerm (ssymTerm "a") (pevalUMinusNumTerm $ ssymTerm "b")
        ],
      testGroup
        "UMinus"
        [ testCase "On concrete" $ do
            pevalUMinusNumTerm (conTerm 1 :: Term Integer) @=? conTerm (-1)
            pevalUMinusNumTerm (conTerm 1 :: Term (WordN 3)) @=? conTerm (-1),
          testCase "On UMinus" $ do
            pevalUMinusNumTerm (pevalUMinusNumTerm (ssymTerm "a" :: Term Integer)) @=? ssymTerm "a",
          testCase "On Add concrete" $ do
            pevalUMinusNumTerm (pevalAddNumTerm (conTerm 1) (ssymTerm "a" :: Term Integer))
              @=? pevalAddNumTerm (conTerm $ -1) (pevalUMinusNumTerm $ ssymTerm "a"),
          testCase "On Add uminus" $ do
            pevalUMinusNumTerm (pevalAddNumTerm (pevalUMinusNumTerm $ ssymTerm "a") (ssymTerm "b" :: Term Integer))
              @=? pevalAddNumTerm (ssymTerm "a") (pevalUMinusNumTerm $ ssymTerm "b")
            pevalUMinusNumTerm (pevalAddNumTerm (ssymTerm "a") (pevalUMinusNumTerm $ ssymTerm "b" :: Term Integer))
              @=? pevalAddNumTerm (pevalUMinusNumTerm $ ssymTerm "a") (ssymTerm "b"),
          testCase "On Times concrete" $ do
            pevalUMinusNumTerm (pevalTimesNumTerm (conTerm 3) (ssymTerm "a" :: Term Integer))
              @=? pevalTimesNumTerm (conTerm $ -3) (ssymTerm "a"),
          testCase "On symbolic" $ do
            pevalUMinusNumTerm (ssymTerm "a" :: Term Integer)
              @=? uminusNumTerm (ssymTerm "a")
        ],
      testGroup
        "Times"
        [ testCase "On both concrete" $ do
            pevalTimesNumTerm (conTerm 3 :: Term Integer) (conTerm 5)
              @=? conTerm 15,
          testCase "On left 0" $ do
            pevalTimesNumTerm (conTerm 0 :: Term Integer) (ssymTerm "a")
              @=? conTerm 0,
          testCase "On right 0" $ do
            pevalTimesNumTerm (ssymTerm "a") (conTerm 0 :: Term Integer)
              @=? conTerm 0,
          testCase "On left 1" $ do
            pevalTimesNumTerm (conTerm 1 :: Term Integer) (ssymTerm "a")
              @=? ssymTerm "a",
          testCase "On right 1" $ do
            pevalTimesNumTerm (ssymTerm "a") (conTerm 1 :: Term Integer)
              @=? ssymTerm "a",
          testCase "On left -1" $ do
            pevalTimesNumTerm (conTerm $ -1 :: Term Integer) (ssymTerm "a")
              @=? pevalUMinusNumTerm (ssymTerm "a"),
          testCase "On right -1" $ do
            pevalTimesNumTerm (ssymTerm "a") (conTerm $ -1 :: Term Integer)
              @=? pevalUMinusNumTerm (ssymTerm "a"),
          testCase "On left concrete and right times concrete symbolics" $ do
            pevalTimesNumTerm (conTerm 3) (pevalTimesNumTerm (conTerm 5 :: Term Integer) (ssymTerm "a"))
              @=? pevalTimesNumTerm (conTerm 15) (ssymTerm "a"),
          testCase "On right concrete and left times concrete symbolics" $ do
            pevalTimesNumTerm (pevalTimesNumTerm (conTerm 5 :: Term Integer) (ssymTerm "a")) (conTerm 3)
              @=? pevalTimesNumTerm (conTerm 15) (ssymTerm "a"),
          testCase "On left concrete and right add concrete symbolics" $ do
            pevalTimesNumTerm (conTerm 3) (pevalAddNumTerm (conTerm 5 :: Term Integer) (ssymTerm "a"))
              @=? pevalAddNumTerm (conTerm 15) (pevalTimesNumTerm (conTerm 3) (ssymTerm "a")),
          testCase "On right concrete and left add concrete symbolics" $ do
            pevalTimesNumTerm (pevalAddNumTerm (conTerm 5 :: Term Integer) (ssymTerm "a")) (conTerm 3)
              @=? pevalAddNumTerm (conTerm 15) (pevalTimesNumTerm (conTerm 3) (ssymTerm "a")),
          testCase "On left concrete and right uminus" $ do
            pevalTimesNumTerm (conTerm 3 :: Term Integer) (pevalUMinusNumTerm (ssymTerm "a"))
              @=? pevalTimesNumTerm (conTerm $ -3) (ssymTerm "a"),
          testCase "On left times concrete symbolics" $ do
            pevalTimesNumTerm (pevalTimesNumTerm (conTerm 3 :: Term Integer) (ssymTerm "a")) (ssymTerm "b")
              @=? pevalTimesNumTerm (conTerm 3) (pevalTimesNumTerm (ssymTerm "a") (ssymTerm "b")),
          testCase "On right times concrete symbolics" $ do
            pevalTimesNumTerm (ssymTerm "b") (pevalTimesNumTerm (conTerm 3 :: Term Integer) (ssymTerm "a"))
              @=? pevalTimesNumTerm (conTerm 3) (pevalTimesNumTerm (ssymTerm "b") (ssymTerm "a")),
          testCase "On left uminus" $ do
            pevalTimesNumTerm (pevalUMinusNumTerm $ ssymTerm "a") (ssymTerm "b" :: Term Integer)
              @=? pevalUMinusNumTerm (pevalTimesNumTerm (ssymTerm "a") (ssymTerm "b")),
          testCase "On right uminus" $ do
            pevalTimesNumTerm (ssymTerm "a") (pevalUMinusNumTerm $ ssymTerm "b" :: Term Integer)
              @=? pevalUMinusNumTerm (pevalTimesNumTerm (ssymTerm "a") (ssymTerm "b")),
          testCase "On right concrete and left uminus" $ do
            pevalTimesNumTerm (pevalUMinusNumTerm (ssymTerm "a")) (conTerm 3 :: Term Integer)
              @=? pevalTimesNumTerm (conTerm $ -3) (ssymTerm "a"),
          testCase "On left concrete" $ do
            pevalTimesNumTerm (conTerm 3 :: Term Integer) (ssymTerm "a")
              @=? timesNumTerm
                (conTerm 3 :: Term Integer)
                (ssymTerm "a" :: Term Integer),
          testCase "On right concrete" $ do
            pevalTimesNumTerm (ssymTerm "a") (conTerm 3 :: Term Integer)
              @=? timesNumTerm
                (conTerm 3 :: Term Integer)
                (ssymTerm "a" :: Term Integer),
          testCase "On no concrete" $ do
            pevalTimesNumTerm (ssymTerm "a") (ssymTerm "b" :: Term Integer)
              @=? timesNumTerm (ssymTerm "a" :: Term Integer) (ssymTerm "b" :: Term Integer),
          testCase "Unfold 1" $ do
            pevalTimesNumTerm
              (conTerm 3)
              (pevalITETerm (ssymTerm "a") (conTerm 5 :: Term Integer) (ssymTerm "a"))
              @=? pevalITETerm (ssymTerm "a") (conTerm 15) (pevalTimesNumTerm (conTerm 3) (ssymTerm "a"))
            pevalTimesNumTerm
              (pevalITETerm (ssymTerm "a") (conTerm 5 :: Term Integer) (ssymTerm "a"))
              (conTerm 3)
              @=? pevalITETerm (ssymTerm "a") (conTerm 15) (pevalTimesNumTerm (ssymTerm "a") (conTerm 3))
        ],
      testGroup
        "Abs"
        [ testCase "On concrete" $ do
            pevalAbsNumTerm (conTerm 10 :: Term Integer) @=? conTerm 10
            pevalAbsNumTerm (conTerm $ -10 :: Term Integer) @=? conTerm 10,
          testCase "On UMinus" $ do
            pevalAbsNumTerm (pevalUMinusNumTerm $ ssymTerm "a" :: Term Integer) @=? pevalAbsNumTerm (ssymTerm "a"),
          testCase "On Abs" $ do
            pevalAbsNumTerm (pevalAbsNumTerm $ ssymTerm "a" :: Term Integer) @=? pevalAbsNumTerm (ssymTerm "a"),
          testCase "On Times Integer" $ do
            pevalAbsNumTerm (pevalTimesNumTerm (ssymTerm "a") (ssymTerm "b") :: Term Integer)
              @=? pevalTimesNumTerm (pevalAbsNumTerm (ssymTerm "a")) (pevalAbsNumTerm (ssymTerm "b")),
          testCase "On Times BV" $ do
            pevalAbsNumTerm (pevalTimesNumTerm (ssymTerm "a") (ssymTerm "b") :: Term (IntN 5))
              @=? absNumTerm (pevalTimesNumTerm (ssymTerm "a") (ssymTerm "b") :: Term (IntN 5))
            pevalAbsNumTerm (pevalTimesNumTerm (ssymTerm "a") (ssymTerm "b") :: Term (WordN 5))
              @=? absNumTerm (pevalTimesNumTerm (ssymTerm "a") (ssymTerm "b") :: Term (WordN 5)),
          testCase "On symbolic" $ do
            pevalAbsNumTerm (ssymTerm "a" :: Term Integer)
              @=? absNumTerm (ssymTerm "a")
        ],
      testGroup
        "Signum"
        [ testCase "On concrete" $ do
            pevalSignumNumTerm (conTerm 10 :: Term Integer) @=? conTerm 1
            pevalSignumNumTerm (conTerm 0 :: Term Integer) @=? conTerm 0
            pevalSignumNumTerm (conTerm $ -10 :: Term Integer) @=? conTerm (-1),
          testCase "On UMinus Integer" $ do
            pevalSignumNumTerm (pevalUMinusNumTerm $ ssymTerm "a" :: Term Integer)
              @=? pevalUMinusNumTerm (pevalSignumNumTerm $ ssymTerm "a"),
          testCase "On UMinus BV" $ do
            pevalSignumNumTerm (pevalUMinusNumTerm $ ssymTerm "a" :: Term (IntN 5))
              @=? signumNumTerm (pevalUMinusNumTerm $ ssymTerm "a" :: Term (IntN 5))
            pevalSignumNumTerm (pevalUMinusNumTerm $ ssymTerm "a" :: Term (WordN 5))
              @=? signumNumTerm (pevalUMinusNumTerm $ ssymTerm "a" :: Term (WordN 5)),
          testCase "On Times Integer" $ do
            pevalSignumNumTerm (pevalTimesNumTerm (ssymTerm "a") (ssymTerm "b") :: Term Integer)
              @=? pevalTimesNumTerm (pevalSignumNumTerm $ ssymTerm "a") (pevalSignumNumTerm $ ssymTerm "b"),
          testCase "On Times BV" $ do
            pevalSignumNumTerm (pevalTimesNumTerm (ssymTerm "a") (ssymTerm "b") :: Term (IntN 5))
              @=? signumNumTerm (pevalTimesNumTerm (ssymTerm "a") (ssymTerm "b") :: Term (IntN 5))
            pevalSignumNumTerm (pevalTimesNumTerm (ssymTerm "a") (ssymTerm "b") :: Term (WordN 5))
              @=? signumNumTerm (pevalTimesNumTerm (ssymTerm "a") (ssymTerm "b") :: Term (WordN 5)),
          testCase "On symbolics" $ do
            pevalSignumNumTerm (ssymTerm "a" :: Term Integer)
              @=? signumNumTerm (ssymTerm "a")
        ],
      let concSignedBV :: Integer -> Term (IntN 5) = conTerm . fromInteger
          concUnsignedBV :: Integer -> Term (WordN 5) = conTerm . fromInteger
       in testGroup
            "Lt"
            [ testCase "On both concrete" $ do
                pevalLtNumTerm (conTerm 1 :: Term Integer) (conTerm 2) @=? conTerm True
                pevalLtNumTerm (conTerm 2 :: Term Integer) (conTerm 2) @=? conTerm False
                pevalLtNumTerm (conTerm 3 :: Term Integer) (conTerm 2) @=? conTerm False
                pevalLtNumTerm (conTerm 1 :: Term (IntN 2)) (conTerm 0) @=? conTerm False
                pevalLtNumTerm (conTerm 2 :: Term (IntN 2)) (conTerm 0) @=? conTerm True
                pevalLtNumTerm (conTerm 3 :: Term (IntN 2)) (conTerm 0) @=? conTerm True
                pevalLtNumTerm (conTerm 1 :: Term (WordN 2)) (conTerm 2) @=? conTerm True
                pevalLtNumTerm (conTerm 2 :: Term (WordN 2)) (conTerm 2) @=? conTerm False
                pevalLtNumTerm (conTerm 3 :: Term (WordN 2)) (conTerm 2) @=? conTerm False,
              testCase "On left constant and right add concrete Integers" $ do
                pevalLtNumTerm (conTerm 1 :: Term Integer) (pevalAddNumTerm (conTerm 2) (ssymTerm "a"))
                  @=? pevalLtNumTerm (conTerm $ -1 :: Term Integer) (ssymTerm "a"),
              testCase "On right constant left add concrete Integers" $ do
                pevalLtNumTerm (pevalAddNumTerm (conTerm 2) (ssymTerm "a")) (conTerm 1 :: Term Integer)
                  @=? pevalLtNumTerm (conTerm 1 :: Term Integer) (pevalUMinusNumTerm $ ssymTerm "a"),
              testCase "On right constant Integers" $ do
                pevalLtNumTerm (ssymTerm "a") (conTerm 1 :: Term Integer)
                  @=? pevalLtNumTerm (conTerm $ -1 :: Term Integer) (pevalUMinusNumTerm $ ssymTerm "a"),
              testCase "On right constant and left uminus Integers" $ do
                pevalLtNumTerm (pevalUMinusNumTerm $ ssymTerm "a") (conTerm 1 :: Term Integer)
                  @=? pevalLtNumTerm (conTerm $ -1 :: Term Integer) (ssymTerm "a"),
              testCase "On left add concrete Integers" $ do
                pevalLtNumTerm (pevalAddNumTerm (conTerm 2) (ssymTerm "a")) (ssymTerm "b" :: Term Integer)
                  @=? pevalLtNumTerm (conTerm 2 :: Term Integer) (pevalAddNumTerm (ssymTerm "b") (pevalUMinusNumTerm $ ssymTerm "a")),
              testCase "On right add concrete Integers" $ do
                pevalLtNumTerm (ssymTerm "b" :: Term Integer) (pevalAddNumTerm (conTerm 2) (ssymTerm "a"))
                  @=? pevalLtNumTerm (conTerm $ -2 :: Term Integer) (pevalAddNumTerm (ssymTerm "a") (pevalUMinusNumTerm $ ssymTerm "b")),
              testCase "On left constant and right add concrete BVs should not be simplified" $ do
                pevalLtNumTerm (concSignedBV 1) (pevalAddNumTerm (conTerm 2) (ssymTerm "a"))
                  @=? ltNumTerm (concSignedBV 1) (pevalAddNumTerm (concSignedBV 2) (ssymTerm "a"))
                pevalLtNumTerm (concUnsignedBV 1) (pevalAddNumTerm (conTerm 2) (ssymTerm "a"))
                  @=? ltNumTerm (concUnsignedBV 1) (pevalAddNumTerm (concUnsignedBV 2) (ssymTerm "a")),
              testCase "On right constant and left add concrete BVs should not be simplified" $ do
                pevalLtNumTerm (pevalAddNumTerm (concSignedBV 2) (ssymTerm "a")) (conTerm 1)
                  @=? ltNumTerm (pevalAddNumTerm (concSignedBV 2) (ssymTerm "a")) (concSignedBV 1)
                pevalLtNumTerm (pevalAddNumTerm (concUnsignedBV 2) (ssymTerm "a")) (conTerm 1)
                  @=? ltNumTerm (pevalAddNumTerm (concUnsignedBV 2) (ssymTerm "a")) (concUnsignedBV 1),
              testCase "On right constant BVs should not be simplified" $ do
                pevalLtNumTerm (ssymTerm "a") (concSignedBV 1)
                  @=? ltNumTerm (ssymTerm "a" :: Term (IntN 5)) (concSignedBV 1)
                pevalLtNumTerm (ssymTerm "a") (concUnsignedBV 1)
                  @=? ltNumTerm (ssymTerm "a" :: Term (WordN 5)) (concUnsignedBV 1),
              testCase "On right constant and left uminus BVs should not be simplified" $ do
                pevalLtNumTerm (pevalUMinusNumTerm $ ssymTerm "a") (concSignedBV 1)
                  @=? ltNumTerm (pevalUMinusNumTerm $ ssymTerm "a" :: Term (IntN 5)) (concSignedBV 1)
                pevalLtNumTerm (pevalUMinusNumTerm $ ssymTerm "a") (concUnsignedBV 1)
                  @=? ltNumTerm (pevalUMinusNumTerm $ ssymTerm "a" :: Term (WordN 5)) (concUnsignedBV 1),
              testCase "On left add concrete BVs should not be simplified" $ do
                pevalLtNumTerm (pevalAddNumTerm (concSignedBV 2) (ssymTerm "a")) (ssymTerm "b")
                  @=? ltNumTerm (pevalAddNumTerm (concSignedBV 2) (ssymTerm "a")) (ssymTerm "b" :: Term (IntN 5))
                pevalLtNumTerm (pevalAddNumTerm (concUnsignedBV 2) (ssymTerm "a")) (ssymTerm "b")
                  @=? ltNumTerm (pevalAddNumTerm (concUnsignedBV 2) (ssymTerm "a")) (ssymTerm "b" :: Term (WordN 5)),
              testCase "On right add concrete BVs should not be simplified" $ do
                pevalLtNumTerm (ssymTerm "b") (pevalAddNumTerm (concSignedBV 2) (ssymTerm "a"))
                  @=? ltNumTerm
                    (ssymTerm "b" :: Term (IntN 5))
                    (pevalAddNumTerm (concSignedBV 2) (ssymTerm "a"))
                pevalLtNumTerm (ssymTerm "b") (pevalAddNumTerm (concUnsignedBV 2) (ssymTerm "a"))
                  @=? ltNumTerm
                    (ssymTerm "b" :: Term (WordN 5))
                    (pevalAddNumTerm (concUnsignedBV 2) (ssymTerm "a")),
              testCase "On symbolic" $ do
                pevalLtNumTerm (ssymTerm "a" :: Term Integer) (ssymTerm "b")
                  @=? ltNumTerm (ssymTerm "a" :: Term Integer) (ssymTerm "b" :: Term Integer)
            ],
      let concSignedBV :: Integer -> Term (IntN 5) = conTerm . fromInteger
          concUnsignedBV :: Integer -> Term (WordN 5) = conTerm . fromInteger
       in testGroup
            "Le"
            [ testCase "On both concrete" $ do
                pevalLeNumTerm (conTerm 1 :: Term Integer) (conTerm 2) @=? conTerm True
                pevalLeNumTerm (conTerm 2 :: Term Integer) (conTerm 2) @=? conTerm True
                pevalLeNumTerm (conTerm 3 :: Term Integer) (conTerm 2) @=? conTerm False
                pevalLeNumTerm (conTerm 0 :: Term (IntN 2)) (conTerm 0) @=? conTerm True
                pevalLeNumTerm (conTerm 1 :: Term (IntN 2)) (conTerm 0) @=? conTerm False
                pevalLeNumTerm (conTerm 2 :: Term (IntN 2)) (conTerm 0) @=? conTerm True
                pevalLeNumTerm (conTerm 3 :: Term (IntN 2)) (conTerm 0) @=? conTerm True
                pevalLeNumTerm (conTerm 1 :: Term (WordN 2)) (conTerm 2) @=? conTerm True
                pevalLeNumTerm (conTerm 2 :: Term (WordN 2)) (conTerm 2) @=? conTerm True
                pevalLeNumTerm (conTerm 3 :: Term (WordN 2)) (conTerm 2) @=? conTerm False,
              testCase "On left constant and right add concrete Integers" $ do
                pevalLeNumTerm (conTerm 1 :: Term Integer) (pevalAddNumTerm (conTerm 2) (ssymTerm "a"))
                  @=? pevalLeNumTerm (conTerm $ -1 :: Term Integer) (ssymTerm "a"),
              testCase "On right constant and left add concrete Integers" $ do
                pevalLeNumTerm (pevalAddNumTerm (conTerm 2) (ssymTerm "a")) (conTerm 1 :: Term Integer)
                  @=? pevalLeNumTerm (conTerm 1 :: Term Integer) (pevalUMinusNumTerm $ ssymTerm "a"),
              testCase "On right constant Integers" $ do
                pevalLeNumTerm (ssymTerm "a") (conTerm 1 :: Term Integer)
                  @=? pevalLeNumTerm (conTerm $ -1 :: Term Integer) (pevalUMinusNumTerm $ ssymTerm "a"),
              testCase "On right constant left uminus Integers" $ do
                pevalLeNumTerm (pevalUMinusNumTerm $ ssymTerm "a") (conTerm 1 :: Term Integer)
                  @=? pevalLeNumTerm (conTerm $ -1 :: Term Integer) (ssymTerm "a"),
              testCase "On left add concrete Integers" $ do
                pevalLeNumTerm (pevalAddNumTerm (conTerm 2) (ssymTerm "a")) (ssymTerm "b" :: Term Integer)
                  @=? pevalLeNumTerm (conTerm 2 :: Term Integer) (pevalAddNumTerm (ssymTerm "b") (pevalUMinusNumTerm $ ssymTerm "a")),
              testCase "On right add concrete Integers" $ do
                pevalLeNumTerm (ssymTerm "b" :: Term Integer) (pevalAddNumTerm (conTerm 2) (ssymTerm "a"))
                  @=? pevalLeNumTerm (conTerm $ -2 :: Term Integer) (pevalAddNumTerm (ssymTerm "a") (pevalUMinusNumTerm $ ssymTerm "b")),
              testCase "On left constant and right add concrete BVs should not be simplified" $ do
                pevalLeNumTerm (concSignedBV 1) (pevalAddNumTerm (conTerm 2) (ssymTerm "a"))
                  @=? leNumTerm (concSignedBV 1) (pevalAddNumTerm (concSignedBV 2) (ssymTerm "a"))
                pevalLeNumTerm (concUnsignedBV 1) (pevalAddNumTerm (conTerm 2) (ssymTerm "a"))
                  @=? leNumTerm (concUnsignedBV 1) (pevalAddNumTerm (concUnsignedBV 2) (ssymTerm "a")),
              testCase "On right constant and left add concrete BVs should not be simplified" $ do
                pevalLeNumTerm (pevalAddNumTerm (concSignedBV 2) (ssymTerm "a")) (conTerm 1)
                  @=? leNumTerm (pevalAddNumTerm (concSignedBV 2) (ssymTerm "a")) (concSignedBV 1)
                pevalLeNumTerm (pevalAddNumTerm (concUnsignedBV 2) (ssymTerm "a")) (conTerm 1)
                  @=? leNumTerm (pevalAddNumTerm (concUnsignedBV 2) (ssymTerm "a")) (concUnsignedBV 1),
              testCase "On right constant BVs should not be simplified" $ do
                pevalLeNumTerm (ssymTerm "a") (concSignedBV 1)
                  @=? leNumTerm (ssymTerm "a" :: Term (IntN 5)) (concSignedBV 1)
                pevalLeNumTerm (ssymTerm "a") (concUnsignedBV 1)
                  @=? leNumTerm (ssymTerm "a" :: Term (WordN 5)) (concUnsignedBV 1),
              testCase "On right constant and left uminus BVs should not be simplified" $ do
                pevalLeNumTerm (pevalUMinusNumTerm $ ssymTerm "a") (concSignedBV 1)
                  @=? leNumTerm (pevalUMinusNumTerm $ ssymTerm "a" :: Term (IntN 5)) (concSignedBV 1)
                pevalLeNumTerm (pevalUMinusNumTerm $ ssymTerm "a") (concUnsignedBV 1)
                  @=? leNumTerm (pevalUMinusNumTerm $ ssymTerm "a" :: Term (WordN 5)) (concUnsignedBV 1),
              testCase "On left add concrete BVs should not be simplified" $ do
                pevalLeNumTerm (pevalAddNumTerm (concSignedBV 2) (ssymTerm "a")) (ssymTerm "b")
                  @=? leNumTerm (pevalAddNumTerm (concSignedBV 2) (ssymTerm "a")) (ssymTerm "b" :: Term (IntN 5))
                pevalLeNumTerm (pevalAddNumTerm (concUnsignedBV 2) (ssymTerm "a")) (ssymTerm "b")
                  @=? leNumTerm (pevalAddNumTerm (concUnsignedBV 2) (ssymTerm "a")) (ssymTerm "b" :: Term (WordN 5)),
              testCase "Lt on right add concrete BVs should not be simplified" $ do
                pevalLeNumTerm (ssymTerm "b") (pevalAddNumTerm (concSignedBV 2) (ssymTerm "a"))
                  @=? leNumTerm
                    (ssymTerm "b" :: Term (IntN 5))
                    (pevalAddNumTerm (concSignedBV 2) (ssymTerm "a"))
                pevalLeNumTerm (ssymTerm "b") (pevalAddNumTerm (concUnsignedBV 2) (ssymTerm "a"))
                  @=? leNumTerm
                    (ssymTerm "b" :: Term (WordN 5))
                    (pevalAddNumTerm (concUnsignedBV 2) (ssymTerm "a")),
              testCase "On symbolic" $ do
                pevalLeNumTerm (ssymTerm "a" :: Term Integer) (ssymTerm "b")
                  @=? leNumTerm (ssymTerm "a" :: Term Integer) (ssymTerm "b" :: Term Integer)
            ],
      testCase "Gt should be delegated to Lt" $
        pevalGtNumTerm (ssymTerm "a" :: Term Integer) (ssymTerm "b")
          @=? pevalLtNumTerm (ssymTerm "b" :: Term Integer) (ssymTerm "a"),
      testCase "Ge should be delegated to Le" $ do
        pevalGeNumTerm (ssymTerm "a" :: Term Integer) (ssymTerm "b")
          @=? pevalLeNumTerm (ssymTerm "b" :: Term Integer) (ssymTerm "a")
    ]
