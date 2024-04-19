{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.SymPrim.Prim.NumTests (numTests) where

import Grisette.Core.Data.BV (IntN, WordN)
import Grisette.SymPrim.Prim.Term
  ( PEvalNumTerm
      ( pevalAbsNumTerm,
        pevalAddNumTerm,
        pevalMulNumTerm,
        pevalNegNumTerm,
        pevalSignumNumTerm
      ),
    PEvalOrdTerm (pevalLeOrdTerm, pevalLtOrdTerm),
    SupportedPrim (pevalITETerm),
    Term,
    absNumTerm,
    addNumTerm,
    conTerm,
    leOrdTerm,
    ltOrdTerm,
    mulNumTerm,
    negNumTerm,
    pevalGeOrdTerm,
    pevalGtOrdTerm,
    pevalSubNumTerm,
    signumNumTerm,
    ssymTerm,
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
          testCase "On both neg" $ do
            pevalAddNumTerm (pevalNegNumTerm $ ssymTerm "a" :: Term Integer) (pevalNegNumTerm $ ssymTerm "b")
              @=? pevalNegNumTerm (pevalAddNumTerm (ssymTerm "a") (ssymTerm "b")),
          testCase "On both mul the same concrete" $ do
            pevalAddNumTerm
              (pevalMulNumTerm (conTerm 3) (ssymTerm "a") :: Term Integer)
              (pevalMulNumTerm (conTerm 3) (ssymTerm "b"))
              @=? pevalMulNumTerm (conTerm 3) (pevalAddNumTerm (ssymTerm "a") (ssymTerm "b")),
          testCase "On both mul the same symbolic" $ do
            pevalAddNumTerm
              (pevalMulNumTerm (conTerm 3) (ssymTerm "a") :: Term Integer)
              (pevalMulNumTerm (conTerm 3) (ssymTerm "a"))
              @=? pevalMulNumTerm (conTerm 6) (ssymTerm "a")
            pevalAddNumTerm
              (pevalMulNumTerm (conTerm 3) (ssymTerm "a") :: Term Integer)
              (pevalMulNumTerm (conTerm 4) (ssymTerm "a"))
              @=? pevalMulNumTerm (conTerm 7) (ssymTerm "a"),
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
        "sub"
        [ testCase "sub num should be delegated to add and neg" $ do
            pevalSubNumTerm (ssymTerm "a" :: Term Integer) (ssymTerm "b")
              @=? pevalAddNumTerm (ssymTerm "a") (pevalNegNumTerm $ ssymTerm "b")
        ],
      testGroup
        "Neg"
        [ testCase "On concrete" $ do
            pevalNegNumTerm (conTerm 1 :: Term Integer) @=? conTerm (-1)
            pevalNegNumTerm (conTerm 1 :: Term (WordN 3)) @=? conTerm (-1),
          testCase "On Neg" $ do
            pevalNegNumTerm (pevalNegNumTerm (ssymTerm "a" :: Term Integer)) @=? ssymTerm "a",
          testCase "On Add concrete" $ do
            pevalNegNumTerm (pevalAddNumTerm (conTerm 1) (ssymTerm "a" :: Term Integer))
              @=? pevalAddNumTerm (conTerm $ -1) (pevalNegNumTerm $ ssymTerm "a"),
          testCase "On Add neg" $ do
            pevalNegNumTerm (pevalAddNumTerm (pevalNegNumTerm $ ssymTerm "a") (ssymTerm "b" :: Term Integer))
              @=? pevalAddNumTerm (ssymTerm "a") (pevalNegNumTerm $ ssymTerm "b")
            pevalNegNumTerm (pevalAddNumTerm (ssymTerm "a") (pevalNegNumTerm $ ssymTerm "b" :: Term Integer))
              @=? pevalAddNumTerm (pevalNegNumTerm $ ssymTerm "a") (ssymTerm "b"),
          testCase "On Mul concrete" $ do
            pevalNegNumTerm (pevalMulNumTerm (conTerm 3) (ssymTerm "a" :: Term Integer))
              @=? pevalMulNumTerm (conTerm $ -3) (ssymTerm "a"),
          testCase "On symbolic" $ do
            pevalNegNumTerm (ssymTerm "a" :: Term Integer)
              @=? negNumTerm (ssymTerm "a")
        ],
      testGroup
        "Mul"
        [ testCase "On both concrete" $ do
            pevalMulNumTerm (conTerm 3 :: Term Integer) (conTerm 5)
              @=? conTerm 15,
          testCase "On left 0" $ do
            pevalMulNumTerm (conTerm 0 :: Term Integer) (ssymTerm "a")
              @=? conTerm 0,
          testCase "On right 0" $ do
            pevalMulNumTerm (ssymTerm "a") (conTerm 0 :: Term Integer)
              @=? conTerm 0,
          testCase "On left 1" $ do
            pevalMulNumTerm (conTerm 1 :: Term Integer) (ssymTerm "a")
              @=? ssymTerm "a",
          testCase "On right 1" $ do
            pevalMulNumTerm (ssymTerm "a") (conTerm 1 :: Term Integer)
              @=? ssymTerm "a",
          testCase "On left -1" $ do
            pevalMulNumTerm (conTerm $ -1 :: Term Integer) (ssymTerm "a")
              @=? pevalNegNumTerm (ssymTerm "a"),
          testCase "On right -1" $ do
            pevalMulNumTerm (ssymTerm "a") (conTerm $ -1 :: Term Integer)
              @=? pevalNegNumTerm (ssymTerm "a"),
          testCase "On left concrete and right mul concrete symbolics" $ do
            pevalMulNumTerm (conTerm 3) (pevalMulNumTerm (conTerm 5 :: Term Integer) (ssymTerm "a"))
              @=? pevalMulNumTerm (conTerm 15) (ssymTerm "a"),
          testCase "On right concrete and left mul concrete symbolics" $ do
            pevalMulNumTerm (pevalMulNumTerm (conTerm 5 :: Term Integer) (ssymTerm "a")) (conTerm 3)
              @=? pevalMulNumTerm (conTerm 15) (ssymTerm "a"),
          testCase "On left concrete and right add concrete symbolics" $ do
            pevalMulNumTerm (conTerm 3) (pevalAddNumTerm (conTerm 5 :: Term Integer) (ssymTerm "a"))
              @=? pevalAddNumTerm (conTerm 15) (pevalMulNumTerm (conTerm 3) (ssymTerm "a")),
          testCase "On right concrete and left add concrete symbolics" $ do
            pevalMulNumTerm (pevalAddNumTerm (conTerm 5 :: Term Integer) (ssymTerm "a")) (conTerm 3)
              @=? pevalAddNumTerm (conTerm 15) (pevalMulNumTerm (conTerm 3) (ssymTerm "a")),
          testCase "On left concrete and right neg" $ do
            pevalMulNumTerm (conTerm 3 :: Term Integer) (pevalNegNumTerm (ssymTerm "a"))
              @=? pevalMulNumTerm (conTerm $ -3) (ssymTerm "a"),
          testCase "On left mul concrete symbolics" $ do
            pevalMulNumTerm (pevalMulNumTerm (conTerm 3 :: Term Integer) (ssymTerm "a")) (ssymTerm "b")
              @=? pevalMulNumTerm (conTerm 3) (pevalMulNumTerm (ssymTerm "a") (ssymTerm "b")),
          testCase "On right mul concrete symbolics" $ do
            pevalMulNumTerm (ssymTerm "b") (pevalMulNumTerm (conTerm 3 :: Term Integer) (ssymTerm "a"))
              @=? pevalMulNumTerm (conTerm 3) (pevalMulNumTerm (ssymTerm "b") (ssymTerm "a")),
          testCase "On left neg" $ do
            pevalMulNumTerm (pevalNegNumTerm $ ssymTerm "a") (ssymTerm "b" :: Term Integer)
              @=? pevalNegNumTerm (pevalMulNumTerm (ssymTerm "a") (ssymTerm "b")),
          testCase "On right neg" $ do
            pevalMulNumTerm (ssymTerm "a") (pevalNegNumTerm $ ssymTerm "b" :: Term Integer)
              @=? pevalNegNumTerm (pevalMulNumTerm (ssymTerm "a") (ssymTerm "b")),
          testCase "On right concrete and left neg" $ do
            pevalMulNumTerm (pevalNegNumTerm (ssymTerm "a")) (conTerm 3 :: Term Integer)
              @=? pevalMulNumTerm (conTerm $ -3) (ssymTerm "a"),
          testCase "On left concrete" $ do
            pevalMulNumTerm (conTerm 3 :: Term Integer) (ssymTerm "a")
              @=? mulNumTerm
                (conTerm 3 :: Term Integer)
                (ssymTerm "a" :: Term Integer),
          testCase "On right concrete" $ do
            pevalMulNumTerm (ssymTerm "a") (conTerm 3 :: Term Integer)
              @=? mulNumTerm
                (conTerm 3 :: Term Integer)
                (ssymTerm "a" :: Term Integer),
          testCase "On no concrete" $ do
            pevalMulNumTerm (ssymTerm "a") (ssymTerm "b" :: Term Integer)
              @=? mulNumTerm (ssymTerm "a" :: Term Integer) (ssymTerm "b" :: Term Integer),
          testCase "Unfold 1" $ do
            pevalMulNumTerm
              (conTerm 3)
              (pevalITETerm (ssymTerm "a") (conTerm 5 :: Term Integer) (ssymTerm "a"))
              @=? pevalITETerm (ssymTerm "a") (conTerm 15) (pevalMulNumTerm (conTerm 3) (ssymTerm "a"))
            pevalMulNumTerm
              (pevalITETerm (ssymTerm "a") (conTerm 5 :: Term Integer) (ssymTerm "a"))
              (conTerm 3)
              @=? pevalITETerm (ssymTerm "a") (conTerm 15) (pevalMulNumTerm (ssymTerm "a") (conTerm 3))
        ],
      testGroup
        "Abs"
        [ testCase "On concrete" $ do
            pevalAbsNumTerm (conTerm 10 :: Term Integer) @=? conTerm 10
            pevalAbsNumTerm (conTerm $ -10 :: Term Integer) @=? conTerm 10,
          testCase "On Neg Integer" $ do
            pevalAbsNumTerm (pevalNegNumTerm $ ssymTerm "a" :: Term Integer) @=? pevalAbsNumTerm (ssymTerm "a"),
          testCase "On Neg BV" $ do
            pevalAbsNumTerm (pevalNegNumTerm $ ssymTerm "a" :: Term (IntN 5)) @=? pevalAbsNumTerm (ssymTerm "a")
            pevalAbsNumTerm (pevalNegNumTerm $ ssymTerm "a" :: Term (WordN 5)) @=? negNumTerm (ssymTerm "a"),
          testCase "On Abs Integer" $ do
            pevalAbsNumTerm (pevalAbsNumTerm $ ssymTerm "a" :: Term Integer) @=? pevalAbsNumTerm (ssymTerm "a"),
          testCase "On Abs BV" $ do
            pevalAbsNumTerm (pevalAbsNumTerm $ ssymTerm "a" :: Term (IntN 5)) @=? pevalAbsNumTerm (ssymTerm "a")
            pevalAbsNumTerm (pevalAbsNumTerm $ ssymTerm "a" :: Term (WordN 5)) @=? ssymTerm "a",
          testCase "On Mul Integer" $ do
            pevalAbsNumTerm (pevalMulNumTerm (ssymTerm "a") (ssymTerm "b") :: Term Integer)
              @=? pevalMulNumTerm (pevalAbsNumTerm (ssymTerm "a")) (pevalAbsNumTerm (ssymTerm "b")),
          testCase "On Mul BV" $ do
            pevalAbsNumTerm (pevalMulNumTerm (ssymTerm "a") (ssymTerm "b") :: Term (IntN 5))
              @=? absNumTerm (pevalMulNumTerm (ssymTerm "a") (ssymTerm "b") :: Term (IntN 5))
            pevalAbsNumTerm (pevalMulNumTerm (ssymTerm "a") (ssymTerm "b") :: Term (WordN 5))
              @=? pevalMulNumTerm (ssymTerm "a") (ssymTerm "b"),
          testCase "On symbolic Integer" $ do
            pevalAbsNumTerm (ssymTerm "a" :: Term Integer)
              @=? absNumTerm (ssymTerm "a"),
          testCase "On symbolic BV" $ do
            pevalAbsNumTerm (ssymTerm "a" :: Term (IntN 5)) @=? absNumTerm (ssymTerm "a")
            pevalAbsNumTerm (ssymTerm "a" :: Term (WordN 5)) @=? ssymTerm "a"
        ],
      testGroup
        "Signum"
        [ testCase "On concrete" $ do
            pevalSignumNumTerm (conTerm 10 :: Term Integer) @=? conTerm 1
            pevalSignumNumTerm (conTerm 0 :: Term Integer) @=? conTerm 0
            pevalSignumNumTerm (conTerm $ -10 :: Term Integer) @=? conTerm (-1),
          testCase "On Neg Integer" $ do
            pevalSignumNumTerm (pevalNegNumTerm $ ssymTerm "a" :: Term Integer)
              @=? pevalNegNumTerm (pevalSignumNumTerm $ ssymTerm "a"),
          testCase "On Neg BV" $ do
            pevalSignumNumTerm (pevalNegNumTerm $ ssymTerm "a" :: Term (IntN 5))
              @=? signumNumTerm (pevalNegNumTerm $ ssymTerm "a" :: Term (IntN 5))
            pevalSignumNumTerm (pevalNegNumTerm $ ssymTerm "a" :: Term (WordN 5))
              @=? signumNumTerm (pevalNegNumTerm $ ssymTerm "a" :: Term (WordN 5)),
          testCase "On Mul Integer" $ do
            pevalSignumNumTerm (pevalMulNumTerm (ssymTerm "a") (ssymTerm "b") :: Term Integer)
              @=? pevalMulNumTerm (pevalSignumNumTerm $ ssymTerm "a") (pevalSignumNumTerm $ ssymTerm "b"),
          testCase "On Mul BV" $ do
            pevalSignumNumTerm (pevalMulNumTerm (ssymTerm "a") (ssymTerm "b") :: Term (IntN 5))
              @=? signumNumTerm (pevalMulNumTerm (ssymTerm "a") (ssymTerm "b") :: Term (IntN 5))
            pevalSignumNumTerm (pevalMulNumTerm (ssymTerm "a") (ssymTerm "b") :: Term (WordN 5))
              @=? signumNumTerm (pevalMulNumTerm (ssymTerm "a") (ssymTerm "b") :: Term (WordN 5)),
          testCase "On symbolics" $ do
            pevalSignumNumTerm (ssymTerm "a" :: Term Integer)
              @=? signumNumTerm (ssymTerm "a")
        ],
      let concSignedBV :: Integer -> Term (IntN 5) = conTerm . fromInteger
          concUnsignedBV :: Integer -> Term (WordN 5) = conTerm . fromInteger
       in testGroup
            "Lt"
            [ testCase "On both concrete" $ do
                pevalLtOrdTerm (conTerm 1 :: Term Integer) (conTerm 2) @=? conTerm True
                pevalLtOrdTerm (conTerm 2 :: Term Integer) (conTerm 2) @=? conTerm False
                pevalLtOrdTerm (conTerm 3 :: Term Integer) (conTerm 2) @=? conTerm False
                pevalLtOrdTerm (conTerm 1 :: Term (IntN 2)) (conTerm 0) @=? conTerm False
                pevalLtOrdTerm (conTerm 2 :: Term (IntN 2)) (conTerm 0) @=? conTerm True
                pevalLtOrdTerm (conTerm 3 :: Term (IntN 2)) (conTerm 0) @=? conTerm True
                pevalLtOrdTerm (conTerm 1 :: Term (WordN 2)) (conTerm 2) @=? conTerm True
                pevalLtOrdTerm (conTerm 2 :: Term (WordN 2)) (conTerm 2) @=? conTerm False
                pevalLtOrdTerm (conTerm 3 :: Term (WordN 2)) (conTerm 2) @=? conTerm False,
              testCase "On left constant and right add concrete Integers" $ do
                pevalLtOrdTerm (conTerm 1 :: Term Integer) (pevalAddNumTerm (conTerm 2) (ssymTerm "a"))
                  @=? pevalLtOrdTerm (conTerm $ -1 :: Term Integer) (ssymTerm "a"),
              testCase "On right constant left add concrete Integers" $ do
                pevalLtOrdTerm (pevalAddNumTerm (conTerm 2) (ssymTerm "a")) (conTerm 1 :: Term Integer)
                  @=? pevalLtOrdTerm (conTerm 1 :: Term Integer) (pevalNegNumTerm $ ssymTerm "a"),
              testCase "On right constant Integers" $ do
                pevalLtOrdTerm (ssymTerm "a") (conTerm 1 :: Term Integer)
                  @=? pevalLtOrdTerm (conTerm $ -1 :: Term Integer) (pevalNegNumTerm $ ssymTerm "a"),
              testCase "On right constant and left neg Integers" $ do
                pevalLtOrdTerm (pevalNegNumTerm $ ssymTerm "a") (conTerm 1 :: Term Integer)
                  @=? pevalLtOrdTerm (conTerm $ -1 :: Term Integer) (ssymTerm "a"),
              testCase "On left add concrete Integers" $ do
                pevalLtOrdTerm (pevalAddNumTerm (conTerm 2) (ssymTerm "a")) (ssymTerm "b" :: Term Integer)
                  @=? pevalLtOrdTerm (conTerm 2 :: Term Integer) (pevalAddNumTerm (ssymTerm "b") (pevalNegNumTerm $ ssymTerm "a")),
              testCase "On right add concrete Integers" $ do
                pevalLtOrdTerm (ssymTerm "b" :: Term Integer) (pevalAddNumTerm (conTerm 2) (ssymTerm "a"))
                  @=? pevalLtOrdTerm (conTerm $ -2 :: Term Integer) (pevalAddNumTerm (ssymTerm "a") (pevalNegNumTerm $ ssymTerm "b")),
              testCase "On left constant and right add concrete BVs should not be simplified" $ do
                pevalLtOrdTerm (concSignedBV 1) (pevalAddNumTerm (conTerm 2) (ssymTerm "a"))
                  @=? ltOrdTerm (concSignedBV 1) (pevalAddNumTerm (concSignedBV 2) (ssymTerm "a"))
                pevalLtOrdTerm (concUnsignedBV 1) (pevalAddNumTerm (conTerm 2) (ssymTerm "a"))
                  @=? ltOrdTerm (concUnsignedBV 1) (pevalAddNumTerm (concUnsignedBV 2) (ssymTerm "a")),
              testCase "On right constant and left add concrete BVs should not be simplified" $ do
                pevalLtOrdTerm (pevalAddNumTerm (concSignedBV 2) (ssymTerm "a")) (conTerm 1)
                  @=? ltOrdTerm (pevalAddNumTerm (concSignedBV 2) (ssymTerm "a")) (concSignedBV 1)
                pevalLtOrdTerm (pevalAddNumTerm (concUnsignedBV 2) (ssymTerm "a")) (conTerm 1)
                  @=? ltOrdTerm (pevalAddNumTerm (concUnsignedBV 2) (ssymTerm "a")) (concUnsignedBV 1),
              testCase "On right constant BVs should not be simplified" $ do
                pevalLtOrdTerm (ssymTerm "a") (concSignedBV 1)
                  @=? ltOrdTerm (ssymTerm "a" :: Term (IntN 5)) (concSignedBV 1)
                pevalLtOrdTerm (ssymTerm "a") (concUnsignedBV 1)
                  @=? ltOrdTerm (ssymTerm "a" :: Term (WordN 5)) (concUnsignedBV 1),
              testCase "On right constant and left neg BVs should not be simplified" $ do
                pevalLtOrdTerm (pevalNegNumTerm $ ssymTerm "a") (concSignedBV 1)
                  @=? ltOrdTerm (pevalNegNumTerm $ ssymTerm "a" :: Term (IntN 5)) (concSignedBV 1)
                pevalLtOrdTerm (pevalNegNumTerm $ ssymTerm "a") (concUnsignedBV 1)
                  @=? ltOrdTerm (pevalNegNumTerm $ ssymTerm "a" :: Term (WordN 5)) (concUnsignedBV 1),
              testCase "On left add concrete BVs should not be simplified" $ do
                pevalLtOrdTerm (pevalAddNumTerm (concSignedBV 2) (ssymTerm "a")) (ssymTerm "b")
                  @=? ltOrdTerm (pevalAddNumTerm (concSignedBV 2) (ssymTerm "a")) (ssymTerm "b" :: Term (IntN 5))
                pevalLtOrdTerm (pevalAddNumTerm (concUnsignedBV 2) (ssymTerm "a")) (ssymTerm "b")
                  @=? ltOrdTerm (pevalAddNumTerm (concUnsignedBV 2) (ssymTerm "a")) (ssymTerm "b" :: Term (WordN 5)),
              testCase "On right add concrete BVs should not be simplified" $ do
                pevalLtOrdTerm (ssymTerm "b") (pevalAddNumTerm (concSignedBV 2) (ssymTerm "a"))
                  @=? ltOrdTerm
                    (ssymTerm "b" :: Term (IntN 5))
                    (pevalAddNumTerm (concSignedBV 2) (ssymTerm "a"))
                pevalLtOrdTerm (ssymTerm "b") (pevalAddNumTerm (concUnsignedBV 2) (ssymTerm "a"))
                  @=? ltOrdTerm
                    (ssymTerm "b" :: Term (WordN 5))
                    (pevalAddNumTerm (concUnsignedBV 2) (ssymTerm "a")),
              testCase "On symbolic" $ do
                pevalLtOrdTerm (ssymTerm "a" :: Term Integer) (ssymTerm "b")
                  @=? ltOrdTerm (ssymTerm "a" :: Term Integer) (ssymTerm "b" :: Term Integer)
            ],
      let concSignedBV :: Integer -> Term (IntN 5) = conTerm . fromInteger
          concUnsignedBV :: Integer -> Term (WordN 5) = conTerm . fromInteger
       in testGroup
            "Le"
            [ testCase "On both concrete" $ do
                pevalLeOrdTerm (conTerm 1 :: Term Integer) (conTerm 2) @=? conTerm True
                pevalLeOrdTerm (conTerm 2 :: Term Integer) (conTerm 2) @=? conTerm True
                pevalLeOrdTerm (conTerm 3 :: Term Integer) (conTerm 2) @=? conTerm False
                pevalLeOrdTerm (conTerm 0 :: Term (IntN 2)) (conTerm 0) @=? conTerm True
                pevalLeOrdTerm (conTerm 1 :: Term (IntN 2)) (conTerm 0) @=? conTerm False
                pevalLeOrdTerm (conTerm 2 :: Term (IntN 2)) (conTerm 0) @=? conTerm True
                pevalLeOrdTerm (conTerm 3 :: Term (IntN 2)) (conTerm 0) @=? conTerm True
                pevalLeOrdTerm (conTerm 1 :: Term (WordN 2)) (conTerm 2) @=? conTerm True
                pevalLeOrdTerm (conTerm 2 :: Term (WordN 2)) (conTerm 2) @=? conTerm True
                pevalLeOrdTerm (conTerm 3 :: Term (WordN 2)) (conTerm 2) @=? conTerm False,
              testCase "On left constant and right add concrete Integers" $ do
                pevalLeOrdTerm (conTerm 1 :: Term Integer) (pevalAddNumTerm (conTerm 2) (ssymTerm "a"))
                  @=? pevalLeOrdTerm (conTerm $ -1 :: Term Integer) (ssymTerm "a"),
              testCase "On right constant and left add concrete Integers" $ do
                pevalLeOrdTerm (pevalAddNumTerm (conTerm 2) (ssymTerm "a")) (conTerm 1 :: Term Integer)
                  @=? pevalLeOrdTerm (conTerm 1 :: Term Integer) (pevalNegNumTerm $ ssymTerm "a"),
              testCase "On right constant Integers" $ do
                pevalLeOrdTerm (ssymTerm "a") (conTerm 1 :: Term Integer)
                  @=? pevalLeOrdTerm (conTerm $ -1 :: Term Integer) (pevalNegNumTerm $ ssymTerm "a"),
              testCase "On right constant left neg Integers" $ do
                pevalLeOrdTerm (pevalNegNumTerm $ ssymTerm "a") (conTerm 1 :: Term Integer)
                  @=? pevalLeOrdTerm (conTerm $ -1 :: Term Integer) (ssymTerm "a"),
              testCase "On left add concrete Integers" $ do
                pevalLeOrdTerm (pevalAddNumTerm (conTerm 2) (ssymTerm "a")) (ssymTerm "b" :: Term Integer)
                  @=? pevalLeOrdTerm (conTerm 2 :: Term Integer) (pevalAddNumTerm (ssymTerm "b") (pevalNegNumTerm $ ssymTerm "a")),
              testCase "On right add concrete Integers" $ do
                pevalLeOrdTerm (ssymTerm "b" :: Term Integer) (pevalAddNumTerm (conTerm 2) (ssymTerm "a"))
                  @=? pevalLeOrdTerm (conTerm $ -2 :: Term Integer) (pevalAddNumTerm (ssymTerm "a") (pevalNegNumTerm $ ssymTerm "b")),
              testCase "On left constant and right add concrete BVs should not be simplified" $ do
                pevalLeOrdTerm (concSignedBV 1) (pevalAddNumTerm (conTerm 2) (ssymTerm "a"))
                  @=? leOrdTerm (concSignedBV 1) (pevalAddNumTerm (concSignedBV 2) (ssymTerm "a"))
                pevalLeOrdTerm (concUnsignedBV 1) (pevalAddNumTerm (conTerm 2) (ssymTerm "a"))
                  @=? leOrdTerm (concUnsignedBV 1) (pevalAddNumTerm (concUnsignedBV 2) (ssymTerm "a")),
              testCase "On right constant and left add concrete BVs should not be simplified" $ do
                pevalLeOrdTerm (pevalAddNumTerm (concSignedBV 2) (ssymTerm "a")) (conTerm 1)
                  @=? leOrdTerm (pevalAddNumTerm (concSignedBV 2) (ssymTerm "a")) (concSignedBV 1)
                pevalLeOrdTerm (pevalAddNumTerm (concUnsignedBV 2) (ssymTerm "a")) (conTerm 1)
                  @=? leOrdTerm (pevalAddNumTerm (concUnsignedBV 2) (ssymTerm "a")) (concUnsignedBV 1),
              testCase "On right constant BVs should not be simplified" $ do
                pevalLeOrdTerm (ssymTerm "a") (concSignedBV 1)
                  @=? leOrdTerm (ssymTerm "a" :: Term (IntN 5)) (concSignedBV 1)
                pevalLeOrdTerm (ssymTerm "a") (concUnsignedBV 1)
                  @=? leOrdTerm (ssymTerm "a" :: Term (WordN 5)) (concUnsignedBV 1),
              testCase "On right constant and left neg BVs should not be simplified" $ do
                pevalLeOrdTerm (pevalNegNumTerm $ ssymTerm "a") (concSignedBV 1)
                  @=? leOrdTerm (pevalNegNumTerm $ ssymTerm "a" :: Term (IntN 5)) (concSignedBV 1)
                pevalLeOrdTerm (pevalNegNumTerm $ ssymTerm "a") (concUnsignedBV 1)
                  @=? leOrdTerm (pevalNegNumTerm $ ssymTerm "a" :: Term (WordN 5)) (concUnsignedBV 1),
              testCase "On left add concrete BVs should not be simplified" $ do
                pevalLeOrdTerm (pevalAddNumTerm (concSignedBV 2) (ssymTerm "a")) (ssymTerm "b")
                  @=? leOrdTerm (pevalAddNumTerm (concSignedBV 2) (ssymTerm "a")) (ssymTerm "b" :: Term (IntN 5))
                pevalLeOrdTerm (pevalAddNumTerm (concUnsignedBV 2) (ssymTerm "a")) (ssymTerm "b")
                  @=? leOrdTerm (pevalAddNumTerm (concUnsignedBV 2) (ssymTerm "a")) (ssymTerm "b" :: Term (WordN 5)),
              testCase "Lt on right add concrete BVs should not be simplified" $ do
                pevalLeOrdTerm (ssymTerm "b") (pevalAddNumTerm (concSignedBV 2) (ssymTerm "a"))
                  @=? leOrdTerm
                    (ssymTerm "b" :: Term (IntN 5))
                    (pevalAddNumTerm (concSignedBV 2) (ssymTerm "a"))
                pevalLeOrdTerm (ssymTerm "b") (pevalAddNumTerm (concUnsignedBV 2) (ssymTerm "a"))
                  @=? leOrdTerm
                    (ssymTerm "b" :: Term (WordN 5))
                    (pevalAddNumTerm (concUnsignedBV 2) (ssymTerm "a")),
              testCase "On symbolic" $ do
                pevalLeOrdTerm (ssymTerm "a" :: Term Integer) (ssymTerm "b")
                  @=? leOrdTerm (ssymTerm "a" :: Term Integer) (ssymTerm "b" :: Term Integer)
            ],
      testCase "Gt should be delegated to Lt" $
        pevalGtOrdTerm (ssymTerm "a" :: Term Integer) (ssymTerm "b")
          @=? pevalLtOrdTerm (ssymTerm "b" :: Term Integer) (ssymTerm "a"),
      testCase "Ge should be delegated to Le" $ do
        pevalGeOrdTerm (ssymTerm "a" :: Term Integer) (ssymTerm "b")
          @=? pevalLeOrdTerm (ssymTerm "b" :: Term Integer) (ssymTerm "a")
    ]
