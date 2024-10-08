{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.SymPrim.Prim.BoolTests (boolTests) where

import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.Prim.Term
  ( PEvalNumTerm (pevalAddNumTerm),
    SupportedPrim (pevalITETerm),
    Term,
    andTerm,
    conTerm,
    eqTerm,
    notTerm,
    orTerm,
    pevalAndTerm,
    pevalEqTerm,
    pevalImplyTerm,
    pevalNEqTerm,
    pevalNotTerm,
    pevalOrTerm,
    pevalXorTerm,
    ssymTerm,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

boolTests :: Test
boolTests =
  testGroup
    "Bool"
    [ testGroup
        "Not"
        [ testCase "On concrete" $ do
            pevalNotTerm (conTerm True) @?= conTerm False
            pevalNotTerm (conTerm True) @?= conTerm False,
          testCase "On general symbolic" $ do
            pevalNotTerm (ssymTerm "a") @?= notTerm (ssymTerm "a" :: Term Bool),
          testCase "On Not" $ do
            pevalNotTerm (pevalNotTerm (ssymTerm "a")) @?= ssymTerm "a",
          testCase "On Or Not" $ do
            pevalNotTerm (pevalOrTerm (pevalNotTerm (ssymTerm "a")) (ssymTerm "b"))
              @?= pevalAndTerm (ssymTerm "a") (pevalNotTerm (ssymTerm "b"))
            pevalNotTerm (pevalOrTerm (ssymTerm "a") (pevalNotTerm (ssymTerm "b")))
              @?= pevalAndTerm (pevalNotTerm (ssymTerm "a")) (ssymTerm "b"),
          testCase "On And Not" $ do
            pevalNotTerm (pevalAndTerm (pevalNotTerm (ssymTerm "a")) (ssymTerm "b"))
              @?= pevalOrTerm (ssymTerm "a") (pevalNotTerm (ssymTerm "b"))
            pevalNotTerm (pevalAndTerm (ssymTerm "a") (pevalNotTerm (ssymTerm "b")))
              @?= pevalOrTerm (pevalNotTerm (ssymTerm "a")) (ssymTerm "b")
        ],
      testGroup
        "Eqv & NEqv"
        [ testCase "Eqv on both concrete" $ do
            pevalEqTerm (conTerm True) (conTerm True) @?= conTerm True
            pevalEqTerm (conTerm True) (conTerm False) @?= conTerm False
            pevalEqTerm (conTerm False) (conTerm True) @?= conTerm False
            pevalEqTerm (conTerm False) (conTerm False) @?= conTerm True
            pevalEqTerm (conTerm (1 :: Integer)) (conTerm 1) @?= conTerm True
            pevalEqTerm (conTerm (1 :: Integer)) (conTerm 2) @?= conTerm False
            pevalEqTerm (conTerm (1 :: IntN 4)) (conTerm 1) @?= conTerm True
            pevalEqTerm (conTerm (1 :: IntN 4)) (conTerm 2) @?= conTerm False
            pevalEqTerm (conTerm (1 :: WordN 4)) (conTerm 1) @?= conTerm True
            pevalEqTerm (conTerm (1 :: WordN 4)) (conTerm 2) @?= conTerm False,
          testCase "Eqv on single concrete always put concrete ones in the right" $ do
            pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)
              @?= eqTerm (ssymTerm "a" :: Term Integer) (conTerm 1 :: Term Integer)
            pevalEqTerm (conTerm 1) (ssymTerm "a" :: Term Integer)
              @?= eqTerm (ssymTerm "a" :: Term Integer) (conTerm 1 :: Term Integer),
          testCase "Eqv on general symbolic" $ do
            pevalEqTerm (ssymTerm "a" :: Term Integer) (ssymTerm "b")
              @?= eqTerm (ssymTerm "a" :: Term Integer) (ssymTerm "b" :: Term Integer),
          testCase "Eqv on Bool with single concrete" $ do
            pevalEqTerm (conTerm True) (ssymTerm "a") @?= ssymTerm "a"
            pevalEqTerm (ssymTerm "a") (conTerm True) @?= ssymTerm "a"
            pevalEqTerm (conTerm False) (ssymTerm "a") @?= pevalNotTerm (ssymTerm "a")
            pevalEqTerm (ssymTerm "a") (conTerm False) @?= pevalNotTerm (ssymTerm "a"),
          testCase "NEqv on general symbolic" $ do
            pevalNEqTerm (ssymTerm "a" :: Term Integer) (ssymTerm "b")
              @?= pevalNotTerm (pevalEqTerm (ssymTerm "a" :: Term Integer) (ssymTerm "b")),
          testCase "Eqv(Not(x), x) / Eqv(x, Not(x))" $ do
            pevalEqTerm (pevalNotTerm (ssymTerm "a")) (ssymTerm "a") @?= conTerm False
            pevalEqTerm (ssymTerm "a") (pevalNotTerm (ssymTerm "a")) @?= conTerm False,
          testCase "Eqv(n1+x, n2)" $ do
            pevalEqTerm (pevalAddNumTerm (conTerm 1 :: Term Integer) (ssymTerm "a")) (conTerm 3)
              @?= pevalEqTerm (ssymTerm "a") (conTerm 2 :: Term Integer)
            pevalEqTerm (pevalAddNumTerm (conTerm 1 :: Term (IntN 4)) (ssymTerm "a")) (conTerm 3)
              @?= pevalEqTerm (ssymTerm "a") (conTerm 2 :: Term (IntN 4))
            pevalEqTerm (pevalAddNumTerm (conTerm 1 :: Term (WordN 4)) (ssymTerm "a")) (conTerm 3)
              @?= pevalEqTerm (ssymTerm "a") (conTerm 2 :: Term (WordN 4)),
          testCase "Eqv(n1, n2+x)" $ do
            pevalEqTerm (conTerm 3) (pevalAddNumTerm (conTerm 1 :: Term Integer) (ssymTerm "a"))
              @?= pevalEqTerm (ssymTerm "a") (conTerm 2 :: Term Integer)
            pevalEqTerm (conTerm 3) (pevalAddNumTerm (conTerm 1 :: Term (IntN 4)) (ssymTerm "a"))
              @?= pevalEqTerm (ssymTerm "a") (conTerm 2 :: Term (IntN 4))
            pevalEqTerm (conTerm 3) (pevalAddNumTerm (conTerm 1 :: Term (WordN 4)) (ssymTerm "a"))
              @?= pevalEqTerm (ssymTerm "a") (conTerm 2 :: Term (WordN 4)),
          testCase "Eqv(l, ITE(c, l, f)) / Eqv(l, ITE(c, t, l) / Eqv(ITE(c, r, f), r) / Eqv(ITE(c, t, r), r)" $ do
            pevalEqTerm (ssymTerm "a" :: Term Integer) (pevalITETerm (ssymTerm "b") (ssymTerm "a") (ssymTerm "c"))
              @?= pevalOrTerm (ssymTerm "b") (pevalEqTerm (ssymTerm "a") (ssymTerm "c" :: Term Integer))
            pevalEqTerm (ssymTerm "a" :: Term Integer) (pevalITETerm (ssymTerm "b") (ssymTerm "c") (ssymTerm "a"))
              @?= pevalOrTerm (pevalNotTerm $ ssymTerm "b") (pevalEqTerm (ssymTerm "a") (ssymTerm "c" :: Term Integer))
            pevalEqTerm (pevalITETerm (ssymTerm "b") (ssymTerm "a") (ssymTerm "c")) (ssymTerm "a" :: Term Integer)
              @?= pevalOrTerm (ssymTerm "b") (pevalEqTerm (ssymTerm "c") (ssymTerm "a" :: Term Integer))
            pevalEqTerm (pevalITETerm (ssymTerm "b") (ssymTerm "c") (ssymTerm "a")) (ssymTerm "a" :: Term Integer)
              @?= pevalOrTerm (pevalNotTerm $ ssymTerm "b") (pevalEqTerm (ssymTerm "c") (ssymTerm "a" :: Term Integer))
        ],
      testGroup
        "Or"
        [ testCase "On both concrete" $ do
            pevalOrTerm (conTerm True) (conTerm True) @?= conTerm True
            pevalOrTerm (conTerm True) (conTerm False) @?= conTerm True
            pevalOrTerm (conTerm False) (conTerm True) @?= conTerm True
            pevalOrTerm (conTerm False) (conTerm False) @?= conTerm False,
          testCase "On general symbolic" $ do
            pevalOrTerm (ssymTerm "a") (ssymTerm "b")
              @?= orTerm (ssymTerm "a" :: Term Bool) (ssymTerm "b" :: Term Bool),
          testCase "Or(x, y) -> True" $ do
            pevalOrTerm (conTerm True) (ssymTerm "b") @?= conTerm True
            pevalOrTerm (ssymTerm "a") (conTerm True) @?= conTerm True
            pevalOrTerm
              (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2))
              @?= conTerm True
            pevalOrTerm (pevalNotTerm (ssymTerm "a")) (ssymTerm "a") @?= conTerm True
            pevalOrTerm (ssymTerm "a") (pevalNotTerm (ssymTerm "a")) @?= conTerm True,
          testCase "Or(x, y) -> x" $ do
            pevalOrTerm (ssymTerm "a") (conTerm False) @?= ssymTerm "a"
            pevalOrTerm
              (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2))
              @?= pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)
            pevalOrTerm (ssymTerm "a") (ssymTerm "a") @?= ssymTerm "a",
          testCase "Or(x, y) -> y" $ do
            pevalOrTerm (conTerm False) (ssymTerm "a") @?= ssymTerm "a"
            pevalOrTerm
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2))
              (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              @?= pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1),
          testCase "Or(x, Or(y1, y2)) -> True" $ do
            pevalOrTerm (pevalNotTerm (ssymTerm "a")) (pevalOrTerm (ssymTerm "a") (ssymTerm "b")) @?= conTerm True
            pevalOrTerm (ssymTerm "a") (pevalOrTerm (pevalNotTerm (ssymTerm "a")) (ssymTerm "b")) @?= conTerm True
            pevalOrTerm
              (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              (pevalOrTerm (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)) (ssymTerm "b"))
              @?= conTerm True

            pevalOrTerm (pevalNotTerm (ssymTerm "a")) (pevalOrTerm (ssymTerm "b") (ssymTerm "a")) @?= conTerm True
            pevalOrTerm (ssymTerm "a") (pevalOrTerm (ssymTerm "b") (pevalNotTerm (ssymTerm "a"))) @?= conTerm True
            pevalOrTerm
              (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              (pevalOrTerm (ssymTerm "b") (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)))
              @?= conTerm True,
          testCase "Or(x, Or(y1, y2)) -> Or(x, y2)" $ do
            pevalOrTerm
              (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              (pevalOrTerm (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)) (ssymTerm "b"))
              @?= pevalOrTerm (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)) (ssymTerm "b"),
          testCase "Or(x, Or(y1, y2)) -> Or(x, y1)" $ do
            pevalOrTerm
              (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              (pevalOrTerm (ssymTerm "b") (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)))
              @?= pevalOrTerm (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)) (ssymTerm "b"),
          testCase "Or(x, y@Or(y1, y2)) -> y" $ do
            pevalOrTerm (ssymTerm "a") (pevalOrTerm (ssymTerm "a") (ssymTerm "b"))
              @?= pevalOrTerm (ssymTerm "a") (ssymTerm "b")
            pevalOrTerm
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2))
              (pevalOrTerm (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)) (ssymTerm "b"))
              @?= pevalOrTerm (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)) (ssymTerm "b")
            pevalOrTerm (ssymTerm "a") (pevalOrTerm (ssymTerm "b") (ssymTerm "a"))
              @?= pevalOrTerm (ssymTerm "b") (ssymTerm "a")
            pevalOrTerm
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2))
              (pevalOrTerm (ssymTerm "b") (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)))
              @?= pevalOrTerm (ssymTerm "b") (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)),
          testCase "Or(Or(x1, x2), y) -> True" $ do
            pevalOrTerm (pevalOrTerm (ssymTerm "a") (ssymTerm "b")) (pevalNotTerm (ssymTerm "a")) @?= conTerm True
            pevalOrTerm (pevalOrTerm (pevalNotTerm (ssymTerm "a")) (ssymTerm "b")) (ssymTerm "a") @?= conTerm True
            pevalOrTerm
              (pevalOrTerm (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)) (ssymTerm "b"))
              (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              @?= conTerm True

            pevalOrTerm (pevalOrTerm (ssymTerm "b") (ssymTerm "a")) (pevalNotTerm (ssymTerm "a")) @?= conTerm True
            pevalOrTerm (pevalOrTerm (ssymTerm "b") (pevalNotTerm (ssymTerm "a"))) (ssymTerm "a") @?= conTerm True
            pevalOrTerm
              (pevalOrTerm (ssymTerm "b") (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)))
              (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              @?= conTerm True,
          testCase "Or(x@Or(x1, x2), y) -> x" $ do
            pevalOrTerm (pevalOrTerm (ssymTerm "a") (ssymTerm "b")) (ssymTerm "a")
              @?= pevalOrTerm (ssymTerm "a") (ssymTerm "b")
            pevalOrTerm
              (pevalOrTerm (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)) (ssymTerm "b"))
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2))
              @?= pevalOrTerm (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)) (ssymTerm "b")
            pevalOrTerm (pevalOrTerm (ssymTerm "b") (ssymTerm "a")) (ssymTerm "a")
              @?= pevalOrTerm (ssymTerm "b") (ssymTerm "a")
            pevalOrTerm
              (pevalOrTerm (ssymTerm "b") (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)))
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2))
              @?= pevalOrTerm (ssymTerm "b") (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)),
          testCase "Or(Or(x1, x2), y) -> Or(x2, y)" $ do
            pevalOrTerm
              (pevalOrTerm (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)) (ssymTerm "b"))
              (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              @?= pevalOrTerm (ssymTerm "b") (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)),
          testCase "Or(Or(x1, x2), y) -> Or(x1, y)" $ do
            pevalOrTerm
              (pevalOrTerm (ssymTerm "b") (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)))
              (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              @?= pevalOrTerm (ssymTerm "b") (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)),
          testCase "Or(x, And(y1, y2)) -> x" $ do
            pevalOrTerm
              (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              (pevalAndTerm (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)) (ssymTerm "b"))
              @?= pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)
            pevalOrTerm
              (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              (pevalAndTerm (ssymTerm "b") (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)))
              @?= pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1),
          testCase "Or(x, And(y1, y2)) -> Or(x, y2)" $ do
            pevalOrTerm (ssymTerm "a") (pevalAndTerm (pevalNotTerm (ssymTerm "a")) (ssymTerm "b"))
              @?= pevalOrTerm (ssymTerm "a") (ssymTerm "b")
            pevalOrTerm (pevalNotTerm (ssymTerm "a")) (pevalAndTerm (ssymTerm "a") (ssymTerm "b"))
              @?= pevalOrTerm (pevalNotTerm (ssymTerm "a")) (ssymTerm "b")
            pevalOrTerm
              (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              (pevalAndTerm (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)) (ssymTerm "b"))
              @?= pevalOrTerm (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)) (ssymTerm "b"),
          testCase "Or(And(x1, x2), y) -> y" $ do
            pevalOrTerm
              (pevalAndTerm (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)) (ssymTerm "b"))
              (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              @?= pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)
            pevalOrTerm
              (pevalAndTerm (ssymTerm "b") (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)))
              (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              @?= pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1),
          testCase "Or(x, And(y1, y2)) -> Or(x, y1)" $ do
            pevalOrTerm (ssymTerm "a") (pevalAndTerm (ssymTerm "b") (pevalNotTerm (ssymTerm "a")))
              @?= pevalOrTerm (ssymTerm "a") (ssymTerm "b")
            pevalOrTerm (pevalNotTerm (ssymTerm "a")) (pevalAndTerm (ssymTerm "b") (ssymTerm "a"))
              @?= pevalOrTerm (pevalNotTerm (ssymTerm "a")) (ssymTerm "b")
            pevalOrTerm
              (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              (pevalAndTerm (ssymTerm "b") (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)))
              @?= pevalOrTerm (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)) (ssymTerm "b"),
          testCase "Or(Not(x), Not(y)) -> Not(And(x, y))" $ do
            pevalOrTerm (pevalNotTerm (ssymTerm "a")) (pevalNotTerm (ssymTerm "b"))
              @?= pevalNotTerm (pevalAndTerm (ssymTerm "a") (ssymTerm "b"))
        ],
      testGroup
        "And"
        [ testCase "Oith both concrete" $ do
            pevalAndTerm (conTerm True) (conTerm True) @?= conTerm True
            pevalAndTerm (conTerm True) (conTerm False) @?= conTerm False
            pevalAndTerm (conTerm False) (conTerm True) @?= conTerm False
            pevalAndTerm (conTerm False) (conTerm False) @?= conTerm False,
          testCase "On general symbolic" $ do
            pevalAndTerm (ssymTerm "a") (ssymTerm "b")
              @?= andTerm (ssymTerm "a" :: Term Bool) (ssymTerm "b" :: Term Bool),
          testCase "And(x, y) -> False" $ do
            pevalAndTerm (conTerm False) (ssymTerm "b") @?= conTerm False
            pevalAndTerm (ssymTerm "a") (conTerm False) @?= conTerm False
            pevalAndTerm
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2))
              @?= conTerm False
            pevalAndTerm (pevalNotTerm (ssymTerm "a")) (ssymTerm "a") @?= conTerm False
            pevalAndTerm (ssymTerm "a") (pevalNotTerm (ssymTerm "a")) @?= conTerm False,
          testCase "And(x, y) -> x" $ do
            pevalAndTerm (ssymTerm "a") (conTerm True) @?= ssymTerm "a"
            pevalAndTerm
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2))
              @?= pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)
            pevalAndTerm (ssymTerm "a") (ssymTerm "a") @?= ssymTerm "a",
          testCase "And(x, y) -> y" $ do
            pevalAndTerm (conTerm True) (ssymTerm "a") @?= ssymTerm "a"
            pevalAndTerm
              (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2))
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              @?= pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1),
          testCase "And(x, And(y1, y2)) -> False" $ do
            pevalAndTerm (pevalNotTerm (ssymTerm "a")) (pevalAndTerm (ssymTerm "a") (ssymTerm "b")) @?= conTerm False
            pevalAndTerm (ssymTerm "a") (pevalAndTerm (pevalNotTerm (ssymTerm "a")) (ssymTerm "b")) @?= conTerm False
            pevalAndTerm
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              (pevalAndTerm (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)) (ssymTerm "b"))
              @?= conTerm False

            pevalAndTerm (pevalNotTerm (ssymTerm "a")) (pevalAndTerm (ssymTerm "b") (ssymTerm "a")) @?= conTerm False
            pevalAndTerm (ssymTerm "a") (pevalAndTerm (ssymTerm "b") (pevalNotTerm (ssymTerm "a"))) @?= conTerm False
            pevalAndTerm
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              (pevalAndTerm (ssymTerm "b") (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)))
              @?= conTerm False,
          testCase "And(x, And(y1, y2)) -> And(x, y2)" $ do
            pevalAndTerm
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              (pevalAndTerm (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)) (ssymTerm "b"))
              @?= pevalAndTerm (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)) (ssymTerm "b"),
          testCase "And(x, And(y1, y2)) -> And(x, y1)" $ do
            pevalAndTerm
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              (pevalAndTerm (ssymTerm "b") (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)))
              @?= pevalAndTerm (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)) (ssymTerm "b"),
          testCase "And(x, y@And(y1, y2)) -> y" $ do
            pevalAndTerm (ssymTerm "a") (pevalAndTerm (ssymTerm "a") (ssymTerm "b"))
              @?= pevalAndTerm (ssymTerm "a") (ssymTerm "b")
            pevalAndTerm
              (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2))
              (pevalAndTerm (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)) (ssymTerm "b"))
              @?= pevalAndTerm (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)) (ssymTerm "b")
            pevalAndTerm (ssymTerm "a") (pevalAndTerm (ssymTerm "b") (ssymTerm "a"))
              @?= pevalAndTerm (ssymTerm "b") (ssymTerm "a")
            pevalAndTerm
              (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2))
              (pevalAndTerm (ssymTerm "b") (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)))
              @?= pevalAndTerm (ssymTerm "b") (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)),
          testCase "And(And(x1, x2), y) -> False" $ do
            pevalAndTerm (pevalAndTerm (ssymTerm "a") (ssymTerm "b")) (pevalNotTerm (ssymTerm "a")) @?= conTerm False
            pevalAndTerm (pevalAndTerm (pevalNotTerm (ssymTerm "a")) (ssymTerm "b")) (ssymTerm "a") @?= conTerm False
            pevalAndTerm
              (pevalAndTerm (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)) (ssymTerm "b"))
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              @?= conTerm False

            pevalAndTerm (pevalAndTerm (ssymTerm "b") (ssymTerm "a")) (pevalNotTerm (ssymTerm "a")) @?= conTerm False
            pevalAndTerm (pevalAndTerm (ssymTerm "b") (pevalNotTerm (ssymTerm "a"))) (ssymTerm "a") @?= conTerm False
            pevalAndTerm
              (pevalAndTerm (ssymTerm "b") (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)))
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              @?= conTerm False,
          testCase "And(x@And(x1, x2), y) -> x" $ do
            pevalAndTerm (pevalAndTerm (ssymTerm "a") (ssymTerm "b")) (ssymTerm "a")
              @?= pevalAndTerm (ssymTerm "a") (ssymTerm "b")
            pevalAndTerm
              (pevalAndTerm (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)) (ssymTerm "b"))
              (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2))
              @?= pevalAndTerm (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)) (ssymTerm "b")
            pevalAndTerm (pevalAndTerm (ssymTerm "b") (ssymTerm "a")) (ssymTerm "a")
              @?= pevalAndTerm (ssymTerm "b") (ssymTerm "a")
            pevalAndTerm
              (pevalAndTerm (ssymTerm "b") (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)))
              (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2))
              @?= pevalAndTerm (ssymTerm "b") (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)),
          testCase "And(And(x1, x2), y) -> And(x2, y)" $ do
            pevalAndTerm
              (pevalAndTerm (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)) (ssymTerm "b"))
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              @?= pevalAndTerm (ssymTerm "b") (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)),
          testCase "And(And(x1, x2), y) -> And(x1, y)" $ do
            pevalAndTerm
              (pevalAndTerm (ssymTerm "b") (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)))
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              @?= pevalAndTerm (ssymTerm "b") (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)),
          testCase "And(x, Or(y1, y2)) -> x" $ do
            pevalAndTerm
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              (pevalOrTerm (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)) (ssymTerm "b"))
              @?= pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)
            pevalAndTerm
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              (pevalOrTerm (ssymTerm "b") (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)))
              @?= pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1),
          testCase "And(x, Or(y1, y2)) -> And(x, y2)" $ do
            pevalAndTerm (ssymTerm "a") (pevalOrTerm (pevalNotTerm (ssymTerm "a")) (ssymTerm "b"))
              @?= pevalAndTerm (ssymTerm "a") (ssymTerm "b")
            pevalAndTerm (pevalNotTerm (ssymTerm "a")) (pevalOrTerm (ssymTerm "a") (ssymTerm "b"))
              @?= pevalAndTerm (pevalNotTerm (ssymTerm "a")) (ssymTerm "b")
            pevalAndTerm
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              (pevalOrTerm (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)) (ssymTerm "b"))
              @?= pevalAndTerm (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)) (ssymTerm "b"),
          testCase "And(Or(x1, x2), y) -> y" $ do
            pevalAndTerm
              (pevalOrTerm (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)) (ssymTerm "b"))
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              @?= pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)
            pevalAndTerm
              (pevalOrTerm (ssymTerm "b") (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)))
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              @?= pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1),
          testCase "And(x, Or(y1, y2)) -> And(x, y1)" $ do
            pevalAndTerm (ssymTerm "a") (pevalOrTerm (ssymTerm "b") (pevalNotTerm (ssymTerm "a")))
              @?= pevalAndTerm (ssymTerm "a") (ssymTerm "b")
            pevalAndTerm (pevalNotTerm (ssymTerm "a")) (pevalOrTerm (ssymTerm "b") (ssymTerm "a"))
              @?= pevalAndTerm (pevalNotTerm (ssymTerm "a")) (ssymTerm "b")
            pevalAndTerm
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              (pevalOrTerm (ssymTerm "b") (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)))
              @?= pevalAndTerm (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)) (ssymTerm "b"),
          testCase "And(Not(x), Not(y)) -> Not(Or(x, y))" $ do
            pevalAndTerm (pevalNotTerm (ssymTerm "a")) (pevalNotTerm (ssymTerm "b"))
              @?= pevalNotTerm (pevalOrTerm (ssymTerm "a") (ssymTerm "b"))
        ],
      testGroup
        "ITE"
        [ testCase "On concrete condition" $ do
            pevalITETerm (conTerm True) (ssymTerm "a" :: Term Integer) (ssymTerm "b")
              @?= ssymTerm "a"
            pevalITETerm (conTerm False) (ssymTerm "a" :: Term Integer) (ssymTerm "b")
              @?= ssymTerm "b",
          testCase "On same branches" $ do
            pevalITETerm (ssymTerm "c") (ssymTerm "a" :: Term Integer) (ssymTerm "a")
              @?= ssymTerm "a",
          testCase "On both not" $ do
            pevalITETerm (ssymTerm "c") (pevalNotTerm $ ssymTerm "a") (pevalNotTerm $ ssymTerm "b")
              @?= pevalNotTerm (pevalITETerm (ssymTerm "c") (ssymTerm "a") (ssymTerm "b")),
          testCase "On not in condition" $ do
            pevalITETerm (pevalNotTerm $ ssymTerm "c") (ssymTerm "a" :: Term Integer) (ssymTerm "b")
              @?= pevalITETerm (ssymTerm "c") (ssymTerm "b") (ssymTerm "a"),
          testCase "On all arguments as ITE with same conditions" $ do
            pevalITETerm
              (pevalITETerm (ssymTerm "a") (ssymTerm "b") (ssymTerm "c"))
              (pevalITETerm (ssymTerm "a") (ssymTerm "d" :: Term Integer) (ssymTerm "e"))
              (pevalITETerm (ssymTerm "a") (ssymTerm "f" :: Term Integer) (ssymTerm "g"))
              @?= pevalITETerm
                (ssymTerm "a")
                (pevalITETerm (ssymTerm "b") (ssymTerm "d") (ssymTerm "f"))
                (pevalITETerm (ssymTerm "c") (ssymTerm "e") (ssymTerm "g")),
          testCase "On with true branch as ITE" $ do
            pevalITETerm
              (ssymTerm "a")
              (pevalITETerm (ssymTerm "a") (ssymTerm "b" :: Term Integer) (ssymTerm "c"))
              (ssymTerm "d")
              @?= pevalITETerm (ssymTerm "a") (ssymTerm "b") (ssymTerm "d")
            pevalITETerm
              (ssymTerm "a")
              (pevalITETerm (ssymTerm "b") (ssymTerm "c" :: Term Integer) (ssymTerm "d"))
              (ssymTerm "c")
              @?= pevalITETerm
                (pevalOrTerm (pevalNotTerm $ ssymTerm "a") (ssymTerm "b"))
                (ssymTerm "c")
                (ssymTerm "d")
            pevalITETerm
              (ssymTerm "a")
              (pevalITETerm (ssymTerm "b") (ssymTerm "c" :: Term Integer) (ssymTerm "d"))
              (ssymTerm "d")
              @?= pevalITETerm
                (pevalAndTerm (ssymTerm "a") (ssymTerm "b"))
                (ssymTerm "c")
                (ssymTerm "d"),
          testCase "On false branch as ITE" $ do
            pevalITETerm
              (ssymTerm "a")
              (ssymTerm "b")
              (pevalITETerm (ssymTerm "a") (ssymTerm "c" :: Term Integer) (ssymTerm "d"))
              @?= pevalITETerm (ssymTerm "a") (ssymTerm "b") (ssymTerm "d")
            pevalITETerm
              (ssymTerm "a")
              (ssymTerm "b")
              (pevalITETerm (ssymTerm "c") (ssymTerm "b" :: Term Integer) (ssymTerm "d"))
              @?= pevalITETerm
                (pevalOrTerm (ssymTerm "a") (ssymTerm "c"))
                (ssymTerm "b")
                (ssymTerm "d")
            pevalITETerm
              (ssymTerm "a")
              (ssymTerm "b")
              (pevalITETerm (ssymTerm "c") (ssymTerm "d" :: Term Integer) (ssymTerm "b"))
              @?= pevalITETerm
                (pevalOrTerm (ssymTerm "a") (pevalNotTerm $ ssymTerm "c"))
                (ssymTerm "b")
                (ssymTerm "d"),
          testCase "On both And" $ do
            pevalITETerm
              (ssymTerm "a")
              (pevalAndTerm (ssymTerm "b") (ssymTerm "c"))
              (pevalAndTerm (ssymTerm "b") (ssymTerm "d"))
              @?= pevalAndTerm (ssymTerm "b") (pevalITETerm (ssymTerm "a") (ssymTerm "c") (ssymTerm "d"))
            pevalITETerm
              (ssymTerm "a")
              (pevalAndTerm (ssymTerm "c") (ssymTerm "b"))
              (pevalAndTerm (ssymTerm "b") (ssymTerm "d"))
              @?= pevalAndTerm (ssymTerm "b") (pevalITETerm (ssymTerm "a") (ssymTerm "c") (ssymTerm "d"))
            pevalITETerm
              (ssymTerm "a")
              (pevalAndTerm (ssymTerm "b") (ssymTerm "c"))
              (pevalAndTerm (ssymTerm "d") (ssymTerm "b"))
              @?= pevalAndTerm (ssymTerm "b") (pevalITETerm (ssymTerm "a") (ssymTerm "c") (ssymTerm "d"))
            pevalITETerm
              (ssymTerm "a")
              (pevalAndTerm (ssymTerm "c") (ssymTerm "b"))
              (pevalAndTerm (ssymTerm "d") (ssymTerm "b"))
              @?= pevalAndTerm (ssymTerm "b") (pevalITETerm (ssymTerm "a") (ssymTerm "c") (ssymTerm "d")),
          testCase "On left And" $ do
            pevalITETerm
              (ssymTerm "a")
              (pevalAndTerm (ssymTerm "b") (ssymTerm "c"))
              (ssymTerm "b")
              @?= pevalAndTerm (ssymTerm "b") (pevalOrTerm (pevalNotTerm (ssymTerm "a")) (ssymTerm "c"))
            pevalITETerm
              (ssymTerm "a")
              (pevalAndTerm (ssymTerm "b") (ssymTerm "c"))
              (ssymTerm "c")
              @?= pevalAndTerm (ssymTerm "c") (pevalOrTerm (pevalNotTerm (ssymTerm "a")) (ssymTerm "b"))
            pevalITETerm
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              (pevalAndTerm (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)) (ssymTerm "b"))
              (ssymTerm "c")
              @?= pevalAndTerm (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)) (ssymTerm "c")
            pevalITETerm
              (ssymTerm "a")
              (pevalAndTerm (pevalNotTerm $ ssymTerm "a") (ssymTerm "b"))
              (ssymTerm "c")
              @?= pevalAndTerm (pevalNotTerm $ ssymTerm "a") (ssymTerm "c")
            pevalITETerm
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              (pevalAndTerm (ssymTerm "b") (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)))
              (ssymTerm "c")
              @?= pevalAndTerm (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)) (ssymTerm "c")
            pevalITETerm
              (ssymTerm "a")
              (pevalAndTerm (ssymTerm "b") (pevalNotTerm $ ssymTerm "a"))
              (ssymTerm "c")
              @?= pevalAndTerm (pevalNotTerm $ ssymTerm "a") (ssymTerm "c")
            pevalITETerm
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              (pevalAndTerm (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)) (ssymTerm "b"))
              (ssymTerm "c")
              @?= pevalITETerm (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)) (ssymTerm "b") (ssymTerm "c")
            pevalITETerm
              (ssymTerm "a")
              (pevalAndTerm (ssymTerm "a") (ssymTerm "b"))
              (ssymTerm "c")
              @?= pevalITETerm (ssymTerm "a") (ssymTerm "b") (ssymTerm "c")
            pevalITETerm
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              (pevalAndTerm (ssymTerm "b") (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)))
              (ssymTerm "c")
              @?= pevalITETerm (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)) (ssymTerm "b") (ssymTerm "c")
            pevalITETerm
              (ssymTerm "a")
              (pevalAndTerm (ssymTerm "b") (ssymTerm "a"))
              (ssymTerm "c")
              @?= pevalITETerm (ssymTerm "a") (ssymTerm "b") (ssymTerm "c"),
          testCase "On right And" $ do
            pevalITETerm
              (ssymTerm "a")
              (ssymTerm "b")
              (pevalAndTerm (ssymTerm "b") (ssymTerm "c"))
              @?= pevalAndTerm (ssymTerm "b") (pevalOrTerm (ssymTerm "a") (ssymTerm "c"))
            pevalITETerm
              (ssymTerm "a")
              (ssymTerm "c")
              (pevalAndTerm (ssymTerm "b") (ssymTerm "c"))
              @?= pevalAndTerm (ssymTerm "c") (pevalOrTerm (ssymTerm "a") (ssymTerm "b")),
          testCase "On both Or" $ do
            pevalITETerm
              (ssymTerm "a")
              (pevalOrTerm (ssymTerm "b") (ssymTerm "c"))
              (pevalOrTerm (ssymTerm "b") (ssymTerm "d"))
              @?= pevalOrTerm (ssymTerm "b") (pevalITETerm (ssymTerm "a") (ssymTerm "c") (ssymTerm "d"))
            pevalITETerm
              (ssymTerm "a")
              (pevalOrTerm (ssymTerm "c") (ssymTerm "b"))
              (pevalOrTerm (ssymTerm "b") (ssymTerm "d"))
              @?= pevalOrTerm (ssymTerm "b") (pevalITETerm (ssymTerm "a") (ssymTerm "c") (ssymTerm "d"))
            pevalITETerm
              (ssymTerm "a")
              (pevalOrTerm (ssymTerm "b") (ssymTerm "c"))
              (pevalOrTerm (ssymTerm "d") (ssymTerm "b"))
              @?= pevalOrTerm (ssymTerm "b") (pevalITETerm (ssymTerm "a") (ssymTerm "c") (ssymTerm "d"))
            pevalITETerm
              (ssymTerm "a")
              (pevalOrTerm (ssymTerm "c") (ssymTerm "b"))
              (pevalOrTerm (ssymTerm "d") (ssymTerm "b"))
              @?= pevalOrTerm (ssymTerm "b") (pevalITETerm (ssymTerm "a") (ssymTerm "c") (ssymTerm "d")),
          testCase "On left Or" $ do
            pevalITETerm
              (ssymTerm "a")
              (pevalOrTerm (ssymTerm "b") (ssymTerm "c"))
              (ssymTerm "b")
              @?= pevalOrTerm (ssymTerm "b") (pevalAndTerm (ssymTerm "a") (ssymTerm "c"))
            pevalITETerm
              (ssymTerm "a")
              (pevalOrTerm (ssymTerm "b") (ssymTerm "c"))
              (ssymTerm "c")
              @?= pevalOrTerm (ssymTerm "c") (pevalAndTerm (ssymTerm "a") (ssymTerm "b"))
            pevalITETerm
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              (pevalOrTerm (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)) (ssymTerm "b"))
              (ssymTerm "c")
              @?= pevalITETerm (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)) (ssymTerm "b") (ssymTerm "c")
            pevalITETerm
              (ssymTerm "a")
              (pevalOrTerm (pevalNotTerm $ ssymTerm "a") (ssymTerm "b"))
              (ssymTerm "c")
              @?= pevalITETerm (ssymTerm "a") (ssymTerm "b") (ssymTerm "c")
            pevalITETerm
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              (pevalOrTerm (ssymTerm "b") (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)))
              (ssymTerm "c")
              @?= pevalITETerm (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)) (ssymTerm "b") (ssymTerm "c")
            pevalITETerm
              (ssymTerm "a")
              (pevalOrTerm (ssymTerm "b") (pevalNotTerm $ ssymTerm "a"))
              (ssymTerm "c")
              @?= pevalITETerm (ssymTerm "a") (ssymTerm "b") (ssymTerm "c")
            pevalITETerm
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              (pevalOrTerm (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)) (ssymTerm "b"))
              (ssymTerm "c")
              @?= pevalOrTerm (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)) (ssymTerm "c")
            pevalITETerm
              (ssymTerm "a")
              (pevalOrTerm (ssymTerm "a") (ssymTerm "b"))
              (ssymTerm "c")
              @?= pevalOrTerm (ssymTerm "a") (ssymTerm "c")
            pevalITETerm
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              (pevalOrTerm (ssymTerm "b") (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)))
              (ssymTerm "c")
              @?= pevalOrTerm (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)) (ssymTerm "c")
            pevalITETerm
              (ssymTerm "a")
              (pevalOrTerm (ssymTerm "b") (ssymTerm "a"))
              (ssymTerm "c")
              @?= pevalOrTerm (ssymTerm "a") (ssymTerm "c"),
          testCase "On right Or" $ do
            pevalITETerm
              (ssymTerm "a")
              (ssymTerm "b")
              (pevalOrTerm (ssymTerm "b") (ssymTerm "c"))
              @?= pevalOrTerm (ssymTerm "b") (pevalAndTerm (pevalNotTerm (ssymTerm "a")) (ssymTerm "c"))
            pevalITETerm
              (ssymTerm "a")
              (ssymTerm "c")
              (pevalOrTerm (ssymTerm "b") (ssymTerm "c"))
              @?= pevalOrTerm (ssymTerm "c") (pevalAndTerm (pevalNotTerm (ssymTerm "a")) (ssymTerm "b")),
          testCase "On const boolean in branches" $ do
            pevalITETerm
              (ssymTerm "a")
              (conTerm True)
              (ssymTerm "b")
              @?= pevalOrTerm (ssymTerm "a") (ssymTerm "b")
            pevalITETerm
              (ssymTerm "a")
              (conTerm False)
              (ssymTerm "b")
              @?= pevalAndTerm (pevalNotTerm $ ssymTerm "a") (ssymTerm "b")
            pevalITETerm
              (ssymTerm "a")
              (ssymTerm "b")
              (conTerm True)
              @?= pevalOrTerm (pevalNotTerm $ ssymTerm "a") (ssymTerm "b")
            pevalITETerm
              (ssymTerm "a")
              (ssymTerm "b")
              (conTerm False)
              @?= pevalAndTerm (ssymTerm "a") (ssymTerm "b"),
          testCase "On condition equal to some branch" $ do
            pevalITETerm
              (ssymTerm "a")
              (ssymTerm "a")
              (ssymTerm "b")
              @?= pevalOrTerm (ssymTerm "a") (ssymTerm "b")
            pevalITETerm
              (ssymTerm "a")
              (ssymTerm "b")
              (ssymTerm "a")
              @?= pevalAndTerm (ssymTerm "a") (ssymTerm "b"),
          testCase "On left Not" $ do
            pevalITETerm (ssymTerm "a") (pevalNotTerm (ssymTerm "a")) (ssymTerm "b")
              @?= pevalAndTerm (pevalNotTerm $ ssymTerm "a") (ssymTerm "b"),
          testCase "On right Not" $ do
            pevalITETerm (ssymTerm "a") (ssymTerm "b") (pevalNotTerm (ssymTerm "a"))
              @?= pevalOrTerm (pevalNotTerm $ ssymTerm "a") (ssymTerm "b"),
          testCase "On left Not And" $ do
            pevalITETerm
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              (pevalNotTerm (pevalAndTerm (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)) (ssymTerm "b")))
              (ssymTerm "c")
              @?= pevalOrTerm (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)) (ssymTerm "c")
            pevalITETerm
              (ssymTerm "a")
              (pevalNotTerm (pevalAndTerm (pevalNotTerm $ ssymTerm "a") (ssymTerm "b")))
              (ssymTerm "c")
              @?= pevalOrTerm (ssymTerm "a") (ssymTerm "c")
            pevalITETerm
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              (pevalNotTerm (pevalAndTerm (ssymTerm "b") (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2))))
              (ssymTerm "c")
              @?= pevalOrTerm (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)) (ssymTerm "c")
            pevalITETerm
              (ssymTerm "a")
              (pevalNotTerm (pevalAndTerm (ssymTerm "b") (pevalNotTerm $ ssymTerm "a")))
              (ssymTerm "c")
              @?= pevalOrTerm (ssymTerm "a") (ssymTerm "c")
            pevalITETerm
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              (pevalNotTerm (pevalAndTerm (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2)) (ssymTerm "b")))
              (ssymTerm "c")
              @?= pevalITETerm (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)) (pevalNotTerm $ ssymTerm "b") (ssymTerm "c")
            pevalITETerm
              (ssymTerm "a")
              (pevalNotTerm (pevalAndTerm (ssymTerm "a") (ssymTerm "b")))
              (ssymTerm "c")
              @?= pevalITETerm (ssymTerm "a") (pevalNotTerm $ ssymTerm "b") (ssymTerm "c")
            pevalITETerm
              (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1))
              (pevalNotTerm (pevalAndTerm (ssymTerm "b") (pevalNEqTerm (ssymTerm "a" :: Term Integer) (conTerm 2))))
              (ssymTerm "c")
              @?= pevalITETerm (pevalEqTerm (ssymTerm "a" :: Term Integer) (conTerm 1)) (pevalNotTerm $ ssymTerm "b") (ssymTerm "c")
            pevalITETerm
              (ssymTerm "a")
              (pevalNotTerm (pevalAndTerm (ssymTerm "b") (ssymTerm "a")))
              (ssymTerm "c")
              @?= pevalITETerm (ssymTerm "a") (pevalNotTerm $ ssymTerm "b") (ssymTerm "c")
        ],
      testGroup
        "Imply"
        [ testCase "pevalImplyTerm" $ do
            ssymTerm "a"
              `pevalImplyTerm` ssymTerm "b"
              @?= pevalOrTerm (pevalNotTerm $ ssymTerm "a") (ssymTerm "b")
        ],
      testGroup
        "Xor"
        [ testCase "pevalXorTerm" $ do
            ssymTerm "a"
              `pevalXorTerm` ssymTerm "b"
              @?= pevalOrTerm
                (pevalAndTerm (pevalNotTerm $ ssymTerm "a") (ssymTerm "b"))
                (pevalAndTerm (ssymTerm "a") (pevalNotTerm $ ssymTerm "b"))
        ]
    ]
