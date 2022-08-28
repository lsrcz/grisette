{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pizza.IR.SymPrim.Data.Prim.BoolTests where

import Pizza.IR.SymPrim.Data.BV
import Pizza.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Pizza.IR.SymPrim.Data.Prim.InternedTerm.Term
import Pizza.IR.SymPrim.Data.Prim.PartialEval.Bool
import Pizza.IR.SymPrim.Data.Prim.PartialEval.Num
import Test.Tasty
import Test.Tasty.HUnit

boolTests :: TestTree
boolTests =
  testGroup
    "BoolTests"
    [ testGroup
        "Not"
        [ testCase "On concrete" $ do
            pevalNotTerm (concTerm True) @=? concTerm False
            pevalNotTerm (concTerm True) @=? concTerm False,
          testCase "On general symbolic" $ do
            pevalNotTerm (ssymbTerm "a") @=? notTerm (ssymbTerm "a" :: Term Bool),
          testCase "On Not" $ do
            pevalNotTerm (pevalNotTerm (ssymbTerm "a")) @=? ssymbTerm "a",
          testCase "On Or Not" $ do
            pevalNotTerm (pevalOrTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "b"))
              @=? pevalAndTerm (ssymbTerm "a") (pevalNotTerm (ssymbTerm "b"))
            pevalNotTerm (pevalOrTerm (ssymbTerm "a") (pevalNotTerm (ssymbTerm "b")))
              @=? pevalAndTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "b"),
          testCase "On And Not" $ do
            pevalNotTerm (pevalAndTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "b"))
              @=? pevalOrTerm (ssymbTerm "a") (pevalNotTerm (ssymbTerm "b"))
            pevalNotTerm (pevalAndTerm (ssymbTerm "a") (pevalNotTerm (ssymbTerm "b")))
              @=? pevalOrTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "b")
        ],
      testGroup
        "Eqv & NEqv"
        [ testCase "Eqv on both concrete" $ do
            pevalEqvTerm (concTerm True) (concTerm True) @=? concTerm True
            pevalEqvTerm (concTerm True) (concTerm False) @=? concTerm False
            pevalEqvTerm (concTerm False) (concTerm True) @=? concTerm False
            pevalEqvTerm (concTerm False) (concTerm False) @=? concTerm True
            pevalEqvTerm (concTerm (1 :: Integer)) (concTerm 1) @=? concTerm True
            pevalEqvTerm (concTerm (1 :: Integer)) (concTerm 2) @=? concTerm False
            pevalEqvTerm (concTerm (1 :: IntN 4)) (concTerm 1) @=? concTerm True
            pevalEqvTerm (concTerm (1 :: IntN 4)) (concTerm 2) @=? concTerm False
            pevalEqvTerm (concTerm (1 :: WordN 4)) (concTerm 1) @=? concTerm True
            pevalEqvTerm (concTerm (1 :: WordN 4)) (concTerm 2) @=? concTerm False,
          testCase "Eqv on single concrete always put concrete ones in the right" $ do
            pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)
              @=? eqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1 :: Term Integer)
            pevalEqvTerm (concTerm 1) (ssymbTerm "a" :: Term Integer)
              @=? eqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1 :: Term Integer),
          testCase "Eqv on general symbolic" $ do
            pevalEqvTerm (ssymbTerm "a" :: Term Integer) (ssymbTerm "b")
              @=? eqvTerm (ssymbTerm "a" :: Term Integer) (ssymbTerm "b" :: Term Integer),
          testCase "Eqv on Bool with single concrete" $ do
            pevalEqvTerm (concTerm True) (ssymbTerm "a") @=? ssymbTerm "a"
            pevalEqvTerm (ssymbTerm "a") (concTerm True) @=? ssymbTerm "a"
            pevalEqvTerm (concTerm False) (ssymbTerm "a") @=? pevalNotTerm (ssymbTerm "a")
            pevalEqvTerm (ssymbTerm "a") (concTerm False) @=? pevalNotTerm (ssymbTerm "a"),
          testCase "NEqv on general symbolic" $ do
            pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (ssymbTerm "b")
              @=? pevalNotTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (ssymbTerm "b")),
          testCase "Eqv(Not(x), x) / Eqv(x, Not(x))" $ do
            pevalEqvTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "a") @=? concTerm False
            pevalEqvTerm (ssymbTerm "a") (pevalNotTerm (ssymbTerm "a")) @=? concTerm False,
          testCase "Eqv(n1+x, n2)" $ do
            pevalEqvTerm (pevalAddNumTerm (concTerm 1 :: Term Integer) (ssymbTerm "a")) (concTerm 3)
              @=? pevalEqvTerm (ssymbTerm "a") (concTerm 2 :: Term Integer)
            pevalEqvTerm (pevalAddNumTerm (concTerm 1 :: Term (IntN 4)) (ssymbTerm "a")) (concTerm 3)
              @=? pevalEqvTerm (ssymbTerm "a") (concTerm 2 :: Term (IntN 4))
            pevalEqvTerm (pevalAddNumTerm (concTerm 1 :: Term (WordN 4)) (ssymbTerm "a")) (concTerm 3)
              @=? pevalEqvTerm (ssymbTerm "a") (concTerm 2 :: Term (WordN 4)),
          testCase "Eqv(n1, n2+x)" $ do
            pevalEqvTerm (concTerm 3) (pevalAddNumTerm (concTerm 1 :: Term Integer) (ssymbTerm "a"))
              @=? pevalEqvTerm (ssymbTerm "a") (concTerm 2 :: Term Integer)
            pevalEqvTerm (concTerm 3) (pevalAddNumTerm (concTerm 1 :: Term (IntN 4)) (ssymbTerm "a"))
              @=? pevalEqvTerm (ssymbTerm "a") (concTerm 2 :: Term (IntN 4))
            pevalEqvTerm (concTerm 3) (pevalAddNumTerm (concTerm 1 :: Term (WordN 4)) (ssymbTerm "a"))
              @=? pevalEqvTerm (ssymbTerm "a") (concTerm 2 :: Term (WordN 4)),
          testCase "Eqv(l, ITE(c, l, f)) / Eqv(l, ITE(c, t, l) / Eqv(ITE(c, r, f), r) / Eqv(ITE(c, t, r), r)" $ do
            pevalEqvTerm (ssymbTerm "a" :: Term Integer) (pevalITETerm (ssymbTerm "b") (ssymbTerm "a") (ssymbTerm "c"))
              @=? pevalOrTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a") (ssymbTerm "c" :: Term Integer))
            pevalEqvTerm (ssymbTerm "a" :: Term Integer) (pevalITETerm (ssymbTerm "b") (ssymbTerm "c") (ssymbTerm "a"))
              @=? pevalOrTerm (pevalNotTerm $ ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a") (ssymbTerm "c" :: Term Integer))
            pevalEqvTerm (pevalITETerm (ssymbTerm "b") (ssymbTerm "a") (ssymbTerm "c")) (ssymbTerm "a" :: Term Integer)
              @=? pevalOrTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "c") (ssymbTerm "a" :: Term Integer))
            pevalEqvTerm (pevalITETerm (ssymbTerm "b") (ssymbTerm "c") (ssymbTerm "a")) (ssymbTerm "a" :: Term Integer)
              @=? pevalOrTerm (pevalNotTerm $ ssymbTerm "b") (pevalEqvTerm (ssymbTerm "c") (ssymbTerm "a" :: Term Integer))
        ],
      testGroup
        "Or"
        [ testCase "On both concrete" $ do
            pevalOrTerm (concTerm True) (concTerm True) @=? concTerm True
            pevalOrTerm (concTerm True) (concTerm False) @=? concTerm True
            pevalOrTerm (concTerm False) (concTerm True) @=? concTerm True
            pevalOrTerm (concTerm False) (concTerm False) @=? concTerm False,
          testCase "On general symbolic" $ do
            pevalOrTerm (ssymbTerm "a") (ssymbTerm "b")
              @=? orTerm (ssymbTerm "a" :: Term Bool) (ssymbTerm "b" :: Term Bool),
          testCase "Or(x, y) -> True" $ do
            pevalOrTerm (concTerm True) (ssymbTerm "b") @=? concTerm True
            pevalOrTerm (ssymbTerm "a") (concTerm True) @=? concTerm True
            pevalOrTerm
              (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2))
              @=? concTerm True
            pevalOrTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "a") @=? concTerm True
            pevalOrTerm (ssymbTerm "a") (pevalNotTerm (ssymbTerm "a")) @=? concTerm True,
          testCase "Or(x, y) -> x" $ do
            pevalOrTerm (ssymbTerm "a") (concTerm False) @=? ssymbTerm "a"
            pevalOrTerm
              (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2))
              @=? pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)
            pevalOrTerm (ssymbTerm "a") (ssymbTerm "a") @=? ssymbTerm "a",
          testCase "Or(x, y) -> y" $ do
            pevalOrTerm (concTerm False) (ssymbTerm "a") @=? ssymbTerm "a"
            pevalOrTerm
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2))
              (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              @=? pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1),
          testCase "Or(x, Or(y1, y2)) -> True" $ do
            pevalOrTerm (pevalNotTerm (ssymbTerm "a")) (pevalOrTerm (ssymbTerm "a") (ssymbTerm "b")) @=? concTerm True
            pevalOrTerm (ssymbTerm "a") (pevalOrTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "b")) @=? concTerm True
            pevalOrTerm
              (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              (pevalOrTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
              @=? concTerm True

            pevalOrTerm (pevalNotTerm (ssymbTerm "a")) (pevalOrTerm (ssymbTerm "b") (ssymbTerm "a")) @=? concTerm True
            pevalOrTerm (ssymbTerm "a") (pevalOrTerm (ssymbTerm "b") (pevalNotTerm (ssymbTerm "a"))) @=? concTerm True
            pevalOrTerm
              (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              (pevalOrTerm (ssymbTerm "b") (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
              @=? concTerm True,
          testCase "Or(x, Or(y1, y2)) -> Or(x, y2)" $ do
            pevalOrTerm
              (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              (pevalOrTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
              @=? pevalOrTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b"),
          testCase "Or(x, Or(y1, y2)) -> Or(x, y1)" $ do
            pevalOrTerm
              (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              (pevalOrTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
              @=? pevalOrTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b"),
          testCase "Or(x, y@Or(y1, y2)) -> y" $ do
            pevalOrTerm (ssymbTerm "a") (pevalOrTerm (ssymbTerm "a") (ssymbTerm "b"))
              @=? pevalOrTerm (ssymbTerm "a") (ssymbTerm "b")
            pevalOrTerm
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2))
              (pevalOrTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b"))
              @=? pevalOrTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b")
            pevalOrTerm (ssymbTerm "a") (pevalOrTerm (ssymbTerm "b") (ssymbTerm "a"))
              @=? pevalOrTerm (ssymbTerm "b") (ssymbTerm "a")
            pevalOrTerm
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2))
              (pevalOrTerm (ssymbTerm "b") (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)))
              @=? pevalOrTerm (ssymbTerm "b") (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)),
          testCase "Or(Or(x1, x2), y) -> True" $ do
            pevalOrTerm (pevalOrTerm (ssymbTerm "a") (ssymbTerm "b")) (pevalNotTerm (ssymbTerm "a")) @=? concTerm True
            pevalOrTerm (pevalOrTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "b")) (ssymbTerm "a") @=? concTerm True
            pevalOrTerm
              (pevalOrTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
              (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              @=? concTerm True

            pevalOrTerm (pevalOrTerm (ssymbTerm "b") (ssymbTerm "a")) (pevalNotTerm (ssymbTerm "a")) @=? concTerm True
            pevalOrTerm (pevalOrTerm (ssymbTerm "b") (pevalNotTerm (ssymbTerm "a"))) (ssymbTerm "a") @=? concTerm True
            pevalOrTerm
              (pevalOrTerm (ssymbTerm "b") (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
              (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              @=? concTerm True,
          testCase "Or(x@Or(x1, x2), y) -> x" $ do
            pevalOrTerm (pevalOrTerm (ssymbTerm "a") (ssymbTerm "b")) (ssymbTerm "a")
              @=? pevalOrTerm (ssymbTerm "a") (ssymbTerm "b")
            pevalOrTerm
              (pevalOrTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b"))
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2))
              @=? pevalOrTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b")
            pevalOrTerm (pevalOrTerm (ssymbTerm "b") (ssymbTerm "a")) (ssymbTerm "a")
              @=? pevalOrTerm (ssymbTerm "b") (ssymbTerm "a")
            pevalOrTerm
              (pevalOrTerm (ssymbTerm "b") (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)))
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2))
              @=? pevalOrTerm (ssymbTerm "b") (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)),
          testCase "Or(Or(x1, x2), y) -> Or(x2, y)" $ do
            pevalOrTerm
              (pevalOrTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
              (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              @=? pevalOrTerm (ssymbTerm "b") (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)),
          testCase "Or(Or(x1, x2), y) -> Or(x1, y)" $ do
            pevalOrTerm
              (pevalOrTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
              (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              @=? pevalOrTerm (ssymbTerm "b") (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)),
          testCase "Or(x, And(y1, y2)) -> x" $ do
            pevalOrTerm
              (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              (pevalAndTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
              @=? pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)
            pevalOrTerm
              (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              (pevalAndTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
              @=? pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1),
          testCase "Or(x, And(y1, y2)) -> Or(x, y2)" $ do
            pevalOrTerm (ssymbTerm "a") (pevalAndTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "b"))
              @=? pevalOrTerm (ssymbTerm "a") (ssymbTerm "b")
            pevalOrTerm (pevalNotTerm (ssymbTerm "a")) (pevalAndTerm (ssymbTerm "a") (ssymbTerm "b"))
              @=? pevalOrTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "b")
            pevalOrTerm
              (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              (pevalAndTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
              @=? pevalOrTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b"),
          testCase "Or(And(x1, x2), y) -> y" $ do
            pevalOrTerm
              (pevalAndTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
              (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              @=? pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)
            pevalOrTerm
              (pevalAndTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
              (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              @=? pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1),
          testCase "Or(x, And(y1, y2)) -> Or(x, y1)" $ do
            pevalOrTerm (ssymbTerm "a") (pevalAndTerm (ssymbTerm "b") (pevalNotTerm (ssymbTerm "a")))
              @=? pevalOrTerm (ssymbTerm "a") (ssymbTerm "b")
            pevalOrTerm (pevalNotTerm (ssymbTerm "a")) (pevalAndTerm (ssymbTerm "b") (ssymbTerm "a"))
              @=? pevalOrTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "b")
            pevalOrTerm
              (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              (pevalAndTerm (ssymbTerm "b") (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
              @=? pevalOrTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b"),
          testCase "Or(Not(x), Not(y)) -> Not(And(x, y))" $ do
            pevalOrTerm (pevalNotTerm (ssymbTerm "a")) (pevalNotTerm (ssymbTerm "b"))
              @=? pevalNotTerm (pevalAndTerm (ssymbTerm "a") (ssymbTerm "b"))
        ],
      testGroup
        "And"
        [ testCase "Oith both concrete" $ do
            pevalAndTerm (concTerm True) (concTerm True) @=? concTerm True
            pevalAndTerm (concTerm True) (concTerm False) @=? concTerm False
            pevalAndTerm (concTerm False) (concTerm True) @=? concTerm False
            pevalAndTerm (concTerm False) (concTerm False) @=? concTerm False,
          testCase "On general symbolic" $ do
            pevalAndTerm (ssymbTerm "a") (ssymbTerm "b")
              @=? andTerm (ssymbTerm "a" :: Term Bool) (ssymbTerm "b" :: Term Bool),
          testCase "And(x, y) -> False" $ do
            pevalAndTerm (concTerm False) (ssymbTerm "b") @=? concTerm False
            pevalAndTerm (ssymbTerm "a") (concTerm False) @=? concTerm False
            pevalAndTerm
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2))
              @=? concTerm False
            pevalAndTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "a") @=? concTerm False
            pevalAndTerm (ssymbTerm "a") (pevalNotTerm (ssymbTerm "a")) @=? concTerm False,
          testCase "And(x, y) -> x" $ do
            pevalAndTerm (ssymbTerm "a") (concTerm True) @=? ssymbTerm "a"
            pevalAndTerm
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2))
              @=? pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)
            pevalAndTerm (ssymbTerm "a") (ssymbTerm "a") @=? ssymbTerm "a",
          testCase "And(x, y) -> y" $ do
            pevalAndTerm (concTerm True) (ssymbTerm "a") @=? ssymbTerm "a"
            pevalAndTerm
              (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2))
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              @=? pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1),
          testCase "And(x, And(y1, y2)) -> False" $ do
            pevalAndTerm (pevalNotTerm (ssymbTerm "a")) (pevalAndTerm (ssymbTerm "a") (ssymbTerm "b")) @=? concTerm False
            pevalAndTerm (ssymbTerm "a") (pevalAndTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "b")) @=? concTerm False
            pevalAndTerm
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              (pevalAndTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
              @=? concTerm False

            pevalAndTerm (pevalNotTerm (ssymbTerm "a")) (pevalAndTerm (ssymbTerm "b") (ssymbTerm "a")) @=? concTerm False
            pevalAndTerm (ssymbTerm "a") (pevalAndTerm (ssymbTerm "b") (pevalNotTerm (ssymbTerm "a"))) @=? concTerm False
            pevalAndTerm
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              (pevalAndTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
              @=? concTerm False,
          testCase "And(x, And(y1, y2)) -> And(x, y2)" $ do
            pevalAndTerm
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              (pevalAndTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
              @=? pevalAndTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b"),
          testCase "And(x, And(y1, y2)) -> And(x, y1)" $ do
            pevalAndTerm
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              (pevalAndTerm (ssymbTerm "b") (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
              @=? pevalAndTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b"),
          testCase "And(x, y@And(y1, y2)) -> y" $ do
            pevalAndTerm (ssymbTerm "a") (pevalAndTerm (ssymbTerm "a") (ssymbTerm "b"))
              @=? pevalAndTerm (ssymbTerm "a") (ssymbTerm "b")
            pevalAndTerm
              (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2))
              (pevalAndTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b"))
              @=? pevalAndTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b")
            pevalAndTerm (ssymbTerm "a") (pevalAndTerm (ssymbTerm "b") (ssymbTerm "a"))
              @=? pevalAndTerm (ssymbTerm "b") (ssymbTerm "a")
            pevalAndTerm
              (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2))
              (pevalAndTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)))
              @=? pevalAndTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)),
          testCase "And(And(x1, x2), y) -> False" $ do
            pevalAndTerm (pevalAndTerm (ssymbTerm "a") (ssymbTerm "b")) (pevalNotTerm (ssymbTerm "a")) @=? concTerm False
            pevalAndTerm (pevalAndTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "b")) (ssymbTerm "a") @=? concTerm False
            pevalAndTerm
              (pevalAndTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              @=? concTerm False

            pevalAndTerm (pevalAndTerm (ssymbTerm "b") (ssymbTerm "a")) (pevalNotTerm (ssymbTerm "a")) @=? concTerm False
            pevalAndTerm (pevalAndTerm (ssymbTerm "b") (pevalNotTerm (ssymbTerm "a"))) (ssymbTerm "a") @=? concTerm False
            pevalAndTerm
              (pevalAndTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              @=? concTerm False,
          testCase "And(x@And(x1, x2), y) -> x" $ do
            pevalAndTerm (pevalAndTerm (ssymbTerm "a") (ssymbTerm "b")) (ssymbTerm "a")
              @=? pevalAndTerm (ssymbTerm "a") (ssymbTerm "b")
            pevalAndTerm
              (pevalAndTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b"))
              (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2))
              @=? pevalAndTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b")
            pevalAndTerm (pevalAndTerm (ssymbTerm "b") (ssymbTerm "a")) (ssymbTerm "a")
              @=? pevalAndTerm (ssymbTerm "b") (ssymbTerm "a")
            pevalAndTerm
              (pevalAndTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)))
              (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2))
              @=? pevalAndTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)),
          testCase "And(And(x1, x2), y) -> And(x2, y)" $ do
            pevalAndTerm
              (pevalAndTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              @=? pevalAndTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)),
          testCase "And(And(x1, x2), y) -> And(x1, y)" $ do
            pevalAndTerm
              (pevalAndTerm (ssymbTerm "b") (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              @=? pevalAndTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)),
          testCase "And(x, Or(y1, y2)) -> x" $ do
            pevalAndTerm
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              (pevalOrTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
              @=? pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)
            pevalAndTerm
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              (pevalOrTerm (ssymbTerm "b") (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
              @=? pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1),
          testCase "And(x, Or(y1, y2)) -> And(x, y2)" $ do
            pevalAndTerm (ssymbTerm "a") (pevalOrTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "b"))
              @=? pevalAndTerm (ssymbTerm "a") (ssymbTerm "b")
            pevalAndTerm (pevalNotTerm (ssymbTerm "a")) (pevalOrTerm (ssymbTerm "a") (ssymbTerm "b"))
              @=? pevalAndTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "b")
            pevalAndTerm
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              (pevalOrTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
              @=? pevalAndTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b"),
          testCase "And(Or(x1, x2), y) -> y" $ do
            pevalAndTerm
              (pevalOrTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              @=? pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)
            pevalAndTerm
              (pevalOrTerm (ssymbTerm "b") (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              @=? pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1),
          testCase "And(x, Or(y1, y2)) -> And(x, y1)" $ do
            pevalAndTerm (ssymbTerm "a") (pevalOrTerm (ssymbTerm "b") (pevalNotTerm (ssymbTerm "a")))
              @=? pevalAndTerm (ssymbTerm "a") (ssymbTerm "b")
            pevalAndTerm (pevalNotTerm (ssymbTerm "a")) (pevalOrTerm (ssymbTerm "b") (ssymbTerm "a"))
              @=? pevalAndTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "b")
            pevalAndTerm
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              (pevalOrTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
              @=? pevalAndTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b"),
          testCase "And(Not(x), Not(y)) -> Not(Or(x, y))" $ do
            pevalAndTerm (pevalNotTerm (ssymbTerm "a")) (pevalNotTerm (ssymbTerm "b"))
              @=? pevalNotTerm (pevalOrTerm (ssymbTerm "a") (ssymbTerm "b"))
        ],
      testGroup
        "ITE"
        [ testCase "On concrete condition" $ do
            pevalITETerm (concTerm True) (ssymbTerm "a" :: Term Integer) (ssymbTerm "b")
              @=? ssymbTerm "a"
            pevalITETerm (concTerm False) (ssymbTerm "a" :: Term Integer) (ssymbTerm "b")
              @=? ssymbTerm "b",
          testCase "On same branches" $ do
            pevalITETerm (ssymbTerm "c") (ssymbTerm "a" :: Term Integer) (ssymbTerm "a")
              @=? ssymbTerm "a",
          testCase "On both not" $ do
            pevalITETerm (ssymbTerm "c") (pevalNotTerm $ ssymbTerm "a") (pevalNotTerm $ ssymbTerm "b")
              @=? pevalNotTerm (pevalITETerm (ssymbTerm "c") (ssymbTerm "a") (ssymbTerm "b")),
          testCase "On not in condition" $ do
            pevalITETerm (pevalNotTerm $ ssymbTerm "c") (ssymbTerm "a" :: Term Integer) (ssymbTerm "b")
              @=? pevalITETerm (ssymbTerm "c") (ssymbTerm "b") (ssymbTerm "a"),
          testCase "On all arguments as ITE with same conditions" $ do
            pevalITETerm
              (pevalITETerm (ssymbTerm "a") (ssymbTerm "b") (ssymbTerm "c"))
              (pevalITETerm (ssymbTerm "a") (ssymbTerm "d" :: Term Integer) (ssymbTerm "e"))
              (pevalITETerm (ssymbTerm "a") (ssymbTerm "f" :: Term Integer) (ssymbTerm "g"))
              @=? pevalITETerm
                (ssymbTerm "a")
                (pevalITETerm (ssymbTerm "b") (ssymbTerm "d") (ssymbTerm "f"))
                (pevalITETerm (ssymbTerm "c") (ssymbTerm "e") (ssymbTerm "g")),
          testCase "On with true branch as ITE" $ do
            pevalITETerm
              (ssymbTerm "a")
              (pevalITETerm (ssymbTerm "a") (ssymbTerm "b" :: Term Integer) (ssymbTerm "c"))
              (ssymbTerm "d")
              @=? pevalITETerm (ssymbTerm "a") (ssymbTerm "b") (ssymbTerm "d")
            pevalITETerm
              (ssymbTerm "a")
              (pevalITETerm (ssymbTerm "b") (ssymbTerm "c" :: Term Integer) (ssymbTerm "d"))
              (ssymbTerm "c")
              @=? pevalITETerm
                (pevalOrTerm (pevalNotTerm $ ssymbTerm "a") (ssymbTerm "b"))
                (ssymbTerm "c")
                (ssymbTerm "d")
            pevalITETerm
              (ssymbTerm "a")
              (pevalITETerm (ssymbTerm "b") (ssymbTerm "c" :: Term Integer) (ssymbTerm "d"))
              (ssymbTerm "d")
              @=? pevalITETerm
                (pevalAndTerm (ssymbTerm "a") (ssymbTerm "b"))
                (ssymbTerm "c")
                (ssymbTerm "d"),
          testCase "On false branch as ITE" $ do
            pevalITETerm
              (ssymbTerm "a")
              (ssymbTerm "b")
              (pevalITETerm (ssymbTerm "a") (ssymbTerm "c" :: Term Integer) (ssymbTerm "d"))
              @=? pevalITETerm (ssymbTerm "a") (ssymbTerm "b") (ssymbTerm "d")
            pevalITETerm
              (ssymbTerm "a")
              (ssymbTerm "b")
              (pevalITETerm (ssymbTerm "c") (ssymbTerm "b" :: Term Integer) (ssymbTerm "d"))
              @=? pevalITETerm
                (pevalOrTerm (ssymbTerm "a") (ssymbTerm "c"))
                (ssymbTerm "b")
                (ssymbTerm "d")
            pevalITETerm
              (ssymbTerm "a")
              (ssymbTerm "b")
              (pevalITETerm (ssymbTerm "c") (ssymbTerm "d" :: Term Integer) (ssymbTerm "b"))
              @=? pevalITETerm
                (pevalOrTerm (ssymbTerm "a") (pevalNotTerm $ ssymbTerm "c"))
                (ssymbTerm "b")
                (ssymbTerm "d"),
          testCase "On both And" $ do
            pevalITETerm
              (ssymbTerm "a")
              (pevalAndTerm (ssymbTerm "b") (ssymbTerm "c"))
              (pevalAndTerm (ssymbTerm "b") (ssymbTerm "d"))
              @=? pevalAndTerm (ssymbTerm "b") (pevalITETerm (ssymbTerm "a") (ssymbTerm "c") (ssymbTerm "d"))
            pevalITETerm
              (ssymbTerm "a")
              (pevalAndTerm (ssymbTerm "c") (ssymbTerm "b"))
              (pevalAndTerm (ssymbTerm "b") (ssymbTerm "d"))
              @=? pevalAndTerm (ssymbTerm "b") (pevalITETerm (ssymbTerm "a") (ssymbTerm "c") (ssymbTerm "d"))
            pevalITETerm
              (ssymbTerm "a")
              (pevalAndTerm (ssymbTerm "b") (ssymbTerm "c"))
              (pevalAndTerm (ssymbTerm "d") (ssymbTerm "b"))
              @=? pevalAndTerm (ssymbTerm "b") (pevalITETerm (ssymbTerm "a") (ssymbTerm "c") (ssymbTerm "d"))
            pevalITETerm
              (ssymbTerm "a")
              (pevalAndTerm (ssymbTerm "c") (ssymbTerm "b"))
              (pevalAndTerm (ssymbTerm "d") (ssymbTerm "b"))
              @=? pevalAndTerm (ssymbTerm "b") (pevalITETerm (ssymbTerm "a") (ssymbTerm "c") (ssymbTerm "d")),
          testCase "On left And" $ do
            pevalITETerm
              (ssymbTerm "a")
              (pevalAndTerm (ssymbTerm "b") (ssymbTerm "c"))
              (ssymbTerm "b")
              @=? pevalAndTerm (ssymbTerm "b") (pevalOrTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "c"))
            pevalITETerm
              (ssymbTerm "a")
              (pevalAndTerm (ssymbTerm "b") (ssymbTerm "c"))
              (ssymbTerm "c")
              @=? pevalAndTerm (ssymbTerm "c") (pevalOrTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "b"))
            pevalITETerm
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              (pevalAndTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
              (ssymbTerm "c")
              @=? pevalAndTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "c")
            pevalITETerm
              (ssymbTerm "a")
              (pevalAndTerm (pevalNotTerm $ ssymbTerm "a") (ssymbTerm "b"))
              (ssymbTerm "c")
              @=? pevalAndTerm (pevalNotTerm $ ssymbTerm "a") (ssymbTerm "c")
            pevalITETerm
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              (pevalAndTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
              (ssymbTerm "c")
              @=? pevalAndTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "c")
            pevalITETerm
              (ssymbTerm "a")
              (pevalAndTerm (ssymbTerm "b") (pevalNotTerm $ ssymbTerm "a"))
              (ssymbTerm "c")
              @=? pevalAndTerm (pevalNotTerm $ ssymbTerm "a") (ssymbTerm "c")
            pevalITETerm
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              (pevalAndTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
              (ssymbTerm "c")
              @=? pevalITETerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b") (ssymbTerm "c")
            pevalITETerm
              (ssymbTerm "a")
              (pevalAndTerm (ssymbTerm "a") (ssymbTerm "b"))
              (ssymbTerm "c")
              @=? pevalITETerm (ssymbTerm "a") (ssymbTerm "b") (ssymbTerm "c")
            pevalITETerm
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              (pevalAndTerm (ssymbTerm "b") (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
              (ssymbTerm "c")
              @=? pevalITETerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b") (ssymbTerm "c")
            pevalITETerm
              (ssymbTerm "a")
              (pevalAndTerm (ssymbTerm "b") (ssymbTerm "a"))
              (ssymbTerm "c")
              @=? pevalITETerm (ssymbTerm "a") (ssymbTerm "b") (ssymbTerm "c"),
          testCase "On right And" $ do
            pevalITETerm
              (ssymbTerm "a")
              (ssymbTerm "b")
              (pevalAndTerm (ssymbTerm "b") (ssymbTerm "c"))
              @=? pevalAndTerm (ssymbTerm "b") (pevalOrTerm (ssymbTerm "a") (ssymbTerm "c"))
            pevalITETerm
              (ssymbTerm "a")
              (ssymbTerm "c")
              (pevalAndTerm (ssymbTerm "b") (ssymbTerm "c"))
              @=? pevalAndTerm (ssymbTerm "c") (pevalOrTerm (ssymbTerm "a") (ssymbTerm "b")),
          testCase "On both Or" $ do
            pevalITETerm
              (ssymbTerm "a")
              (pevalOrTerm (ssymbTerm "b") (ssymbTerm "c"))
              (pevalOrTerm (ssymbTerm "b") (ssymbTerm "d"))
              @=? pevalOrTerm (ssymbTerm "b") (pevalITETerm (ssymbTerm "a") (ssymbTerm "c") (ssymbTerm "d"))
            pevalITETerm
              (ssymbTerm "a")
              (pevalOrTerm (ssymbTerm "c") (ssymbTerm "b"))
              (pevalOrTerm (ssymbTerm "b") (ssymbTerm "d"))
              @=? pevalOrTerm (ssymbTerm "b") (pevalITETerm (ssymbTerm "a") (ssymbTerm "c") (ssymbTerm "d"))
            pevalITETerm
              (ssymbTerm "a")
              (pevalOrTerm (ssymbTerm "b") (ssymbTerm "c"))
              (pevalOrTerm (ssymbTerm "d") (ssymbTerm "b"))
              @=? pevalOrTerm (ssymbTerm "b") (pevalITETerm (ssymbTerm "a") (ssymbTerm "c") (ssymbTerm "d"))
            pevalITETerm
              (ssymbTerm "a")
              (pevalOrTerm (ssymbTerm "c") (ssymbTerm "b"))
              (pevalOrTerm (ssymbTerm "d") (ssymbTerm "b"))
              @=? pevalOrTerm (ssymbTerm "b") (pevalITETerm (ssymbTerm "a") (ssymbTerm "c") (ssymbTerm "d")),
          testCase "On left Or" $ do
            pevalITETerm
              (ssymbTerm "a")
              (pevalOrTerm (ssymbTerm "b") (ssymbTerm "c"))
              (ssymbTerm "b")
              @=? pevalOrTerm (ssymbTerm "b") (pevalAndTerm (ssymbTerm "a") (ssymbTerm "c"))
            pevalITETerm
              (ssymbTerm "a")
              (pevalOrTerm (ssymbTerm "b") (ssymbTerm "c"))
              (ssymbTerm "c")
              @=? pevalOrTerm (ssymbTerm "c") (pevalAndTerm (ssymbTerm "a") (ssymbTerm "b"))
            pevalITETerm
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              (pevalOrTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
              (ssymbTerm "c")
              @=? pevalITETerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b") (ssymbTerm "c")
            pevalITETerm
              (ssymbTerm "a")
              (pevalOrTerm (pevalNotTerm $ ssymbTerm "a") (ssymbTerm "b"))
              (ssymbTerm "c")
              @=? pevalITETerm (ssymbTerm "a") (ssymbTerm "b") (ssymbTerm "c")
            pevalITETerm
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              (pevalOrTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
              (ssymbTerm "c")
              @=? pevalITETerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b") (ssymbTerm "c")
            pevalITETerm
              (ssymbTerm "a")
              (pevalOrTerm (ssymbTerm "b") (pevalNotTerm $ ssymbTerm "a"))
              (ssymbTerm "c")
              @=? pevalITETerm (ssymbTerm "a") (ssymbTerm "b") (ssymbTerm "c")
            pevalITETerm
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              (pevalOrTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
              (ssymbTerm "c")
              @=? pevalOrTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "c")
            pevalITETerm
              (ssymbTerm "a")
              (pevalOrTerm (ssymbTerm "a") (ssymbTerm "b"))
              (ssymbTerm "c")
              @=? pevalOrTerm (ssymbTerm "a") (ssymbTerm "c")
            pevalITETerm
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              (pevalOrTerm (ssymbTerm "b") (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
              (ssymbTerm "c")
              @=? pevalOrTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "c")
            pevalITETerm
              (ssymbTerm "a")
              (pevalOrTerm (ssymbTerm "b") (ssymbTerm "a"))
              (ssymbTerm "c")
              @=? pevalOrTerm (ssymbTerm "a") (ssymbTerm "c"),
          testCase "On right Or" $ do
            pevalITETerm
              (ssymbTerm "a")
              (ssymbTerm "b")
              (pevalOrTerm (ssymbTerm "b") (ssymbTerm "c"))
              @=? pevalOrTerm (ssymbTerm "b") (pevalAndTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "c"))
            pevalITETerm
              (ssymbTerm "a")
              (ssymbTerm "c")
              (pevalOrTerm (ssymbTerm "b") (ssymbTerm "c"))
              @=? pevalOrTerm (ssymbTerm "c") (pevalAndTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "b")),
          testCase "On const boolean in branches" $ do
            pevalITETerm
              (ssymbTerm "a")
              (concTerm True)
              (ssymbTerm "b")
              @=? pevalOrTerm (ssymbTerm "a") (ssymbTerm "b")
            pevalITETerm
              (ssymbTerm "a")
              (concTerm False)
              (ssymbTerm "b")
              @=? pevalAndTerm (pevalNotTerm $ ssymbTerm "a") (ssymbTerm "b")
            pevalITETerm
              (ssymbTerm "a")
              (ssymbTerm "b")
              (concTerm True)
              @=? pevalOrTerm (pevalNotTerm $ ssymbTerm "a") (ssymbTerm "b")
            pevalITETerm
              (ssymbTerm "a")
              (ssymbTerm "b")
              (concTerm False)
              @=? pevalAndTerm (ssymbTerm "a") (ssymbTerm "b"),
          testCase "On condition equal to some branch" $ do
            pevalITETerm
              (ssymbTerm "a")
              (ssymbTerm "a")
              (ssymbTerm "b")
              @=? pevalOrTerm (ssymbTerm "a") (ssymbTerm "b")
            pevalITETerm
              (ssymbTerm "a")
              (ssymbTerm "b")
              (ssymbTerm "a")
              @=? pevalAndTerm (ssymbTerm "a") (ssymbTerm "b"),
          testCase "On left Not" $ do
            pevalITETerm (ssymbTerm "a") (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "b")
              @=? pevalAndTerm (pevalNotTerm $ ssymbTerm "a") (ssymbTerm "b"),
          testCase "On right Not" $ do
            pevalITETerm (ssymbTerm "a") (ssymbTerm "b") (pevalNotTerm (ssymbTerm "a"))
              @=? pevalOrTerm (pevalNotTerm $ ssymbTerm "a") (ssymbTerm "b"),
          testCase "On left Not And" $ do
            pevalITETerm
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              (pevalNotTerm (pevalAndTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b")))
              (ssymbTerm "c")
              @=? pevalOrTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "c")
            pevalITETerm
              (ssymbTerm "a")
              (pevalNotTerm (pevalAndTerm (pevalNotTerm $ ssymbTerm "a") (ssymbTerm "b")))
              (ssymbTerm "c")
              @=? pevalOrTerm (ssymbTerm "a") (ssymbTerm "c")
            pevalITETerm
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              (pevalNotTerm (pevalAndTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2))))
              (ssymbTerm "c")
              @=? pevalOrTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "c")
            pevalITETerm
              (ssymbTerm "a")
              (pevalNotTerm (pevalAndTerm (ssymbTerm "b") (pevalNotTerm $ ssymbTerm "a")))
              (ssymbTerm "c")
              @=? pevalOrTerm (ssymbTerm "a") (ssymbTerm "c")
            pevalITETerm
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              (pevalNotTerm (pevalAndTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b")))
              (ssymbTerm "c")
              @=? pevalITETerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (pevalNotTerm $ ssymbTerm "b") (ssymbTerm "c")
            pevalITETerm
              (ssymbTerm "a")
              (pevalNotTerm (pevalAndTerm (ssymbTerm "a") (ssymbTerm "b")))
              (ssymbTerm "c")
              @=? pevalITETerm (ssymbTerm "a") (pevalNotTerm $ ssymbTerm "b") (ssymbTerm "c")
            pevalITETerm
              (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
              (pevalNotTerm (pevalAndTerm (ssymbTerm "b") (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2))))
              (ssymbTerm "c")
              @=? pevalITETerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (pevalNotTerm $ ssymbTerm "b") (ssymbTerm "c")
            pevalITETerm
              (ssymbTerm "a")
              (pevalNotTerm (pevalAndTerm (ssymbTerm "b") (ssymbTerm "a")))
              (ssymbTerm "c")
              @=? pevalITETerm (ssymbTerm "a") (pevalNotTerm $ ssymbTerm "b") (ssymbTerm "c")
        ],
      testGroup
        "Imply"
        [ testCase "pevalImplyTerm" $ do
            ssymbTerm "a"
              `pevalImplyTerm` ssymbTerm "b"
              @=? pevalOrTerm (pevalNotTerm $ ssymbTerm "a") (ssymbTerm "b")
        ],
      testGroup
        "Xor"
        [ testCase "pevalXorTerm" $ do
            ssymbTerm "a"
              `pevalXorTerm` ssymbTerm "b"
              @=? pevalOrTerm
                (pevalAndTerm (pevalNotTerm $ ssymbTerm "a") (ssymbTerm "b"))
                (pevalAndTerm (ssymbTerm "a") (pevalNotTerm $ ssymbTerm "b"))
        ]
    ]
