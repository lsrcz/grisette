{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pizza.IR.SymPrim.Data.Prim.BoolSpec where

import Pizza.IR.SymPrim.Data.BV
import Pizza.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Pizza.IR.SymPrim.Data.Prim.InternedTerm.Term
import Pizza.IR.SymPrim.Data.Prim.PartialEval.Bool
import Pizza.IR.SymPrim.Data.Prim.PartialEval.Num
import Test.Hspec

spec :: Spec
spec = do
  describe "Not" $ do
    describe "Not construction" $ do
      it "Not on concrete" $ do
        pevalNotTerm (concTerm True) `shouldBe` concTerm False
        pevalNotTerm (concTerm True) `shouldBe` concTerm False
      it "Not on general symbolic" $ do
        pevalNotTerm (ssymbTerm "a") `shouldBe` notTerm (ssymbTerm "a" :: Term Bool)
      it "Not on Not" $ do
        pevalNotTerm (pevalNotTerm (ssymbTerm "a")) `shouldBe` ssymbTerm "a"
      it "Not on Or Not" $ do
        pevalNotTerm (pevalOrTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "b"))
          `shouldBe` pevalAndTerm (ssymbTerm "a") (pevalNotTerm (ssymbTerm "b"))
        pevalNotTerm (pevalOrTerm (ssymbTerm "a") (pevalNotTerm (ssymbTerm "b")))
          `shouldBe` pevalAndTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "b")
      it "Not on And Not" $ do
        pevalNotTerm (pevalAndTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "b"))
          `shouldBe` pevalOrTerm (ssymbTerm "a") (pevalNotTerm (ssymbTerm "b"))
        pevalNotTerm (pevalAndTerm (ssymbTerm "a") (pevalNotTerm (ssymbTerm "b")))
          `shouldBe` pevalOrTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "b")
  describe "Eqv & NEqv" $ do
    describe "Eqv construction" $ do
      it "Eqv with both concrete" $ do
        pevalEqvTerm (concTerm True) (concTerm True) `shouldBe` concTerm True
        pevalEqvTerm (concTerm True) (concTerm False) `shouldBe` concTerm False
        pevalEqvTerm (concTerm False) (concTerm True) `shouldBe` concTerm False
        pevalEqvTerm (concTerm False) (concTerm False) `shouldBe` concTerm True
        pevalEqvTerm (concTerm (1 :: Integer)) (concTerm 1) `shouldBe` concTerm True
        pevalEqvTerm (concTerm (1 :: Integer)) (concTerm 2) `shouldBe` concTerm False
        pevalEqvTerm (concTerm (1 :: IntN 4)) (concTerm 1) `shouldBe` concTerm True
        pevalEqvTerm (concTerm (1 :: IntN 4)) (concTerm 2) `shouldBe` concTerm False
        pevalEqvTerm (concTerm (1 :: WordN 4)) (concTerm 1) `shouldBe` concTerm True
        pevalEqvTerm (concTerm (1 :: WordN 4)) (concTerm 2) `shouldBe` concTerm False
      it "Eqv with single concrete always put concrete ones in the right" $ do
        pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)
          `shouldBe` eqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1 :: Term Integer)
        pevalEqvTerm (concTerm 1) (ssymbTerm "a" :: Term Integer)
          `shouldBe` eqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1 :: Term Integer)
      it "Eqv on general symbolic" $ do
        pevalEqvTerm (ssymbTerm "a" :: Term Integer) (ssymbTerm "b")
          `shouldBe` eqvTerm (ssymbTerm "a" :: Term Integer) (ssymbTerm "b" :: Term Integer)
      it "Eqv on Bool with single concrete" $ do
        pevalEqvTerm (concTerm True) (ssymbTerm "a") `shouldBe` ssymbTerm "a"
        pevalEqvTerm (ssymbTerm "a") (concTerm True) `shouldBe` ssymbTerm "a"
        pevalEqvTerm (concTerm False) (ssymbTerm "a") `shouldBe` pevalNotTerm (ssymbTerm "a")
        pevalEqvTerm (ssymbTerm "a") (concTerm False) `shouldBe` pevalNotTerm (ssymbTerm "a")
      it "NEqv on general symbolic" $ do
        pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (ssymbTerm "b")
          `shouldBe` pevalNotTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (ssymbTerm "b"))
      it "Eqv(Not(x), x) / Eqv(x, Not(x))" $ do
        pevalEqvTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "a") `shouldBe` concTerm False
        pevalEqvTerm (ssymbTerm "a") (pevalNotTerm (ssymbTerm "a")) `shouldBe` concTerm False
      it "Eqv(n1+x, n2)" $ do
        pevalEqvTerm (pevalAddNumTerm (concTerm 1 :: Term Integer) (ssymbTerm "a")) (concTerm 3)
          `shouldBe` pevalEqvTerm (ssymbTerm "a") (concTerm 2 :: Term Integer)
        pevalEqvTerm (pevalAddNumTerm (concTerm 1 :: Term (IntN 4)) (ssymbTerm "a")) (concTerm 3)
          `shouldBe` pevalEqvTerm (ssymbTerm "a") (concTerm 2 :: Term (IntN 4))
        pevalEqvTerm (pevalAddNumTerm (concTerm 1 :: Term (WordN 4)) (ssymbTerm "a")) (concTerm 3)
          `shouldBe` pevalEqvTerm (ssymbTerm "a") (concTerm 2 :: Term (WordN 4))
      it "Eqv(n1, n2+x)" $ do
        pevalEqvTerm (concTerm 3) (pevalAddNumTerm (concTerm 1 :: Term Integer) (ssymbTerm "a"))
          `shouldBe` pevalEqvTerm (ssymbTerm "a") (concTerm 2 :: Term Integer)
        pevalEqvTerm (concTerm 3) (pevalAddNumTerm (concTerm 1 :: Term (IntN 4)) (ssymbTerm "a"))
          `shouldBe` pevalEqvTerm (ssymbTerm "a") (concTerm 2 :: Term (IntN 4))
        pevalEqvTerm (concTerm 3) (pevalAddNumTerm (concTerm 1 :: Term (WordN 4)) (ssymbTerm "a"))
          `shouldBe` pevalEqvTerm (ssymbTerm "a") (concTerm 2 :: Term (WordN 4))
      it "Eqv(l, ITE(c, l, f)) / Eqv(l, ITE(c, t, l) / Eqv(ITE(c, r, f), r) / Eqv(ITE(c, t, r), r)" $ do
        pevalEqvTerm (ssymbTerm "a" :: Term Integer) (pevalITETerm (ssymbTerm "b") (ssymbTerm "a") (ssymbTerm "c"))
          `shouldBe` pevalOrTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a") (ssymbTerm "c" :: Term Integer))
        pevalEqvTerm (ssymbTerm "a" :: Term Integer) (pevalITETerm (ssymbTerm "b") (ssymbTerm "c") (ssymbTerm "a"))
          `shouldBe` pevalOrTerm (pevalNotTerm $ ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a") (ssymbTerm "c" :: Term Integer))
        pevalEqvTerm (pevalITETerm (ssymbTerm "b") (ssymbTerm "a") (ssymbTerm "c")) (ssymbTerm "a" :: Term Integer)
          `shouldBe` pevalOrTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "c") (ssymbTerm "a" :: Term Integer))
        pevalEqvTerm (pevalITETerm (ssymbTerm "b") (ssymbTerm "c") (ssymbTerm "a")) (ssymbTerm "a" :: Term Integer)
          `shouldBe` pevalOrTerm (pevalNotTerm $ ssymbTerm "b") (pevalEqvTerm (ssymbTerm "c") (ssymbTerm "a" :: Term Integer))
  describe "Or" $ do
    describe "Or construction" $ do
      it "Or with both concrete" $ do
        pevalOrTerm (concTerm True) (concTerm True) `shouldBe` concTerm True
        pevalOrTerm (concTerm True) (concTerm False) `shouldBe` concTerm True
        pevalOrTerm (concTerm False) (concTerm True) `shouldBe` concTerm True
        pevalOrTerm (concTerm False) (concTerm False) `shouldBe` concTerm False
      it "Or on general symbolic" $ do
        pevalOrTerm (ssymbTerm "a") (ssymbTerm "b")
          `shouldBe` orTerm (ssymbTerm "a" :: Term Bool) (ssymbTerm "b" :: Term Bool)
      it "Or(x, y) -> True" $ do
        pevalOrTerm (concTerm True) (ssymbTerm "b") `shouldBe` concTerm True
        pevalOrTerm (ssymbTerm "a") (concTerm True) `shouldBe` concTerm True
        pevalOrTerm
          (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2))
          `shouldBe` concTerm True
        pevalOrTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "a") `shouldBe` concTerm True
        pevalOrTerm (ssymbTerm "a") (pevalNotTerm (ssymbTerm "a")) `shouldBe` concTerm True
      it "Or(x, y) -> x" $ do
        pevalOrTerm (ssymbTerm "a") (concTerm False) `shouldBe` ssymbTerm "a"
        pevalOrTerm
          (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2))
          `shouldBe` pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)
        pevalOrTerm (ssymbTerm "a") (ssymbTerm "a") `shouldBe` ssymbTerm "a"
      it "Or(x, y) -> y" $ do
        pevalOrTerm (concTerm False) (ssymbTerm "a") `shouldBe` ssymbTerm "a"
        pevalOrTerm
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2))
          (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          `shouldBe` pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)
      it "Or(x, Or(y1, y2)) -> True" $ do
        pevalOrTerm (pevalNotTerm (ssymbTerm "a")) (pevalOrTerm (ssymbTerm "a") (ssymbTerm "b")) `shouldBe` concTerm True
        pevalOrTerm (ssymbTerm "a") (pevalOrTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "b")) `shouldBe` concTerm True
        pevalOrTerm
          (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          (pevalOrTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
          `shouldBe` concTerm True

        pevalOrTerm (pevalNotTerm (ssymbTerm "a")) (pevalOrTerm (ssymbTerm "b") (ssymbTerm "a")) `shouldBe` concTerm True
        pevalOrTerm (ssymbTerm "a") (pevalOrTerm (ssymbTerm "b") (pevalNotTerm (ssymbTerm "a"))) `shouldBe` concTerm True
        pevalOrTerm
          (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          (pevalOrTerm (ssymbTerm "b") (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
          `shouldBe` concTerm True
      it "Or(x, Or(y1, y2)) -> Or(x, y2)" $ do
        pevalOrTerm
          (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          (pevalOrTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
          `shouldBe` pevalOrTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b")
      it "Or(x, Or(y1, y2)) -> Or(x, y1)" $ do
        pevalOrTerm
          (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          (pevalOrTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
          `shouldBe` pevalOrTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b")

      it "Or(x, y@Or(y1, y2)) -> y" $ do
        pevalOrTerm (ssymbTerm "a") (pevalOrTerm (ssymbTerm "a") (ssymbTerm "b"))
          `shouldBe` pevalOrTerm (ssymbTerm "a") (ssymbTerm "b")
        pevalOrTerm
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2))
          (pevalOrTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b"))
          `shouldBe` pevalOrTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b")
        pevalOrTerm (ssymbTerm "a") (pevalOrTerm (ssymbTerm "b") (ssymbTerm "a"))
          `shouldBe` pevalOrTerm (ssymbTerm "b") (ssymbTerm "a")
        pevalOrTerm
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2))
          (pevalOrTerm (ssymbTerm "b") (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)))
          `shouldBe` pevalOrTerm (ssymbTerm "b") (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
      it "Or(Or(x1, x2), y) -> True" $ do
        pevalOrTerm (pevalOrTerm (ssymbTerm "a") (ssymbTerm "b")) (pevalNotTerm (ssymbTerm "a")) `shouldBe` concTerm True
        pevalOrTerm (pevalOrTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "b")) (ssymbTerm "a") `shouldBe` concTerm True
        pevalOrTerm
          (pevalOrTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
          (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          `shouldBe` concTerm True

        pevalOrTerm (pevalOrTerm (ssymbTerm "b") (ssymbTerm "a")) (pevalNotTerm (ssymbTerm "a")) `shouldBe` concTerm True
        pevalOrTerm (pevalOrTerm (ssymbTerm "b") (pevalNotTerm (ssymbTerm "a"))) (ssymbTerm "a") `shouldBe` concTerm True
        pevalOrTerm
          (pevalOrTerm (ssymbTerm "b") (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
          (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          `shouldBe` concTerm True
      it "Or(x@Or(x1, x2), y) -> x" $ do
        pevalOrTerm (pevalOrTerm (ssymbTerm "a") (ssymbTerm "b")) (ssymbTerm "a")
          `shouldBe` pevalOrTerm (ssymbTerm "a") (ssymbTerm "b")
        pevalOrTerm
          (pevalOrTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b"))
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2))
          `shouldBe` pevalOrTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b")
        pevalOrTerm (pevalOrTerm (ssymbTerm "b") (ssymbTerm "a")) (ssymbTerm "a")
          `shouldBe` pevalOrTerm (ssymbTerm "b") (ssymbTerm "a")
        pevalOrTerm
          (pevalOrTerm (ssymbTerm "b") (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)))
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2))
          `shouldBe` pevalOrTerm (ssymbTerm "b") (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
      it "Or(Or(x1, x2), y) -> Or(x2, y)" $ do
        pevalOrTerm
          (pevalOrTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
          (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          `shouldBe` pevalOrTerm (ssymbTerm "b") (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
      it "Or(Or(x1, x2), y) -> Or(x1, y)" $ do
        pevalOrTerm
          (pevalOrTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
          (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          `shouldBe` pevalOrTerm (ssymbTerm "b") (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
      it "Or(x, And(y1, y2)) -> x" $ do
        pevalOrTerm
          (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          (pevalAndTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
          `shouldBe` pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)
        pevalOrTerm
          (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          (pevalAndTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
          `shouldBe` pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)
      it "Or(x, And(y1, y2)) -> Or(x, y2)" $ do
        pevalOrTerm (ssymbTerm "a") (pevalAndTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "b"))
          `shouldBe` pevalOrTerm (ssymbTerm "a") (ssymbTerm "b")
        pevalOrTerm (pevalNotTerm (ssymbTerm "a")) (pevalAndTerm (ssymbTerm "a") (ssymbTerm "b"))
          `shouldBe` pevalOrTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "b")
        pevalOrTerm
          (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          (pevalAndTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
          `shouldBe` pevalOrTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b")
      it "Or(And(x1, x2), y) -> y" $ do
        pevalOrTerm
          (pevalAndTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
          (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          `shouldBe` pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)
        pevalOrTerm
          (pevalAndTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
          (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          `shouldBe` pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)
      it "Or(x, And(y1, y2)) -> Or(x, y1)" $ do
        pevalOrTerm (ssymbTerm "a") (pevalAndTerm (ssymbTerm "b") (pevalNotTerm (ssymbTerm "a")))
          `shouldBe` pevalOrTerm (ssymbTerm "a") (ssymbTerm "b")
        pevalOrTerm (pevalNotTerm (ssymbTerm "a")) (pevalAndTerm (ssymbTerm "b") (ssymbTerm "a"))
          `shouldBe` pevalOrTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "b")
        pevalOrTerm
          (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          (pevalAndTerm (ssymbTerm "b") (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
          `shouldBe` pevalOrTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b")
      it "Or(Not(x), Not(y)) -> Not(And(x, y))" $ do
        pevalOrTerm (pevalNotTerm (ssymbTerm "a")) (pevalNotTerm (ssymbTerm "b"))
          `shouldBe` pevalNotTerm (pevalAndTerm (ssymbTerm "a") (ssymbTerm "b"))
  describe "And" $ do
    describe "And construction" $ do
      it "And with both concrete" $ do
        pevalAndTerm (concTerm True) (concTerm True) `shouldBe` concTerm True
        pevalAndTerm (concTerm True) (concTerm False) `shouldBe` concTerm False
        pevalAndTerm (concTerm False) (concTerm True) `shouldBe` concTerm False
        pevalAndTerm (concTerm False) (concTerm False) `shouldBe` concTerm False
      it "And on general symbolic" $ do
        pevalAndTerm (ssymbTerm "a") (ssymbTerm "b")
          `shouldBe` andTerm (ssymbTerm "a" :: Term Bool) (ssymbTerm "b" :: Term Bool)
      it "And(x, y) -> False" $ do
        pevalAndTerm (concTerm False) (ssymbTerm "b") `shouldBe` concTerm False
        pevalAndTerm (ssymbTerm "a") (concTerm False) `shouldBe` concTerm False
        pevalAndTerm
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2))
          `shouldBe` concTerm False
        pevalAndTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "a") `shouldBe` concTerm False
        pevalAndTerm (ssymbTerm "a") (pevalNotTerm (ssymbTerm "a")) `shouldBe` concTerm False
      it "And(x, y) -> x" $ do
        pevalAndTerm (ssymbTerm "a") (concTerm True) `shouldBe` ssymbTerm "a"
        pevalAndTerm
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2))
          `shouldBe` pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)
        pevalAndTerm (ssymbTerm "a") (ssymbTerm "a") `shouldBe` ssymbTerm "a"
      it "And(x, y) -> y" $ do
        pevalAndTerm (concTerm True) (ssymbTerm "a") `shouldBe` ssymbTerm "a"
        pevalAndTerm
          (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2))
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          `shouldBe` pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)
      it "And(x, And(y1, y2)) -> False" $ do
        pevalAndTerm (pevalNotTerm (ssymbTerm "a")) (pevalAndTerm (ssymbTerm "a") (ssymbTerm "b")) `shouldBe` concTerm False
        pevalAndTerm (ssymbTerm "a") (pevalAndTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "b")) `shouldBe` concTerm False
        pevalAndTerm
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          (pevalAndTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
          `shouldBe` concTerm False

        pevalAndTerm (pevalNotTerm (ssymbTerm "a")) (pevalAndTerm (ssymbTerm "b") (ssymbTerm "a")) `shouldBe` concTerm False
        pevalAndTerm (ssymbTerm "a") (pevalAndTerm (ssymbTerm "b") (pevalNotTerm (ssymbTerm "a"))) `shouldBe` concTerm False
        pevalAndTerm
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          (pevalAndTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
          `shouldBe` concTerm False
      it "And(x, And(y1, y2)) -> And(x, y2)" $ do
        pevalAndTerm
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          (pevalAndTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
          `shouldBe` pevalAndTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b")
      it "And(x, And(y1, y2)) -> And(x, y1)" $ do
        pevalAndTerm
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          (pevalAndTerm (ssymbTerm "b") (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
          `shouldBe` pevalAndTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b")
      it "And(x, y@And(y1, y2)) -> y" $ do
        pevalAndTerm (ssymbTerm "a") (pevalAndTerm (ssymbTerm "a") (ssymbTerm "b"))
          `shouldBe` pevalAndTerm (ssymbTerm "a") (ssymbTerm "b")
        pevalAndTerm
          (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2))
          (pevalAndTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b"))
          `shouldBe` pevalAndTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b")
        pevalAndTerm (ssymbTerm "a") (pevalAndTerm (ssymbTerm "b") (ssymbTerm "a"))
          `shouldBe` pevalAndTerm (ssymbTerm "b") (ssymbTerm "a")
        pevalAndTerm
          (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2))
          (pevalAndTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)))
          `shouldBe` pevalAndTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
      it "And(And(x1, x2), y) -> False" $ do
        pevalAndTerm (pevalAndTerm (ssymbTerm "a") (ssymbTerm "b")) (pevalNotTerm (ssymbTerm "a")) `shouldBe` concTerm False
        pevalAndTerm (pevalAndTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "b")) (ssymbTerm "a") `shouldBe` concTerm False
        pevalAndTerm
          (pevalAndTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          `shouldBe` concTerm False

        pevalAndTerm (pevalAndTerm (ssymbTerm "b") (ssymbTerm "a")) (pevalNotTerm (ssymbTerm "a")) `shouldBe` concTerm False
        pevalAndTerm (pevalAndTerm (ssymbTerm "b") (pevalNotTerm (ssymbTerm "a"))) (ssymbTerm "a") `shouldBe` concTerm False
        pevalAndTerm
          (pevalAndTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          `shouldBe` concTerm False
      it "And(x@And(x1, x2), y) -> x" $ do
        pevalAndTerm (pevalAndTerm (ssymbTerm "a") (ssymbTerm "b")) (ssymbTerm "a")
          `shouldBe` pevalAndTerm (ssymbTerm "a") (ssymbTerm "b")
        pevalAndTerm
          (pevalAndTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b"))
          (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2))
          `shouldBe` pevalAndTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b")
        pevalAndTerm (pevalAndTerm (ssymbTerm "b") (ssymbTerm "a")) (ssymbTerm "a")
          `shouldBe` pevalAndTerm (ssymbTerm "b") (ssymbTerm "a")
        pevalAndTerm
          (pevalAndTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)))
          (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2))
          `shouldBe` pevalAndTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
      it "And(And(x1, x2), y) -> And(x2, y)" $ do
        pevalAndTerm
          (pevalAndTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          `shouldBe` pevalAndTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
      it "And(And(x1, x2), y) -> And(x1, y)" $ do
        pevalAndTerm
          (pevalAndTerm (ssymbTerm "b") (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          `shouldBe` pevalAndTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
      it "And(x, Or(y1, y2)) -> x" $ do
        pevalAndTerm
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          (pevalOrTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
          `shouldBe` pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)
        pevalAndTerm
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          (pevalOrTerm (ssymbTerm "b") (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
          `shouldBe` pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)
      it "And(x, Or(y1, y2)) -> And(x, y2)" $ do
        pevalAndTerm (ssymbTerm "a") (pevalOrTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "b"))
          `shouldBe` pevalAndTerm (ssymbTerm "a") (ssymbTerm "b")
        pevalAndTerm (pevalNotTerm (ssymbTerm "a")) (pevalOrTerm (ssymbTerm "a") (ssymbTerm "b"))
          `shouldBe` pevalAndTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "b")
        pevalAndTerm
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          (pevalOrTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
          `shouldBe` pevalAndTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b")
      it "And(Or(x1, x2), y) -> y" $ do
        pevalAndTerm
          (pevalOrTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          `shouldBe` pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)
        pevalAndTerm
          (pevalOrTerm (ssymbTerm "b") (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          `shouldBe` pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)
      it "And(x, Or(y1, y2)) -> And(x, y1)" $ do
        pevalAndTerm (ssymbTerm "a") (pevalOrTerm (ssymbTerm "b") (pevalNotTerm (ssymbTerm "a")))
          `shouldBe` pevalAndTerm (ssymbTerm "a") (ssymbTerm "b")
        pevalAndTerm (pevalNotTerm (ssymbTerm "a")) (pevalOrTerm (ssymbTerm "b") (ssymbTerm "a"))
          `shouldBe` pevalAndTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "b")
        pevalAndTerm
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          (pevalOrTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
          `shouldBe` pevalAndTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b")
      it "And(Not(x), Not(y)) -> Not(Or(x, y))" $ do
        pevalAndTerm (pevalNotTerm (ssymbTerm "a")) (pevalNotTerm (ssymbTerm "b"))
          `shouldBe` pevalNotTerm (pevalOrTerm (ssymbTerm "a") (ssymbTerm "b"))
  describe "ITE" $ do
    describe "ITE construction" $ do
      it "ITE with concrete condition" $ do
        pevalITETerm (concTerm True) (ssymbTerm "a" :: Term Integer) (ssymbTerm "b")
          `shouldBe` ssymbTerm "a"
        pevalITETerm (concTerm False) (ssymbTerm "a" :: Term Integer) (ssymbTerm "b")
          `shouldBe` ssymbTerm "b"
      it "ITE with same branches" $ do
        pevalITETerm (ssymbTerm "c") (ssymbTerm "a" :: Term Integer) (ssymbTerm "a")
          `shouldBe` ssymbTerm "a"
      it "ITE with both not" $ do
        pevalITETerm (ssymbTerm "c") (pevalNotTerm $ ssymbTerm "a") (pevalNotTerm $ ssymbTerm "b")
          `shouldBe` pevalNotTerm (pevalITETerm (ssymbTerm "c") (ssymbTerm "a") (ssymbTerm "b"))
      it "ITE with not in condition" $ do
        pevalITETerm (pevalNotTerm $ ssymbTerm "c") (ssymbTerm "a" :: Term Integer) (ssymbTerm "b")
          `shouldBe` pevalITETerm (ssymbTerm "c") (ssymbTerm "b") (ssymbTerm "a")
      it "ITE with all arguments as ITE with same conditions" $ do
        pevalITETerm
          (pevalITETerm (ssymbTerm "a") (ssymbTerm "b") (ssymbTerm "c"))
          (pevalITETerm (ssymbTerm "a") (ssymbTerm "d" :: Term Integer) (ssymbTerm "e"))
          (pevalITETerm (ssymbTerm "a") (ssymbTerm "f" :: Term Integer) (ssymbTerm "g"))
          `shouldBe` pevalITETerm
            (ssymbTerm "a")
            (pevalITETerm (ssymbTerm "b") (ssymbTerm "d") (ssymbTerm "f"))
            (pevalITETerm (ssymbTerm "c") (ssymbTerm "e") (ssymbTerm "g"))
      it "ITE with true branch as ITE" $ do
        pevalITETerm
          (ssymbTerm "a")
          (pevalITETerm (ssymbTerm "a") (ssymbTerm "b" :: Term Integer) (ssymbTerm "c"))
          (ssymbTerm "d")
          `shouldBe` pevalITETerm (ssymbTerm "a") (ssymbTerm "b") (ssymbTerm "d")
        pevalITETerm
          (ssymbTerm "a")
          (pevalITETerm (ssymbTerm "b") (ssymbTerm "c" :: Term Integer) (ssymbTerm "d"))
          (ssymbTerm "c")
          `shouldBe` pevalITETerm
            (pevalOrTerm (pevalNotTerm $ ssymbTerm "a") (ssymbTerm "b"))
            (ssymbTerm "c")
            (ssymbTerm "d")
        pevalITETerm
          (ssymbTerm "a")
          (pevalITETerm (ssymbTerm "b") (ssymbTerm "c" :: Term Integer) (ssymbTerm "d"))
          (ssymbTerm "d")
          `shouldBe` pevalITETerm
            (pevalAndTerm (ssymbTerm "a") (ssymbTerm "b"))
            (ssymbTerm "c")
            (ssymbTerm "d")
      it "ITE with false branch as ITE" $ do
        pevalITETerm
          (ssymbTerm "a")
          (ssymbTerm "b")
          (pevalITETerm (ssymbTerm "a") (ssymbTerm "c" :: Term Integer) (ssymbTerm "d"))
          `shouldBe` pevalITETerm (ssymbTerm "a") (ssymbTerm "b") (ssymbTerm "d")
        pevalITETerm
          (ssymbTerm "a")
          (ssymbTerm "b")
          (pevalITETerm (ssymbTerm "c") (ssymbTerm "b" :: Term Integer) (ssymbTerm "d"))
          `shouldBe` pevalITETerm
            (pevalOrTerm (ssymbTerm "a") (ssymbTerm "c"))
            (ssymbTerm "b")
            (ssymbTerm "d")
        pevalITETerm
          (ssymbTerm "a")
          (ssymbTerm "b")
          (pevalITETerm (ssymbTerm "c") (ssymbTerm "d" :: Term Integer) (ssymbTerm "b"))
          `shouldBe` pevalITETerm
            (pevalOrTerm (ssymbTerm "a") (pevalNotTerm $ ssymbTerm "c"))
            (ssymbTerm "b")
            (ssymbTerm "d")
      it "ITE with both And" $ do
        pevalITETerm
          (ssymbTerm "a")
          (pevalAndTerm (ssymbTerm "b") (ssymbTerm "c"))
          (pevalAndTerm (ssymbTerm "b") (ssymbTerm "d"))
          `shouldBe` pevalAndTerm (ssymbTerm "b") (pevalITETerm (ssymbTerm "a") (ssymbTerm "c") (ssymbTerm "d"))
        pevalITETerm
          (ssymbTerm "a")
          (pevalAndTerm (ssymbTerm "c") (ssymbTerm "b"))
          (pevalAndTerm (ssymbTerm "b") (ssymbTerm "d"))
          `shouldBe` pevalAndTerm (ssymbTerm "b") (pevalITETerm (ssymbTerm "a") (ssymbTerm "c") (ssymbTerm "d"))
        pevalITETerm
          (ssymbTerm "a")
          (pevalAndTerm (ssymbTerm "b") (ssymbTerm "c"))
          (pevalAndTerm (ssymbTerm "d") (ssymbTerm "b"))
          `shouldBe` pevalAndTerm (ssymbTerm "b") (pevalITETerm (ssymbTerm "a") (ssymbTerm "c") (ssymbTerm "d"))
        pevalITETerm
          (ssymbTerm "a")
          (pevalAndTerm (ssymbTerm "c") (ssymbTerm "b"))
          (pevalAndTerm (ssymbTerm "d") (ssymbTerm "b"))
          `shouldBe` pevalAndTerm (ssymbTerm "b") (pevalITETerm (ssymbTerm "a") (ssymbTerm "c") (ssymbTerm "d"))
      it "ITE with left And" $ do
        pevalITETerm
          (ssymbTerm "a")
          (pevalAndTerm (ssymbTerm "b") (ssymbTerm "c"))
          (ssymbTerm "b")
          `shouldBe` pevalAndTerm (ssymbTerm "b") (pevalOrTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "c"))
        pevalITETerm
          (ssymbTerm "a")
          (pevalAndTerm (ssymbTerm "b") (ssymbTerm "c"))
          (ssymbTerm "c")
          `shouldBe` pevalAndTerm (ssymbTerm "c") (pevalOrTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "b"))
        pevalITETerm
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          (pevalAndTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
          (ssymbTerm "c")
          `shouldBe` pevalAndTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "c")
        pevalITETerm
          (ssymbTerm "a")
          (pevalAndTerm (pevalNotTerm $ ssymbTerm "a") (ssymbTerm "b"))
          (ssymbTerm "c")
          `shouldBe` pevalAndTerm (pevalNotTerm $ ssymbTerm "a") (ssymbTerm "c")
        pevalITETerm
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          (pevalAndTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
          (ssymbTerm "c")
          `shouldBe` pevalAndTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "c")
        pevalITETerm
          (ssymbTerm "a")
          (pevalAndTerm (ssymbTerm "b") (pevalNotTerm $ ssymbTerm "a"))
          (ssymbTerm "c")
          `shouldBe` pevalAndTerm (pevalNotTerm $ ssymbTerm "a") (ssymbTerm "c")
        pevalITETerm
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          (pevalAndTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
          (ssymbTerm "c")
          `shouldBe` pevalITETerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b") (ssymbTerm "c")
        pevalITETerm
          (ssymbTerm "a")
          (pevalAndTerm (ssymbTerm "a") (ssymbTerm "b"))
          (ssymbTerm "c")
          `shouldBe` pevalITETerm (ssymbTerm "a") (ssymbTerm "b") (ssymbTerm "c")
        pevalITETerm
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          (pevalAndTerm (ssymbTerm "b") (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
          (ssymbTerm "c")
          `shouldBe` pevalITETerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b") (ssymbTerm "c")
        pevalITETerm
          (ssymbTerm "a")
          (pevalAndTerm (ssymbTerm "b") (ssymbTerm "a"))
          (ssymbTerm "c")
          `shouldBe` pevalITETerm (ssymbTerm "a") (ssymbTerm "b") (ssymbTerm "c")
      it "ITE with right And" $ do
        pevalITETerm
          (ssymbTerm "a")
          (ssymbTerm "b")
          (pevalAndTerm (ssymbTerm "b") (ssymbTerm "c"))
          `shouldBe` pevalAndTerm (ssymbTerm "b") (pevalOrTerm (ssymbTerm "a") (ssymbTerm "c"))
        pevalITETerm
          (ssymbTerm "a")
          (ssymbTerm "c")
          (pevalAndTerm (ssymbTerm "b") (ssymbTerm "c"))
          `shouldBe` pevalAndTerm (ssymbTerm "c") (pevalOrTerm (ssymbTerm "a") (ssymbTerm "b"))

      it "ITE with both Or" $ do
        pevalITETerm
          (ssymbTerm "a")
          (pevalOrTerm (ssymbTerm "b") (ssymbTerm "c"))
          (pevalOrTerm (ssymbTerm "b") (ssymbTerm "d"))
          `shouldBe` pevalOrTerm (ssymbTerm "b") (pevalITETerm (ssymbTerm "a") (ssymbTerm "c") (ssymbTerm "d"))
        pevalITETerm
          (ssymbTerm "a")
          (pevalOrTerm (ssymbTerm "c") (ssymbTerm "b"))
          (pevalOrTerm (ssymbTerm "b") (ssymbTerm "d"))
          `shouldBe` pevalOrTerm (ssymbTerm "b") (pevalITETerm (ssymbTerm "a") (ssymbTerm "c") (ssymbTerm "d"))
        pevalITETerm
          (ssymbTerm "a")
          (pevalOrTerm (ssymbTerm "b") (ssymbTerm "c"))
          (pevalOrTerm (ssymbTerm "d") (ssymbTerm "b"))
          `shouldBe` pevalOrTerm (ssymbTerm "b") (pevalITETerm (ssymbTerm "a") (ssymbTerm "c") (ssymbTerm "d"))
        pevalITETerm
          (ssymbTerm "a")
          (pevalOrTerm (ssymbTerm "c") (ssymbTerm "b"))
          (pevalOrTerm (ssymbTerm "d") (ssymbTerm "b"))
          `shouldBe` pevalOrTerm (ssymbTerm "b") (pevalITETerm (ssymbTerm "a") (ssymbTerm "c") (ssymbTerm "d"))
      it "ITE with left Or" $ do
        pevalITETerm
          (ssymbTerm "a")
          (pevalOrTerm (ssymbTerm "b") (ssymbTerm "c"))
          (ssymbTerm "b")
          `shouldBe` pevalOrTerm (ssymbTerm "b") (pevalAndTerm (ssymbTerm "a") (ssymbTerm "c"))
        pevalITETerm
          (ssymbTerm "a")
          (pevalOrTerm (ssymbTerm "b") (ssymbTerm "c"))
          (ssymbTerm "c")
          `shouldBe` pevalOrTerm (ssymbTerm "c") (pevalAndTerm (ssymbTerm "a") (ssymbTerm "b"))
        pevalITETerm
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          (pevalOrTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
          (ssymbTerm "c")
          `shouldBe` pevalITETerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b") (ssymbTerm "c")
        pevalITETerm
          (ssymbTerm "a")
          (pevalOrTerm (pevalNotTerm $ ssymbTerm "a") (ssymbTerm "b"))
          (ssymbTerm "c")
          `shouldBe` pevalITETerm (ssymbTerm "a") (ssymbTerm "b") (ssymbTerm "c")
        pevalITETerm
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          (pevalOrTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
          (ssymbTerm "c")
          `shouldBe` pevalITETerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "b") (ssymbTerm "c")
        pevalITETerm
          (ssymbTerm "a")
          (pevalOrTerm (ssymbTerm "b") (pevalNotTerm $ ssymbTerm "a"))
          (ssymbTerm "c")
          `shouldBe` pevalITETerm (ssymbTerm "a") (ssymbTerm "b") (ssymbTerm "c")
        pevalITETerm
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          (pevalOrTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b"))
          (ssymbTerm "c")
          `shouldBe` pevalOrTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "c")
        pevalITETerm
          (ssymbTerm "a")
          (pevalOrTerm (ssymbTerm "a") (ssymbTerm "b"))
          (ssymbTerm "c")
          `shouldBe` pevalOrTerm (ssymbTerm "a") (ssymbTerm "c")
        pevalITETerm
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          (pevalOrTerm (ssymbTerm "b") (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)))
          (ssymbTerm "c")
          `shouldBe` pevalOrTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "c")
        pevalITETerm
          (ssymbTerm "a")
          (pevalOrTerm (ssymbTerm "b") (ssymbTerm "a"))
          (ssymbTerm "c")
          `shouldBe` pevalOrTerm (ssymbTerm "a") (ssymbTerm "c")
      it "ITE with right Or" $ do
        pevalITETerm
          (ssymbTerm "a")
          (ssymbTerm "b")
          (pevalOrTerm (ssymbTerm "b") (ssymbTerm "c"))
          `shouldBe` pevalOrTerm (ssymbTerm "b") (pevalAndTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "c"))
        pevalITETerm
          (ssymbTerm "a")
          (ssymbTerm "c")
          (pevalOrTerm (ssymbTerm "b") (ssymbTerm "c"))
          `shouldBe` pevalOrTerm (ssymbTerm "c") (pevalAndTerm (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "b"))
      it "ITE with const boolean in branches" $ do
        pevalITETerm
          (ssymbTerm "a")
          (concTerm True)
          (ssymbTerm "b")
          `shouldBe` pevalOrTerm (ssymbTerm "a") (ssymbTerm "b")
        pevalITETerm
          (ssymbTerm "a")
          (concTerm False)
          (ssymbTerm "b")
          `shouldBe` pevalAndTerm (pevalNotTerm $ ssymbTerm "a") (ssymbTerm "b")
        pevalITETerm
          (ssymbTerm "a")
          (ssymbTerm "b")
          (concTerm True)
          `shouldBe` pevalOrTerm (pevalNotTerm $ ssymbTerm "a") (ssymbTerm "b")
        pevalITETerm
          (ssymbTerm "a")
          (ssymbTerm "b")
          (concTerm False)
          `shouldBe` pevalAndTerm (ssymbTerm "a") (ssymbTerm "b")
      it "ITE with condition equal to some branch" $ do
        pevalITETerm
          (ssymbTerm "a")
          (ssymbTerm "a")
          (ssymbTerm "b")
          `shouldBe` pevalOrTerm (ssymbTerm "a") (ssymbTerm "b")
        pevalITETerm
          (ssymbTerm "a")
          (ssymbTerm "b")
          (ssymbTerm "a")
          `shouldBe` pevalAndTerm (ssymbTerm "a") (ssymbTerm "b")
      it "ITE with left Not" $ do
        pevalITETerm (ssymbTerm "a") (pevalNotTerm (ssymbTerm "a")) (ssymbTerm "b")
          `shouldBe` pevalAndTerm (pevalNotTerm $ ssymbTerm "a") (ssymbTerm "b")
      it "ITE with right Not" $ do
        pevalITETerm (ssymbTerm "a") (ssymbTerm "b") (pevalNotTerm (ssymbTerm "a"))
          `shouldBe` pevalOrTerm (pevalNotTerm $ ssymbTerm "a") (ssymbTerm "b")
      it "ITE with left Not And" $ do
        pevalITETerm
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          (pevalNotTerm (pevalAndTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b")))
          (ssymbTerm "c")
          `shouldBe` pevalOrTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "c")
        pevalITETerm
          (ssymbTerm "a")
          (pevalNotTerm (pevalAndTerm (pevalNotTerm $ ssymbTerm "a") (ssymbTerm "b")))
          (ssymbTerm "c")
          `shouldBe` pevalOrTerm (ssymbTerm "a") (ssymbTerm "c")
        pevalITETerm
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          (pevalNotTerm (pevalAndTerm (ssymbTerm "b") (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2))))
          (ssymbTerm "c")
          `shouldBe` pevalOrTerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (ssymbTerm "c")
        pevalITETerm
          (ssymbTerm "a")
          (pevalNotTerm (pevalAndTerm (ssymbTerm "b") (pevalNotTerm $ ssymbTerm "a")))
          (ssymbTerm "c")
          `shouldBe` pevalOrTerm (ssymbTerm "a") (ssymbTerm "c")
        pevalITETerm
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          (pevalNotTerm (pevalAndTerm (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2)) (ssymbTerm "b")))
          (ssymbTerm "c")
          `shouldBe` pevalITETerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (pevalNotTerm $ ssymbTerm "b") (ssymbTerm "c")
        pevalITETerm
          (ssymbTerm "a")
          (pevalNotTerm (pevalAndTerm (ssymbTerm "a") (ssymbTerm "b")))
          (ssymbTerm "c")
          `shouldBe` pevalITETerm (ssymbTerm "a") (pevalNotTerm $ ssymbTerm "b") (ssymbTerm "c")
        pevalITETerm
          (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1))
          (pevalNotTerm (pevalAndTerm (ssymbTerm "b") (pevalNotEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 2))))
          (ssymbTerm "c")
          `shouldBe` pevalITETerm (pevalEqvTerm (ssymbTerm "a" :: Term Integer) (concTerm 1)) (pevalNotTerm $ ssymbTerm "b") (ssymbTerm "c")
        pevalITETerm
          (ssymbTerm "a")
          (pevalNotTerm (pevalAndTerm (ssymbTerm "b") (ssymbTerm "a")))
          (ssymbTerm "c")
          `shouldBe` pevalITETerm (ssymbTerm "a") (pevalNotTerm $ ssymbTerm "b") (ssymbTerm "c")
  describe "Imply" $ do
    it "pevalImplyTerm should work" $ do
      ssymbTerm "a"
        `pevalImplyTerm` ssymbTerm "b"
        `shouldBe` pevalOrTerm (pevalNotTerm $ ssymbTerm "a") (ssymbTerm "b")
  describe "Xor" $ do
    it "pevalXorTerm should work" $ do
      ssymbTerm "a"
        `pevalXorTerm` ssymbTerm "b"
        `shouldBe` pevalOrTerm
          (pevalAndTerm (pevalNotTerm $ ssymbTerm "a") (ssymbTerm "b"))
          (pevalAndTerm (ssymbTerm "a") (pevalNotTerm $ ssymbTerm "b"))
