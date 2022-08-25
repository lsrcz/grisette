{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Pizza.IR.SymPrim.Data.Prim.TabularFuncSpec where

import Pizza.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Pizza.IR.SymPrim.Data.Prim.InternedTerm.Term
import Pizza.IR.SymPrim.Data.Prim.PartialEval.Bool
import Pizza.IR.SymPrim.Data.Prim.PartialEval.TabularFunc
import Pizza.IR.SymPrim.Data.TabularFunc
import Test.Hspec

spec :: Spec
spec = do
  describe "ApplyF" $ do
    it "ApplyF with concrete" $ do
      let f :: Integer =-> Integer =
            TabularFunc [(1, 2), (3, 4)] 5
      pevalTabularFuncApplyTerm (concTerm f) (concTerm 0) `shouldBe` concTerm 5
      pevalTabularFuncApplyTerm (concTerm f) (concTerm 1) `shouldBe` concTerm 2
      pevalTabularFuncApplyTerm (concTerm f) (concTerm 2) `shouldBe` concTerm 5
      pevalTabularFuncApplyTerm (concTerm f) (concTerm 3) `shouldBe` concTerm 4
      pevalTabularFuncApplyTerm (concTerm f) (concTerm 4) `shouldBe` concTerm 5
    it "ApplyF with concrete function" $ do
      let f :: Integer =-> Integer =
            TabularFunc [(1, 2), (3, 4)] 5
      pevalTabularFuncApplyTerm (concTerm f) (ssymbTerm "b")
        `shouldBe` pevalITETerm
          (pevalEqvTerm (concTerm 1 :: Term Integer) (ssymbTerm "b"))
          (concTerm 2)
          ( pevalITETerm
              (pevalEqvTerm (concTerm 3 :: Term Integer) (ssymbTerm "b"))
              (concTerm 4)
              (concTerm 5)
          )
    it "ApplyF with symbolic" $ do
      pevalTabularFuncApplyTerm (ssymbTerm "f" :: Term (Integer =-> Integer)) (ssymbTerm "a")
        `shouldBe` tabularFuncApplyTerm
          (ssymbTerm "f" :: Term (Integer =-> Integer))
          (ssymbTerm "a" :: Term Integer)
