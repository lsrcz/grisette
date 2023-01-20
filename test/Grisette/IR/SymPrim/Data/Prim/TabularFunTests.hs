{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.IR.SymPrim.Data.Prim.TabularFunTests where

import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bool
import Grisette.IR.SymPrim.Data.Prim.PartialEval.TabularFun
import Grisette.IR.SymPrim.Data.TabularFun
import Test.Tasty
import Test.Tasty.HUnit

tabularFunTests :: TestTree
tabularFunTests =
  testGroup
    "TabularFunTests"
    [ testGroup
        "ApplyF"
        [ testCase "On concrete" $ do
            let f :: Integer =-> Integer =
                  TabularFun [(1, 2), (3, 4)] 5
            pevalTabularFunApplyTerm (conTerm f) (conTerm 0) @=? conTerm 5
            pevalTabularFunApplyTerm (conTerm f) (conTerm 1) @=? conTerm 2
            pevalTabularFunApplyTerm (conTerm f) (conTerm 2) @=? conTerm 5
            pevalTabularFunApplyTerm (conTerm f) (conTerm 3) @=? conTerm 4
            pevalTabularFunApplyTerm (conTerm f) (conTerm 4) @=? conTerm 5,
          testCase "On concrete function" $ do
            let f :: Integer =-> Integer =
                  TabularFun [(1, 2), (3, 4)] 5
            pevalTabularFunApplyTerm (conTerm f) (ssymTerm "b")
              @=? pevalITETerm
                (pevalEqvTerm (conTerm 1 :: Term Integer) (ssymTerm "b"))
                (conTerm 2)
                ( pevalITETerm
                    (pevalEqvTerm (conTerm 3 :: Term Integer) (ssymTerm "b"))
                    (conTerm 4)
                    (conTerm 5)
                ),
          testCase "On symbolic" $ do
            pevalTabularFunApplyTerm (ssymTerm "f" :: Term (Integer =-> Integer)) (ssymTerm "a")
              @=? tabularFunApplyTerm
                (ssymTerm "f" :: Term (Integer =-> Integer))
                (ssymTerm "a" :: Term Integer)
        ]
    ]
