{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.IR.SymPrim.Data.Prim.TabularFuncTests where

import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bool
import Grisette.IR.SymPrim.Data.Prim.PartialEval.TabularFunc
import Grisette.IR.SymPrim.Data.TabularFunc
import Test.Tasty
import Test.Tasty.HUnit

tabularFuncTests :: TestTree
tabularFuncTests =
  testGroup
    "TabularFuncTests"
    [ testGroup
        "ApplyF"
        [ testCase "On concrete" $ do
            let f :: Integer =-> Integer =
                  TabularFunc [(1, 2), (3, 4)] 5
            pevalTabularFuncApplyTerm (conTerm f) (conTerm 0) @=? conTerm 5
            pevalTabularFuncApplyTerm (conTerm f) (conTerm 1) @=? conTerm 2
            pevalTabularFuncApplyTerm (conTerm f) (conTerm 2) @=? conTerm 5
            pevalTabularFuncApplyTerm (conTerm f) (conTerm 3) @=? conTerm 4
            pevalTabularFuncApplyTerm (conTerm f) (conTerm 4) @=? conTerm 5,
          testCase "On concrete function" $ do
            let f :: Integer =-> Integer =
                  TabularFunc [(1, 2), (3, 4)] 5
            pevalTabularFuncApplyTerm (conTerm f) (ssymTerm "b")
              @=? pevalITETerm
                (pevalEqvTerm (conTerm 1 :: Term Integer) (ssymTerm "b"))
                (conTerm 2)
                ( pevalITETerm
                    (pevalEqvTerm (conTerm 3 :: Term Integer) (ssymTerm "b"))
                    (conTerm 4)
                    (conTerm 5)
                ),
          testCase "On symbolic" $ do
            pevalTabularFuncApplyTerm (ssymTerm "f" :: Term (Integer =-> Integer)) (ssymTerm "a")
              @=? tabularFuncApplyTerm
                (ssymTerm "f" :: Term (Integer =-> Integer))
                (ssymTerm "a" :: Term Integer)
        ]
    ]
