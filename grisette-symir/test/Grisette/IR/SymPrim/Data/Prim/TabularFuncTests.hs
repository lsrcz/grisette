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
            pevalTabularFuncApplyTerm (concTerm f) (concTerm 0) @=? concTerm 5
            pevalTabularFuncApplyTerm (concTerm f) (concTerm 1) @=? concTerm 2
            pevalTabularFuncApplyTerm (concTerm f) (concTerm 2) @=? concTerm 5
            pevalTabularFuncApplyTerm (concTerm f) (concTerm 3) @=? concTerm 4
            pevalTabularFuncApplyTerm (concTerm f) (concTerm 4) @=? concTerm 5,
          testCase "On concrete function" $ do
            let f :: Integer =-> Integer =
                  TabularFunc [(1, 2), (3, 4)] 5
            pevalTabularFuncApplyTerm (concTerm f) (ssymbTerm "b")
              @=? pevalITETerm
                (pevalEqvTerm (concTerm 1 :: Term Integer) (ssymbTerm "b"))
                (concTerm 2)
                ( pevalITETerm
                    (pevalEqvTerm (concTerm 3 :: Term Integer) (ssymbTerm "b"))
                    (concTerm 4)
                    (concTerm 5)
                ),
          testCase "On symbolic" $ do
            pevalTabularFuncApplyTerm (ssymbTerm "f" :: Term (Integer =-> Integer)) (ssymbTerm "a")
              @=? tabularFuncApplyTerm
                (ssymbTerm "f" :: Term (Integer =-> Integer))
                (ssymbTerm "a" :: Term Integer)
        ]
    ]
