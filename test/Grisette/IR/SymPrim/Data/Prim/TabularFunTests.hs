{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.IR.SymPrim.Data.Prim.TabularFunTests (tabularFunTests) where

import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
  ( conTerm,
    ssymTerm,
    tabularFunApplyTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term (Term)
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bool
  ( pevalEqvTerm,
    pevalITETerm,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.TabularFun
  ( pevalTabularFunApplyTerm,
  )
import Grisette.IR.SymPrim.Data.TabularFun
  ( type (=->) (TabularFun),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

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
