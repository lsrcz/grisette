{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.SymPrim.Prim.TabularFunTests (tabularFunTests) where

import Grisette.SymPrim.Prim.Term
  ( PEvalApplyTerm (pevalApplyTerm),
    SupportedPrim (pevalITETerm),
    Term,
    applyTerm,
    conTerm,
    pevalEqTerm,
    ssymTerm,
  )
import Grisette.SymPrim.TabularFun
  ( type (=->) (TabularFun),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@=?))

tabularFunTests :: Test
tabularFunTests =
  testGroup
    "TabularFun"
    [ testGroup
        "Apply"
        [ testCase "On concrete" $ do
            let f :: Integer =-> Integer =
                  TabularFun [(1, 2), (3, 4)] 5
            pevalApplyTerm (conTerm f) (conTerm 0) @=? conTerm 5
            pevalApplyTerm (conTerm f) (conTerm 1) @=? conTerm 2
            pevalApplyTerm (conTerm f) (conTerm 2) @=? conTerm 5
            pevalApplyTerm (conTerm f) (conTerm 3) @=? conTerm 4
            pevalApplyTerm (conTerm f) (conTerm 4) @=? conTerm 5,
          testCase "On concrete function" $ do
            let f :: Integer =-> Integer =
                  TabularFun [(1, 2), (3, 4)] 5
            pevalApplyTerm (conTerm f) (ssymTerm "b")
              @=? pevalITETerm
                (pevalEqTerm (conTerm 1 :: Term Integer) (ssymTerm "b"))
                (conTerm 2)
                ( pevalITETerm
                    (pevalEqTerm (conTerm 3 :: Term Integer) (ssymTerm "b"))
                    (conTerm 4)
                    (conTerm 5)
                ),
          testCase "On symbolic" $ do
            pevalApplyTerm (ssymTerm "f" :: Term (Integer =-> Integer)) (ssymTerm "a")
              @=? applyTerm
                (ssymTerm "f" :: Term (Integer =-> Integer))
                (ssymTerm "a" :: Term Integer)
        ]
    ]
