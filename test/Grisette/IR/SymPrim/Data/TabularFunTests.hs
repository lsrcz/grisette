{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.IR.SymPrim.Data.TabularFunTests (tabularFunTests) where

import Grisette.Core.Data.Class.Function
import Grisette.IR.SymPrim.Data.TabularFun
import Test.Tasty
import Test.Tasty.HUnit

tabularFunTests :: TestTree
tabularFunTests =
  testGroup
    "TabularFunTests"
    [ testCase "Tabular application" $ do
        let f :: Integer =-> Integer = TabularFun [(1, 2), (3, 4)] 5
        (f # 0) @=? 5
        (f # 1) @=? 2
        (f # 2) @=? 5
        (f # 3) @=? 4
        (f # 4) @=? 5
    ]
