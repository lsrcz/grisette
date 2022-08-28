{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Pizza.IR.SymPrim.Data.TabularFuncTests where

import Pizza.Core.Data.Class.Function
import Pizza.IR.SymPrim.Data.TabularFunc
import Test.Tasty
import Test.Tasty.HUnit

tabularFuncTests :: TestTree
tabularFuncTests =
  testGroup
    "TabularFuncTests"
    [ testCase "Tabular application" $ do
        let f :: Integer =-> Integer = TabularFunc [(1, 2), (3, 4)] 5
        (f # 0) @=? 5
        (f # 1) @=? 2
        (f # 2) @=? 5
        (f # 3) @=? 4
        (f # 4) @=? 5
    ]
