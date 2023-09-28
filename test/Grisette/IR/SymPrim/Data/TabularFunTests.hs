{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.IR.SymPrim.Data.TabularFunTests (tabularFunTests) where

import Grisette.Core.Data.Class.Function (Function ((#)))
import Grisette.IR.SymPrim.Data.TabularFun
  ( type (=->) (TabularFun),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@=?))

tabularFunTests :: Test
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
