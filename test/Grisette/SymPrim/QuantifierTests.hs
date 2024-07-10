{-# LANGUAGE OverloadedStrings #-}

module Grisette.SymPrim.QuantifierTests (quantifierTests) where

import Grisette
  ( LogicalOp (symImplies),
    ModelOps (isEmptyModel),
    SymEq ((.==)),
    SymOrd ((.>)),
    solve,
    z3,
  )
import Grisette.Internal.SymPrim.Quantifier (existsSym, forallSym)
import Grisette.Internal.SymPrim.SymInteger (SymInteger)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool)

quantifierTests :: Test
quantifierTests =
  testGroup
    "Quantifier"
    [ testCase "Basic" $ do
        let l@[x, y] = ["x", "y"] :: [SymInteger]
        r <- solve z3 $ forallSym l $ (x .== y) `symImplies` (x + 1 .== y + 1)
        case r of
          Left err -> error $ show err
          Right mo ->
            assertBool "no elements should be in the model" $ isEmptyModel mo,
      testCase "Basic2" $ do
        let [x, y] = ["x", "y"] :: [SymInteger]
        r <- solve z3 $ forallSym x $ existsSym y $ x .> y
        case r of
          Left err -> error $ show err
          Right mo ->
            assertBool "no elements should be in the model" $ isEmptyModel mo
    ]
