module Main where

import qualified Pizza.IR.SymPrim.Data.BVTests
import qualified Pizza.IR.SymPrim.Data.Prim.BVTests
import Pizza.IR.SymPrim.Data.Prim.BitsTests
import Pizza.IR.SymPrim.Data.Prim.BoolTests
import Pizza.IR.SymPrim.Data.Prim.IntegerTests
import Pizza.IR.SymPrim.Data.Prim.ModelTests
import Pizza.IR.SymPrim.Data.Prim.NumTests
import qualified Pizza.IR.SymPrim.Data.Prim.TabularFuncTests
import Pizza.IR.SymPrim.Data.SymPrimTests
import qualified Pizza.IR.SymPrim.Data.TabularFuncTests
import Test.Tasty
import Test.Tasty.Ingredients
import qualified Test.Tasty.Ingredients.ConsoleReporter as ConsoleReporter
import qualified Test.Tasty.Runners.Reporter as Reporter

main :: IO ()
main = defaultMainWithIngredients [composeReporters Reporter.ingredient ConsoleReporter.consoleTestReporter] tests

tests :: TestTree
tests =
  testGroup
    "pizza-symir"
    [ testGroup
        "Pizza.IR.SymPrim.Data"
        [ testGroup
            "Prim"
            [ bitsTests,
              boolTests,
              Pizza.IR.SymPrim.Data.Prim.BVTests.bvTests,
              integerTests,
              modelTests,
              numTests,
              Pizza.IR.SymPrim.Data.Prim.TabularFuncTests.tabularFuncTests
            ],
          Pizza.IR.SymPrim.Data.BVTests.bvTests,
          symPrimTests,
          Pizza.IR.SymPrim.Data.TabularFuncTests.tabularFuncTests
        ]
    ]
