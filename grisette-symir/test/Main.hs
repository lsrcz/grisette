module Main where

import qualified Grisette.IR.SymPrim.Data.BVTests
import qualified Grisette.IR.SymPrim.Data.Prim.BVTests
import Grisette.IR.SymPrim.Data.Prim.BitsTests
import Grisette.IR.SymPrim.Data.Prim.BoolTests
import Grisette.IR.SymPrim.Data.Prim.IntegerTests
import Grisette.IR.SymPrim.Data.Prim.ModelTests
import Grisette.IR.SymPrim.Data.Prim.NumTests
import qualified Grisette.IR.SymPrim.Data.Prim.TabularFuncTests
import Grisette.IR.SymPrim.Data.SymPrimTests
import qualified Grisette.IR.SymPrim.Data.TabularFuncTests
import Test.Tasty
import Test.Tasty.Ingredients
import qualified Test.Tasty.Ingredients.ConsoleReporter as ConsoleReporter
import qualified Test.Tasty.Runners.Reporter as Reporter

main :: IO ()
main = defaultMainWithIngredients [composeReporters Reporter.ingredient ConsoleReporter.consoleTestReporter] tests

tests :: TestTree
tests =
  testGroup
    "grisette-symir"
    [ testGroup
        "Grisette.IR.SymPrim.Data"
        [ testGroup
            "Prim"
            [ bitsTests,
              boolTests,
              Grisette.IR.SymPrim.Data.Prim.BVTests.bvTests,
              integerTests,
              modelTests,
              numTests,
              Grisette.IR.SymPrim.Data.Prim.TabularFuncTests.tabularFuncTests
            ],
          Grisette.IR.SymPrim.Data.BVTests.bvTests,
          symPrimTests,
          Grisette.IR.SymPrim.Data.TabularFuncTests.tabularFuncTests
        ]
    ]
