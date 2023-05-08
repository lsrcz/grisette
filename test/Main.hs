module Main where

import Grisette.Backend.SBV.Data.SMT.CEGISTests
import Grisette.Backend.SBV.Data.SMT.LoweringTests
import Grisette.Backend.SBV.Data.SMT.TermRewritingTests
import Grisette.Core.Control.Monad.UnionMTests
import qualified Grisette.Core.Data.BVTests
import Grisette.Core.Data.DynBVTests
import qualified Grisette.IR.SymPrim.Data.Prim.BVTests
import Grisette.IR.SymPrim.Data.Prim.BitsTests
import Grisette.IR.SymPrim.Data.Prim.BoolTests
import Grisette.IR.SymPrim.Data.Prim.IntegralTests
import Grisette.IR.SymPrim.Data.Prim.ModelTests
import Grisette.IR.SymPrim.Data.Prim.NumTests
import qualified Grisette.IR.SymPrim.Data.Prim.TabularFunTests
import Grisette.IR.SymPrim.Data.SymPrimTests
import qualified Grisette.IR.SymPrim.Data.TabularFunTests
import Test.Tasty
import Test.Tasty.Ingredients
import qualified Test.Tasty.Ingredients.ConsoleReporter as ConsoleReporter
import qualified Test.Tasty.Runners.Reporter as Reporter

main :: IO ()
main = defaultMainWithIngredients [composeReporters Reporter.ingredient ConsoleReporter.consoleTestReporter] tests

tests :: TestTree
tests =
  testGroup
    "grisette"
    [ coreTests,
      irTests,
      sbvTests
    ]

coreTests :: TestTree
coreTests =
  testGroup
    "Grisette.Core.Control.Monad.UnionM"
    [ unionMTests,
      Grisette.Core.Data.BVTests.bvTests
    ]

irTests :: TestTree
irTests =
  testGroup
    "grisette-symir"
    [ testGroup
        "Grisette.IR.SymPrim.Data"
        [ testGroup
            "Prim"
            [ bitsTests,
              boolTests,
              Grisette.IR.SymPrim.Data.Prim.BVTests.bvTests,
              dynBVTests,
              integralTests,
              modelTests,
              numTests,
              Grisette.IR.SymPrim.Data.Prim.TabularFunTests.tabularFunTests
            ],
          symPrimTests,
          Grisette.IR.SymPrim.Data.TabularFunTests.tabularFunTests
        ]
    ]

sbvTests :: TestTree
sbvTests =
  testGroup
    "Grisette.Backend.SBV.Data.SMT"
    [ cegisTests,
      loweringTests,
      termRewritingTests
    ]
