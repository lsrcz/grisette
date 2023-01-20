module Main where

{-
import Grisette.Core.Control.ExceptionTests
import Grisette.Core.Control.Monad.UnionMBaseTests
import Grisette.Core.Data.Class.BoolTests
import Grisette.Core.Data.Class.EvaluateSymTests
import Grisette.Core.Data.Class.ExtractSymbolicsTests
import Grisette.Core.Data.Class.GenSymTests
import Grisette.Core.Data.Class.MergeableTests
import Grisette.Core.Data.Class.SEqTests
import Grisette.Core.Data.Class.SOrdTests
import Grisette.Core.Data.Class.SimpleMergeableTests
import Grisette.Core.Data.Class.ToConTests
import Grisette.Core.Data.Class.ToSymTests
import Grisette.Core.Data.Class.UnionLikeTests
import Grisette.Core.Data.UnionBaseTests
import Grisette.Lib.Control.Monad.ExceptTests
import Grisette.Lib.Control.Monad.TransTests
import Grisette.Lib.Control.MonadTests
import Grisette.Lib.Data.FoldableTests
import Grisette.Lib.Data.TraversableTests
-}

import qualified Grisette.IR.SymPrim.Data.BVTests
import qualified Grisette.IR.SymPrim.Data.Prim.BVTests
import Grisette.IR.SymPrim.Data.Prim.BitsTests
import Grisette.IR.SymPrim.Data.Prim.BoolTests
import Grisette.IR.SymPrim.Data.Prim.IntegerTests
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
    [ irTests
    ]

{-
coreTests :: TestTree
coreTests =
  testGroup
    "grisette-core"
    [ testGroup
        "Grisette"
        [ testGroup
            "Core"
            [ testGroup
                "Control"
                [ testGroup
                    "Monad"
                    [ unionMBaseTests
                    ],
                  exceptionTests
                ],
              testGroup
                "Data"
                [ testGroup
                    "Class"
                    [ boolTests,
                      gevaluateSymTests,
                      gextractSymbolicsTests,
                      genSymTests,
                      mergeableTests,
                      seqTests,
                      simpleMergeableTests,
                      sordTests,
                      toConTests,
                      toSymTests,
                      unionLikeTests
                    ],
                  unionBaseTests
                ]
            ],
          testGroup
            "Lib"
            [ testGroup
                "Control"
                [ testGroup
                    "Monad"
                    [ monadExceptFunctionTests,
                      monadTransFunctionTests
                    ],
                  monadFunctionTests
                ],
              testGroup
                "Data"
                [ foldableFunctionTests,
                  traversableFunctionTests
                ]
            ]
        ]
    ]
-}

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
              integerTests,
              modelTests,
              numTests,
              Grisette.IR.SymPrim.Data.Prim.TabularFunTests.tabularFunTests
            ],
          Grisette.IR.SymPrim.Data.BVTests.bvTests,
          symPrimTests,
          Grisette.IR.SymPrim.Data.TabularFunTests.tabularFunTests
        ]
    ]
