module Main where

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
import Test.Tasty
import Test.Tasty.Ingredients
import qualified Test.Tasty.Ingredients.ConsoleReporter as ConsoleReporter
import qualified Test.Tasty.Runners.Reporter as Reporter

main :: IO ()
main = defaultMainWithIngredients [composeReporters Reporter.ingredient ConsoleReporter.consoleTestReporter] tests

tests :: TestTree
tests =
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
