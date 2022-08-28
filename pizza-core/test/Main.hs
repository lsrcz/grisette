module Main where

import Pizza.Core.Control.ExceptionTests
import Pizza.Core.Control.Monad.UnionMBaseTests
import Pizza.Core.Data.Class.BoolTests
import Pizza.Core.Data.Class.EvaluateSymTests
import Pizza.Core.Data.Class.ExtractSymbolicsTests
import Pizza.Core.Data.Class.GenSymTests
import Pizza.Core.Data.Class.MergeableTests
import Pizza.Core.Data.Class.SEqTests
import Pizza.Core.Data.Class.SOrdTests
import Pizza.Core.Data.Class.SimpleMergeableTests
import Pizza.Core.Data.Class.ToConTests
import Pizza.Core.Data.Class.ToSymTests
import Pizza.Core.Data.Class.UnionLikeTests
import Pizza.Core.Data.UnionBaseTests
import Pizza.Lib.Control.Monad.ExceptTests
import Pizza.Lib.Control.Monad.TransTests
import Pizza.Lib.Control.MonadTests
import Pizza.Lib.Data.FoldableTests
import Pizza.Lib.Data.TraversableTests
import Test.Tasty
import Test.Tasty.Ingredients
import qualified Test.Tasty.Ingredients.ConsoleReporter as ConsoleReporter
import qualified Test.Tasty.Runners.Reporter as Reporter

main :: IO ()
main = defaultMainWithIngredients [composeReporters Reporter.ingredient ConsoleReporter.consoleTestReporter] tests

tests :: TestTree
tests =
  testGroup
    "pizza-core"
    [ testGroup
        "Pizza"
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
                      evaluateSymTests,
                      extractSymbolicsTests,
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
