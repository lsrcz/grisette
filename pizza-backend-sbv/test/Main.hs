module Main where

import Pizza.Backend.SBV.Data.SMT.CEGISTests
import Pizza.Backend.SBV.Data.SMT.LoweringTests
import Pizza.Backend.SBV.Data.SMT.TermRewritingTests
import Test.Tasty
import Test.Tasty.Ingredients
import qualified Test.Tasty.Ingredients.ConsoleReporter as ConsoleReporter
import qualified Test.Tasty.Runners.Reporter as Reporter

main :: IO ()
main = defaultMainWithIngredients [composeReporters Reporter.ingredient ConsoleReporter.consoleTestReporter] tests

tests :: TestTree
tests =
  testGroup
    "Pizza.Backend.SBV.Data.SMT"
    [ cegisTests,
      loweringTests,
      termRewritingTests
    ]
