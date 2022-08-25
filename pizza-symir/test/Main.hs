module Main where

import Pizza.TestUtils.Runner
import qualified Spec

main :: IO ()
main = runPizzaTests "symir" Spec.spec
