module Main (main) where

import Test.DocTest (doctest)

main :: IO ()
main = do
  doctest ["-isrc", "--fast", "src"]
