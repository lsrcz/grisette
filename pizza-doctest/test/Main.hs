module Main where

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = do
  core <- glob "../pizza-core/src/**/*.hs"
  symir <- glob "../pizza-symir/src/**/*.hs"
  lib <- glob "../pizza-lib/src/**/*.hs"
  sbv <- glob "../pizza-backend-sbv/src/**/*.hs"
  pizza <- glob "../pizza/src/**/*.hs"
  doctest $ core ++ symir
  doctest lib
  doctest sbv
  doctest pizza
