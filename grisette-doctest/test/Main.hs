module Main where

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = do
  core <- glob "../grisette-core/src/**/*.hs"
  symir <- glob "../grisette-symir/src/**/*.hs"
  lib <- glob "../grisette-lib/src/**/*.hs"
  sbv <- glob "../grisette-backend-sbv/src/**/*.hs"
  grisette <- glob "../grisette/src/**/*.hs"
  doctest $ core ++ symir
  doctest lib
  doctest sbv
  doctest grisette
