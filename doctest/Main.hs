module Main (main) where

import Test.DocTest (doctest)

main :: IO ()
main = do
  doctest
    [ "-isrc",
      "--fast",
      "-XBinaryLiterals",
      "-XDataKinds",
      "-XDeriveAnyClass",
      "-XDeriveGeneric",
      "-XDeriveLift",
      "-XDerivingStrategies",
      "-XDerivingVia",
      "-XFlexibleContexts",
      "-XFlexibleInstances",
      "-XFunctionalDependencies",
      "-XLambdaCase",
      "-XMonoLocalBinds",
      "-XMultiParamTypeClasses",
      "-XOverloadedStrings",
      "-XScopedTypeVariables",
      "-XStandaloneDeriving",
      "-XTemplateHaskell",
      "-XTypeApplications",
      "-XTypeOperators",
      "-XUndecidableInstances",
      "-Wno-unrecognised-warning-flags",
      "-Wno-x-partial",
      "-Wno-deriving-defaults",
      "src"
    ]
