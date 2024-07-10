{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.SymPrim.Prim.GeneralFunTests (generalFunTests) where

import Grisette
  ( EvalSym (evalSym),
    ExtractSym (extractSym),
    ModelRep (buildModel),
    ModelValuePair ((::=)),
    Solvable (con),
    SymInteger,
    SymbolSetRep (buildSymbolSet),
    TypedAnySymbol,
    (-->),
    type (-->),
    type (-~>),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

generalFunTests :: Test
generalFunTests =
  testGroup
    "GeneralFun"
    [ testCase "EvalSym" $ do
        let x :: Integer --> Integer =
              "a" --> "a" + "b"
        let xe :: Integer --> Integer =
              "a" --> "a" + 1
        let m = buildModel ("b" ::= (1 :: Integer), "a" ::= (2 :: Integer))
        evalSym True m x @?= xe,
      testCase "EvalSym nested" $ do
        let x :: Integer --> Integer --> Integer =
              "a" --> (con $ "b" --> "a" + "b" + "c" :: SymInteger -~> SymInteger)
        let xe :: Integer --> Integer --> Integer =
              "a" --> (con $ "b" --> "a" + "b" + 3 :: SymInteger -~> SymInteger)
        let m =
              buildModel
                ( "b" ::= (1 :: Integer),
                  "a" ::= (2 :: Integer),
                  "c" ::= (3 :: Integer)
                )
        evalSym True m x @?= xe,
      testCase "ExtractSym" $ do
        let x0 :: Integer --> Integer = "a" --> "a" + "c"
        let x :: Integer --> Integer --> Integer =
              "a" --> (con $ "b" --> "a" + "b" + "c" :: SymInteger -~> SymInteger)
        extractSym x0 @?= buildSymbolSet ("c" :: TypedAnySymbol Integer)
        extractSym x @?= buildSymbolSet ("c" :: TypedAnySymbol Integer)
    ]
