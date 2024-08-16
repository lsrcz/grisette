{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.SymPrim.GeneralFunTests (generalFunTests) where

import Grisette
  ( EvalSym (evalSym),
    ExtractSym (extractSym),
    ITEOp (symIte),
    ModelRep (buildModel),
    ModelValuePair ((::=)),
    Solvable (con),
    SymbolSetRep (buildSymbolSet),
    TypedAnySymbol,
    TypedSymbol (TypedSymbol),
    indexed,
    (-->),
    type (-->),
  )
import Grisette.Internal.SymPrim.GeneralFun (type (-->) (GeneralFun))
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( PEvalNumTerm (pevalAddNumTerm),
    conTerm,
    isymTerm,
    iteTerm,
    ssymTerm,
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
              "a" --> con $ "b" --> "a" + "b" + "c"
        let xe :: Integer --> Integer --> Integer =
              "a" --> con $ "b" --> "a" + "b" + 3
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
              "a" --> con $ "b" --> "a" + "b" + "c"
        extractSym x0 @?= buildSymbolSet ("c" :: TypedAnySymbol Integer)
        extractSym x @?= buildSymbolSet ("c" :: TypedAnySymbol Integer),
      testGroup
        "ITEOp"
        [ testCase "basic" $ do
            let x0 :: Integer --> Integer --> Integer =
                  "a" --> con ("c" --> "a" + "c" + "b")
            let x :: Integer --> Integer --> Integer =
                  "a" --> con ("b" --> "a" + "b" + "c")
            let expected =
                  GeneralFun (TypedSymbol $ indexed "arg" 2) $
                    conTerm $
                      GeneralFun (TypedSymbol $ indexed "arg" 1) $
                        iteTerm
                          (ssymTerm "x")
                          ( pevalAddNumTerm
                              ( pevalAddNumTerm
                                  (isymTerm "arg" 2)
                                  (isymTerm "arg" 1)
                              )
                              (ssymTerm "b")
                          )
                          ( pevalAddNumTerm
                              ( pevalAddNumTerm
                                  (isymTerm "arg" 2)
                                  (isymTerm "arg" 1)
                              )
                              (ssymTerm "c")
                          )
            symIte "x" x0 x @?= expected,
          testCase "interfering names" $ do
            let x0 :: Integer --> Integer --> Integer =
                  "a" --> con ("b" --> "a" + "b" + "c")
            let x :: Integer --> Integer --> Integer =
                  "b" --> con ("a" --> "a" + "b" + "c")
            let expected =
                  GeneralFun (TypedSymbol $ indexed "arg" 2) $
                    conTerm $
                      GeneralFun (TypedSymbol $ indexed "arg" 1) $
                        iteTerm
                          (ssymTerm "x")
                          ( pevalAddNumTerm
                              ( pevalAddNumTerm
                                  (isymTerm "arg" 2)
                                  (isymTerm "arg" 1)
                              )
                              (ssymTerm "c")
                          )
                          ( pevalAddNumTerm
                              ( pevalAddNumTerm
                                  (isymTerm "arg" 1)
                                  (isymTerm "arg" 2)
                              )
                              (ssymTerm "c")
                          )
            symIte "x" x0 x @?= expected
        ]
    ]
