{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.IR.SymPrim.Data.Prim.ModelTests (modelTests) where

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Grisette (TypedSymbol)
import Grisette.Core.Data.BV (IntN, WordN)
import Grisette.Core.Data.Class.ModelOps
  ( ModelOps
      ( emptyModel,
        exact,
        exceptFor,
        extendTo,
        insertValue,
        restrictTo,
        valueOf
      ),
    ModelRep (buildModel),
  )
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
  ( Term,
    conTerm,
    someTypedSymbol,
    ssymTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.Model
  ( Model (Model),
    ModelValuePair ((::=)),
    SymbolSet (SymbolSet),
    equation,
    evaluateTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.ModelValue (toModelValue)
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bool
  ( pevalEqvTerm,
    pevalITETerm,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Num
  ( pevalAddNumTerm,
    pevalUMinusNumTerm,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@=?))

modelTests :: Test
modelTests =
  let asymbol :: TypedSymbol Integer = "a"
      bsymbol :: TypedSymbol Bool = "b"
      csymbol :: TypedSymbol Integer = "c"
      dsymbol :: TypedSymbol Bool = "d"
      esymbol :: TypedSymbol (WordN 4) = "e"
      fsymbol :: TypedSymbol (IntN 4) = "f"
      gsymbol :: TypedSymbol (WordN 16) = "g"
      hsymbol :: TypedSymbol (IntN 16) = "h"
      m1 = emptyModel
      m2 = insertValue asymbol 1 m1
      m3 = insertValue bsymbol True m2
   in testGroup
        "Model"
        [ testCase "empty model is really empty" $ do
            emptyModel @=? Model M.empty,
          testCase "inserting to model" $ do
            m3
              @=? Model
                ( M.fromList
                    [ (someTypedSymbol asymbol, toModelValue (1 :: Integer)),
                      (someTypedSymbol bsymbol, toModelValue True)
                    ]
                ),
          testCase "equation" $ do
            equation asymbol m3 @=? Just (pevalEqvTerm (ssymTerm "a") (conTerm 1 :: Term Integer))
            equation bsymbol m3 @=? Just (pevalEqvTerm (ssymTerm "b") (conTerm True))
            equation csymbol m3 @=? Nothing,
          testCase "valueOf" $ do
            valueOf asymbol m3 @=? Just (1 :: Integer)
            valueOf bsymbol m3 @=? Just True
            valueOf csymbol m3 @=? (Nothing :: Maybe Integer),
          testCase "exceptFor" $ do
            exceptFor (SymbolSet $ S.fromList [someTypedSymbol asymbol]) m3
              @=? Model
                ( M.fromList
                    [ (someTypedSymbol bsymbol, toModelValue True)
                    ]
                ),
          testCase "restrictTo" $ do
            restrictTo (SymbolSet $ S.fromList [someTypedSymbol asymbol]) m3
              @=? Model
                ( M.fromList
                    [ (someTypedSymbol asymbol, toModelValue (1 :: Integer))
                    ]
                ),
          testCase "extendTo" $ do
            extendTo
              ( SymbolSet $
                  S.fromList
                    [ someTypedSymbol csymbol,
                      someTypedSymbol dsymbol,
                      someTypedSymbol esymbol,
                      someTypedSymbol fsymbol
                    ]
              )
              m3
              @=? Model
                ( M.fromList
                    [ (someTypedSymbol asymbol, toModelValue (1 :: Integer)),
                      (someTypedSymbol bsymbol, toModelValue True),
                      (someTypedSymbol csymbol, toModelValue (0 :: Integer)),
                      (someTypedSymbol dsymbol, toModelValue False),
                      (someTypedSymbol esymbol, toModelValue (0 :: WordN 4)),
                      (someTypedSymbol fsymbol, toModelValue (0 :: IntN 4))
                    ]
                ),
          testCase "exact" $ do
            exact
              ( SymbolSet $
                  S.fromList
                    [ someTypedSymbol asymbol,
                      someTypedSymbol csymbol
                    ]
              )
              m3
              @=? Model
                ( M.fromList
                    [ (someTypedSymbol asymbol, toModelValue (1 :: Integer)),
                      (someTypedSymbol csymbol, toModelValue (0 :: Integer))
                    ]
                ),
          testCase "evaluateTerm" $ do
            evaluateTerm False m3 (conTerm (1 :: Integer)) @=? conTerm 1
            evaluateTerm True m3 (conTerm (1 :: Integer)) @=? conTerm 1
            evaluateTerm False m3 (ssymTerm "a" :: Term Integer) @=? conTerm 1
            evaluateTerm True m3 (ssymTerm "a" :: Term Integer) @=? conTerm 1
            evaluateTerm False m3 (ssymTerm "x" :: Term Integer) @=? ssymTerm "x"
            evaluateTerm True m3 (ssymTerm "x" :: Term Integer) @=? conTerm 0
            evaluateTerm False m3 (ssymTerm "y" :: Term Bool) @=? ssymTerm "y"
            evaluateTerm True m3 (ssymTerm "y" :: Term Bool) @=? conTerm False
            evaluateTerm False m3 (ssymTerm "z" :: Term (WordN 4)) @=? ssymTerm "z"
            evaluateTerm True m3 (ssymTerm "z" :: Term (WordN 4)) @=? conTerm 0
            evaluateTerm False m3 (pevalUMinusNumTerm $ ssymTerm "a" :: Term Integer) @=? conTerm (-1)
            evaluateTerm True m3 (pevalUMinusNumTerm $ ssymTerm "a" :: Term Integer) @=? conTerm (-1)
            evaluateTerm False m3 (pevalUMinusNumTerm $ ssymTerm "x" :: Term Integer) @=? pevalUMinusNumTerm (ssymTerm "x")
            evaluateTerm True m3 (pevalUMinusNumTerm $ ssymTerm "x" :: Term Integer) @=? conTerm 0
            evaluateTerm False m3 (pevalAddNumTerm (ssymTerm "a") (ssymTerm "a") :: Term Integer) @=? conTerm 2
            evaluateTerm True m3 (pevalAddNumTerm (ssymTerm "a") (ssymTerm "a") :: Term Integer) @=? conTerm 2
            evaluateTerm False m3 (pevalAddNumTerm (ssymTerm "x") (ssymTerm "a") :: Term Integer) @=? pevalAddNumTerm (conTerm 1) (ssymTerm "x")
            evaluateTerm True m3 (pevalAddNumTerm (ssymTerm "x") (ssymTerm "a") :: Term Integer) @=? conTerm 1
            evaluateTerm False m3 (pevalAddNumTerm (ssymTerm "x") (ssymTerm "y") :: Term Integer) @=? pevalAddNumTerm (ssymTerm "x") (ssymTerm "y")
            evaluateTerm True m3 (pevalAddNumTerm (ssymTerm "x") (ssymTerm "y") :: Term Integer) @=? conTerm 0
            evaluateTerm False m3 (pevalITETerm (ssymTerm "b") (pevalAddNumTerm (ssymTerm "a") (ssymTerm "a")) (ssymTerm "a") :: Term Integer)
              @=? conTerm 2
            evaluateTerm True m3 (pevalITETerm (ssymTerm "b") (pevalAddNumTerm (ssymTerm "a") (ssymTerm "a")) (ssymTerm "a") :: Term Integer)
              @=? conTerm 2
            evaluateTerm False m3 (pevalITETerm (ssymTerm "x") (pevalAddNumTerm (ssymTerm "a") (ssymTerm "a")) (ssymTerm "a") :: Term Integer)
              @=? pevalITETerm (ssymTerm "x") (conTerm 2) (conTerm 1)
            evaluateTerm True m3 (pevalITETerm (ssymTerm "x") (pevalAddNumTerm (ssymTerm "a") (ssymTerm "a")) (ssymTerm "a") :: Term Integer)
              @=? conTerm 1
            evaluateTerm False m3 (pevalITETerm (ssymTerm "b") (ssymTerm "x") (pevalAddNumTerm (conTerm 1) (ssymTerm "y")) :: Term Integer)
              @=? ssymTerm "x"
            evaluateTerm True m3 (pevalITETerm (ssymTerm "b") (ssymTerm "x") (pevalAddNumTerm (conTerm 1) (ssymTerm "y")) :: Term Integer)
              @=? conTerm 0
            evaluateTerm False m3 (pevalITETerm (ssymTerm "z") (ssymTerm "x") (pevalAddNumTerm (conTerm 1) (ssymTerm "y")) :: Term Integer)
              @=? pevalITETerm (ssymTerm "z") (ssymTerm "x") (pevalAddNumTerm (conTerm 1) (ssymTerm "y"))
            evaluateTerm True m3 (pevalITETerm (ssymTerm "z") (ssymTerm "x") (pevalAddNumTerm (conTerm 1) (ssymTerm "y")) :: Term Integer)
              @=? conTerm 1,
          testCase "construction from ModelValuePair" $ do
            buildModel (asymbol ::= 1) @=? Model (M.singleton (someTypedSymbol asymbol) (toModelValue (1 :: Integer)))
            buildModel (asymbol ::= 1, bsymbol ::= True)
              @=? Model
                ( M.fromList
                    [ (someTypedSymbol asymbol, toModelValue (1 :: Integer)),
                      (someTypedSymbol bsymbol, toModelValue True)
                    ]
                )
            buildModel
              ( asymbol ::= 1,
                bsymbol ::= True,
                csymbol ::= 2
              )
              @=? Model
                ( M.fromList
                    [ (someTypedSymbol asymbol, toModelValue (1 :: Integer)),
                      (someTypedSymbol bsymbol, toModelValue True),
                      (someTypedSymbol csymbol, toModelValue (2 :: Integer))
                    ]
                )
            buildModel
              ( asymbol ::= 1,
                bsymbol ::= True,
                csymbol ::= 2,
                dsymbol ::= False
              )
              @=? Model
                ( M.fromList
                    [ (someTypedSymbol asymbol, toModelValue (1 :: Integer)),
                      (someTypedSymbol bsymbol, toModelValue True),
                      (someTypedSymbol csymbol, toModelValue (2 :: Integer)),
                      (someTypedSymbol dsymbol, toModelValue False)
                    ]
                )
            buildModel
              ( asymbol ::= 1,
                bsymbol ::= True,
                csymbol ::= 2,
                dsymbol ::= False,
                esymbol ::= 3
              )
              @=? Model
                ( M.fromList
                    [ (someTypedSymbol asymbol, toModelValue (1 :: Integer)),
                      (someTypedSymbol bsymbol, toModelValue True),
                      (someTypedSymbol csymbol, toModelValue (2 :: Integer)),
                      (someTypedSymbol dsymbol, toModelValue False),
                      (someTypedSymbol esymbol, toModelValue (3 :: WordN 4))
                    ]
                )
            buildModel
              ( asymbol ::= 1,
                bsymbol ::= True,
                csymbol ::= 2,
                dsymbol ::= False,
                esymbol ::= 3,
                fsymbol ::= 4
              )
              @=? Model
                ( M.fromList
                    [ (someTypedSymbol asymbol, toModelValue (1 :: Integer)),
                      (someTypedSymbol bsymbol, toModelValue True),
                      (someTypedSymbol csymbol, toModelValue (2 :: Integer)),
                      (someTypedSymbol dsymbol, toModelValue False),
                      (someTypedSymbol esymbol, toModelValue (3 :: WordN 4)),
                      (someTypedSymbol fsymbol, toModelValue (4 :: IntN 4))
                    ]
                )
            buildModel
              ( asymbol ::= 1,
                bsymbol ::= True,
                csymbol ::= 2,
                dsymbol ::= False,
                esymbol ::= 3,
                fsymbol ::= 4,
                gsymbol ::= 5
              )
              @=? Model
                ( M.fromList
                    [ (someTypedSymbol asymbol, toModelValue (1 :: Integer)),
                      (someTypedSymbol bsymbol, toModelValue True),
                      (someTypedSymbol csymbol, toModelValue (2 :: Integer)),
                      (someTypedSymbol dsymbol, toModelValue False),
                      (someTypedSymbol esymbol, toModelValue (3 :: WordN 4)),
                      (someTypedSymbol fsymbol, toModelValue (4 :: IntN 4)),
                      (someTypedSymbol gsymbol, toModelValue (5 :: WordN 16))
                    ]
                )
            buildModel
              ( asymbol ::= 1,
                bsymbol ::= True,
                csymbol ::= 2,
                dsymbol ::= False,
                esymbol ::= 3,
                fsymbol ::= 4,
                gsymbol ::= 5,
                hsymbol ::= 6
              )
              @=? Model
                ( M.fromList
                    [ (someTypedSymbol asymbol, toModelValue (1 :: Integer)),
                      (someTypedSymbol bsymbol, toModelValue True),
                      (someTypedSymbol csymbol, toModelValue (2 :: Integer)),
                      (someTypedSymbol dsymbol, toModelValue False),
                      (someTypedSymbol esymbol, toModelValue (3 :: WordN 4)),
                      (someTypedSymbol fsymbol, toModelValue (4 :: IntN 4)),
                      (someTypedSymbol gsymbol, toModelValue (5 :: WordN 16)),
                      (someTypedSymbol hsymbol, toModelValue (6 :: IntN 16))
                    ]
                )
        ]
