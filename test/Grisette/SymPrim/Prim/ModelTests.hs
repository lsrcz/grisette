{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.SymPrim.Prim.ModelTests (modelTests) where

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Grisette
  ( IntN,
    ModelOps
      ( emptyModel,
        exact,
        exceptFor,
        extendTo,
        insertValue,
        restrictTo,
        valueOf
      ),
    ModelRep (buildModel),
    TypedAnySymbol,
    TypedConstantSymbol,
    WordN,
  )
import Grisette.Internal.SymPrim.Prim.Model
  ( Model (Model),
    ModelValuePair ((::=)),
    SymbolSet (SymbolSet),
    equation,
    evalTerm,
  )
import Grisette.Internal.SymPrim.Prim.Term
  ( PEvalNumTerm (pevalAddNumTerm, pevalNegNumTerm),
    SupportedPrim (pevalITETerm),
    Term,
    conTerm,
    pevalEqTerm,
    someTypedSymbol,
    ssymTerm,
    toModelValue,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@=?))

modelTests :: Test
modelTests =
  let asymbol :: TypedAnySymbol Integer = "a"
      bsymbol :: TypedAnySymbol Bool = "b"
      csymbol :: TypedAnySymbol Integer = "c"
      dsymbol :: TypedAnySymbol Bool = "d"
      esymbol :: TypedAnySymbol (WordN 4) = "e"
      fsymbol :: TypedAnySymbol (IntN 4) = "f"
      gsymbol :: TypedAnySymbol (WordN 16) = "g"
      hsymbol :: TypedAnySymbol (IntN 16) = "h"
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
            equation asymbol m3 @=? Just (pevalEqTerm (ssymTerm "a") (conTerm 1 :: Term Integer))
            equation bsymbol m3 @=? Just (pevalEqTerm (ssymTerm "b") (conTerm True))
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
          testCase "evalTerm" $ do
            evalTerm False m3 S.empty (conTerm (1 :: Integer)) @=? conTerm 1
            evalTerm True m3 S.empty (conTerm (1 :: Integer)) @=? conTerm 1
            evalTerm False m3 S.empty (ssymTerm "a" :: Term Integer) @=? conTerm 1
            evalTerm True m3 S.empty (ssymTerm "a" :: Term Integer) @=? conTerm 1
            evalTerm False m3 S.empty (ssymTerm "x" :: Term Integer) @=? ssymTerm "x"
            evalTerm True m3 S.empty (ssymTerm "x" :: Term Integer) @=? conTerm 0
            evalTerm
              False
              m3
              (S.singleton (someTypedSymbol ("x" :: TypedConstantSymbol Integer)))
              (ssymTerm "x" :: Term Integer)
              @=? ssymTerm "x"
            evalTerm
              True
              m3
              (S.singleton (someTypedSymbol ("x" :: TypedConstantSymbol Integer)))
              (ssymTerm "x" :: Term Integer)
              @=? ssymTerm "x"
            evalTerm False m3 S.empty (ssymTerm "y" :: Term Bool) @=? ssymTerm "y"
            evalTerm True m3 S.empty (ssymTerm "y" :: Term Bool) @=? conTerm False
            evalTerm False m3 S.empty (ssymTerm "z" :: Term (WordN 4)) @=? ssymTerm "z"
            evalTerm True m3 S.empty (ssymTerm "z" :: Term (WordN 4)) @=? conTerm 0
            evalTerm False m3 S.empty (pevalNegNumTerm $ ssymTerm "a" :: Term Integer) @=? conTerm (-1)
            evalTerm True m3 S.empty (pevalNegNumTerm $ ssymTerm "a" :: Term Integer) @=? conTerm (-1)
            evalTerm False m3 S.empty (pevalNegNumTerm $ ssymTerm "x" :: Term Integer) @=? pevalNegNumTerm (ssymTerm "x")
            evalTerm True m3 S.empty (pevalNegNumTerm $ ssymTerm "x" :: Term Integer) @=? conTerm 0
            evalTerm False m3 S.empty (pevalAddNumTerm (ssymTerm "a") (ssymTerm "a") :: Term Integer) @=? conTerm 2
            evalTerm True m3 S.empty (pevalAddNumTerm (ssymTerm "a") (ssymTerm "a") :: Term Integer) @=? conTerm 2
            evalTerm False m3 S.empty (pevalAddNumTerm (ssymTerm "x") (ssymTerm "a") :: Term Integer) @=? pevalAddNumTerm (conTerm 1) (ssymTerm "x")
            evalTerm True m3 S.empty (pevalAddNumTerm (ssymTerm "x") (ssymTerm "a") :: Term Integer) @=? conTerm 1
            evalTerm False m3 S.empty (pevalAddNumTerm (ssymTerm "x") (ssymTerm "y") :: Term Integer) @=? pevalAddNumTerm (ssymTerm "x") (ssymTerm "y")
            evalTerm True m3 S.empty (pevalAddNumTerm (ssymTerm "x") (ssymTerm "y") :: Term Integer) @=? conTerm 0
            evalTerm False m3 S.empty (pevalITETerm (ssymTerm "b") (pevalAddNumTerm (ssymTerm "a") (ssymTerm "a")) (ssymTerm "a") :: Term Integer)
              @=? conTerm 2
            evalTerm True m3 S.empty (pevalITETerm (ssymTerm "b") (pevalAddNumTerm (ssymTerm "a") (ssymTerm "a")) (ssymTerm "a") :: Term Integer)
              @=? conTerm 2
            evalTerm False m3 S.empty (pevalITETerm (ssymTerm "x") (pevalAddNumTerm (ssymTerm "a") (ssymTerm "a")) (ssymTerm "a") :: Term Integer)
              @=? pevalITETerm (ssymTerm "x") (conTerm 2) (conTerm 1)
            evalTerm True m3 S.empty (pevalITETerm (ssymTerm "x") (pevalAddNumTerm (ssymTerm "a") (ssymTerm "a")) (ssymTerm "a") :: Term Integer)
              @=? conTerm 1
            evalTerm False m3 S.empty (pevalITETerm (ssymTerm "b") (ssymTerm "x") (pevalAddNumTerm (conTerm 1) (ssymTerm "y")) :: Term Integer)
              @=? ssymTerm "x"
            evalTerm True m3 S.empty (pevalITETerm (ssymTerm "b") (ssymTerm "x") (pevalAddNumTerm (conTerm 1) (ssymTerm "y")) :: Term Integer)
              @=? conTerm 0
            evalTerm False m3 S.empty (pevalITETerm (ssymTerm "z") (ssymTerm "x") (pevalAddNumTerm (conTerm 1) (ssymTerm "y")) :: Term Integer)
              @=? pevalITETerm (ssymTerm "z") (ssymTerm "x") (pevalAddNumTerm (conTerm 1) (ssymTerm "y"))
            evalTerm True m3 S.empty (pevalITETerm (ssymTerm "z") (ssymTerm "x") (pevalAddNumTerm (conTerm 1) (ssymTerm "y")) :: Term Integer)
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
