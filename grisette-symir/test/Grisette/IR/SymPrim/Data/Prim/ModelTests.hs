{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.IR.SymPrim.Data.Prim.ModelTests where

import Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Grisette.Core.Data.Class.ModelOps
import Grisette.IR.SymPrim.Data.BV
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Grisette.IR.SymPrim.Data.Prim.Model as Model
import Grisette.IR.SymPrim.Data.Prim.ModelValue
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bool
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Num
import Test.Tasty
import Test.Tasty.HUnit
import Type.Reflection

modelTests :: TestTree
modelTests =
  let asymbol :: TypedSymbol Integer = TypedSymbol (SimpleSymbol "a")
      bsymbol :: TypedSymbol Bool = TypedSymbol (SimpleSymbol "b")
      csymbol :: TypedSymbol Integer = TypedSymbol (SimpleSymbol "c")
      dsymbol :: TypedSymbol Bool = TypedSymbol $ SimpleSymbol "d"
      esymbol :: TypedSymbol (WordN 4) = TypedSymbol $ SimpleSymbol "e"
      fsymbol :: TypedSymbol (IntN 4) = TypedSymbol $ SimpleSymbol "f"
      m1 = emptyModel
      m2 = insertValue asymbol 1 m1
      m3 = insertValue bsymbol True m2
   in testGroup
        "ModelTests"
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
            equation asymbol m3 @=? Just (pevalEqvTerm (ssymbTerm "a") (concTerm 1 :: Term Integer))
            equation bsymbol m3 @=? Just (pevalEqvTerm (ssymbTerm "b") (concTerm True))
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
            evaluateTerm False m3 (concTerm (1 :: Integer)) @=? concTerm 1
            evaluateTerm True m3 (concTerm (1 :: Integer)) @=? concTerm 1
            evaluateTerm False m3 (ssymbTerm "a" :: Term Integer) @=? concTerm 1
            evaluateTerm True m3 (ssymbTerm "a" :: Term Integer) @=? concTerm 1
            evaluateTerm False m3 (ssymbTerm "x" :: Term Integer) @=? ssymbTerm "x"
            evaluateTerm True m3 (ssymbTerm "x" :: Term Integer) @=? concTerm 0
            evaluateTerm False m3 (ssymbTerm "y" :: Term Bool) @=? ssymbTerm "y"
            evaluateTerm True m3 (ssymbTerm "y" :: Term Bool) @=? concTerm False
            evaluateTerm False m3 (ssymbTerm "z" :: Term (WordN 4)) @=? ssymbTerm "z"
            evaluateTerm True m3 (ssymbTerm "z" :: Term (WordN 4)) @=? concTerm 0
            evaluateTerm False m3 (pevalUMinusNumTerm $ ssymbTerm "a" :: Term Integer) @=? concTerm (-1)
            evaluateTerm True m3 (pevalUMinusNumTerm $ ssymbTerm "a" :: Term Integer) @=? concTerm (-1)
            evaluateTerm False m3 (pevalUMinusNumTerm $ ssymbTerm "x" :: Term Integer) @=? pevalUMinusNumTerm (ssymbTerm "x")
            evaluateTerm True m3 (pevalUMinusNumTerm $ ssymbTerm "x" :: Term Integer) @=? concTerm 0
            evaluateTerm False m3 (pevalAddNumTerm (ssymbTerm "a") (ssymbTerm "a") :: Term Integer) @=? concTerm 2
            evaluateTerm True m3 (pevalAddNumTerm (ssymbTerm "a") (ssymbTerm "a") :: Term Integer) @=? concTerm 2
            evaluateTerm False m3 (pevalAddNumTerm (ssymbTerm "x") (ssymbTerm "a") :: Term Integer) @=? pevalAddNumTerm (concTerm 1) (ssymbTerm "x")
            evaluateTerm True m3 (pevalAddNumTerm (ssymbTerm "x") (ssymbTerm "a") :: Term Integer) @=? concTerm 1
            evaluateTerm False m3 (pevalAddNumTerm (ssymbTerm "x") (ssymbTerm "y") :: Term Integer) @=? pevalAddNumTerm (ssymbTerm "x") (ssymbTerm "y")
            evaluateTerm True m3 (pevalAddNumTerm (ssymbTerm "x") (ssymbTerm "y") :: Term Integer) @=? concTerm 0
            evaluateTerm False m3 (pevalITETerm (ssymbTerm "b") (pevalAddNumTerm (ssymbTerm "a") (ssymbTerm "a")) (ssymbTerm "a") :: Term Integer)
              @=? concTerm 2
            evaluateTerm True m3 (pevalITETerm (ssymbTerm "b") (pevalAddNumTerm (ssymbTerm "a") (ssymbTerm "a")) (ssymbTerm "a") :: Term Integer)
              @=? concTerm 2
            evaluateTerm False m3 (pevalITETerm (ssymbTerm "x") (pevalAddNumTerm (ssymbTerm "a") (ssymbTerm "a")) (ssymbTerm "a") :: Term Integer)
              @=? pevalITETerm (ssymbTerm "x") (concTerm 2) (concTerm 1)
            evaluateTerm True m3 (pevalITETerm (ssymbTerm "x") (pevalAddNumTerm (ssymbTerm "a") (ssymbTerm "a")) (ssymbTerm "a") :: Term Integer)
              @=? concTerm 1
            evaluateTerm False m3 (pevalITETerm (ssymbTerm "b") (ssymbTerm "x") (pevalAddNumTerm (concTerm 1) (ssymbTerm "y")) :: Term Integer)
              @=? ssymbTerm "x"
            evaluateTerm True m3 (pevalITETerm (ssymbTerm "b") (ssymbTerm "x") (pevalAddNumTerm (concTerm 1) (ssymbTerm "y")) :: Term Integer)
              @=? concTerm 0
            evaluateTerm False m3 (pevalITETerm (ssymbTerm "z") (ssymbTerm "x") (pevalAddNumTerm (concTerm 1) (ssymbTerm "y")) :: Term Integer)
              @=? pevalITETerm (ssymbTerm "z") (ssymbTerm "x") (pevalAddNumTerm (concTerm 1) (ssymbTerm "y"))
            evaluateTerm True m3 (pevalITETerm (ssymbTerm "z") (ssymbTerm "x") (pevalAddNumTerm (concTerm 1) (ssymbTerm "y")) :: Term Integer)
              @=? concTerm 1
        ]
