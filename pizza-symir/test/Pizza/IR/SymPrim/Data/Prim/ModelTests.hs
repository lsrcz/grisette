{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Pizza.IR.SymPrim.Data.Prim.ModelTests where

import Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Pizza.IR.SymPrim.Data.BV
import Pizza.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Pizza.IR.SymPrim.Data.Prim.InternedTerm.Term
import Pizza.IR.SymPrim.Data.Prim.Model as Model
import Pizza.IR.SymPrim.Data.Prim.ModelValue
import Pizza.IR.SymPrim.Data.Prim.PartialEval.Bool
import Pizza.IR.SymPrim.Data.Prim.PartialEval.Num
import Test.Tasty
import Test.Tasty.HUnit
import Type.Reflection

modelTests :: TestTree
modelTests =
  let asymbol = TermSymbol (typeRep @Integer) (SimpleSymbol "a")
      bsymbol = TermSymbol (typeRep @Bool) (SimpleSymbol "b")
      csymbol = TermSymbol (typeRep @Integer) (SimpleSymbol "c")
      dsymbol = TermSymbol (typeRep @Bool) $ SimpleSymbol "d"
      esymbol = TermSymbol (typeRep @(WordN 4)) $ SimpleSymbol "e"
      fsymbol = TermSymbol (typeRep @(IntN 4)) $ SimpleSymbol "f"
      m1 = Model.empty
      m2 = Model.insert m1 asymbol (1 :: Integer)
      m3 = Model.insert m2 bsymbol True
   in testGroup
        "ModelTests"
        [ testCase "empty model is really empty" $ do
            Model.empty @=? Model M.empty,
          testCase "inserting to model" $ do
            m3
              @=? Model
                ( M.fromList
                    [ (asymbol, toModelValue (1 :: Integer)),
                      (bsymbol, toModelValue True)
                    ]
                ),
          testCase "equation" $ do
            equation m3 asymbol @=? Just (pevalEqvTerm (ssymbTerm "a") (concTerm 1 :: Term Integer))
            equation m3 bsymbol @=? Just (pevalEqvTerm (ssymbTerm "b") (concTerm True))
            equation m3 csymbol @=? Nothing,
          testCase "valueOf" $ do
            valueOf m3 asymbol @=? Just (1 :: Integer)
            valueOf m3 bsymbol @=? Just True
            valueOf m3 csymbol @=? (Nothing :: Maybe Integer),
          testCase "exceptFor" $ do
            exceptFor m3 (S.fromList [asymbol])
              @=? Model
                ( M.fromList
                    [ (bsymbol, toModelValue True)
                    ]
                ),
          testCase "restrictTo" $ do
            restrictTo m3 (S.fromList [asymbol])
              @=? Model
                ( M.fromList
                    [ (asymbol, toModelValue (1 :: Integer))
                    ]
                ),
          testCase "extendTo" $ do
            extendTo
              m3
              ( S.fromList
                  [ csymbol,
                    dsymbol,
                    esymbol,
                    fsymbol
                  ]
              )
              @=? Model
                ( M.fromList
                    [ (asymbol, toModelValue (1 :: Integer)),
                      (bsymbol, toModelValue True),
                      (csymbol, toModelValue (0 :: Integer)),
                      (dsymbol, toModelValue False),
                      (esymbol, toModelValue (0 :: WordN 4)),
                      (fsymbol, toModelValue (0 :: IntN 4))
                    ]
                ),
          testCase "exact" $ do
            exact
              m3
              ( S.fromList
                  [ asymbol,
                    csymbol
                  ]
              )
              @=? Model
                ( M.fromList
                    [ (asymbol, toModelValue (1 :: Integer)),
                      (csymbol, toModelValue (0 :: Integer))
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
