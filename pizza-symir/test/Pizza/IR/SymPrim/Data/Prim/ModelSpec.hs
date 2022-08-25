{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Pizza.IR.SymPrim.Data.Prim.ModelSpec where

import Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Pizza.IR.SymPrim.Data.BV
import Pizza.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Pizza.IR.SymPrim.Data.Prim.InternedTerm.Term
import Pizza.IR.SymPrim.Data.Prim.Model as Model
import Pizza.IR.SymPrim.Data.Prim.ModelValue
import Pizza.IR.SymPrim.Data.Prim.PartialEval.Bool
import Pizza.IR.SymPrim.Data.Prim.PartialEval.Num
import Test.Hspec
import Type.Reflection

spec :: Spec
spec = do
  describe "empty model" $ do
    let asymbol = TermSymbol (typeRep @Integer) (SimpleSymbol "a")
    let bsymbol = TermSymbol (typeRep @Bool) (SimpleSymbol "b")
    let csymbol = TermSymbol (typeRep @Integer) (SimpleSymbol "c")
    let dsymbol = TermSymbol (typeRep @Bool) $ SimpleSymbol "d"
    let esymbol = TermSymbol (typeRep @(WordN 4)) $ SimpleSymbol "e"
    let fsymbol = TermSymbol (typeRep @(IntN 4)) $ SimpleSymbol "f"
    let m1 = Model.empty
    let m2 = Model.insert m1 asymbol (1 :: Integer)
    let m3 = Model.insert m2 bsymbol True
    it "empty model is really empty" $ do
      Model.empty `shouldBe` Model M.empty
    it "inserting to model" $ do
      m3
        `shouldBe` Model
          ( M.fromList
              [ (asymbol, toModelValue (1 :: Integer)),
                (bsymbol, toModelValue True)
              ]
          )
    it "equation" $ do
      equation m3 asymbol `shouldBe` Just (pevalEqvTerm (ssymbTerm "a") (concTerm 1 :: Term Integer))
      equation m3 bsymbol `shouldBe` Just (pevalEqvTerm (ssymbTerm "b") (concTerm True))
      equation m3 csymbol `shouldBe` Nothing
    it "valueOf" $ do
      valueOf m3 asymbol `shouldBe` Just (1 :: Integer)
      valueOf m3 bsymbol `shouldBe` Just True
      valueOf m3 csymbol `shouldBe` (Nothing :: Maybe Integer)
    it "exceptFor" $ do
      exceptFor m3 (S.fromList [asymbol])
        `shouldBe` Model
          ( M.fromList
              [ (bsymbol, toModelValue True)
              ]
          )
    it "restrictTo" $ do
      restrictTo m3 (S.fromList [asymbol])
        `shouldBe` Model
          ( M.fromList
              [ (asymbol, toModelValue (1 :: Integer))
              ]
          )
    it "extendTo" $ do
      extendTo
        m3
        ( S.fromList
            [ csymbol,
              dsymbol,
              esymbol,
              fsymbol
            ]
        )
        `shouldBe` Model
          ( M.fromList
              [ (asymbol, toModelValue (1 :: Integer)),
                (bsymbol, toModelValue True),
                (csymbol, toModelValue (0 :: Integer)),
                (dsymbol, toModelValue False),
                (esymbol, toModelValue (0 :: WordN 4)),
                (fsymbol, toModelValue (0 :: IntN 4))
              ]
          )
    it "exact" $ do
      exact
        m3
        ( S.fromList
            [ asymbol,
              csymbol
            ]
        )
        `shouldBe` Model
          ( M.fromList
              [ (asymbol, toModelValue (1 :: Integer)),
                (csymbol, toModelValue (0 :: Integer))
              ]
          )
    it "evaluateTerm" $ do
      evaluateTerm False m3 (concTerm (1 :: Integer)) `shouldBe` concTerm 1
      evaluateTerm True m3 (concTerm (1 :: Integer)) `shouldBe` concTerm 1
      evaluateTerm False m3 (ssymbTerm "a" :: Term Integer) `shouldBe` concTerm 1
      evaluateTerm True m3 (ssymbTerm "a" :: Term Integer) `shouldBe` concTerm 1
      evaluateTerm False m3 (ssymbTerm "x" :: Term Integer) `shouldBe` ssymbTerm "x"
      evaluateTerm True m3 (ssymbTerm "x" :: Term Integer) `shouldBe` concTerm 0
      evaluateTerm False m3 (ssymbTerm "y" :: Term Bool) `shouldBe` ssymbTerm "y"
      evaluateTerm True m3 (ssymbTerm "y" :: Term Bool) `shouldBe` concTerm False
      evaluateTerm False m3 (ssymbTerm "z" :: Term (WordN 4)) `shouldBe` ssymbTerm "z"
      evaluateTerm True m3 (ssymbTerm "z" :: Term (WordN 4)) `shouldBe` concTerm 0
      evaluateTerm False m3 (pevalUMinusNumTerm $ ssymbTerm "a" :: Term Integer) `shouldBe` concTerm (-1)
      evaluateTerm True m3 (pevalUMinusNumTerm $ ssymbTerm "a" :: Term Integer) `shouldBe` concTerm (-1)
      evaluateTerm False m3 (pevalUMinusNumTerm $ ssymbTerm "x" :: Term Integer) `shouldBe` pevalUMinusNumTerm (ssymbTerm "x")
      evaluateTerm True m3 (pevalUMinusNumTerm $ ssymbTerm "x" :: Term Integer) `shouldBe` concTerm 0
      evaluateTerm False m3 (pevalAddNumTerm (ssymbTerm "a") (ssymbTerm "a") :: Term Integer) `shouldBe` concTerm 2
      evaluateTerm True m3 (pevalAddNumTerm (ssymbTerm "a") (ssymbTerm "a") :: Term Integer) `shouldBe` concTerm 2
      evaluateTerm False m3 (pevalAddNumTerm (ssymbTerm "x") (ssymbTerm "a") :: Term Integer) `shouldBe` pevalAddNumTerm (concTerm 1) (ssymbTerm "x")
      evaluateTerm True m3 (pevalAddNumTerm (ssymbTerm "x") (ssymbTerm "a") :: Term Integer) `shouldBe` concTerm 1
      evaluateTerm False m3 (pevalAddNumTerm (ssymbTerm "x") (ssymbTerm "y") :: Term Integer) `shouldBe` pevalAddNumTerm (ssymbTerm "x") (ssymbTerm "y")
      evaluateTerm True m3 (pevalAddNumTerm (ssymbTerm "x") (ssymbTerm "y") :: Term Integer) `shouldBe` concTerm 0
      evaluateTerm False m3 (pevalITETerm (ssymbTerm "b") (pevalAddNumTerm (ssymbTerm "a") (ssymbTerm "a")) (ssymbTerm "a") :: Term Integer)
        `shouldBe` concTerm 2
      evaluateTerm True m3 (pevalITETerm (ssymbTerm "b") (pevalAddNumTerm (ssymbTerm "a") (ssymbTerm "a")) (ssymbTerm "a") :: Term Integer)
        `shouldBe` concTerm 2
      evaluateTerm False m3 (pevalITETerm (ssymbTerm "x") (pevalAddNumTerm (ssymbTerm "a") (ssymbTerm "a")) (ssymbTerm "a") :: Term Integer)
        `shouldBe` pevalITETerm (ssymbTerm "x") (concTerm 2) (concTerm 1)
      evaluateTerm True m3 (pevalITETerm (ssymbTerm "x") (pevalAddNumTerm (ssymbTerm "a") (ssymbTerm "a")) (ssymbTerm "a") :: Term Integer)
        `shouldBe` concTerm 1
      evaluateTerm False m3 (pevalITETerm (ssymbTerm "b") (ssymbTerm "x") (pevalAddNumTerm (concTerm 1) (ssymbTerm "y")) :: Term Integer)
        `shouldBe` ssymbTerm "x"
      evaluateTerm True m3 (pevalITETerm (ssymbTerm "b") (ssymbTerm "x") (pevalAddNumTerm (concTerm 1) (ssymbTerm "y")) :: Term Integer)
        `shouldBe` concTerm 0
      evaluateTerm False m3 (pevalITETerm (ssymbTerm "z") (ssymbTerm "x") (pevalAddNumTerm (concTerm 1) (ssymbTerm "y")) :: Term Integer)
        `shouldBe` pevalITETerm (ssymbTerm "z") (ssymbTerm "x") (pevalAddNumTerm (concTerm 1) (ssymbTerm "y"))
      evaluateTerm True m3 (pevalITETerm (ssymbTerm "z") (ssymbTerm "x") (pevalAddNumTerm (concTerm 1) (ssymbTerm "y")) :: Term Integer)
        `shouldBe` concTerm 1
