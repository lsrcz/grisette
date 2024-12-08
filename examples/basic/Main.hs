--  For explanation the code in this example, please refer to the README.md.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Grisette

type IntExpr = Expr SymInteger

type BoolExpr = Expr SymBool

type UExpr a = Union (Expr a)

type UIntExpr = UExpr SymInteger

type UBoolExpr = UExpr SymBool

data Expr a where
  IntVal :: SymInteger -> IntExpr
  BoolVal :: SymBool -> BoolExpr
  Add :: UIntExpr -> UIntExpr -> IntExpr
  Mul :: UIntExpr -> UIntExpr -> IntExpr
  BAnd :: UBoolExpr -> UBoolExpr -> BoolExpr
  BOr :: UBoolExpr -> UBoolExpr -> BoolExpr
  Eq :: (BasicSymPrim a) => UExpr a -> UExpr a -> BoolExpr

deriving instance Show (Expr a)

deriveGADTAll ''Expr
makeSmartCtor ''Expr

eval :: Expr a -> a
eval (IntVal a) = a
eval (BoolVal a) = a
eval (Add a b) = eval .# a + eval .# b
eval (Mul a b) = eval .# a * eval .# b
eval (BAnd a b) = eval .# a .&& eval .# b
eval (BOr a b) = eval .# a .|| eval .# b
eval (Eq a b) = eval .# a .== eval .# b

verifyEquivalent :: (BasicSymPrim a) => Expr a -> Expr a -> IO ()
verifyEquivalent e1 e2 = do
  res <- solve z3 $ eval e1 ./= eval e2
  case res of
    Left Unsat -> putStrLn "The two expressions are equivalent"
    Left err -> putStrLn $ "The solver returned unexpected response: " <> show err
    Right model -> do
      putStrLn "The two expressions are not equivalent, under the model:"
      print model
      putStrLn $ "lhs: " <> show e1
      putStrLn $ "rhs: " <> show e2
      putStrLn $ "lhs evaluates to: " <> show (evalSym False model $ eval e1)
      putStrLn $ "rhs evaluates to: " <> show (evalSym False model $ eval e2)

synthesisRewriteTarget :: (BasicSymPrim a) => Expr a -> UExpr a -> IO ()
synthesisRewriteTarget expr sketch = do
  r <- cegisForAll z3 expr $ cegisPostCond $ eval expr .== eval .# sketch
  case r of
    (_, CEGISSuccess model) -> do
      putStrLn $ "For the target expression: " <> show expr
      putStrLn "Successfully synthesized RHS:"
      print $ evalSym False model sketch
    (cex, failure) -> do
      putStrLn $ "Synthesis failed with error: " ++ show failure
      putStrLn $ "Counter example list: " ++ show cex

productOfSum :: Expr SymInteger
productOfSum = Mul (intVal "a") (add (intVal "b") (intVal "c"))

sumOfProduct :: Expr SymInteger
sumOfProduct =
  Add (mul (intVal "a") (intVal "b")) (mul (intVal "a") (intVal "c"))

allSum :: Expr SymInteger
allSum = Add (intVal "a") (add (intVal "b") (intVal "c"))

xPlusX :: Expr SymInteger
xPlusX = Add (intVal "x") (intVal "x")

xTimesC :: UExpr SymInteger
xTimesC = mul (intVal "x") (intVal "c")

nextLevel :: [UExpr SymInteger] -> Fresh (UExpr SymInteger)
nextLevel exprs = do
  lhs <- chooseUnionFresh exprs
  rhs <- chooseUnionFresh exprs
  chooseUnionFresh [add lhs rhs, mul lhs rhs, lhs]

getSketch :: Fresh (UExpr SymInteger)
getSketch = do
  let atom = [intVal "a", intVal "b", intVal "c"]
  l2 <- nextLevel atom
  r2 <- nextLevel atom
  nextLevel [l2, r2]

sketch :: UExpr SymInteger
sketch = runFresh getSketch "sketch"

main :: IO ()
main = do
  putStrLn "---- verifying productOfSum and sumOfProduct are equivalent ----"
  verifyEquivalent productOfSum sumOfProduct
  putStrLn "---- verifying productOfSum and allSum are equivalent (should fail) ----"
  verifyEquivalent productOfSum allSum

  putStrLn "---- synthesis x + x => x * 2 ----"
  synthesisRewriteTarget xPlusX xTimesC
  putStrLn "---- synthesis a * (b + c) => a * b + a * c ----"
  synthesisRewriteTarget productOfSum sketch
