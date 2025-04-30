{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | This tutorial demonstrates Grisette's core capabilities through a simple
-- expression language.
--
-- What you'll learn:
-- 1. How to define the syntax of the language using GADTs
-- 2. How to define the semantics of the language
-- 3. How to verify expression equivalence using SMT solvers
-- 4. How to synthesize equivalent expressions using CEGIS
--
-- Key concepts:
--
-- * Symbolic values (`SymInteger`, `SymBool`)
-- * Union types for representing choices
-- * Interpreter construction
-- * SMT-based verification
-- * Program synthesis
--
-- Prerequisites: Basic Haskell, familiarity with GADTs
module Main (main) where

import Grisette

-- * The expression language

-- | In this example, we will define the syntax and semantics of a simple
-- boolean and integer expression language.
--
-- The language syntax follows the following grammar:
--
-- Expr -> IntExpr | BoolExpr
-- IntExpr -> IntVal int
--          | Add IntExpr IntExpr
--          | Mul IntExpr IntExpr
-- BoolExpr -> BoolVal bool
--           | BAnd BoolExpr BoolExpr
--           | BOr BoolExpr BoolExpr
--           | Eq Expr Expr

-- * Build the syntax of the expression language.

-- A symbolic expression can be represented in Grisette as a GADT as follows.
--
-- We will see how to define values of this expression type later.
data Expr a where
  -- | A symbolic integer value.
  --
  -- The symbolic integer (`SymInteger`) type is a symbolic (primitive) type,
  -- representing SMT integer terms.
  IntVal :: SymInteger -> IntExpr
  -- | A symbolic boolean value.
  --
  -- `SymBool` is another symbolic (primitive) type, representing SMT boolean
  -- terms.
  BoolVal :: SymBool -> BoolExpr
  -- | Addition of two symbolic integer expressions.
  --
  -- Here, `IntUExpr` is a type alias for `UExpr SymInteger`, which is a union
  -- of `Expr SymInteger`.
  --
  -- A union value is a value representing a choice of symbolic expressions.
  -- Our addition operator can take choices as operands to represent a program
  -- space.
  Add :: IntUExpr -> IntUExpr -> IntExpr
  -- | Multiplication of two symbolic integer expressions.
  Mul :: IntUExpr -> IntUExpr -> IntExpr
  -- | Logical AND of two symbolic boolean expressions.
  BAnd :: BoolUExpr -> BoolUExpr -> BoolExpr
  -- | Logical OR of two symbolic boolean expressions.
  BOr :: BoolUExpr -> BoolUExpr -> BoolExpr
  -- | Equality check between two symbolic expressions.
  --
  -- `BasicSymPrim` is a constraint that contains all the symbolic primitive
  -- types that Grisette supports, including `SymInteger` and `SymBool`.
  Eq :: (BasicSymPrim a) => UExpr a -> UExpr a -> BoolExpr

-- Some type aliases for clarity.
type IntExpr = Expr SymInteger

type BoolExpr = Expr SymBool

type UExpr a = Union (Expr a)

type IntUExpr = UExpr SymInteger

type BoolUExpr = UExpr SymBool

-- The `makeGrisetteBasicADT` function is provided by Grisette to automatically
-- derive some instances and construct some smart constructors for the GADT.
--
-- The GADT will then be fully compatible with Grisette library. And we will see
-- their usage in the following code.
makeGrisetteBasicADT ''Expr

-- Here we construct some symbolic expressions.
-- We use the data constructors to construct the expressions.
e0 = IntVal 1

e1 = IntVal "a"

-- |
-- >>> target = Add (intVal "a") (intVal "a")
-- >>> sketch = Mul (intVal "a") (intVal "c")
--
-- >>> cegisForAll z3 target $ cegisPostCond $ eval target .== eval sketch
-- ([],CEGISSuccess (Model {c -> 2 :: Integer}))
e2 = IntVal ("a" + 1)

-- intVal is a smart constructor provided by Grisette to construct `Union` of
-- expressions.
e3 = intVal 1

e4 = Add (intVal "a") e3

add2 = add (intVal "a") (intVal 2)

mul2 = mul (intVal "a") (intVal 2)

-- We can then use `mrgIf` function to perform a choice between two symbolic
-- expressions.
--
-- The following code will construct the following expression:
--
-- >>> chosen
-- {If choice (Add {IntVal a} {IntVal 2}) (Mul {IntVal a} {IntVal 2})}
--
-- In this code, the `choice` can be used to choose between `add2`(i.e., a + 2)
-- and `mul2` (i.e., a * 2).
--
-- A synthesizer can then pick true or false for the `choice` variable to
-- decide which expression to pick. If the synthesizer picks true, the result is

-- $a+2$; otherwise, it is $a*2$.

chosen :: IntUExpr
chosen = mrgIf "choice" add2 mul2

-- * Defining the semantics of the expression language.

-- | Evaluation of a symbolic expression.
--
-- >>> eval (IntVal 1)
-- 1
-- >>> eval (IntVal "a")
-- a
-- >>> eval (Add (intVal 1) (intVal 2))
-- 3
-- >>> eval (Eq (intVal "x") (intVal 2))
-- (= x 2)
--
-- The `.#` combinator handles the application on `Union`s:
--
-- 1. It extracts all the symbolic choices from the expression.
-- 2. It applies the `eval` function to each of the symbolic choices.
-- 3. It merges the results into a single value.
--
-- as shown in the following pseudo code:
--
-- eval {If a choice1 choice2) = {If a (eval choice1) (eval choice2)}
--
-- >>> expr = mrgIf "choice" (add (intVal "a") (intVal 2)) (mul (intVal "a") (intVal 2)) :: IntUExpr
-- >>> eval .# expr
eval :: Expr a -> a
eval (IntVal a) = a
eval (BoolVal a) = a
eval (Add a b) = eval .# a + eval .# b
eval (Mul a b) = eval .# a * eval .# b
eval (BAnd a b) = eval .# a .&& eval .# b
eval (BOr a b) = eval .# a .|| eval .# b
eval (Eq a b) = eval .# a .== eval .# b

-- With the syntax and semantics defined, it is easy to build solver-aided tools
-- in Grisette. The following code builds a verifier to check if two expressions
-- are equivalent.
verifyEquivalent :: (BasicSymPrim a) => Expr a -> Expr a -> IO ()
verifyEquivalent e1 e2 = do
  -- We check if there exists a counter-example that falsifies the equivalence
  -- of the two expressions.
  res <- solve z3 $ eval e1 ./= eval e2
  case res of
    -- If the solver cannot find such a counter-example, the two expressions are
    -- equivalent.
    Left Unsat -> putStrLn "The two expressions are equivalent"
    -- If the solver returns an unexpected response, we print the error.
    Left err -> putStrLn $ "The solver returned unexpected response: " <> show err
    -- If the solver finds a counter-example, we print the counter-example and
    -- the two expressions.
    Right model -> do
      putStrLn "The two expressions are not equivalent, under the model:"
      print model
      putStrLn $ "lhs: " <> show e1
      putStrLn $ "rhs: " <> show e2
      -- We can use tools like `evalSym` to evaluate the expressions under the
      -- model to see why the two expressions are not equivalent.
      putStrLn $ "lhs evaluates to: " <> show (evalSym False model $ eval e1)
      putStrLn $ "rhs evaluates to: " <> show (evalSym False model $ eval e2)

-- | Build a synthesizer to find an equivalent expression for a given target
-- expression.
--
-- The synthesis problem can be formulated as finding values for holes in a
-- sketch expression that make it equivalent to a target expression.
-- Mathematically:
--
-- ∃s ∀x. target(x) = sketch(s,x)
--
-- Where:
--
-- * s represents the symbolic constants (holes) in the sketch that we want to
--   synthesize - these are the values we're trying to find
-- * x represents the free variables in both expressions - these are the inputs
--   that both expressions must handle equivalently
-- * target(x) is the target expression we want to match
-- * sketch(s,x) is the template expression with holes that we want to fill
--
-- We solve this using Counter-Example Guided Inductive Synthesis (CEGIS),
-- which:
--
-- 1. Proposes candidate values for holes
-- 2. Checks if the resulting expression matches the target for all inputs
-- 3. If not, uses counter-examples to refine the candidates
-- 4. Repeats until a solution is found or synthesis is proven impossible
--
-- For example, given target @a + a@ and sketch @a * c@:
--
-- * We want to synthesize a value for @c@ (the hole)
-- * Such that for any input value of @a@
-- * The equation @a + a = a * c@ holds
-- * CEGIS will discover @c = 2@ is the solution, since @a + a = a * 2@ for all
--   @a@
--
-- Grisette provides built-in CEGIS functionality to solve such synthesis
-- problems.
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

-- * Verification and synthesis examples

aPlusB :: IntExpr
aPlusB = Add (intVal "a") (intVal "b")

bPlusA :: IntExpr
bPlusA = Add (intVal "b") (intVal "a")

aPlusA :: IntExpr
aPlusA = Add (intVal "a") (intVal "a")

productOfSum :: IntExpr
productOfSum = Mul (intVal "a") (add (intVal "b") (intVal "c"))

sumOfProduct :: IntExpr
sumOfProduct =
  Add (mul (intVal "a") (intVal "b")) (mul (intVal "a") (intVal "c"))

allSum :: IntExpr
allSum = Add (intVal "a") (add (intVal "b") (intVal "c"))

xPlusX :: IntExpr
xPlusX = Add (intVal "x") (intVal "x")

xTimesC :: IntUExpr
xTimesC = mul (intVal "x") (intVal "c")

-- | We can use the `Fresh` monad to build sketches in a modular way.
--
-- Given a list of expressions, this function builds a new expression that
-- represents one of three symbolic choices:
--
-- * Addition of two expressions symbolically chosen from the input list
-- * Multiplication of two expressions symbolically chosen from the input list
-- * A single expression symbolically chosen from the input list
--
-- The `chooseUnionFresh` function generates fresh symbolic variables to
-- represent these choices. For example, given expressions [a, b, c], it
-- creates:
--
-- > {If cond@1 a {If cond@2 b c}}
--
-- Where cond@1 and cond@2 are unique symbolic boolean variables managed by the
-- `Fresh` monad. The solver can then determine the values of these boolean
-- variables to select the desired expressions. Each call to `chooseUnionFresh`
-- generates new variables, ensuring no overlap between different choices.
nextLevel :: [IntUExpr] -> Fresh IntUExpr
nextLevel exprs = do
  lhs <- chooseUnionFresh exprs
  rhs <- chooseUnionFresh exprs
  chooseUnionFresh [add lhs rhs, mul lhs rhs, lhs]

-- | Builds a hierarchical sketch for expression synthesis.
--
-- This function creates a two-level sketch where:
--
-- 1. The base level consists of atomic variables a, b, and c
-- 2. The middle level (l2, r2) applies nextLevel to combine these atoms
-- 3. The final level combines the middle level expressions
--
-- This creates a rich space of possible expressions by composing
-- additions, multiplications and variable selections at multiple levels.
getSketch :: Fresh IntUExpr
getSketch = do
  let atom = [intVal "a", intVal "b", intVal "c"]
  l2 <- nextLevel atom
  r2 <- nextLevel atom
  nextLevel [l2, r2]

sketch :: IntUExpr
sketch = runFresh getSketch "sketch"

main :: IO ()
main = do
  putStrLn "---- verifying a + b and b + a are equivalent ----"
  verifyEquivalent aPlusB bPlusA
  putStrLn "---- verifying a + a and a + b are not equivalent (should fail) ----"
  verifyEquivalent aPlusA aPlusB

  putStrLn "---- verifying productOfSum and sumOfProduct are equivalent ----"
  verifyEquivalent productOfSum sumOfProduct
  putStrLn "---- verifying productOfSum and allSum are equivalent (should fail) ----"
  verifyEquivalent productOfSum allSum

  putStrLn "---- synthesis x + x => x * 2 ----"
  synthesisRewriteTarget xPlusX xTimesC
  putStrLn "---- synthesis a * (b + c) => a * b + a * c ----"
  synthesisRewriteTarget productOfSum sketch
