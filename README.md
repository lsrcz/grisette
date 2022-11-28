# Grisette

[![Haskell Tests](https://github.com/lsrcz/grisette/actions/workflows/test.yml/badge.svg)](https://github.com/lsrcz/grisette/actions/workflows/test.yml)
[![codecov](https://codecov.io/gh/lsrcz/grisette/branch/main/graph/badge.svg?token=MNDRFY2JEB)](https://codecov.io/gh/lsrcz/grisette)

Grisette is a reusable symbolic evaluation library for Haskell. By translating
programs into constraints, Grisette can help the development of program
reasoning tools, including verification, synthesis, and more.

## Features

- Fast state merging with efficiently constraint generation.
- Symbolic evaluating monadic effects with monad transformers, including error handling, stateful
  computation, etc.
- Extensible symbolic evaluation framework with Haskell library
  interoperability.

## Installation
[TODO] after publishing to Hackage, we will list the cabal command here.

You also need to install an SMT solver and make it available through `PATH`.
Grisette currently uses [sbv](http://leventerkok.github.io/sbv/) as the backend
and supports whatever solvers sbv supports, including
[Z3](http://github.com/Z3Prover/z3/wiki), [CVC4](http://cvc4.github.io/),
[Yices](http://yices.csl.sri.com/), [Boolector](http://fmv.jku.at/boolector/),
etc.

**Example section would go here**

## License
The Grisette library is distributed under the terms of the BSD3 license. The
[LICENSE](LICENSE) file contains the full license text.

## Citing Grisette
If you use Grisette in your research, please use the following bibtex entry:

```bibtex
TODO
```


## Example

The following is an executable example for Grisette to demonstrate how to
use symbolic evaluation to find a list with some specific properties.

The main module `Grisette` contains almost all you need for implementing a
symbolic evaluation tool. Grisette provides the representation and operations
for symbolic values, the monadic encoding of multipath execution, and a solver
interface.

A program with a set of symbolic constants (i.e. placeholders for concrete
values) can be compiled to logical constraints, and the constraints can be
solved with off-the-shelf SMT solvers.

```haskell
-- Allows to create symbolic constants from strings.
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Grisette

-- A symbolic value with four symbolic constants (a, b, c, d).
-- It is a list of length 1 when a is true,
-- or a list of length 2 when a is false.
list :: UnionM [SymInteger]
list = mrgIf "a" (single ["b"]) (single ["c", "d"])

-- The UnionM container is a monad,
-- the branches ([b], [c, d]) can be extracted with do notation.
formula :: SymBool
formula = getSingle $ do -- getSingle converts UnionM SymBool to SymBool
  xs <- list
  mrgReturn $ head xs + last xs ==~ 5 -- ==~ is symbolic equality operator

main :: IO ()
main = do
  -- Call the z3 solver. `UnboundedReasoning` means that no approximation should be made
  res <- solveFormula (UnboundedReasoning z3) formula
  case res of
    Left err -> print err
    Right model -> do
      -- a model is an assignment of symbolic constants to concrete values
      -- Model {b -> 0 :: Integer, c -> 5 :: Integer, a -> False :: Bool, d -> 0 :: Integer}
      print model
      let conList = evaluateSymToCon model list :: [Integer]
      -- conList = [5,0]
      print conList
```

## Example

This one is adapted from [this blog
post](https://www.cs.utexas.edu/~bornholt/post/building-synthesizer.html) by
James Bornholt.

```haskell
-- allows the creation of symbolic constants from strings
{-# LANGUAGE OverloadedStrings #-}
-- type class derivations
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
-- type signature for locally defined variable
{-# LANGUAGE ScopedTypeVariables #-} 
-- used for Grisette SMT config
{-# LANGUAGE DataKinds #-}

module Main where

import Grisette
import GHC.Generics

data SProgram
  = SInt SymInteger
  | SPlus (UnionM SProgram) (UnionM SProgram)
  | SMul (UnionM SProgram) (UnionM SProgram)
  deriving (Generic, Show)
  deriving (GMergeable SymBool, GEvaluateSym Model) via (Default SProgram)

interpretU :: UnionM SProgram -> SymInteger
interpretU = getSingle . fmap interpret

interpret :: SProgram -> SymInteger
interpret (SInt x) = x
interpret (SPlus x y) = interpretU x + interpretU y
interpret (SMul x y) = interpretU x * interpretU y

exprHole :: [SProgram] -> Fresh (UnionM SProgram)
exprHole terminals = do
  l <- chooseFresh terminals
  r <- chooseFresh terminals
  chooseFresh [SPlus l r, SMul l r]

sketch :: SymInteger -> UnionM SProgram
sketch x = flip runFresh "sk" $ do
  p <- simpleFresh ()
  q <- simpleFresh ()
  let subexpr = exprHole $ SInt <$> [p, q, x]
  r <- SPlus <$> subexpr <*> subexpr
  return $ mrgReturn r

solverConfig :: GrisetteSMTConfig 5
solverConfig = BoundedReasoning boolector

ioPair :: IO ()
ioPair = do
  let f1 = interpretU (sketch 1) ==~ 10
  let f2 = interpretU (sketch 2) ==~ 20
  res <- solveFormula solverConfig (f1 &&~ f2)
  case res of
    Left err -> print err
    Right model -> do
      let x :: SymInteger = "x"
      print $ evaluateSym False model (sketch x)

cegis :: IO ()
cegis = do
  let x :: SymInteger = "x"
  res <- cegisFormula solverConfig x (interpretU (sketch x) ==~ x * 10)
  case res of
    Left err -> print err
    Right (_, model) ->
      print $ evaluateSym False model (sketch x)

main :: IO ()
main = do
  ioPair
  cegis
```
  
## Example

The main module, `Grisette`, includes almost everything you need to write a
symbolic evaluation tool with Grisette.

```haskell
-- Allows to create symbolic constants from strings.
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Grisette
```

The core of Grisette is symbolic value types. There are two types of symbolic
values in Grisette: solvable types and unsolvable types. Solvable types are
types that can be directly supported and solved by the underlying solver, for
example, `SymInteger` stands for mathematical unbounded integers.

In Grisette, a solvable value can hold three types of values:
- Concrete constants, e.g., 1, 2, 3, etc,
- Symbolic constants, e.g., `a`, `b`, `c`, etc,
- Symbolic formulas that are built with concrete constants, symbolic constants
  and symbolic operations, e.g., `a + b`, `a * b`, `c + 1`, etc.

A symbolic constant is a placeholder for a concrete value, and a solver can
assign value to the symbolic constant to satisfy a symbolic formula (make it
true). With the `OverloadedStrings` extension, we can create symbolic constants
from strings. The same string refers to the same symbolic constant.

Other types, e.g., lists, may not be directly supported by a solver. These types
are called unsolvable types and are represented with a monadic symbolic union
type `UnionM`. A symbolic union maintain a set of values and the path conditions
that lead to the values. The solver can be used to determine the truth of the
conditions, and the value for the branch.

In the following example, `UnionM [SymInteger]` represents a symbolic list of
symbolic integers. The `mrgIf` combinator is the symbolic counterpart of the
`if` statement, and it takes a symbolic Boolean value as the condition. The
result of the following code is a symbolic union indicating that the list is
`[b]` when `a` is true, and `[c, c + 1]` when `a` is false.

```haskell
-- A symbolic value with four symbolic constants
-- It is a list of length 1 when a is true,
-- or a list of length 2 when a is false.
list :: UnionM [SymInteger]
list = mrgIf "a" (single ["b"]) (single ["c", "c" + 1])
```

The `UnionM` container is monadic, and the branches can be extracted from the
container with the `do` notation. The `mrgReturn` combinator augments the
`return` combinator to help perform state merging, and the `getSingle`
combinator converts a `UnionM SymBool` to a `SymBool` formula. Note that in the
result there is no longer a list type, thus the result can be directly solved by
an SMT solver.

```haskell
-- The UnionM container is a monad,
-- the branches ([b], [c, d]) can be extracted with do notation.
--
-- The code models the symbolic evaluation of the following program:
-- xs = a
-- return $ x == [1, 2]
formula :: SymBool
formula = getSingle $ do -- getSingle converts UnionM SymBool to SymBool
  xs <- list
  mrgReturn $ xs ==~ [1,2] -- ==~ is symbolic equality operator
```

Then we can call a solver to determine the concrete assignments for the symbolic
values. The result of a solver call would be an error, or a model, which is
a mapping from the symbolic constants to concrete values. The mapping shows an
assignment of the symbolic constants that can make the formula `f` true.

The model can be directly printed as a mapping, or can be used to evaluate a
symbolic value. In the following example, the
`evaluateSymToCon` combinator evaluates `list` to a list of concrete integers. It
shows that under the model, `list` is `[1, 2]`, which is expected.

For example, you can evaluate a program sketch with a model to get the
synthesized program or evaluate some program inputs with a model to get a
counterexample in a verification task. Suppose that `list` is a program sketch,
and `f` specifies the desired behavior of a program. Then the evaluated program
sketch will be the synthesized program.


```haskell
main :: IO ()
main = do
  -- Call solver with solveFormula function.
  res <- solveFormula (UnboundedReasoning z3) formula
  -- The result of solveFormula is either a model or an error.
  case res of
    Left err -> print err
    Right model -> do
      -- The model can be printed as a mapping
      -- Model {c -> 1 :: Integer, a -> False :: Bool}
      -- meaning that f is True when a is False, c is 1
      print model
      -- The model can also be used to evaluate a back to a concrete list
      let conList = evaluateSymToCon model list :: [Integer]
      -- conList = [1,2]
      print conList
```

