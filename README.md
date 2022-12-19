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

```bash
$ cabal install grisette
```

You also need to install an SMT solver and make it available through `PATH`.
Grisette currently uses [sbv](http://leventerkok.github.io/sbv/) as the backend
and supports whatever solvers sbv supports, including
[Z3](http://github.com/Z3Prover/z3/wiki), [CVC4](http://cvc4.github.io/),
[Yices](http://yices.csl.sri.com/), [Boolector](http://fmv.jku.at/boolector/),
etc.

## Example

The example is adapted from [this blog
post](https://www.cs.utexas.edu/~bornholt/post/building-synthesizer.html) by
James Bornholt.
In the example, we build a simple program synthesizer with Grisette,
including the definition of the syntax tree, the interpreter, the program space, and
the solver calls.
The code is commented with explanations.

First, we can define the symbolic program type and its interpreter.
Unlike the concrete program type, a symbolic program can represent a whole
space of programs. The interpreter for it will interpret all the programs in
the program space at once, and generate a single symbolic value to represent
the results.

```haskell
-- The definition of the syntax tree for a symbolic program.
data SProgram
  -- SInt represent a constant in the syntax tree. A solver can find out what
  -- value this constant should be.
  = SInt SymInteger
  -- SPlus and SMul represent the addition and multiplication operators.
  -- They take two arguments, which are also symbolic programs.
  -- The UnionM container allows us to represent a space of possible programs
  -- rather than a single program as operands.
  | SPlus (UnionM SProgram) (UnionM SProgram)
  | SMul (UnionM SProgram) (UnionM SProgram)
  -- Generic helps us derive other type class instances for SProgram.
  deriving (Generic, Show)
  -- Some type classes provided by Grisette for building symbolic evaluation
  -- tools. See the documentation for more details.
  deriving (GMergeable SymBool, GEvaluateSym Model) via (Default SProgram)

-- An interpreter for SProgram.
-- The interpreter would interpret all the programs in a program space at
-- once, and generate a single symbolic formula to represent all the results.
interpretU :: UnionM SProgram -> SymInteger
interpretU = getSingle . fmap interpret

interpret :: SProgram -> SymInteger
interpret (SInt x) = x
interpret (SPlus x y) = interpretU x + interpretU y
interpret (SMul x y) = interpretU x * interpretU y
```

Then we can define the program space with the `Fresh` monad. The program space 
is represented as a symbolic program. The solver will figure out what program
in the space meets the specifications.

```haskell
-- A function that generates a program space.
-- The result is maintained in the Fresh monad, which allows us to generate
-- fresh symbolic variables. These fresh symbolic variables will be instantiated
-- with 'runFresh'.
--
-- For example, when 'freshExpr' is called with [SInt 1, SInt 2], it will
-- generate a program space as follows:
-- {1 or 2} {+ or *} {1 or 2}
--
-- It represents either
-- 1 + 1, 1 * 1, 1 + 2, 1 * 2, 2 + 1, 2 * 1, 2 + 2, or 2 * 2.
freshExpr :: [SProgram] -> Fresh (UnionM SProgram)
freshExpr terminals = do
  -- choose the left hand side operand
  l <- chooseFresh terminals
  -- choose the right hand side operand
  r <- chooseFresh terminals
  -- choose the operator
  chooseFresh [SPlus l r, SMul l r]

-- A program space:
-- \x -> {x or 1 or 2} {+ or *} {x or 1 or 2}
space :: SymInteger -> UnionM SProgram
space x = runFresh (freshExpr [SInt x, SInt 1, SInt 2]) "space"
```

Finally, we can define the solver configuration and build the constraints
for the program space.
We can call the solver to solve the constraints, and we will get a
synthesizer from I/O pairs.

```haskell
-- The solver configuration. We use the bounded reasoning mode with Boolector
-- for faster (but may be unsound) solving.
solverConfig :: GrisetteSMTConfig 5
solverConfig = BoundedReasoning boolector

-- A function that synthesizes programs within the search space given some
-- input-output pairs.
ioPair :: [(Integer, Integer)] -> IO ()
ioPair pairs = do
  -- Call the solver. The result may be an error or a model.
  -- Here we use the 'constraint' function to construct the constraint.
  -- The 'constraint' function takes a list of symbolic input-output pairs and
  -- we use the 'toSym' function to convert the concrete input-output pairs to
  -- symbolic ones.
  res <- solveFormula solverConfig (constraint $ toSym pairs)
  case res of
    -- Print the error if the solver fails.
    Left err -> print err
    Right model -> do
      let x :: SymInteger = "x"
      -- Evaluate the program space to get the concrete program.
      print $ evaluateSym False model (space x)
  where
    constraint :: [(SymInteger, SymInteger)] -> SymBool
    -- The conc function converts a concrete Boolean to a symbolic Boolean.
    constraint [] = conc True
    -- The '~' postfixed operators are the symbolic versions of the
    -- corresponding Haskell operators.
    constraint ((x, y) : xs) = interpretU (space x) ==~ y &&~ constraint xs

main :: IO ()
main = do
  -- Call the synthesizer. The printed result could be verbose.
  -- Grisette provides functionalities to convert it to easier-to-print
  -- programs via the 'ToCon' type class. Please check the documentation for 
  -- more details.
  ioPair [(1, 1), (2, 4), (3, 9)]
  -- UMrg (Single (SMul (UMrg (Single (SInt x))) (UMrg (Single (SInt x))))) 
  -- The results means the program \x -> x * x
  ioPair [(1, 2), (2, 4), (3, 6)]
  -- UMrg (Single (SPlus (UMrg (Single (SInt x))) (UMrg (Single (SInt x)))))
  -- The results means the program \x -> x + x
```

## Documentation

- Haddock documentation: [grisette-core](https://hackage.haskell.org/package/grisette-core) (core constructs), [grisette-symir](https://hackage.haskell.org/package/grisette-symir) (solvable type/symbolic IR), [grisette-backend-sbv](https://hackage.haskell.org/package/grisette-backend-sbv) (solver backend), [grisette](https://hackage.haskell.org/package/grisette) (aggregated package, exports constructs from the other three packages).
- Grisette essentials (WIP).
- Grisette tutorials (WIP).

## License
The Grisette library is distributed under the terms of the BSD3 license. The
[LICENSE](LICENSE) file contains the full license text.

## Citing Grisette
If you use Grisette in your research, please use the following bibtex entry:

```bibtex
TODO
```
