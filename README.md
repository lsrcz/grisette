# Grisette

[![Haskell Tests](https://github.com/lsrcz/grisette/actions/workflows/test.yml/badge.svg)](https://github.com/lsrcz/grisette/actions/workflows/test.yml)
[![codecov](https://codecov.io/gh/lsrcz/grisette/branch/main/graph/badge.svg?token=MNDRFY2JEB)](https://codecov.io/gh/lsrcz/grisette)

Grisette is a symbolic evaluation library for Haskell. By translating
programs into constraints, Grisette can help the development of program
reasoning tools, including verification and synthesis.

## Features

- **Multi-path** symbolic evaluation with efficient (customizable) state merging.
- Symbolic evaluation is **purely functional**. The propagated symbolic value includes the assertion / error state of the execution, yet it is just a data structure. As a result, Grisette is a library that does not modify the Haskell compiler.  
- The separation of symbolic and concrete values is enforced with **static types**. These types help discover opportunities for partial evaluation as well as safe use of Haskell libraries. 

## Design and Benefits 

- Modular purely functional design, with a focus on composability.
  - Allows for symbolic evaluation of user-defined data structures / data
    structures from third-party libraries.
  - Allows for symbolic evaluation of error-handling code with user-defined
    error types.
  - Allows for memoization (tested and benchmarked) / parallelization (not
    tested and benchmarked yet) of symbolic evaluation.
- Core multi-path symbolic evaluation semantics modeled as a monad, allowing for
  easy integration with other monadic effects, for example:
  - error handling via `ExceptT`,
  - stateful computation via `StateT`,
  - unstructured control flow via `ContT`, etc.

## Installation

### Install Grisette

Grisette is available via
[Hackage](https://hackage.haskell.org/package/grisette). You can install it with
`cabal`:

```bash
$ cabal install grisette
```

However, Grisette is a library and is usually used as a dependency of other
packages. You can add it to your project's `.cabal` file:

```cabal
library
  ...
  build-depends: grisette >= 0.1 < 0.2
```

### Install SMT Solvers

To run the examples, you also need to install an SMT solver and make it 
available through `PATH`. We recommend that you start with
[Z3](https://github.com/Z3Prover/z3), as it supports all our examples and is
usually easier to install.
[Boolector](https://github.com/Boolector/boolector) is significantly more
efficient on some examples, but it does not support all of them.

#### Install Z3

On Ubuntu, you can install Z3 with:

```bash
$ apt update && apt install z3
```

On macOS, with [Homebrew](https://brew.sh/), you can install Z3 with:

```bash
brew install z3
```

You may also build Z3 from source, which may be more efficient on your system.
Please refer to the [Z3 homepage](https://github.com/Z3Prover/z3) for the build
instructions.

#### Install Boolector

Boolector from major package managers are usually outdated or inexist. We
recommend that you build Boolector from source with the CaDiCaL SAT solver,
which is usually more efficient on our examples.
Please refer to the [Boolector homepage](https://github.com/Boolector/boolector)
for the build instructions.

## Example

The following example uses Grisette to build a synthesizer of arithmetic programs. Given the input-output pair (2,5), the synthesizer outputs the program (\x -> x+3).  The example is adapted from [this blog
post](https://www.cs.utexas.edu/~bornholt/post/building-synthesizer.html) by
James Bornholt.


The example has three parts:
- We define the arithmetic language. The language is _symbolic_:
  - its syntax tree represents a set of concrete syntax trees. 
  - its interpreter accepts such symbolic syntax trees, interpreting at once all represented concrete syntax trees. 
- We define the candidate program space of the synthesizer by creating a particular symbolic syntax tree. The synthesizer will search the space of concrete trees for a solution. 
- We interpret the symbolic syntax tree and pass the resulting constraints to the solver. If a solution exists, the solver returns a concrete tree that agrees with the input-out example. 

given io (2,5)
-- would be nice to show how we get from syntax to space 
model <- solve $ interpretU (space 2) ==~ 5 
-- print the program given space and model 


This code block defines the symbolic syntax and the interpreter.
A single input program will be (\x -> E), where E is defined by the grammar E -> c | x | E + E | E * E. 

If we defined it with SPlus SProgram SProgram, we get a grammar. Each _instance_ is a single tree, such as XXX. 
If we instead define it as SPlus (UnionM SProgram) (UnionM SProgram), we get a data structure that represents sets of trees (with shared subtrees). The instance {...} + { x+y, x-y } represents four trees. 
The UnionM container represents a symbolic set of SPrograms. It's a select. Each solution from the solver will select exactly one member of this set. 

```haskell
-- The definition of the syntax tree for a symbolic program.
data SProgram
  -- SInt represent a **constant** in the syntax tree. A solver can find out what -- TODO: this is a variable leaf, not a ?? leaf
  -- value this constant should be.
  = SInt SymInteger
  -- SPlus and SMul are binary nodes whose children are **sets** of symbolic symbolic programs. A union is such a set. 
  | SPlus (UnionM SProgram) (UnionM SProgram)
  | SMul (UnionM SProgram) (UnionM SProgram)
  -- Generic helps us derive other type class instances for SProgram.
  deriving (Generic, Show)
  -- Some type classes provided by Grisette for building symbolic evaluation
  -- tools. See the documentation for more details.
  deriving (GMergeable SymBool, GEvaluateSym Model) via (Default SProgram)

-- An interpreter for SProgram.
-- The interpreter interprets all trees represented by an SProgram.
-- The result of the interpretation is a single symbolic formula (an SymInteger) that represents the evaluation of all trees. 
-- To switch among those results, the formula uses the symbolic variables that _select_ the members of the Unions.  -- TODO: call these guards? 
interpret :: SProgram -> SymInteger
interpret (SInt x) = x
interpret (SPlus x y) = interpretU x + interpretU y
interpret (SMul x y) = interpretU x * interpretU y

-- interpet the set of programs
interpretU :: UnionM SProgram -> SymInteger
interpretU = getSingle . fmap interpret -- the result is. union of formulas that can be merged (because they are SymIngeters). getSingle merges them into a single value. 
-- TODO: rename getSingle to suggest that merging happens here ?
```

Now we want to generat a particular instance of SProgram, such as XXX. This set of trees will prresnet the candidate space of the synthesizer. 
We need symbolic guards for the unions in the SProgram. These control what concrete tree the synthesizer selects. -- Say this in Part 1 (i. unions have guards that select; ii. the sovler finds the values for hte guards, thus selecting a tree). 

We are now ready to define the program space with the `Fresh` monad. The program space 
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
-- TODO: use this example above 
-- {1 or 2} {+ or *} {1 or 2}
--
-- It represents either
-- 1 + 1, 1 * 1, 1 + 2, 1 * 2, 2 + 1, 2 * 1, 2 + 2, or 2 * 2.
freshExpr :: [SProgram] -> Fresh (UnionM SProgram)
-- TODO: in the future, can we hide this under a rosette syntax-grammar like construct 
freshExpr terminals = do
  -- choose the left hand side operand
  l <- chooseFresh terminals
  -- choose the right hand side operand
  r <- chooseFresh terminals
  -- choose the operator
  chooseFresh [SPlus l r, SMul l r]
  
  -- TODO: the full code should create ASTs of a given depth k 
  -- say see full code for depth-k trees 

-- move this above freshExpr because it states the goal of our exercise here (it defiens the API). 
-- A program space:
-- \x -> {x or 1 or 2} {+ or *} {x or 1 or 2}
space :: SymInteger -> UnionM SProgram
space x = runFresh (freshExpr [SInt x, SInt 1, SInt 2]) "space"
```

Finally, we define the solver configuration and build the constraints
for the program space.
We call the solver to solve the constraints. The solution represents the program that satisfies the io pair(s). 

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
    -- Print an error message if no solution was found in the space. 
    Left err -> print err
    Right model -> do
      let x :: SymInteger = "x"
      -- Evaluate the program space to get the concrete program.
      print $ evaluateSym False model (space x) -- TODO: in the future, it would be nice to reduce ht eboiler plate here 
  where    
    -- make it top level since it's important 
    constraint :: [(SymInteger, SymInteger)] -> SymBool
    constraint [] = conc True   -- 'conc' type-converts from concrete to symbolic values 
    -- The '~' postfixed operators are the symbolic versions of the
    -- corresponding Haskell operators.
    constraint ((x, y) : xs) = interpretU (space x) ==~ y &&~ constraint xs

-- TODO: give this first 
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
