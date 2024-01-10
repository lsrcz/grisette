# Grisette

[![Haskell Tests](https://github.com/lsrcz/grisette/actions/workflows/test.yml/badge.svg)](https://github.com/lsrcz/grisette/actions/workflows/test.yml)

Grisette is a symbolic evaluation library for Haskell. By translating
programs into constraints, Grisette can help the development of program
reasoning tools, including verification and synthesis.

For a detailed description of the system, please refer to our POPL'23 paper
[Grisette: Symbolic Compilation as a Functional Programming Library](https://lsrcz.github.io/files/POPL23.pdf).

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
[Hackage](https://hackage.haskell.org/package/grisette). You can add it to your
project with `cabal`, and we also provided a stack template for quickly starting a
new project with Grisette.

#### Manually writing cabal file

Grisette is a library and is usually used as a dependency of other
packages. You can add it to your project's `.cabal` file:

```cabal
library
  ...
  build-depends: grisette >= 0.4.1 < 0.5
```

#### Quick start template with `stack new`

You can quickly start an stack-based Grisette project with `stack new`:

```bash
$ stack new <projectname> github:lsrcz/grisette
```

You can specify the resolver version with the parameters:

```bash
$ stack new a-new-project github:lsrcz/grisette -p "resolver:lts-19.33"
```

For more details, please see the
[template file](https://github.com/lsrcz/stack-templates/blob/main/grisette.hsfiles) and the
[documentation for stack templates](https://docs.haskellstack.org/en/stable/templates_command/).

You can test your installation by running the following command:

```bash
$ stack run app
```

The command assumes a working [Z3](https://github.com/Z3Prover/z3) available
through `PATH`. You can install it with the instructions below.

### Install SMT Solvers

To run the examples, you also need to install an SMT solver and make it
available through `PATH`. We recommend that you start with
[Z3](https://github.com/Z3Prover/z3), as it supports all our examples and is
usually easier to install.
[Boolector](https://github.com/Boolector/boolector) is significantly more
efficient on some examples, but it does not support all of the examples.

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

The following example uses Grisette to build a synthesizer of arithmetic programs. Given the input-output pair (2,5), the synthesizer may output the program (\x -> x+3). The example is adapted from [this blog
post](https://www.cs.utexas.edu/~bornholt/post/building-synthesizer.html) by
James Bornholt.

The example has three parts:

- We define the arithmetic language. The language is _symbolic_:
  - its syntax tree represents a set of concrete syntax trees, and
  - its interpreter accepts such symbolic syntax trees, and interprete at once all represented concrete syntax trees.
- We define the candidate program space of the synthesizer by creating a particular symbolic syntax tree. The synthesizer will search the space of concrete trees for a solution.
- We interpret the symbolic syntax tree and pass the resulting constraints to the solver. If a solution exists, the solver returns a concrete tree that agrees with the input-out example.

### Defining the Arithmetic Language

We will synthesize single-input programs in this example.
A single input program will be `\x -> E`, where `E` is an expression defined by
the following grammar:

```
E -> c      -- constant
   | x      -- value for input variable
   | E + E  -- addition
   | E * E  -- multiplication
```

The syntax defines how a concrete expression is represented. To synthesis a
program, we need to define symbolic program spaces. This relies on the `UnionM`
container provided by the library to represent multiple ASTs compactly in a
single value.

To make this expression space type work with Grisette, a set of type classes
should be derived. This includes `Mergeable`, `EvaluateSym`, etc.
The `Mergeable` type classes allows to represent multiple ASTs compactly in a
`UnionM`, while the `EvaluateSym` type class allows to evaluate it given a model
returned by a solver to replace the symbolic holes inside to concrete values.

```haskell
data SExpr
  -- `SConst` represents a constant in the syntax tree.
  --
  -- `SConst 1` is the constant 1, while `SConst "c1"` is a symbolic constant,
  -- and the solver can be used to find out what the concrete value should be.
  = SConst SymInteger
  -- `SInput` is very similar to the `SConst`, but is for inputs. We separate
  -- these two mainly for clarity.
  | SInput SymInteger
  -- `SPlus` and `SMul` represent the addition and multiplication operators.
  --
  -- The children are **sets** of symbolic programs. Here `UnionM`s are such
  -- sets.
  --
  -- The solver will try to pick one concrete program from the set of programs.
  | SPlus (UnionM SExpr) (UnionM SExpr)
  | SMul (UnionM SExpr) (UnionM SExpr)
  -- `Generic` helps us derive other type class instances for `SExpr`.
  deriving stock (Generic, Show)
  -- Some type classes provided by Grisette for building symbolic evaluation
  -- tools. See the documentation for more details.
  deriving (Mergeable, EvaluateSym)
    via (Default SExpr)

-- A template haskell procedure to help the construction of `SExpr` sets.
--
-- >>> SConst 1 :: SExpr
-- SConst 1
-- >>> mrgSConst 1 :: UnionM SExpr
-- UMrg (Single (SConst 1))
$(makeUnionWrapper "mrg" ''SExpr)
```

Then we can define the program space.
The following code defines a program space `\x -> x + {x, c}`. Some example
programs in this space are `\x -> x + x`, `\x -> x + 1`, and `\x -> x + 2`.
The solver will be used to choose the right hand side of the addition. It may
choose to use the input variable `x`, or synthesize a constant `c`.

```haskell
space :: SymInteger -> SExpr
space x = SPlus
  (mrgSInput x)
  (mrgIf "choice" (mrgSInput x) (mrgSConst "c"))
```

We then need to convert this program space to its logical encoding, and we do this by writing an interpreter to interpret all the
expressions represented by an `SExpr` all at once. The interpreter looks very
similar to a normal interpreter, except that the `onUnion` combinator is used
to lift the interpreter to work on `UnionM` values.

```haskell
interpret :: SExpr -> SymInteger
interpret (SInt x) = x
interpret (SPlus x y) = interpretU x + interpretU y
interpret (SMul x y) = interpretU x * interpretU y

-- interpet a set of programs
interpretU :: UnionM SExpr -> SymInteger
interpretU = onUnion interpret
```

And we can compose the interpreter with the program space to get it executable.

```haskell
executableSpace :: Integer -> SymInteger
executableSpace = interpret . space . toSym
```

Then we can do synthesis. We call the program space on the input 2, and construct the constraint that the result is equal to 5. We then call the solver with the `solve` function. The solver is able to find a solution, and it will return the assignments to the symbolic constants as a model.

We can then use the model to evaluate the program space, and get the synthesized program.

```haskell
example :: IO ()
example = do
  Right model <- solve (UnboundedReasoning z3) $ executableSpace 2 ==~ 5
  print $ evaluateSym False model (space "x")
  -- result: SPlus {SInput x} {SConst 3}
  let synthesizedProgram :: Integer -> Integer =
        evaluateSymToCon model . executableSpace
  print $ synthesizedProgram 10
  -- result: 13
```

For more details, please refer to [the Grisette examples](https://github.com/lsrcz/grisette-examples) (WIP).

## Documentation

- Haddock documentation at [grisette](https://hackage.haskell.org/package/grisette).
- Grisette essentials (WIP).
- Grisette tutorials (WIP).

## License

The Grisette library is distributed under the terms of the BSD3 license. The
[LICENSE](LICENSE) file contains the full license text.

## Citing Grisette

If you use Grisette in your research, please use the following bibtex entry:

```bibtex
@article{10.1145/3571209,
author = {Lu, Sirui and Bod\'{\i}k, Rastislav},
title = {Grisette: Symbolic Compilation as a Functional Programming Library},
year = {2023},
issue_date = {January 2023},
publisher = {Association for Computing Machinery},
address = {New York, NY, USA},
volume = {7},
number = {POPL},
url = {https://doi.org/10.1145/3571209},
doi = {10.1145/3571209},
abstract = {The development of constraint solvers simplified automated reasoning about programs and shifted the engineering burden to implementing symbolic compilation tools that translate programs into efficiently solvable constraints. We describe Grisette, a reusable symbolic evaluation framework for implementing domain-specific symbolic compilers. Grisette evaluates all execution paths and merges their states into a normal form that avoids making guards mutually exclusive. This ordered-guards representation reduces the constraint size 5-fold and the solving time more than 2-fold. Grisette is designed entirely as a library, which sidesteps the complications of lifting the host language into the symbolic domain. Grisette is purely functional, enabling memoization of symbolic compilation as well as monadic integration with host libraries. Grisette is statically typed, which allows catching programming errors at compile time rather than delaying their detection to the constraint solver. We implemented Grisette in Haskell and evaluated it on benchmarks that stress both the symbolic evaluation and constraint solving.},
journal = {Proc. ACM Program. Lang.},
month = {jan},
articleno = {16},
numpages = {33},
keywords = {State Merging, Symbolic Compilation}
}
```
