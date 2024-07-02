# Grisette

[![Haskell Tests](https://github.com/lsrcz/grisette/actions/workflows/test.yml/badge.svg)](https://github.com/lsrcz/grisette/actions/workflows/test.yml)
[![Hackage Version](https://img.shields.io/hackage/v/grisette)](https://hackage.haskell.org/package/grisette)
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/grisette)](https://packdeps.haskellers.com/feed?needle=grisette)

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
  build-depends: grisette >= 0.7 < 0.8
```

#### Quick start template with `stack new`

You can quickly start an stack-based Grisette project with `stack new`:

```bash
$ stack new <projectname> github:lsrcz/grisette
```

You can specify the resolver version with the parameters:

```bash
$ stack new a-new-project github:lsrcz/grisette -p "resolver:lts-22.27"
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

To run the examples, you need to install an SMT solver and make it available
through `PATH`. We recommend that you start with
[Z3](https://github.com/Z3Prover/z3), as it supports all our examples and is
usually easier to install. [Boolector](https://github.com/Boolector/boolector)
and its successor [Bitwuzla](https://github.com/bitwuzla/bitwuzla) are usually
significantly more efficient on bit vectors.

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

#### Install Boolector/Bitwuzla

Boolector/Bitwuzla from major package managers are usually outdated or inexist.
We recommend that you build them from source with the CaDiCaL SAT solver. Please
refer to the [Boolector homepage](https://github.com/Boolector/boolector) and
[Bitwuzla homepage](https://github.com/bitwuzla/bitwuzla) for the build
instructions.

## Example

The following example uses Grisette to build a synthesizer of arithmetic
programs. Given the input-output pair (2,5), the synthesizer may output the
program (\x -> x+3). The example is adapted from [this blog
post](https://www.cs.utexas.edu/~bornholt/post/building-synthesizer.html) by
James Bornholt.

The example has three parts:

- We define the arithmetic language. The language is _symbolic_:
  - its syntax tree represents a set of concrete syntax trees (i.e.,
    representing a program space), and
  - its interpreter accepts such symbolic syntax trees, and interpret all
    represented concrete syntax trees.
- We define the candidate program space of the synthesizer by creating a
  particular symbolic syntax tree. The synthesizer will search the space of
  concrete trees for a solution.
- We interpret the symbolic syntax tree and pass the resulting constraints to
  the solver. If a solution exists, the solver returns a concrete tree that
  agrees with the input-out example.

### Defining the Arithmetic Language

We will synthesize a single-input program `\x -> E` in this example. Here the
`E` is an expression type, and is defined by the following grammar.

```
E -> c      -- constant
   | x      -- value for input variable
   | E + E  -- addition
   | E * E  -- multiplication
```

The syntax defines how a concrete expression is represented. To synthesize a
program, we need to define ***symbolic*** program spaces. We do this with the
`Union` container provided by Grisette to represent choices within multiple ASTs
compactly in a single value.

```haskell
data SymExpr
  -- `SymConst` represents a constant in the syntax tree.
  --
  -- `SymConst 1` is the constant 1, while `SymConst "c1"` is a symbolic
  -- constant, representing a hole in the expression. The solver can be used to
  -- find out what the concrete value for a symbolic constant should be.
  = SymConst SymInteger
  -- `SymInput` is exactly the same as `SymConst`, but is for inputs. We
  -- separate them just for clarity.
  | SymInput SymInteger
  -- `SymAdd` and `SymMul` represent the addition and multiplication operators.
  --
  -- The children are **choices** from some symbolic expressions, which is
  -- represented by the `Union` monadic container.
  --
  -- The solver will try to pick one choice from them.
  | SymAdd (Union SymExpr) (Union SymExpr)
  | SymMul (Union SymExpr) (Union SymExpr)
  -- `Generic` helps us derive other type class instances for `SymExpr`.
  deriving stock (Generic, Show)
  -- Some type classes provided by Grisette for building symbolic evaluation
  -- tools. See the documentation for more details.
  deriving (Mergeable, EvalSym) via (Default SymExpr)

-- The following template haskell procedures can also derive the instances we
-- need.
-- derive ''SymExpr [''Generic, ''Show, ''Mergeable, ''EvalSym]
-- deriveAllExcept ''SymExpr [''Ord]
```

Some smart constructors help us build program spaces.

```haskell
-- A template haskell procedure generates smart constructors for
-- `Union SymExpr`.
--
-- >>> SymConst 1 :: SymExpr
-- SymConst 1
-- >>> mrgSymConst 1 :: Union SymExpr
-- {SymConst 1}
mkMergeConstructor "mrg" ''SymExpr
```

Then, the following code defines a program space `\x -> x + {x, c}`. Some
example programs in this space are `\x -> x + x`, `\x -> x + 1`, and `\x -> x +
2`. The solver will be used to choose the right hand side of the addition. It
may choose to use the input variable `x`, or synthesize a constant `c`.

```haskell
progSpace :: SymInteger -> SymExpr
progSpace x =
  SymAdd
    (mrgSymInput x)
    (mrgIf "choice" (mrgSymInput x) (mrgSymConst "c"))
```

We can then convert this program space to its logical encoding and reason about
it. This is done simply writing an interpreter to interpret all the expressions
represented by an `SymExpr` all at once.

The interpreter is similar to a concrete interpreter, except that the `onUnion`
combinator is used to lift the interpreter to work on `Union` values (a space of
expressions).

```haskell
interpret :: SymExpr -> SymInteger
interpret (SymConst x) = x
interpret (SymInput x) = x
interpret (SymAdd x y) = interpretSpace x + interpretSpace y
interpret (SymMul x y) = interpretSpace x * interpretSpace y

-- interpret a program space
interpretSpace :: Union SymExpr -> SymInteger
interpretSpace = onUnion interpret
```

We can then compose the interpreter with the program space to make it
executable.

```haskell
executableSpace :: Integer -> SymInteger
executableSpace = interpret . space . toSym
```

Then we can do synthesis. We call the program space on the input 2, and
construct the constraint that the result is equal to 5. We then call the solver
with the `solve` function. The solver finds a solution such that the condition
evaluates to true. It returns the solution as a *model*, which contains an
assignment to the symbolic constants (holes).

We can then get the synthesized program by evaluating the program space with the
model.

```haskell
example :: IO ()
example = do
  Right model <- solve (precise z3) $ executableSpace 2 .== 5
  -- result: SymPlus {SymInput x} {SymConst 3}
  print $ evalSym False model (progSpace "x")
  let synthesizedProgram :: Integer -> Integer =
        evalSymToCon model . executableSpace
  -- result: 13
  print $ synthesizedProgram 10
```

The complete code is at [examples/basic/Main.hs](examples/basic/Main.hs). More
examples are available in Grisette's [tutorials](tutorials).

## Documentation

- Haddock documentation at [grisette](https://hackage.haskell.org/package/grisette).
- A tutorial to Grisette is in the [tutorials](tutorials) directory. They are
  provided as jupyter notebooks with the [IHaskell](https://github.com/IHaskell/IHaskell) kernel.

## License

The Grisette library is distributed under the terms of the BSD3 license. The
[LICENSE](LICENSE) file contains the full license text.

## Note

### Floating-points

Grisette currently supports boolean, uninterpreted functions, bitvector,
integer, and floating point theories. However, if you want to use the floating
point theory, please make sure that you have the latest libBF (>=0.6.8) and sbv 
installed (>=10.10.6). We've detected and fixed several bugs that would prevent
a sound reasoning for floating points.

### Unified interfaces

Since 0.7.0.0, Grisette provides a [unified
interface](https://hackage.haskell.org/package/grisette/docs/Grisette-Unified.html)
to symbolic and concrete evaluations. GHC 9.0 or earlier, without the
[QuickLook](https://dl.acm.org/doi/10.1145/3408971) type inference algorithm for
impredicative types, may fail to resolve some constraints . You may need to
provide additional constraints in your code to help the compiler.

If you don't use the unified interface, Grisette should work fine with GHC 8.10
or later.

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
