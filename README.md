# Grisette

[![Haskell Tests](https://github.com/lsrcz/grisette/actions/workflows/test.yml/badge.svg)](https://github.com/lsrcz/grisette/actions/workflows/test.yml)
[![Hackage Version](https://img.shields.io/hackage/v/grisette)](https://hackage.haskell.org/package/grisette)

Grisette is a symbolic evaluation library for Haskell.
By translating programs into SMT constraints, Grisette can help the development
of program reasoning tools, including verification and synthesis.

For a detailed description of the system, please refer to our POPL'23 paper
[Grisette: Symbolic Compilation as a Functional Programming Library](https://lsrcz.github.io/files/POPL23.pdf).

## Design and Benefits

- **Separate the concern** of problem modeling and symbolic compilation. Users
  only need to focus on modeling the problem and write interpreters, and the
  symbolic compilation algorithms are provided by Grisette.
- **Supports rich theories** including booleans, uninterpreted functions,
  bitvectors, integers, real numbers, and floating points.
- **Multi-path symbolic evaluation** with efficient state merging, suitable for
  whole program verification, program synthesis, and other symbolic reasoning
  tasks.
- **Modular purely functional design**, with a focus on composability.
  - Use our familiar Haskell facilities like `Either` to maintain exceptions
    (e.g., assertions and assumptions).
  - Allows for symbolic evaluation of user-defined data structures / data
    structures from third-party libraries.
  - Allows for memoization / parallelization of symbolic evaluation.
- **Core multi-path symbolic evaluation semantics modeled as a monad**, allowing
  for easy integration with other monadic effects, for example:
  - error handling via `ExceptT`,
  - stateful computation via `StateT`,
  - unstructured control flow via `ContT`, etc.

## Installation

### Install Grisette

Grisette is available on [Hackage](https://hackage.haskell.org/package/grisette)
and Stackage. You can add it to your project with `cabal`, and we also provided
a stack template for quickly starting a new project with Grisette.

#### Manually writing cabal file

Grisette is a library and is usually used as a dependency of other packages. You
can add it to your project's `.cabal` file:

```cabal
library
  ...
  build-depends: grisette >= 0.10 < 0.11
```

#### Using stack

Note: Grisette on Stackage is currently outdated. Please make sure to use
`extra-deps` to get the latest version of Grisette from stackage. In your
`stack.yaml` file, add:

```yaml
extra-deps:
  - grisette-0.10.0.0
```

and in your `package.yaml` file:

```yaml
dependencies:
  - grisette >= 0.10 < 0.11
```

#### Quick start template with `stack new`

You can quickly start an stack-based Grisette project with `stack new`:

```bash
$ stack new <projectname> github:lsrcz/grisette
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

The following example uses Grisette to build a symbolic domain-specific language
for boolean and integer expressions.

We will
- define the *syntax* and *semantics* of an arithmetic language, and
- build a *verifier* to check if a given arithmetic expression is equivalent to
  another, and
- build a *synthesizer* to find an arithmetic expression that is equivalent to
  a given expression.

### Defining the Syntax

Our language is a simple boolean and integer expression language, following the
grammar:

```haskell
Expr -> IntExpr | BoolExpr
IntExpr -> IntVal int
         | Add IntExpr IntExpr
         | Mul IntExpr IntExpr
BoolExpr -> BoolVal bool
          | BAnd BoolExpr BoolExpr
          | BOr BoolExpr BoolExpr
          | Eq Expr Expr
```

A symbolic expression can be represented in Grisette as a GADT as follows. In
the GADT,

- `SymInteger` and `SymBool` are symbolic (primitive) types, and they represent
  SMT terms of integer and boolean theories, respectively.
- `Union` represents choices of symbolic expressions, and we introduce it to
  represent program spaces and allow the synthesizer to choose operands from
  different symbolic expressions.
- `BasicSymPrim` is a constraint that contains all the symbolic primitive types
  that Grisette supports, including `SymInteger` and `SymBool`.

```haskell
data Expr a where
  IntVal :: SymInteger -> IntExpr
  BoolVal :: SymBool -> BoolExpr
  Add :: UIntExpr -> UIntExpr -> IntExpr
  Mul :: UIntExpr -> UIntExpr -> IntExpr
  BAnd :: UBoolExpr -> UBoolExpr -> BoolExpr
  BOr :: UBoolExpr -> UBoolExpr -> BoolExpr
  Eq :: (BasicSymPrim a) => UExpr a -> UExpr a -> BoolExpr

type IntExpr = Expr SymInteger
type BoolExpr = Expr SymBool
type UExpr a = Union (Expr a)
type UIntExpr = UExpr SymInteger
type UBoolExpr = UExpr SymBool
```

To make this GADT works well with Grisette, we need to derive some instances and
get some smart constructors:

- `deriveGADTAll` derives all the instances related to Grisette, and
- `makeSmartCtor` generates smart constructors for the GADT.

```haskell
deriving instance Show (Expr a)
deriveGADTAll ''Expr
makeSmartCtor ''Expr

> intVal 1 :: UIntExpr -- smart constructor for IntVal in Unions
{IntVal 1}
-- Add takes two UIntExprs, use the smart constructors
> Add (intVal "a") (intVal 1)
Add {IntVal a} {IntVal 1}
```

The introduction of `Union` allows us to represent choices of expressions, and
the following code chooses between `a + 2` or `a * 2`. A synthesizer can then pick
true or false for the `choice` variable to decide which expression to pick. If
the synthesizer picks true, the result is `a + 2`; otherwise, it is `a * 2`.

```haskell
add2 = add (intVal "a") (intVal 2)
mul2 = mul (intVal "a") (intVal 2)
> mrgIf "choice" add2 mul2 :: UIntExpr
{If choice {Add {IntVal a} {IntVal 2}} {Mul {IntVal a} {IntVal 2}}}
```

### Defining the Semantics
The semantics of the expressions can be defined by the following interpreter.
Grisette provides various combinators for working with symbolic values. In the
interpreter, the `.#` operator is very important. It conceptually

- extracts all the choices from the `Union` container,
- apply the `eval` function to each choice, and
- merge the results into a single value.

```haskell
eval :: Expr a -> a
eval (IntVal a) = a
eval (BoolVal a) = a
eval (Add a b) = eval .# a + eval .# b
eval (Mul a b) = eval .# a * eval .# b
eval (BAnd a b) = eval .# a .&& eval .# b
eval (BOr a b) = eval .# a .|| eval .# b
eval (Eq a b) = eval .# a .== eval .# b
```

There are other operators like `.==`, `.&&`, `.||`, etc. These operators are
provided by Grisette and have symbolic semantics. They construct constraints
instead of evaluating to a concrete value.

We may also write `eval` with do-notations as `Union` is a monad. Please refer
to the [tutorials](tutorials) for more details.

### Get a verifier
With the syntax and semantics defined, we can build a verifier to check if two
expressions are equivalent. This can be done by checking if there exists a
counter-example that falsifies the equivalence of the two expressions.

In the following code, we verify that $a+b$ and $b+a$ are equivalent, as there
does not exist a counter-example that makes the two expressions evaluate to
different values.

```haskell
lhs = Add (intVal "a") (intVal "b")
rhs = Add (intVal "b") (intVal "a")
rhs2 = Add (intVal "a") (intVal "a")

> solve z3 $ eval lhs ./= eval rhs
Left Unsat
```

In the following code, we verify that $a+b$ and $a+a$ are not equivalent, as
there exists a counter-example that makes the two expressions evaluate to
different values. The counter-example is $a=0$, $b=1$, such that $a+b=1$ and
$a+a=0$.

``` haskell
> solve z3 $ eval lhs ./= eval rhs2
Right (Model {a -> 0 :: Integer, b -> 1 :: Integer})
```

### Get a synthesizer
We can also build a synthesizer using the built-in CEGIS algorithm in Grisette.
Given a target expression, we can synthesize an expression using a sketch with
"symbolic holes" that is equivalent to the target expression.

In the following code, we synthesize an expression that is equivalent to $a+a$
using a sketch with a "symbolic hole" $c$. The `cegisForAll` function treats all
the variables in the sketch but not in the target expression as holes to fill
in.

```haskell
target = Add (intVal "a") (intVal "a")
sketch = Mul (intVal "a") (intVal "c")

> cegisForAll z3 target $ cegisPostCond $ eval target .== eval sketch
([],CEGISSuccess (Model {c -> 2 :: Integer}))
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

Grisette is fully compatible with GHC 9.6+, and works in most cases with GHC
8.10+.

### CLC proposal #10
As the type classes provided by Grisette implements
[CLC proposal #10](https://github.com/haskell/core-libraries-committee/issues/10),
which requires `base-4.18.0.0` to work reliably, Grisette is fully compatible
with GHC 9.6.
You may experience instance resolution failure when using older GHCs in the
client code (Grisette itself is buildable against GHC 8.10+ with some tricks).

### Quantifiers

Grisette currently supports universal and existential quantifiers $\forall$ and
$\exists$, but only when building with sbv >= 10.1. This also means that you
need to use GHC >= 9.2.

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
impredicative types, may fail to resolve some constraints. You may need to
provide additional constraints in your code to help the compiler.

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
