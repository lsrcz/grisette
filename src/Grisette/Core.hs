{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Trustworthy #-}
-- Disable this warning because we are re-exporting things.
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- |
-- Module      :   Grisette.Core
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core
  ( -- * Organization of the module

    -- | The module is organized by first introducing symbolic values, which
    -- includes [solvable (primitive)](#g:solvable) types and
    -- [unsolvable (non-primitive)](#g:unsolvable) types.
    -- Solvable types are those directly supported by the underlying solver.
    -- Examples include symbolic Booleans ('Grisette.SymPrim.SymBool'), integers
    -- ('Grisette.SymPrim.SymInteger'), and bit vectors
    -- (@'Grisette.SymPrim.SymWordN' n@). Other types are unsolvable. They need
    -- 'Union' monadic container and 'Mergeable' instances to be merged.
    --
    -- We will elaborate the internal details of the symbolic values in the
    -- documentation, but it is likely that you can skip them and directly go
    -- through the APIs and the operations to start using Grisette.
    --
    -- [Various operations](#g:symops) are provided for
    -- manipulating symbolic values, including solvable and unsolvable types.
    -- Apart from the basic operations for each type of symbolic values, we also
    -- provide generic operations for:
    --
    -- * Symbolic equality and comparison ('SymEq', 'SymOrd')
    -- * Conversion between concrete and symbolic values ('ToCon', 'ToSym')
    -- * Merging of symbolic values ('Mergeable', 'SimpleMergeable', 'TryMerge')
    -- * Symbolic branching ('SymBranching')
    --
    -- Additional tools for building symbolic evaluation based applications are
    -- also provided:
    --
    -- * Pretty printing (e.g., 'PPrint')
    -- * Symbolic generation, or generating fresh symbolic values (e.g.,
    --   'GenSym')
    -- * Error handling (e.g., 'symAssert')
    -- * Solver backend interface (e.g., 'solve', 'cegis')
    -- * Substitutions for symbolic values given the solving results (e.g.,
    --  'ExtractSym', 'EvalSym', 'SubstSym').
    --
    -- Finally [some utilities](#g:utils), including helpers for
    -- deriving instances for the type classes are provided in this module.

    -- * Note for the examples in the documentation

    -- | The examples may assume a [z3](https://github.com/Z3Prover/z3) solver
    -- available in @PATH@.

    -- * Introduction to symbolic values in Grisette

    -- ** Symbolic evaluation primer

    -- | Grisette is a tool for performing symbolic evaluation on programs. With
    -- Grisette, you can construct your own symbolic DSL, and get the symbolic
    -- evaluator for it without the need of manually implementing the symbolic
    -- evaluation algorithms.
    --
    -- Symbolic evaluation is a technique that allows us to analyze all
    -- possible runs of a program by representing inputs (or the program
    -- itself) as symbolic values and evaluating the program to produce
    -- symbolic constraints over these values.
    -- These constraints can then be used to find concrete assignments to the
    -- symbolic values that meet certain criteria, such as triggering a bug in
    -- a verification task or satisfying a specification in a synthesis task.
    --
    -- For example, in the following pseudo code, suppose that we want to know
    -- whether the assertion at the end of the code will hold for all possible
    -- inputs:
    --
    -- > a = input()
    -- > b = 4
    -- > if (a < 5) {
    -- >   b -= a
    -- > }
    -- > assert (b > 0)
    --
    -- We can represent the input @a@ as a symbolic integer, and do symbolic
    -- evaluation for it. By exploring __all__ possible program paths, we will
    -- know that the result for the condition in the assertion would then be a
    -- symbolic formula:
    --
    -- > (ite (< a 5) (> (- 4 a) 0) (> 4 0)).
    --
    -- The original program violates the assertion when this formula is false.
    -- We can use a solver to ask whether this could happen, and solver will
    -- tell us that when @a@ is 4, the assertion will not hold.
    --
    -- Our resulting formula captures the information about all possible program
    -- runs. One of the challenges of symbolic evaluation is the well-known path
    -- explosion problem, which occurs when traditional symbolic evaluation
    -- techniques evaluate and reason about the possible paths one-by-one.
    -- This can lead to exponential growth in the number of paths that need to
    -- be analyzed, making the process impractical for many tasks.
    --
    -- To address this issue, Grisette uses a technique called "path merging".
    -- In path merging, symbolic values are merged together in order to reduce
    -- the number of paths that need to be explored simultaneously. This can
    -- significantly improve the performance of symbolic evaluation, especially
    -- for tasks such as program synthesis that are prone to the path explosion
    -- problem. This path merging is done automatically by Grisette, with a set
    -- of symbolic values.
    --
    -- In Grisette, we make a distinction between two kinds of symbolic values:
    -- __/solvable types/__ and __/unsolvable types/__. These two types of
    -- values are merged differently.

    -- ** Solvable types (solver-primitive types) #solvable#

    -- | __/Solvable types/__ are types that are directly supported by the
    -- underlying solver and are represented as symbolic formulas. Examples
    -- include symbolic Booleans, integers and bit vectors. The values of
    -- solvable types can be __/fully/__ merged into a single value, which
    -- can significantly reduce the number of paths that need to be explored.
    --
    -- For example, there are three symbolic Booleans @a@, @b@ and @c@, in the
    -- following code, and a symbolic Boolean @x@ is defined to be the result of
    -- a conditional expression:
    --
    -- > x = if a then b else c -- pseudo code, not real Grisette code
    --
    -- The result would be a single symbolic integer formula:
    --
    -- >>> x = symIte "a" "b" "c" :: SymInteger -- real Grisette code
    -- >>> x
    -- (ite a b c)
    --
    -- If we further add 1 to @x@, we will not have to split to two paths,
    -- but we can directly construct a formula with the merged value:
    --
    -- >>> x + 1
    -- (+ 1 (ite a b c))

    -- ** Unsolvable types (non-solver-primitive types) #unsolvable#

    -- | __/Unsolvable types/__, on the other hand, are types that are not
    -- directly supported by the solver and cannot be fully merged into a
    -- single formula. Examples include lists, algebraic data types, and
    -- concrete Haskell integer types. To symbolically evaluate values with
    -- unsolvable types, Grisette provides a symbolic union container ('Union'),
    -- which is a set of values guarded by their path conditions.
    -- The values of unsolvable types are __/partially/__ merged in a symbolic
    -- union, which is essentially an if-then-else tree.
    --
    -- For example, assume that the lists have the type
    -- @['Grisette.SymPrim.SymBool']@.
    -- In the following example, the result shows that @[b]@ and @[c]@ can be
    -- merged together in the same symbolic union because they have the same
    -- length:
    --
    -- > x = if a then [b] else [c] -- pseudo code, not real Grisette code
    --
    -- Or, in real Grisette code:
    --
    -- >>> mrgIf "a" (return ["b"]) (return ["c"]) :: Union [SymBool]
    -- {[(ite a b c)]}
    --
    -- Note that we are using 'mrgIf' instead of 'symIte' for 'Union's.
    --
    -- The second example shows that @[b]@ and @[c,d]@ cannot be merged
    -- together because they have different lengths:
    --
    -- > if a then [b] else [c, d] -- pseudo code, not real Grisette code
    --
    -- In real Grisette code:
    --
    -- >>> mrgIf "a" (return ["b"]) (return ["c", "d"]) :: Union [SymBool]
    -- {If a [b] [c,d]}
    --
    -- The following example is more complicated. To make the merging
    -- efficient, Grisette would maintain a representation invariant of the
    -- symbolic unions. In this case, the lists with length 1 should be placed
    -- at the then branch, and the lists with length 2 should be placed at the
    -- else branch.
    --
    -- > x = if a1 then [b] else if a2 then [c, d] else [f] -- pseudo code, not real Grisette code
    --
    -- In real Grisette code:
    --
    -- >>> x = mrgIf "a1" (return ["b"]) (mrgIf "a2" (return ["c", "d"]) (return ["f"])) :: Union [SymBool]
    -- >>> x
    -- {If (|| a1 (! a2)) [(ite a1 b f)] [c,d]}
    --
    -- When we further operate on this partially merged values,
    -- we will need to split into multiple paths. For example, when we apply
    -- 'head' onto the last result, and we will distribute 'head' among the
    -- branches:
    --
    -- > head x -- pseudo code, not real Grisette code
    -- > -- intermediate result: If (|| a1 (! a2)) (Single (head [(ite a1 b f)])) (Single (head [c,d]))
    -- > -- intermediate result: If (|| a1 (! a2)) (Single (ite a1 b f)) (Single c)
    --
    -- This \"path splitting\" is done with the monad instance of 'Union'. The
    -- result is then merged into a single formula, and further operation on it
    -- won't have to split into multiple paths:
    --
    -- >>> x = mrgIf "a1" (return ["b"]) (mrgIf "a2" (return ["c", "d"]) (return ["f"])) :: Union [SymBool]
    -- >>> :{
    -- do
    --   v <- x             -- Path splitted with the (>>=)
    --   mrgReturn $ head v -- mrgReturn helps with the merging
    -- :}
    -- {(ite (|| a1 (! a2)) (ite a1 b f) c)}
    --
    -- Generally, merging the possible branches in a symbolic union can reduce
    -- the number of paths to be explored in the future, but can make the path
    -- conditions larger and harder to solve. To have a good balance between
    -- this, Grisette has built a hierarchical merging algorithm, which is
    -- configurable via 'MergingStrategy'. For algebraic data types, we have
    -- prebuilt merging strategies via the derivation of the 'Mergeable' type
    -- class (Also see 'GMergeable' for detail of derivation). You only need to
    -- know the details of the merging algorithm if you are going to work with
    -- complex non-algebraic data types, or you'd like to configure the merging
    -- based on your domain knowledge to tweak the performance.

    -- * Solvable types

    -- |
    -- Grisette currently provides an implementation for the following solvable
    -- types:
    --
    -- * 'Grisette.SymPrim.SymBool' ('Bool', symbolic Booleans)
    -- * 'Grisette.SymPrim.SymInteger' ('Integer', symbolic unbounded integers)
    -- * @'Grisette.SymPrim.SymIntN' n@ (@'Grisette.SymPrim.IntN' n@, symbolic
    --   signed bit vectors of bit width @n@)
    -- * @'Grisette.SymPrim.SymWordN' n@ (@'Grisette.SymPrim.WordN' n@, symbolic
    --   unsigned bit vectors of bit width @n@)
    -- * @'Grisette.SymPrim.SymFP' eb sb@ (@'Grisette.SymPrim.FP' eb sb@,
    --   symbolic IEEE-754 floating point numbers with @eb@ exponent bits and
    --   @sb@ significand bits)
    -- * 'Grisette.SymPrim.SymAlgReal' ('Grisette.SymPrim.AlgReal'), symbolic
    --   algebraic real numbers.
    -- * @'Grisette.SymPrim.SymBool' t'Grisette.SymPrim.=~>' 'Grisette.SymPrim.SymBool'@
    --   (@'Bool' t'Grisette.SymPrim.=->' 'Bool'@, symbolic functions,
    --   uninterpreted or represented as a table for the input-outputs
    --   relations).
    -- * @'Grisette.SymPrim.SymBool' t'Grisette.SymPrim.-~>' 'Grisette.SymPrim.SymBool'@
    --   (@'Bool' t'Grisette.SymPrim.-->' 'Bool'@, symbolic functions,
    --   uninterpreted or represented as a formula over some bound variables.
    --
    -- The bit-width of bit vector types and floating point types have their
    -- are checked at compile time.
    --
    -- Grisette also provides runtime-checked versions of bit-vectors, which
    -- might be more convenient in many scenarios:
    -- 'Grisette.SymPrim.SomeSymIntN' and 'Grisette.SymPrim.SomeSymWordN'.
    --
    -- Values of a solvable type can consist of concrete values, symbolic
    -- constants (placeholders for concrete values that can be assigned by a
    -- solver to satisfy a formula), and complex symbolic formulas with
    -- symbolic operations. The `Solvable` type class provides a way to
    -- construct concrete values and symbolic constants.
    --
    -- __Examples:__
    --
    -- >>> con True :: SymBool -- a concrete value
    -- true
    -- >>> true :: SymBool -- via the LogicalOp instance
    -- true
    -- >>> 1 :: SymInteger -- via the Num instance
    -- 1
    -- >>> ssym "a" :: SymBool -- a symbolic constant
    -- a
    --
    -- With the @OverloadedStrings@ GHC extension enabled, symbolic constants
    -- can also be constructed from strings.
    --
    -- >>> "a" :: SymBool
    -- a
    --
    -- Symbolic operations are provided through a set of type classes,
    -- such as 'SymEq', 'SymOrd', and 'Num'. Please check out the documentation for
    -- more details.
    --
    -- __Examples:__
    --
    -- >>> let a = "a" :: SymInteger
    -- >>> let b = "b" :: SymInteger
    -- >>> a .== b
    -- (= a b)

    -- ** Identifiers and symbols

    -- A symbolic constant is created out of a 'Symbol'. Two symbolic constants
    -- with the same type and the same 'Symbol' are the same symbolic value.
    --
    -- A symbolic value is an 'Identifier' with a possible index.
    --
    -- A simple symbol can be created out of a string literal:
    --
    -- >>> "a" :: Symbol
    -- a
    Identifier (..),
    Symbol (..),
    identifier,
    withInfo,
    withLoc,
    uniqueIdentifier,
    simple,
    indexed,
    symbolIdentifier,
    modifyIdentifier,

    -- ** Creation and extraction of solvable values
    Solvable (..),
    pattern Con,
    slocsym,
    ilocsym,

    -- * Basic symbolic operations #symops#

    -- ** Basic logical operators
    LogicalOp (..),
    ITEOp (..),

    -- ** Symbolic equality
    SymEq (..),
    SymEq1 (..),
    symEq1,
    SymEq2 (..),
    symEq2,

    -- ** Symbolic comparison
    SymOrd (..),
    SymOrd1 (..),
    symCompare1,
    SymOrd2 (..),
    symCompare2,
    symMax,
    symMin,
    mrgMax,
    mrgMin,

    -- ** Bit-vectors
    BV (..),
    bvExtract,
    SizedBV (..),
    sizedBVExtract,
    SymShift (..),
    SafeSymShift (..),
    SymRotate (..),
    SafeSymRotate (..),
    SignConversion (..),

    -- ** Safe operation for Numbers
    DivOr (..),
    divOrZero,
    modOrDividend,
    quotOrZero,
    remOrDividend,
    divModOrZeroDividend,
    quotRemOrZeroDividend,
    SafeDiv (..),
    SafeLinearArith (..),
    FdivOr (..),
    fdivOrZero,
    recipOrZero,
    SafeFdiv (..),
    LogBaseOr (..),
    logBaseOrZero,
    SafeLogBase (..),

    -- ** Functions
    Function (..),
    Apply (..),

    -- ** IEEE Floating points
    fpIsNaN,
    fpIsPositiveZero,
    fpIsNegativeZero,
    fpIsPositiveInfinite,
    fpIsNegativeInfinite,
    fpIsPositive,
    fpIsNegative,
    fpIsInfinite,
    fpIsZero,
    fpIsNormal,
    fpIsSubnormal,
    fpIsPoint,
    SymIEEEFPTraits (..),
    IEEEFPConstants (..),
    IEEEFPRoundingMode (..),
    IEEEFPOp (..),
    IEEEFPRoundingOp (..),
    IEEEFPConvertible (..),
    IEEEFPToAlgReal (..),

    -- ** Conversion between Concrete and Symbolic values
    ToCon (..),
    ToCon1 (..),
    toCon1,
    ToCon2 (..),
    toCon2,
    ToSym (..),
    ToSym1 (..),
    toSym1,
    ToSym2 (..),
    toSym2,

    -- ** Conversion between different symbolic types
    BitCast (..),
    BitCastCanonical (..),
    BitCastOr (..),
    BitCastOrCanonical,
    bitCastOrCanonical,
    SafeBitCast (..),
    SymFromIntegral (..),

    -- * Unsolvable types and merging

    -- | There are types that be directly represented as SMT formulas and
    -- therefore not directly supported by the SMT solvers. These types are
    -- referred to as __/unsolvable types/__.
    -- To symbolically evaluate such types, we represent them with
    -- __/symbolic unions/__.
    -- A symbolic union is a set of multiple values from different execution
    -- paths, each guarded with a corresponding path condition.
    -- The value of a union can be determined by an SMT solver based on the
    -- truth value of the path conditions.

    -- ** 'Union' basics

    -- |
    -- In Grisette, the symbolic union type is 'Union'.
    -- Two constructs are useful in constructing symbolic union values: 'mrgIf'
    -- and 'mrgSingle'.
    -- 'mrgSingle' unconditionally wraps a value in a symbolic union container,
    -- while 'mrgIf' models branching control flow semantics with symbolic
    -- conditions.
    -- Here are some examples of using 'mrgSingle' and 'mrgIf':
    --
    -- >>> mrgSingle ["a"] :: Union [SymInteger]
    -- {[a]}
    -- >>> mrgIf "a" (mrgSingle ["b"]) (mrgSingle ["c", "d"]) :: Union [SymInteger]
    -- {If a [b] [c,d]}
    --
    -- 'Union' is a monad, and its bind operation is similar to tree
    -- substitution.
    -- This means you can then use monadic constructs to model sequential programs.
    -- For example, the following pseudo code can be
    -- modeled in Grisette with the combinators provided by Grisette, and the do-notation:
    --
    -- > x = if a then [b] else [b,c] -- pseudo code, not Grisette code
    -- > y = if d then [e] else [e,f]
    -- > return (x ++ y :: [SymInteger])
    --
    -- >>> :{
    --   ret :: Union [SymInteger]
    --   ret = do x <- mrgIf "a" (return ["b"]) (return ["b","c"])
    --            y <- mrgIf "d" (return ["e"]) (return ["e","f"])
    --            -- we will explain mrgReturn later, but rule of thumb is to
    --            -- always use mrg* functions if provided by Grisette and if
    --            -- you don't understand what will happen with ordinary return.
    --            mrgReturn $ x ++ y
    -- :}
    --
    -- When this code is evaluated, the result would be as follows:
    --
    -- >>> ret
    -- {If (&& a d) [b,e] (If (|| a d) [b,(ite a e c),(ite a f e)] [b,c,e,f])}
    --
    -- In the result, we can see that the results are reorganized and the two
    -- lists with the same length are merged together.
    -- This is important for scaling symbolic evaluation to real-world
    -- problems.
    --
    -- The 'Grisette.Lib.Control.Monad.mrgReturn' function is crucial for
    -- ensuring that the results are merged. It resolves the 'Mergeable'
    -- constraint, and retrieves a merging strategy for the contained type from
    -- the constraint.
    -- The merging strategy is then cached in the 'Union' container to
    -- help merge the result of the entire do-block.
    -- This is necessary due to
    -- [the constrained-monad problem](https://dl.acm.org/doi/10.1145/2500365.2500602),
    -- as the 'return' function from the 'Monad' type class cannot resolve
    -- extra constraints.
    -- Our solution to this problem with merging strategy caching is inspired
    -- by the knowledge propagation technique introduced in the
    -- [blog post](https://okmij.org/ftp/Haskell/set-monad.html#PE) by Oleg Kiselyov.
    --
    -- In addition to 'Grisette.Lib.Control.Monad.mrgReturn',
    -- Grisette provides many combinators with the @mrg@ prefix.
    -- You should use these combinators and always have the result cache the merging strategy.
    -- Consider the following code:
    --
    -- >>> return 1 :: Union Integer
    -- <1>
    -- >>> mrgReturn 1 :: Union Integer
    -- {1}
    -- >>> mrgIf "a" (return 1) (return 2) :: Union Integer
    -- {If a 1 2}
    --
    -- In the first example, using 'return' instead of
    -- 'Grisette.Lib.Control.Monad.mrgReturn' results in a unmerged container
    -- (printed as @<@@...@@>@), which means that no merging strategy is cached.
    -- In the second and third example, using
    -- 'Grisette.Lib.Control.Monad.mrgReturn' or 'mrgIf' results in a merged
    -- container (printed as @{...}@) with a cached merging strategy.
    --
    -- When working with 'Union', it is important to always use the @mrg@
    -- prefixed combinators to ensure that the merging strategy is properly
    -- cached. This will enable Grisette to properly merge the results of the
    -- entire do-block and scale symbolic evaluation to real-world problems.
    -- Those functions that merges the results can also further propagate the
    -- cached merging strategy, see how the result is merged:
    --
    -- >>> f x y = mrgIf "f" (return x) (return y) :: Union Integer
    -- >>> do; a <- mrgIf "a" (return 1) (return 2); f a (a + 1)
    -- {If (&& a f) 1 (If (|| a f) 2 3)}
    --
    -- For more details on this, see the documentation for 'Union',
    -- 'MergingStrategy', and the [merging section](#g:merging) in
    -- this module.

    -- ** Working with user-defined types

    -- | To make a type compatible with the symbolic evaluation and merging in
    -- Grisette, you need to implement the 'Mergeable' type class.
    -- If you are only working with algebraic
    -- data types, you can derive the 'Mergeable' instance automatically
    -- For example:
    --
    -- >>> import GHC.Generics
    -- >>> :{
    --   data X = X SymInteger Integer
    --     deriving (Generic, Show)
    --     deriving (Mergeable) via (Default X)
    -- :}
    --
    -- Grisette also provide 'Grisette.deriveAll' template haskell procedure to
    -- derive all the instances for the classes that are relevant. This include
    -- 'GHC.Generics.Generic', 'Show', 'Mergeable', etc. If you only want to
    -- derive some of them, you can also use 'Grisette.deriveAllExcept' or
    -- 'Grisette.derive'.
    --
    -- >>> :{
    -- data X = X SymInteger Integer
    -- deriveAllExcept ''X [''Ord]
    -- -- Ord is excluded because it is a symbolic type
    -- :}
    --
    -- Having the 'Mergeable' instance allows you to use the 'Union' type to
    -- represent values of type @X@, and have them merged with the 'mrgIf'
    -- combinator:
    --
    -- >>> mrgIf "c1" (mrgSingle $ X "b" 1) (mrgIf "c2" (mrgSingle $ X "c" 2) (mrgSingle $ X "d" 1)) :: Union X
    -- {If (|| c1 (! c2)) (X (ite c1 b d) 1) (X c 2)}
    --
    -- __Note that deriving all instances may need to enable many__
    -- __extensions, see 'Grisette.deriveAll' for details.__

    -- ** Using monad transformers

    -- | It is also possible to apply monad transformers onto 'Union' to extend
    -- it with various mechanisms.
    -- For example, by applying 'Control.Monad.Except.ExceptT',
    -- you can symbolically evaluate a program with error handling.
    -- To do this, you will need to define an error type and derive the 'Mergeable'
    -- instance for it. Then, you can use the combinators provided by
    -- 'Control.Monad.Except.MonadError' (and the @mrg*@ variants of them in
    -- "Grisette.Lib.Control.Monad.Except") for error handling, and the 'mrgIf'
    -- combinator will also work with transformed 'Union' containers.
    --
    -- Here's an example using the 'Control.Monad.Except.ExceptT' transformer
    -- to model error handling in Grisette:
    --
    -- >>> import Control.Monad.Except
    -- >>> :{
    --   data Error = Fail
    --   deriveAll ''Error
    -- :}
    --
    -- >>> mrgIf "a" (throwError Fail) (return "x") :: ExceptT Error Union SymInteger
    -- ExceptT {If a (Left Fail) (Right x)}
    --
    -- This will return a symbolic union value representing a program that
    -- throws an error in the @then@ branch and returns a value in the @else@
    -- branch.

    -- ** Details of the merging algorithm #merging#

    -- | __The following is the details of the merging algorithm.__
    -- __If you are not going to manually configure the system by writing a__
    -- __`MergingStrategy` and will only use the derived strategies,__
    -- __you can safely ignore the following contents in this section.__
    --
    -- In Grisette, the symbolic union has the Ordered Guards (ORG)
    -- representation, which can be viewed as a nested if-then-else with some
    -- representation invariant.
    --
    -- For example, the following symbolic union represents a symbolic list of
    -- symbolic integers with length 1, 2 or 3. The values are kept sorted in the
    -- container: the list with length 1 is placed at the first place, the
    -- list with length 2 is placed at the second place, and so on.

    -- |
    -- \[
    --   \left\{\begin{aligned}%
    --   &\texttt{[a]}&&\mathrm{if}&&\texttt{c1}\\%
    --   &\texttt{[b,b]}&&\mathrm{else~if}&&\texttt{c2}\\%
    --   &\texttt{[a,b,c]}&&\mathrm{otherwise}\end{aligned}\right.
    -- \]
    --
    -- In Haskell syntax, the container is represented as the following nested
    -- if-then-else tree
    --
    -- > If c1 [a] (If c2 [b,b] [a,b,c])
    --
    -- The representations means that when @c1@ is true, then the value is a
    -- list @[a]@, or when @c1@ is false and @c2@ is true, the value is a list
    -- @[b,b]@, or otherwise the value is @[a,b,c]@.
    --
    -- This representation allows you to use
    -- the constructs that are not supported by the underlying solvers freely.
    -- For example, when applying 'head' on this structure, we can just
    -- distribute the 'head' function through the three branches, and get
    --
    -- \[
    --   \left\{\begin{aligned}%
    --     &\texttt{head [a]}&&\mathrm{if}&&\texttt{c1}\\%
    --     &\texttt{head [b,b]}&&\mathrm{else~if}&&\texttt{c2}\\%
    --     &\texttt{head [a,b,c]}&&\mathrm{otherwise}
    --   \end{aligned}\right.
    -- \]
    --
    -- or, equivalently
    --
    -- \[
    --   \left\{\begin{aligned}%
    --     &\texttt{a}&&\mathrm{if}&&\texttt{c1}\\%
    --     &\texttt{b}&&\mathrm{else~if}&&\texttt{c2}\\%
    --     &\texttt{a}&&\mathrm{otherwise}%
    --   \end{aligned}\right.
    -- \]
    --
    -- Further symbolic evaluation will also distribute computations
    -- over the branches in this result, and the identical branches will cause
    -- redundant computation. To mitigate this, Grisette would try to merge
    -- the branches, and the previous result would become
    --
    -- \[
    --   \left\{\begin{aligned}%
    --     &\texttt{a}&&\mathrm{if}&&\texttt{c1}\vee\neg\texttt{c2}\\%
    --     &\texttt{b}&&\mathrm{otherwise}\\%
    --   \end{aligned}\right.
    -- \]
    --
    -- Note that if the contained type is symbolic Boolean, we may further merge
    -- the values into a single formula.
    --
    -- \[
    --   \left\{\begin{aligned}&\texttt{(ite c1 a (ite c2 b a))}&&\mathrm{unconditional}\end{aligned}\right.
    -- \]
    --
    -- In Grisette, such merging happens in the `mrgIf` or
    -- `Grisette.Internal.Core.SimpleMergeable.mrgIfPropagatedStrategy`
    -- functions, which model the symbolic conditional branching semantics.
    -- To keep the merging efficient and generate small constraints, we enforce
    -- that the symbolic union maintains the values in a sorted way, and merge
    -- the unions with a mergesort-style merging algorithm. In the following
    -- example, the two ORG containers passed to the 'mrgIf' are sorted
    -- with the natural ordering on the integers. In the result, the values are
    -- also organized in a sorted way, and the path conditions are correctly
    -- maintained.
    --
    -- \[
    --   \texttt{mrgIf}\left[%
    --     \texttt{c},%
    --     \left\{\begin{aligned}%
    --       &\texttt{1}&&\mathrm{if}&&\texttt{c1}\\%
    --       &\texttt{3}&&\mathrm{else~if}&&\texttt{c2}\\%
    --       &\texttt{4}&&\mathrm{otherwise}%
    --     \end{aligned}\right.,%
    --     \left\{\begin{aligned}%
    --       &\texttt{1}&&\mathrm{if}&&\texttt{c3}\\%
    --       &\texttt{2}&&\mathrm{else~if}&&\texttt{c4}\\%
    --       &\texttt{4}&&\mathrm{otherwise}%
    --     \end{aligned}\right.%
    --   \right]%
    --   =\left\{\begin{aligned}%
    --     &\texttt{1}&&\mathrm{if}&&\texttt{(ite c c1 c3)}\\%
    --     &\texttt{2}&&\mathrm{else~if}&&\texttt{(&& (! c) c3)}\\%
    --     &\texttt{3}&&\mathrm{else~if}&&\texttt{(&& c c2)}\\%
    --     &\texttt{4}&&\mathrm{otherwise}%
    --   \end{aligned}\right.
    -- \]
    --
    -- So far, we have described ORG as a flat list of values.
    -- When the list is long, it is beneficial to partition the values and merge
    -- (some or all) partitions into nested ORG values.
    -- This hierarchical ORG representation is particularly useful for complex
    -- data types, such as tuples, which tend to yield long lists.
    --
    -- In the following example, @v*@ are values, while @t*@ are ORG containers.
    -- The values in the containers are first partitioned into three groups:
    -- @{v11, v12, v13}@, @{v2}@, and @{v13}@.
    -- In each group, the values share some common features, (e.g., they are
    -- constructed with the same data constructor).
    -- The values in each group are organized in a subtree, e.g., the first group
    -- is organized in the subtree @t1@.
    -- The hierarchical representation also keeps a representation invariant:
    -- at each level in the hierarchy, the values (or the subtrees) are also sorted
    -- by some criteria. Here, in the first level, the values are sorted by the
    -- constructor declaration order, and in the second level, the values are sorted
    -- by the concrete field in the constructor @A@.
    -- This criteria is given by the 'MergingStrategy' in the 'Mergeable' class,
    -- called the root merging strategy of the type.
    --
    -- > data X = A Integer SymInteger | B | C
    --
    -- \[
    --   \left\{\begin{aligned}%
    --     &\texttt{t1}&&\mathrm{if}&&\texttt{c1}\\%
    --     &\texttt{B}&&\mathrm{else if}&&\texttt{c2}\\%
    --     &\texttt{C}&&\mathrm{otherwise}&&%
    --   \end{aligned}\right.%
    --   \hspace{2em}\mathrm{where}\hspace{2em}%
    --   \texttt{t1} = \left\{\begin{aligned}%
    --     &\texttt{A 1 a}&&\mathrm{if}&&\texttt{c11}\\%
    --     &\texttt{A 3 b}&&\mathrm{else if}&&\texttt{c12}\\%
    --     &\texttt{A 4 (&& x y)}&&\mathrm{otherwise}&&%
    --   \end{aligned}\right.
    -- \]
    --
    -- In Haskell syntax, it can be represented as follows:
    --
    -- > If      c1    (If c11 (A 1 a) (If c12 (A 3 b) (A 4 (&& x y))))
    -- >   (If   c2    B
    -- >               C)
    --
    -- All the symbolic unions in Grisette should maintain the hierarchical
    -- sorted invariant, and are sorted in the same way, with respect to the same
    -- merging strategy (the root merging strategy for the type).
    -- We can then merge the containers with a hierarchical
    -- merging algorithm: at each level, we align the values and subtrees, and merge
    -- the aligned ones. In the following example, 'mrgIf' resolves the root merging
    -- strategy @s@, and calls the function @mrgIf'@, which accepts the merging strategy
    -- as an argument. The symbolic union operands to the @mrgIf'@ function must be
    -- sorted with the merging strategy passed to the function.
    --
    -- Here we use the name of the subtrees and values to indicate the order of them:
    --
    -- * @t1@ should be placed before @v3@ in the @then@ branch,
    -- * @t1@ and @v4@ can be aligned with @t1'@ and @v4'@, respectively.
    --
    -- The aligned subtrees will be merged recursively with @mrgIf'@, with a
    -- __/sub-strategy/__ given by the merging strategy @s@ for the subtrees.
    -- For example, @t1@ and @t1'@ will be merged with the sub-strategy @s'@.
    -- The aligned values will be merged with a merging function, also given by
    -- the merging strategy @s@. For example, @v4@ and @v4'@ will be merged with
    -- the merging function @f'@.
    --
    -- The ordering and the sub-strategies are abstracted with 'SortedStrategy',
    -- and the merging functions will be wrapped in 'SimpleStrategy'.
    -- The merging algorithm can then be configured by implementing the merging
    -- strategies for the types contained in a symbolic union. See the documentation
    -- for 'MergingStrategy' for details.
    --
    -- \[
    --   \begin{aligned}%
    --       & \texttt{mrgIf}\left[%
    --        \texttt{c},%
    --        \left\{\begin{aligned}%
    --          &\texttt{t1}&&\mathrm{if}&&\texttt{c1}\\%
    --          &\texttt{v3}&&\mathrm{else~if}&&\texttt{c2}\\%
    --          &\texttt{v4}&&\mathrm{otherwise}%
    --        \end{aligned}\right.,%
    --        \left\{\begin{aligned}%
    --          &\texttt{t1'}&&\mathrm{if}&&\texttt{c3}\\%
    --          &\texttt{t2'}&&\mathrm{else~if}&&\texttt{c4}\\%
    --          &\texttt{v4'}&&\mathrm{otherwise}%
    --        \end{aligned}\right.%
    --      \right]\\%
    --     =~ & \texttt{mrgIf'}\left[%
    --        \texttt{s},%
    --        \texttt{c},%
    --        \left\{\begin{aligned}%
    --          &\texttt{t1}&&\mathrm{if}&&\texttt{c1}\\%
    --          &\texttt{v3}&&\mathrm{else~if}&&\texttt{c2}\\%
    --          &\texttt{v4}&&\mathrm{otherwise}%
    --        \end{aligned}\right.,%
    --        \left\{\begin{aligned}%
    --          &\texttt{t1'}&&\mathrm{if}&&\texttt{c3}\\%
    --          &\texttt{t2'}&&\mathrm{else~if}&&\texttt{c4}\\%
    --          &\texttt{v4'}&&\mathrm{otherwise}%
    --        \end{aligned}\right.%
    --      \right]\\%
    --     =~ & \left\{\begin{aligned}%
    --        &\texttt{mrgIf' s' c t1 t1'}&&\mathrm{if}&&\texttt{(ite c c1 c3)}\\%
    --        &\texttt{t2'}&&\mathrm{else~if}&&\texttt{(&& (! c) c4)}\\%
    --        &\texttt{v3}&&\mathrm{else~if}&&\texttt{(&& c c2)}\\%
    --        &\texttt{f' c v4 v4'}&&\mathrm{otherwise}%
    --       \end{aligned}\right.
    --   \end{aligned}
    -- \]
    --
    -- For more details of the algorithm, please refer to
    -- [Grisette's paper](https://lsrcz.github.io/files/POPL23.pdf).

    -- ** Union Monad
    Union,
    IsConcrete,
    unionUnaryOp,
    unionBinOp,
    liftUnion,
    unionMergingStrategy,
    liftToMonadUnion,
    unionSize,

    -- ** Mergeable
    Mergeable (..),
    Mergeable1 (..),
    rootStrategy1,
    Mergeable2 (..),
    rootStrategy2,
    Mergeable3 (..),
    rootStrategy3,

    -- ** Merging strategy
    MergingStrategy (..),

    -- ** Manual merging strategy construction
    wrapStrategy,
    product2Strategy,
    DynamicSortedIdx (..),
    StrategyList (..),
    buildStrategyList,
    resolveStrategy,
    resolveStrategy',

    -- ** Simple mergeable types
    SimpleMergeable (..),
    SimpleMergeable1 (..),
    mrgIte1,
    SimpleMergeable2 (..),
    mrgIte2,

    -- ** Symbolic branching
    SymBranching (..),
    mrgIf,
    mergeWithStrategy,
    merge,

    -- ** TryMerge operations
    MonadTryMerge,
    TryMerge (..),
    mrgSingle,
    mrgSingleWithStrategy,
    tryMerge,

    -- ** PlainUnion operations
    PlainUnion (..),
    pattern Single,
    pattern If,
    simpleMerge,
    (.#),
    onUnion,
    onUnion2,
    onUnion3,
    onUnion4,
    MonadUnion,

    -- * Pretty printing
    PPrint (..),
    docToTextWith,
    docToTextWithWidth,
    docToText,
    pformatTextWith,
    pformatTextWithWidth,
    pformatText,
    pprint,
    PPrint1 (..),
    pformatPrec1,
    pformatList1,
    PPrint2 (..),
    pformatPrec2,
    pformatList2,

    -- ** Helpers for pretty printing
    groupedEnclose,
    condEnclose,
    pformatWithConstructor,
    pformatWithConstructorNoAlign,
    viaShowsPrec,

    -- ** Re-export Prettyprinter for convenience
    module Prettyprinter,

    -- * Symbolic Generation

    -- | It is usually useful to generate complex symbolic values. For example,
    -- in program synthesis task, we may want to generate symbolic programs
    -- to represent the search space given some specification.
    --
    -- To help with this, we provide a set of classes and functions. The core
    -- of the symbolic generation is the 'Fresh' monad, the 'GenSymSimple' class,
    -- and the 'GenSym' class.
    --
    -- The 'Fresh' monad is a combination of specialized state and reader
    -- monads. It keeps an identifier, and an index. The generated symbolic
    -- constants would be constructed with both the identifier and the index.
    -- Each time a new symbolic constant is generated, the index would be
    -- incremented. This keeps that the generated symbolic constants are unique.
    --
    -- The 'GenSymSimple' class helps generate symbolic values that do not require
    -- a symbolic union, for example, symbolic Booleans.
    -- It provides the 'simpleFresh' function, which accepts a specification
    -- for the symbolic values to generate.
    --
    -- We do not need any specification to generate a symbolic Boolean, so
    -- we provide a unit value as the specification:
    --
    -- >>> runFresh (simpleFresh ()) "x" :: SymBool
    -- x@0
    --
    -- We can generate a list of symbolic Booleans by specifying the length
    -- of the list, and the specification for the elements. The two elements
    -- in the generated list are unique as they have different indices.
    --
    -- >>> runFresh (simpleFresh (SimpleListSpec 2 ())) "x" :: [SymBool]
    -- [x@0,x@1]
    -- >>> runFresh (simpleFresh (SimpleListSpec 2 (SimpleListSpec 1 ()))) "x" :: [[SymBool]]
    -- [[x@0],[x@1]]
    --
    -- The 'GenSym' class helps generate symbolic values that require a symbolic
    -- union, for example, lists with different lengths.
    -- It provides the 'fresh' function, which accepts a specification
    -- for the symbolic values to generate.
    --
    -- We can generate a list of length 0, 1, or 2 by specifying the minimum
    -- and maximum lengths, and the specification for the elements:
    --
    -- >>> runFresh (fresh (ListSpec 0 2 ())) "x" :: Union [SymBool]
    -- {If x@2 [] (If x@3 [x@1] [x@0,x@1])}
    --
    -- We can generate many symbolic values at once with the 'Fresh' monad.
    -- The symbolic constants are ensured to be unique:
    --
    -- >>> :{
    --   flip runFresh "x" $ do
    --     a :: SymBool <- simpleFresh ()
    --     b :: Union [SymBool] <- fresh (ListSpec 0 2 ())
    --     return (a, b)
    -- :}
    -- (x@0,{If x@3 [] (If x@4 [x@2] [x@1,x@2])})
    --
    -- When you are just generating a symbolic value, and do not need to compose
    -- multiple 'simpleFresh' or 'fresh' calls, you can use the 'genSym' and
    -- 'genSymSimple' functions instead.
    --
    -- >>> genSymSimple (SimpleListSpec 2 ()) "x" :: [SymBool]
    -- [x@0,x@1]
    -- >>> genSym (ListSpec 0 2 ()) "x" :: Union [SymBool]
    -- {If x@2 [] (If x@3 [x@1] [x@0,x@1])}
    --
    -- Symbolic choices from a list of symbolic values is very useful.
    -- With the 'chooseFresh' function,
    -- we can generate a symbolic value by choosing from a list of
    -- alternative values.
    -- Grisette would generate symbolic Boolean guards to perform the symbolic
    -- choice.
    --
    -- >>> :{
    --   (flip runFresh "x" $ do
    --     a <- simpleFresh ()
    --     b <- simpleFresh ()
    --     chooseFresh [[a],[a,b],[a,a,b]]) :: Union [SymBool]
    -- :}
    -- {If x@2 [x@0] (If x@3 [x@0,x@1] [x@0,x@0,x@1])}

    -- ** Symbolic Generation Context
    FreshIndex (..),

    -- ** Symbolic Generation Monad
    MonadFresh (..),
    nextFreshIndex,
    liftFresh,
    Fresh,
    FreshT (..),
    runFresh,
    runFreshT,
    mrgRunFreshT,
    freshString,

    -- ** Symbolic Generation Class
    GenSym (..),
    GenSymSimple (..),
    genSym,
    genSymSimple,

    -- ** Symbolic Generation Class Derivation
    derivedNoSpecFresh,
    derivedNoSpecSimpleFresh,
    derivedSameShapeSimpleFresh,

    -- ** Symbolic choice
    chooseFresh,
    chooseSimpleFresh,
    chooseUnionFresh,
    choose,
    chooseSimple,
    chooseUnion,

    -- ** Useful specifications
    EnumGenBound (..),
    EnumGenUpperBound (..),
    ListSpec (..),
    SimpleListSpec (..),

    -- * Error Handling #errors#

    -- |
    -- Grisette supports using 'Control.Monad.Except.ExceptT' to handle errors,
    -- and provides the @mrg*@ variants for the combinators in "Grisette.Lib.Mtl",
    -- for example, 'Grisette.Lib.Control.Monad.Except.mrgThrowError'.
    --
    -- >>> import Control.Monad.Except
    -- >>> import Grisette.Lib.Control.Monad.Except
    -- >>> mrgThrowError AssertionError :: ExceptT AssertionError Union ()
    -- ExceptT {Left AssertionError}
    --
    -- You can define your own error types, and reason about them with the
    -- solver APIs.
    --
    -- >>> import GHC.Generics
    -- >>> :{
    --   data Error = Error1 | Error2 | Error3
    --     deriving (Show, Generic)
    --     deriving (Mergeable) via Default Error
    -- :}
    --
    -- >>> let [a,b,c] = ["a","b","c"] :: [SymBool]
    -- >>> res = mrgIf a (throwError Error1) (mrgIf b (return c) (throwError Error2)) :: ExceptT Error Union SymBool
    -- >>> res
    -- ExceptT {If (|| a (! b)) (If a (Left Error1) (Left Error2)) (Right c)}
    -- >>> solveExcept z3 (\case Left _ -> con False; Right x -> x) res
    -- Right (Model {a -> False :: Bool, b -> True :: Bool, c -> True :: Bool})
    --
    -- The solver call in the above example means that we want the solver to
    -- find the conditions under which no error is thrown, and the result is
    -- true. For more details, please refer to the
    -- [documentation of the solver APIs](#g:solver).
    --
    -- For those who prefer to encode errors as assertions and assumptions,
    -- we provide the 'symAssert' and 'symAssume' functions. These functions
    -- relies on the 'TransformError' type class to transform the assertions
    -- and assumptions to the user-defined error type.
    -- See their documentation for details.

    -- ** Predefined errors
    AssertionError (..),
    VerificationConditions (..),

    -- ** Error transformation
    TransformError (..),
    symAssert,
    symAssume,
    symAssertWith,
    symAssertTransformableError,
    symThrowTransformableError,

    -- ** Simulate CBMC error handling
    CBMCEither (..),
    CBMCExceptT (..),
    cbmcExcept,
    mapCBMCExceptT,
    withCBMCExceptT,

    -- * Solver backend #solver#

    -- | Grisette abstracts the solver backend with the 'Solver' type class,
    -- and the most basic solver call is the 'solve' function.
    --
    -- In the following code, we will search for the integer solutions to two
    -- equation systems.
    -- The first equation system, as shown below, has the solution @(x, y) = (13, -7)@.
    --
    -- \[
    --   \left\{
    --     \begin{aligned}
    --       x + y &= 6 \\
    --       x - y &= 20
    --     \end{aligned}
    --   \right.
    -- \]
    --
    -- The second equation system, as shown below, has no integer solutions.
    --
    -- \[
    --   \left\{
    --     \begin{aligned}
    --       x + y &= 6 \\
    --       x - y &= 19
    --     \end{aligned}
    --   \right.
    -- \]
    --
    -- >>> import Grisette.SymPrim
    -- >>> import Grisette.Backend
    -- >>> let x = "x" :: SymInteger
    -- >>> let y = "y" :: SymInteger
    -- >>> solve z3 (x + y .== 6 .&& x - y .== 20)
    -- Right (Model {x -> 13 :: Integer, y -> -7 :: Integer})
    -- >>> solve z3 (x + y .== 6 .&& x - y .== 19)
    -- Left Unsat
    --
    -- The first parameter of 'solve' is the solver configuration. Here we are
    -- using the configuration for the Z3 solver.
    --
    -- The second parameter is the formula to be solved. It has the type
    -- 'SymBool'.
    --
    -- The 'solve' function would return a model if the formula is satisfiable.
    -- The model is a mapping from symbolic variables to concrete values,
    -- as shown in the following example.
    --
    -- > Right (Model {x -> 13 :: Integer, y -> -7 :: Integer})
    --
    -- This model maps x to 13, and y to -7. With this model, we can then
    -- evaluate symbolic values. The following code evaluates the product of
    -- x and y under the solution of the equation system.
    --
    -- >>> Right m <- solve z3 (x + y .== 6 .&& x - y .== 20)
    -- >>> evalSym False m (x * y)
    -- -91
    --
    -- You may notice that the first argument to the 'evalSym' function is
    -- a Boolean value 'False'. This argument controls whether the evaluation
    -- should assign a default value to the symbolic constants that does not
    -- appear in the model. When the argument is 'False', the evaluation would
    -- preserve any symbolic constants that does not appear in the model, and
    -- partially evaluate the expression. When the argument is 'True', the
    -- evaluation would assign a default value to the symbolic constants that
    -- does not appear in the model, e.g., 0 for integers, and the evaluation
    -- result would become a concrete value -91.
    --
    -- >>> let z = "z" :: SymInteger
    -- >>> evalSym False m (x * y + z)
    -- (+ -91 z)
    -- >>> evalSym True m (x * y + z)
    -- -91
    --
    -- Grisette also provides convenient functions to solve problems with error
    -- handling. The lambda case function used in the following code means that
    -- we would like the solver to find path that would not lead to an error.
    -- This is done by mapping left values (failed paths) to false, and right
    -- values (successful paths) to true.
    --
    -- The following example finds bugs in a program in the hard way. It is an
    -- overkill for such a simple program, but it is a good example to show how
    -- to use Grisette to solve problems with error handling.
    --
    -- We can first define the error type used in the program.
    --
    -- >>> import Control.Monad.Except
    -- >>> import Grisette.Lib.Control.Monad.Trans.Except
    -- >>> import Control.Exception
    -- >>> import GHC.Generics
    -- >>> :{
    -- data Error = Arith | Assert
    --   deriving (Show, Generic)
    --   deriving (Mergeable, SymEq) via (Default Error)
    -- :}
    --
    -- Then we can perform the symbolic evaluation. The `divs` function throws
    -- 'ArithException' when the divisor is 0, which would be transformed to
    -- @Arith@, and the `symAssert` function would throw 'AssertionError' when
    -- the condition is false, which would be transformed to @Assert@.
    --
    -- >>> let x = "x" :: SymInteger
    -- >>> let y = "y" :: SymInteger
    -- >>> assert = symAssertWith Assert :: SymBool -> ExceptT Error Union ()
    -- >>> sdiv l r = mrgWithExceptT (\(_::ArithException) -> Arith) $ safeDiv l r
    -- >>> :{
    --   -- equivalent concrete program:
    --   -- let x = x `div` y
    --   -- if z > 0 then assert (x >= y) else return ()
    --   res :: ExceptT Error Union ()
    --   res = do
    --     z <- x `sdiv` y
    --     mrgIf (z .> 0) (assert (x .>= y)) (return ())
    -- :}
    --
    -- Then we can ask the solver to find a counter-example that would lead to
    -- an assertion violation error, but do not trigger the division by zero
    -- error.
    -- This can be done by asking the solver to find a path that produce
    -- @Left Assert@.
    --
    -- >>> res
    -- ExceptT {If (|| (= y 0) (&& (< 0 (div x y)) (! (<= y x)))) (If (= y 0) (Left Arith) (Left Assert)) (Right ())}
    --
    -- > >>> solveExcept (UnboundedReasoning z3) (.== Left Assert) res
    -- > Right (Model {x -> -6 :: Integer, y -> -3 :: Integer}) -- possible output
    --
    -- Grisette also provide implementation for counter-example guided inductive
    -- synthesis (CEGIS) algorithm. See the documentation for 'CEGISSolver' for
    -- more details.

    -- ** Solver interfaces
    SolvingFailure (..),
    MonadicSolver (..),
    monadicSolverSolve,
    SolverCommand (..),
    ConfigurableSolver (..),
    Solver (..),
    solverSolve,
    withSolver,
    solve,
    solverSolveMulti,
    solveMulti,

    -- ** Union with exceptions
    UnionWithExcept (..),
    solverSolveExcept,
    solveExcept,
    solverSolveMultiExcept,
    solveMultiExcept,

    -- ** Generic Counter-example Guided Inductive Synthesis (CEGIS) interface
    VerifierResult (..),
    SynthesisConstraintFun,
    VerifierFun,
    CEGISResult (..),
    solverGenericCEGIS,
    solverGenericCEGISWithRefinement,
    genericCEGIS,
    genericCEGISWithRefinement,

    -- ** CEGIS interfaces with pre/post conditions
    CEGISCondition (..),
    solverCegisMultiInputs,
    solverCegis,
    solverCegisExcept,
    solverCegisExceptStdVC,
    solverCegisExceptVC,
    solverCegisExceptMultiInputs,
    solverCegisExceptStdVCMultiInputs,
    solverCegisExceptVCMultiInputs,
    solverCegisForAll,
    solverCegisForAllExcept,
    solverCegisForAllExceptStdVC,
    solverCegisForAllExceptVC,
    cegisPostCond,
    cegisPrePost,
    cegisMultiInputs,
    cegis,
    cegisExcept,
    cegisExceptStdVC,
    cegisExceptVC,
    cegisExceptMultiInputs,
    cegisExceptStdVCMultiInputs,
    cegisExceptVCMultiInputs,
    cegisForAll,
    cegisForAllExcept,
    cegisForAllExceptStdVC,
    cegisForAllExceptVC,

    -- * Substitutions for symbolic values given solver models

    -- ** Symbolic constant extraction

    -- Grisette supports the extraction of the symbolic constant symbols from a
    -- symbolic value. This is useful for manipulating the models returned by
    -- the solver. The builtin CEGIS procedure relies on this.
    SymbolSetOps (..),
    SymbolSetRep (..),
    ExtractSym (..),
    ExtractSym1 (..),
    extractSym1,
    extractSymMaybe1,
    ExtractSym2 (..),
    extractSym2,
    extractSymMaybe2,

    -- ** Evaluation with a model

    -- |
    -- When given a satisfiable formula, a solver can return a model that specifies
    -- the concrete assignments of the variables in the formula to make the formula
    -- true. We can use this model to evaluate some symbolic values by substituting
    -- the symbolic constants with the concrete assignments.
    ModelOps (..),
    ModelRep (..),
    EvalSym (..),
    evalSymToCon,
    EvalSym1 (..),
    evalSym1,
    evalSymToCon1,
    EvalSym2 (..),
    evalSym2,
    evalSymToCon2,

    -- ** Substitution of a symbol
    SubstSym (..),
    SubstSym1 (..),
    substSym1,
    SubstSym2 (..),
    substSym2,

    -- * Utilities #utils#

    -- ** Memoization
    htmemo,
    htmemo2,
    htmemo3,
    htmup,
    htmemoFix,

    -- ** Generic deriving of classes

    -- *** Default wrappers
    Default (..),
    Default1 (..),

    -- *** 'SymEq'
    SymEqArgs (..),
    GSymEq (..),
    genericSymEq,
    genericLiftSymEq,

    -- *** 'SymOrd'
    SymOrdArgs (..),
    GSymOrd (..),
    genericSymCompare,
    genericLiftSymCompare,

    -- *** 'Mergeable'
    MergeableArgs (..),
    GMergeable (..),
    genericRootStrategy,
    genericLiftRootStrategy,

    -- *** 'SimpleMergeable'
    SimpleMergeableArgs (..),
    GSimpleMergeable (..),
    genericMrgIte,
    genericLiftMrgIte,

    -- *** 'ToCon'
    ToConArgs (..),
    GToCon (..),
    genericToCon,
    genericLiftToCon,

    -- *** 'ToSym'
    ToSymArgs (..),
    GToSym (..),
    genericToSym,
    genericLiftToSym,

    -- *** 'EvalSym'
    EvalSymArgs (..),
    GEvalSym (..),
    genericEvalSym,
    genericLiftEvalSym,

    -- *** 'ExtractSym'
    ExtractSymArgs (..),
    GExtractSym (..),
    genericExtractSymMaybe,
    genericLiftExtractSymMaybe,

    -- *** 'SubstSym'
    SubstSymArgs (..),
    GSubstSym (..),
    genericSubstSym,
    genericLiftSubstSym,

    -- *** 'PPrint'
    genericPFormatPrec,
    genericLiftPFormatPrec,
    genericPFormatList,
    genericLiftPFormatList,
    PPrintArgs (..),
    GPPrint (..),
    PPrintType (..),
  )
where

#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter
#else
import Data.Text.Prettyprint.Doc as Prettyprinter
#endif

import Generics.Deriving (Default (..), Default1 (..))
import Grisette.Internal.Core.Control.Exception
  ( AssertionError (..),
    VerificationConditions (..),
  )
import Grisette.Internal.Core.Control.Monad.CBMCExcept
  ( CBMCEither (..),
    CBMCExceptT (..),
    cbmcExcept,
    mapCBMCExceptT,
    withCBMCExceptT,
  )
import Grisette.Internal.Core.Control.Monad.Class.Union (MonadUnion)
import Grisette.Internal.Core.Control.Monad.Union
  ( IsConcrete,
    Union,
    liftToMonadUnion,
    liftUnion,
    unionBinOp,
    unionMergingStrategy,
    unionSize,
    unionUnaryOp,
  )
import Grisette.Internal.Core.Data.Class.BitCast
  ( BitCast (..),
    BitCastCanonical (..),
    BitCastOr (..),
    BitCastOrCanonical,
    bitCastOrCanonical,
  )
import Grisette.Internal.Core.Data.Class.BitVector
  ( BV (..),
    SizedBV (..),
    bvExtract,
    sizedBVExtract,
  )
import Grisette.Internal.Core.Data.Class.CEGISSolver
  ( CEGISCondition (..),
    CEGISResult (..),
    SynthesisConstraintFun,
    VerifierFun,
    VerifierResult (..),
    cegis,
    cegisExcept,
    cegisExceptMultiInputs,
    cegisExceptStdVC,
    cegisExceptStdVCMultiInputs,
    cegisExceptVC,
    cegisExceptVCMultiInputs,
    cegisForAll,
    cegisForAllExcept,
    cegisForAllExceptStdVC,
    cegisForAllExceptVC,
    cegisMultiInputs,
    cegisPostCond,
    cegisPrePost,
    genericCEGIS,
    genericCEGISWithRefinement,
    solverCegis,
    solverCegisExcept,
    solverCegisExceptMultiInputs,
    solverCegisExceptStdVC,
    solverCegisExceptStdVCMultiInputs,
    solverCegisExceptVC,
    solverCegisExceptVCMultiInputs,
    solverCegisForAll,
    solverCegisForAllExcept,
    solverCegisForAllExceptStdVC,
    solverCegisForAllExceptVC,
    solverCegisMultiInputs,
    solverGenericCEGIS,
    solverGenericCEGISWithRefinement,
  )
import Grisette.Internal.Core.Data.Class.Error
  ( TransformError (..),
    symAssert,
    symAssertTransformableError,
    symAssertWith,
    symAssume,
    symThrowTransformableError,
  )
import Grisette.Internal.Core.Data.Class.EvalSym
  ( EvalSym (..),
    EvalSym1 (..),
    EvalSym2 (..),
    EvalSymArgs (..),
    GEvalSym (..),
    evalSym1,
    evalSym2,
    evalSymToCon,
    evalSymToCon1,
    evalSymToCon2,
    genericEvalSym,
    genericLiftEvalSym,
  )
import Grisette.Internal.Core.Data.Class.ExtractSym
  ( ExtractSym (..),
    ExtractSym1 (..),
    ExtractSym2 (..),
    ExtractSymArgs (..),
    GExtractSym (..),
    extractSym1,
    extractSym2,
    extractSymMaybe1,
    extractSymMaybe2,
    genericExtractSymMaybe,
    genericLiftExtractSymMaybe,
  )
import Grisette.Internal.Core.Data.Class.Function (Apply (..), Function (..))
import Grisette.Internal.Core.Data.Class.GenSym
  ( EnumGenBound (..),
    EnumGenUpperBound (..),
    Fresh,
    FreshIndex (..),
    FreshT (..),
    GenSym (..),
    GenSymSimple (..),
    ListSpec (..),
    MonadFresh (..),
    SimpleListSpec (..),
    choose,
    chooseFresh,
    chooseSimple,
    chooseSimpleFresh,
    chooseUnion,
    chooseUnionFresh,
    derivedNoSpecFresh,
    derivedNoSpecSimpleFresh,
    derivedSameShapeSimpleFresh,
    freshString,
    genSym,
    genSymSimple,
    liftFresh,
    mrgRunFreshT,
    nextFreshIndex,
    runFresh,
    runFreshT,
  )
import Grisette.Internal.Core.Data.Class.IEEEFP
  ( IEEEFPConstants (..),
    IEEEFPConvertible (..),
    IEEEFPOp (..),
    IEEEFPRoundingMode (..),
    IEEEFPRoundingOp (..),
    IEEEFPToAlgReal (..),
    fpIsInfinite,
    fpIsNaN,
    fpIsNegative,
    fpIsNegativeInfinite,
    fpIsNegativeZero,
    fpIsNormal,
    fpIsPoint,
    fpIsPositive,
    fpIsPositiveInfinite,
    fpIsPositiveZero,
    fpIsSubnormal,
    fpIsZero,
  )
import Grisette.Internal.Core.Data.Class.ITEOp (ITEOp (..))
import Grisette.Internal.Core.Data.Class.LogicalOp (LogicalOp (..))
import Grisette.Internal.Core.Data.Class.Mergeable
  ( DynamicSortedIdx (..),
    GMergeable (..),
    Mergeable (..),
    Mergeable1 (..),
    Mergeable2 (..),
    Mergeable3 (..),
    MergeableArgs (..),
    MergingStrategy (..),
    StrategyList (..),
    buildStrategyList,
    genericLiftRootStrategy,
    genericRootStrategy,
    product2Strategy,
    resolveStrategy,
    resolveStrategy',
    rootStrategy1,
    rootStrategy2,
    rootStrategy3,
    wrapStrategy,
  )
import Grisette.Internal.Core.Data.Class.ModelOps
  ( ModelOps (..),
    ModelRep (..),
    SymbolSetOps (..),
    SymbolSetRep (..),
  )
import Grisette.Internal.Core.Data.Class.PPrint
  ( GPPrint (..),
    PPrint (..),
    PPrint1 (..),
    PPrint2 (..),
    PPrintArgs (..),
    PPrintType (..),
    condEnclose,
    docToText,
    docToTextWith,
    docToTextWithWidth,
    genericLiftPFormatList,
    genericLiftPFormatPrec,
    genericPFormatList,
    genericPFormatPrec,
    groupedEnclose,
    pformatList1,
    pformatList2,
    pformatPrec1,
    pformatPrec2,
    pformatText,
    pformatTextWith,
    pformatTextWithWidth,
    pformatWithConstructor,
    pformatWithConstructorNoAlign,
    pprint,
    viaShowsPrec,
  )
import Grisette.Internal.Core.Data.Class.PlainUnion
  ( PlainUnion (..),
    onUnion,
    onUnion2,
    onUnion3,
    onUnion4,
    simpleMerge,
    (.#),
    pattern If,
    pattern Single,
  )
import Grisette.Internal.Core.Data.Class.SafeBitCast (SafeBitCast (..))
import Grisette.Internal.Core.Data.Class.SafeDiv
  ( DivOr (..),
    SafeDiv (..),
    divModOrZeroDividend,
    divOrZero,
    modOrDividend,
    quotOrZero,
    quotRemOrZeroDividend,
    remOrDividend,
  )
import Grisette.Internal.Core.Data.Class.SafeFdiv
  ( FdivOr (..),
    SafeFdiv (..),
    fdivOrZero,
    recipOrZero,
  )
import Grisette.Internal.Core.Data.Class.SafeLinearArith (SafeLinearArith (..))
import Grisette.Internal.Core.Data.Class.SafeLogBase
  ( LogBaseOr (..),
    SafeLogBase (..),
    logBaseOrZero,
  )
import Grisette.Internal.Core.Data.Class.SafeSymRotate (SafeSymRotate (..))
import Grisette.Internal.Core.Data.Class.SafeSymShift (SafeSymShift (..))
import Grisette.Internal.Core.Data.Class.SignConversion (SignConversion (..))
import Grisette.Internal.Core.Data.Class.SimpleMergeable
  ( GSimpleMergeable (..),
    SimpleMergeable (..),
    SimpleMergeable1 (..),
    SimpleMergeable2 (..),
    SimpleMergeableArgs (..),
    SymBranching (..),
    genericLiftMrgIte,
    genericMrgIte,
    merge,
    mergeWithStrategy,
    mrgIf,
    mrgIte1,
    mrgIte2,
  )
import Grisette.Internal.Core.Data.Class.Solvable
  ( Solvable (..),
    ilocsym,
    slocsym,
    pattern Con,
  )
import Grisette.Internal.Core.Data.Class.Solver
  ( ConfigurableSolver (..),
    MonadicSolver (..),
    Solver (..),
    SolverCommand (..),
    SolvingFailure (..),
    UnionWithExcept (..),
    monadicSolverSolve,
    solve,
    solveExcept,
    solveMulti,
    solveMultiExcept,
    solverSolve,
    solverSolveExcept,
    solverSolveMulti,
    solverSolveMultiExcept,
    withSolver,
  )
import Grisette.Internal.Core.Data.Class.SubstSym
  ( GSubstSym (..),
    SubstSym (..),
    SubstSym1 (..),
    SubstSym2 (..),
    SubstSymArgs (..),
    genericLiftSubstSym,
    genericSubstSym,
    substSym1,
    substSym2,
  )
import Grisette.Internal.Core.Data.Class.SymEq
  ( GSymEq (..),
    SymEq (..),
    SymEq1 (..),
    SymEq2 (..),
    SymEqArgs (..),
    genericLiftSymEq,
    genericSymEq,
    symEq1,
    symEq2,
  )
import Grisette.Internal.Core.Data.Class.SymFromIntegral
  ( SymFromIntegral (..),
  )
import Grisette.Internal.Core.Data.Class.SymIEEEFP
  ( SymIEEEFPTraits (..),
  )
import Grisette.Internal.Core.Data.Class.SymOrd
  ( GSymOrd (..),
    SymOrd (..),
    SymOrd1 (..),
    SymOrd2 (..),
    SymOrdArgs (..),
    genericLiftSymCompare,
    genericSymCompare,
    mrgMax,
    mrgMin,
    symCompare1,
    symCompare2,
    symMax,
    symMin,
  )
import Grisette.Internal.Core.Data.Class.SymRotate (SymRotate (..))
import Grisette.Internal.Core.Data.Class.SymShift (SymShift (..))
import Grisette.Internal.Core.Data.Class.ToCon
  ( GToCon (..),
    ToCon (..),
    ToCon1 (..),
    ToCon2 (..),
    ToConArgs (..),
    genericLiftToCon,
    genericToCon,
    toCon1,
    toCon2,
  )
import Grisette.Internal.Core.Data.Class.ToSym
  ( GToSym (..),
    ToSym (..),
    ToSym1 (..),
    ToSym2 (..),
    ToSymArgs (..),
    genericLiftToSym,
    genericToSym,
    toSym1,
    toSym2,
  )
import Grisette.Internal.Core.Data.Class.TryMerge
  ( MonadTryMerge,
    TryMerge (..),
    mrgSingle,
    mrgSingleWithStrategy,
    tryMerge,
  )
import Grisette.Internal.Core.Data.MemoUtils
  ( htmemo,
    htmemo2,
    htmemo3,
    htmemoFix,
    htmup,
  )
import Grisette.Internal.Core.Data.Symbol
  ( Identifier (..),
    Symbol (..),
    identifier,
    indexed,
    modifyIdentifier,
    simple,
    symbolIdentifier,
    uniqueIdentifier,
    withInfo,
    withLoc,
  )
import Instances.TH.Lift ()

-- $setup
-- >>> import Grisette
-- >>> import Grisette.Core
-- >>> import Grisette.Lib.Base
-- >>> import Grisette.SymPrim
-- >>> import Grisette.TH
