{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.Core
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core
  ( -- * Note for the examples

    --

    -- | This module does not contain the implementation for symbolic primitive
    -- types, and the examples in this module rely on the implementations in
    -- the [grisette-symir](https://hackage.haskell.org/package/grisette-symir/)
    -- and [grisette-backend-sbv](https://hackage.haskell.org/package/grisette-backend-sbv) packages,
    -- which provides the solvable type and the solver backend implementations,
    -- respectively.
    --
    -- The examples may also assume a z3 solver available in @PATH@.

    -- * Symbolic values

    -- | Grisette performs symbolic evaluation for programs.
    -- Symbolic evaluation is a technique to analyze all possible runs of a
    -- program. In concrete evaluation, a program is run on a specific input
    -- and only a single control flow is explored. In contrast, with symbolic
    -- evaluation, multiple paths are explored simultaneously, and we can
    -- analyze the program paths for a whole space of programs or inputs.
    --
    -- With symbolic evaluation,
    -- the inputs (or the program itself) are represented as symbolic
    -- values, and the evaluation result would become symbolic constraints
    -- over the symbolic values
    -- indicating the possible outcomes of the conditional branches.
    -- After the evaluation, constraint solvers can be used to find concrete assignments
    -- to the symbolic values to meet some criteria.
    -- For example, in verification tasks, we can make the program inputs
    -- symbolic, and use solvers to find concrete counterexamples to trigger the
    -- bugs.
    -- In synthesis tasks, we can make the program itself symbolic, and use
    -- the solver to find concrete programs that meet some specification.
    --
    -- Traditional symbolic evaluation techniques evaluate and reason about
    -- the possible paths one-by-one. In many tasks, for example, program
    -- synthesis, such a technique will cause the well-known path explosion
    -- problem and does not scale well.
    -- This path explosion problem can be alleviated by merging the symbolic
    -- values of the program branches. This can reduce the number of paths to
    -- execute, and works well in practice.
    --
    -- As a result, the symbolic value representation and the merging algorithm
    -- becomes critical to the performance of symbolic evaluation.
    -- In Grisette, we make a distinction between two kinds of symbolic values:
    -- __/solvable types/__ and __/unsolvable types/__.
    --
    -- __/Solvable types/__ are types that are directly supported by the
    -- underlying solver, and will be represented directly as symbolic formulas.
    -- Such types include symbolic Boolean values, symbolic integers, and
    -- symbolic bit vectors, etc.
    -- The values of solvable types can be __/fully/__ merged into a single
    -- value, for example, assume that @a@, @b@, @c@ are symbolic Boolean
    -- values (have the 'SymBool' type),
    -- the following program will be evaluated to a single
    -- symbolic Boolean formula, shown in the format of SMT-LIB:
    --
    -- > x = if a then b else c -- pseudo code, not real Grisette code
    -- > -- result: x is (ite a b c)
    --
    -- If we further add 1 to @x@, we will not have to split to two paths,
    -- but we can directly construct a formula with the merged state.
    --
    -- > x + 1
    -- > -- result (+ 1 (ite a b c))
    --
    -- __/Unsolvable types/__ are the types that are not directly supported by
    -- the solver. These types cannot hold values from different execution paths
    -- by themselves, and cannot be fully merged into a single formula.
    -- Such types includes lists, algebraic data types, and concrete Haskell
    -- integer types, etc.
    -- To symbolically evaluate values in such types, Grisette
    -- provides a symbolic union container, which is essentially a set of values
    -- guarded by their path conditions (the symbolic condition that leads to
    -- the value).
    -- The values of unsolvable types will be __/partially/__ merged in a
    -- symbolic union (see 'UnionMBase').
    --
    -- In the following example, assume that the lists have the type @[SymBool]@,
    -- Grisette would merge the lists with the same lengths together, and keep
    -- the lists with different lengths in a symbolic union.
    -- The symbolic union is simply an if-then-else tree, where 'If' represents
    -- the symbolic conditionals, and 'Single' represents unconditional choices.
    -- In the following example, the first symbolic union shows that @[b]@ and
    -- @[c]@ can be merged together into a single list, and the second symbolic
    -- union means that the result would be @[b]@ if @a@ is true, or @[c,d]@ if
    -- @a@ is false.
    --
    -- > if a then [b] else [c] -- pseudo code, not real Grisette code
    -- > -- result: Single [(ite a b c)]
    -- > if a then [b] else [c, d] -- pseudo code, not real Grisette code
    -- > -- result: If a (Single [b]) (Single [c,d])
    -- > if a1 then [b] else if a2 then [c, d] else [f] -- pseudo code, not real Grisette code
    -- > -- result: If (|| a1 (! a2)) (Single [(ite a1 b f)]) (Single [c,d])
    --
    -- When we further operate on this partially merged values,
    -- we will need to split into multiple paths. For example, when we apply 'head'
    -- onto the last result, we will distribute 'head' among the branches:
    --
    -- > head (if a1 then [b] else if a2 then [c, d] else [f]) -- pseudo code, not real Grisette code
    -- > -- intermediate result: If (|| a1 (! a2)) (Single (head [(ite a1 b f)])) (Single (head [c,d]))
    -- > -- intermediate result: If (|| a1 (! a2)) (Single (ite a1 b f)) (Single c)
    --
    -- Then the result would be further merged
    --
    -- > -- final result: Single (ite (|| a1 (! a2)) (ite a1 b f) c)
    --
    -- Generally, merging the possible branches in a symbolic union can reduce
    -- the number of paths to be explored in the future, but can make the path
    -- conditions larger and harder to solve. To have a good balance between
    -- this, Grisette has built a hierarchical merging algorithm, which is
    -- configurable via 'GMergingStrategy'. For algebraic data types, we have
    -- prebuilt merging strategies via the derivation of the 'GMergeable' type
    -- class. You only need to know the details of the merging algorithm if you
    -- are going to work with non-algebraic data types.

    -- ** Solvable types

    -- | A solvable type is a type that can be represented as a formula and is
    -- directly supported by the underlying constraint solvers.
    -- The solvable type implementation provided by [grisette-symir](https://hackage.haskell.org/package/grisette-symir) package
    -- currently supports symbolic Boolean types (@SymBool@ or @Sym Bool@),
    -- symbolic unbounded integers (@SymInteger@ or @Sym Integer@),
    -- and symbolic bit vectors (signed bit vectors has the type @SymIntN 5@ or
    -- @Sym (IntN 5)@, and unsigned bit vectors has the type @SymWordN 5@ or
    -- @Sym (WordN 5)@). Please see the documentation for the [grisette-symir](https://hackage.haskell.org/package/grisette-symir)
    -- package for details.
    --
    -- A solvable type value can consist of the following constructs:
    --
    -- * Concrete values, e.g., @SymInteger@ can represent a concrete integer 1,
    -- * Symbolic constants, e.g., @a@, @b@, etc. These symbolic constants can
    -- be considered as placeholders for concrete values, and a solver can
    -- decide the concrete assignments to them to satisfy a formula. See
    -- [Solver Interface](#solver) for the details of the solver interface.
    -- * Complex symbolic formula with symbolic operations, e.g., @(+ a 1)@.
    --
    -- We provide the construction procedure of a concrete value or symbolic
    -- constant via the 'Solvable' type class.
    --
    -- >>> import Grisette.Core
    -- >>> import Grisette.Lib.Base
    -- >>> import Grisette.IR.SymPrim -- provided by grisette-symir package
    -- >>> conc True :: SymBool
    -- true
    -- >>> ssymb "a" :: SymBool
    -- a
    --
    -- With the @OverloadedStrings@ GHC extension enabled, a symbolic constant
    -- can also be constructed from a string.
    --
    -- >>> :set -XOverloadedStrings
    -- >>> "a" :: SymBool
    -- a
    --
    -- We provide the symbolic operations via a set of type classes,
    -- including 'GSEq', 'GSOrd', and 'Num', etc. As our system supports
    -- alternative solvable type implementation other than the one from the
    -- [grisette-symir](https://hackage.haskell.org/package/grisette-symir) package, the 'GSEq' and 'GSOrd' type classes are
    -- parametrized with the solvable type implementation (the symbolic Boolean
    -- type provided by the implementation). In most
    -- cases, you can just work with the implementation in [grisette-symir](https://hackage.haskell.org/package/grisette-symir)
    -- package, and you can use specialized 'SEq' and 'SOrd'
    -- constraints and the specialized operators in [grisette-symir](https://hackage.haskell.org/package/grisette-symir).
    -- This may reduce the need for manual type annotations.
    --
    -- >>> let a = "a" :: SymInteger
    -- >>> let b = "b" :: SymInteger
    -- >>> a `gsymeq` b :: SymBool -- G means generic, the type annotation is required
    -- (= a b)
    -- >>> a ==~ b -- The type annotation is not required
    -- (= a b)

    -- *** Creation of solvable type values
    Solvable (..),
    pattern Conc,
    slocsymb,
    ilocsymb,

    -- *** Symbolic operators

    -- | #symop#
    LogicalOp (..),
    ITEOp (..),
    GSEq (..),
    SymBoolOp,
    GSOrd (..),
    BVConcat (..),
    BVExtend (..),
    BVSelect (..),
    bvextract,
    SignedDivMod (..),
    UnsignedDivMod (..),
    SignedQuotRem (..),
    GSymIntegerOp,
    Function (..),

    -- ** Unsolvable types

    -- | There are types that are not directly supported by the underlying
    -- solver and cannot be represented as SMT formulas. We call these types
    -- unsolvable types.
    -- To symbolically evaluate such types, we represent them with
    -- __/symbolic unions/__.
    -- A symbolic union is a set of multiple values from different execution
    -- paths, each guarded with its path condition.
    -- The value of a union is decided by the truth value of the path conditions,
    -- and can be determined by an SMT solver.
    --
    -- The symbolic union type in Grisette is 'UnionMBase'. It is parametrized
    -- with the symbolic Boolean type, and usually you can just use the
    -- 'UnionM' type provided by [grisette-symir](https://hackage.haskell.org/package/grisette-symir).
    -- Two constructs are useful in constructing such values: 'mrgIf' and
    -- 'mrgSingle'.
    -- The 'mrgSingle' construct unconditionally wraps a value into
    -- the symbolic union container, while the 'mrgIf' model the branching
    -- control flow semantics with symbolic conditions.
    --
    -- >>> mrgSingle ["a"] :: UnionM [SymInteger]
    -- UMrg (Single [a])
    -- >>> mrgIf "a" (mrgSingle ["b"]) (mrgSingle ["c", "d"]) :: UnionM [SymInteger]
    -- UMrg (If a (Single [b]) (Single [c,d]))
    --
    -- 'UnionM' is a monad, and its bind operation is similar to tree
    -- substitution.
    -- You can then use monadic constructs to model sequential programs.
    -- For example, the following code, when evaluated symbolically, can be
    -- modeled in Grisette with the do-notation.
    --
    -- > x = if a then [b] else [b,c] -- pseudo code, not Grisette code
    -- > y = if d then [e] else [e,f]
    -- > return (x ++ y :: [SymInteger])
    --
    -- >>> :{
    --   ret :: UnionM [SymInteger]
    --   ret = do x <- mrgIf "a" (single ["b"]) (single ["b","c"])
    --            y <- mrgIf "d" (single ["e"]) (single ["e","f"])
    --            mrgReturn $ x ++ y
    -- :}
    --
    -- >>> ret
    -- UMrg (If (&& a d) (Single [b,e]) (If (|| a d) (Single [b,(ite a e c),(ite a f e)]) (Single [b,c,e,f])))
    --
    -- In the result, we can see that the results are reorganized and the two
    -- lists with the same length are merged together.
    -- This is very important to scale symbolic evaluation to real-world
    -- problems.
    -- The 'mrgReturn' function is the key to ensure that the results
    -- are merged. It would resolve 'GMergeable' constraint, and get a merging
    -- strategy for the contained type from the constraint.
    -- The merging strategy would be cached in the 'UnionMBase' container to
    -- help merge the result of the entire do-block.
    -- This is needed due to [the constrained-monad problem](https://dl.acm.org/doi/10.1145/2500365.2500602),
    -- and our solution to it is inspired by the [blog post](https://okmij.org/ftp/Haskell/set-monad.html#PE).
    --
    -- Grisette provides many combinators with the @mrg@ prefix.
    -- You should stick to them and always have the result cache the merging strategy.
    -- See the following example, 'UAny' means that no merging strategy is cached,
    -- while 'UMrg' means that the merging strategy is cached.
    --
    -- >>> return 1 :: UnionM Integer
    -- UAny (Single 1)
    -- >>> mrgReturn 1 :: UnionM Integer
    -- UMrg (Single 1)
    -- >>> mrgIf "a" (return 1) (return 2) :: UnionM Integer
    -- UMrg (If a (Single 1) (Single 2))
    --
    -- If a function can merge the results, then it can also further propagate
    -- the merging strategy and help merging.
    --
    -- >>> f x y = mrgIf "f" (return x) (return y)
    -- >>> do; a <- mrgIf "a" (return 1) (return 2); f a (a + 1) :: UnionM Integer
    -- UMrg (If (&& a f) (Single 1) (If (|| a f) (Single 2) (Single 3)))
    --
    -- For more details of this, see the documentation for 'UnionMBase' and
    -- 'GMergingStrategy'.
    --
    -- To make a type compatible with this usage, you need to implement the
    -- 'GMergeable' type class.
    -- If you are only working with algebraic
    -- data types, you can simply derive the type class, and you do not need to
    -- know the details of the merging algorithm.
    -- The following shows an example
    --
    -- >>> :set -XDerivingStrategies
    -- >>> :set -XDerivingVia
    -- >>> :set -XDeriveGeneric
    -- >>> import GHC.Generics
    -- >>> :{
    --   data X = X SymInteger Integer
    --     deriving (Generic, Show)
    --     deriving (GMergeable SymBool) via (Default X)
    -- :}
    --
    -- >>> mrgIf "c1" (mrgSingle $ X "b" 1) (mrgIf "c2" (mrgSingle $ X "c" 2) (mrgSingle $ X "d" 1)) :: UnionM X
    -- UMrg (If (|| c1 (! c2)) (Single (X (ite c1 b d) 1)) (Single (X c 2)))
    --
    -- We can apply monad transformers onto 'UnionMBase' to extend it with
    -- various mechanisms. For example, by applying 'Control.Monad.ExceptT', we
    -- can symbolically evaluate a program with error handling.
    --
    -- Let us use the following pseudo code as an example. In the @then@
    -- branch, we throw an error, and in the @else@ branch, we return a value.
    --
    -- > if a then throw Fail else return x -- pseudo code
    --
    -- To make this work in Grisette, we need to define the error type first,
    -- and derive the 'GMergeable' instance for it.
    --
    -- >>> :{
    --   data Error = Fail
    --     deriving (Generic, Show)
    --     deriving (GMergeable SymBool) via (Default Error)
    -- :}
    --
    -- Then we can use the combinators provided by 'MonadError' for error
    -- handling. Note that the 'mrgIf' combinators also works with transformed
    -- `UnionMBase` containers.
    --
    -- >>> import Control.Monad.Except
    -- >>> mrgIf "a" (throwError Fail) (return "x") :: ExceptT Error UnionM SymInteger
    -- ExceptT (UMrg (If a (Single (Left Fail)) (Single (Right x))))
    --
    -- __The following is the details of the merging algorithm.__
    -- __If you are not going to manually configure the system by writing a__
    -- __`GMergingStrategy`, you can safely ignore the following contents in this section.__
    --
    -- In Grisette, the symbolic union has the Ordered Guards (ORG)
    -- representation, which can be viewed as a nested if-then-else with some
    -- representation invariant.
    --
    -- For example, the following symbolic union represents a symbolic list of
    -- symbolic integers with length 1, 2 or 3. The values are kept sorted in the
    -- container: the list with length 1 is placed at the first place, the
    -- list with length 2 is placed at the second place, and so on.
    --
    -- \[
    --   \left\{\begin{aligned}
    --     &\texttt{[a]}&&\mathrm{if}&&\texttt{c1}\\
    --     &\texttt{[b,b]}&&\mathrm{else~if}&&\texttt{c2}\\&
    --     \texttt{[a,b,c]}&&\mathrm{otherwise}
    --   \end{aligned}\right.
    -- \]
    --
    -- In Haskell syntax, the container is represented as the following nested
    -- if-then-else tree
    --
    -- > If c1 (Single [a]) (If c2 (Single [b,b]) (Single [a,b,c]))
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
    --   \left\{\begin{aligned}
    --     &\texttt{head [a]}&&\mathrm{if}&&\texttt{c1}\\
    --     &\texttt{head [b,b]}&&\mathrm{else~if}&&\texttt{c2}\\
    --     &\texttt{head [a,b,c]}&&\mathrm{otherwise}
    --   \end{aligned}\right.
    -- \]
    --
    -- or, equivalently
    --
    -- \[
    --   \left\{\begin{aligned}
    --     &\texttt{a}&&\mathrm{if}&&\texttt{c1}\\
    --     &\texttt{b}&&\mathrm{else~if}&&\texttt{c2}\\
    --     &\texttt{a}&&\mathrm{otherwise}
    --   \end{aligned}\right.
    -- \]
    --
    -- Further symbolic evaluation will also distribute computations
    -- over the branches in this result, and the identical branches will cause
    -- redundant computation. To mitigate this, Grisette would try to merge
    -- the branches, and the previous result would become
    --
    -- \[
    --   \left\{\begin{aligned}
    --     &\texttt{a}&&\mathrm{if}&&\texttt{c1}\vee\neg\texttt{c2}\\
    --     &\texttt{b}&&\mathrm{otherwise}\\
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
    -- In Grisette, such merging happens in the `mrgIf` or `unionIf` functions,
    -- which model the symbolic conditional branching semantics.
    -- To keep the merging efficient and generate small constraints, we enforce
    -- that the symbolic union maintains the values in a sorted way, and merge
    -- the unions with a mergesort-style merging algorithm. In the following
    -- example, the two ORG containers passed to the 'mrgIf' are sorted
    -- with the natural ordering on the integers. In the result, the values are
    -- also organized in a sorted way, and the path conditions are correctly
    -- maintained.
    --
    -- \[
    --   \texttt{mrgIf}\left[
    --     \texttt{c},
    --     \left\{\begin{aligned}
    --       &\texttt{1}&&\mathrm{if}&&\texttt{c1}\\
    --       &\texttt{3}&&\mathrm{else~if}&&\texttt{c2}\\
    --       &\texttt{4}&&\mathrm{otherwise}
    --     \end{aligned}\right.,
    --     \left\{\begin{aligned}
    --       &\texttt{1}&&\mathrm{if}&&\texttt{c3}\\
    --       &\texttt{2}&&\mathrm{else~if}&&\texttt{c4}\\
    --       &\texttt{4}&&\mathrm{otherwise}
    --     \end{aligned}\right.
    --   \right]
    --   =\left\{\begin{aligned}
    --     &\texttt{1}&&\mathrm{if}&&\texttt{(ite c c1 c3)}\\
    --     &\texttt{2}&&\mathrm{else~if}&&\texttt{(&& (! c) c3)}\\
    --     &\texttt{3}&&\mathrm{else~if}&&\texttt{(&& c c2)}\\
    --     &\texttt{4}&&\mathrm{otherwise}
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
    -- This criteria is given by the 'GMergingStrategy' in the 'GMergeable' class,
    -- called the root merging strategy of the type.
    --
    -- > data X = A Integer SymInteger | B | C
    --
    -- \[
    --   \left\{\begin{aligned}
    --     &\texttt{t1}&&\mathrm{if}&&\texttt{c1}\\
    --     &\texttt{B}&&\mathrm{else if}&&\texttt{c2}\\
    --     &\texttt{C}&&\mathrm{otherwise}&&
    --   \end{aligned}\right.
    --   \hspace{2em}\mathrm{where}\hspace{2em}
    --   \texttt{t1} = \left\{\begin{aligned}
    --     &\texttt{A 1 a}&&\mathrm{if}&&\texttt{c11}\\
    --     &\texttt{A 3 b}&&\mathrm{else if}&&\texttt{c12}\\
    --     &\texttt{A 4 (&& x y)}&&\mathrm{otherwise}&&
    --   \end{aligned}\right.
    -- \]
    --
    -- In Haskell syntax, it can be represented as follows:
    --
    -- > If      c1    (If c11 (Single (A 1 a)) (If c12 (Single (A 3 b)) (Single (A 4 (&& x y)))))
    -- >   (If   c2    (Single B)
    -- >               (Single C))
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
    -- for 'GMergingStrategy' for details.
    --
    -- \[
    --   \begin{aligned}
    --       & \texttt{mrgIf}\left[
    --        \texttt{c},
    --        \left\{\begin{aligned}
    --          &\texttt{t1}&&\mathrm{if}&&\texttt{c1}\\
    --          &\texttt{v3}&&\mathrm{else~if}&&\texttt{c2}\\
    --          &\texttt{v4}&&\mathrm{otherwise}
    --        \end{aligned}\right.,
    --        \left\{\begin{aligned}
    --          &\texttt{t1'}&&\mathrm{if}&&\texttt{c3}\\
    --          &\texttt{t2'}&&\mathrm{else~if}&&\texttt{c4}\\
    --          &\texttt{v4'}&&\mathrm{otherwise}
    --        \end{aligned}\right.
    --      \right]\\
    --     =~ & \texttt{mrgIf'}\left[
    --        \texttt{s},
    --        \texttt{c},
    --        \left\{\begin{aligned}
    --          &\texttt{t1}&&\mathrm{if}&&\texttt{c1}\\
    --          &\texttt{v3}&&\mathrm{else~if}&&\texttt{c2}\\
    --          &\texttt{v4}&&\mathrm{otherwise}
    --        \end{aligned}\right.,
    --        \left\{\begin{aligned}
    --          &\texttt{t1'}&&\mathrm{if}&&\texttt{c3}\\
    --          &\texttt{t2'}&&\mathrm{else~if}&&\texttt{c4}\\
    --          &\texttt{v4'}&&\mathrm{otherwise}
    --        \end{aligned}\right.
    --      \right]\\
    --     =~ & \left\{\begin{aligned}
    --        &\texttt{mrgIf' s' c t1 t1'}&&\mathrm{if}&&\texttt{(ite c c1 c3)}\\
    --        &\texttt{t2'}&&\mathrm{else~if}&&\texttt{(&& (! c) c4)}\\
    --        &\texttt{v3}&&\mathrm{else~if}&&\texttt{(&& c c2)}\\
    --        &\texttt{f' c v4 v4'}&&\mathrm{otherwise}
    --       \end{aligned}\right.
    --   \end{aligned}
    -- \]
    --
    -- For more details of the algorithm, please refer to
    -- [Grisette's paper](https://lsrcz.github.io/files/POPL23.pdf).

    -- *** UnionM Monad
    UnionMBase,
    IsConcrete,
    makeUnionMWrapper,
    makeUnionMWrapper',
    liftToGMonadUnion,

    -- *** Merging

    -- **** Mergeable
    GMergeable (..),
    GMergeable1 (..),
    gmergingStrategy1,
    GMergeable2 (..),
    gmergingStrategy2,
    GMergeable3 (..),
    gmergingStrategy3,

    -- **** Merging strategy
    GMergingStrategy (..),
    derivedGMergingStrategy,

    -- **** Manual merging strategy construction
    gwrapStrategy,
    gproduct2Strategy,
    DynamicSortedIdx (..),
    StrategyList (..),
    gbuildStrategyList,
    gresolveStrategy,
    gresolveStrategy',

    -- **** Simple mergeable types
    GSimpleMergeable (..),
    GSimpleMergeable1 (..),
    gmrgIte1,
    GSimpleMergeable2 (..),
    gmrgIte2,

    -- **** UnionLike operations
    GUnionLike (..),
    mrgIf,
    merge,
    mrgSingle,
    GUnionPrjOp (..),
    pattern SingleU,
    pattern IfU,
    GMonadUnion,
    getSingle,
    (#~),

    -- * Conversion between Concrete and Symbolic values
    ToCon (..),
    ToSym (..),

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
    -- >>> runFresh (fresh (ListSpec 0 2 ())) "x" :: UnionM [SymBool]
    -- UMrg (If x@2 (Single []) (If x@3 (Single [x@1]) (Single [x@0,x@1])))
    --
    -- We can generate many symbolic values at once with the 'Fresh' monad.
    -- The symbolic constants are ensured to be unique:
    --
    -- >>> :set -XScopedTypeVariables
    -- >>> :{
    --   flip runFresh "x" $ do
    --     a :: SymBool <- simpleFresh ()
    --     b :: UnionM [SymBool] <- fresh (ListSpec 0 2 ())
    --     return (a, b)
    -- :}
    -- (x@0,UMrg (If x@3 (Single []) (If x@4 (Single [x@2]) (Single [x@1,x@2]))))
    --
    -- When you are just generating a symbolic value, and do not need to compose
    -- multiple 'simpleFresh' or 'fresh' calls, you can use the 'genSym' and
    -- 'genSymSimple' functions instead.
    --
    -- >>> genSymSimple (SimpleListSpec 2 ()) "x" :: [SymBool]
    -- [x@0,x@1]
    -- >>> genSym (ListSpec 0 2 ()) "x" :: UnionM [SymBool]
    -- UMrg (If x@2 (Single []) (If x@3 (Single [x@1]) (Single [x@0,x@1])))
    --
    -- Symbolic choices from a list of symbolic values is very useful.
    -- With the 'gchooseFresh' function (specialized as @chooseFresh@ in [grisette-symir](https://hackage.haskell.org/package/grisette-symir)),
    -- we can generate a symbolic value by choosing from a list of
    -- alternative values.
    -- Grisette would generate symbolic Boolean guards to perform the symbolic
    -- choice.
    --
    -- >>> :{
    --   (flip runFresh "x" $ do
    --     a <- simpleFresh ()
    --     b <- simpleFresh ()
    --     chooseFresh [[a],[a,b],[a,a,b]]) :: UnionM [SymBool]
    -- :}
    -- UMrg (If x@2 (Single [x@0]) (If x@3 (Single [x@0,x@1]) (Single [x@0,x@0,x@1])))

    -- ** Symbolic Generation Context
    FreshIndex (..),
    FreshIdent (..),
    name,
    nameWithInfo,
    FileLocation (..),
    nameWithLoc,

    -- ** Symbolic Generation Monad
    MonadFresh (..),
    Fresh,
    FreshT,
    runFresh,
    runFreshT,

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
    gchooseFresh,
    gchooseSimpleFresh,
    gchooseUnionFresh,
    gchoose,
    gchooseSimple,
    gchooseUnion,

    -- ** Useful specifications
    EnumGenBound (..),
    EnumGenUpperBound (..),
    ListSpec (..),
    SimpleListSpec (..),

    -- * Error Handling

    -- |
    -- Grisette supports using 'Control.Monad.Except.ExceptT' to handle errors,
    -- and provides the @mrg*@ variants for the combinators in "Grisette.Lib.Mtl",
    -- for example, 'Grisette.Lib.Control.Monad.Except.mrgThrowError'.
    --
    -- >>> import Control.Monad.Except
    -- >>> import Grisette.Lib.Mtl
    -- >>> mrgThrowError AssertionError :: ExceptT AssertionError UnionM ()
    -- ExceptT (UMrg (Single (Left AssertionError)))
    --
    -- You can define your own error types, and reason about them with the
    -- solver APIs.
    --
    -- >>> :set -XDerivingVia -XDeriveGeneric -XDerivingStrategies -XLambdaCase
    -- >>> import GHC.Generics
    -- >>> import Grisette.Backend.SBV
    -- >>> :{
    --   data Error = Error1 | Error2 | Error3
    --     deriving (Show, Generic)
    --     deriving (GMergeable SymBool) via Default Error
    -- :}
    --
    -- >>> let [a,b,c] = ["a","b","c"] :: [SymBool]
    -- >>> res = mrgIf a (throwError Error1) (mrgIf b (return c) (throwError Error2)) :: ExceptT Error UnionM SymBool
    -- >>> res
    -- ExceptT (UMrg (If (|| a (! b)) (If a (Single (Left Error1)) (Single (Left Error2))) (Single (Right c))))
    -- >>> solveExcept (UnboundedReasoning z3) (\case Left _ -> conc False; Right x -> x) res
    -- Right (Model {a -> False :: Bool, b -> True :: Bool, c -> True :: Bool})
    --
    -- The solver call in the above example means that we want the solver to
    -- find the conditions under which no error is thrown, and the result is
    -- true. For more details, please refer to the
    -- [documentation of the solver APIs](#solver).
    --
    -- For those who prefer to encode errors as assertions and assumptions,
    -- we provide the 'symAssert' and 'symAssume' functions. These functions
    -- relies on the 'TransformError' type class to transform the assertions
    -- and assumptions to the user-defined error type.
    -- See their documentation for details.

    -- | #errors#

    -- ** Predefined errors
    AssertionError (..),
    VerificationConditions (..),

    -- ** Error transformation
    TransformError (..),
    symAssert,
    symAssume,
    symAssertTransformableError,
    symThrowTransformableError,

    -- ** Simulate CBMC error handling
    CBMCEither (..),
    CBMCExceptT (..),
    cbmcExcept,
    mapCBMCExceptT,
    withCBMCExceptT,

    -- * Solver backend

    -- | #solver#

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
    -- >>> import Grisette.IR.SymPrim
    -- >>> import Grisette.Backend.SBV
    -- >>> let x = "x" :: SymInteger
    -- >>> let y = "y" :: SymInteger
    -- >>> solve (UnboundedReasoning z3) (x + y ==~ 6 &&~ x - y ==~ 20)
    -- Right (Model {x -> 13 :: Integer, y -> -7 :: Integer})
    -- >>> solve (UnboundedReasoning z3) (x + y ==~ 6 &&~ x - y ==~ 19)
    -- Left Unsat
    --
    -- The first parameter of 'solve' is the solver configuration.
    -- Here it means that we should not perform any approximation, and should
    -- use the Z3 solver. For more details, see the documentation of
    -- the [grisette-backend-sbv](https://hackage.haskell.org/package/grisette-backend-sbv) package.
    --
    -- The second parameter is the formula to be solved. With the
    -- [grisette-backend-sbv](https://hackage.haskell.org/package/grisette-backend-sbv) backend, it have the type @SymBool@.
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
    -- >>> Right m <- solve (UnboundedReasoning z3) (x + y ==~ 6 &&~ x - y ==~ 20)
    -- >>> evaluateSym False m (x * y)
    -- -91
    --
    -- You may notice that the first argument to the 'evaluateSym' function is
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
    -- >>> evaluateSym False m (x * y + z)
    -- (+ -91 z)
    -- >>> evaluateSym True m (x * y + z)
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
    -- >>> :set -XLambdaCase -XDeriveGeneric -XDerivingStrategies -XDerivingVia
    -- >>> import Control.Monad.Except
    -- >>> import Control.Exception
    -- >>> import GHC.Generics
    -- >>> :{
    -- data Error = Arith | Assert
    --   deriving (Show, Generic)
    --   deriving (GMergeable SymBool, GSEq SymBool) via (Default Error)
    -- :}
    --
    -- Then we define how to transform the generic errors to the error type.
    --
    -- >>> :{
    --   instance TransformError ArithException Error where
    --     transformError _ = Arith
    --   instance TransformError AssertionError Error where
    --     transformError _ = Assert
    -- :}
    --
    -- Then we can perform the symbolic evaluation. The `divs` function throws
    -- 'ArithException' when the divisor is 0, which would be transformed to
    -- @Arith@, and the `symAssert` function would throw 'AssertionError' when
    -- the condition is false, which would be transformed to @Assert@.
    --
    -- >>> let x = "x" :: SymInteger
    -- >>> let y = "y" :: SymInteger
    -- >>> :{
    --   -- equivalent concrete program:
    --   -- let x = x `div` y
    --   -- if z > 0 then assert (x >= y) else return ()
    --   res :: ExceptT Error UnionM ()
    --   res = do
    --     z <- x `divs` y
    --     mrgIf (z >~ 0) (symAssert (x >=~ y)) (return ())
    -- :}
    --
    -- Then we can ask the solver to find a counter-example that would lead to
    -- an assertion violation error, but do not trigger the division by zero
    -- error.
    -- This can be done by asking the solver to find a path that produce
    -- @Left Assert@.
    --
    -- >>> res
    -- ExceptT (UMrg (If (|| (= y 0) (&& (< 0 (div x y)) (! (<= y x)))) (If (= y 0) (Single (Left Arith)) (Single (Left Assert))) (Single (Right ()))))
    --
    -- > >>> solveExcept (UnboundedReasoning z3) (==~ Left Assert) res
    -- > Right (Model {x -> -6 :: Integer, y -> -3 :: Integer}) -- possible output
    --
    -- Grisette also provide implementation for counter-example guided inductive
    -- synthesis (CEGIS) algorithm. See the documentation for 'CEGISSolver' for
    -- more details.

    -- ** Solver interface
    Solver (..),
    UnionWithExcept (..),
    solveExcept,
    solveMultiExcept,

    -- ** Counter-example Guided Inductive Synthesis (CEGIS)
    CEGISSolver (..),
    CEGISCondition (..),
    cegisPostCond,
    cegisPrePost,
    cegis,
    cegisExcept,
    cegisExceptStdVC,
    cegisExceptVC,
    cegisExceptMultiInputs,
    cegisExceptStdVCMultiInputs,
    cegisExceptVCMultiInputs,

    -- ** Symbolic constant extraction

    -- Grisette supports the extraction of the symbolic constant symbols from a
    -- symbolic value. This is useful for manipulating the models returned by
    -- the solver. The builtin CEGIS procedure relies on this.
    SymbolSetOps (..),
    SymbolSetRep (..),
    GExtractSymbolics (..),

    -- ** Evaluation with a model

    -- |
    -- When given a satisfiable formula, a solver can return a model that specifies
    -- the concrete assignments of the variables in the formula to make the formula
    -- true. We can use this model to evaluate some symbolic values by substituting
    -- the symbolic constants with the concrete assignments.
    ModelOps (..),
    ModelRep (..),
    GEvaluateSym (..),
    gevaluateSymToCon,

    -- ** Substitution of a symbol
    GSubstituteSym (..),
    GSubstituteSym' (..),
    GSubstituteSymSymbol (..),

    -- * Type Class Derivation
    Default (..),
    Default1 (..),

    -- * Utilities

    -- ** Memoization
    htmemo,
    htmemo2,
    htmemo3,
    htmup,
    htmemoFix,

    -- ** Bundled Constructor Wrappers
    uTrue,
    uFalse,
    uunit,
    uTuple2,
    uTuple3,
    uJust,
    uNothing,
    uLeft,
    uRight,
    uInL,
    uInR,
    uAssertionViolation,
    uAssumptionViolation,
  )
where

import Generics.Deriving (Default (..), Default1 (..))
import Grisette.Core.BuiltinUnionMWrappers
import Grisette.Core.Control.Exception
import Grisette.Core.Control.Monad.CBMCExcept
import Grisette.Core.Control.Monad.Union
import Grisette.Core.Control.Monad.UnionMBase
import Grisette.Core.Data.Class.BitVector
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.CEGISSolver
import Grisette.Core.Data.Class.Error
import Grisette.Core.Data.Class.Evaluate
import Grisette.Core.Data.Class.ExtractSymbolics
import Grisette.Core.Data.Class.Function
import Grisette.Core.Data.Class.GenSym
import Grisette.Core.Data.Class.Integer
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Data.Class.ModelOps
import Grisette.Core.Data.Class.SOrd
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Core.Data.Class.Solvable
import Grisette.Core.Data.Class.Solver
import Grisette.Core.Data.Class.Substitute
import Grisette.Core.Data.Class.ToCon
import Grisette.Core.Data.Class.ToSym
import Grisette.Core.Data.FileLocation
import Grisette.Core.Data.MemoUtils
import Grisette.Core.TH

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.Lib.Base
-- >>> import Grisette.IR.SymPrim
-- >>> :set -XDataKinds
-- >>> :set -XBinaryLiterals
-- >>> :set -XFlexibleContexts
-- >>> :set -XFlexibleInstances
-- >>> :set -XFunctionalDependencies
