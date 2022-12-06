{-# LANGUAGE PatternSynonyms #-}

module Grisette.Core
  ( -- * Note for the examples

    --

    -- | This module does not contain actual implementation for symbolic primitive types, and
    -- the examples in this module cannot be executed solely with @grisette-core@ package.
    -- They rely on the implementation in @grisette-symir@ and @grisette-backend-sbv@ packages,
    -- which provides the solvable type and the solver backend implementations,
    -- respectively.

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
    -- After the evaluation, constraint solvers can be used to assign concrete
    -- values to the symbolic values to meet some criteria.
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
    -- The symbolic value representation and the merging algorithm is then
    -- critical to the performance of symbolic evaluation. 
    -- In Grisette, we make a distinction between two kinds of symbolic values:
    -- __/solvable types/__ and __/unsolvable types/__. 
    --
    -- __/Solvable types/__ are types that are directly supported by the
    -- underlying solver, and will be represented directly as symbolic formulas.
    -- Such types include symbolic Boolean values, symbolic integers, and
    -- symbolic bitvectors, etc.
    -- The values of solvable types can be __/fully/__ merged into a single
    -- value, for example, assume that @a@, @b@, @c@ are symbolic Boolean
    -- values (have the 'SymBool' type),
    -- the following program will be evaluated to a single
    -- symbolic Boolean formula, shown in the format of SMT-LIB:
    --
    -- > if a then b else c -- pseudo code, not real Grisette code
    -- > -- result: (ite a b c)
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
    -- symbolic union (see 'UnionM').
    --
    -- In the following example, assume that the lists have the type @[SymBool]@,
    -- Grisette would merge the lists with the same lengths together, and keep
    -- the lists with different lengths in a symbolic union.
    -- The symbolic union is simply an if-then-else tree, in the following
    -- example, 'If' represents the symbolic conditionals, and 'Single'
    -- represents unconditional choices.
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
    -- Generally, merging the possible branches in a symbolic union can reduce
    -- the number of paths to be explored in the future, but may make the path
    -- conditions larger and harder to solve. To have a good balance between
    -- this, Grisette has built a hierarical merging algorithm, which is
    -- configurable via 'GMergingStrategy'. For algebraic data types, we have
    -- prebuilt merging strategies via the derivation of the 'GMergeable' type
    -- class. You only need to know the details of the merging algorithm if you
    -- are going to add support for non-algebraic data types.

    -- ** Solvable types

    -- | A solvable type is a type that can be represented as a formula and is
    -- directly supported by the underlying constraint solvers.
    -- The solvable type implementation provided by @grisette-symir@ package
    -- currently supports symbolic Boolean types (@SymBool@ or @Sym Bool@),
    -- symbolic unbounded integers (@SymInteger@ or @Sym Integer@),
    -- and symbolic bitvectors (signed bitvectors has the type @SymIntN 5@ or
    -- @Sym (IntN 5)@, and unsigned bitvectors has the type @SymWordN 5@ or
    -- @Sym (WordN 5)@). Please see the documentation for the @grisette-symir@
    -- package for details.
    --
    -- A solvable type value may consist of the following constructs:
    --
    -- * Concrete values, e.g., @SymBool@ can represent a concrete true value,
    -- * Symbolic constants, e.g., @a@, @b@, etc. These symbolic constants can
    -- be considered as placeholders for concrete values, and a solver can
    -- decide the concrete assignments to them to satisfy a formula. See
    -- [Solver Interface](#solver) for the details of the solver interface.
    -- * Complex symbolic formula with symbolic operations, e.g., @(&& a b)@.
    --
    -- We provide the construction procedure of a concrete value or symbolic
    -- constant via the 'Solvable' type class.
    --
    -- >>> import Grisette.Core
    -- >>> import Grisette.Lib.Base
    -- >>> import Grisette.IR.SymPrim
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
    -- @grisette-symir@ package, the 'GSEq' and 'GSOrd' type classes are
    -- parametrized with the solvable type implementation (the symbolic Boolean
    -- type provided by the implementation). In most
    -- cases, you can just work with the implementation in @grisette-symir@
    -- package, and you can use specialized 'SEq' and 'SOrd'
    -- constraints and the specialized operators in @grisette-symir@.
    -- This may reduce the need for manual type annotations.
    --
    -- >>> let a = "a" :: SymInteger
    -- >>> let b = "b" :: SymInteger
    -- >>> a `gsymeq` b :: SymBool -- The type annotation is required
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
    (#~),

    -- ** Unsolvable types

    -- | There are types that are not directly supported by the underlying
    -- solver and cannot be represented as SMT formulas. We call these types
    -- unsolvable types.
    -- To help symbolic evaluate such types, we represent them with
    -- __/symbolic unions/__.
    -- A symbolic union is a set of multiple values from different execution
    -- paths, each guarded with their path condition.
    -- Its value is decided by the truth value of the path conditions, and can
    -- be determined by an SMT solver.
    --
    -- The symbolic union type in Grisette is 'UnionMBase'. It is parametrized
    -- with the symbolic Boolean type, and usually you can just use the
    -- 'UnionM' type provided by @grisette-symir@.
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
    -- You can then use monadic constructs to model sequential programs
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
    --
    -- Grisette provides many combinators with the @mrg@ prefix.
    -- These combinators would cache the merging strategy and help merge the result.
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
    -- 'GMergeable' type class for it. If you are only working with algebraic
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
    -- can symbolically evaluate a program with error handling mechanisms.
    --
    -- Let us use the following pseudo code as an example. In the then
    -- branch, we want to throw an error, while in the else branch, we return a
    -- value.
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
    -- handling
    --
    -- >>> import Control.Monad.Except
    -- >>> mrgIf "a" (throwError Fail) (return "x") :: ExceptT Error UnionM SymInteger
    -- ExceptT (UMrg (If a (Single (Left Fail)) (Single (Right x))))
    --
    -- __If you are not going to manually configure the system by writing a__
    -- __`GMergingStrategy`, you can safely ignore the following contents in this section.__
    -- 
    -- In Grisette, the symbolic union has the Ordered Guards (ORG)
    -- representation, which can be viewed as a nested if-then-else with some
    -- representation invariant.
    --
    -- For example, the following symbolic union represents a symbolic list of
    -- symbolic integers with length 1, 2 or 3
    --
    -- \[
    --   \left\{\begin{aligned}&\texttt{[a]}&&\mathrm{if}&&\texttt{c1}\\&\texttt{[b,b]}&&\mathrm{else~if}&&\texttt{c2}\\&\texttt{[a,b,c]}&&\mathrm{otherwise}\end{aligned}\right.
    -- \]
    --
    -- In Haskell syntax, it may be represented as the following nested
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
    --   \left\{\begin{aligned}&\texttt{head [a]}&&\mathrm{if}&&\texttt{c1}\\&\texttt{head [b,b]}&&\mathrm{else~if}&&\texttt{c2}\\&\texttt{head [a,b,c]}&&\mathrm{otherwise}\end{aligned}\right.
    -- \]
    --
    -- or, equivalently
    --
    -- \[
    --   \left\{\begin{aligned}&\texttt{a}&&\mathrm{if}&&\texttt{c1}\\&\texttt{b}&&\mathrm{else~if}&&\texttt{c2}\\&\texttt{a}&&\mathrm{otherwise}\end{aligned}\right.
    -- \]
    --
    -- Further symbolic evaluation may also distribute further computations
    -- over the branches in this result, and the identical branches will cause
    -- redundant computation. To mitigate this, Grisette would try to merge
    -- the branches, and the previous result would become
    --
    -- \[
    --   \left\{\begin{aligned}&\texttt{a}&&\mathrm{if}&&\texttt{c1}\vee\neg\texttt{c2}\\&\texttt{b}&&\mathrm{otherwise}\\\end{aligned}\right.
    -- \]
    --
    -- In Grisette, such merging happens in the `mrgIf` or `unionIf` functions,
    -- which model the symbolic conditional branching semantics.
    -- To keep the merging efficient and generate small constraints, we enforce
    -- that the symbolic union maintains the values in a sorted way, and merge
    -- the unions with a mergesort-style merging algorithm. In the following
    -- example, the two ORG container passed to the 'mrgIf' are sorted
    -- with the natural ordering on the integers. In the result, the values are
    -- also organized in a sorted way, and the path conditions are correctly
    -- maintained.
    --
    -- \[
    --   \texttt{mrgIf}\left[\texttt{c}, \left\{\begin{aligned}&\texttt{1}&&\mathrm{if}&&\texttt{c1}\\&\texttt{3}&&\mathrm{else~if}&&\texttt{c2}\\&\texttt{4}&&\mathrm{otherwise}\end{aligned}\right.,\left\{\begin{aligned}&\texttt{1}&&\mathrm{if}&&\texttt{c3}\\&\texttt{2}&&\mathrm{else~if}&&\texttt{c4}\\&\texttt{4}&&\mathrm{otherwise}\end{aligned}\right.\right]=\left\{\begin{aligned}&\texttt{1}&&\mathrm{if}&&\texttt{(ite c c1 c3)}\\&\texttt{2}&&\mathrm{else~if}&&\texttt{(&& (! c) c3)}\\&\texttt{3}&&\mathrm{else~if}&&\texttt{(&& c c2)}\\&\texttt{4}&&\mathrm{otherwise}\end{aligned}\right.
    -- \]
    --
    -- So far, we have described ORG as a flat list of values.
    -- When the list is long, it is beneficial to partition the values and merge
    -- (some or all) partitionals into nested ORG values.
    -- This hierarchical ORG representation is particularly useful for complex
    -- data types, such as tuples, which tend to yield long lists.
    --
    -- In the following example, @v*@ are values, while @t*@ are ORG containers;
    -- @t1@ and 
    --
    -- \[
    --   \texttt{mrgIf}\left[\texttt{s}, \texttt{c}, \left\{\begin{aligned}&\texttt{t1}&&\mathrm{if}&&\texttt{c1}\\&\texttt{v3}&&\mathrm{else~if}&&\texttt{c2}\\&\texttt{v4}&&\mathrm{otherwise}\end{aligned}\right., \left\{\begin{aligned}&\texttt{t1'}&&\mathrm{if}&&\texttt{c3}\\&\texttt{t2'}&&\mathrm{else~if}&&\texttt{c4}\\&\texttt{v4'}&&\mathrm{otherwise}\end{aligned}\right.\right]
    -- \]
    --
    -- \[
    --   \left\{\begin{aligned}&\texttt{t1}&&\mathrm{if}&&\texttt{c1}\\&\texttt{v2}&&\mathrm{else if}&&\texttt{c2}\\&\texttt{v3}&&\mathrm{otherwise}&&\end{aligned}\right.\hspace{2em}\mathrm{where}\hspace{2em}\texttt{t1} = \left\{\begin{aligned}&\texttt{v11}&&\mathrm{if}&&\texttt{c11}\\&\texttt{v12}&&\mathrm{else if}&&\texttt{c12}\\&\texttt{v13}&&\mathrm{otherwise}&&\end{aligned}\right.
    -- \]
    -- 
    -- In Haskell syntax, it can be represented as follows:
    --
    -- > If      c1    (If c11 (Single v11) (If c12 (Single v12) (Single v13)))
    -- >   (If   c2    (Single v2)
    -- >               (Single v3))
    --
    -- In the nested container, when @c1@ is true, the result will be further
    -- determined by @c11@ and @c12@. This container, would also maintain the
    -- values in a __/hierarically/__ sorted way.
    --
    --
    -- In Grisette, a type, 'UnionM', has this symbolic union semantics. It is
    -- a monad, so you can extract the values from it with a monadic
    -- do-notation. We also provide combinators performing the symbolic @if@,
    -- namely 'mrgIf'. These helps write symbolic code easily.
    --
    -- For example, a symbolic program shown as the following pseudo-code
    --
    -- > let a = if a then [b] else [b,c]
    -- > let d = if d then [e] else [e,f]
    -- > x = a ++ d
    --
    -- may be implemented in Grisette as
    --
    -- >>> :{
    --   x :: UnionM [SymInteger]
    --   x = do a <- mrgIf "a" (single ["b"]) (single ["b","c"])
    --          d <- mrgIf "d" (single ["e"]) (single ["e","f"])
    --          mrgReturn $ a ++ d
    -- :}
    --
    -- >>> x
    -- UMrg (If (&& a d) (Single [b,e]) (If (|| a d) (Single [b,(ite a e c),(ite a f e)]) (Single [b,c,e,f])))
    --
    -- You can see that the path conditions will be augmented, and the result
    -- says that when @(&& a d)@ is true, then the result is @[b,e]@. If that
    -- is not true, then when @(|| a d)@ is true, the result is 
    -- @[b,(ite a e c),(ite a f e)]@, or the result is @[b,c,e,f]@.
    --
    -- The interesting part here is that the two lists with the same length are
    -- merged together. This is very important to mitigate the path explosion
    -- problem in symbolic evaluation. A good merging algorithm for the values
    -- is the key to the efficiency of Grisette.
    --
    -- In Grisette, the merging for the types are abstracted in the
    -- 'GMergingStrategy' type class. In most cases, when you are using
    -- algebraic data types in your code, you can simply derive the `GMergeable`
    -- class to get the merging strategy described in
    -- [Grisette's paper](https://lsrcz.github.io/files/POPL23.pdf), and you
    -- don't need to know the details of the merging strategy and how it works.
    --
    -- Sometimes you may want to use Grisette with non-algebraic data types,
    -- or the default derived merging strategy is not efficient enough. In these
    -- cases, you may configure the Grisette system by manually implement the
    -- `GMergeable` class for your types. See the documentation for
    -- `GMergingStrategy` for details, or refer to
    -- [Grisette's paper](https://lsrcz.github.io/files/POPL23.pdf).

    -- *** UnionM Monad
    UnionMBase,
    IsConcrete,
    GUnionPrjOp (..),
    pattern SingleU,
    pattern IfU,
    makeUnionMWrapper,
    makeUnionMWrapper',
    liftToGMonadUnion,

    -- *** Merging
    GMergingStrategy (..),
    derivedGMergingStrategy,
    gwrapStrategy,
    gproduct2Strategy,
    GMergeable (..),
    GMergeable1 (..),
    GMergeable2 (..),
    gmergingStrategy1,
    gmergingStrategy2,
    DynamicSortedIdx (..),
    StrategyList (..),
    gbuildStrategyList,
    gresolveStrategy,
    gresolveStrategy',
    -- withMergeable,
    GSimpleMergeable (..),
    GSimpleMergeable1 (..),
    gmrgIte1,
    GSimpleMergeable2 (..),
    gmrgIte2,
    GUnionLike (..),
    mrgIf,
    mrgSingle,
    merge,
    GMonadUnion,
    getSingle,

    -- * Conversion between Concrete and Symbolic values

    ToCon (..),
    ToSym (..),
    GSubstituteSym (..),
    GSubstituteSym' (..),
    GSubstituteSymSymbol (..),
    
    -- * Symbolic Generation

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
    GGenSym (..),
    GenSymSimple (..),
    ggenSym,
    genSymSimple,

    -- ** Symbolic Generation Class Derivation
    derivedNoSpecGFresh,
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
    -- | #errors#
    AssertionError (..),
    VerificationConditions (..),
    ArithException (..),
    TransformError (..),
    symAssert,
    symAssume,
    symFailIfNot,
    symThrowTransformableError,
    CBMCEither (..),
    CBMCExceptT (..),
    cbmcExcept,
    mapCBMCExceptT,
    withCBMCExceptT,

    -- * Solver backend

    -- ** Solver interface

    -- | #solver#
    Solver (..),
    ExtractUnionEither (..),
    solveFallable,
    solveMultiFallable,
    cegisFallable,
    cegisFallable',

    -- ** Symbolic constant extraction
    SymbolSetOps (..),
    SymbolSetRep (..),
    GExtractSymbolics (..),

    -- ** Evaluation with a model
    ModelOps (..),
    ModelRep (..),
    GEvaluateSym (..),
    gevaluateSymToCon,

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
import Grisette.Core.Data.Class.Error
import Grisette.Core.Data.Class.Evaluate
import Grisette.Core.Data.Class.ExtractSymbolics
import Grisette.Core.Data.Class.Function
import Grisette.Core.Data.Class.GenSym
import Grisette.Core.Data.Class.Integer
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Data.Class.ModelOps
import Grisette.Core.Data.Class.Solvable
import Grisette.Core.Data.Class.SOrd
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Core.Data.Class.Solver
import Grisette.Core.Data.Class.Substitute
import Grisette.Core.Data.Class.ToCon
import Grisette.Core.Data.Class.ToSym
import Grisette.Core.Data.FileLocation
import Grisette.Core.Data.MemoUtils
import Grisette.Core.TH

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XBinaryLiterals
-- >>> :set -XFlexibleContexts
-- >>> :set -XFlexibleInstances
-- >>> :set -XFunctionalDependencies

