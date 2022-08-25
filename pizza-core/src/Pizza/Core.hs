{-# LANGUAGE PatternSynonyms #-}

module Pizza.Core
  ( -- * Note for the examples

    --

    -- | This module does not contain actual implementation for symbolic primitive types, and
    -- the examples in this module cannot be executed solely with @pizza-core@ package.
    -- They rely on the implementation in @pizza-symprim@ package.

    -- * Symbolic Operators

    -- | #symop#
    SEq (..),
    SOrd (..),
    LogicalOp (..),
    SymBoolOp,
    ITEOp (..),
    BVConcat (..),
    BVExtend (..),
    BVSelect (..),
    bvextract,
    SignedDivMod (..),
    UnsignedDivMod (..),
    SignedQuotRem (..),
    SymIntegerOp,
    Function (..),
    (#~),

    -- * Symbolic Primitives
    PrimWrapper (..),
    pattern Conc,
    slocsymb,
    ilocsymb,
    ExtractSymbolics (..),

    -- * UnionM Monad
    UnionMBase,
    IsConcrete,
    UnionPrjOp (..),
    pattern SingleU,
    pattern IfU,
    makeUnionMWrapper,
    makeUnionMWrapper',
    liftToMonadUnion,

    -- * Merging
    MergingStrategy (..),
    derivedMergingStrategy,
    wrapStrategy,
    product2Strategy,
    Mergeable (..),
    Mergeable1 (..),
    Mergeable2 (..),
    mergingStrategy1,
    mergingStrategy2,
    DynamicSortedIdx (..),
    StrategyList (..),
    buildStrategyList,
    resolveStrategy,
    resolveStrategy',
    -- withMergeable,
    SimpleMergeable (..),
    SimpleMergeable1 (..),
    mrgIte1,
    SimpleMergeable2 (..),
    mrgIte2,
    -- withSimpleMergeable,
    -- withSimpleMergeableU,
    UnionLike (..),
    mrgIf,
    mrgSingle,
    merge,
    -- withUnionSimpleMergeable,
    -- withUnionSimpleMergeableU,
    MonadUnion,
    getSingle,
    {-
    mrgReturnWithStrategy,
    mrgBindWithStrategy,
    mrgReturn,
    (>>=~),
    -}

    {-
        -- * Wrapped Monadic Combinators with Mergeable Knowledge Propagaion
        mrgFoldM,
        (>>~),
        mrgMzero,
        mrgMplus,
        mrgFmap,

        mrgFoldlM,
        mrgFoldrM,
        mrgTraverse_,
        mrgFor_,
        mrgMapM_,
        mrgForM_,
        mrgSequence_,
        mrgMsum,

        mrgTraverse,
        mrgSequenceA,
        mrgFor,
        mrgMapM,
        mrgForM,
        mrgSequence,

        mrgLift,
        mrgThrowError,
        mrgCatchError,
        -}

    -- * Standard Errors

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

    -- * Symbolic Generation

    -- ** Symbolic Generation Context
    GenSymIndex (..),
    GenSymIdent,
    pattern GenSymIdent,
    name,
    nameWithInfo,
    FileLocation (..),
    nameWithLoc,

    -- ** Symbolic Generation Monad
    MonadGenSymFresh (..),
    GenSymFresh,
    GenSymFreshT,
    runGenSymFresh,
    runGenSymFreshT,

    -- ** Symbolic Generation Class
    GenSym (..),
    GenSymSimple (..),
    genSym,
    genSymSimple,

    -- ** Symbolic Generation Class Derivation
    derivedNoSpecGenSymFresh,
    derivedNoSpecGenSymSimpleFresh,
    derivedSameShapeGenSymSimpleFresh,

    -- ** Symbolic choice
    choose,
    simpleChoose,
    chooseU,

    -- ** Useful specifications
    EnumGenBound (..),
    EnumGenUpperBound (..),
    ListSpec (..),
    SimpleListSpec (..),

    -- * Evaluation and Conversion between Concrete and Symbolic values
    EvaluateSym (..),
    evaluateSymToCon,
    ToCon (..),
    ToSym (..),

    -- * Solver interface
    Solver (..),
    ExtractUnionEither (..),
    solveFallable,
    solveMultiFallable,
    cegisFallable,
    cegisFallable',

    -- * Memoization
    htmemo,
    htmemo2,
    htmemo3,
    htmup,
    htmemoFix,

    -- * Bundled Constructor Wrappers
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

    -- * Type Class Derivation
    Default (..),
    Default1 (..),
  )
where

import Generics.Deriving (Default (..), Default1 (..))
import Pizza.Core.BuiltinUnionMWrappers
import Pizza.Core.Control.Exception
import Pizza.Core.Control.Monad.CBMCExcept
import Pizza.Core.Control.Monad.Union
import Pizza.Core.Control.Monad.UnionMBase
import Pizza.Core.Data.Class.BitVector
import Pizza.Core.Data.Class.Bool
import Pizza.Core.Data.Class.Error
import Pizza.Core.Data.Class.Evaluate
import Pizza.Core.Data.Class.ExtractSymbolics
import Pizza.Core.Data.Class.Function
import Pizza.Core.Data.Class.GenSym
import Pizza.Core.Data.Class.Integer
import Pizza.Core.Data.Class.Mergeable
import Pizza.Core.Data.Class.PrimWrapper
import Pizza.Core.Data.Class.SOrd
import Pizza.Core.Data.Class.SimpleMergeable
import Pizza.Core.Data.Class.Solver
import Pizza.Core.Data.Class.ToCon
import Pizza.Core.Data.Class.ToSym
import Pizza.Core.Data.FileLocation
import Pizza.Core.Data.MemoUtils
import Pizza.Core.TH
