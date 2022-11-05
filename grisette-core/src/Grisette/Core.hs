{-# LANGUAGE PatternSynonyms #-}

module Grisette.Core
  ( -- * Note for the examples

    --

    -- | This module does not contain actual implementation for symbolic primitive types, and
    -- the examples in this module cannot be executed solely with @grisette-core@ package.
    -- They rely on the implementation in @grisette-symprim@ package.

    -- * Symbolic Operators

    -- | #symop#
    GSEq (..),
    GSOrd (..),
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
    GSymIntegerOp,
    Function (..),
    (#~),

    -- * Symbolic Primitives
    PrimWrapper (..),
    pattern Conc,
    slocsymb,
    ilocsymb,
    GExtractSymbolics (..),

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

    -- * Evaluation and Conversion between Concrete and Symbolic values
    GEvaluateSym (..),
    ModelOps (..),
    SymbolSetOps (..),
    gevaluateSymToCon,
    ToCon (..),
    ToSym (..),
    SubstituteSym (..),
    SubstituteSym' (..),
    SubstituteSymSymbol (..),

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
import Grisette.Core.Data.Class.PrimWrapper
import Grisette.Core.Data.Class.SOrd
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Core.Data.Class.Solver
import Grisette.Core.Data.Class.Substitute
import Grisette.Core.Data.Class.ToCon
import Grisette.Core.Data.Class.ToSym
import Grisette.Core.Data.FileLocation
import Grisette.Core.Data.MemoUtils
import Grisette.Core.TH
