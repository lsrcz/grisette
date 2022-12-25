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
    GUnionPrjOp (..),
    pattern SingleU,
    pattern IfU,
    makeUnionWrapper,
    makeUnionWrapper',
    liftToGMonadUnion,

    -- * Merging
    GMergingStrategy (..),
    derivedGRootStrategy,
    gwrapStrategy,
    gproduct2Strategy,
    GMergeable (..),
    GMergeable1 (..),
    GMergeable2 (..),
    GMergeable3 (..),
    grootStrategy1,
    grootStrategy2,
    grootStrategy3,
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
    -- withGSimpleMergeable,
    -- withGSimpleMergeableU,
    GUnionLike (..),
    mrgIf,
    mrgSingle,
    merge,
    -- withUnionGSimpleMergeable,
    -- withUnionGSimpleMergeableU,
    GMonadUnion,
    simpleMerge,
    {-
    mrgReturnWithStrategy,
    mrgBindWithStrategy,
    mrgReturn,
    (>>=~),
    -}

    {-
        -- * Wrapped Monadic Combinators with GMergeable Knowledge Propagaion
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
    FreshIndex (..),
    FreshIdent,
    pattern FreshIdent,
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

    -- * Evaluation and Conversion between Concrete and Symbolic values
    GEvaluateSym (..),
    ModelOps (..),
    SymbolSetOps (..),
    gevaluateSymToCon,
    ToCon (..),
    ToSym (..),
    GSubstituteSym (..),
    GSubstituteSym' (..),
    GSubstituteSymSymbol (..),

    -- * Solver interface
    Solver (..),
    UnionWithExcept (..),
    solveExcept,
    solveMultiExcept,
    CEGISSolver (..),
    CEGISCondition (..),
    cegisPostCond,
    cegisExcept,
    cegisExceptVC,
    cegisExceptMultiInputs,
    cegisExceptVCMultiInputs,

    -- * Memoization
    htmemo,
    htmemo2,
    htmemo3,
    htmup,
    htmemoFix,

    -- * Bundled Constructor Wrappers
    mrgTrue,
    mrgFalse,
    mrgUnit,
    mrgTuple2,
    mrgTuple3,
    mrgJust,
    mrgNothing,
    mrgLeft,
    mrgRight,
    mrgInL,
    mrgInR,
    mrgAssertionViolation,
    mrgAssumptionViolation,

    -- * Type Class Derivation
    Default (..),
    Default1 (..),
  )
where

import Generics.Deriving (Default (..), Default1 (..))
import Grisette.Core.BuiltinUnionWrappers
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
