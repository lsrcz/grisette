{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- |
-- Module      :   Grisette.Unified
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Unified
  ( -- * Evaluation mode
    EvalModeTag (..),
    IsConMode,
    BaseMonad,

    -- * Aggregated constraints
    genEvalMode,
    TheoryToUnify (..),
    EvalModeBase,
    EvalModeInteger,
    EvalModeBV,
    EvalModeFP,
    EvalModeAlgReal,
    EvalModeAll,
    MonadEvalModeAll,
    DecideEvalMode (..),
    withMode,
    EvalModeConvertible (..),

    -- * Unified operations

    -- ** Unified simple mergeable
    UnifiedBranching (..),
    UnifiedSimpleMergeable (..),
    UnifiedSimpleMergeable1 (..),
    UnifiedSimpleMergeable2 (..),
    mrgIf,
    liftUnion,
    mrgIte,
    mrgIte1,
    liftMrgIte,
    mrgIte2,
    liftMrgIte2,
    simpleMerge,
    (.#),
    onUnion,
    onUnion2,
    onUnion3,
    onUnion4,

    -- ** Unified ITE operator
    UnifiedITEOp (..),
    symIte,
    symIteMerge,

    -- ** Unified SymEq
    UnifiedSymEq (..),
    UnifiedSymEq1 (..),
    UnifiedSymEq2 (..),
    (.==),
    (./=),
    symDistinct,
    liftSymEq,
    symEq1,
    liftSymEq2,
    symEq2,

    -- ** Unified SymOrd
    UnifiedSymOrd (..),
    UnifiedSymOrd1 (..),
    UnifiedSymOrd2 (..),
    (.<=),
    (.<),
    (.>=),
    (.>),
    symCompare,
    liftSymCompare,
    symCompare1,
    liftSymCompare2,
    symCompare2,
    symMax,
    symMin,
    mrgMax,
    mrgMin,

    -- ** Unified finite bits
    UnifiedFiniteBits (..),
    symTestBit,
    symSetBitTo,
    symFromBits,
    symBitBlast,
    symLsb,
    symMsb,
    symPopCount,
    symCountLeadingZeros,
    symCountTrailingZeros,

    -- ** Unified conversions
    UnifiedFromIntegral (..),
    symFromIntegral,
    UnifiedSafeBitCast (..),
    safeBitCast,
    UnifiedSafeFromFP (..),
    safeFromFP,

    -- ** Unified safe ops
    UnifiedSafeDiv (..),
    safeDiv,
    safeMod,
    safeDivMod,
    safeQuot,
    safeRem,
    safeQuotRem,
    UnifiedSafeLinearArith (..),
    safeAdd,
    safeNeg,
    safeSub,
    UnifiedSafeSymRotate (..),
    safeSymRotateL,
    safeSymRotateR,
    UnifiedSafeSymShift (..),
    safeSymShiftL,
    safeSymShiftR,
    safeSymStrictShiftL,
    safeSymStrictShiftR,
    UnifiedSafeFdiv (..),
    safeFdiv,

    -- ** Shared constraints
    UnifiedPrim,
    UnifiedBasicPrim,

    -- * Unified types

    -- ** Boolean
    GetBool,

    -- ** Bit-vector
    GetIntN,
    GetWordN,
    GetSomeWordN,
    GetSomeIntN,
    UnifiedBV,
    SafeUnifiedBV,
    SafeUnifiedSomeBV,

    -- ** Integer
    GetInteger,
    UnifiedInteger,

    -- ** FP
    GetFP,
    GetFPRoundingMode,
    UnifiedFP,
    SafeUnifiedFP,

    -- ** AlgReal
    GetAlgReal,
    UnifiedAlgReal,

    -- ** Data
    GetData,
    UnifiedData,
    extractData,
    wrapData,

    -- ** Functions
    GetFun,
    GetFun2,
    GetFun3,
    GetFun4,
    GetFun5,
    GetFun6,
    GetFun7,
    GetFun8,
    UnifiedFun,
    UnifiedFunConstraint,
    unifiedFunInstanceName,
    genUnifiedFunInstance,

    -- ** Supplemental conversions
    UnifiedBVBVConversion,
    UnifiedBVFPConversion,
    SafeUnifiedBVFPConversion,
    UnifiedFPFPConversion,
  )
where

import Grisette.Internal.Internal.Impl.Unified.Class.UnifiedSimpleMergeable (onUnion, onUnion2, onUnion3, onUnion4, (.#))
import Grisette.Internal.Unified.BVBVConversion
  ( UnifiedBVBVConversion,
  )
import Grisette.Internal.Unified.BVFPConversion
  ( SafeUnifiedBVFPConversion,
    UnifiedBVFPConversion,
  )
import Grisette.Internal.Unified.Class.UnifiedFiniteBits
  ( UnifiedFiniteBits (..),
    symBitBlast,
    symCountLeadingZeros,
    symCountTrailingZeros,
    symFromBits,
    symLsb,
    symMsb,
    symPopCount,
    symSetBitTo,
    symTestBit,
  )
import Grisette.Internal.Unified.Class.UnifiedFromIntegral
  ( UnifiedFromIntegral (..),
    symFromIntegral,
  )
import Grisette.Internal.Unified.Class.UnifiedITEOp
  ( UnifiedITEOp (..),
    symIte,
    symIteMerge,
  )
import Grisette.Internal.Unified.Class.UnifiedSafeBitCast
  ( UnifiedSafeBitCast (..),
    safeBitCast,
  )
import Grisette.Internal.Unified.Class.UnifiedSafeDiv
  ( UnifiedSafeDiv (..),
    safeDiv,
    safeDivMod,
    safeMod,
    safeQuot,
    safeQuotRem,
    safeRem,
  )
import Grisette.Internal.Unified.Class.UnifiedSafeFdiv
  ( UnifiedSafeFdiv (..),
    safeFdiv,
  )
import Grisette.Internal.Unified.Class.UnifiedSafeFromFP
  ( UnifiedSafeFromFP (..),
    safeFromFP,
  )
import Grisette.Internal.Unified.Class.UnifiedSafeLinearArith
  ( UnifiedSafeLinearArith (..),
    safeAdd,
    safeNeg,
    safeSub,
  )
import Grisette.Internal.Unified.Class.UnifiedSafeSymRotate
  ( UnifiedSafeSymRotate (..),
    safeSymRotateL,
    safeSymRotateR,
  )
import Grisette.Internal.Unified.Class.UnifiedSafeSymShift
  ( UnifiedSafeSymShift (..),
    safeSymShiftL,
    safeSymShiftR,
    safeSymStrictShiftL,
    safeSymStrictShiftR,
  )
import Grisette.Internal.Unified.Class.UnifiedSimpleMergeable
  ( UnifiedBranching (..),
    UnifiedSimpleMergeable (..),
    UnifiedSimpleMergeable1 (..),
    UnifiedSimpleMergeable2 (..),
    liftMrgIte,
    liftMrgIte2,
    liftUnion,
    mrgIf,
    mrgIte,
    mrgIte1,
    mrgIte2,
    simpleMerge,
  )
import Grisette.Internal.Unified.Class.UnifiedSymEq
  ( UnifiedSymEq (..),
    UnifiedSymEq1 (..),
    UnifiedSymEq2 (..),
    liftSymEq,
    liftSymEq2,
    symDistinct,
    symEq1,
    symEq2,
    (./=),
    (.==),
  )
import Grisette.Internal.Unified.Class.UnifiedSymOrd
  ( UnifiedSymOrd (..),
    UnifiedSymOrd1 (..),
    UnifiedSymOrd2 (..),
    mrgMax,
    mrgMin,
    symMax,
    symMin,
    (.<),
    (.<=),
    (.>),
    (.>=),
  )
import Grisette.Internal.Unified.EvalMode
  ( EvalModeAlgReal,
    EvalModeAll,
    EvalModeBV,
    EvalModeBase,
    EvalModeFP,
    EvalModeInteger,
    MonadEvalModeAll,
    genEvalMode,
  )
import Grisette.Internal.Unified.EvalModeTag
  ( EvalModeTag (..),
    IsConMode,
  )
import Grisette.Internal.Unified.FPFPConversion
  ( UnifiedFPFPConversion,
  )
import Grisette.Internal.Unified.Theories (TheoryToUnify (..))
import Grisette.Internal.Unified.UnifiedAlgReal
  ( GetAlgReal,
    UnifiedAlgReal,
  )
import Grisette.Internal.Unified.UnifiedBV
  ( GetIntN,
    GetSomeIntN,
    GetSomeWordN,
    GetWordN,
    SafeUnifiedBV,
    SafeUnifiedSomeBV,
    UnifiedBV,
  )
import Grisette.Internal.Unified.UnifiedBool (UnifiedBool (..))
import Grisette.Internal.Unified.UnifiedData
  ( BaseMonad,
    GetData,
    UnifiedData,
    extractData,
    liftSymCompare,
    liftSymCompare2,
    symCompare,
    symCompare1,
    symCompare2,
    wrapData,
  )
import Grisette.Internal.Unified.UnifiedFP
  ( GetFP,
    GetFPRoundingMode,
    SafeUnifiedFP,
    UnifiedFP,
  )
import Grisette.Internal.Unified.UnifiedFun
  ( GetFun,
    GetFun2,
    GetFun3,
    GetFun4,
    GetFun5,
    GetFun6,
    GetFun7,
    GetFun8,
    UnifiedFun,
    UnifiedFunConstraint,
    genUnifiedFunInstance,
    unifiedFunInstanceName,
  )
import Grisette.Internal.Unified.UnifiedInteger
  ( GetInteger,
    UnifiedInteger,
  )
import Grisette.Internal.Unified.UnifiedPrim
  ( UnifiedBasicPrim,
    UnifiedPrim,
  )
import Grisette.Internal.Unified.Util
  ( DecideEvalMode (..),
    EvalModeConvertible (..),
    withMode,
  )
