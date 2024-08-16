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

    -- * Unified operations

    -- ** Unified simple mergeable
    UnifiedBranching (..),
    UnifiedSimpleMergeable (..),
    UnifiedSimpleMergeable1 (..),
    UnifiedSimpleMergeable2 (..),
    mrgIf,
    liftBaseMonad,
    mrgIte,
    mrgIte1,
    liftMrgIte,
    mrgIte2,
    liftMrgIte2,
    simpleMerge,

    -- ** Unified ITE operator
    UnifiedITEOp (..),
    symIte,
    symIteMerge,

    -- ** Unified SymEq
    UnifiedSymEq (..),
    (.==),
    (./=),
    symDistinct,
    liftSymEq,
    symEq1,
    liftSymEq2,
    symEq2,

    -- ** Unified SymOrd
    UnifiedSymOrd (..),
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

import Grisette.Unified.Internal.BVBVConversion
  ( UnifiedBVBVConversion,
  )
import Grisette.Unified.Internal.BVFPConversion
  ( SafeUnifiedBVFPConversion,
    UnifiedBVFPConversion,
  )
import Grisette.Unified.Internal.BaseMonad (BaseMonad)
import Grisette.Unified.Internal.Class.UnifiedFiniteBits
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
import Grisette.Unified.Internal.Class.UnifiedFromIntegral
  ( UnifiedFromIntegral (..),
    symFromIntegral,
  )
import Grisette.Unified.Internal.Class.UnifiedITEOp
  ( UnifiedITEOp (..),
    symIte,
    symIteMerge,
  )
import Grisette.Unified.Internal.Class.UnifiedSafeBitCast
  ( UnifiedSafeBitCast (..),
    safeBitCast,
  )
import Grisette.Unified.Internal.Class.UnifiedSafeDiv
  ( UnifiedSafeDiv (..),
    safeDiv,
    safeDivMod,
    safeMod,
    safeQuot,
    safeQuotRem,
    safeRem,
  )
import Grisette.Unified.Internal.Class.UnifiedSafeFdiv
  ( UnifiedSafeFdiv (..),
    safeFdiv,
  )
import Grisette.Unified.Internal.Class.UnifiedSafeFromFP
  ( UnifiedSafeFromFP (..),
    safeFromFP,
  )
import Grisette.Unified.Internal.Class.UnifiedSafeLinearArith
  ( UnifiedSafeLinearArith (..),
    safeAdd,
    safeNeg,
    safeSub,
  )
import Grisette.Unified.Internal.Class.UnifiedSafeSymRotate
  ( UnifiedSafeSymRotate (..),
    safeSymRotateL,
    safeSymRotateR,
  )
import Grisette.Unified.Internal.Class.UnifiedSafeSymShift
  ( UnifiedSafeSymShift (..),
    safeSymShiftL,
    safeSymShiftR,
    safeSymStrictShiftL,
    safeSymStrictShiftR,
  )
import Grisette.Unified.Internal.Class.UnifiedSimpleMergeable
  ( UnifiedBranching (..),
    UnifiedSimpleMergeable (..),
    UnifiedSimpleMergeable1 (..),
    UnifiedSimpleMergeable2 (..),
    liftBaseMonad,
    liftMrgIte,
    liftMrgIte2,
    mrgIf,
    mrgIte,
    mrgIte1,
    mrgIte2,
    simpleMerge,
  )
import Grisette.Unified.Internal.Class.UnifiedSymEq
  ( UnifiedSymEq (..),
    liftSymEq,
    liftSymEq2,
    symDistinct,
    symEq1,
    symEq2,
    (./=),
    (.==),
  )
import Grisette.Unified.Internal.Class.UnifiedSymOrd
  ( UnifiedSymOrd (..),
    liftSymCompare,
    liftSymCompare2,
    mrgMax,
    mrgMin,
    symCompare,
    symCompare1,
    symCompare2,
    symMax,
    symMin,
    (.<),
    (.<=),
    (.>),
    (.>=),
  )
import Grisette.Unified.Internal.EvalMode
  ( EvalModeAlgReal,
    EvalModeAll,
    EvalModeBV,
    EvalModeBase,
    EvalModeFP,
    EvalModeInteger,
    MonadEvalModeAll,
    genEvalMode,
  )
import Grisette.Unified.Internal.EvalModeTag
  ( EvalModeTag (..),
    IsConMode,
  )
import Grisette.Unified.Internal.FPFPConversion
  ( UnifiedFPFPConversion,
  )
import Grisette.Unified.Internal.Theories (TheoryToUnify (..))
import Grisette.Unified.Internal.UnifiedAlgReal
  ( GetAlgReal,
    UnifiedAlgReal,
  )
import Grisette.Unified.Internal.UnifiedBV
  ( GetIntN,
    GetSomeIntN,
    GetSomeWordN,
    GetWordN,
    SafeUnifiedBV,
    SafeUnifiedSomeBV,
    UnifiedBV,
  )
import Grisette.Unified.Internal.UnifiedBool (UnifiedBool (..))
import Grisette.Unified.Internal.UnifiedData
  ( GetData,
    UnifiedData,
    extractData,
    wrapData,
  )
import Grisette.Unified.Internal.UnifiedFP
  ( GetFP,
    GetFPRoundingMode,
    SafeUnifiedFP,
    UnifiedFP,
  )
import Grisette.Unified.Internal.UnifiedFun
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
import Grisette.Unified.Internal.UnifiedInteger
  ( GetInteger,
    UnifiedInteger,
  )
