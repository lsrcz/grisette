{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Grisette.Unified
  ( -- * Evaluation mode
    EvaluationMode (..),
    IsConMode,
    BaseMonad,

    -- * Aggregated constraints
    IsMode,
    MonadWithMode,

    -- * Unified type classes

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

    -- ** Unified ITE operator
    UnifiedITEOp (..),
    symIte,
    symIteMerge,

    -- ** Unified SEq
    UnifiedSEq (..),
    (.==),
    (./=),
    liftSEq,
    seq1,
    liftSEq2,
    seq2,

    -- ** Unified SOrd
    UnifiedSOrd (..),
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

    -- ** Unified safe ops
    UnifiedSafeDivision (..),
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

    -- ** Data
    GetData,
    UnifiedData,
    extractData,
    wrapData,

    -- ** TH
    mkUnifiedConstructor,
    mkUnifiedConstructor',
  )
where

import Grisette.Unified.Internal.BaseMonad (BaseMonad)
import Grisette.Unified.Internal.Class.UnifiedITEOp
  ( UnifiedITEOp (..),
    symIte,
    symIteMerge,
  )
import Grisette.Unified.Internal.Class.UnifiedSEq
  ( UnifiedSEq (..),
    liftSEq,
    liftSEq2,
    seq1,
    seq2,
    (./=),
    (.==),
  )
import Grisette.Unified.Internal.Class.UnifiedSOrd
  ( UnifiedSOrd (..),
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
import Grisette.Unified.Internal.Class.UnifiedSafeDivision
  ( UnifiedSafeDivision (..),
    safeDiv,
    safeDivMod,
    safeMod,
    safeQuot,
    safeQuotRem,
    safeRem,
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
  )
import Grisette.Unified.Internal.EvaluationMode
  ( EvaluationMode (..),
    IsConMode,
  )
import Grisette.Unified.Internal.IsMode (IsMode)
import Grisette.Unified.Internal.MonadWithMode (MonadWithMode)
import Grisette.Unified.Internal.TH.UnifiedConstructor
  ( mkUnifiedConstructor,
    mkUnifiedConstructor',
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
import Grisette.Unified.Internal.UnifiedInteger
  ( GetInteger,
    UnifiedInteger,
  )
