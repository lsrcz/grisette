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
    EvalMode,
    MonadWithMode,

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

    -- ** Unified ITE operator
    UnifiedITEOp (..),
    symIte,
    symIteMerge,

    -- ** Unified SymEq
    UnifiedSymEq (..),
    (.==),
    (./=),
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
  )
where

import Grisette.Unified.Internal.BaseMonad (BaseMonad)
import Grisette.Unified.Internal.Class.UnifiedITEOp
  ( UnifiedITEOp (..),
    symIte,
    symIteMerge,
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
import Grisette.Unified.Internal.Class.UnifiedSymEq
  ( UnifiedSymEq (..),
    liftSymEq,
    liftSymEq2,
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
import Grisette.Unified.Internal.EvalMode (EvalMode)
import Grisette.Unified.Internal.EvalModeTag
  ( EvalModeTag (..),
    IsConMode,
  )
import Grisette.Unified.Internal.MonadWithMode (MonadWithMode)
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
