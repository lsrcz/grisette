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
    UnifiedBranching,
    mrgIf,
    liftBaseMonad,

    -- ** Unified ITE operator
    UnifiedITEOp (..),
    symIte,
    symIteMerge,

    -- ** Unified SEq
    UnifiedSEq,
    (.==),
    (./=),
    liftSEq,
    seq1,
    liftSEq2,
    seq2,
    UnifiedSimpleMergeable (..),

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

    -- ** Integer
    GetInteger,

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
import Grisette.Unified.Internal.Class.UnifiedBranching
  ( UnifiedBranching (..),
    liftBaseMonad,
    mrgIf,
  )
import Grisette.Unified.Internal.Class.UnifiedITEOp (UnifiedITEOp (..), symIte, symIteMerge)
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
import Grisette.Unified.Internal.Class.UnifiedSimpleMergeable
  ( UnifiedSimpleMergeable (..),
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
  ( UnifiedInteger (..),
  )
