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
    UnifiedBranching (..),
    UnifiedITEOp (..),
    UnifiedSEq ((.==), (./=)),
    UnifiedSimpleMergeable (..),
    UnifiedSOrd (..),
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

import Grisette.Unified.Internal.Class.UnifiedBranching
  ( UnifiedBranching (..),
  )
import Grisette.Unified.Internal.Class.UnifiedITEOp (UnifiedITEOp (..))
import Grisette.Unified.Internal.Class.UnifiedSEq (UnifiedSEq (..))
import Grisette.Unified.Internal.Class.UnifiedSOrd
  ( UnifiedSOrd (..),
    mrgMax,
    mrgMin,
    symMax,
    symMin,
  )
import Grisette.Unified.Internal.Class.UnifiedSimpleMergeable
  ( UnifiedSimpleMergeable (..),
  )
import Grisette.Unified.Internal.EvaluationMode
  ( BaseMonad,
    EvaluationMode (..),
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
