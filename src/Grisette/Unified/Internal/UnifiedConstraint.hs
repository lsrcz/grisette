{-# LANGUAGE ConstraintKinds #-}

module Grisette.Unified.Internal.UnifiedConstraint (UnifiedPrimitive) where

import Grisette.Unified.Internal.Class.UnifiedITEOp (UnifiedITEOp)
import Grisette.Unified.Internal.Class.UnifiedSEq (UnifiedSEq)
import Grisette.Unified.Internal.Class.UnifiedSOrd (UnifiedSOrd)
import Grisette.Unified.Internal.Class.UnifiedSimpleMergeable
  ( UnifiedSimpleMergeable,
  )

type UnifiedPrimitive mode t =
  ( UnifiedITEOp mode t,
    UnifiedSEq mode t,
    UnifiedSOrd mode t,
    UnifiedSimpleMergeable mode t
  )
