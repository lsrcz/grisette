{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MonoLocalBinds #-}

module Grisette.Unified.Internal.UnifiedConstraint (UnifiedPrimitive) where

import Grisette.Unified.Internal.Class.UnifiedITEOp (UnifiedITEOp)
import Grisette.Unified.Internal.Class.UnifiedSimpleMergeable
  ( UnifiedSimpleMergeable,
  )
import Grisette.Unified.Internal.Class.UnifiedSymEq (UnifiedSymEq)
import Grisette.Unified.Internal.Class.UnifiedSymOrd (UnifiedSymOrd)

type UnifiedPrimitive mode t =
  ( UnifiedITEOp mode t,
    UnifiedSymEq mode t,
    UnifiedSymOrd mode t,
    UnifiedSimpleMergeable mode t
  )
