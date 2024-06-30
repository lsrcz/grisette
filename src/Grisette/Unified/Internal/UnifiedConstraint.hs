{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MonoLocalBinds #-}

-- |
-- Module      :   Grisette.Unified.Internal.UnifiedConstraint
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Unified.Internal.UnifiedConstraint (UnifiedPrimitive) where

import Grisette.Unified.Internal.Class.UnifiedITEOp (UnifiedITEOp)
import Grisette.Unified.Internal.Class.UnifiedSimpleMergeable
  ( UnifiedSimpleMergeable,
  )
import Grisette.Unified.Internal.Class.UnifiedSymEq (UnifiedSymEq)
import Grisette.Unified.Internal.Class.UnifiedSymOrd (UnifiedSymOrd)

-- | Basic constraints for a unified primitive type.
type UnifiedPrimitive mode t =
  ( UnifiedITEOp mode t,
    UnifiedSymEq mode t,
    UnifiedSymOrd mode t,
    UnifiedSimpleMergeable mode t
  )
