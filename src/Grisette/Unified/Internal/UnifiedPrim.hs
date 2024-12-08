{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}

-- |
-- Module      :   Grisette.Unified.Internal.UnifiedPrim
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Unified.Internal.UnifiedPrim
  ( UnifiedPrim,
    BasicUnifiedPrim,
  )
where

import Grisette.Internal.SymPrim.SymPrim (Prim)
import Grisette.Unified.Internal.Class.UnifiedITEOp
  ( UnifiedITEOp,
  )
import Grisette.Unified.Internal.Class.UnifiedSimpleMergeable
  ( UnifiedSimpleMergeable,
  )

-- | A type that is used as a constraint for all the (unified) primitive types
-- in Grisette.
type UnifiedPrim mode a =
  ( Prim a,
    UnifiedITEOp mode a
  )

-- | A type that is used as a constraint for all the basic (unified) primitive
-- types in Grisette.
--
-- 'GetSomeWordN' is not considered as a basic (unified) primitive type.
type BasicUnifiedPrim mode a =
  ( UnifiedPrim mode a,
    UnifiedSimpleMergeable mode a
  )
