{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

-- |
-- Module      :   Grisette.Internal.Unified.UnifiedPrim
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Unified.UnifiedPrim
  ( UnifiedPrim,
    UnifiedBasicPrim,
  )
where

import Grisette.Internal.SymPrim.SymPrim (Prim)
import Grisette.Internal.Unified.BaseConstraint (ConSymConversion)
import Grisette.Internal.Unified.Class.UnifiedITEOp
  ( UnifiedITEOp,
  )
import Grisette.Internal.Unified.Class.UnifiedRep
  ( UnifiedConRep (ConType),
    UnifiedSymRep (SymType),
  )
import Grisette.Internal.Unified.Class.UnifiedSimpleMergeable
  ( UnifiedSimpleMergeable,
  )
import Grisette.Internal.Unified.Class.UnifiedSolvable (UnifiedSolvable)
import Grisette.Internal.Unified.Class.UnifiedSymEq (UnifiedSymEq)
import Grisette.Internal.Unified.Class.UnifiedSymOrd (UnifiedSymOrd)

-- | A type that is used as a constraint for all the (unified) primitive types
-- in Grisette.
type UnifiedPrim mode a =
  ( Prim a,
    UnifiedITEOp mode a,
    UnifiedSymEq mode a,
    UnifiedSymOrd mode a
  )

-- | A type that is used as a constraint for all the basic (unified) primitive
-- types in Grisette.
--
-- 'Grisette.Internal.Unified.GetSomeWordN' is not considered as a basic (unified)
-- primitive type.
type UnifiedBasicPrim mode a =
  ( UnifiedPrim mode a,
    UnifiedSimpleMergeable mode a,
    UnifiedConRep a,
    UnifiedSymRep a,
    UnifiedSolvable mode a (ConType a),
    ConSymConversion (ConType a) (SymType a) a
  )
