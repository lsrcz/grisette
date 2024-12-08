{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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

import Grisette.Internal.Core.Data.Class.ToCon (ToCon)
import Grisette.Internal.Core.Data.Class.ToSym (ToSym)
import Grisette.Internal.SymPrim.SymPrim (Prim)
import Grisette.Unified.Internal.Class.UnifiedITEOp
  ( UnifiedITEOp,
  )
import Grisette.Unified.Internal.Class.UnifiedRep (UnifiedConRep (ConType), UnifiedSymRep (SymType))
import Grisette.Unified.Internal.Class.UnifiedSimpleMergeable
  ( UnifiedSimpleMergeable,
  )
import Grisette.Unified.Internal.Class.UnifiedSymEq (UnifiedSymEq)
import Grisette.Unified.Internal.Class.UnifiedSymOrd (UnifiedSymOrd)

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
-- 'GetSomeWordN' is not considered as a basic (unified) primitive type.
type BasicUnifiedPrim mode a =
  ( UnifiedPrim mode a,
    UnifiedSimpleMergeable mode a,
    UnifiedConRep a,
    UnifiedSymRep a,
    ToCon a (ConType a),
    ToSym a (SymType a),
    ToCon (SymType a) a,
    ToSym (ConType a) a
  )
