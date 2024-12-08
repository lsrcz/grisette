{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :   Grisette.Unified.Internal.UnifiedBool
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Unified.Internal.UnifiedBool (UnifiedBool (..)) where

import Grisette.Internal.Core.Data.Class.LogicalOp (LogicalOp)
import Grisette.Internal.SymPrim.SymBool (SymBool)
import Grisette.Internal.SymPrim.SymPrim (Prim)
import Grisette.Unified.Internal.BaseConstraint
  ( ConSymConversion,
  )
import Grisette.Unified.Internal.Class.UnifiedRep
  ( UnifiedConRep (ConType),
    UnifiedSymRep (SymType),
  )
import Grisette.Unified.Internal.EvalModeTag (EvalModeTag (C, S))

-- | Evaluation mode with unified 'Bool' type.
class
  ( Prim (GetBool mode),
    UnifiedConRep (GetBool mode),
    UnifiedSymRep (GetBool mode),
    ConType (GetBool mode) ~ Bool,
    SymType (GetBool mode) ~ SymBool,
    ConSymConversion Bool SymBool (GetBool mode),
    LogicalOp (GetBool mode)
  ) =>
  UnifiedBool (mode :: EvalModeTag)
  where
  -- | Get a unified Boolean type. Resolves to 'Bool' in 'Con' mode, and
  -- 'SymBool' in 'Sym' mode.
  type GetBool mode = bool | bool -> mode

instance UnifiedBool 'C where
  type GetBool 'C = Bool

instance UnifiedBool 'S where
  type GetBool 'S = SymBool
