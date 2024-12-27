{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :   Grisette.Internal.Internal.Decl.Unified.UnifiedBool
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Decl.Unified.UnifiedBool
  ( UnifiedBool (..),
  )
where

import Grisette.Internal.Core.Data.Class.LogicalOp (LogicalOp)
import Grisette.Internal.SymPrim.SymBool (SymBool)
import Grisette.Internal.SymPrim.SymPrim (Prim)
import Grisette.Internal.Unified.BaseConstraint
  ( ConSymConversion,
  )
import Grisette.Internal.Unified.Class.UnifiedRep
  ( UnifiedConRep (ConType),
    UnifiedSymRep (SymType),
  )
import Grisette.Internal.Unified.EvalModeTag (EvalModeTag)

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
  -- | Get a unified Boolean type. Resolves to 'Bool' in 'C' mode, and
  -- 'SymBool' in 'S' mode.
  type GetBool mode = bool | bool -> mode
