{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilyDependencies #-}

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
import Grisette.Unified.Internal.BaseConstraint
  ( BasicGrisetteType,
    ConSymConversion,
  )
import Grisette.Unified.Internal.EvalModeTag (EvalModeTag (Con, Sym))

class
  ( BasicGrisetteType (GetBool mode),
    ConSymConversion Bool SymBool (GetBool mode),
    LogicalOp (GetBool mode)
  ) =>
  UnifiedBool (mode :: EvalModeTag)
  where
  -- | Get a unified Boolean type. Resolves to 'Bool' in 'Con' mode, and
  -- 'SymBool' in 'Sym' mode.
  type GetBool mode = bool | bool -> mode

instance UnifiedBool 'Con where
  type GetBool 'Con = Bool

instance UnifiedBool 'Sym where
  type GetBool 'Sym = SymBool
