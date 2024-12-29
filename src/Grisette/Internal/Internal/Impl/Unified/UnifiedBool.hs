{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :   Grisette.Internal.Internal.Impl.Unified.UnifiedBool
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Impl.Unified.UnifiedBool () where

import Grisette.Internal.Internal.Decl.Unified.UnifiedBool
  ( UnifiedBool (GetBool),
  )
import Grisette.Internal.Internal.Impl.Core.Data.Class.SymOrd ()
import Grisette.Internal.SymPrim.SymBool (SymBool)
import Grisette.Internal.Unified.EvalModeTag (EvalModeTag (C, S))

instance UnifiedBool 'C where
  type GetBool 'C = Bool

instance UnifiedBool 'S where
  type GetBool 'S = SymBool
