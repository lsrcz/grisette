{-# LANGUAGE DataKinds #-}

-- |
-- Module      :   Grisette.Internal.Internal.Impl.Unified.EvalMode
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Impl.Unified.EvalMode () where

import Grisette.Internal.Internal.Decl.Unified.EvalMode
  ( EvalModeAll,
    EvalModeBV,
    EvalModeBase,
    EvalModeFP,
  )
import Grisette.Internal.Internal.Impl.Unified.BVFPConversion ()
import Grisette.Internal.Internal.Impl.Unified.FPFPConversion ()
import Grisette.Internal.Internal.Impl.Unified.UnifiedBV ()
import Grisette.Internal.Internal.Impl.Unified.UnifiedFP ()
import Grisette.Internal.Unified.EvalModeTag (EvalModeTag (C, S))

instance EvalModeBase 'C

instance EvalModeBase 'S

instance EvalModeAll 'C

instance EvalModeAll 'S

instance EvalModeBV 'C

instance EvalModeBV 'S

instance EvalModeFP 'C

instance EvalModeFP 'S
