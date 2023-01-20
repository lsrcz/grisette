-- |
-- Module      :   Grisette.Backend.SBV
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Backend.SBV
  ( -- * Grisette SBV backend configuration
    GrisetteSMTConfig (..),
    sbvConfig,

    -- * SBV backend solver configuration
    SBV.SMTConfig (..),
    SBV.boolector,
    SBV.cvc4,
    SBV.yices,
    SBV.dReal,
    SBV.z3,
    SBV.mathSAT,
    SBV.abc,
    SBV.Timing (..),
  )
where

import qualified Data.SBV as SBV
import Grisette.Backend.SBV.Data.SMT.Solving