-- Disable this warning because we are re-exporting things.
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- |
-- Module      :   Grisette.Backend
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Backend
  ( -- * SMT backend configuration
    GrisetteSMTConfig (..),
    boolector,
    bitwuzla,
    cvc4,
    cvc5,
    yices,
    dReal,
    z3,
    mathSAT,
    abc,

    -- * Changing the extra configurations
    ApproximationConfig (..),
    ExtraConfig (..),
    precise,
    approximate,
    withTimeout,
    clearTimeout,

    -- * SBV backend solver configuration
    SBV.SMTConfig (..),
    SBV.Logic (..),
    SBVC.SMTOption (..),
    SBV.Timing (..),
    SBV.SMTSolver (..),
  )
where

import qualified Data.SBV as SBV
import qualified Data.SBV.Control as SBVC
import Grisette.Internal.Backend.Solving
  ( ApproximationConfig (..),
    ExtraConfig (..),
    GrisetteSMTConfig (..),
    abc,
    approximate,
    bitwuzla,
    boolector,
    clearTimeout,
    cvc4,
    cvc5,
    dReal,
    mathSAT,
    precise,
    withTimeout,
    yices,
    z3,
  )
