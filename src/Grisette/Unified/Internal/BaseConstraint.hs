{-# LANGUAGE ConstraintKinds #-}

-- |
-- Module      :   Grisette.Unified.Internal.EvaluationMode
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Unified.Internal.BaseConstraint
  ( BasicGrisetteType,
    ConSymConversion,
    SafeIntegral,
  )
where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Grisette
  ( AllSyms,
    EvaluateSym,
    ExtractSymbolics,
    GPretty,
    Mergeable,
    SEq,
    SOrd,
    SafeDivision,
    SafeLinearArith,
    SubstituteSym,
    ToCon,
    ToSym,
  )
import Language.Haskell.TH.Syntax (Lift)

-- | A type that is used as a constraint for all the types in Grisette.
type BasicGrisetteType t =
  ( AllSyms t,
    Eq t,
    EvaluateSym t,
    ExtractSymbolics t,
    GPretty t,
    Hashable t,
    Lift t,
    Mergeable t,
    NFData t,
    SEq t,
    Show t,
    SOrd t,
    SubstituteSym t
  )

-- | A type that is used as a constraint for all the types in Grisette that can
-- be converted between concrete and symbolic types.
type ConSymConversion conType symType t =
  ( ToCon t conType,
    ToSym conType t,
    ToCon symType t,
    ToSym t symType
  )

-- | A type that is used as a constraint for all the types in Grisette that can
-- be used in safe integral operations.
type SafeIntegral e v m = (SafeDivision e v m, SafeLinearArith e v m)
