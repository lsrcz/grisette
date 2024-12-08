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
  ( ConSymConversion,
  )
where

import Grisette.Internal.Core.Data.Class.ToCon (ToCon)
import Grisette.Internal.Core.Data.Class.ToSym (ToSym)

-- | A type that is used as a constraint for all the types in Grisette that can
-- be converted between concrete and symbolic types.
type ConSymConversion conType symType t =
  ( ToCon t conType,
    ToSym conType t,
    ToCon symType t,
    ToSym t symType
  )
