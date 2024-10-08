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
  )
where

import Control.DeepSeq (NFData)
import Data.Bytes.Serial (Serial)
import Data.Hashable (Hashable)
import Grisette.Internal.Core.Data.Class.EvalSym (EvalSym)
import Grisette.Internal.Core.Data.Class.ExtractSym (ExtractSym)
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.PPrint (PPrint)
import Grisette.Internal.Core.Data.Class.SubstSym (SubstSym)
import Grisette.Internal.Core.Data.Class.SymEq (SymEq)
import Grisette.Internal.Core.Data.Class.SymOrd (SymOrd)
import Grisette.Internal.Core.Data.Class.ToCon (ToCon)
import Grisette.Internal.Core.Data.Class.ToSym (ToSym)
import Grisette.Internal.SymPrim.AllSyms (AllSyms)
import Language.Haskell.TH.Syntax (Lift)

-- | A type that is used as a constraint for all the types in Grisette.
type BasicGrisetteType t =
  ( AllSyms t,
    Eq t,
    EvalSym t,
    ExtractSym t,
    PPrint t,
    Hashable t,
    Lift t,
    Mergeable t,
    NFData t,
    SymEq t,
    Show t,
    SymOrd t,
    Serial t,
    SubstSym t
  )

-- | A type that is used as a constraint for all the types in Grisette that can
-- be converted between concrete and symbolic types.
type ConSymConversion conType symType t =
  ( ToCon t conType,
    ToSym conType t,
    ToCon symType t,
    ToSym t symType
  )
