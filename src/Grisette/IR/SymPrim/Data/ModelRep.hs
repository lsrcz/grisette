{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.ModelRep
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.ModelRep (ModelSymPair (..)) where

import Grisette.Core.Data.Class.ModelOps
  ( ModelOps (emptyModel, insertValue),
    ModelRep (buildModel),
  )
import Grisette.IR.SymPrim.Data.Prim.Model (Model)
import Grisette.IR.SymPrim.Data.Prim.Term
  ( LinkedRep (underlyingTerm),
    Term (SymTerm),
  )

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> import Grisette.Backend.SBV
-- >>> import Data.Proxy

-- ModelRep

-- | A pair of a symbolic constant and its value.
-- This is used to build a model from a list of symbolic constants and their values.
--
-- >>> buildModel ("a" := (1 :: Integer), "b" := True) :: Model
-- Model {a -> 1 :: Integer, b -> True :: Bool}
data ModelSymPair ct st where
  (:=) :: (LinkedRep ct st) => st -> ct -> ModelSymPair ct st

instance ModelRep (ModelSymPair ct st) Model where
  buildModel (sym := val) =
    case underlyingTerm sym of
      SymTerm _ symbol -> insertValue symbol val emptyModel
      _ -> error "buildModel: should only use symbolic constants"
