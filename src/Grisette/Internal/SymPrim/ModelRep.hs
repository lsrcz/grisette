{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.ModelRep
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.ModelRep (ModelSymPair (..)) where

import Grisette.Internal.Core.Data.Class.ModelOps
  ( ModelOps (emptyModel, insertValue),
    ModelRep (buildModel),
  )
import Grisette.Internal.SymPrim.Prim.Model (Model)
import Grisette.Internal.SymPrim.Prim.Term
  ( LinkedRep (underlyingTerm),
    Term (SymTerm),
  )

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
-- >>> import Grisette.Backend
-- >>> import Data.Proxy

-- ModelRep

-- | A pair of a symbolic constant and its value.
-- This is used to build a model from a list of symbolic constants and their values.
--
-- >>> buildModel ("a" := (1 :: Integer), "b" := True) :: Model
-- Model {a -> 1 :: Integer, b -> true :: Bool}
data ModelSymPair ct st where
  (:=) :: (LinkedRep ct st) => st -> ct -> ModelSymPair ct st

instance ModelRep (ModelSymPair ct st) Model where
  buildModel (sym := val) =
    case underlyingTerm sym of
      SymTerm _ _ _ _ symbol -> insertValue symbol val emptyModel
      _ -> error "buildModel: should only use symbolic constants"
