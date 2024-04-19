{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :   Grisette.Backend.SBV.Data.SMT.SymBiMap
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Backend.SBV.Data.SMT.SymBiMap
  ( SymBiMap (..),
    emptySymBiMap,
    sizeBiMap,
    addBiMap,
    addBiMapIntermediate,
    findStringToSymbol,
    lookupTerm,
  )
where

import Data.Dynamic (Dynamic)
import qualified Data.HashMap.Strict as M
import GHC.Stack (HasCallStack)
import Grisette.SymPrim.Prim.SomeTerm
  ( SomeTerm,
  )
import Grisette.SymPrim.Prim.Term
  ( SomeTypedSymbol,
  )

data SymBiMap = SymBiMap
  { biMapToSBV :: M.HashMap SomeTerm Dynamic,
    biMapFromSBV :: M.HashMap String SomeTypedSymbol
  }
  deriving (Show)

emptySymBiMap :: SymBiMap
emptySymBiMap = SymBiMap M.empty M.empty

sizeBiMap :: SymBiMap -> Int
sizeBiMap = M.size . biMapToSBV

addBiMap :: (HasCallStack) => SomeTerm -> Dynamic -> String -> SomeTypedSymbol -> SymBiMap -> SymBiMap
addBiMap s d n sb (SymBiMap t f) = SymBiMap (M.insert s d t) (M.insert n sb f)

addBiMapIntermediate :: (HasCallStack) => SomeTerm -> Dynamic -> SymBiMap -> SymBiMap
addBiMapIntermediate s d (SymBiMap t f) = SymBiMap (M.insert s d t) f

findStringToSymbol :: String -> SymBiMap -> Maybe SomeTypedSymbol
findStringToSymbol s (SymBiMap _ f) = M.lookup s f

lookupTerm :: (HasCallStack) => SomeTerm -> SymBiMap -> Maybe Dynamic
lookupTerm t m = M.lookup t (biMapToSBV m)
