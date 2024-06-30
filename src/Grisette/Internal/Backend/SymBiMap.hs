{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :   Grisette.Internal.Backend.SymBiMap
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Backend.SymBiMap
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
import Grisette.Internal.SymPrim.Prim.SomeTerm
  ( SomeTerm,
  )
import Grisette.Internal.SymPrim.Prim.Term
  ( SomeTypedSymbol,
  )

-- | A bidirectional map between symbolic Grisette terms and sbv terms.
data SymBiMap = SymBiMap
  { biMapToSBV :: M.HashMap SomeTerm Dynamic,
    biMapFromSBV :: M.HashMap String SomeTypedSymbol
  }
  deriving (Show)

-- | An empty bidirectional map.
emptySymBiMap :: SymBiMap
emptySymBiMap = SymBiMap M.empty M.empty

-- | The size of the bidirectional map.
sizeBiMap :: SymBiMap -> Int
sizeBiMap = M.size . biMapToSBV

-- | Add a new entry to the bidirectional map.
addBiMap :: (HasCallStack) => SomeTerm -> Dynamic -> String -> SomeTypedSymbol -> SymBiMap -> SymBiMap
addBiMap s d n sb (SymBiMap t f) = SymBiMap (M.insert s d t) (M.insert n sb f)

-- | Add a new entry to the bidirectional map for intermediate values.
addBiMapIntermediate :: (HasCallStack) => SomeTerm -> Dynamic -> SymBiMap -> SymBiMap
addBiMapIntermediate s d (SymBiMap t f) = SymBiMap (M.insert s d t) f

-- | Find a symbolic Grisette term from a string.
findStringToSymbol :: String -> SymBiMap -> Maybe SomeTypedSymbol
findStringToSymbol s (SymBiMap _ f) = M.lookup s f

-- | Look up an sbv value with a symbolic Grisette term in the bidirectional
-- map.
lookupTerm :: (HasCallStack) => SomeTerm -> SymBiMap -> Maybe Dynamic
lookupTerm t m = M.lookup t (biMapToSBV m)
