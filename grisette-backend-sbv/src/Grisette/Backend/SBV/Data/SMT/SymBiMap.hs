{-# LANGUAGE ScopedTypeVariables #-}

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

import Data.Dynamic
import qualified Data.HashMap.Strict as M
import GHC.Stack
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.SomeTerm
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term

data SymBiMap = SymBiMap
  { biMapToSBV :: M.HashMap SomeTerm Dynamic,
    biMapFromSBV :: M.HashMap String TermSymbol
  }
  deriving (Show)

emptySymBiMap :: SymBiMap
emptySymBiMap = SymBiMap M.empty M.empty

sizeBiMap :: SymBiMap -> Int
sizeBiMap = M.size . biMapToSBV

addBiMap :: HasCallStack => SomeTerm -> Dynamic -> String -> TermSymbol -> SymBiMap -> SymBiMap
addBiMap s d n sb (SymBiMap t f) = SymBiMap (M.insert s d t) (M.insert n sb f)

addBiMapIntermediate :: HasCallStack => SomeTerm -> Dynamic -> SymBiMap -> SymBiMap
addBiMapIntermediate s d (SymBiMap t f) = SymBiMap (M.insert s d t) f

findStringToSymbol :: String -> SymBiMap -> Maybe TermSymbol
findStringToSymbol s (SymBiMap _ f) = M.lookup s f

lookupTerm :: HasCallStack => SomeTerm -> SymBiMap -> Maybe Dynamic
lookupTerm t m = M.lookup t (biMapToSBV m)
