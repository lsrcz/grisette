{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    QuantifiedSymbolInfo (..),
    attachNextQuantifiedSymbolInfo,
  )
where

import Control.DeepSeq (NFData)
import Data.Dynamic (Dynamic)
import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable)
import GHC.Stack (HasCallStack)
import Grisette.Internal.Backend.QuantifiedStack (QuantifiedStack)
import Grisette.Internal.Core.Data.Symbol
  ( Symbol (IndexedSymbol, SimpleSymbol),
    withInfo,
  )
import Grisette.Internal.SymPrim.Prim.SomeTerm
  ( SomeTerm,
  )
import Grisette.Internal.SymPrim.Prim.Term
  ( IsSymbolKind,
    SomeTypedSymbol,
    SymbolKind (AnySymbol, NonFuncSymbol),
    TypedSymbol (TypedSymbol),
    castSomeTypedSymbol,
  )
import Language.Haskell.TH.Syntax (Lift)

-- | A bidirectional map between symbolic Grisette terms and sbv terms.
data SymBiMap = SymBiMap
  { biMapToSBV :: M.HashMap SomeTerm (QuantifiedStack -> Dynamic),
    biMapFromSBV :: M.HashMap String (SomeTypedSymbol 'AnySymbol),
    quantifiedSymbolNum :: Int
  }

newtype QuantifiedSymbolInfo = QuantifiedSymbolInfo Int
  deriving (Ord, Eq, Show, Hashable, Lift, NFData)

nextQuantifiedSymbolInfo :: SymBiMap -> (SymBiMap, QuantifiedSymbolInfo)
nextQuantifiedSymbolInfo (SymBiMap t f num) =
  (SymBiMap t f (num + 1), QuantifiedSymbolInfo num)

attachQuantifiedSymbolInfo ::
  QuantifiedSymbolInfo -> TypedSymbol 'NonFuncSymbol a -> TypedSymbol 'NonFuncSymbol a
attachQuantifiedSymbolInfo
  info
  (TypedSymbol (SimpleSymbol ident)) =
    TypedSymbol $ SimpleSymbol $ withInfo ident info
attachQuantifiedSymbolInfo
  info
  (TypedSymbol (IndexedSymbol ident idx)) =
    TypedSymbol $ IndexedSymbol (withInfo ident info) idx

attachNextQuantifiedSymbolInfo ::
  SymBiMap -> TypedSymbol 'NonFuncSymbol a -> (SymBiMap, TypedSymbol 'NonFuncSymbol a)
attachNextQuantifiedSymbolInfo m s =
  let (m', info) = nextQuantifiedSymbolInfo m
   in (m', attachQuantifiedSymbolInfo info s)

-- | An empty bidirectional map.
emptySymBiMap :: SymBiMap
emptySymBiMap = SymBiMap M.empty M.empty 0

-- | The size of the bidirectional map.
sizeBiMap :: SymBiMap -> Int
sizeBiMap = M.size . biMapToSBV

-- | Add a new entry to the bidirectional map.
addBiMap ::
  (HasCallStack) =>
  SomeTerm ->
  Dynamic ->
  String ->
  SomeTypedSymbol knd ->
  SymBiMap ->
  SymBiMap
addBiMap s d n sb (SymBiMap t f num) =
  case castSomeTypedSymbol sb of
    Just sb' -> SymBiMap (M.insert s (const d) t) (M.insert n sb' f) num
    _ -> error "Casting to AnySymbol, should not fail"

-- | Add a new entry to the bidirectional map for intermediate values.
addBiMapIntermediate ::
  (HasCallStack) => SomeTerm -> (QuantifiedStack -> Dynamic) -> SymBiMap -> SymBiMap
addBiMapIntermediate s d (SymBiMap t f num) = SymBiMap (M.insert s d t) f num

-- | Find a symbolic Grisette term from a string.
findStringToSymbol :: (IsSymbolKind knd) => String -> SymBiMap -> Maybe (SomeTypedSymbol knd)
findStringToSymbol s (SymBiMap _ f _) = do
  r <- M.lookup s f
  castSomeTypedSymbol r

-- | Look up an sbv value with a symbolic Grisette term in the bidirectional
-- map.
lookupTerm :: (HasCallStack) => SomeTerm -> SymBiMap -> Maybe (QuantifiedStack -> Dynamic)
lookupTerm t m = M.lookup t (biMapToSBV m)
