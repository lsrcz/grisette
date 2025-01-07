{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

-- |
-- Module      :   Grisette.Internal.Backend.SymBiMap
-- Copyright   :   (c) Sirui Lu 2021-2024
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
    attachNextQuantifiedSymbolInfo,
  )
where

import Data.Dynamic (Dynamic)
import qualified Data.HashMap.Strict as M
import GHC.Stack (HasCallStack)
import Grisette.Internal.Backend.QuantifiedStack (QuantifiedStack)
import Grisette.Internal.Core.Data.SExpr (SExpr (Atom, List, NumberAtom))
import Grisette.Internal.Core.Data.Symbol
  ( mapIdentifier,
    mapMetadata,
  )
import Grisette.Internal.SymPrim.Prim.SomeTerm
  ( SomeTerm,
  )
import Grisette.Internal.SymPrim.Prim.Term
  ( IsSymbolKind,
    SomeTypedAnySymbol,
    SomeTypedSymbol,
    TypedConstantSymbol,
    TypedSymbol (unTypedSymbol),
    castSomeTypedSymbol,
    typedConstantSymbol,
    pattern SupportedConstantTypedSymbol,
  )

-- | A bidirectional map between symbolic Grisette terms and sbv terms.
data SymBiMap = SymBiMap
  { biMapToSBV :: M.HashMap SomeTerm (QuantifiedStack -> Dynamic),
    biMapSize :: Int,
    biMapFromSBV :: M.HashMap String SomeTypedAnySymbol,
    quantifiedSymbolNum :: Int
  }

instance Show SymBiMap where
  show (SymBiMap t s f _) =
    "SymBiMap { size: "
      ++ show s
      ++ ", toSBV: "
      ++ show (M.keys t)
      ++ ", fromSBV: "
      ++ show (M.toList f)
      ++ " }"

-- | Information about a quantified symbol.
-- newtype QuantifiedSymbolInfo = QuantifiedSymbolInfo Int
--   deriving (Generic, Ord, Eq, Show, Hashable, Lift, NFData)
nextQuantifiedSymbolInfo :: SymBiMap -> (SymBiMap, SExpr -> SExpr)
nextQuantifiedSymbolInfo (SymBiMap t s f num) =
  ( SymBiMap t s f (num + 1),
    \meta ->
      List
        [ Atom "grisette-quantified",
          NumberAtom $ fromIntegral num,
          meta
        ]
  )

attachQuantifiedSymbolInfo ::
  (SExpr -> SExpr) -> TypedConstantSymbol a -> TypedConstantSymbol a
attachQuantifiedSymbolInfo info tsym@SupportedConstantTypedSymbol =
  typedConstantSymbol $
    mapIdentifier (mapMetadata info) $
      unTypedSymbol tsym
attachQuantifiedSymbolInfo _ _ = error "Should not happen"

-- | Attach the next quantified symbol info to a symbol.
attachNextQuantifiedSymbolInfo ::
  SymBiMap -> TypedConstantSymbol a -> (SymBiMap, TypedConstantSymbol a)
attachNextQuantifiedSymbolInfo m s =
  let (m', info) = nextQuantifiedSymbolInfo m
   in (m', attachQuantifiedSymbolInfo info s)

-- | An empty bidirectional map.
emptySymBiMap :: SymBiMap
emptySymBiMap = SymBiMap M.empty 0 M.empty 0

-- | The size of the bidirectional map.
sizeBiMap :: SymBiMap -> Int
sizeBiMap = biMapSize

-- | Add a new entry to the bidirectional map.
addBiMap ::
  (HasCallStack) =>
  SomeTerm ->
  Dynamic ->
  String ->
  SomeTypedSymbol knd ->
  SymBiMap ->
  SymBiMap
addBiMap s d n sb (SymBiMap t sz f num) =
  case castSomeTypedSymbol sb of
    Just sb' -> SymBiMap (M.insert s (const d) t) (sz + 1) (M.insert n sb' f) num
    _ -> error "Casting to AnySymbol, should not fail"

-- | Add a new entry to the bidirectional map for intermediate values.
addBiMapIntermediate ::
  (HasCallStack) => SomeTerm -> (QuantifiedStack -> Dynamic) -> SymBiMap -> SymBiMap
addBiMapIntermediate s d (SymBiMap t sz f num) =
  SymBiMap (M.insert s d t) (sz + 1) f num

-- | Find a symbolic Grisette term from a string.
findStringToSymbol :: (IsSymbolKind knd) => String -> SymBiMap -> Maybe (SomeTypedSymbol knd)
findStringToSymbol s (SymBiMap _ _ f _) = do
  r <- M.lookup s f
  castSomeTypedSymbol r

-- | Look up an sbv value with a symbolic Grisette term in the bidirectional
-- map.
lookupTerm :: (HasCallStack) => SomeTerm -> SymBiMap -> Maybe (QuantifiedStack -> Dynamic)
lookupTerm t m = M.lookup t (biMapToSBV m)
