{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

-- |
-- Module      :   Grisette.Internal.Backend.QuantifiedStack
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Backend.QuantifiedStack
  ( QuantifiedSymbols (..),
    QuantifiedStack,
    addQuantified,
    lookupQuantified,
    emptyQuantifiedSymbols,
    addQuantifiedSymbol,
    isQuantifiedSymbol,
    emptyQuantifiedStack,
  )
where

import Data.Dynamic (Dynamic)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.Hashable (Hashable (hashWithSalt))
import GHC.Stack (HasCallStack)
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( IsSymbolKind,
    SomeTypedConstantSymbol,
    SomeTypedSymbol,
    SupportedPrim (castTypedSymbol),
    TypedConstantSymbol,
    TypedSymbol,
    castSomeTypedSymbol,
    someTypedSymbol,
  )

-- | A set of quantified symbols.
newtype QuantifiedSymbols = QuantifiedSymbols
  { _symbols :: S.HashSet SomeTypedConstantSymbol
  }
  deriving (Show)

-- | An empty set of quantified symbols.
emptyQuantifiedSymbols :: QuantifiedSymbols
emptyQuantifiedSymbols = QuantifiedSymbols S.empty

-- | Add a quantified symbol to the set.
addQuantifiedSymbol ::
  TypedConstantSymbol a -> QuantifiedSymbols -> QuantifiedSymbols
addQuantifiedSymbol s (QuantifiedSymbols t) =
  QuantifiedSymbols (S.insert (someTypedSymbol s) t)

-- | Check if a symbol is quantified.
isQuantifiedSymbol ::
  (SupportedPrim a, IsSymbolKind knd) =>
  TypedSymbol knd a ->
  QuantifiedSymbols ->
  Bool
isQuantifiedSymbol s (QuantifiedSymbols t) =
  case castTypedSymbol s of
    Just s' -> S.member (someTypedSymbol s') t
    _ -> False

-- | A stack of quantified symbols.
newtype QuantifiedStack = QuantifiedStack
  {_stack :: M.HashMap SomeTypedConstantSymbol Dynamic}

instance Eq QuantifiedStack where
  QuantifiedStack s1 == QuantifiedStack s2 = M.keysSet s1 == M.keysSet s2

instance Hashable QuantifiedStack where
  hashWithSalt s (QuantifiedStack t) = hashWithSalt s (M.keys t)

-- | An empty stack of quantified symbols.
emptyQuantifiedStack :: QuantifiedStack
emptyQuantifiedStack = QuantifiedStack M.empty

-- | Add a quantified symbol to the stack.
addQuantified ::
  TypedConstantSymbol a -> Dynamic -> QuantifiedStack -> QuantifiedStack
addQuantified s d (QuantifiedStack t) =
  QuantifiedStack (M.insert (someTypedSymbol s) d t)

-- | Look up a quantified symbol in the stack.
lookupQuantified ::
  (HasCallStack, IsSymbolKind knd) =>
  SomeTypedSymbol knd ->
  QuantifiedStack ->
  Maybe Dynamic
lookupQuantified s (QuantifiedStack t) =
  (`M.lookup` t) =<< castSomeTypedSymbol s
