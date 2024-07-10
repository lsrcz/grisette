{-# LANGUAGE DataKinds #-}

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
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import GHC.Stack (HasCallStack)
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( IsSymbolKind,
    SomeTypedSymbol,
    SupportedPrim (castTypedSymbol),
    SymbolKind (NonFuncSymbol),
    TypedSymbol,
    castSomeTypedSymbol,
    someTypedSymbol,
  )

newtype QuantifiedSymbols = QuantifiedSymbols
  { _symbols :: S.HashSet (SomeTypedSymbol 'NonFuncSymbol)
  }
  deriving (Show)

emptyQuantifiedSymbols :: QuantifiedSymbols
emptyQuantifiedSymbols = QuantifiedSymbols S.empty

addQuantifiedSymbol ::
  TypedSymbol 'NonFuncSymbol a -> QuantifiedSymbols -> QuantifiedSymbols
addQuantifiedSymbol s (QuantifiedSymbols t) =
  QuantifiedSymbols (S.insert (someTypedSymbol s) t)

isQuantifiedSymbol ::
  (SupportedPrim a, IsSymbolKind knd) =>
  TypedSymbol knd a ->
  QuantifiedSymbols ->
  Bool
isQuantifiedSymbol s (QuantifiedSymbols t) =
  case castTypedSymbol s of
    Just s' -> S.member (someTypedSymbol s') t
    _ -> False

newtype QuantifiedStack = QuantifiedStack
  {_stack :: M.HashMap (SomeTypedSymbol 'NonFuncSymbol) Dynamic}

emptyQuantifiedStack :: QuantifiedStack
emptyQuantifiedStack = QuantifiedStack M.empty

addQuantified :: TypedSymbol 'NonFuncSymbol a -> Dynamic -> QuantifiedStack -> QuantifiedStack
addQuantified s d (QuantifiedStack t) =
  QuantifiedStack (M.insert (someTypedSymbol s) d t)

lookupQuantified ::
  (HasCallStack, IsSymbolKind knd) =>
  SomeTypedSymbol knd ->
  QuantifiedStack ->
  Maybe Dynamic
lookupQuantified s (QuantifiedStack t) =
  (`M.lookup` t) =<< castSomeTypedSymbol s
