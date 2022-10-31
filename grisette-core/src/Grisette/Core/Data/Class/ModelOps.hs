{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Grisette.Core.Data.Class.ModelOps
  ( ModelOps (..),
    SymbolSetOps (..),
    ModelRep (..),
  )
where

import Data.Hashable
import Data.Typeable

class Monoid symbolSet => SymbolSetOps symbolSet typedSymbol | symbolSet -> typedSymbol where
  emptySet :: symbolSet
  containsSymbol :: forall a. typedSymbol a -> symbolSet -> Bool
  insertSymbol :: forall a. typedSymbol a -> symbolSet -> symbolSet
  intersectionSet :: symbolSet -> symbolSet -> symbolSet
  unionSet :: symbolSet -> symbolSet -> symbolSet
  differenceSet :: symbolSet -> symbolSet -> symbolSet

class
  SymbolSetOps symbolSet typedSymbol =>
  ModelOps model symbolSet typedSymbol
    | model -> symbolSet typedSymbol
  where
  emptyModel :: model
  valueOf :: typedSymbol t -> model -> Maybe t
  insertValue :: typedSymbol t -> t -> model -> model
  exceptFor :: symbolSet -> model -> model
  restrictTo :: symbolSet -> model -> model
  extendTo :: symbolSet -> model -> model
  exact :: symbolSet -> model -> model
  exact s = restrictTo s . extendTo s

class ModelOps model symbolSet typedSymbol => ModelRep rep model symbolSet (typedSymbol :: * -> *) where
  buildModel :: rep -> model
