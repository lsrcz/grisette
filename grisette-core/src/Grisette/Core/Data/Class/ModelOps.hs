{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Grisette.Core.Data.Class.ModelOps
  ( ModelOps (..),
    SymbolSetOps (..),
  )
where

import Data.Hashable
import Data.Typeable

class Monoid symbolSet => SymbolSetOps symbolSet symbol | symbolSet -> symbol where
  emptySet :: symbolSet
  containsSymbol :: symbol -> symbolSet -> Bool
  insertSymbol :: symbol -> symbolSet -> symbolSet
  intersectionSet :: symbolSet -> symbolSet -> symbolSet
  unionSet :: symbolSet -> symbolSet -> symbolSet
  differenceSet :: symbolSet -> symbolSet -> symbolSet

class SymbolSetOps symbolSet symbol => ModelOps model symbolSet symbol | model -> symbolSet symbol where
  emptyModel :: model
  valueOf :: forall t. (Typeable t) => symbol -> model -> Maybe t
  insertValue :: (Eq a, Show a, Hashable a, Typeable a) => symbol -> a -> model -> model
  exceptFor :: symbolSet -> model -> model
  restrictTo :: symbolSet -> model -> model
  extendTo :: symbolSet -> model -> model
  exact :: symbolSet -> model -> model
  exact s = restrictTo s . extendTo s
