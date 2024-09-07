{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.ModelOps
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.ModelOps
  ( -- * Model and symbolic set operations
    SymbolSetOps (..),
    SymbolSetRep (..),
    ModelOps (..),
    ModelRep (..),
  )
where

import Data.Kind (Type)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim

-- | The operations on symbolic constant sets
--
-- Note that symbolic constants with different types are considered different.
--
-- >>> let aBool = "a" :: TypedAnySymbol Bool
-- >>> let bBool = "b" :: TypedAnySymbol Bool
-- >>> let cBool = "c" :: TypedAnySymbol Bool
-- >>> let aInteger = "a" :: TypedAnySymbol Integer
-- >>> emptySet :: AnySymbolSet
-- SymbolSet {}
-- >>> containsSymbol aBool (buildSymbolSet aBool :: AnySymbolSet)
-- True
-- >>> containsSymbol bBool (buildSymbolSet aBool :: AnySymbolSet)
-- False
-- >>> insertSymbol aBool (buildSymbolSet aBool :: AnySymbolSet)
-- SymbolSet {a :: Bool}
-- >>> insertSymbol aInteger (buildSymbolSet aBool :: AnySymbolSet)
-- SymbolSet {a :: Bool, a :: Integer}
-- >>> let abSet = buildSymbolSet (aBool, bBool) :: AnySymbolSet
-- >>> let acSet = buildSymbolSet (aBool, cBool) :: AnySymbolSet
-- >>> intersectionSet abSet acSet
-- SymbolSet {a :: Bool}
-- >>> unionSet abSet acSet
-- SymbolSet {a :: Bool, b :: Bool, c :: Bool}
-- >>> differenceSet abSet acSet
-- SymbolSet {b :: Bool}
class
  (Monoid symbolSet) =>
  SymbolSetOps symbolSet (typedSymbol :: Type -> Type)
    | symbolSet -> typedSymbol
  where
  -- | Construct an empty set
  emptySet :: symbolSet

  -- | Check if the set is empty
  isEmptySet :: symbolSet -> Bool

  -- | Check if the set contains the given symbol
  containsSymbol :: forall a. typedSymbol a -> symbolSet -> Bool

  -- | Insert a symbol into the set
  insertSymbol :: forall a. typedSymbol a -> symbolSet -> symbolSet

  -- | Set intersection
  intersectionSet :: symbolSet -> symbolSet -> symbolSet

  -- | Set union
  unionSet :: symbolSet -> symbolSet -> symbolSet

  -- | Set difference
  differenceSet :: symbolSet -> symbolSet -> symbolSet

-- | A type class for building a symbolic constant set manually from a symbolic
-- constant set representation
--
-- >>> buildSymbolSet ("a" :: TypedAnySymbol Bool, "b" :: TypedAnySymbol Bool) :: AnySymbolSet
-- SymbolSet {a :: Bool, b :: Bool}
class
  (SymbolSetOps symbolSet typedSymbol) =>
  SymbolSetRep rep symbolSet (typedSymbol :: Type -> Type)
  where
  -- | Build a symbolic constant set
  buildSymbolSet :: rep -> symbolSet

-- | The operations on Models.
--
-- Note that symbolic constants with different types are considered different.
--
-- >>> let aBool = "a" :: TypedAnySymbol Bool
-- >>> let bBool = "b" :: TypedAnySymbol Bool
-- >>> let cBool = "c" :: TypedAnySymbol Bool
-- >>> let aInteger = "a" :: TypedAnySymbol Integer
-- >>> emptyModel :: Model
-- Model {}
-- >>> valueOf aBool (buildModel (aBool ::= True) :: Model)
-- Just True
-- >>> valueOf bBool (buildModel (aBool ::= True) :: Model)
-- Nothing
-- >>> insertValue bBool False (buildModel (aBool ::= True) :: Model)
-- Model {a -> true :: Bool, b -> false :: Bool}
-- >>> let abModel = buildModel (aBool ::= True, bBool ::= False) :: Model
-- >>> let acSet = buildSymbolSet (aBool, cBool) :: AnySymbolSet
-- >>> exceptFor acSet abModel
-- Model {b -> false :: Bool}
-- >>> restrictTo acSet abModel
-- Model {a -> true :: Bool}
-- >>> extendTo acSet abModel
-- Model {a -> true :: Bool, b -> false :: Bool, c -> false :: Bool}
-- >>> exact acSet abModel
-- Model {a -> true :: Bool, c -> false :: Bool}
class
  (SymbolSetOps symbolSet typedSymbol) =>
  ModelOps model symbolSet typedSymbol
    | model -> symbolSet typedSymbol
  where
  -- | Construct an empty model
  emptyModel :: model

  -- | Check if the model is empty
  isEmptyModel :: model -> Bool

  -- | Check if the model contains the given symbol
  modelContains :: typedSymbol a -> model -> Bool

  -- | Extract the assigned value for a given symbolic constant
  valueOf :: typedSymbol t -> model -> Maybe t

  -- | Insert an assignment into the model
  insertValue :: typedSymbol t -> t -> model -> model

  -- | Returns a model that removed all the assignments for the symbolic
  -- constants in the set
  exceptFor :: symbolSet -> model -> model

  -- | Returns a model that removed the assignments for the symbolic constants
  exceptFor' :: typedSymbol t -> model -> model

  -- | Returns a model that only keeps the assignments for the symbolic
  -- constants in the set
  restrictTo :: symbolSet -> model -> model

  -- | Returns a model that extends the assignments for the symbolic constants
  -- in the set by assigning default values to them
  extendTo :: symbolSet -> model -> model

  -- | Returns a model that contains the assignments for exactly the symbolic
  -- constants in the set by removing assignments for the symbolic constants that
  -- are not in the set and add assignments for the missing symbolic constants
  -- by assigning default values to them.
  exact :: symbolSet -> model -> model
  exact s = restrictTo s . extendTo s

-- | A type class for building a model manually from a model representation
class ModelRep rep model | rep -> model where
  -- | Build a model
  --
  -- >>> let aBool = "a" :: TypedAnySymbol Bool
  -- >>> let bBool = "b" :: TypedAnySymbol Bool
  -- >>> buildModel (aBool ::= True, bBool ::= False) :: Model
  -- Model {a -> true :: Bool, b -> false :: Bool}
  buildModel :: rep -> model
