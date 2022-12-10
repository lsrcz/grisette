{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :   Grisette.Core.Data.Class.ModelOps
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only

module Grisette.Core.Data.Class.ModelOps
  ( -- * Note for the examples

    --

    -- | This module does not contain actual implementation for symbolic primitive types, and
    -- the examples in this module cannot be executed solely with @grisette-core@ package.
    -- They rely on the implementation in @grisette-symir@ package.

    -- * Model and symbolic set operations

    SymbolSetOps (..),
    SymbolSetRep (..),
    ModelOps (..),
    ModelRep (..),
  )
where

import Data.Hashable
import Data.Typeable
import Data.Kind

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim

-- | The operations on symbolic constant sets
--
-- Note that symbolic constants with different types are considered different.
--
-- >>> let aBool = "a" :: TypedSymbol Bool
-- >>> let bBool = "b" :: TypedSymbol Bool
-- >>> let cBool = "c" :: TypedSymbol Bool
-- >>> let aInteger = "a" :: TypedSymbol Integer
-- >>> emptySet :: SymbolSet
-- SymbolSet {}
-- >>> containsSymbol aBool (buildSymbolSet aBool :: SymbolSet)
-- True
-- >>> containsSymbol bBool (buildSymbolSet aBool :: SymbolSet)
-- False
-- >>> insertSymbol aBool (buildSymbolSet aBool :: SymbolSet)
-- SymbolSet {a :: Bool}
-- >>> insertSymbol aInteger (buildSymbolSet aBool :: SymbolSet)
-- SymbolSet {a :: Bool, a :: Integer}
-- >>> let abSet = buildSymbolSet (aBool, bBool) :: SymbolSet
-- >>> let acSet = buildSymbolSet (aBool, cBool) :: SymbolSet
-- >>> intersectionSet abSet acSet
-- SymbolSet {a :: Bool}
-- >>> unionSet abSet acSet
-- SymbolSet {a :: Bool, b :: Bool, c :: Bool}
-- >>> differenceSet abSet acSet
-- SymbolSet {b :: Bool}
class
  Monoid symbolSet =>
  SymbolSetOps symbolSet (typedSymbol :: Type -> Type)
    | symbolSet -> typedSymbol
  where
  -- | Construct an empty set
  emptySet :: symbolSet
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
-- >>> buildSymbolSet ("a" :: TypedSymbol Bool, "b" :: TypedSymbol Bool) :: SymbolSet
-- SymbolSet {a :: Bool, b :: Bool}
class
  SymbolSetOps symbolSet typedSymbol =>
  SymbolSetRep rep symbolSet (typedSymbol :: * -> *)
  where
  -- | Build a symbolic constant set
  buildSymbolSet :: rep -> symbolSet


-- | The operations on Models.
--
-- Note that symbolic constants with different types are considered different.
--
-- >>> let aBool = "a" :: TypedSymbol Bool
-- >>> let bBool = "b" :: TypedSymbol Bool
-- >>> let cBool = "c" :: TypedSymbol Bool
-- >>> let aInteger = "a" :: TypedSymbol Integer
-- >>> emptyModel :: Model
-- Model {}
-- >>> valueOf aBool (buildModel (aBool ::= True) :: Model)
-- Just True
-- >>> valueOf bBool (buildModel (aBool ::= True) :: Model)
-- Nothing
-- >>> insertValue bBool False (buildModel (aBool ::= True) :: Model)
-- Model {a -> True :: Bool, b -> False :: Bool}
-- >>> let abModel = buildModel (aBool ::= True, bBool ::= False) :: Model
-- >>> let acSet = buildSymbolSet (aBool, cBool) :: SymbolSet
-- >>> exceptFor acSet abModel
-- Model {b -> False :: Bool}
-- >>> restrictTo acSet abModel
-- Model {a -> True :: Bool}
-- >>> extendTo acSet abModel
-- Model {a -> True :: Bool, b -> False :: Bool, c -> False :: Bool}
-- >>> exact acSet abModel
-- Model {a -> True :: Bool, c -> False :: Bool}
class
  SymbolSetOps symbolSet typedSymbol =>
  ModelOps model symbolSet typedSymbol
    | model -> symbolSet typedSymbol
  where
  -- | Construct an empty model
  emptyModel :: model
  -- | Extract the assigned value for a given symbolic constant
  valueOf :: typedSymbol t -> model -> Maybe t
  -- | Insert an assignment into the model
  insertValue :: typedSymbol t -> t -> model -> model
  -- | Returns a model that removed all the assignments for the symbolic
  -- constants in the set
  exceptFor :: symbolSet -> model -> model
  -- | Returns a model that only keeps the assignments for the symbolic
  -- constants in the set
  restrictTo :: symbolSet -> model -> model
  -- | Returns a model that extends the assignments for the symbolic constants
  -- in the set by assigning default values to them
  extendTo :: symbolSet -> model -> model
  -- | Returns a model that containes the assignments for exactly the symbolic
  -- contants in the set by removing assignments for the symbolic constants that
  -- are not in the set and add assignments for the missing symbolic constants
  -- by assigning default values to them.
  exact :: symbolSet -> model -> model
  exact s = restrictTo s . extendTo s

-- | A type class for building a model manually from a model representation
class ModelOps model symbolSet typedSymbol => ModelRep rep model symbolSet (typedSymbol :: * -> *) where
  -- | Build a model
  -- >>> let aBool = "a" :: TypedSymbol Bool
  -- >>> let bBool = "b" :: TypedSymbol Bool
  -- >>> buildModel (aBool ::= True, bBool ::= False) :: Model
  -- Model {a -> True :: Bool, b -> False :: Bool}
  buildModel :: rep -> model
