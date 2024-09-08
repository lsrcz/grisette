{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Prim.Model
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.Prim.Model
  ( SymbolSet (..),
    ConstantSymbolSet,
    AnySymbolSet,
    Model (..),
    ModelValuePair (..),
    equation,
    evalTerm,
  )
where

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.Hashable (Hashable)
import Data.List (sort, sortOn)
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import Grisette.Internal.Core.Data.Class.ModelOps
  ( ModelOps
      ( emptyModel,
        exceptFor,
        exceptFor',
        extendTo,
        insertValue,
        isEmptyModel,
        modelContains,
        restrictTo,
        valueOf
      ),
    ModelRep (buildModel),
    SymbolSetOps
      ( containsSymbol,
        differenceSet,
        emptySet,
        insertSymbol,
        intersectionSet,
        isEmptySet,
        unionSet
      ),
    SymbolSetRep (buildSymbolSet),
  )
import Grisette.Internal.SymPrim.GeneralFun (generalSubstSomeTerm)
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( SomeTypedAnySymbol,
    SomeTypedConstantSymbol,
    SupportedPrim,
    SymbolKind (AnyKind, ConstantKind),
    Term,
    TypedAnySymbol,
  )
import Grisette.Internal.SymPrim.Prim.Term
  ( ModelValue,
    SomeTypedSymbol (SomeTypedSymbol),
    SupportedPrim (defaultValue),
    TypedSymbol,
    conTerm,
    defaultValueDynamic,
    pevalEqTerm,
    showUntyped,
    someTypedSymbol,
    symTerm,
    toModelValue,
    unsafeFromModelValue,
    withSymbolSupported,
  )

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim

-- | Set of symbols.
--
-- Check 'Grisette.Core.SymbolSetOps' for operations, and
-- 'Grisette.Core.SymbolSetRep' for manual constructions.
newtype SymbolSet knd = SymbolSet
  { unSymbolSet :: S.HashSet (SomeTypedSymbol knd)
  }
  deriving (Eq, Generic, Hashable)

-- | Set of constant symbols. Excluding unintepreted functions.
type ConstantSymbolSet = SymbolSet 'ConstantKind

-- | Set of any symbols.
type AnySymbolSet = SymbolSet 'AnyKind

instance Semigroup (SymbolSet knd) where
  SymbolSet s1 <> SymbolSet s2 = SymbolSet $ S.union s1 s2

instance Monoid (SymbolSet knd) where
  mempty = emptySet

instance Show (SymbolSet knd) where
  showsPrec prec (SymbolSet s) = showParen (prec >= 10) $ \x ->
    "SymbolSet {"
      ++ go0 (sort $ show <$> S.toList s)
      ++ "}"
      ++ x
    where
      go0 [] = ""
      go0 [x] = x
      go0 (x : xs) = x ++ ", " ++ go0 xs

-- | Model returned by the solver.
--
-- Check 'Grisette.Core.ModelOps' for operations, and 'Grisette.Core.ModelRep'
-- for manual constructions.
newtype Model = Model
  { unModel :: M.HashMap SomeTypedAnySymbol ModelValue
  }
  deriving (Eq, Generic, Hashable)

instance Semigroup Model where
  Model m1 <> Model m2 = Model $ M.union m1 m2

instance Monoid Model where
  mempty = emptyModel

instance Show Model where
  showsPrec prec (Model m) = showParen (prec >= 10) $ \x ->
    "Model {"
      ++ go0 (sortOn (\(x, _) -> show x) $ M.toList m)
      ++ "}"
      ++ x
    where
      go0 [] = ""
      go0 [(SomeTypedSymbol _ s, v)] = showUntyped s ++ " -> " ++ show v
      go0 ((SomeTypedSymbol _ s, v) : xs) = showUntyped s ++ " -> " ++ show v ++ ", " ++ go0 xs

-- | Given a typed symbol and a model, return the equation (symbol = value)
-- encoded in the model.
equation :: TypedAnySymbol a -> Model -> Maybe (Term Bool)
equation tsym m = withSymbolSupported tsym $
  case valueOf tsym m of
    Just v -> Just $ pevalEqTerm (symTerm tsym) (conTerm v)
    Nothing -> Nothing

instance SymbolSetOps (SymbolSet knd) (TypedSymbol knd) where
  emptySet = SymbolSet S.empty
  isEmptySet (SymbolSet s) = S.null s
  containsSymbol s =
    S.member (someTypedSymbol s) . unSymbolSet
  insertSymbol s = SymbolSet . S.insert (someTypedSymbol s) . unSymbolSet
  intersectionSet (SymbolSet s1) (SymbolSet s2) = SymbolSet $ S.intersection s1 s2
  unionSet (SymbolSet s1) (SymbolSet s2) = SymbolSet $ S.union s1 s2
  differenceSet (SymbolSet s1) (SymbolSet s2) = SymbolSet $ S.difference s1 s2

instance
  SymbolSetRep
    (SomeTypedSymbol knd)
    (SymbolSet knd)
    (TypedSymbol knd)
  where
  buildSymbolSet sym = SymbolSet $ S.singleton sym

instance
  SymbolSetRep
    [SomeTypedSymbol knd]
    (SymbolSet knd)
    (TypedSymbol knd)
  where
  buildSymbolSet = SymbolSet . S.fromList

instance
  SymbolSetRep
    [TypedSymbol knd t]
    (SymbolSet knd)
    (TypedSymbol knd)
  where
  buildSymbolSet sym = buildSymbolSet $ someTypedSymbol <$> sym

instance
  SymbolSetRep
    (TypedSymbol knd t)
    (SymbolSet knd)
    (TypedSymbol knd)
  where
  buildSymbolSet sym = insertSymbol sym emptySet

instance
  SymbolSetRep
    ( TypedSymbol knd a,
      TypedSymbol knd b
    )
    (SymbolSet knd)
    (TypedSymbol knd)
  where
  buildSymbolSet (sym1, sym2) =
    insertSymbol sym2
      . insertSymbol sym1
      $ emptySet

instance
  SymbolSetRep
    ( TypedSymbol knd a,
      TypedSymbol knd b,
      TypedSymbol knd c
    )
    (SymbolSet knd)
    (TypedSymbol knd)
  where
  buildSymbolSet (sym1, sym2, sym3) =
    insertSymbol sym3
      . insertSymbol sym2
      . insertSymbol sym1
      $ emptySet

instance
  SymbolSetRep
    ( TypedSymbol knd a,
      TypedSymbol knd b,
      TypedSymbol knd c,
      TypedSymbol knd d
    )
    (SymbolSet knd)
    (TypedSymbol knd)
  where
  buildSymbolSet (sym1, sym2, sym3, sym4) =
    insertSymbol sym4
      . insertSymbol sym3
      . insertSymbol sym2
      . insertSymbol sym1
      $ emptySet

instance
  SymbolSetRep
    ( TypedSymbol knd a,
      TypedSymbol knd b,
      TypedSymbol knd c,
      TypedSymbol knd d,
      TypedSymbol knd e
    )
    (SymbolSet knd)
    (TypedSymbol knd)
  where
  buildSymbolSet (sym1, sym2, sym3, sym4, sym5) =
    insertSymbol sym5
      . insertSymbol sym4
      . insertSymbol sym3
      . insertSymbol sym2
      . insertSymbol sym1
      $ emptySet

instance
  SymbolSetRep
    ( TypedSymbol knd a,
      TypedSymbol knd b,
      TypedSymbol knd c,
      TypedSymbol knd d,
      TypedSymbol knd e,
      TypedSymbol knd f
    )
    (SymbolSet knd)
    (TypedSymbol knd)
  where
  buildSymbolSet (sym1, sym2, sym3, sym4, sym5, sym6) =
    insertSymbol sym6
      . insertSymbol sym5
      . insertSymbol sym4
      . insertSymbol sym3
      . insertSymbol sym2
      . insertSymbol sym1
      $ emptySet

instance
  SymbolSetRep
    ( TypedSymbol knd a,
      TypedSymbol knd b,
      TypedSymbol knd c,
      TypedSymbol knd d,
      TypedSymbol knd e,
      TypedSymbol knd f,
      TypedSymbol knd g
    )
    (SymbolSet knd)
    (TypedSymbol knd)
  where
  buildSymbolSet (sym1, sym2, sym3, sym4, sym5, sym6, sym7) =
    insertSymbol sym7
      . insertSymbol sym6
      . insertSymbol sym5
      . insertSymbol sym4
      . insertSymbol sym3
      . insertSymbol sym2
      . insertSymbol sym1
      $ emptySet

instance
  SymbolSetRep
    ( TypedSymbol knd a,
      TypedSymbol knd b,
      TypedSymbol knd c,
      TypedSymbol knd d,
      TypedSymbol knd e,
      TypedSymbol knd f,
      TypedSymbol knd g,
      TypedSymbol knd h
    )
    (SymbolSet knd)
    (TypedSymbol knd)
  where
  buildSymbolSet (sym1, sym2, sym3, sym4, sym5, sym6, sym7, sym8) =
    insertSymbol sym8
      . insertSymbol sym7
      . insertSymbol sym6
      . insertSymbol sym5
      . insertSymbol sym4
      . insertSymbol sym3
      . insertSymbol sym2
      . insertSymbol sym1
      $ emptySet

instance ModelOps Model AnySymbolSet TypedAnySymbol where
  emptyModel = Model M.empty
  isEmptyModel (Model m) = M.null m
  valueOf :: forall t. TypedAnySymbol t -> Model -> Maybe t
  valueOf sym (Model m) =
    withSymbolSupported sym $
      (unsafeFromModelValue @t)
        <$> M.lookup (someTypedSymbol sym) m
  modelContains sym (Model m) = M.member (someTypedSymbol sym) m
  exceptFor (SymbolSet s) (Model m) = Model $ S.foldl' (flip M.delete) m s
  exceptFor' s (Model m) = Model $ M.delete (someTypedSymbol s) m
  restrictTo (SymbolSet s) (Model m) =
    Model $
      S.foldl'
        ( \acc sym -> case M.lookup sym m of
            Just v -> M.insert sym v acc
            Nothing -> acc
        )
        M.empty
        s
  extendTo (SymbolSet s) (Model m) =
    Model $
      S.foldl'
        ( \acc sym@(SomeTypedSymbol _ (tsym :: TypedAnySymbol t)) -> case M.lookup sym acc of
            Just _ -> acc
            Nothing -> withSymbolSupported tsym $ M.insert sym (defaultValueDynamic (Proxy @t)) acc
        )
        m
        s
  insertValue sym (v :: t) (Model m) =
    withSymbolSupported sym $
      Model $
        M.insert (someTypedSymbol sym) (toModelValue v) m

-- | Evaluate a term in the given model.
evalTerm ::
  (SupportedPrim a) =>
  Bool ->
  Model ->
  S.HashSet SomeTypedConstantSymbol ->
  Term a ->
  Term a
evalTerm fillDefault (Model ma) =
  generalSubstSomeTerm
    ( \(sym :: TypedSymbol 'AnyKind a) ->
        withSymbolSupported sym $
          case (M.lookup (someTypedSymbol sym) ma) of
            Nothing ->
              if fillDefault
                then conTerm (defaultValue @a)
                else symTerm sym
            Just dy ->
              conTerm (unsafeFromModelValue @a dy)
    )

-- |
-- A type used for building a model by hand.
--
-- >>> buildModel ("x" ::= (1 :: Integer), "y" ::= True) :: Model
-- Model {x -> 1 :: Integer, y -> true :: Bool}
data ModelValuePair t = (TypedAnySymbol t) ::= t deriving (Show)

instance ModelRep (ModelValuePair t) Model where
  buildModel (sym ::= val) = insertValue sym val emptyModel

instance (ModelRep a Model, ModelRep b Model) => ModelRep (a, b) Model where
  buildModel (a, b) = buildModel a <> buildModel b

instance
  ( ModelRep a Model,
    ModelRep b Model,
    ModelRep c Model
  ) =>
  ModelRep (a, b, c) Model
  where
  buildModel (a, b, c) = buildModel a <> buildModel b <> buildModel c

instance
  ( ModelRep a Model,
    ModelRep b Model,
    ModelRep c Model,
    ModelRep d Model
  ) =>
  ModelRep (a, b, c, d) Model
  where
  buildModel (a, b, c, d) =
    buildModel a <> buildModel b <> buildModel c <> buildModel d

instance
  ( ModelRep a Model,
    ModelRep b Model,
    ModelRep c Model,
    ModelRep d Model,
    ModelRep e Model
  ) =>
  ModelRep (a, b, c, d, e) Model
  where
  buildModel (a, b, c, d, e) =
    buildModel a <> buildModel b <> buildModel c <> buildModel d <> buildModel e

instance
  ( ModelRep a Model,
    ModelRep b Model,
    ModelRep c Model,
    ModelRep d Model,
    ModelRep e Model,
    ModelRep f Model
  ) =>
  ModelRep (a, b, c, d, e, f) Model
  where
  buildModel (a, b, c, d, e, f) =
    buildModel a
      <> buildModel b
      <> buildModel c
      <> buildModel d
      <> buildModel e
      <> buildModel f

instance
  ( ModelRep a Model,
    ModelRep b Model,
    ModelRep c Model,
    ModelRep d Model,
    ModelRep e Model,
    ModelRep f Model,
    ModelRep g Model
  ) =>
  ModelRep (a, b, c, d, e, f, g) Model
  where
  buildModel (a, b, c, d, e, f, g) =
    buildModel a
      <> buildModel b
      <> buildModel c
      <> buildModel d
      <> buildModel e
      <> buildModel f
      <> buildModel g

instance
  ( ModelRep a Model,
    ModelRep b Model,
    ModelRep c Model,
    ModelRep d Model,
    ModelRep e Model,
    ModelRep f Model,
    ModelRep g Model,
    ModelRep h Model
  ) =>
  ModelRep (a, b, c, d, e, f, g, h) Model
  where
  buildModel (a, b, c, d, e, f, g, h) =
    buildModel a
      <> buildModel b
      <> buildModel c
      <> buildModel d
      <> buildModel e
      <> buildModel f
      <> buildModel g
      <> buildModel h
