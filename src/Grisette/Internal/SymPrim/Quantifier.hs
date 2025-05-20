{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Quantifier
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.Quantifier
  ( forallSet,
    forallSym,
    existsSet,
    existsSym,
    forallFresh,
    existsFresh,
  )
where

import Data.Bifunctor (Bifunctor (first))
import qualified Data.HashSet as HS
import Data.List (sort)
import GHC.Stack (HasCallStack)
import Grisette.Internal.Core.Control.Monad.Union (Union)
import Grisette.Internal.Core.Data.Class.ExtractSym
  ( ExtractSym (extractSymMaybe),
  )
import Grisette.Internal.Core.Data.Class.GenSym
  ( Fresh,
    FreshT (FreshT),
    GenSym (fresh),
    MonadFresh,
    liftFresh,
  )
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.TryMerge (TryMerge, mrgSingle)
import Grisette.Internal.Core.Data.Class.UnionView (liftUnion, simpleMerge)
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( SomeTypedSymbol (SomeTypedSymbol),
    existsTerm,
    forallTerm,
  )
import Grisette.Internal.SymPrim.Prim.Model
  ( ConstantSymbolSet,
    SymbolSet (SymbolSet),
  )
import Grisette.Internal.SymPrim.SymBool (SymBool (SymBool))

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
-- >>> import Grisette.Backend
-- >>> import Grisette.Lib.Base

-- | Forall quantifier over a set of constant symbols. Quantifier over functions
-- is not supported.
--
-- >>> let xsym = "x" :: TypedConstantSymbol Integer
-- >>> let ysym = "y" :: TypedConstantSymbol Integer
-- >>> let x = "x" :: SymInteger
-- >>> let y = "y" :: SymInteger
-- >>> forallSet (buildSymbolSet [xsym, ysym]) (x .== y)
-- (forall x :: Integer (forall y :: Integer (= x y)))
--
-- Only available with SBV 10.1.0 or later.
forallSet :: ConstantSymbolSet -> SymBool -> SymBool
forallSet (SymbolSet set) b =
  foldr
    ( \(SomeTypedSymbol s) (SymBool b') ->
        SymBool $ forallTerm s b'
    )
    b
    (sort $ HS.toList set)

-- | Forall quantifier over all symbolic constants in a value. Quantifier over
-- functions is not supported.
--
-- >>> let a = ["x", "y"] :: [SymInteger]
-- >>> forallSym a $ sum a .== 0
-- (forall x :: Integer (forall y :: Integer (= (+ x y) 0)))
--
-- Only available with sbv 10.1.0 or later.
forallSym :: (HasCallStack, ExtractSym a) => a -> SymBool -> SymBool
forallSym s b =
  case extractSymMaybe s of
    Just s' -> forallSet s' b
    Nothing ->
      error
        "Cannot use forall here. Only non-function symbols can be quantified."

-- | Exists quantifier over a set of constant symbols. Quantifier over functions
-- is not supported.
--
-- >>> let xsym = "x" :: TypedConstantSymbol Integer
-- >>> let ysym = "y" :: TypedConstantSymbol Integer
-- >>> let x = "x" :: SymInteger
-- >>> let y = "y" :: SymInteger
-- >>> existsSet (buildSymbolSet [xsym, ysym]) (x .== y)
-- (exists x :: Integer (exists y :: Integer (= x y)))
--
-- Only available with SBV 10.1.0 or later.
existsSet :: ConstantSymbolSet -> SymBool -> SymBool
existsSet (SymbolSet set) b =
  foldr
    ( \(SomeTypedSymbol s) (SymBool b') ->
        SymBool $ existsTerm s b'
    )
    b
    (sort $ HS.toList set)

-- | Exists quantifier over all symbolic constants in a value. Quantifier over
-- functions is not supported.
--
-- >>> let a = ["x", "y"] :: [SymInteger]
-- >>> existsSym a $ sum a .== 0
-- (exists x :: Integer (exists y :: Integer (= (+ x y) 0)))
--
-- Only available with sbv 10.1.0 or later.
existsSym :: (HasCallStack, ExtractSym a) => a -> SymBool -> SymBool
existsSym s b =
  case extractSymMaybe s of
    Just s' -> existsSet s' b
    Nothing ->
      error
        "Cannot use exists here. Only non-function symbols can be quantified."

freshTUnionToFreshUnion ::
  forall a.
  (Mergeable a) =>
  FreshT Union a ->
  Fresh (Union a)
freshTUnionToFreshUnion (FreshT v) =
  FreshT $ \ident index ->
    return $ simpleMerge $ first mrgSingle <$> v ident index

-- | Forall quantifier over symbolic constants in a freshly generated value.
-- Quantifier over functions is not supported.
--
-- >>> :{
-- x :: Fresh SymBool
-- x = forallFresh () $ \(a :: SymBool) ->
--       existsFresh () $ \(b :: SymBool) ->
--         mrgReturn $ a .== b
-- :}
--
-- >>> runFresh x "x"
-- (forall x@0 :: Bool (exists x@1 :: Bool (= x@0 x@1)))
--
-- Only available with sbv 10.1.0 or later.
forallFresh ::
  ( HasCallStack,
    ExtractSym v,
    MonadFresh m,
    GenSym spec v,
    TryMerge m
  ) =>
  spec ->
  (v -> FreshT Union SymBool) ->
  m SymBool
forallFresh spec f = do
  u <- fresh spec
  p <- liftFresh . fmap simpleMerge . freshTUnionToFreshUnion $ do
    liftUnion u >>= f
  mrgSingle $ forallSym u p

-- | Exists quantifier over symbolic constants in a freshly generated value.
-- Quantifier over functions is not supported.
--
-- >>> :{
-- x :: Fresh SymBool
-- x = forallFresh () $ \(a :: SymBool) ->
--       existsFresh () $ \(b :: SymBool) ->
--         mrgReturn $ a .== b
-- :}
--
-- >>> runFresh x "x"
-- (forall x@0 :: Bool (exists x@1 :: Bool (= x@0 x@1)))
--
-- Only available with sbv 10.1.0 or later.
existsFresh ::
  ( HasCallStack,
    ExtractSym v,
    MonadFresh m,
    GenSym spec v,
    TryMerge m
  ) =>
  spec ->
  (v -> FreshT Union SymBool) ->
  m SymBool
existsFresh spec f = do
  u <- fresh spec
  p <- liftFresh . fmap simpleMerge . freshTUnionToFreshUnion $ do
    liftUnion u >>= f
  mrgSingle $ existsSym u p
