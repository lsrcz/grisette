{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import GHC.Stack (HasCallStack)
import Grisette
  ( FreshT (FreshT),
    liftFresh,
    mrgSingle,
    simpleMerge,
  )
import Grisette.Internal.Core.Control.Monad.Union (Union, liftUnion)
import Grisette.Internal.Core.Data.Class.ExtractSym
  ( ExtractSym (extractSymMaybe),
  )
import Grisette.Internal.Core.Data.Class.GenSym
  ( Fresh,
    GenSym (fresh),
    MonadFresh,
  )
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.TryMerge (TryMerge)
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( SomeTypedSymbol (SomeTypedSymbol),
    TypedSymbol (TypedSymbol),
    existsTerm,
    forallTerm,
  )
import Grisette.Internal.SymPrim.Prim.Model
  ( ConstantSymbolSet,
    SymbolSet (SymbolSet),
  )
import Grisette.Internal.SymPrim.SymBool (SymBool (SymBool))

#if MIN_VERSION_sbv(10,1,0)
sbvVersionCheck :: HasCallStack => ()
sbvVersionCheck = ()
#else
sbvVersionCheck :: HasCallStack => ()
sbvVersionCheck =
  error 
    "Quantifiers are only available when you build with SBV 10.1.0 or later."
#endif

forallSet :: ConstantSymbolSet -> SymBool -> SymBool
forallSet (SymbolSet set) b =
  sbvVersionCheck `seq`
    HS.foldr
      ( \(SomeTypedSymbol _ s@TypedSymbol {}) (SymBool b') ->
          SymBool $ forallTerm s b'
      )
      b
      set

forallSym :: (HasCallStack, ExtractSym a) => a -> SymBool -> SymBool
forallSym s b =
  sbvVersionCheck `seq` case extractSymMaybe s of
    Just s' -> forallSet s' b
    Nothing ->
      error
        "Cannot use forall here. Only non-function symbols can be quantified."

existsSet :: ConstantSymbolSet -> SymBool -> SymBool
existsSet (SymbolSet set) b =
  sbvVersionCheck `seq`
    HS.foldr
      ( \(SomeTypedSymbol _ s@TypedSymbol {}) (SymBool b') ->
          SymBool $ existsTerm s b'
      )
      b
      set

existsSym :: (HasCallStack, ExtractSym a) => a -> SymBool -> SymBool
existsSym s b =
  sbvVersionCheck `seq` case extractSymMaybe s of
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
