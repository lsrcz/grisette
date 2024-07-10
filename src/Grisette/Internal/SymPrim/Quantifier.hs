{-# LANGUAGE DataKinds #-}

module Grisette.Internal.SymPrim.Quantifier
  ( forallSet,
    forallSym,
    existsSet,
    existsSym,
  )
where

import qualified Data.HashSet as HS
import GHC.Stack (HasCallStack)
import Grisette.Internal.Core.Data.Class.ExtractSym
  ( ExtractSym (extractSymMaybe),
  )
import Grisette.Internal.SymPrim.Prim.Model (SymbolSet (SymbolSet))
import Grisette.Internal.SymPrim.Prim.Term
  ( SomeTypedSymbol (SomeTypedSymbol),
    SymbolKind (NonFuncSymbol),
    TypedSymbol (TypedSymbol),
    existsTerm,
    forallTerm,
  )
import Grisette.Internal.SymPrim.SymBool (SymBool (SymBool))

forallSet :: SymbolSet 'NonFuncSymbol -> SymBool -> SymBool
forallSet (SymbolSet set) b =
  HS.foldr
    ( \(SomeTypedSymbol _ s@TypedSymbol {}) (SymBool b') ->
        SymBool $ forallTerm s b'
    )
    b
    set

forallSym :: (HasCallStack, ExtractSym a) => a -> SymBool -> SymBool
forallSym s b = case extractSymMaybe s of
  Just s' -> forallSet s' b
  Nothing ->
    error "Cannot use forall here. Only non-function symbols can be quantified."

existsSet :: SymbolSet 'NonFuncSymbol -> SymBool -> SymBool
existsSet (SymbolSet set) b =
  HS.foldr
    ( \(SomeTypedSymbol _ s@TypedSymbol {}) (SymBool b') ->
        SymBool $ existsTerm s b'
    )
    b
    set

existsSym :: (HasCallStack, ExtractSym a) => a -> SymBool -> SymBool
existsSym s b = case extractSymMaybe s of
  Just s' -> existsSet s' b
  Nothing ->
    error "Cannot use exists here. Only non-function symbols can be quantified."
