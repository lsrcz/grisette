{-# LANGUAGE RankNTypes #-}

module Grisette.IR.SymPrim.Data.Prim.InternedTerm.TermSubstitution (substTerm) where

import {-# SOURCE #-} Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term

substTerm :: forall a b. (SupportedPrim a, SupportedPrim b) => TypedSymbol a -> Term a -> Term b -> Term b
