{-# LANGUAGE RankNTypes #-}

module Grisette.IR.SymPrim.Data.Prim.InternedTerm.TermUtils
  ( identity,
    identityWithTypeRep,
    introSupportedPrimConstraint,
    extractSymbolicsTerm,
    castTerm,
    pformat,
    termSize,
    termsSize,
  )
where

import qualified Data.HashSet as S
import Data.Interned (Id)
import Data.Typeable (TypeRep, Typeable)
import {-# SOURCE #-} Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
  ( SomeTypedSymbol,
    SupportedPrim,
    Term,
  )

identity :: Term t -> Id
identityWithTypeRep :: forall t. Term t -> (TypeRep, Id)
introSupportedPrimConstraint :: forall t a. Term t -> ((SupportedPrim t) => a) -> a
extractSymbolicsTerm :: (SupportedPrim a) => Term a -> S.HashSet SomeTypedSymbol
castTerm :: forall a b. (Typeable b) => Term a -> Maybe (Term b)
pformat :: forall t. (SupportedPrim t) => Term t -> String
termsSize :: [Term a] -> Int
termSize :: Term a -> Int
