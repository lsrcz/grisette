{-# LANGUAGE RankNTypes #-}

module Pizza.IR.SymPrim.Data.Prim.InternedTerm.GeneralFuncSubst (generalFuncSubst) where

import {-# SOURCE #-} Pizza.IR.SymPrim.Data.Prim.InternedTerm.Term

generalFuncSubst :: forall a b. (SupportedPrim a, SupportedPrim b) => TermSymbol -> Term a -> Term b -> Term b
