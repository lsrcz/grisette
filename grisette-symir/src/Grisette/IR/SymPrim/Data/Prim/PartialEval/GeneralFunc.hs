{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.IR.SymPrim.Data.Prim.PartialEval.GeneralFunc (pevalGeneralFuncApplyTerm) where

import Grisette.Core.Data.Class.Function
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Grisette.IR.SymPrim.Data.Prim.PartialEval.PartialEval

pevalGeneralFuncApplyTerm :: (SupportedPrim a, SupportedPrim b) => Term (a --> b) -> Term a -> Term b
pevalGeneralFuncApplyTerm = totalize2 doPevalGeneralFuncApplyTerm generalFuncApplyTerm

doPevalGeneralFuncApplyTerm :: (SupportedPrim a, SupportedPrim b) => Term (a --> b) -> Term a -> Maybe (Term b)
doPevalGeneralFuncApplyTerm (ConcTerm _ f) a = Just $ f # a
doPevalGeneralFuncApplyTerm _ _ = Nothing
