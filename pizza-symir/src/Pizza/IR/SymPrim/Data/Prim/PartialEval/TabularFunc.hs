{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Pizza.IR.SymPrim.Data.Prim.PartialEval.TabularFunc
  ( pevalTabularFuncApplyTerm,
  )
where

import Pizza.Core.Data.Class.Function
import Pizza.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Pizza.IR.SymPrim.Data.Prim.InternedTerm.Term
import Pizza.IR.SymPrim.Data.Prim.PartialEval.Bool
import Pizza.IR.SymPrim.Data.Prim.PartialEval.PartialEval
import Pizza.IR.SymPrim.Data.TabularFunc

pevalTabularFuncApplyTerm :: (SupportedPrim a, SupportedPrim b) => Term (a =-> b) -> Term a -> Term b
pevalTabularFuncApplyTerm = totalize2 doPevalTabularFuncApplyTerm tabularFuncApplyTerm

doPevalTabularFuncApplyTerm :: (SupportedPrim a, SupportedPrim b) => Term (a =-> b) -> Term a -> Maybe (Term b)
doPevalTabularFuncApplyTerm (ConcTerm _ f) (ConcTerm _ a) = Just $ concTerm $ f # a
doPevalTabularFuncApplyTerm (ConcTerm _ (TabularFunc f d)) a = Just $ go f
  where
    go [] = concTerm d
    go ((x, y) : xs) = pevalITETerm (pevalEqvTerm a (concTerm x)) (concTerm y) (go xs)
doPevalTabularFuncApplyTerm _ _ = Nothing
