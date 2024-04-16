{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.Prim.PartialEval.TabularFun
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.Prim.PartialEval.TabularFun
  ( pevalTabularFunApplyTerm,
  )
where

import Grisette.Core.Data.Class.Function (Function ((#)))
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bool
  ( pevalEqvTerm,
    pevalITETerm,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.PartialEval
  ( totalize2,
  )
import Grisette.IR.SymPrim.Data.Prim.Term
  ( SupportedPrim,
    Term (ConTerm),
    conTerm,
    tabularFunApplyTerm,
  )
import Grisette.IR.SymPrim.Data.TabularFun
  ( type (=->) (TabularFun),
  )

pevalTabularFunApplyTerm :: (SupportedPrim a, SupportedPrim b) => Term (a =-> b) -> Term a -> Term b
pevalTabularFunApplyTerm = totalize2 doPevalTabularFunApplyTerm tabularFunApplyTerm

doPevalTabularFunApplyTerm :: (SupportedPrim a, SupportedPrim b) => Term (a =-> b) -> Term a -> Maybe (Term b)
doPevalTabularFunApplyTerm (ConTerm _ f) (ConTerm _ a) = Just $ conTerm $ f # a
doPevalTabularFunApplyTerm (ConTerm _ (TabularFun f d)) a = Just $ go f
  where
    go [] = conTerm d
    go ((x, y) : xs) = pevalITETerm (pevalEqvTerm a (conTerm x)) (conTerm y) (go xs)
doPevalTabularFunApplyTerm _ _ = Nothing
