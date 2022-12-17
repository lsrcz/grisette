-- |
-- Module      :   Grisette.IR.SymPrim.Data.Prim.PartialEval.Integer
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.Prim.PartialEval.Integer
  ( pevalDivIntegerTerm,
    pevalModIntegerTerm,
  )
where

import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Unfold

-- div
pevalDivIntegerTerm :: Term Integer -> Term Integer -> Term Integer
pevalDivIntegerTerm = binaryUnfoldOnce doPevalDivIntegerTerm divIntegerTerm

doPevalDivIntegerTerm :: Term Integer -> Term Integer -> Maybe (Term Integer)
doPevalDivIntegerTerm (ConcTerm _ a) (ConcTerm _ b) | b /= 0 = Just $ concTerm $ a `div` b
doPevalDivIntegerTerm a (ConcTerm _ 1) = Just a
doPevalDivIntegerTerm _ _ = Nothing

-- mod
pevalModIntegerTerm :: Term Integer -> Term Integer -> Term Integer
pevalModIntegerTerm = binaryUnfoldOnce doPevalModIntegerTerm modIntegerTerm

doPevalModIntegerTerm :: Term Integer -> Term Integer -> Maybe (Term Integer)
doPevalModIntegerTerm (ConcTerm _ a) (ConcTerm _ b) | b /= 0 = Just $ concTerm $ a `mod` b
doPevalModIntegerTerm _ (ConcTerm _ 1) = Just $ concTerm 0
doPevalModIntegerTerm _ (ConcTerm _ (-1)) = Just $ concTerm 0
doPevalModIntegerTerm _ _ = Nothing
