{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.Prim.PartialEval.Integral
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.Prim.PartialEval.Integral
  ( pevalDivIntegralTerm,
    pevalModIntegralTerm,
    pevalQuotIntegralTerm,
    pevalRemIntegralTerm,
    pevalDivBoundedIntegralTerm,
    pevalModBoundedIntegralTerm,
    pevalQuotBoundedIntegralTerm,
    pevalRemBoundedIntegralTerm,
  )
where

import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
  ( SupportedPrim,
    Term (ConTerm),
    conTerm,
    divBoundedIntegralTerm,
    divIntegralTerm,
    modIntegralTerm,
    quotBoundedIntegralTerm,
    quotIntegralTerm,
    remIntegralTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Unfold
  ( binaryUnfoldOnce,
  )

-- div
pevalDivIntegralTerm :: (SupportedPrim a, Integral a) => Term a -> Term a -> Term a
pevalDivIntegralTerm = binaryUnfoldOnce doPevalDivIntegralTerm divIntegralTerm

doPevalDivIntegralTerm :: (SupportedPrim a, Integral a) => Term a -> Term a -> Maybe (Term a)
doPevalDivIntegralTerm (ConTerm _ a) (ConTerm _ b) | b /= 0 = Just $ conTerm $ a `div` b
doPevalDivIntegralTerm a (ConTerm _ 1) = Just a
doPevalDivIntegralTerm _ _ = Nothing

pevalDivBoundedIntegralTerm :: (SupportedPrim a, Bounded a, Integral a) => Term a -> Term a -> Term a
pevalDivBoundedIntegralTerm = binaryUnfoldOnce doPevalDivBoundedIntegralTerm divBoundedIntegralTerm

doPevalDivBoundedIntegralTerm :: (SupportedPrim a, Bounded a, Integral a) => Term a -> Term a -> Maybe (Term a)
doPevalDivBoundedIntegralTerm (ConTerm _ a) (ConTerm _ b) | b /= 0 && (b /= -1 || a /= minBound) = Just $ conTerm $ a `div` b
doPevalDivBoundedIntegralTerm a (ConTerm _ 1) = Just a
doPevalDivBoundedIntegralTerm _ _ = Nothing

-- mod
pevalModIntegralTerm :: (SupportedPrim a, Integral a) => Term a -> Term a -> Term a
pevalModIntegralTerm = binaryUnfoldOnce doPevalModIntegralTerm modIntegralTerm

doPevalModIntegralTerm :: (SupportedPrim a, Integral a) => Term a -> Term a -> Maybe (Term a)
doPevalModIntegralTerm (ConTerm _ a) (ConTerm _ b) | b /= 0 = Just $ conTerm $ a `mod` b
doPevalModIntegralTerm _ (ConTerm _ 1) = Just $ conTerm 0
doPevalModIntegralTerm _ (ConTerm _ (-1)) = Just $ conTerm 0
doPevalModIntegralTerm _ _ = Nothing

pevalModBoundedIntegralTerm :: (SupportedPrim a, Integral a) => Term a -> Term a -> Term a
pevalModBoundedIntegralTerm = pevalModIntegralTerm

-- quot
pevalQuotIntegralTerm :: (SupportedPrim a, Integral a) => Term a -> Term a -> Term a
pevalQuotIntegralTerm = binaryUnfoldOnce doPevalQuotIntegralTerm quotIntegralTerm

doPevalQuotIntegralTerm :: (SupportedPrim a, Integral a) => Term a -> Term a -> Maybe (Term a)
doPevalQuotIntegralTerm (ConTerm _ a) (ConTerm _ b) | b /= 0 = Just $ conTerm $ a `quot` b
doPevalQuotIntegralTerm a (ConTerm _ 1) = Just a
doPevalQuotIntegralTerm _ _ = Nothing

pevalQuotBoundedIntegralTerm :: (SupportedPrim a, Bounded a, Integral a) => Term a -> Term a -> Term a
pevalQuotBoundedIntegralTerm = binaryUnfoldOnce doPevalQuotBoundedIntegralTerm quotBoundedIntegralTerm

doPevalQuotBoundedIntegralTerm :: (SupportedPrim a, Bounded a, Integral a) => Term a -> Term a -> Maybe (Term a)
doPevalQuotBoundedIntegralTerm (ConTerm _ a) (ConTerm _ b) | b /= 0 && (b /= -1 || a /= minBound) = Just $ conTerm $ a `quot` b
doPevalQuotBoundedIntegralTerm a (ConTerm _ 1) = Just a
doPevalQuotBoundedIntegralTerm _ _ = Nothing

-- rem
pevalRemIntegralTerm :: (SupportedPrim a, Integral a) => Term a -> Term a -> Term a
pevalRemIntegralTerm = binaryUnfoldOnce doPevalRemIntegralTerm remIntegralTerm

doPevalRemIntegralTerm :: (SupportedPrim a, Integral a) => Term a -> Term a -> Maybe (Term a)
doPevalRemIntegralTerm (ConTerm _ a) (ConTerm _ b) | b /= 0 = Just $ conTerm $ a `rem` b
doPevalRemIntegralTerm _ (ConTerm _ 1) = Just $ conTerm 0
doPevalRemIntegralTerm _ (ConTerm _ (-1)) = Just $ conTerm 0
doPevalRemIntegralTerm _ _ = Nothing

pevalRemBoundedIntegralTerm :: (SupportedPrim a, Bounded a, Integral a) => Term a -> Term a -> Term a
pevalRemBoundedIntegralTerm = pevalRemIntegralTerm
