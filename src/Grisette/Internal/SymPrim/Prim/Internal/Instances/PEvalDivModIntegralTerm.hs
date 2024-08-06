{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalDivModIntegralTerm
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalDivModIntegralTerm
  ( pevalDefaultDivIntegralTerm,
    pevalDefaultDivBoundedIntegralTerm,
    pevalDefaultModIntegralTerm,
    pevalDefaultQuotIntegralTerm,
    pevalDefaultQuotBoundedIntegralTerm,
    pevalDefaultRemIntegralTerm,
  )
where

import GHC.TypeNats (KnownNat, type (<=))
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.Prim.Internal.Instances.SupportedPrim ()
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( PEvalDivModIntegralTerm
      ( pevalDivIntegralTerm,
        pevalModIntegralTerm,
        pevalQuotIntegralTerm,
        pevalRemIntegralTerm,
        withSbvDivModIntegralTermConstraint
      ),
    SupportedPrim (withPrim),
    Term (ConTerm),
    conTerm,
    divIntegralTerm,
    modIntegralTerm,
    quotIntegralTerm,
    remIntegralTerm,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Unfold (binaryUnfoldOnce)

-- div
pevalDefaultDivIntegralTerm ::
  (PEvalDivModIntegralTerm a) => Term a -> Term a -> Term a
pevalDefaultDivIntegralTerm =
  binaryUnfoldOnce doPevalDefaultDivIntegralTerm divIntegralTerm

doPevalDefaultDivIntegralTerm ::
  (PEvalDivModIntegralTerm a) => Term a -> Term a -> Maybe (Term a)
doPevalDefaultDivIntegralTerm (ConTerm _ a) (ConTerm _ b)
  | b /= 0 = Just $ conTerm $ a `div` b
doPevalDefaultDivIntegralTerm a (ConTerm _ 1) = Just a
doPevalDefaultDivIntegralTerm _ _ = Nothing

pevalDefaultDivBoundedIntegralTerm ::
  (PEvalDivModIntegralTerm a, Bounded a) => Term a -> Term a -> Term a
pevalDefaultDivBoundedIntegralTerm =
  binaryUnfoldOnce doPevalDefaultDivBoundedIntegralTerm divIntegralTerm

doPevalDefaultDivBoundedIntegralTerm ::
  (PEvalDivModIntegralTerm a, Bounded a) =>
  Term a ->
  Term a ->
  Maybe (Term a)
doPevalDefaultDivBoundedIntegralTerm (ConTerm _ a) (ConTerm _ b)
  | b /= 0 && (b /= -1 || a /= minBound) = Just $ conTerm $ a `div` b
doPevalDefaultDivBoundedIntegralTerm a (ConTerm _ 1) = Just a
doPevalDefaultDivBoundedIntegralTerm _ _ = Nothing

-- mod
pevalDefaultModIntegralTerm ::
  (PEvalDivModIntegralTerm a) => Term a -> Term a -> Term a
pevalDefaultModIntegralTerm =
  binaryUnfoldOnce doPevalDefaultModIntegralTerm modIntegralTerm

doPevalDefaultModIntegralTerm ::
  (PEvalDivModIntegralTerm a) => Term a -> Term a -> Maybe (Term a)
doPevalDefaultModIntegralTerm (ConTerm _ a) (ConTerm _ b)
  | b /= 0 = Just $ conTerm $ a `mod` b
doPevalDefaultModIntegralTerm _ (ConTerm _ 1) = Just $ conTerm 0
doPevalDefaultModIntegralTerm _ (ConTerm _ (-1)) = Just $ conTerm 0
doPevalDefaultModIntegralTerm _ _ = Nothing

-- quot
pevalDefaultQuotIntegralTerm ::
  (PEvalDivModIntegralTerm a) => Term a -> Term a -> Term a
pevalDefaultQuotIntegralTerm =
  binaryUnfoldOnce doPevalDefaultQuotIntegralTerm quotIntegralTerm

doPevalDefaultQuotIntegralTerm ::
  (PEvalDivModIntegralTerm a) => Term a -> Term a -> Maybe (Term a)
doPevalDefaultQuotIntegralTerm (ConTerm _ a) (ConTerm _ b)
  | b /= 0 = Just $ conTerm $ a `quot` b
doPevalDefaultQuotIntegralTerm a (ConTerm _ 1) = Just a
doPevalDefaultQuotIntegralTerm _ _ = Nothing

pevalDefaultQuotBoundedIntegralTerm ::
  (PEvalDivModIntegralTerm a, Bounded a) => Term a -> Term a -> Term a
pevalDefaultQuotBoundedIntegralTerm =
  binaryUnfoldOnce doPevalDefaultQuotBoundedIntegralTerm quotIntegralTerm

doPevalDefaultQuotBoundedIntegralTerm ::
  (PEvalDivModIntegralTerm a, Bounded a) =>
  Term a ->
  Term a ->
  Maybe (Term a)
doPevalDefaultQuotBoundedIntegralTerm (ConTerm _ a) (ConTerm _ b)
  | b /= 0 && (b /= -1 || a /= minBound) = Just $ conTerm $ a `quot` b
doPevalDefaultQuotBoundedIntegralTerm a (ConTerm _ 1) = Just a
doPevalDefaultQuotBoundedIntegralTerm _ _ = Nothing

-- rem
pevalDefaultRemIntegralTerm ::
  (PEvalDivModIntegralTerm a) => Term a -> Term a -> Term a
pevalDefaultRemIntegralTerm =
  binaryUnfoldOnce doPevalDefaultRemIntegralTerm remIntegralTerm

doPevalDefaultRemIntegralTerm ::
  (PEvalDivModIntegralTerm a) => Term a -> Term a -> Maybe (Term a)
doPevalDefaultRemIntegralTerm (ConTerm _ a) (ConTerm _ b)
  | b /= 0 = Just $ conTerm $ a `rem` b
doPevalDefaultRemIntegralTerm _ (ConTerm _ 1) = Just $ conTerm 0
doPevalDefaultRemIntegralTerm _ (ConTerm _ (-1)) = Just $ conTerm 0
doPevalDefaultRemIntegralTerm _ _ = Nothing

instance PEvalDivModIntegralTerm Integer where
  pevalDivIntegralTerm = pevalDefaultDivIntegralTerm
  pevalModIntegralTerm = pevalDefaultModIntegralTerm
  pevalQuotIntegralTerm = pevalDefaultQuotIntegralTerm
  pevalRemIntegralTerm = pevalDefaultRemIntegralTerm
  withSbvDivModIntegralTermConstraint r = r

instance (KnownNat n, 1 <= n) => PEvalDivModIntegralTerm (IntN n) where
  pevalDivIntegralTerm = pevalDefaultDivBoundedIntegralTerm
  pevalModIntegralTerm = pevalDefaultModIntegralTerm
  pevalQuotIntegralTerm = pevalDefaultQuotBoundedIntegralTerm
  pevalRemIntegralTerm = pevalDefaultRemIntegralTerm
  withSbvDivModIntegralTermConstraint r = withPrim @(IntN n) r

instance (KnownNat n, 1 <= n) => PEvalDivModIntegralTerm (WordN n) where
  pevalDivIntegralTerm = pevalDefaultDivIntegralTerm
  pevalModIntegralTerm = pevalDefaultModIntegralTerm
  pevalQuotIntegralTerm = pevalDefaultQuotIntegralTerm
  pevalRemIntegralTerm = pevalDefaultRemIntegralTerm
  withSbvDivModIntegralTermConstraint r = withPrim @(WordN n) r
