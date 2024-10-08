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
    introSupportedPrimConstraint,
    modIntegralTerm,
    quotIntegralTerm,
    remIntegralTerm,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Unfold (binaryUnfoldOnce)

-- | Default partial evaluation of division operation for integral types.
pevalDefaultDivIntegralTerm ::
  (PEvalDivModIntegralTerm a, Integral a) => Term a -> Term a -> Term a
pevalDefaultDivIntegralTerm l r =
  introSupportedPrimConstraint l $
    binaryUnfoldOnce doPevalDefaultDivIntegralTerm divIntegralTerm l r

doPevalDefaultDivIntegralTerm ::
  (PEvalDivModIntegralTerm a, Integral a) => Term a -> Term a -> Maybe (Term a)
doPevalDefaultDivIntegralTerm (ConTerm _ _ _ _ a) (ConTerm _ _ _ _ b)
  | b /= 0 = Just $ conTerm $ a `div` b
doPevalDefaultDivIntegralTerm a (ConTerm _ _ _ _ 1) = Just a
doPevalDefaultDivIntegralTerm _ _ = Nothing

-- | Default partial evaluation of division operation for bounded integral
-- types.
pevalDefaultDivBoundedIntegralTerm ::
  (PEvalDivModIntegralTerm a, Bounded a, Integral a) =>
  Term a ->
  Term a ->
  Term a
pevalDefaultDivBoundedIntegralTerm l r =
  introSupportedPrimConstraint l $
    binaryUnfoldOnce doPevalDefaultDivBoundedIntegralTerm divIntegralTerm l r

doPevalDefaultDivBoundedIntegralTerm ::
  (PEvalDivModIntegralTerm a, Bounded a, Integral a) =>
  Term a ->
  Term a ->
  Maybe (Term a)
doPevalDefaultDivBoundedIntegralTerm (ConTerm _ _ _ _ a) (ConTerm _ _ _ _ b)
  | b /= 0 && (b /= -1 || a /= minBound) = Just $ conTerm $ a `div` b
doPevalDefaultDivBoundedIntegralTerm a (ConTerm _ _ _ _ 1) = Just a
doPevalDefaultDivBoundedIntegralTerm _ _ = Nothing

-- | Default partial evaluation of modulo operation for integral types.
pevalDefaultModIntegralTerm ::
  (PEvalDivModIntegralTerm a, Integral a) => Term a -> Term a -> Term a
pevalDefaultModIntegralTerm l r =
  introSupportedPrimConstraint l $
    binaryUnfoldOnce doPevalDefaultModIntegralTerm modIntegralTerm l r

doPevalDefaultModIntegralTerm ::
  (PEvalDivModIntegralTerm a, Integral a) => Term a -> Term a -> Maybe (Term a)
doPevalDefaultModIntegralTerm (ConTerm _ _ _ _ a) (ConTerm _ _ _ _ b)
  | b /= 0 = Just $ conTerm $ a `mod` b
doPevalDefaultModIntegralTerm _ (ConTerm _ _ _ _ 1) = Just $ conTerm 0
doPevalDefaultModIntegralTerm _ (ConTerm _ _ _ _ (-1)) = Just $ conTerm 0
doPevalDefaultModIntegralTerm _ _ = Nothing

-- | Default partial evaluation of quotient operation for integral types.
pevalDefaultQuotIntegralTerm ::
  (PEvalDivModIntegralTerm a, Integral a) => Term a -> Term a -> Term a
pevalDefaultQuotIntegralTerm l r =
  introSupportedPrimConstraint l $
    binaryUnfoldOnce doPevalDefaultQuotIntegralTerm quotIntegralTerm l r

doPevalDefaultQuotIntegralTerm ::
  (PEvalDivModIntegralTerm a, Integral a) => Term a -> Term a -> Maybe (Term a)
doPevalDefaultQuotIntegralTerm (ConTerm _ _ _ _ a) (ConTerm _ _ _ _ b)
  | b /= 0 = Just $ conTerm $ a `quot` b
doPevalDefaultQuotIntegralTerm a (ConTerm _ _ _ _ 1) = Just a
doPevalDefaultQuotIntegralTerm _ _ = Nothing

-- | Default partial evaluation of quotient operation for bounded integral
-- types.
pevalDefaultQuotBoundedIntegralTerm ::
  (PEvalDivModIntegralTerm a, Bounded a, Integral a) => Term a -> Term a -> Term a
pevalDefaultQuotBoundedIntegralTerm l r =
  introSupportedPrimConstraint l $
    binaryUnfoldOnce doPevalDefaultQuotBoundedIntegralTerm quotIntegralTerm l r

doPevalDefaultQuotBoundedIntegralTerm ::
  (PEvalDivModIntegralTerm a, Bounded a, Integral a) =>
  Term a ->
  Term a ->
  Maybe (Term a)
doPevalDefaultQuotBoundedIntegralTerm (ConTerm _ _ _ _ a) (ConTerm _ _ _ _ b)
  | b /= 0 && (b /= -1 || a /= minBound) = Just $ conTerm $ a `quot` b
doPevalDefaultQuotBoundedIntegralTerm a (ConTerm _ _ _ _ 1) = Just a
doPevalDefaultQuotBoundedIntegralTerm _ _ = Nothing

-- | Default partial evaluation of remainder operation for integral types.
pevalDefaultRemIntegralTerm ::
  (PEvalDivModIntegralTerm a, Integral a) => Term a -> Term a -> Term a
pevalDefaultRemIntegralTerm l r =
  introSupportedPrimConstraint l $
    binaryUnfoldOnce doPevalDefaultRemIntegralTerm remIntegralTerm l r

doPevalDefaultRemIntegralTerm ::
  (PEvalDivModIntegralTerm a, Integral a) => Term a -> Term a -> Maybe (Term a)
doPevalDefaultRemIntegralTerm (ConTerm _ _ _ _ a) (ConTerm _ _ _ _ b)
  | b /= 0 = Just $ conTerm $ a `rem` b
doPevalDefaultRemIntegralTerm _ (ConTerm _ _ _ _ 1) = Just $ conTerm 0
doPevalDefaultRemIntegralTerm _ (ConTerm _ _ _ _ (-1)) = Just $ conTerm 0
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
