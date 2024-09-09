{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalNumTerm
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalNumTerm
  ( pevalDefaultAddNumTerm,
    pevalDefaultNegNumTerm,
  )
where

import Control.Monad (msum)
import Data.Bits (Bits)
import Data.SBV (Bits (isSigned))
import GHC.TypeLits (KnownNat, type (<=))
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP (FP, ValidFP)
import Grisette.Internal.SymPrim.Prim.Internal.Instances.SupportedPrim ()
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( PEvalNumTerm
      ( pevalAbsNumTerm,
        pevalAddNumTerm,
        pevalMulNumTerm,
        pevalNegNumTerm,
        pevalSignumNumTerm,
        withSbvNumTermConstraint
      ),
    SupportedPrim (withPrim),
    Term (AbsNumTerm, AddNumTerm, ConTerm, MulNumTerm, NegNumTerm),
    absNumTerm,
    addNumTerm,
    conTerm,
    introSupportedPrimConstraint,
    mulNumTerm,
    negNumTerm,
    pevalSubNumTerm,
    signumNumTerm,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Unfold
  ( binaryUnfoldOnce,
    generalBinaryUnfolded,
    generalUnaryUnfolded,
    unaryUnfoldOnce,
  )

-- | Default partial evaluation of addition of numerical terms.
pevalDefaultAddNumTerm :: (PEvalNumTerm a, Eq a) => Term a -> Term a -> Term a
pevalDefaultAddNumTerm l r =
  introSupportedPrimConstraint l $
    binaryUnfoldOnce
      doPevalDefaultAddNumTerm
      (\a b -> normalizeAddNum $ addNumTerm a b)
      l
      r

doPevalDefaultAddNumTerm ::
  (PEvalNumTerm a, Eq a) => Term a -> Term a -> Maybe (Term a)
doPevalDefaultAddNumTerm (ConTerm _ _ _ _ a) (ConTerm _ _ _ _ b) = Just $ conTerm $ a + b
doPevalDefaultAddNumTerm l@(ConTerm _ _ _ _ a) b = case (a, b) of
  (0, k) -> Just k
  (l1, AddNumTerm _ _ _ _ (ConTerm _ _ _ _ j) k) ->
    Just $ pevalAddNumTerm (conTerm $ l1 + j) k
  _ -> doPevalDefaultAddNumTermNoCon l b
doPevalDefaultAddNumTerm a r@(ConTerm {}) = doPevalDefaultAddNumTerm r a
doPevalDefaultAddNumTerm l r = doPevalDefaultAddNumTermNoCon l r

doPevalDefaultAddNumTermNoCon ::
  (PEvalNumTerm a) => Term a -> Term a -> Maybe (Term a)
doPevalDefaultAddNumTermNoCon (AddNumTerm _ _ _ _ i@ConTerm {} j) k =
  Just $ pevalAddNumTerm i $ pevalAddNumTerm j k
doPevalDefaultAddNumTermNoCon i (AddNumTerm _ _ _ _ j@ConTerm {} k) =
  Just $ pevalAddNumTerm j $ pevalAddNumTerm i k
doPevalDefaultAddNumTermNoCon (NegNumTerm _ _ _ _ i) (NegNumTerm _ _ _ _ j) =
  Just $ pevalNegNumTerm $ pevalAddNumTerm i j
doPevalDefaultAddNumTermNoCon
  (MulNumTerm _ _ _ _ (ConTerm _ _ _ _ i) j)
  (MulNumTerm _ _ _ _ (ConTerm _ _ _ _ k) l)
    | j == l = Just $ pevalMulNumTerm (conTerm $ i + k) j
doPevalDefaultAddNumTermNoCon
  (MulNumTerm _ _ _ _ i@ConTerm {} j)
  (MulNumTerm _ _ _ _ k@(ConTerm {}) l)
    | i == k = Just $ pevalMulNumTerm i (pevalAddNumTerm j l)
doPevalDefaultAddNumTermNoCon _ _ = Nothing

normalizeAddNum :: (PEvalNumTerm a) => Term a -> Term a
normalizeAddNum (AddNumTerm _ _ _ _ l r@(ConTerm {})) = addNumTerm r l
normalizeAddNum v = v

-- | Default partial evaluation of negation of numerical terms.
pevalDefaultNegNumTerm :: (PEvalNumTerm a, Eq a) => Term a -> Term a
pevalDefaultNegNumTerm l =
  introSupportedPrimConstraint l $
    unaryUnfoldOnce doPevalDefaultNegNumTerm negNumTerm l

doPevalDefaultNegNumTerm :: (PEvalNumTerm a) => Term a -> Maybe (Term a)
doPevalDefaultNegNumTerm (ConTerm _ _ _ _ a) = Just $ conTerm $ -a
doPevalDefaultNegNumTerm (NegNumTerm _ _ _ _ v) = Just v
doPevalDefaultNegNumTerm (AddNumTerm _ _ _ _ (ConTerm _ _ _ _ l) r) =
  Just $ pevalSubNumTerm (conTerm $ -l) r
doPevalDefaultNegNumTerm (AddNumTerm _ _ _ _ (NegNumTerm _ _ _ _ l) r) =
  Just $ pevalAddNumTerm l (pevalNegNumTerm r)
doPevalDefaultNegNumTerm (AddNumTerm _ _ _ _ l (NegNumTerm _ _ _ _ r)) =
  Just $ pevalAddNumTerm (pevalNegNumTerm l) r
doPevalDefaultNegNumTerm (MulNumTerm _ _ _ _ (ConTerm _ _ _ _ l) r) =
  Just $ pevalMulNumTerm (conTerm $ -l) r
doPevalDefaultNegNumTerm (MulNumTerm _ _ _ _ (NegNumTerm {}) _) =
  error "Should not happen"
doPevalDefaultNegNumTerm (MulNumTerm _ _ _ _ _ (NegNumTerm {})) =
  error "Should not happen"
doPevalDefaultNegNumTerm (AddNumTerm _ _ _ _ _ ConTerm {}) = error "Should not happen"
doPevalDefaultNegNumTerm _ = Nothing

-- Mul
pevalDefaultMulNumTerm :: (PEvalNumTerm a, Eq a) => Term a -> Term a -> Term a
pevalDefaultMulNumTerm l r =
  introSupportedPrimConstraint l $
    binaryUnfoldOnce
      doPevalDefaultMulNumTerm
      (\a b -> normalizeMulNum $ mulNumTerm a b)
      l
      r

normalizeMulNum :: (PEvalNumTerm a) => Term a -> Term a
normalizeMulNum (MulNumTerm _ _ _ _ l r@(ConTerm {})) = mulNumTerm r l
normalizeMulNum v = v

doPevalDefaultMulNumTerm ::
  (PEvalNumTerm a, Eq a) => Term a -> Term a -> Maybe (Term a)
doPevalDefaultMulNumTerm (ConTerm _ _ _ _ a) (ConTerm _ _ _ _ b) =
  Just $ conTerm $ a * b
doPevalDefaultMulNumTerm l@(ConTerm _ _ _ _ a) b = case (a, b) of
  (0, _) -> Just $ conTerm 0
  (1, k) -> Just k
  (-1, k) -> Just $ pevalNegNumTerm k
  (l1, MulNumTerm _ _ _ _ (ConTerm _ _ _ _ j) k) ->
    Just $ pevalMulNumTerm (conTerm $ l1 * j) k
  (l1, AddNumTerm _ _ _ _ (ConTerm _ _ _ _ j) k) ->
    Just $ pevalAddNumTerm (conTerm $ l1 * j) (pevalMulNumTerm (conTerm l1) k)
  (l1, NegNumTerm _ _ _ _ j) -> Just (pevalMulNumTerm (conTerm $ -l1) j)
  (_, MulNumTerm _ _ _ _ _ ConTerm {}) -> error "Should not happen"
  (_, AddNumTerm _ _ _ _ _ ConTerm {}) -> error "Should not happen"
  _ -> doPevalDefaultMulNumTermNoCon l b
doPevalDefaultMulNumTerm a r@(ConTerm {}) = doPevalDefaultMulNumTerm r a
doPevalDefaultMulNumTerm l r = doPevalDefaultMulNumTermNoCon l r

doPevalDefaultMulNumTermNoCon ::
  (PEvalNumTerm a) => Term a -> Term a -> Maybe (Term a)
doPevalDefaultMulNumTermNoCon (MulNumTerm _ _ _ _ i@ConTerm {} j) k =
  Just $ pevalMulNumTerm i $ pevalMulNumTerm j k
doPevalDefaultMulNumTermNoCon i (MulNumTerm _ _ _ _ j@ConTerm {} k) =
  Just $ pevalMulNumTerm j $ pevalMulNumTerm i k
doPevalDefaultMulNumTermNoCon (NegNumTerm _ _ _ _ i) j =
  Just $ pevalNegNumTerm $ pevalMulNumTerm i j
doPevalDefaultMulNumTermNoCon i (NegNumTerm _ _ _ _ j) =
  Just $ pevalNegNumTerm $ pevalMulNumTerm i j
doPevalDefaultMulNumTermNoCon i j@ConTerm {} = Just $ pevalMulNumTerm j i
doPevalDefaultMulNumTermNoCon (MulNumTerm _ _ _ _ _ ConTerm {}) _ =
  error "Should not happen"
doPevalDefaultMulNumTermNoCon _ (MulNumTerm _ _ _ _ _ ConTerm {}) =
  error "Should not happen"
doPevalDefaultMulNumTermNoCon _ _ = Nothing

-- Abs
pevalBitsAbsNumTerm :: (PEvalNumTerm a, Bits a) => Term a -> Term a
pevalBitsAbsNumTerm l =
  introSupportedPrimConstraint l $
    unaryUnfoldOnce doPevalBitsAbsNumTerm absNumTerm l

doPevalGeneralAbsNumTerm :: (PEvalNumTerm a) => Term a -> Maybe (Term a)
doPevalGeneralAbsNumTerm (ConTerm _ _ _ _ a) = Just $ conTerm $ abs a
doPevalGeneralAbsNumTerm (NegNumTerm _ _ _ _ v) = Just $ pevalAbsNumTerm v
doPevalGeneralAbsNumTerm t@(AbsNumTerm {}) = Just t
doPevalGeneralAbsNumTerm _ = Nothing

doPevalBitsAbsNumTerm ::
  forall a. (PEvalNumTerm a, Bits a) => Term a -> Maybe (Term a)
doPevalBitsAbsNumTerm t =
  msum
    [ if isSigned (undefined :: a) then Nothing else Just t,
      doPevalGeneralAbsNumTerm t
    ]

doPevalNoOverflowAbsNumTerm :: (PEvalNumTerm a) => Term a -> Maybe (Term a)
doPevalNoOverflowAbsNumTerm t =
  msum
    [ doPevalGeneralAbsNumTerm t,
      case t of
        MulNumTerm _ _ _ _ l r ->
          Just $ pevalMulNumTerm (pevalAbsNumTerm l) $ pevalAbsNumTerm r
        _ -> Nothing
    ]

-- Signum

pevalGeneralSignumNumTerm :: (PEvalNumTerm a) => Term a -> Term a
pevalGeneralSignumNumTerm l =
  introSupportedPrimConstraint l $
    unaryUnfoldOnce doPevalGeneralSignumNumTerm signumNumTerm l

doPevalGeneralSignumNumTerm :: (PEvalNumTerm a) => Term a -> Maybe (Term a)
doPevalGeneralSignumNumTerm (ConTerm _ _ _ _ a) = Just $ conTerm $ signum a
doPevalGeneralSignumNumTerm _ = Nothing

doPevalNoOverflowSignumNumTerm :: (PEvalNumTerm a) => Term a -> Maybe (Term a)
doPevalNoOverflowSignumNumTerm t =
  msum
    [ doPevalGeneralSignumNumTerm t,
      case t of
        NegNumTerm _ _ _ _ v -> Just $ pevalNegNumTerm $ pevalSignumNumTerm v
        MulNumTerm _ _ _ _ l r ->
          Just $
            pevalMulNumTerm (pevalSignumNumTerm l) $
              pevalSignumNumTerm r
        _ -> Nothing
    ]

instance PEvalNumTerm Integer where
  pevalAddNumTerm = pevalDefaultAddNumTerm
  pevalNegNumTerm = pevalDefaultNegNumTerm
  pevalMulNumTerm = pevalDefaultMulNumTerm
  pevalAbsNumTerm = unaryUnfoldOnce doPevalNoOverflowAbsNumTerm absNumTerm
  pevalSignumNumTerm =
    unaryUnfoldOnce doPevalNoOverflowSignumNumTerm signumNumTerm
  withSbvNumTermConstraint r = r

instance (KnownNat n, 1 <= n) => PEvalNumTerm (WordN n) where
  pevalAddNumTerm = pevalDefaultAddNumTerm
  pevalNegNumTerm = pevalDefaultNegNumTerm
  pevalMulNumTerm = pevalDefaultMulNumTerm
  pevalAbsNumTerm = pevalBitsAbsNumTerm
  pevalSignumNumTerm = pevalGeneralSignumNumTerm
  withSbvNumTermConstraint r = withPrim @(WordN n) r

instance (KnownNat n, 1 <= n) => PEvalNumTerm (IntN n) where
  pevalAddNumTerm = pevalDefaultAddNumTerm
  pevalNegNumTerm = pevalDefaultNegNumTerm
  pevalMulNumTerm = pevalDefaultMulNumTerm
  pevalAbsNumTerm = pevalBitsAbsNumTerm
  pevalSignumNumTerm = pevalGeneralSignumNumTerm
  withSbvNumTermConstraint r = withPrim @(IntN n) r

instance (ValidFP eb sb) => PEvalNumTerm (FP eb sb) where
  pevalAddNumTerm = generalBinaryUnfolded (+) addNumTerm
  pevalNegNumTerm = generalUnaryUnfolded negate negNumTerm
  pevalMulNumTerm = generalBinaryUnfolded (*) mulNumTerm
  pevalAbsNumTerm = generalUnaryUnfolded abs absNumTerm
  pevalSignumNumTerm = generalUnaryUnfolded signum signumNumTerm
  withSbvNumTermConstraint r = withPrim @(FP eb sb) r

instance PEvalNumTerm AlgReal where
  pevalAddNumTerm = pevalDefaultAddNumTerm
  pevalNegNumTerm = pevalDefaultNegNumTerm
  pevalMulNumTerm = pevalDefaultMulNumTerm
  pevalAbsNumTerm = unaryUnfoldOnce doPevalNoOverflowAbsNumTerm absNumTerm
  pevalSignumNumTerm =
    unaryUnfoldOnce doPevalNoOverflowSignumNumTerm signumNumTerm
  withSbvNumTermConstraint r = withPrim @AlgReal r
