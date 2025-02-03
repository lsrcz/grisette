{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
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
import Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalBitwiseTerm ()
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
    Term,
    absNumTerm,
    addNumTerm,
    conTerm,
    mulNumTerm,
    negNumTerm,
    pevalSubNumTerm,
    signumNumTerm,
    pattern AbsNumTerm,
    pattern AddNumTerm,
    pattern ConTerm,
    pattern MulNumTerm,
    pattern NegNumTerm,
    pattern SupportedTerm,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Unfold
  ( binaryUnfoldOnce,
    generalBinaryUnfolded,
    generalUnaryUnfolded,
    unaryUnfoldOnce,
  )

-- | Default partial evaluation of addition of numerical terms.
pevalDefaultAddNumTerm :: (PEvalNumTerm a, Eq a) => Term a -> Term a -> Term a
pevalDefaultAddNumTerm l@SupportedTerm r =
  binaryUnfoldOnce
    doPevalDefaultAddNumTerm
    (\a b -> normalizeAddNum $ addNumTerm a b)
    l
    r

doPevalDefaultAddNumTerm ::
  (PEvalNumTerm a, Eq a) => Term a -> Term a -> Maybe (Term a)
doPevalDefaultAddNumTerm (ConTerm a) (ConTerm b) = Just $ conTerm $ a + b
doPevalDefaultAddNumTerm l@(ConTerm a) b = case (a, b) of
  (0, k) -> Just k
  (l1, AddNumTerm (ConTerm j) k) ->
    Just $ pevalAddNumTerm (conTerm $ l1 + j) k
  _ -> doPevalDefaultAddNumTermNoCon l b
doPevalDefaultAddNumTerm a r@(ConTerm {}) = doPevalDefaultAddNumTerm r a
doPevalDefaultAddNumTerm l r = doPevalDefaultAddNumTermNoCon l r

doPevalDefaultAddNumTermNoCon ::
  (PEvalNumTerm a) => Term a -> Term a -> Maybe (Term a)
doPevalDefaultAddNumTermNoCon (AddNumTerm i@ConTerm {} j) k =
  Just $ pevalAddNumTerm i $ pevalAddNumTerm j k
doPevalDefaultAddNumTermNoCon i (AddNumTerm j@ConTerm {} k) =
  Just $ pevalAddNumTerm j $ pevalAddNumTerm i k
doPevalDefaultAddNumTermNoCon (NegNumTerm i) (NegNumTerm j) =
  Just $ pevalNegNumTerm $ pevalAddNumTerm i j
doPevalDefaultAddNumTermNoCon
  (MulNumTerm (ConTerm i) j)
  (MulNumTerm (ConTerm k) l)
    | j == l = Just $ pevalMulNumTerm (conTerm $ i + k) j
doPevalDefaultAddNumTermNoCon
  (MulNumTerm i@ConTerm {} j)
  (MulNumTerm k@(ConTerm {}) l)
    | i == k = Just $ pevalMulNumTerm i (pevalAddNumTerm j l)
doPevalDefaultAddNumTermNoCon _ _ = Nothing

normalizeAddNum :: (PEvalNumTerm a) => Term a -> Term a
normalizeAddNum (AddNumTerm l r@(ConTerm {})) = addNumTerm r l
normalizeAddNum v = v

-- | Default partial evaluation of negation of numerical terms.
pevalDefaultNegNumTerm :: (PEvalNumTerm a, Eq a) => Term a -> Term a
pevalDefaultNegNumTerm l@SupportedTerm =
  unaryUnfoldOnce doPevalDefaultNegNumTerm negNumTerm l

doPevalDefaultNegNumTerm :: (PEvalNumTerm a) => Term a -> Maybe (Term a)
doPevalDefaultNegNumTerm (ConTerm a) = Just $ conTerm $ -a
doPevalDefaultNegNumTerm (NegNumTerm v) = Just v
doPevalDefaultNegNumTerm (AddNumTerm (ConTerm l) r) =
  Just $ pevalSubNumTerm (conTerm $ -l) r
doPevalDefaultNegNumTerm (AddNumTerm (NegNumTerm l) r) =
  Just $ pevalAddNumTerm l (pevalNegNumTerm r)
doPevalDefaultNegNumTerm (AddNumTerm l (NegNumTerm r)) =
  Just $ pevalAddNumTerm (pevalNegNumTerm l) r
doPevalDefaultNegNumTerm (MulNumTerm (ConTerm l) r) =
  Just $ pevalMulNumTerm (conTerm $ -l) r
doPevalDefaultNegNumTerm (MulNumTerm (NegNumTerm {}) _) =
  error "Should not happen"
doPevalDefaultNegNumTerm (MulNumTerm _ (NegNumTerm {})) =
  error "Should not happen"
doPevalDefaultNegNumTerm (AddNumTerm _ ConTerm {}) = error "Should not happen"
doPevalDefaultNegNumTerm _ = Nothing

-- Mul
pevalDefaultMulNumTerm :: (PEvalNumTerm a, Eq a) => Term a -> Term a -> Term a
pevalDefaultMulNumTerm l@SupportedTerm r =
  binaryUnfoldOnce
    doPevalDefaultMulNumTerm
    (\a b -> normalizeMulNum $ mulNumTerm a b)
    l
    r

normalizeMulNum :: (PEvalNumTerm a) => Term a -> Term a
normalizeMulNum (MulNumTerm l r@(ConTerm {})) = mulNumTerm r l
normalizeMulNum v = v

doPevalDefaultMulNumTerm ::
  (PEvalNumTerm a, Eq a) => Term a -> Term a -> Maybe (Term a)
doPevalDefaultMulNumTerm (ConTerm a) (ConTerm b) =
  Just $ conTerm $ a * b
doPevalDefaultMulNumTerm l@(ConTerm a) b = case (a, b) of
  (0, _) -> Just $ conTerm 0
  (1, k) -> Just k
  (-1, k) -> Just $ pevalNegNumTerm k
  (l1, MulNumTerm (ConTerm j) k) ->
    Just $ pevalMulNumTerm (conTerm $ l1 * j) k
  (l1, AddNumTerm (ConTerm j) k) ->
    Just $ pevalAddNumTerm (conTerm $ l1 * j) (pevalMulNumTerm (conTerm l1) k)
  (l1, NegNumTerm j) -> Just (pevalMulNumTerm (conTerm $ -l1) j)
  (_, MulNumTerm _ ConTerm {}) -> error "Should not happen"
  (_, AddNumTerm _ ConTerm {}) -> error "Should not happen"
  _ -> doPevalDefaultMulNumTermNoCon l b
doPevalDefaultMulNumTerm a r@(ConTerm {}) = doPevalDefaultMulNumTerm r a
doPevalDefaultMulNumTerm l r = doPevalDefaultMulNumTermNoCon l r

doPevalDefaultMulNumTermNoCon ::
  (PEvalNumTerm a) => Term a -> Term a -> Maybe (Term a)
doPevalDefaultMulNumTermNoCon (MulNumTerm i@ConTerm {} j) k =
  Just $ pevalMulNumTerm i $ pevalMulNumTerm j k
doPevalDefaultMulNumTermNoCon i (MulNumTerm j@ConTerm {} k) =
  Just $ pevalMulNumTerm j $ pevalMulNumTerm i k
doPevalDefaultMulNumTermNoCon (NegNumTerm i) j =
  Just $ pevalNegNumTerm $ pevalMulNumTerm i j
doPevalDefaultMulNumTermNoCon i (NegNumTerm j) =
  Just $ pevalNegNumTerm $ pevalMulNumTerm i j
doPevalDefaultMulNumTermNoCon i j@ConTerm {} = Just $ pevalMulNumTerm j i
doPevalDefaultMulNumTermNoCon (MulNumTerm _ ConTerm {}) _ =
  error "Should not happen"
doPevalDefaultMulNumTermNoCon _ (MulNumTerm _ ConTerm {}) =
  error "Should not happen"
doPevalDefaultMulNumTermNoCon _ _ = Nothing

-- Abs
pevalBitsAbsNumTerm :: (PEvalNumTerm a, Bits a) => Term a -> Term a
pevalBitsAbsNumTerm l@SupportedTerm =
  unaryUnfoldOnce doPevalBitsAbsNumTerm absNumTerm l

doPevalGeneralAbsNumTerm :: (PEvalNumTerm a) => Term a -> Maybe (Term a)
doPevalGeneralAbsNumTerm (ConTerm a) = Just $ conTerm $ abs a
doPevalGeneralAbsNumTerm (NegNumTerm v) = Just $ pevalAbsNumTerm v
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
        MulNumTerm l r ->
          Just $ pevalMulNumTerm (pevalAbsNumTerm l) $ pevalAbsNumTerm r
        _ -> Nothing
    ]

-- Signum

pevalGeneralSignumNumTerm :: (PEvalNumTerm a) => Term a -> Term a
pevalGeneralSignumNumTerm l@SupportedTerm =
  unaryUnfoldOnce doPevalGeneralSignumNumTerm signumNumTerm l

doPevalGeneralSignumNumTerm :: (PEvalNumTerm a) => Term a -> Maybe (Term a)
doPevalGeneralSignumNumTerm (ConTerm a) = Just $ conTerm $ signum a
doPevalGeneralSignumNumTerm _ = Nothing

doPevalNoOverflowSignumNumTerm :: (PEvalNumTerm a) => Term a -> Maybe (Term a)
doPevalNoOverflowSignumNumTerm t =
  msum
    [ doPevalGeneralSignumNumTerm t,
      case t of
        NegNumTerm v -> Just $ pevalNegNumTerm $ pevalSignumNumTerm v
        MulNumTerm l r ->
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
