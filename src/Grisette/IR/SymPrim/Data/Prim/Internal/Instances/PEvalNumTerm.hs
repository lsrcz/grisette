{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.IR.SymPrim.Data.Prim.Internal.Instances.PEvalNumTerm
  ( pevalDefaultAddNumTerm,
    pevalDefaultNegNumTerm,
  )
where

import Control.Monad (msum)
import Data.Bits (Bits)
import Data.SBV (Bits (isSigned))
import GHC.TypeLits (KnownNat, type (<=))
import Grisette.Core.Data.BV (IntN, WordN)
import Grisette.IR.SymPrim.Data.Prim.Internal.Instances.SupportedPrim ()
import Grisette.IR.SymPrim.Data.Prim.Internal.Term
  ( PEvalNumTerm
      ( pevalAbsNumTerm,
        pevalAddNumTerm,
        pevalMulNumTerm,
        pevalNegNumTerm,
        pevalSignumNumTerm
      ),
    Term (AbsNumTerm, AddNumTerm, ConTerm, MulNumTerm, NegNumTerm),
    absNumTerm,
    addNumTerm,
    conTerm,
    mulNumTerm,
    negNumTerm,
    pevalSubNumTerm,
    signumNumTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Unfold
  ( binaryUnfoldOnce,
    unaryUnfoldOnce,
  )

-- Add
pevalDefaultAddNumTerm :: (PEvalNumTerm a) => Term a -> Term a -> Term a
pevalDefaultAddNumTerm =
  binaryUnfoldOnce
    doPevalDefaultAddNumTerm
    (\a b -> normalizeAddNum $ addNumTerm a b)

doPevalDefaultAddNumTerm ::
  (PEvalNumTerm a) => Term a -> Term a -> Maybe (Term a)
doPevalDefaultAddNumTerm (ConTerm _ a) (ConTerm _ b) = Just $ conTerm $ a + b
doPevalDefaultAddNumTerm l@(ConTerm _ a) b = case (a, b) of
  (0, k) -> Just k
  (l1, AddNumTerm _ (ConTerm _ j) k) ->
    Just $ pevalAddNumTerm (conTerm $ l1 + j) k
  _ -> doPevalDefaultAddNumTermNoCon l b
doPevalDefaultAddNumTerm a r@(ConTerm _ _) = doPevalDefaultAddNumTerm r a
doPevalDefaultAddNumTerm l r = doPevalDefaultAddNumTermNoCon l r

doPevalDefaultAddNumTermNoCon ::
  (PEvalNumTerm a) => Term a -> Term a -> Maybe (Term a)
doPevalDefaultAddNumTermNoCon (AddNumTerm _ i@ConTerm {} j) k =
  Just $ pevalAddNumTerm i $ pevalAddNumTerm j k
doPevalDefaultAddNumTermNoCon i (AddNumTerm _ j@ConTerm {} k) =
  Just $ pevalAddNumTerm j $ pevalAddNumTerm i k
doPevalDefaultAddNumTermNoCon (NegNumTerm _ i) (NegNumTerm _ j) =
  Just $ pevalNegNumTerm $ pevalAddNumTerm i j
doPevalDefaultAddNumTermNoCon
  (MulNumTerm _ (ConTerm _ i) j)
  (MulNumTerm _ (ConTerm _ k) l)
    | j == l = Just $ pevalMulNumTerm (conTerm $ i + k) j
doPevalDefaultAddNumTermNoCon
  (MulNumTerm _ i@ConTerm {} j)
  (MulNumTerm _ k@(ConTerm _ _) l)
    | i == k = Just $ pevalMulNumTerm i (pevalAddNumTerm j l)
doPevalDefaultAddNumTermNoCon _ _ = Nothing

normalizeAddNum :: (PEvalNumTerm a) => Term a -> Term a
normalizeAddNum (AddNumTerm _ l r@(ConTerm _ _)) = addNumTerm r l
normalizeAddNum v = v

-- Neg
pevalDefaultNegNumTerm :: (PEvalNumTerm a) => Term a -> Term a
pevalDefaultNegNumTerm = unaryUnfoldOnce doPevalDefaultNegNumTerm negNumTerm

doPevalDefaultNegNumTerm :: (PEvalNumTerm a) => Term a -> Maybe (Term a)
doPevalDefaultNegNumTerm (ConTerm _ a) = Just $ conTerm $ -a
doPevalDefaultNegNumTerm (NegNumTerm _ v) = Just v
doPevalDefaultNegNumTerm (AddNumTerm _ (ConTerm _ l) r) =
  Just $ pevalSubNumTerm (conTerm $ -l) r
doPevalDefaultNegNumTerm (AddNumTerm _ (NegNumTerm _ l) r) =
  Just $ pevalAddNumTerm l (pevalNegNumTerm r)
doPevalDefaultNegNumTerm (AddNumTerm _ l (NegNumTerm _ r)) =
  Just $ pevalAddNumTerm (pevalNegNumTerm l) r
doPevalDefaultNegNumTerm (MulNumTerm _ (ConTerm _ l) r) =
  Just $ pevalMulNumTerm (conTerm $ -l) r
doPevalDefaultNegNumTerm (MulNumTerm _ (NegNumTerm _ _) _) =
  error "Should not happen"
doPevalDefaultNegNumTerm (MulNumTerm _ _ (NegNumTerm _ _)) =
  error "Should not happen"
doPevalDefaultNegNumTerm (AddNumTerm _ _ ConTerm {}) = error "Should not happen"
doPevalDefaultNegNumTerm _ = Nothing

-- Mul
pevalDefaultMulNumTerm :: (PEvalNumTerm a) => Term a -> Term a -> Term a
pevalDefaultMulNumTerm =
  binaryUnfoldOnce
    doPevalDefaultMulNumTerm
    (\a b -> normalizeMulNum $ mulNumTerm a b)

normalizeMulNum :: (PEvalNumTerm a) => Term a -> Term a
normalizeMulNum (MulNumTerm _ l r@(ConTerm _ _)) = mulNumTerm r l
normalizeMulNum v = v

doPevalDefaultMulNumTerm ::
  (PEvalNumTerm a) => Term a -> Term a -> Maybe (Term a)
doPevalDefaultMulNumTerm (ConTerm _ a) (ConTerm _ b) = Just $ conTerm $ a * b
doPevalDefaultMulNumTerm l@(ConTerm _ a) b = case (a, b) of
  (0, _) -> Just $ conTerm 0
  (1, k) -> Just k
  (-1, k) -> Just $ pevalNegNumTerm k
  (l1, MulNumTerm _ (ConTerm _ j) k) ->
    Just $ pevalMulNumTerm (conTerm $ l1 * j) k
  (l1, AddNumTerm _ (ConTerm _ j) k) ->
    Just $ pevalAddNumTerm (conTerm $ l1 * j) (pevalMulNumTerm (conTerm l1) k)
  (l1, NegNumTerm _ j) -> Just (pevalMulNumTerm (conTerm $ -l1) j)
  (_, MulNumTerm _ _ ConTerm {}) -> error "Should not happen"
  (_, AddNumTerm _ _ ConTerm {}) -> error "Should not happen"
  _ -> doPevalDefaultMulNumTermNoCon l b
doPevalDefaultMulNumTerm a r@(ConTerm _ _) = doPevalDefaultMulNumTerm r a
doPevalDefaultMulNumTerm l r = doPevalDefaultMulNumTermNoCon l r

doPevalDefaultMulNumTermNoCon ::
  (PEvalNumTerm a) => Term a -> Term a -> Maybe (Term a)
doPevalDefaultMulNumTermNoCon (MulNumTerm _ i@ConTerm {} j) k =
  Just $ pevalMulNumTerm i $ pevalMulNumTerm j k
doPevalDefaultMulNumTermNoCon i (MulNumTerm _ j@ConTerm {} k) =
  Just $ pevalMulNumTerm j $ pevalMulNumTerm i k
doPevalDefaultMulNumTermNoCon (NegNumTerm _ i) j =
  Just $ pevalNegNumTerm $ pevalMulNumTerm i j
doPevalDefaultMulNumTermNoCon i (NegNumTerm _ j) =
  Just $ pevalNegNumTerm $ pevalMulNumTerm i j
doPevalDefaultMulNumTermNoCon i j@ConTerm {} = Just $ pevalMulNumTerm j i
doPevalDefaultMulNumTermNoCon (MulNumTerm _ _ ConTerm {}) _ =
  error "Should not happen"
doPevalDefaultMulNumTermNoCon _ (MulNumTerm _ _ ConTerm {}) =
  error "Should not happen"
doPevalDefaultMulNumTermNoCon _ _ = Nothing

-- Abs
pevalBitsAbsNumTerm :: (PEvalNumTerm a, Bits a) => Term a -> Term a
pevalBitsAbsNumTerm =
  unaryUnfoldOnce doPevalBitsAbsNumTerm absNumTerm

doPevalGeneralAbsNumTerm :: (PEvalNumTerm a) => Term a -> Maybe (Term a)
doPevalGeneralAbsNumTerm (ConTerm _ a) = Just $ conTerm $ abs a
doPevalGeneralAbsNumTerm (NegNumTerm _ v) = Just $ pevalAbsNumTerm v
doPevalGeneralAbsNumTerm t@(AbsNumTerm _ _) = Just t
doPevalGeneralAbsNumTerm _ = Nothing

doPevalBitsAbsNumTerm ::
  forall a. (PEvalNumTerm a, Bits a) => Term a -> Maybe (Term a)
doPevalBitsAbsNumTerm t =
  msum
    [ if isSigned (undefined :: a) then Nothing else Just t,
      doPevalGeneralAbsNumTerm t
    ]

-- Signum

pevalGeneralSignumNumTerm :: (PEvalNumTerm a) => Term a -> Term a
pevalGeneralSignumNumTerm =
  unaryUnfoldOnce doPevalGeneralSignumNumTerm signumNumTerm

doPevalGeneralSignumNumTerm :: (PEvalNumTerm a) => Term a -> Maybe (Term a)
doPevalGeneralSignumNumTerm (ConTerm _ a) = Just $ conTerm $ signum a
doPevalGeneralSignumNumTerm _ = Nothing

instance PEvalNumTerm Integer where
  pevalAddNumTerm = pevalDefaultAddNumTerm
  pevalNegNumTerm = pevalDefaultNegNumTerm
  pevalMulNumTerm = pevalDefaultMulNumTerm
  pevalAbsNumTerm = unaryUnfoldOnce doPevalIntegerAbsNumTerm absNumTerm
    where
      doPevalIntegerAbsNumTerm t =
        msum
          [ doPevalGeneralAbsNumTerm t,
            case t of
              MulNumTerm _ l r ->
                Just $ pevalMulNumTerm (pevalAbsNumTerm l) $ pevalAbsNumTerm r
              _ -> Nothing
          ]
  pevalSignumNumTerm = unaryUnfoldOnce doPevalIntegerSignumNumTerm signumNumTerm
    where
      doPevalIntegerSignumNumTerm t =
        msum
          [ doPevalGeneralSignumNumTerm t,
            case t of
              NegNumTerm _ v -> Just $ pevalNegNumTerm $ pevalSignumNumTerm v
              MulNumTerm _ l r ->
                Just $
                  pevalMulNumTerm (pevalSignumNumTerm l) $
                    pevalSignumNumTerm r
              _ -> Nothing
          ]

instance (KnownNat n, 1 <= n) => PEvalNumTerm (WordN n) where
  pevalAddNumTerm = pevalDefaultAddNumTerm
  pevalNegNumTerm = pevalDefaultNegNumTerm
  pevalMulNumTerm = pevalDefaultMulNumTerm
  pevalAbsNumTerm = pevalBitsAbsNumTerm
  pevalSignumNumTerm = pevalGeneralSignumNumTerm

instance (KnownNat n, 1 <= n) => PEvalNumTerm (IntN n) where
  pevalAddNumTerm = pevalDefaultAddNumTerm
  pevalNegNumTerm = pevalDefaultNegNumTerm
  pevalMulNumTerm = pevalDefaultMulNumTerm
  pevalAbsNumTerm = pevalBitsAbsNumTerm
  pevalSignumNumTerm = pevalGeneralSignumNumTerm
