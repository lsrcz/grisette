{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalShiftTerm
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalShiftTerm
  ( pevalFiniteBitsSymShiftShiftLeftTerm,
    pevalFiniteBitsSymShiftShiftRightTerm,
  )
where

import Data.Bits (Bits (isSigned, shiftR, zeroBits), FiniteBits (finiteBitSize))
import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (KnownNat, type (<=))
import Grisette.Internal.Core.Data.Class.SymShift (SymShift (symShift))
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.Prim.Internal.Instances.SupportedPrim (bvIsNonZeroFromGEq1)
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( PEvalShiftTerm (pevalShiftLeftTerm, pevalShiftRightTerm, withSbvShiftTermConstraint),
    SupportedNonFuncPrim (withNonFuncPrim),
    SupportedPrim,
    Term (ConTerm),
    conTerm,
    shiftLeftTerm,
    shiftRightTerm,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Unfold (unaryUnfoldOnce)

pevalFiniteBitsSymShiftShiftLeftTerm ::
  forall a.
  (Integral a, SymShift a, FiniteBits a, PEvalShiftTerm a) =>
  Term a ->
  Term a ->
  Term a
pevalFiniteBitsSymShiftShiftLeftTerm t n =
  unaryUnfoldOnce
    (`doPevalFiniteBitsSymShiftShiftLeftTerm` n)
    (`shiftLeftTerm` n)
    t

doPevalFiniteBitsSymShiftShiftLeftTerm ::
  forall a.
  (Integral a, SymShift a, FiniteBits a, SupportedPrim a) =>
  Term a ->
  Term a ->
  Maybe (Term a)
doPevalFiniteBitsSymShiftShiftLeftTerm (ConTerm _ a) (ConTerm _ n)
  | n >= 0 =
      if (fromIntegral n :: Integer) >= fromIntegral (finiteBitSize n)
        then Just $ conTerm zeroBits
        else Just $ conTerm $ symShift a n
doPevalFiniteBitsSymShiftShiftLeftTerm x (ConTerm _ 0) = Just x
-- TODO: Need to handle the overflow case.
-- doPevalShiftLeftTerm (ShiftLeftTerm _ x (ConTerm _ n)) (ConTerm _ n1)
--   | n >= 0 && n1 >= 0 = Just $ pevalShiftLeftTerm x (conTerm $ n + n1)
doPevalFiniteBitsSymShiftShiftLeftTerm _ (ConTerm _ n)
  | n >= 0 && (fromIntegral n :: Integer) >= fromIntegral (finiteBitSize n) =
      Just $ conTerm zeroBits
doPevalFiniteBitsSymShiftShiftLeftTerm _ _ = Nothing

pevalFiniteBitsSymShiftShiftRightTerm ::
  forall a.
  (Integral a, SymShift a, FiniteBits a, PEvalShiftTerm a) =>
  Term a ->
  Term a ->
  Term a
pevalFiniteBitsSymShiftShiftRightTerm t n =
  unaryUnfoldOnce
    (`doPevalFiniteBitsSymShiftShiftRightTerm` n)
    (`shiftRightTerm` n)
    t

doPevalFiniteBitsSymShiftShiftRightTerm ::
  forall a.
  (Integral a, SymShift a, FiniteBits a, SupportedPrim a) =>
  Term a ->
  Term a ->
  Maybe (Term a)
doPevalFiniteBitsSymShiftShiftRightTerm (ConTerm _ a) (ConTerm _ n)
  | n >= 0 && not (isSigned a) =
      if (fromIntegral n :: Integer) >= fromIntegral (finiteBitSize n)
        then Just $ conTerm zeroBits
        else Just $ conTerm $ shiftR a (fromIntegral n)
doPevalFiniteBitsSymShiftShiftRightTerm (ConTerm _ a) (ConTerm _ n)
  -- if n >= 0 then -n must be in the range
  | n >= 0 = Just $ conTerm $ symShift a (-n)
doPevalFiniteBitsSymShiftShiftRightTerm x (ConTerm _ 0) = Just x
-- doPevalFiniteBitsSymShiftShiftRightTerm (ShiftRightTerm _ x (ConTerm _ n)) (ConTerm _ n1)
--   | n >= 0 && n1 >= 0 = Just $ pevalFiniteBitsSymShiftShiftRightTerm x (conTerm $ n + n1)
doPevalFiniteBitsSymShiftShiftRightTerm _ (ConTerm _ n)
  | not (isSigned n)
      && (fromIntegral n :: Integer) >= fromIntegral (finiteBitSize n) =
      Just $ conTerm zeroBits
doPevalFiniteBitsSymShiftShiftRightTerm _ _ = Nothing

instance (KnownNat n, 1 <= n) => PEvalShiftTerm (IntN n) where
  pevalShiftLeftTerm = pevalFiniteBitsSymShiftShiftLeftTerm
  pevalShiftRightTerm = pevalFiniteBitsSymShiftShiftRightTerm
  withSbvShiftTermConstraint p r =
    bvIsNonZeroFromGEq1 (Proxy @n) $
      withNonFuncPrim @(IntN n) p r

instance (KnownNat n, 1 <= n) => PEvalShiftTerm (WordN n) where
  pevalShiftLeftTerm = pevalFiniteBitsSymShiftShiftLeftTerm
  pevalShiftRightTerm = pevalFiniteBitsSymShiftShiftRightTerm
  withSbvShiftTermConstraint p r =
    bvIsNonZeroFromGEq1 (Proxy @n) $
      withNonFuncPrim @(WordN n) p r
