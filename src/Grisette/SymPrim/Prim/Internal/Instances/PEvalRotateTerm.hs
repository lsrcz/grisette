{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.SymPrim.Prim.Internal.Instances.PEvalRotateTerm
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.SymPrim.Prim.Internal.Instances.PEvalRotateTerm
  ( pevalFiniteBitsSymRotateRotateLeftTerm,
    pevalFiniteBitsSymRotateRotateRightTerm,
  )
where

import Data.Bits (Bits (rotateR), FiniteBits (finiteBitSize))
import Data.Proxy (Proxy (Proxy))
import qualified Data.SBV as SBV
import GHC.TypeLits (KnownNat, type (<=))
import Grisette.Core.Data.BV (IntN, WordN)
import Grisette.Core.Data.Class.SymRotate (SymRotate (symRotate))
import Grisette.SymPrim.Prim.Internal.Instances.SupportedPrim (bvIsNonZeroFromGEq1)
import Grisette.SymPrim.Prim.Internal.Term
  ( PEvalRotateTerm (pevalRotateLeftTerm, pevalRotateRightTerm, sbvRotateLeftTerm, sbvRotateRightTerm, withSbvRotateTermConstraint),
    SupportedNonFuncPrim (withNonFuncPrim),
    Term (ConTerm),
    conTerm,
    rotateLeftTerm,
    rotateRightTerm,
  )
import Grisette.SymPrim.Prim.Internal.Unfold (unaryUnfoldOnce)

pevalFiniteBitsSymRotateRotateLeftTerm ::
  forall a.
  (Integral a, SymRotate a, FiniteBits a, PEvalRotateTerm a) =>
  Term a ->
  Term a ->
  Term a
pevalFiniteBitsSymRotateRotateLeftTerm t n =
  unaryUnfoldOnce
    (`doPevalFiniteBitsSymRotateRotateLeftTerm` n)
    (`rotateLeftTerm` n)
    t

doPevalFiniteBitsSymRotateRotateLeftTerm ::
  forall a.
  (Integral a, SymRotate a, FiniteBits a, PEvalRotateTerm a) =>
  Term a ->
  Term a ->
  Maybe (Term a)
doPevalFiniteBitsSymRotateRotateLeftTerm (ConTerm _ a) (ConTerm _ n)
  | n >= 0 = Just $ conTerm $ symRotate a n -- Just $ conTerm $ rotateL a (fromIntegral n)
doPevalFiniteBitsSymRotateRotateLeftTerm x (ConTerm _ 0) = Just x
-- doPevalFiniteBitsSymRotateRotateLeftTerm (RotateLeftTerm _ x (ConTerm _ n)) (ConTerm _ n1)
--   | n >= 0 && n1 >= 0 = Just $ pevalFiniteBitsSymRotateRotateLeftTerm x (conTerm $ n + n1)
doPevalFiniteBitsSymRotateRotateLeftTerm x (ConTerm _ n)
  | n >= 0 && (fromIntegral n :: Integer) >= fromIntegral bs =
      Just $
        pevalFiniteBitsSymRotateRotateLeftTerm
          x
          (conTerm $ n `mod` fromIntegral bs)
  where
    bs = finiteBitSize n
doPevalFiniteBitsSymRotateRotateLeftTerm _ _ = Nothing

pevalFiniteBitsSymRotateRotateRightTerm ::
  forall a.
  (Integral a, SymRotate a, FiniteBits a, PEvalRotateTerm a) =>
  Term a ->
  Term a ->
  Term a
pevalFiniteBitsSymRotateRotateRightTerm t n =
  unaryUnfoldOnce
    (`doPevalFiniteBitsSymRotateRotateRightTerm` n)
    (`rotateRightTerm` n)
    t

doPevalFiniteBitsSymRotateRotateRightTerm ::
  forall a.
  (Integral a, SymRotate a, FiniteBits a, PEvalRotateTerm a) =>
  Term a ->
  Term a ->
  Maybe (Term a)
doPevalFiniteBitsSymRotateRotateRightTerm (ConTerm _ a) (ConTerm _ n)
  | n >= 0 =
      Just . conTerm $
        rotateR
          a
          ( fromIntegral $
              (fromIntegral n :: Integer)
                `mod` fromIntegral (finiteBitSize n)
          )
doPevalFiniteBitsSymRotateRotateRightTerm x (ConTerm _ 0) = Just x
-- doPevalFiniteBitsSymRotateRotateRightTerm (RotateRightTerm _ x (ConTerm _ n)) (ConTerm _ n1)
--   | n >= 0 && n1 >= 0 = Just $ pevalFiniteBitsSymRotateRotateRightTerm x (conTerm $ n + n1)
doPevalFiniteBitsSymRotateRotateRightTerm x (ConTerm _ n)
  | n >= 0 && (fromIntegral n :: Integer) >= fromIntegral bs =
      Just $
        pevalFiniteBitsSymRotateRotateRightTerm
          x
          (conTerm $ n `mod` fromIntegral bs)
  where
    bs = finiteBitSize n
doPevalFiniteBitsSymRotateRotateRightTerm _ _ = Nothing

instance (KnownNat n, 1 <= n) => PEvalRotateTerm (IntN n) where
  pevalRotateLeftTerm = pevalFiniteBitsSymRotateRotateLeftTerm
  pevalRotateRightTerm = pevalFiniteBitsSymRotateRotateRightTerm
  withSbvRotateTermConstraint p r =
    bvIsNonZeroFromGEq1 (Proxy @n) $
      withNonFuncPrim @(IntN n) p r

  -- SBV's rotateLeft and rotateRight are broken for signed values, so we have to
  -- do this
  -- https://github.com/LeventErkok/sbv/issues/673
  sbvRotateLeftTerm p l r =
    withNonFuncPrim @(IntN n) p $
      withSbvRotateTermConstraint @(IntN n) p $
        SBV.sFromIntegral $
          SBV.sRotateLeft
            (SBV.sFromIntegral l :: SBV.SWord n)
            (SBV.sFromIntegral r :: SBV.SWord n)
  sbvRotateRightTerm p l r =
    withNonFuncPrim @(IntN n) p $
      withSbvRotateTermConstraint @(IntN n) p $
        SBV.sFromIntegral $
          SBV.sRotateRight
            (SBV.sFromIntegral l :: SBV.SWord n)
            (SBV.sFromIntegral r :: SBV.SWord n)

instance (KnownNat n, 1 <= n) => PEvalRotateTerm (WordN n) where
  pevalRotateLeftTerm = pevalFiniteBitsSymRotateRotateLeftTerm
  pevalRotateRightTerm = pevalFiniteBitsSymRotateRotateRightTerm
  withSbvRotateTermConstraint p r =
    bvIsNonZeroFromGEq1 (Proxy @n) $
      withNonFuncPrim @(WordN n) p r
