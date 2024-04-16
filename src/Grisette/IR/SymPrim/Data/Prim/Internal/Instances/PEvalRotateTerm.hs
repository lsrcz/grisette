{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.Prim.Internal.Instances.PEvalRotateTerm
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.Prim.Internal.Instances.PEvalRotateTerm
  ( pevalFiniteBitsSymRotateRotateLeftTerm,
    pevalFiniteBitsSymRotateRotateRightTerm,
  )
where

import Data.Bits (Bits (rotateR), FiniteBits (finiteBitSize))
import GHC.TypeLits (KnownNat, type (<=))
import Grisette.Core.Data.BV (IntN, WordN)
import Grisette.Core.Data.Class.SymRotate (SymRotate (symRotate))
import Grisette.IR.SymPrim.Data.Prim.Internal.Instances.SupportedPrim ()
import Grisette.IR.SymPrim.Data.Prim.Internal.Term
  ( PEvalRotateTerm (pevalRotateLeftTerm, pevalRotateRightTerm),
    Term (ConTerm),
    conTerm,
    rotateLeftTerm,
    rotateRightTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.Internal.Unfold (unaryUnfoldOnce)

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

instance (KnownNat n, 1 <= n) => PEvalRotateTerm (WordN n) where
  pevalRotateLeftTerm = pevalFiniteBitsSymRotateRotateLeftTerm
  pevalRotateRightTerm = pevalFiniteBitsSymRotateRotateRightTerm
