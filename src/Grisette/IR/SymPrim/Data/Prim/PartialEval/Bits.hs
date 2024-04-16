{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.Prim.PartialEval.Bits
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.Prim.PartialEval.Bits
  ( pattern BitsConTerm,
    pevalShiftLeftTerm,
    pevalShiftRightTerm,
    pevalRotateLeftTerm,
    pevalRotateRightTerm,
  )
where

import Data.Bits
  ( Bits
      ( isSigned,
        rotateR,
        shiftR,
        zeroBits
      ),
    FiniteBits (finiteBitSize),
  )
import Data.Typeable (Typeable, cast)
import Grisette.Core.Data.Class.SymRotate (SymRotate (symRotate))
import Grisette.Core.Data.Class.SymShift (SymShift (symShift))
import Grisette.IR.SymPrim.Data.Prim.Internal.Term
  ( SupportedPrim,
    Term (ConTerm),
    conTerm,
    rotateLeftTerm,
    rotateRightTerm,
    shiftLeftTerm,
    shiftRightTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Unfold
  ( unaryUnfoldOnce,
  )

bitsConTermView :: (Bits b, Typeable b) => Term a -> Maybe b
bitsConTermView (ConTerm _ b) = cast b
bitsConTermView _ = Nothing

pattern BitsConTerm :: forall b a. (Bits b, Typeable b) => b -> Term a
pattern BitsConTerm b <- (bitsConTermView -> Just b)

-- shift
pevalShiftLeftTerm :: forall a. (Integral a, SymShift a, FiniteBits a, SupportedPrim a, SupportedPrim a) => Term a -> Term a -> Term a
pevalShiftLeftTerm t n = unaryUnfoldOnce (`doPevalShiftLeftTerm` n) (`shiftLeftTerm` n) t

doPevalShiftLeftTerm :: forall a. (Integral a, SymShift a, FiniteBits a, SupportedPrim a) => Term a -> Term a -> Maybe (Term a)
doPevalShiftLeftTerm (ConTerm _ a) (ConTerm _ n)
  | n >= 0 =
      if (fromIntegral n :: Integer) >= fromIntegral (finiteBitSize n)
        then Just $ conTerm zeroBits
        else Just $ conTerm $ symShift a n
doPevalShiftLeftTerm x (ConTerm _ 0) = Just x
-- TODO: Need to handle the overflow case.
-- doPevalShiftLeftTerm (ShiftLeftTerm _ x (ConTerm _ n)) (ConTerm _ n1)
--   | n >= 0 && n1 >= 0 = Just $ pevalShiftLeftTerm x (conTerm $ n + n1)
doPevalShiftLeftTerm _ (ConTerm _ n)
  | n >= 0 && (fromIntegral n :: Integer) >= fromIntegral (finiteBitSize n) =
      Just $ conTerm zeroBits
doPevalShiftLeftTerm _ _ = Nothing

pevalShiftRightTerm :: forall a. (Integral a, SymShift a, FiniteBits a, SupportedPrim a, SupportedPrim a) => Term a -> Term a -> Term a
pevalShiftRightTerm t n = unaryUnfoldOnce (`doPevalShiftRightTerm` n) (`shiftRightTerm` n) t

doPevalShiftRightTerm :: forall a. (Integral a, SymShift a, FiniteBits a, SupportedPrim a) => Term a -> Term a -> Maybe (Term a)
doPevalShiftRightTerm (ConTerm _ a) (ConTerm _ n)
  | n >= 0 && not (isSigned a) =
      if (fromIntegral n :: Integer) >= fromIntegral (finiteBitSize n)
        then Just $ conTerm zeroBits
        else Just $ conTerm $ shiftR a (fromIntegral n)
doPevalShiftRightTerm (ConTerm _ a) (ConTerm _ n)
  | n >= 0 = Just $ conTerm $ symShift a (-n) -- if n >= 0 then -n must be in the range
doPevalShiftRightTerm x (ConTerm _ 0) = Just x
-- doPevalShiftRightTerm (ShiftRightTerm _ x (ConTerm _ n)) (ConTerm _ n1)
--   | n >= 0 && n1 >= 0 = Just $ pevalShiftRightTerm x (conTerm $ n + n1)
doPevalShiftRightTerm _ (ConTerm _ n)
  | not (isSigned n)
      && (fromIntegral n :: Integer) >= fromIntegral (finiteBitSize n) =
      Just $ conTerm zeroBits
doPevalShiftRightTerm _ _ = Nothing

pevalRotateLeftTerm :: forall a. (Integral a, SymRotate a, FiniteBits a, SupportedPrim a, SupportedPrim a) => Term a -> Term a -> Term a
pevalRotateLeftTerm t n = unaryUnfoldOnce (`doPevalRotateLeftTerm` n) (`rotateLeftTerm` n) t

doPevalRotateLeftTerm :: forall a. (Integral a, SymRotate a, FiniteBits a, SupportedPrim a, SupportedPrim a) => Term a -> Term a -> Maybe (Term a)
doPevalRotateLeftTerm (ConTerm _ a) (ConTerm _ n)
  | n >= 0 = Just $ conTerm $ symRotate a n -- Just $ conTerm $ rotateL a (fromIntegral n)
doPevalRotateLeftTerm x (ConTerm _ 0) = Just x
-- doPevalRotateLeftTerm (RotateLeftTerm _ x (ConTerm _ n)) (ConTerm _ n1)
--   | n >= 0 && n1 >= 0 = Just $ pevalRotateLeftTerm x (conTerm $ n + n1)
doPevalRotateLeftTerm x (ConTerm _ n)
  | n >= 0 && (fromIntegral n :: Integer) >= fromIntegral bs =
      Just $ pevalRotateLeftTerm x (conTerm $ n `mod` fromIntegral bs)
  where
    bs = finiteBitSize n
doPevalRotateLeftTerm _ _ = Nothing

pevalRotateRightTerm :: forall a. (Integral a, SymRotate a, FiniteBits a, SupportedPrim a, SupportedPrim a) => Term a -> Term a -> Term a
pevalRotateRightTerm t n = unaryUnfoldOnce (`doPevalRotateRightTerm` n) (`rotateRightTerm` n) t

doPevalRotateRightTerm :: forall a. (Integral a, SymRotate a, FiniteBits a, SupportedPrim a, SupportedPrim a) => Term a -> Term a -> Maybe (Term a)
doPevalRotateRightTerm (ConTerm _ a) (ConTerm _ n)
  | n >= 0 =
      Just . conTerm $
        rotateR
          a
          ( fromIntegral $
              (fromIntegral n :: Integer)
                `mod` fromIntegral (finiteBitSize n)
          )
doPevalRotateRightTerm x (ConTerm _ 0) = Just x
-- doPevalRotateRightTerm (RotateRightTerm _ x (ConTerm _ n)) (ConTerm _ n1)
--   | n >= 0 && n1 >= 0 = Just $ pevalRotateRightTerm x (conTerm $ n + n1)
doPevalRotateRightTerm x (ConTerm _ n)
  | n >= 0 && (fromIntegral n :: Integer) >= fromIntegral bs =
      Just $ pevalRotateRightTerm x (conTerm $ n `mod` fromIntegral bs)
  where
    bs = finiteBitSize n
doPevalRotateRightTerm _ _ = Nothing
