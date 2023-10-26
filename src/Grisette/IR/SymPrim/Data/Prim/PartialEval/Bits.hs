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
    pevalAndBitsTerm,
    pevalOrBitsTerm,
    pevalXorBitsTerm,
    pevalComplementBitsTerm,
    pevalShiftLeftTerm,
    pevalShiftRightTerm,
    pevalRotateLeftTerm,
    pevalRotateRightTerm,
  )
where

import Data.Bits
  ( Bits
      ( bitSizeMaybe,
        complement,
        isSigned,
        rotateL,
        rotateR,
        shiftR,
        xor,
        zeroBits,
        (.&.),
        (.|.)
      ),
    FiniteBits (finiteBitSize),
  )
import Data.Typeable (Typeable, cast)
import Grisette.Core.Data.Class.SymShift (SymShift (symShift))
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
  ( andBitsTerm,
    complementBitsTerm,
    conTerm,
    orBitsTerm,
    rotateLeftTerm,
    rotateRightTerm,
    shiftLeftTerm,
    shiftRightTerm,
    xorBitsTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
  ( SupportedPrim,
    Term
      ( ComplementBitsTerm,
        ConTerm
      ),
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Unfold
  ( binaryUnfoldOnce,
    unaryUnfoldOnce,
  )

bitsConTermView :: (Bits b, Typeable b) => Term a -> Maybe b
bitsConTermView (ConTerm _ b) = cast b
bitsConTermView _ = Nothing

pattern BitsConTerm :: forall b a. (Bits b, Typeable b) => b -> Term a
pattern BitsConTerm b <- (bitsConTermView -> Just b)

-- bitand
pevalAndBitsTerm :: forall a. (Bits a, SupportedPrim a) => Term a -> Term a -> Term a
pevalAndBitsTerm = binaryUnfoldOnce doPevalAndBitsTerm andBitsTerm

doPevalAndBitsTerm :: forall a. (Bits a, SupportedPrim a) => Term a -> Term a -> Maybe (Term a)
doPevalAndBitsTerm (ConTerm _ a) (ConTerm _ b) = Just $ conTerm (a .&. b)
doPevalAndBitsTerm (ConTerm _ a) b
  | a == zeroBits = Just $ conTerm zeroBits
  | a == complement zeroBits = Just b
doPevalAndBitsTerm a (ConTerm _ b)
  | b == zeroBits = Just $ conTerm zeroBits
  | b == complement zeroBits = Just a
doPevalAndBitsTerm a b | a == b = Just a
doPevalAndBitsTerm _ _ = Nothing

-- bitor
pevalOrBitsTerm :: forall a. (Bits a, SupportedPrim a) => Term a -> Term a -> Term a
pevalOrBitsTerm = binaryUnfoldOnce doPevalOrBitsTerm orBitsTerm

doPevalOrBitsTerm :: forall a. (Bits a, SupportedPrim a) => Term a -> Term a -> Maybe (Term a)
doPevalOrBitsTerm (ConTerm _ a) (ConTerm _ b) = Just $ conTerm (a .|. b)
doPevalOrBitsTerm (ConTerm _ a) b
  | a == zeroBits = Just b
  | a == complement zeroBits = Just $ conTerm $ complement zeroBits
doPevalOrBitsTerm a (ConTerm _ b)
  | b == zeroBits = Just a
  | b == complement zeroBits = Just $ conTerm $ complement zeroBits
doPevalOrBitsTerm a b | a == b = Just a
doPevalOrBitsTerm _ _ = Nothing

-- bitxor
pevalXorBitsTerm :: forall a. (Bits a, SupportedPrim a) => Term a -> Term a -> Term a
pevalXorBitsTerm = binaryUnfoldOnce doPevalXorBitsTerm xorBitsTerm

doPevalXorBitsTerm :: forall a. (Bits a, SupportedPrim a) => Term a -> Term a -> Maybe (Term a)
doPevalXorBitsTerm (ConTerm _ a) (ConTerm _ b) = Just $ conTerm (a `xor` b)
doPevalXorBitsTerm (ConTerm _ a) b
  | a == zeroBits = Just b
  | a == complement zeroBits = Just $ pevalComplementBitsTerm b
doPevalXorBitsTerm a (ConTerm _ b)
  | b == zeroBits = Just a
  | b == complement zeroBits = Just $ pevalComplementBitsTerm a
doPevalXorBitsTerm a b | a == b = Just $ conTerm zeroBits
doPevalXorBitsTerm (ComplementBitsTerm _ i) (ComplementBitsTerm _ j) = Just $ pevalXorBitsTerm i j
doPevalXorBitsTerm (ComplementBitsTerm _ i) j = Just $ pevalComplementBitsTerm $ pevalXorBitsTerm i j
doPevalXorBitsTerm i (ComplementBitsTerm _ j) = Just $ pevalComplementBitsTerm $ pevalXorBitsTerm i j
doPevalXorBitsTerm _ _ = Nothing

-- complement
pevalComplementBitsTerm :: forall a. (Bits a, SupportedPrim a) => Term a -> Term a
pevalComplementBitsTerm = unaryUnfoldOnce doPevalComplementBitsTerm complementBitsTerm

doPevalComplementBitsTerm :: forall a. (Bits a, SupportedPrim a) => Term a -> Maybe (Term a)
doPevalComplementBitsTerm (ConTerm _ a) = Just $ conTerm $ complement a
doPevalComplementBitsTerm (ComplementBitsTerm _ a) = Just a
doPevalComplementBitsTerm _ = Nothing

-- shift
pevalShiftLeftTerm :: forall a. (Integral a, SymShift a, FiniteBits a, SupportedPrim a) => Term a -> Term a -> Term a
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

pevalShiftRightTerm :: forall a. (Integral a, SymShift a, FiniteBits a, SupportedPrim a) => Term a -> Term a -> Term a
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

pevalRotateLeftTerm :: forall a. (Integral a, Bits a, SupportedPrim a) => Term a -> Term a -> Term a
pevalRotateLeftTerm t n = unaryUnfoldOnce (`doPevalRotateLeftTerm` n) (`rotateLeftTerm` n) t

doPevalRotateLeftTerm :: forall a. (Integral a, Bits a, SupportedPrim a) => Term a -> Term a -> Maybe (Term a)
doPevalRotateLeftTerm (ConTerm _ a) (ConTerm _ n)
  | n >= 0 = Just $ conTerm $ rotateL a (fromIntegral n)
doPevalRotateLeftTerm x (ConTerm _ 0) = Just x
-- doPevalRotateLeftTerm (RotateLeftTerm _ x (ConTerm _ n)) (ConTerm _ n1)
--   | n >= 0 && n1 >= 0 = Just $ pevalRotateLeftTerm x (conTerm $ n + n1)
doPevalRotateLeftTerm x (ConTerm _ a)
  | case bsize of
      Just s -> s /= 0 && fromIntegral a >= s
      Nothing -> False = do
      cbsize <- bsize
      Just $ pevalRotateLeftTerm x (conTerm $ a `mod` fromIntegral cbsize)
  where
    bsize = bitSizeMaybe (zeroBits :: a)
doPevalRotateLeftTerm _ _ = Nothing

pevalRotateRightTerm :: forall a. (Integral a, Bits a, SupportedPrim a) => Term a -> Term a -> Term a
pevalRotateRightTerm t n = unaryUnfoldOnce (`doPevalRotateRightTerm` n) (`rotateRightTerm` n) t

doPevalRotateRightTerm :: forall a. (Integral a, Bits a, SupportedPrim a) => Term a -> Term a -> Maybe (Term a)
doPevalRotateRightTerm (ConTerm _ a) (ConTerm _ n)
  | n >= 0 = Just $ conTerm $ rotateR a (fromIntegral n)
doPevalRotateRightTerm x (ConTerm _ 0) = Just x
-- doPevalRotateRightTerm (RotateRightTerm _ x (ConTerm _ n)) (ConTerm _ n1)
--   | n >= 0 && n1 >= 0 = Just $ pevalRotateRightTerm x (conTerm $ n + n1)
doPevalRotateRightTerm x (ConTerm _ a)
  | case bsize of
      Just s -> s /= 0 && fromIntegral a >= s
      Nothing -> False = do
      cbsize <- bsize
      Just $ pevalRotateRightTerm x (conTerm $ a `mod` fromIntegral cbsize)
  where
    bsize = bitSizeMaybe (zeroBits :: a)
doPevalRotateRightTerm _ _ = Nothing
