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
    pevalShiftBitsTerm,
    pevalRotateBitsTerm,
  )
where

import Data.Bits
  ( Bits
      ( bitSizeMaybe,
        complement,
        rotate,
        shift,
        xor,
        zeroBits,
        (.&.),
        (.|.)
      ),
  )
import Data.Typeable (Typeable, cast)
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
  ( andBitsTerm,
    complementBitsTerm,
    conTerm,
    orBitsTerm,
    rotateBitsTerm,
    shiftBitsTerm,
    xorBitsTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
  ( SupportedPrim,
    Term (ComplementBitsTerm, ConTerm, RotateBitsTerm, ShiftBitsTerm),
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
pevalShiftBitsTerm :: forall a. (Bits a, SupportedPrim a) => Term a -> Int -> Term a
pevalShiftBitsTerm t n = unaryUnfoldOnce (`doPevalShiftBitsTerm` n) (`shiftBitsTerm` n) t

doPevalShiftBitsTerm :: forall a. (Bits a, SupportedPrim a) => Term a -> Int -> Maybe (Term a)
doPevalShiftBitsTerm (ConTerm _ a) n = Just $ conTerm $ shift a n
doPevalShiftBitsTerm x 0 = Just x
doPevalShiftBitsTerm _ a
  | case bitSizeMaybe (zeroBits :: a) of
      Just b -> a >= b
      Nothing -> False =
      Just $ conTerm zeroBits
doPevalShiftBitsTerm (ShiftBitsTerm _ x n) n1
  | (n >= 0 && n1 >= 0) || (n <= 0 && n1 <= 0) = Just $ shiftBitsTerm x (n + n1)
doPevalShiftBitsTerm _ _ = Nothing

-- rotate
pevalRotateBitsTerm :: forall a. (Bits a, SupportedPrim a) => Term a -> Int -> Term a
pevalRotateBitsTerm t n = unaryUnfoldOnce (`doPevalRotateBitsTerm` n) (`rotateBitsTerm` n) t

doPevalRotateBitsTerm :: forall a. (Bits a, SupportedPrim a) => Term a -> Int -> Maybe (Term a)
doPevalRotateBitsTerm (ConTerm _ a) n = Just $ conTerm $ rotate a n
doPevalRotateBitsTerm x 0 = Just x
doPevalRotateBitsTerm x a
  | case bsize of
      Just s -> s /= 0 && (a >= s || a < 0)
      Nothing -> False = do
      cbsize <- bsize
      if a >= cbsize
        then Just $ pevalRotateBitsTerm x (a - cbsize)
        else Just $ pevalRotateBitsTerm x (a + cbsize)
  where
    bsize = bitSizeMaybe (zeroBits :: a)
doPevalRotateBitsTerm (RotateBitsTerm _ x n) n1 = Just $ rotateBitsTerm x (n + n1)
doPevalRotateBitsTerm _ _ = Nothing
