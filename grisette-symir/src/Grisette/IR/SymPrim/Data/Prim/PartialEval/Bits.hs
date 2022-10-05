{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Grisette.IR.SymPrim.Data.Prim.PartialEval.Bits
  ( pattern BitsConcTerm,
    pevalAndBitsTerm,
    pevalOrBitsTerm,
    pevalXorBitsTerm,
    pevalComplementBitsTerm,
    pevalShiftBitsTerm,
    pevalRotateBitsTerm,
  )
where

import Data.Bits
import Data.Typeable
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Unfold

bitsConcTermView :: (Bits b, Typeable b) => Term a -> Maybe b
bitsConcTermView (ConcTerm _ b) = cast b
bitsConcTermView _ = Nothing

pattern BitsConcTerm :: forall b a. (Bits b, Typeable b) => b -> Term a
pattern BitsConcTerm b <- (bitsConcTermView -> Just b)

-- bitand
pevalAndBitsTerm :: forall a. (Bits a, SupportedPrim a) => Term a -> Term a -> Term a
pevalAndBitsTerm = binaryUnfoldOnce doPevalAndBitsTerm andBitsTerm

doPevalAndBitsTerm :: forall a. (Bits a, SupportedPrim a) => Term a -> Term a -> Maybe (Term a)
doPevalAndBitsTerm (ConcTerm _ a) (ConcTerm _ b) = Just $ concTerm (a .&. b)
doPevalAndBitsTerm (ConcTerm _ a) b
  | a == zeroBits = Just $ concTerm zeroBits
  | a == complement zeroBits = Just b
doPevalAndBitsTerm a (ConcTerm _ b)
  | b == zeroBits = Just $ concTerm zeroBits
  | b == complement zeroBits = Just a
doPevalAndBitsTerm a b | a == b = Just a
doPevalAndBitsTerm _ _ = Nothing

-- bitor
pevalOrBitsTerm :: forall a. (Bits a, SupportedPrim a) => Term a -> Term a -> Term a
pevalOrBitsTerm = binaryUnfoldOnce doPevalOrBitsTerm orBitsTerm

doPevalOrBitsTerm :: forall a. (Bits a, SupportedPrim a) => Term a -> Term a -> Maybe (Term a)
doPevalOrBitsTerm (ConcTerm _ a) (ConcTerm _ b) = Just $ concTerm (a .|. b)
doPevalOrBitsTerm (ConcTerm _ a) b
  | a == zeroBits = Just b
  | a == complement zeroBits = Just $ concTerm $ complement zeroBits
doPevalOrBitsTerm a (ConcTerm _ b)
  | b == zeroBits = Just a
  | b == complement zeroBits = Just $ concTerm $ complement zeroBits
doPevalOrBitsTerm a b | a == b = Just a
doPevalOrBitsTerm _ _ = Nothing

-- bitxor
pevalXorBitsTerm :: forall a. (Bits a, SupportedPrim a) => Term a -> Term a -> Term a
pevalXorBitsTerm = binaryUnfoldOnce doPevalXorBitsTerm xorBitsTerm

doPevalXorBitsTerm :: forall a. (Bits a, SupportedPrim a) => Term a -> Term a -> Maybe (Term a)
doPevalXorBitsTerm (ConcTerm _ a) (ConcTerm _ b) = Just $ concTerm (a `xor` b)
doPevalXorBitsTerm (ConcTerm _ a) b
  | a == zeroBits = Just b
  | a == complement zeroBits = Just $ pevalComplementBitsTerm b
doPevalXorBitsTerm a (ConcTerm _ b)
  | b == zeroBits = Just a
  | b == complement zeroBits = Just $ pevalComplementBitsTerm a
doPevalXorBitsTerm a b | a == b = Just $ concTerm zeroBits
doPevalXorBitsTerm (ComplementBitsTerm _ i) (ComplementBitsTerm _ j) = Just $ pevalXorBitsTerm i j
doPevalXorBitsTerm (ComplementBitsTerm _ i) j = Just $ pevalComplementBitsTerm $ pevalXorBitsTerm i j
doPevalXorBitsTerm i (ComplementBitsTerm _ j) = Just $ pevalComplementBitsTerm $ pevalXorBitsTerm i j
doPevalXorBitsTerm _ _ = Nothing

-- complement
pevalComplementBitsTerm :: forall a. (Bits a, SupportedPrim a) => Term a -> Term a
pevalComplementBitsTerm = unaryUnfoldOnce doPevalComplementBitsTerm complementBitsTerm

doPevalComplementBitsTerm :: forall a. (Bits a, SupportedPrim a) => Term a -> Maybe (Term a)
doPevalComplementBitsTerm (ConcTerm _ a) = Just $ concTerm $ complement a
doPevalComplementBitsTerm (ComplementBitsTerm _ a) = Just a
doPevalComplementBitsTerm _ = Nothing

-- shift
pevalShiftBitsTerm :: forall a. (Bits a, SupportedPrim a) => Term a -> Int -> Term a
pevalShiftBitsTerm t n = unaryUnfoldOnce (`doPevalShiftBitsTerm` n) (`shiftBitsTerm` n) t

doPevalShiftBitsTerm :: forall a. (Bits a, SupportedPrim a) => Term a -> Int -> Maybe (Term a)
doPevalShiftBitsTerm (ConcTerm _ a) n = Just $ concTerm $ shift a n
doPevalShiftBitsTerm x 0 = Just x
doPevalShiftBitsTerm _ a
  | case bitSizeMaybe (zeroBits :: a) of
      Just b -> a >= b
      Nothing -> False =
      Just $ concTerm zeroBits
doPevalShiftBitsTerm (ShiftBitsTerm _ x n) n1
  | (n >= 0 && n1 >= 0) || (n <= 0 && n1 <= 0) = Just $ shiftBitsTerm x (n + n1)
doPevalShiftBitsTerm _ _ = Nothing

-- rotate
pevalRotateBitsTerm :: forall a. (Bits a, SupportedPrim a) => Term a -> Int -> Term a
pevalRotateBitsTerm t n = unaryUnfoldOnce (`doPevalRotateBitsTerm` n) (`rotateBitsTerm` n) t

doPevalRotateBitsTerm :: forall a. (Bits a, SupportedPrim a) => Term a -> Int -> Maybe (Term a)
doPevalRotateBitsTerm (ConcTerm _ a) n = Just $ concTerm $ rotate a n
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
