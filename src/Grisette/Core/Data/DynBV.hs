{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Grisette.Core.Data.DynBV
  ( DynIntN (..),
    DynWordN (..),
  )
where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Bits
import Data.CallStack
import Data.Function
import Data.Hashable
import Data.Maybe
import Data.Typeable
import Debug.Trace
import GHC.Generics
import GHC.TypeNats
import Grisette.Core.Data.BV
import Grisette.Core.Data.Class.BitVector
import Language.Haskell.TH.Syntax

-- Helpers
bvconstHelper :: (HasCallStack) => Int -> Integer -> SomeWordN
bvconstHelper b v | v < 0 = negate $ bvconstHelper b (negate v)
bvconstHelper b v | b <= 0 || b > 64 = error $ "Bad const" ++ show b
bvconstHelper 1 v = SomeWordN (WordN v :: WordN 1)
bvconstHelper 2 v = SomeWordN (WordN v :: WordN 2)
bvconstHelper 3 v = SomeWordN (WordN v :: WordN 3)
bvconstHelper 4 v = SomeWordN (WordN v :: WordN 4)
bvconstHelper 5 v = SomeWordN (WordN v :: WordN 5)
bvconstHelper 6 v = SomeWordN (WordN v :: WordN 6)
bvconstHelper 7 v = SomeWordN (WordN v :: WordN 7)
bvconstHelper 8 v = SomeWordN (WordN v :: WordN 8)
bvconstHelper 9 v = SomeWordN (WordN v :: WordN 9)
bvconstHelper 10 v = SomeWordN (WordN v :: WordN 10)
bvconstHelper 11 v = SomeWordN (WordN v :: WordN 11)
bvconstHelper 12 v = SomeWordN (WordN v :: WordN 12)
bvconstHelper 13 v = SomeWordN (WordN v :: WordN 13)
bvconstHelper 14 v = SomeWordN (WordN v :: WordN 14)
bvconstHelper 15 v = SomeWordN (WordN v :: WordN 15)
bvconstHelper 16 v = SomeWordN (WordN v :: WordN 16)
bvconstHelper 17 v = SomeWordN (WordN v :: WordN 17)
bvconstHelper 18 v = SomeWordN (WordN v :: WordN 18)
bvconstHelper 19 v = SomeWordN (WordN v :: WordN 19)
bvconstHelper 20 v = SomeWordN (WordN v :: WordN 20)
bvconstHelper 21 v = SomeWordN (WordN v :: WordN 21)
bvconstHelper 22 v = SomeWordN (WordN v :: WordN 22)
bvconstHelper 23 v = SomeWordN (WordN v :: WordN 23)
bvconstHelper 24 v = SomeWordN (WordN v :: WordN 24)
bvconstHelper 25 v = SomeWordN (WordN v :: WordN 25)
bvconstHelper 26 v = SomeWordN (WordN v :: WordN 26)
bvconstHelper 27 v = SomeWordN (WordN v :: WordN 27)
bvconstHelper 28 v = SomeWordN (WordN v :: WordN 28)
bvconstHelper 29 v = SomeWordN (WordN v :: WordN 29)
bvconstHelper 30 v = SomeWordN (WordN v :: WordN 30)
bvconstHelper 31 v = SomeWordN (WordN v :: WordN 31)
bvconstHelper 32 v = SomeWordN (WordN v :: WordN 32)
bvconstHelper 33 v = SomeWordN (WordN v :: WordN 33)
bvconstHelper 34 v = SomeWordN (WordN v :: WordN 34)
bvconstHelper 35 v = SomeWordN (WordN v :: WordN 35)
bvconstHelper 36 v = SomeWordN (WordN v :: WordN 36)
bvconstHelper 37 v = SomeWordN (WordN v :: WordN 37)
bvconstHelper 38 v = SomeWordN (WordN v :: WordN 38)
bvconstHelper 39 v = SomeWordN (WordN v :: WordN 39)
bvconstHelper 40 v = SomeWordN (WordN v :: WordN 40)
bvconstHelper 41 v = SomeWordN (WordN v :: WordN 41)
bvconstHelper 42 v = SomeWordN (WordN v :: WordN 42)
bvconstHelper 43 v = SomeWordN (WordN v :: WordN 43)
bvconstHelper 44 v = SomeWordN (WordN v :: WordN 44)
bvconstHelper 45 v = SomeWordN (WordN v :: WordN 45)
bvconstHelper 46 v = SomeWordN (WordN v :: WordN 46)
bvconstHelper 47 v = SomeWordN (WordN v :: WordN 47)
bvconstHelper 48 v = SomeWordN (WordN v :: WordN 48)
bvconstHelper 49 v = SomeWordN (WordN v :: WordN 49)
bvconstHelper 50 v = SomeWordN (WordN v :: WordN 50)
bvconstHelper 51 v = SomeWordN (WordN v :: WordN 51)
bvconstHelper 52 v = SomeWordN (WordN v :: WordN 52)
bvconstHelper 53 v = SomeWordN (WordN v :: WordN 53)
bvconstHelper 54 v = SomeWordN (WordN v :: WordN 54)
bvconstHelper 55 v = SomeWordN (WordN v :: WordN 55)
bvconstHelper 56 v = SomeWordN (WordN v :: WordN 56)
bvconstHelper 57 v = SomeWordN (WordN v :: WordN 57)
bvconstHelper 58 v = SomeWordN (WordN v :: WordN 58)
bvconstHelper 59 v = SomeWordN (WordN v :: WordN 59)
bvconstHelper 60 v = SomeWordN (WordN v :: WordN 60)
bvconstHelper 61 v = SomeWordN (WordN v :: WordN 61)
bvconstHelper 62 v = SomeWordN (WordN v :: WordN 62)
bvconstHelper 63 v = SomeWordN (WordN v :: WordN 63)
bvconstHelper 64 v = SomeWordN (WordN v :: WordN 64)

someBVDynamicSelectHelper :: (HasCallStack) => Int -> Int -> SomeWordN -> SomeWordN
someBVDynamicSelectHelper ix w bv | 1 <= w && ix + w <= n && ix <= 64 && w <= 64 =
  case w of
    0 -> f1 (Proxy @0)
    1 -> f1 (Proxy @1)
    2 -> f1 (Proxy @2)
    3 -> f1 (Proxy @3)
    4 -> f1 (Proxy @4)
    5 -> f1 (Proxy @5)
    6 -> f1 (Proxy @6)
    7 -> f1 (Proxy @7)
    8 -> f1 (Proxy @8)
    9 -> f1 (Proxy @9)
    10 -> f1 (Proxy @10)
    11 -> f1 (Proxy @11)
    12 -> f1 (Proxy @12)
    13 -> f1 (Proxy @13)
    14 -> f1 (Proxy @14)
    15 -> f1 (Proxy @15)
    16 -> f1 (Proxy @16)
    17 -> f1 (Proxy @17)
    18 -> f1 (Proxy @18)
    19 -> f1 (Proxy @19)
    20 -> f1 (Proxy @20)
    21 -> f1 (Proxy @21)
    22 -> f1 (Proxy @22)
    23 -> f1 (Proxy @23)
    24 -> f1 (Proxy @24)
    25 -> f1 (Proxy @25)
    26 -> f1 (Proxy @26)
    27 -> f1 (Proxy @27)
    28 -> f1 (Proxy @28)
    29 -> f1 (Proxy @29)
    30 -> f1 (Proxy @30)
    31 -> f1 (Proxy @31)
    32 -> f1 (Proxy @32)
    33 -> f1 (Proxy @33)
    34 -> f1 (Proxy @34)
    35 -> f1 (Proxy @35)
    36 -> f1 (Proxy @36)
    37 -> f1 (Proxy @37)
    38 -> f1 (Proxy @38)
    39 -> f1 (Proxy @39)
    40 -> f1 (Proxy @40)
    41 -> f1 (Proxy @41)
    42 -> f1 (Proxy @42)
    43 -> f1 (Proxy @43)
    44 -> f1 (Proxy @44)
    45 -> f1 (Proxy @45)
    46 -> f1 (Proxy @46)
    47 -> f1 (Proxy @47)
    48 -> f1 (Proxy @48)
    49 -> f1 (Proxy @49)
    50 -> f1 (Proxy @50)
    51 -> f1 (Proxy @51)
    52 -> f1 (Proxy @52)
    53 -> f1 (Proxy @53)
    54 -> f1 (Proxy @54)
    55 -> f1 (Proxy @55)
    56 -> f1 (Proxy @56)
    57 -> f1 (Proxy @57)
    58 -> f1 (Proxy @58)
    59 -> f1 (Proxy @59)
    60 -> f1 (Proxy @60)
    61 -> f1 (Proxy @61)
    62 -> f1 (Proxy @62)
    63 -> f1 (Proxy @63)
    64 -> f1 (Proxy @64)
  where
    n = finiteBitSize bv
    f1 :: (KnownNat w) => p w -> SomeWordN
    f1 x = case ix of
      0 -> someBVSelect (Proxy @0) x bv
      1 -> someBVSelect (Proxy @1) x bv
      2 -> someBVSelect (Proxy @2) x bv
      3 -> someBVSelect (Proxy @3) x bv
      4 -> someBVSelect (Proxy @4) x bv
      5 -> someBVSelect (Proxy @5) x bv
      6 -> someBVSelect (Proxy @6) x bv
      7 -> someBVSelect (Proxy @7) x bv
      8 -> someBVSelect (Proxy @8) x bv
      9 -> someBVSelect (Proxy @9) x bv
      10 -> someBVSelect (Proxy @10) x bv
      11 -> someBVSelect (Proxy @11) x bv
      12 -> someBVSelect (Proxy @12) x bv
      13 -> someBVSelect (Proxy @13) x bv
      14 -> someBVSelect (Proxy @14) x bv
      15 -> someBVSelect (Proxy @15) x bv
      16 -> someBVSelect (Proxy @16) x bv
      17 -> someBVSelect (Proxy @17) x bv
      18 -> someBVSelect (Proxy @18) x bv
      19 -> someBVSelect (Proxy @19) x bv
      20 -> someBVSelect (Proxy @20) x bv
      21 -> someBVSelect (Proxy @21) x bv
      22 -> someBVSelect (Proxy @22) x bv
      23 -> someBVSelect (Proxy @23) x bv
      24 -> someBVSelect (Proxy @24) x bv
      25 -> someBVSelect (Proxy @25) x bv
      26 -> someBVSelect (Proxy @26) x bv
      27 -> someBVSelect (Proxy @27) x bv
      28 -> someBVSelect (Proxy @28) x bv
      29 -> someBVSelect (Proxy @29) x bv
      30 -> someBVSelect (Proxy @30) x bv
      31 -> someBVSelect (Proxy @31) x bv
      32 -> someBVSelect (Proxy @32) x bv
      33 -> someBVSelect (Proxy @33) x bv
      34 -> someBVSelect (Proxy @34) x bv
      35 -> someBVSelect (Proxy @35) x bv
      36 -> someBVSelect (Proxy @36) x bv
      37 -> someBVSelect (Proxy @37) x bv
      38 -> someBVSelect (Proxy @38) x bv
      39 -> someBVSelect (Proxy @39) x bv
      40 -> someBVSelect (Proxy @40) x bv
      41 -> someBVSelect (Proxy @41) x bv
      42 -> someBVSelect (Proxy @42) x bv
      43 -> someBVSelect (Proxy @43) x bv
      44 -> someBVSelect (Proxy @44) x bv
      45 -> someBVSelect (Proxy @45) x bv
      46 -> someBVSelect (Proxy @46) x bv
      47 -> someBVSelect (Proxy @47) x bv
      48 -> someBVSelect (Proxy @48) x bv
      49 -> someBVSelect (Proxy @49) x bv
      50 -> someBVSelect (Proxy @50) x bv
      51 -> someBVSelect (Proxy @51) x bv
      52 -> someBVSelect (Proxy @52) x bv
      53 -> someBVSelect (Proxy @53) x bv
      54 -> someBVSelect (Proxy @54) x bv
      55 -> someBVSelect (Proxy @55) x bv
      56 -> someBVSelect (Proxy @56) x bv
      57 -> someBVSelect (Proxy @57) x bv
      58 -> someBVSelect (Proxy @58) x bv
      59 -> someBVSelect (Proxy @59) x bv
      60 -> someBVSelect (Proxy @60) x bv
      61 -> someBVSelect (Proxy @61) x bv
      62 -> someBVSelect (Proxy @62) x bv
      63 -> someBVSelect (Proxy @63) x bv
      64 -> someBVSelect (Proxy @64) x bv
someBVDynamicSelectHelper ix w bv = error "Bad select"

binDynWordNR :: (HasCallStack) => (SomeWordN -> SomeWordN -> r) -> DynWordN -> DynWordN -> [r]
binDynWordNR f l r | finiteBitSize l /= finiteBitSize r = throw BitwidthMismatch
binDynWordNR f (DynWordN l) (DynWordN r) = uncurry f <$> zip l r

binDynWordN :: (HasCallStack) => (SomeWordN -> SomeWordN -> SomeWordN) -> DynWordN -> DynWordN -> DynWordN
binDynWordN f l r | finiteBitSize l /= finiteBitSize r = throw BitwidthMismatch
binDynWordN f (DynWordN l) (DynWordN r) = DynWordN $ uncurry f <$> zip l r

unaryDynWordNR :: (HasCallStack) => (SomeWordN -> r) -> DynWordN -> [r]
unaryDynWordNR f (DynWordN l) = f <$> l

unaryDynWordN :: (HasCallStack) => (SomeWordN -> SomeWordN) -> DynWordN -> DynWordN
unaryDynWordN f (DynWordN l) = DynWordN $ f <$> l

singleBitFlippingOp :: (SomeWordN -> Int -> SomeWordN) -> DynWordN -> Int -> DynWordN
singleBitFlippingOp _ s i | i < 0 = s
singleBitFlippingOp f (DynWordN s) i = DynWordN $ go s p1
  where
    p1 = length s - 1 - i `div` dynIntNBitWidthEach
    p2 = i `mod` dynIntNBitWidthEach
    go [] _ = error "Bad"
    go (x : xs) 0 = f x p2 : xs
    go (x : xs) i = x : go xs (i - 1)

dynIntNBitWidthEach :: Int
dynIntNBitWidthEach = 64

-- Definitions
newtype DynWordN = DynWordN [SomeWordN]
  deriving (Lift, Generic, Hashable, NFData)

newtype DynIntN = DynIntN [SomeWordN]
  deriving (Lift, Generic, Hashable, NFData)

-- Conversion
instance BVSignPair DynIntN DynWordN where
  toSigned (DynWordN l) = DynIntN l
  {-# INLINE toSigned #-}
  toUnsigned (DynIntN l) = DynWordN l
  {-# INLINE toUnsigned #-}

-- WordN

instance DynBVLink DynWordN WordN SomeWordN where
  dynBVIntegerRep (DynWordN l) = go l 0
    where
      go [] r = r
      go (x : xs) r = go xs (shiftL r 64 + toInteger x)

  integerToDynBV bitwidth r
    | r < 0 =
        negate $ integerToDynBV bitwidth (negate r)
  integerToDynBV bitwidth r = DynWordN $ reverse $ go bitwidth r
    where
      go b i | b <= 0 = error "Bad"
      go b i
        | b > 64 =
            SomeWordN (WordN $ i `mod` bit 64 :: WordN 64)
              : go (b - 64) (i `div` bit 64)
      go b i = [bvconstHelper b (i `mod` (1 `shiftL` b))]
  someBVToDynBV (SomeWordN i) = sizedBVToDynBV i
  sizedBVToDynBV x@(WordN i) = integerToDynBV (finiteBitSize x) i

instance Eq DynWordN where
  (==) l r = and $ binDynWordNR (==) l r
  {-# INLINE (==) #-}
  (/=) l r = or $ binDynWordNR (/=) l r
  {-# INLINE (/=) #-}

instance Ord DynWordN where
  (<=) = on (<=) dynBVIntegerRep
  {-# INLINE (<=) #-}
  (<) = on (<) dynBVIntegerRep
  {-# INLINE (<) #-}
  (>=) = on (>=) dynBVIntegerRep
  {-# INLINE (>=) #-}
  (>) = on (>) dynBVIntegerRep
  {-# INLINE (>) #-}

instance Show DynWordN where
  show (DynWordN x) = showBV bitwidth ints
    where
      bitwidth = finiteBitSize (head x) + (length x - 1) * 64
      ints = go $ reverse x
      go :: [SomeWordN] -> Integer
      go [] = 0
      go [SomeWordN (v :: WordN x)] = unWordN v
      go (SomeWordN (x :: WordN x) : xs) = unWordN x + go xs * bit 64

instance Bits DynWordN where
  (.&.) = binDynWordN (.&.)
  (.|.) = binDynWordN (.|.)
  xor = binDynWordN xor
  complement = unaryDynWordN complement

  zeroBits = error "zeroBits is not defined for DynIntN as no bitwidth is known"
  bit = error "zeroBits is not defined for DynIntN as no bitwidth is known"

  setBit = singleBitFlippingOp setBit
  clearBit = singleBitFlippingOp clearBit
  complementBit = singleBitFlippingOp complementBit

  testBit (DynWordN s) i = testBit (s !! p1) p2
    where
      p1 = length s - 1 - i `div` dynIntNBitWidthEach
      p2 = i `mod` dynIntNBitWidthEach

  bitSizeMaybe = return . finiteBitSize
  bitSize = finiteBitSize

  isSigned _ = False

  shiftL l i =
    integerToDynBV
      (finiteBitSize l)
      (shiftL underlying i .&. (shiftL 1 (finiteBitSize l) - 1))
    where
      underlying = dynBVIntegerRep l

  shiftR l i = integerToDynBV (finiteBitSize l) (shiftR (dynBVIntegerRep l) i)

  rotateL bv =
    integerToDynBV (finiteBitSize bv)
      . rotateLInteger (finiteBitSize bv) (dynBVIntegerRep bv)
  rotateR bv =
    integerToDynBV (finiteBitSize bv)
      . rotateRInteger (finiteBitSize bv) (dynBVIntegerRep bv)

  popCount = popCount . dynBVIntegerRep

instance FiniteBits DynWordN where
  finiteBitSize (DynWordN s) = finiteBitSize (head s) + 64 * (length s - 1)

instance Num DynWordN where
  negate i = complement i + integerToDynBV (finiteBitSize i) 1
  a + b | finiteBitSize a /= finiteBitSize b = throw BitwidthMismatch
  a + b = integerToDynBV (finiteBitSize a) $ dynBVIntegerRep a + dynBVIntegerRep b
  a * b | finiteBitSize a /= finiteBitSize b = throw BitwidthMismatch
  a * b = integerToDynBV (finiteBitSize a) $ dynBVIntegerRep a * dynBVIntegerRep b
  abs x = x
  signum x | popCount x == 0 = integerToDynBV (finiteBitSize x) 0
  signum x = integerToDynBV (finiteBitSize x) 1
  fromInteger = error "fromInteger is not defined for DynWordN as no bitwidth is known"

instance DynBV DynWordN where
  dynBVConcat l r =
    integerToDynBV (finiteBitSize l + finiteBitSize r) $
      dynBVIntegerRep l `shiftL` finiteBitSize r + dynBVIntegerRep r
  dynBVZext n l
    | bitsize > n = error "dynBVZext: trying to zero extend a value to a smaller size"
    | otherwise = integerToDynBV n (dynBVIntegerRep l)
    where
      bitsize = finiteBitSize l
  dynBVSext n l
    | bitsize > n = error "dynBVSext: trying to zero extend a value to a smaller size"
    | testBit l (bitsize - 1) =
        integerToDynBV n (dynBVIntegerRep l + 1 `shiftL` n - 1 `shiftL` bitsize)
    | otherwise = integerToDynBV n (dynBVIntegerRep l)
    where
      bitsize = finiteBitSize l
  dynBVExt = dynBVZext
  dynBVSelect ix w l
    | ix + w > bitsize || ix < 0 = error "dynBVSelect: trying to select a bitvector outside the bounds of the input"
    | w <= 0 = error "dynBVSelect: trying to select a bitvector of negative or zero size"
    | otherwise = integerToDynBV w (dynBVIntegerRep l `shiftR` ix)
    where
      bitsize = finiteBitSize l

-- IntN

instance DynBVLink DynIntN IntN SomeIntN where
  dynBVIntegerRep (DynIntN l) = go l 0
    where
      go [] r = r
      go (x : xs) r = go xs (shiftL r 64 + toInteger x)
  integerToDynBV bitwidth r
    | r < 0 =
        negate $ integerToDynBV bitwidth (negate r)
  integerToDynBV bitwidth r = DynIntN $ reverse $ go bitwidth r
    where
      go b i | b <= 0 = error "Bad"
      go b i
        | b > 64 =
            SomeWordN (WordN $ i `mod` bit 64 :: WordN 64)
              : go (b - 64) (i `div` bit 64)
      go b i = [bvconstHelper b (i `mod` (1 `shiftL` b))]

  someBVToDynBV (SomeIntN i) = sizedBVToDynBV i

  sizedBVToDynBV x@(IntN i) = integerToDynBV (finiteBitSize x) i

instance Eq DynIntN where
  (==) = on (==) toUnsigned
  {-# INLINE (==) #-}
  (/=) = on (/=) toUnsigned
  {-# INLINE (/=) #-}

binOrdDynIntN ::
  (HasCallStack) =>
  Bool ->
  Bool ->
  DynIntN ->
  DynIntN ->
  Bool
binOrdDynIntN isLessThen canEq l r | finiteBitSize l /= finiteBitSize r = throw BitwidthMismatch
binOrdDynIntN isLessThen canEq (DynIntN l) (DynIntN r) =
  case (signBit (head l), signBit (head r)) of
    (True, False) -> isLessThen
    (False, True) -> not isLessThen
    _ -> go l r
  where
    cmp = if isLessThen then (<) else (>)
    signBit :: SomeWordN -> Bool
    signBit i = testBit i (finiteBitSize i - 1)
    go [] [] = canEq
    go (x : xs) (y : ys) = cmp x y || (x == y && go xs ys)
    go _ _ = undefined

instance Ord DynIntN where
  (<=) = binOrdDynIntN True True
  {-# INLINE (<=) #-}
  (<) = binOrdDynIntN True False
  {-# INLINE (<) #-}
  (>=) = binOrdDynIntN False True
  {-# INLINE (>=) #-}
  (>) = binOrdDynIntN False False
  {-# INLINE (>) #-}

instance Show DynIntN where
  show = show . toUnsigned

instance Bits DynIntN where
  l .&. r = toSigned $ toUnsigned l .&. toUnsigned r
  l .|. r = toSigned $ toUnsigned l .|. toUnsigned r
  l `xor` r = toSigned $ toUnsigned l `xor` toUnsigned r
  complement = toSigned . complement . toUnsigned

  zeroBits = error "zeroBits is not defined for DynIntN as no bitwidth is known"
  bit = error "zeroBits is not defined for DynIntN as no bitwidth is known"

  setBit l = toSigned . setBit (toUnsigned l)
  clearBit l = toSigned . clearBit (toUnsigned l)
  complementBit l = toSigned . complementBit (toUnsigned l)

  testBit s = testBit (toUnsigned s)

  bitSizeMaybe = return . finiteBitSize
  bitSize = finiteBitSize

  isSigned _ = True

  shiftL l i =
    integerToDynBV
      (finiteBitSize l)
      (shiftL underlying i .&. (shiftL 1 (finiteBitSize l) - 1))
    where
      underlying = dynBVIntegerRep l

  shiftR l 0 = l
  shiftR i@(DynIntN l) k
    | k >= n = integerToDynBV n $ if b then -1 else 0
    | otherwise =
        integerToDynBV n $
          if b then maxi - noi + (intRep `shiftR` k) else intRep `shiftR` k
    where
      intRep = dynBVIntegerRep i
      b = testBit (head l) (finiteBitSize (head l) - 1)
      n = finiteBitSize i
      maxi = (1 :: Integer) `shiftL` n
      noi = (1 :: Integer) `shiftL` (n - k)

  rotateL l = toSigned . rotateL (toUnsigned l)
  rotateR l = toSigned . rotateR (toUnsigned l)

  popCount = popCount . toUnsigned

{-
  shiftL _ i | i < 0 = throw Overflow
  shiftL bv i | i == 0 = bv
  shiftL bv i | i >= finiteBitSize bv = bv `xor` bv
  shiftL (DynIntN l) i
    | i `mod` 64 == 0 =
        case head l of
          SomeWordN (h :: WordN h) ->
            DynIntN $
              someBVSelect (Proxy @0) (Proxy @h) (head skipped)
                : tail skipped
                ++ [SomeWordN (0 :: WordN 64) | i <- [1 .. skip]]
    where
      skip = i `div` 64
      skipped = drop skip l
  shiftL (DynIntN l@(h : t)) i =
    DynIntN $
      ( case compare smallStep $ finiteBitSize h of
         LT -> someBVConcat
          (someBVDynamicSelectHelper 0 (finiteBitSize h - smallStep) skippedPaddedH)
          (someBVDynamicSelectHelper (64 - smallStep) smallStep (head skippedPaddedT))
         _ -> someBVDynamicSelectHelper (64 - smallStep) (finiteBitSize h) skippedPaddedH
      )
        : (case compare smallStep $ finiteBitSize h of
            LT -> go (length t) skippedPaddedT
            EQ -> go (length t) (skippedPaddedH:skippedPaddedT)
            GT -> go (length t) (skippedPaddedH:skippedPaddedT))
    where
      skip = (i + 64 - finiteBitSize h) `div` 64
      smallStep = i `mod` 64
      (skippedPaddedH : skippedPaddedT) = drop skip l ++ repeat (SomeWordN (0 :: WordN 64))
      go 0 _ = []
      go x (r1 : r2 : rs) =
        someBVConcat
          (someBVDynamicSelectHelper 0 (64 - smallStep) r1)
          (someBVDynamicSelectHelper (64 - smallStep) smallStep r2)
          : go (x - 1) (r2 : rs)

  shiftR _ i | i < 0 = throw Overflow
  shiftR bv 0 = bv
  shiftR bv i | i >= finiteBitSize bv =
    integerToDynIntN (finiteBitSize bv) (if testBit bv (finiteBitSize bv - 1) then -1 else 0)
  shiftR (DynIntN l) i | i `mod` 64 == 0 =
    DynIntN $
      bvconstHelper (finiteBitSize $ head l) sign
      : [bvconstHelper 64 sign | i <- [1..pad]] ++
      [someBVSext (Proxy @64) (head l)] ++
      reverse (drop (pad + 1) (reverse (drop 1 l)))
    where
      sign = if testBit (head l) (finiteBitSize (head l) - 1) then -1 else 0
      pad = i `div` 64 - 1
      -}

instance FiniteBits DynIntN where
  finiteBitSize = finiteBitSize . toUnsigned

instance Num DynIntN where
  negate = toSigned . negate . toUnsigned
  a + b = toSigned $ toUnsigned a + toUnsigned b
  a * b = toSigned $ toUnsigned a * toUnsigned b
  abs x | testBit x (finiteBitSize x - 1) = negate x
  abs x = x
  signum x | popCount x == 0 = integerToDynBV (finiteBitSize x) 0
  signum x | testBit x (finiteBitSize x - 1) = integerToDynBV (finiteBitSize x) $ -1
  signum x = integerToDynBV (finiteBitSize x) 1
  fromInteger = error "fromInteger is not defined for DynIntN as no bitwidth is known"

instance DynBV DynIntN where
  dynBVConcat l r = toSigned $ dynBVConcat (toUnsigned l) (toUnsigned r)
  dynBVSext n = toSigned . dynBVSext n . toUnsigned
  dynBVZext n = toSigned . dynBVZext n . toUnsigned
  dynBVExt = dynBVSext
  dynBVSelect ix w = toSigned . dynBVSelect ix w . toUnsigned
