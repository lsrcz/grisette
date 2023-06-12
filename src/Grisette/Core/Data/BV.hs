{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- |
-- Module      :   Grisette.Core.Data.BV
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.BV
  ( BitwidthMismatch (..),
    IntN (..),
    WordN (..),
    SomeIntN (..),
    SomeWordN (..),
    unarySomeIntN,
    unarySomeIntNR1,
    binSomeIntN,
    binSomeIntNR1,
    binSomeIntNR2,
    unarySomeWordN,
    unarySomeWordNR1,
    binSomeWordN,
    binSomeWordNR1,
    binSomeWordNR2,
  )
where

import Control.DeepSeq
import Control.Exception
import Data.Bits
import Data.CallStack
import Data.Hashable
import Data.Proxy
import Data.Typeable
import GHC.Enum
import GHC.Generics
import GHC.Read
import GHC.Real
import GHC.TypeNats
import Grisette.Core.Data.Class.BitVector
import Grisette.Utils.Parameterized
import Language.Haskell.TH.Syntax
import Numeric
import Text.Read
import qualified Text.Read.Lex as L

data BitwidthMismatch = BitwidthMismatch
  deriving (Show, Eq, Ord, Generic)

instance Exception BitwidthMismatch where
  displayException BitwidthMismatch = "Bit width does not match"

-- |
-- Symbolic unsigned bit vectors.
newtype WordN (n :: Nat) = WordN {unWordN :: Integer}
  deriving (Eq, Ord, Generic, Lift, Hashable, NFData)

-- |
-- A non-indexed version of 'WordN'.
data SomeWordN where
  SomeWordN :: (KnownNat n, 1 <= n) => WordN n -> SomeWordN

unarySomeWordN :: (HasCallStack) => (forall n. (KnownNat n, 1 <= n) => WordN n -> r) -> SomeWordN -> r
unarySomeWordN op (SomeWordN (w :: WordN w)) = op w
{-# INLINE unarySomeWordN #-}

unarySomeWordNR1 :: (HasCallStack) => (forall n. (KnownNat n, 1 <= n) => WordN n -> WordN n) -> SomeWordN -> SomeWordN
unarySomeWordNR1 op (SomeWordN (w :: WordN w)) = SomeWordN $ op w
{-# INLINE unarySomeWordNR1 #-}

binSomeWordN :: (HasCallStack) => (forall n. (KnownNat n, 1 <= n) => WordN n -> WordN n -> r) -> SomeWordN -> SomeWordN -> r
binSomeWordN op (SomeWordN (l :: WordN l)) (SomeWordN (r :: WordN r)) =
  case sameNat (Proxy @l) (Proxy @r) of
    Just Refl -> op l r
    Nothing -> throw BitwidthMismatch
{-# INLINE binSomeWordN #-}

binSomeWordNR1 :: (HasCallStack) => (forall n. (KnownNat n, 1 <= n) => WordN n -> WordN n -> WordN n) -> SomeWordN -> SomeWordN -> SomeWordN
binSomeWordNR1 op (SomeWordN (l :: WordN l)) (SomeWordN (r :: WordN r)) =
  case sameNat (Proxy @l) (Proxy @r) of
    Just Refl -> SomeWordN $ op l r
    Nothing -> throw BitwidthMismatch
{-# INLINE binSomeWordNR1 #-}

binSomeWordNR2 :: (HasCallStack) => (forall n. (KnownNat n, 1 <= n) => WordN n -> WordN n -> (WordN n, WordN n)) -> SomeWordN -> SomeWordN -> (SomeWordN, SomeWordN)
binSomeWordNR2 op (SomeWordN (l :: WordN l)) (SomeWordN (r :: WordN r)) =
  case sameNat (Proxy @l) (Proxy @r) of
    Just Refl ->
      case op l r of
        (a, b) -> (SomeWordN a, SomeWordN b)
    Nothing -> throw BitwidthMismatch
{-# INLINE binSomeWordNR2 #-}

instance Eq SomeWordN where
  SomeWordN (l :: WordN l) == SomeWordN (r :: WordN r) =
    case sameNat (Proxy @l) (Proxy @r) of
      Just Refl -> l == r
      Nothing -> False
  {-# INLINE (==) #-}
  SomeWordN (l :: WordN l) /= SomeWordN (r :: WordN r) =
    case sameNat (Proxy @l) (Proxy @r) of
      Just Refl -> l /= r
      Nothing -> True
  {-# INLINE (/=) #-}

instance Ord SomeWordN where
  (<=) = binSomeWordN (<=)
  {-# INLINE (<=) #-}
  (<) = binSomeWordN (<)
  {-# INLINE (<) #-}
  (>=) = binSomeWordN (>=)
  {-# INLINE (>=) #-}
  (>) = binSomeWordN (>)
  {-# INLINE (>) #-}
  max = binSomeWordNR1 max
  {-# INLINE max #-}
  min = binSomeWordNR1 min
  {-# INLINE min #-}
  compare = binSomeWordN compare
  {-# INLINE compare #-}

instance Lift SomeWordN where
  liftTyped (SomeWordN w) = [||SomeWordN w||]

instance Hashable SomeWordN where
  s `hashWithSalt` (SomeWordN (w :: WordN n)) = s `hashWithSalt` natVal (Proxy @n) `hashWithSalt` w

instance NFData SomeWordN where
  rnf (SomeWordN w) = rnf w

instance (KnownNat n, 1 <= n) => Show (WordN n) where
  show (WordN w) = if (bitwidth `mod` 4) == 0 then hexRepPre ++ hexRep else binRepPre ++ binRep
    where
      bitwidth = natVal (Proxy :: Proxy n)
      hexRepPre = "0x" ++ replicate (fromIntegral (bitwidth `div` 4) - length hexRep) '0'
      hexRep = showHex w ""
      binRepPre = "0b" ++ replicate (fromIntegral bitwidth - length binRep) '0'
      binRep = showIntAtBase 2 (\x -> if x == 0 then '0' else '1') w ""

convertInt :: (Num a) => L.Lexeme -> ReadPrec a
convertInt (L.Number n)
  | Just i <- L.numberToInteger n = return (fromInteger i)
convertInt _ = pfail

instance (KnownNat n, 1 <= n) => Read (WordN n) where
  readPrec = readNumber convertInt
  readListPrec = readListPrecDefault
  readList = readListDefault

instance Show SomeWordN where
  show (SomeWordN w) = show w

-- |
-- Symbolic signed bit vectors.
newtype IntN (n :: Nat) = IntN {unIntN :: Integer}
  deriving (Eq, Generic, Lift, Hashable, NFData)

-- |
-- A non-indexed version of 'IntN'.
data SomeIntN where
  SomeIntN :: (KnownNat n, 1 <= n) => IntN n -> SomeIntN

unarySomeIntN :: (forall n. (KnownNat n, 1 <= n) => IntN n -> r) -> SomeIntN -> r
unarySomeIntN op (SomeIntN (w :: IntN w)) = op w
{-# INLINE unarySomeIntN #-}

unarySomeIntNR1 :: (forall n. (KnownNat n, 1 <= n) => IntN n -> IntN n) -> SomeIntN -> SomeIntN
unarySomeIntNR1 op (SomeIntN (w :: IntN w)) = SomeIntN $ op w
{-# INLINE unarySomeIntNR1 #-}

binSomeIntN :: (forall n. (KnownNat n, 1 <= n) => IntN n -> IntN n -> r) -> SomeIntN -> SomeIntN -> r
binSomeIntN op (SomeIntN (l :: IntN l)) (SomeIntN (r :: IntN r)) =
  case sameNat (Proxy @l) (Proxy @r) of
    Just Refl -> op l r
    Nothing -> throw BitwidthMismatch
{-# INLINE binSomeIntN #-}

binSomeIntNR1 :: (forall n. (KnownNat n, 1 <= n) => IntN n -> IntN n -> IntN n) -> SomeIntN -> SomeIntN -> SomeIntN
binSomeIntNR1 op (SomeIntN (l :: IntN l)) (SomeIntN (r :: IntN r)) =
  case sameNat (Proxy @l) (Proxy @r) of
    Just Refl -> SomeIntN $ op l r
    Nothing -> throw BitwidthMismatch
{-# INLINE binSomeIntNR1 #-}

binSomeIntNR2 :: (forall n. (KnownNat n, 1 <= n) => IntN n -> IntN n -> (IntN n, IntN n)) -> SomeIntN -> SomeIntN -> (SomeIntN, SomeIntN)
binSomeIntNR2 op (SomeIntN (l :: IntN l)) (SomeIntN (r :: IntN r)) =
  case sameNat (Proxy @l) (Proxy @r) of
    Just Refl ->
      case op l r of
        (a, b) -> (SomeIntN a, SomeIntN b)
    Nothing -> throw BitwidthMismatch
{-# INLINE binSomeIntNR2 #-}

instance Eq SomeIntN where
  SomeIntN (l :: IntN l) == SomeIntN (r :: IntN r) =
    case sameNat (Proxy @l) (Proxy @r) of
      Just Refl -> l == r
      Nothing -> False
  {-# INLINE (==) #-}
  SomeIntN (l :: IntN l) /= SomeIntN (r :: IntN r) =
    case sameNat (Proxy @l) (Proxy @r) of
      Just Refl -> l /= r
      Nothing -> True
  {-# INLINE (/=) #-}

instance Ord SomeIntN where
  (<=) = binSomeIntN (<=)
  {-# INLINE (<=) #-}
  (<) = binSomeIntN (<)
  {-# INLINE (<) #-}
  (>=) = binSomeIntN (>=)
  {-# INLINE (>=) #-}
  (>) = binSomeIntN (>)
  {-# INLINE (>) #-}
  max = binSomeIntNR1 max
  {-# INLINE max #-}
  min = binSomeIntNR1 min
  {-# INLINE min #-}
  compare = binSomeIntN compare
  {-# INLINE compare #-}

instance Lift SomeIntN where
  liftTyped (SomeIntN w) = [||SomeIntN w||]

instance Hashable SomeIntN where
  s `hashWithSalt` (SomeIntN (w :: IntN n)) = s `hashWithSalt` natVal (Proxy @n) `hashWithSalt` w

instance NFData SomeIntN where
  rnf (SomeIntN w) = rnf w

instance (KnownNat n, 1 <= n) => Show (IntN n) where
  show (IntN w) = if (bitwidth `mod` 4) == 0 then hexRepPre ++ hexRep else binRepPre ++ binRep
    where
      bitwidth = natVal (Proxy :: Proxy n)
      hexRepPre = "0x" ++ replicate (fromIntegral (bitwidth `div` 4) - length hexRep) '0'
      hexRep = showHex w ""
      binRepPre = "0b" ++ replicate (fromIntegral bitwidth - length binRep) '0'
      binRep = showIntAtBase 2 (\x -> if x == 0 then '0' else '1') w ""

instance (KnownNat n, 1 <= n) => Read (IntN n) where
  readPrec = readNumber convertInt
  readListPrec = readListPrecDefault
  readList = readListDefault

instance Show SomeIntN where
  show (SomeIntN w) = show w

instance (KnownNat n, 1 <= n) => Bits (WordN n) where
  WordN a .&. WordN b = WordN (a .&. b)
  WordN a .|. WordN b = WordN (a .|. b)
  WordN a `xor` WordN b = WordN (a `xor` b)
  complement a = maxBound `xor` a

  -- shift use default implementation
  -- rotate use default implementation
  zeroBits = WordN 0
  bit i
    | i < 0 || i >= fromIntegral (natVal (Proxy :: Proxy n)) = zeroBits
    | otherwise = WordN (bit i)

  -- setBit use default implementation
  clearBit (WordN a) i = WordN (clearBit a i)

  -- complementBit use default implementation
  testBit (WordN a) = testBit a
  bitSizeMaybe = Just . finiteBitSize
  bitSize = finiteBitSize
  isSigned _ = False
  shiftL (WordN a) i = WordN (a `shiftL` i) .&. maxBound

  -- unsafeShiftL use default implementation
  shiftR (WordN a) i = WordN (a `shiftR` i)

  -- unsafeShiftR use default implementation
  rotateL a 0 = a
  rotateL (WordN a) k
    | k >= n = rotateL (WordN a) (k `mod` n)
    | otherwise = WordN $ l + h
    where
      n = fromIntegral $ natVal (Proxy :: Proxy n)
      s = n - k
      l = a `shiftR` s
      h = (a - (l `shiftL` s)) `shiftL` k
  rotateR a 0 = a
  rotateR (WordN a) k
    | k >= n = rotateR (WordN a) (k `mod` n)
    | otherwise = WordN $ l + h
    where
      n = fromIntegral $ natVal (Proxy :: Proxy n)
      s = n - k
      l = a `shiftR` k
      h = (a - (l `shiftL` k)) `shiftL` s
  popCount (WordN n) = popCount n

instance Bits SomeWordN where
  (.&.) = binSomeWordNR1 (.&.)
  (.|.) = binSomeWordNR1 (.|.)
  xor = binSomeWordNR1 xor
  complement = unarySomeWordNR1 complement
  shift s i = unarySomeWordNR1 (`shift` i) s
  rotate s i = unarySomeWordNR1 (`rotate` i) s
  zeroBits = error "zeroBits is not defined for SomeWordN as no bitwidth is known"
  bit = error "bit is not defined for SomeWordN as no bitwidth is known"
  setBit s i = unarySomeWordNR1 (`setBit` i) s
  clearBit s i = unarySomeWordNR1 (`clearBit` i) s
  complementBit s i = unarySomeWordNR1 (`complementBit` i) s
  testBit s i = unarySomeWordN (`testBit` i) s
  bitSizeMaybe = Just . finiteBitSize
  bitSize = finiteBitSize
  isSigned _ = False
  shiftL s i = unarySomeWordNR1 (`shiftL` i) s
  unsafeShiftL s i = unarySomeWordNR1 (`unsafeShiftL` i) s
  shiftR s i = unarySomeWordNR1 (`shiftR` i) s
  unsafeShiftR s i = unarySomeWordNR1 (`unsafeShiftR` i) s
  rotateL s i = unarySomeWordNR1 (`rotateL` i) s
  rotateR s i = unarySomeWordNR1 (`rotateR` i) s
  popCount = unarySomeWordN popCount

instance (KnownNat n, 1 <= n) => FiniteBits (WordN n) where
  finiteBitSize _ = fromIntegral (natVal (Proxy :: Proxy n))

instance FiniteBits SomeWordN where
  finiteBitSize (SomeWordN (n :: WordN n)) = fromIntegral $ natVal n
  countLeadingZeros = unarySomeWordN countLeadingZeros
  countTrailingZeros = unarySomeWordN countTrailingZeros

instance (KnownNat n, 1 <= n) => Bounded (WordN n) where
  maxBound = WordN ((1 `shiftL` fromIntegral (natVal (Proxy :: Proxy n))) - 1)
  minBound = WordN 0

instance (KnownNat n, 1 <= n) => Enum (WordN n) where
  succ x
    | x /= maxBound = x + 1
    | otherwise = succError $ "WordN " ++ show (natVal (Proxy :: Proxy n))
  pred x
    | x /= minBound = x - 1
    | otherwise = predError $ "WordN " ++ show (natVal (Proxy :: Proxy n))
  toEnum i
    | i >= 0 && toInteger i <= toInteger (maxBound :: WordN n) = WordN (toInteger i)
    | otherwise = toEnumError ("WordN " ++ show (natVal (Proxy :: Proxy n))) i (minBound :: WordN n, maxBound :: WordN n)
  fromEnum (WordN n) = fromEnum n
  enumFrom = boundedEnumFrom
  {-# INLINE enumFrom #-}
  enumFromThen = boundedEnumFromThen
  {-# INLINE enumFromThen #-}

instance Enum SomeWordN where
  toEnum = error "SomeWordN is not really a Enum type as the bit width is unknown, please consider using WordN instead"
  fromEnum = error "SomeWordN is not really a Enum type as the bit width is unknown, please consider using WordN instead"

instance (KnownNat n, 1 <= n) => Real (WordN n) where
  toRational (WordN n) = n % 1

instance Real SomeWordN where
  toRational = unarySomeWordN toRational

instance (KnownNat n, 1 <= n) => Integral (WordN n) where
  quot (WordN x) (WordN y) = WordN (x `quot` y)
  rem (WordN x) (WordN y) = WordN (x `rem` y)
  quotRem (WordN x) (WordN y) = case quotRem x y of
    (q, r) -> (WordN q, WordN r)
  div = quot
  mod = rem
  divMod = quotRem
  toInteger (WordN n) = n

instance Integral SomeWordN where
  quot = binSomeWordNR1 quot
  rem = binSomeWordNR1 rem
  quotRem = binSomeWordNR2 quotRem
  div = binSomeWordNR1 div
  mod = binSomeWordNR1 mod
  divMod = binSomeWordNR2 divMod
  toInteger = unarySomeWordN toInteger

instance (KnownNat n, 1 <= n) => Num (WordN n) where
  WordN x + WordN y = WordN (x + y) .&. maxBound
  WordN x * WordN y = WordN (x * y) .&. maxBound
  WordN x - WordN y
    | x >= y = WordN (x - y)
    | otherwise = WordN ((1 `shiftL` fromIntegral (natVal (Proxy :: Proxy n))) + x - y)
  negate (WordN 0) = WordN 0
  negate a = complement a + WordN 1
  abs x = x
  signum (WordN 0) = 0
  signum _ = 1
  fromInteger !x
    | x == 0 = WordN 0
    | x > 0 = WordN (x .&. unWordN (maxBound :: WordN n))
    | otherwise = -fromInteger (-x)

instance Num SomeWordN where
  (+) = binSomeWordNR1 (+)
  (-) = binSomeWordNR1 (-)
  (*) = binSomeWordNR1 (*)
  negate = unarySomeWordNR1 negate
  abs = unarySomeWordNR1 abs
  signum = unarySomeWordNR1 signum
  fromInteger = error "fromInteger is not defined for SomeWordN as no bitwidth is known"

minusOneIntN :: forall proxy n. (KnownNat n) => proxy n -> IntN n
minusOneIntN _ = IntN (1 `shiftL` fromIntegral (natVal (Proxy :: Proxy n)) - 1)

instance (KnownNat n, 1 <= n) => Bits (IntN n) where
  IntN a .&. IntN b = IntN (a .&. b)
  IntN a .|. IntN b = IntN (a .|. b)
  IntN a `xor` IntN b = IntN (a `xor` b)
  complement a = minusOneIntN (Proxy :: Proxy n) `xor` a

  -- shift use default implementation
  -- rotate use default implementation
  zeroBits = IntN 0
  bit i = IntN (unWordN (bit i :: WordN n))

  -- setBit use default implementation
  clearBit (IntN a) i = IntN (clearBit a i)

  -- complementBit use default implementation
  testBit (IntN a) = testBit a
  bitSizeMaybe = Just . finiteBitSize
  bitSize = finiteBitSize
  isSigned _ = True

  shiftL (IntN a) i = IntN (unWordN $ (WordN a :: WordN n) `shiftL` i)

  -- unsafeShiftL use default implementation
  shiftR i 0 = i
  shiftR (IntN i) k
    | k >= n = if b then IntN (maxi - 1) else IntN 0
    | otherwise = if b then IntN (maxi - noi + (i `shiftR` k)) else IntN (i `shiftR` k)
    where
      b = testBit i (n - 1)
      n = fromIntegral $ natVal (Proxy :: Proxy n)
      maxi = (1 :: Integer) `shiftL` n
      noi = (1 :: Integer) `shiftL` (n - k)

  -- unsafeShiftR use default implementation
  rotateL (IntN i) k = IntN $ unWordN $ rotateL (WordN i :: WordN n) k
  rotateR (IntN i) k = IntN $ unWordN $ rotateR (WordN i :: WordN n) k
  popCount (IntN i) = popCount i

instance Bits SomeIntN where
  (.&.) = binSomeIntNR1 (.&.)
  (.|.) = binSomeIntNR1 (.|.)
  xor = binSomeIntNR1 xor
  complement = unarySomeIntNR1 complement
  shift s i = unarySomeIntNR1 (`shift` i) s
  rotate s i = unarySomeIntNR1 (`rotate` i) s
  zeroBits = error "zeroBits is not defined for SomeIntN as no bitwidth is known"
  bit = error "bit is not defined for SomeIntN as no bitwidth is known"
  setBit s i = unarySomeIntNR1 (`setBit` i) s
  clearBit s i = unarySomeIntNR1 (`clearBit` i) s
  complementBit s i = unarySomeIntNR1 (`complementBit` i) s
  testBit s i = unarySomeIntN (`testBit` i) s
  bitSizeMaybe = Just . finiteBitSize
  bitSize = finiteBitSize
  isSigned _ = False
  shiftL s i = unarySomeIntNR1 (`shiftL` i) s
  unsafeShiftL s i = unarySomeIntNR1 (`unsafeShiftL` i) s
  shiftR s i = unarySomeIntNR1 (`shiftR` i) s
  unsafeShiftR s i = unarySomeIntNR1 (`unsafeShiftR` i) s
  rotateL s i = unarySomeIntNR1 (`rotateL` i) s
  rotateR s i = unarySomeIntNR1 (`rotateR` i) s
  popCount = unarySomeIntN popCount

instance (KnownNat n, 1 <= n) => FiniteBits (IntN n) where
  finiteBitSize _ = fromIntegral (natVal (Proxy :: Proxy n))

instance FiniteBits SomeIntN where
  finiteBitSize (SomeIntN (n :: IntN n)) = fromIntegral $ natVal n
  countLeadingZeros = unarySomeIntN countLeadingZeros
  countTrailingZeros = unarySomeIntN countTrailingZeros

instance (KnownNat n, 1 <= n) => Bounded (IntN n) where
  maxBound = IntN (1 `shiftL` (fromIntegral (natVal (Proxy :: Proxy n)) - 1) - 1)
  minBound = maxBound + 1

instance (KnownNat n, 1 <= n) => Enum (IntN n) where
  succ x
    | x /= maxBound = x + 1
    | otherwise = succError $ "IntN " ++ show (natVal (Proxy :: Proxy n))
  pred x
    | x /= minBound = x - 1
    | otherwise = predError $ "IntN " ++ show (natVal (Proxy :: Proxy n))
  toEnum i
    | i >= fromIntegral (minBound :: IntN n) && i <= fromIntegral (maxBound :: IntN n) = fromIntegral i
    | otherwise = toEnumError ("IntN " ++ show (natVal (Proxy :: Proxy n))) i (minBound :: WordN n, maxBound :: WordN n)
  fromEnum = fromEnum . toInteger
  enumFrom = boundedEnumFrom
  {-# INLINE enumFrom #-}
  enumFromThen = boundedEnumFromThen
  {-# INLINE enumFromThen #-}

instance Enum SomeIntN where
  toEnum = error "SomeIntN is not really a Enum type as the bit width is unknown, please consider using IntN instead"
  fromEnum = error "SomeIntN is not really a Enum type as the bit width is unknown, please consider using IntN instead"

instance (KnownNat n, 1 <= n) => Real (IntN n) where
  toRational i = toInteger i % 1

instance Real SomeIntN where
  toRational = unarySomeIntN toRational

instance (KnownNat n, 1 <= n) => Integral (IntN n) where
  quot x y =
    if x == minBound && y == -1
      then throw Overflow
      else fromInteger (toInteger x `quot` toInteger y)
  rem x y = fromInteger (toInteger x `rem` toInteger y)
  quotRem x y =
    if x == minBound && y == -1
      then throw Overflow
      else case quotRem (toInteger x) (toInteger y) of
        (q, r) -> (fromInteger q, fromInteger r)
  div x y =
    if x == minBound && y == -1
      then throw Overflow
      else fromInteger (toInteger x `div` toInteger y)
  mod x y = fromInteger (toInteger x `mod` toInteger y)
  divMod x y =
    if x == minBound && y == -1
      then throw Overflow
      else case divMod (toInteger x) (toInteger y) of
        (q, r) -> (fromInteger q, fromInteger r)
  toInteger i@(IntN n) = case signum i of
    0 -> 0
    1 -> n
    -1 ->
      let x = negate i
       in if signum x == -1 then -n else negate (toInteger x)
    _ -> undefined

instance Integral SomeIntN where
  quot = binSomeIntNR1 quot
  rem = binSomeIntNR1 rem
  quotRem = binSomeIntNR2 quotRem
  div = binSomeIntNR1 div
  mod = binSomeIntNR1 mod
  divMod = binSomeIntNR2 divMod
  toInteger = unarySomeIntN toInteger

instance (KnownNat n, 1 <= n) => Num (IntN n) where
  IntN x + IntN y = IntN (x + y) .&. minusOneIntN (Proxy :: Proxy n)
  IntN x * IntN y = IntN (x * y) .&. minusOneIntN (Proxy :: Proxy n)
  IntN x - IntN y
    | x >= y = IntN (x - y)
    | otherwise = IntN ((1 `shiftL` fromIntegral (natVal (Proxy :: Proxy n))) + x - y)
  negate (IntN 0) = IntN 0
  negate a = complement a + IntN 1
  abs x = if testBit x (fromIntegral $ natVal (Proxy :: Proxy n) - 1) then negate x else x
  signum (IntN 0) = IntN 0
  signum i = if testBit i (fromIntegral $ natVal (Proxy :: Proxy n) - 1) then -1 else 1
  fromInteger !x = IntN $ if v >= 0 then v else (1 `shiftL` n) + v
    where
      v = unWordN (fromInteger (x + maxn) :: WordN n) - maxn
      n = fromIntegral (natVal (Proxy :: Proxy n))
      maxn = 1 `shiftL` (n - 1) - 1

instance Num SomeIntN where
  (+) = binSomeIntNR1 (+)
  (-) = binSomeIntNR1 (-)
  (*) = binSomeIntNR1 (*)
  negate = unarySomeIntNR1 negate
  abs = unarySomeIntNR1 abs
  signum = unarySomeIntNR1 signum
  fromInteger = error "fromInteger is not defined for SomeIntN as no bitwidth is known"

instance (KnownNat n, 1 <= n) => Ord (IntN n) where
  IntN a <= IntN b
    | as && not bs = True
    | not as && bs = False
    | otherwise = a <= b
    where
      n = fromIntegral (natVal (Proxy :: Proxy n))
      as = testBit a (n - 1)
      bs = testBit b (n - 1)

instance SizedBV WordN where
  sizedBVConcat :: forall l r. (KnownNat l, KnownNat r, 1 <= l, 1 <= r) => WordN l -> WordN r -> WordN (l + r)
  sizedBVConcat (WordN a) (WordN b) = WordN ((a `shiftL` fromIntegral (natVal (Proxy :: Proxy r))) .|. b)
  sizedBVZext _ (WordN v) = WordN v
  sizedBVSext :: forall l r proxy. (KnownNat l, KnownNat r, 1 <= l, KnownNat r, l <= r) => proxy r -> WordN l -> WordN r
  sizedBVSext pr (WordN v) = if s then WordN (maxi - noi + v) else WordN v
    where
      r = fromIntegral $ natVal pr
      l = fromIntegral $ natVal (Proxy :: Proxy l)
      s = testBit v (l - 1)
      maxi = (1 :: Integer) `shiftL` r
      noi = (1 :: Integer) `shiftL` l
  sizedBVExt = sizedBVZext
  sizedBVSelect ::
    forall n ix w p q.
    (KnownNat n, KnownNat ix, KnownNat w, 1 <= n, 1 <= w, ix + w <= n) =>
    p ix ->
    q w ->
    WordN n ->
    WordN w
  sizedBVSelect pix pw (WordN v) = WordN ((v `shiftR` ix) .&. mask)
    where
      ix = fromIntegral $ natVal pix
      w = fromIntegral $ natVal pw
      mask = (1 `shiftL` w) - 1

instance SizedBV IntN where
  sizedBVConcat :: forall l r. (KnownNat l, KnownNat r, 1 <= l, 1 <= r) => IntN l -> IntN r -> IntN (l + r)
  sizedBVConcat (IntN a) (IntN b) = IntN $ unWordN $ sizedBVConcat (WordN a :: WordN l) (WordN b :: WordN r)
  sizedBVZext _ (IntN v) = IntN v
  sizedBVSext :: forall l r proxy. (KnownNat l, KnownNat r, 1 <= l, KnownNat r, l <= r) => proxy r -> IntN l -> IntN r
  sizedBVSext pr (IntN v) = IntN $ unWordN $ sizedBVSext pr (WordN v :: WordN l)
  sizedBVExt = sizedBVSext
  sizedBVSelect ::
    forall n ix w p q.
    (KnownNat n, KnownNat ix, KnownNat w, 1 <= n, 1 <= w, ix + w <= n) =>
    p ix ->
    q w ->
    IntN n ->
    IntN w
  sizedBVSelect pix pw (IntN v) = IntN $ unWordN $ sizedBVSelect pix pw (WordN v :: WordN n)

instance BV SomeWordN where
  bvConcat (SomeWordN (a :: WordN l)) (SomeWordN (b :: WordN r)) =
    case (leqAddPos (Proxy @l) (Proxy @r), knownAdd @l @r KnownProof KnownProof) of
      (LeqProof, KnownProof) ->
        SomeWordN $ sizedBVConcat a b
  {-# INLINE bvConcat #-}
  bvZext l (SomeWordN (a :: WordN n))
    | l < n = error "bvZext: trying to zero extend a value to a smaller size"
    | otherwise = res (Proxy @n)
    where
      n = fromIntegral $ natVal (Proxy @n)
      res :: forall (l :: Nat). Proxy l -> SomeWordN
      res p =
        case (unsafeKnownProof @l (fromIntegral l), unsafeLeqProof @1 @l, unsafeLeqProof @n @l) of
          (KnownProof, LeqProof, LeqProof) -> SomeWordN $ sizedBVZext p a
  bvSext l (SomeWordN (a :: WordN n))
    | l < n = error "bvSext: trying to zero extend a value to a smaller size"
    | otherwise = res (Proxy @n)
    where
      n = fromIntegral $ natVal (Proxy @n)
      res :: forall (l :: Nat). Proxy l -> SomeWordN
      res p =
        case (unsafeKnownProof @l (fromIntegral l), unsafeLeqProof @1 @l, unsafeLeqProof @n @l) of
          (KnownProof, LeqProof, LeqProof) -> SomeWordN $ sizedBVSext p a
  bvExt = bvZext
  bvSelect ix w (SomeWordN (a :: WordN n))
    | ix + w > n = error "bvSelect: trying to select a bitvector outside the bounds of the input"
    | w == 0 = error "bvSelect: trying to select a bitvector of size 0"
    | otherwise = res (Proxy @n) (Proxy @n)
    where
      n = fromIntegral $ natVal (Proxy @n)
      res :: forall (w :: Nat) (ix :: Nat). Proxy w -> Proxy ix -> SomeWordN
      res p1 p2 =
        case ( unsafeKnownProof @ix (fromIntegral ix),
               unsafeKnownProof @w (fromIntegral w),
               unsafeLeqProof @1 @w,
               unsafeLeqProof @(ix + w) @n
             ) of
          (KnownProof, KnownProof, LeqProof, LeqProof) ->
            SomeWordN $ sizedBVSelect (Proxy @ix) (Proxy @w) a

instance BV SomeIntN where
  bvConcat l r = toSigned $ bvConcat (toUnsigned l) (toUnsigned l)
  {-# INLINE bvConcat #-}
  bvZext l = toSigned . bvZext l . toUnsigned
  {-# INLINE bvZext #-}
  bvSext l = toSigned . bvSext l . toUnsigned
  {-# INLINE bvSext #-}
  bvExt l = toSigned . bvExt l . toUnsigned
  {-# INLINE bvExt #-}
  bvSelect ix w = toSigned . bvSelect ix w . toUnsigned
  {-# INLINE bvSelect #-}

instance (KnownNat n, 1 <= n) => BVSignConversion (WordN n) (IntN n) where
  toSigned (WordN i) = IntN i
  toUnsigned (IntN i) = WordN i

instance BVSignConversion SomeWordN SomeIntN where
  toSigned (SomeWordN i) = SomeIntN $ toSigned i
  toUnsigned (SomeIntN i) = SomeWordN $ toUnsigned i
