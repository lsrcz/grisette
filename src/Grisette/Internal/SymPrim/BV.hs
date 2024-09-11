{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.BV
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.BV
  ( IntN (..),
    IntN8,
    IntN16,
    IntN32,
    IntN64,
    WordN (..),
    WordN8,
    WordN16,
    WordN32,
    WordN64,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.DeepSeq (NFData)
import Control.Exception
  ( ArithException (Overflow),
    throw,
  )
import qualified Data.Binary as Binary
import Data.Bits
  ( Bits
      ( bit,
        bitSize,
        bitSizeMaybe,
        clearBit,
        complement,
        isSigned,
        popCount,
        rotateL,
        rotateR,
        shiftL,
        shiftR,
        testBit,
        xor,
        zeroBits,
        (.&.),
        (.|.)
      ),
    FiniteBits (finiteBitSize),
  )
import Data.Bytes.Serial (Serial (deserialize, serialize))
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe, isJust)
import Data.Proxy (Proxy (Proxy))
import Data.SBV (Int16, Int32, Int64, Int8, Word8)
import qualified Data.Serialize as Cereal
import Data.Word (Word16, Word32, Word64)
import GHC.Enum
  ( boundedEnumFrom,
    boundedEnumFromThen,
    predError,
    succError,
    toEnumError,
  )
import GHC.Generics (Generic)
import GHC.Read
  ( Read (readListPrec, readPrec),
    parens,
    readListDefault,
    readListPrecDefault,
    readNumber,
  )
import GHC.Real ((%))
import GHC.TypeNats
  ( KnownNat,
    Nat,
    natVal,
    type (+),
    type (<=),
  )
import Grisette.Internal.Core.Data.Class.BitCast (BitCast (bitCast))
import Grisette.Internal.Core.Data.Class.BitVector
  ( SizedBV
      ( sizedBVConcat,
        sizedBVExt,
        sizedBVSelect,
        sizedBVSext,
        sizedBVZext
      ),
  )
import Grisette.Internal.Core.Data.Class.Function (Apply (FunType, apply))
import Grisette.Internal.Core.Data.Class.SignConversion
  ( SignConversion (toSigned, toUnsigned),
  )
import Grisette.Internal.Core.Data.Class.SymRotate
  ( DefaultFiniteBitsSymRotate (DefaultFiniteBitsSymRotate),
    SymRotate,
  )
import Grisette.Internal.Core.Data.Class.SymShift
  ( DefaultFiniteBitsSymShift (DefaultFiniteBitsSymShift),
    SymShift,
  )
import Language.Haskell.TH.Syntax (Lift)
import Numeric (showHex, showIntAtBase)
import qualified Test.QuickCheck as QC
import Text.ParserCombinators.ReadP (string)
import Text.ParserCombinators.ReadPrec
  ( ReadPrec,
    get,
    look,
    pfail,
  )
import Text.Read (lift)
import qualified Text.Read.Lex as L

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
-- >>> import Grisette.Backend
-- >>> import Data.Proxy

-- |
-- Unsigned bit vector type. Indexed with the bit width. Signedness affect the
-- semantics of the operations, including comparison/extension, etc.
--
-- >>> 3 + 5 :: WordN 5
-- 0b01000
-- >>> sizedBVConcat (0b101 :: WordN 3) (0b110 :: WordN 3)
-- 0b101110
-- >>> sizedBVExt (Proxy @6) (0b101 :: WordN 3)
-- 0b000101
-- >>> (8 :: WordN 4) < (7 :: WordN 4)
-- False
--
-- More operations are available. Please refer to "Grisette.Core#g:symops" for
-- more information.
newtype WordN (n :: Nat) = WordN {unWordN :: Integer}
  deriving (Eq, Ord, Generic, Lift, Hashable, NFData)

-- | 8-bit unsigned bit-vector
type WordN8 = WordN 8

-- | 16-bit unsigned bit-vector
type WordN16 = WordN 16

-- | 32-bit unsigned bit-vector
type WordN32 = WordN 32

-- | 64-bit unsigned bit-vector
type WordN64 = WordN 64

instance (KnownNat n, 1 <= n) => Show (WordN n) where
  show (WordN w) = if (bitwidth `mod` 4) == 0 then hexRepPre ++ hexRep else binRepPre ++ binRep
    where
      bitwidth = natVal (Proxy :: Proxy n)
      hexRepPre = "0x" ++ replicate (fromIntegral (bitwidth `div` 4) - length hexRep) '0'
      hexRep = showHex w ""
      binRepPre = "0b" ++ replicate (fromIntegral bitwidth - length binRep) '0'
      binRep = showIntAtBase 2 (\x -> if x == 0 then '0' else '1') w ""

instance (KnownNat n, 1 <= n) => Serial (WordN n) where
  serialize (WordN w) = serialize w
  deserialize = WordN <$> deserialize

instance (KnownNat n, 1 <= n) => Cereal.Serialize (WordN n) where
  put = serialize
  get = deserialize

instance (KnownNat n, 1 <= n) => Binary.Binary (WordN n) where
  put = serialize
  get = deserialize

convertInt :: (Num a) => L.Lexeme -> ReadPrec a
convertInt (L.Number n)
  | Just i <- L.numberToInteger n = return (fromInteger i)
convertInt _ = pfail

readBinary :: (Num a) => ReadPrec a
readBinary = parens $ do
  r0 <- look
  case r0 of
    ('-' : _) -> do
      _ <- get
      negate <$> parens parse0b
    _ -> parse0b
  where
    isDigit c = isJust (valDig c)
    valDigit c = fromMaybe 0 (valDig c)
    valDig '0' = Just 0
    valDig '1' = Just 1
    valDig _ = Nothing
    parse0b = do
      _ <- Text.Read.lift $ string "0b"
      fromInteger <$> Text.Read.lift (L.readIntP 2 isDigit valDigit)

instance (KnownNat n, 1 <= n) => Read (WordN n) where
  readPrec = readNumber convertInt <|> readBinary
  readListPrec = readListPrecDefault
  readList = readListDefault

-- |
-- Signed bit vector type. Indexed with the bit width. Signedness affects the
-- semantics of the operations, including comparison/extension, etc.
--
-- >>> 3 + 5 :: IntN 5
-- 0b01000
-- >>> sizedBVConcat (0b101 :: IntN 3) (0b110 :: IntN 3)
-- 0b101110
-- >>> sizedBVExt (Proxy @6) (0b101 :: IntN 3)
-- 0b111101
-- >>> (8 :: IntN 4) < (7 :: IntN 4)
-- True
--
-- More operations are available. Please refer to "Grisette.Core#g:symops" for
-- more information.
newtype IntN (n :: Nat) = IntN {unIntN :: Integer}
  deriving (Eq, Generic, Lift, Hashable, NFData)

-- | 8-bit signed bit-vector
type IntN8 = IntN 8

-- | 16-bit signed bit-vector
type IntN16 = IntN 16

-- | 32-bit signed bit-vector
type IntN32 = IntN 32

-- | 64-bit signed bit-vector
type IntN64 = IntN 64

instance (KnownNat n, 1 <= n) => Show (IntN n) where
  show (IntN w) = if (bitwidth `mod` 4) == 0 then hexRepPre ++ hexRep else binRepPre ++ binRep
    where
      bitwidth = natVal (Proxy :: Proxy n)
      hexRepPre = "0x" ++ replicate (fromIntegral (bitwidth `div` 4) - length hexRep) '0'
      hexRep = showHex w ""
      binRepPre = "0b" ++ replicate (fromIntegral bitwidth - length binRep) '0'
      binRep = showIntAtBase 2 (\x -> if x == 0 then '0' else '1') w ""

instance (KnownNat n, 1 <= n) => Serial (IntN n) where
  serialize (IntN w) = serialize w
  deserialize = IntN <$> deserialize

instance (KnownNat n, 1 <= n) => Cereal.Serialize (IntN n) where
  put = serialize
  get = deserialize

instance (KnownNat n, 1 <= n) => Binary.Binary (IntN n) where
  put = serialize
  get = deserialize

instance (KnownNat n, 1 <= n) => Read (IntN n) where
  readPrec = readNumber convertInt <|> readBinary
  readListPrec = readListPrecDefault
  readList = readListDefault

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
  shiftL w i | i >= finiteBitSize w = 0
  shiftL (WordN a) i = WordN (a `shiftL` i) .&. maxBound

  -- unsafeShiftL use default implementation
  shiftR w i | i >= finiteBitSize w = 0
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

instance (KnownNat n, 1 <= n) => FiniteBits (WordN n) where
  finiteBitSize _ = fromIntegral (natVal (Proxy :: Proxy n))

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

instance (KnownNat n, 1 <= n) => Real (WordN n) where
  toRational (WordN n) = n % 1

instance (KnownNat n, 1 <= n) => Integral (WordN n) where
  quot (WordN x) (WordN y) = WordN (x `quot` y)
  rem (WordN x) (WordN y) = WordN (x `rem` y)
  quotRem (WordN x) (WordN y) = case quotRem x y of
    (q, r) -> (WordN q, WordN r)
  div = quot
  mod = rem
  divMod = quotRem
  toInteger (WordN n) = n

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

instance (KnownNat n, 1 <= n) => QC.Arbitrary (WordN n) where
  arbitrary = QC.arbitrarySizedBoundedIntegral

  -- QC.shrinkIntegral assumes that 2 is representable by the number, which is
  -- not the case for 1-bit bit vector.
  shrink i
    | i == 0 = []
    | i == 1 = [0]
    | otherwise = QC.shrinkIntegral i

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

instance (KnownNat n, 1 <= n) => FiniteBits (IntN n) where
  finiteBitSize _ = fromIntegral (natVal (Proxy :: Proxy n))

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

instance (KnownNat n, 1 <= n) => Real (IntN n) where
  toRational i = toInteger i % 1

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
    -1 ->
      let x = negate i
       in if signum x == -1 then -n else negate (toInteger x)
    1 -> n
    _ -> undefined

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

instance (KnownNat n, 1 <= n) => Ord (IntN n) where
  IntN a <= IntN b
    | as && not bs = True
    | not as && bs = False
    | otherwise = a <= b
    where
      n = fromIntegral (natVal (Proxy :: Proxy n))
      as = testBit a (n - 1)
      bs = testBit b (n - 1)

instance (KnownNat n, 1 <= n) => QC.Arbitrary (IntN n) where
  arbitrary = QC.arbitrarySizedBoundedIntegral

  -- QC.shrinkIntegral assumes that 2 is representable by the number, which is
  -- not the case for 1-bit bit vector.
  shrink i
    | i == 0 = []
    | i == 1 = [0]
    | otherwise = QC.shrinkIntegral i

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

instance (KnownNat n, 1 <= n) => SignConversion (WordN n) (IntN n) where
  toSigned (WordN i) = IntN i
  toUnsigned (IntN i) = WordN i

deriving via
  (DefaultFiniteBitsSymShift (IntN n))
  instance
    (KnownNat n, 1 <= n) => SymShift (IntN n)

deriving via
  (DefaultFiniteBitsSymShift (WordN n))
  instance
    (KnownNat n, 1 <= n) => SymShift (WordN n)

deriving via
  (DefaultFiniteBitsSymRotate (IntN n))
  instance
    (KnownNat n, 1 <= n) => SymRotate (IntN n)

deriving via
  (DefaultFiniteBitsSymRotate (WordN n))
  instance
    (KnownNat n, 1 <= n) => SymRotate (WordN n)

#define BITCAST_FROM_INTEGRAL(from, to) \
  instance BitCast (from) (to) where \
    bitCast = fromIntegral

#if 1
BITCAST_FROM_INTEGRAL(WordN 8, Word8)
BITCAST_FROM_INTEGRAL(WordN 16, Word16)
BITCAST_FROM_INTEGRAL(WordN 32, Word32)
BITCAST_FROM_INTEGRAL(WordN 64, Word64)
BITCAST_FROM_INTEGRAL(WordN 8, Int8)
BITCAST_FROM_INTEGRAL(WordN 16, Int16)
BITCAST_FROM_INTEGRAL(WordN 32, Int32)
BITCAST_FROM_INTEGRAL(WordN 64, Int64)

BITCAST_FROM_INTEGRAL(Word8, WordN 8)
BITCAST_FROM_INTEGRAL(Word16, WordN 16)
BITCAST_FROM_INTEGRAL(Word32, WordN 32)
BITCAST_FROM_INTEGRAL(Word64, WordN 64)
BITCAST_FROM_INTEGRAL(Int8, WordN 8)
BITCAST_FROM_INTEGRAL(Int16, WordN 16)
BITCAST_FROM_INTEGRAL(Int32, WordN 32)
BITCAST_FROM_INTEGRAL(Int64, WordN 64)

BITCAST_FROM_INTEGRAL(IntN 8, Word8)
BITCAST_FROM_INTEGRAL(IntN 16, Word16)
BITCAST_FROM_INTEGRAL(IntN 32, Word32)
BITCAST_FROM_INTEGRAL(IntN 64, Word64)
BITCAST_FROM_INTEGRAL(IntN 8, Int8)
BITCAST_FROM_INTEGRAL(IntN 16, Int16)
BITCAST_FROM_INTEGRAL(IntN 32, Int32)
BITCAST_FROM_INTEGRAL(IntN 64, Int64)

BITCAST_FROM_INTEGRAL(Word8, IntN 8)
BITCAST_FROM_INTEGRAL(Word16, IntN 16)
BITCAST_FROM_INTEGRAL(Word32, IntN 32)
BITCAST_FROM_INTEGRAL(Word64, IntN 64)
BITCAST_FROM_INTEGRAL(Int8, IntN 8)
BITCAST_FROM_INTEGRAL(Int16, IntN 16)
BITCAST_FROM_INTEGRAL(Int32, IntN 32)
BITCAST_FROM_INTEGRAL(Int64, IntN 64)
#endif

instance (KnownNat n, 1 <= n) => BitCast (WordN n) (IntN n) where
  bitCast (WordN i) = IntN i

instance (KnownNat n, 1 <= n) => BitCast (IntN n) (WordN n) where
  bitCast (IntN i) = WordN i

#define BITCAST_VIA_WORDx(from, to, intermediate) \
  instance BitCast (from) (to) where \
    bitCast x = bitCast (bitCast x :: intermediate)

#if 1
BITCAST_VIA_WORDx(WordN64, Double, Word64)
BITCAST_VIA_WORDx(Double, WordN64, Word64)
BITCAST_VIA_WORDx(IntN64, Double, Word64)
BITCAST_VIA_WORDx(Double, IntN64, Word64)
BITCAST_VIA_WORDx(WordN32, Float, Word32)
BITCAST_VIA_WORDx(Float, WordN32, Word32)
BITCAST_VIA_WORDx(IntN32, Float, Word32)
BITCAST_VIA_WORDx(Float, IntN32, Word32)
#endif

instance BitCast Bool (WordN 1) where
  bitCast False = 0
  bitCast True = 1

instance BitCast (WordN 1) Bool where
  bitCast 0 = False
  bitCast _ = True

instance BitCast Bool (IntN 1) where
  bitCast False = 0
  bitCast True = 1

instance BitCast (IntN 1) Bool where
  bitCast 0 = False
  bitCast _ = True

instance Apply (IntN n) where
  type FunType (IntN n) = IntN n
  apply = id

instance Apply (WordN n) where
  type FunType (WordN n) = WordN n
  apply = id
