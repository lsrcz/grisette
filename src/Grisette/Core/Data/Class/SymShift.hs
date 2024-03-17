{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Core.Data.Class.SymShift
  ( SymShift (..),
    DefaultFiniteBitsSymShift (..),
  )
where

import Data.Bits (Bits (isSigned, shift, shiftR), FiniteBits (finiteBitSize))
import Data.Functor.Const (Const (Const))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)

-- | A class for shifting operations.
--
-- The `symShift` function shifts the value to the left if the shift amount is
-- positive, and to the right if the shift amount is negative. If shifting
-- beyond the bit width of the value, the result is the same as shifting with
-- the bit width.
--
-- The `symShiftNegated` function shifts the value to the right if the shift
-- amount is positive, and to the left if the shift amount is negative. This
-- function is introduced to handle the asymmetry of the range of values.
class (Bits a) => SymShift a where
  symShift :: a -> a -> a
  symShiftNegated :: a -> a -> a

instance SymShift Int where
  symShift a s
    | s >= finiteBitSize s = 0
    | s <= -finiteBitSize s = if a >= 0 then 0 else -1
    | otherwise = shift a s
  symShiftNegated :: Int -> Int -> Int
  symShiftNegated a s
    | s <= -finiteBitSize a = 0
    | otherwise = symShift a (-s)

newtype DefaultFiniteBitsSymShift a = DefaultFiniteBitsSymShift
  { unDefaultFiniteBitsSymShift :: a
  }
  deriving newtype (Eq, Bits)

instance
  (Integral a, FiniteBits a) =>
  SymShift (DefaultFiniteBitsSymShift a)
  where
  symShift (DefaultFiniteBitsSymShift a) (DefaultFiniteBitsSymShift s)
    | isSigned a = DefaultFiniteBitsSymShift $ symShiftSigned a s
    | otherwise = DefaultFiniteBitsSymShift $ symShiftUnsigned a s
    where
      symShiftUnsigned :: (Integral a, FiniteBits a) => a -> a -> a
      symShiftUnsigned a s
        | s >= fromIntegral (finiteBitSize a) = 0
        | otherwise = shift a (fromIntegral s)
      {-# INLINE symShiftUnsigned #-}

      symShiftSigned :: (Integral a, FiniteBits a) => a -> a -> a
      symShiftSigned a s
        | finiteBitSize s == 1 = shift a (fromIntegral s)
        | finiteBitSize s == 2 = shift a (fromIntegral s)
        | s >= fromIntegral (finiteBitSize a) = 0
        | s <= fromIntegral (-finiteBitSize a) = if a < 0 then -1 else 0
        | otherwise = shift a (fromIntegral s)
      {-# INLINE symShiftSigned #-}
  {-# INLINE symShift #-}
  symShiftNegated (DefaultFiniteBitsSymShift a) (DefaultFiniteBitsSymShift s)
    | isSigned a = DefaultFiniteBitsSymShift $ symShiftSigned a s
    | otherwise = DefaultFiniteBitsSymShift $ symShiftUnsigned a s
    where
      symShiftUnsigned :: (Integral a, FiniteBits a) => a -> a -> a
      symShiftUnsigned a s
        | s >= fromIntegral (finiteBitSize a) = 0
        | otherwise = shiftR a (fromIntegral s)
      {-# INLINE symShiftUnsigned #-}

      symShiftSigned :: (Integral a, FiniteBits a) => a -> a -> a
      symShiftSigned a s
        | finiteBitSize s == 1 = shift a (-fromIntegral s)
        | finiteBitSize s == 2 = shift a (-fromIntegral s)
        | s <= fromIntegral (-finiteBitSize a) = 0
        | s >= fromIntegral (finiteBitSize a) = if a < 0 then -1 else 0
        | otherwise = shift a (-fromIntegral s)
      {-# INLINE symShiftSigned #-}
  {-# INLINE symShiftNegated #-}

deriving via (DefaultFiniteBitsSymShift Int8) instance SymShift Int8

deriving via (DefaultFiniteBitsSymShift Int16) instance SymShift Int16

deriving via (DefaultFiniteBitsSymShift Int32) instance SymShift Int32

deriving via (DefaultFiniteBitsSymShift Int64) instance SymShift Int64

deriving via (DefaultFiniteBitsSymShift Word8) instance SymShift Word8

deriving via (DefaultFiniteBitsSymShift Word16) instance SymShift Word16

deriving via (DefaultFiniteBitsSymShift Word32) instance SymShift Word32

deriving via (DefaultFiniteBitsSymShift Word64) instance SymShift Word64

deriving via (DefaultFiniteBitsSymShift Word) instance SymShift Word

-- Const
instance (SymShift a) => SymShift (Const a b) where
  symShift (Const a) (Const b) = Const $ symShift a b
  {-# INLINE symShift #-}
  symShiftNegated (Const a) (Const b) = Const $ symShiftNegated a b
  {-# INLINE symShiftNegated #-}