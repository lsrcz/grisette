{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Core.Data.Class.SymShift
  ( SymShift (..),
    DefaultFiniteBitsSymShift (..),
  )
where

import Data.Bits (Bits (isSigned, shift), FiniteBits (finiteBitSize))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)

class (Bits a) => SymShift a where
  symShift :: a -> a -> a

instance SymShift Int where
  symShift a s
    | s >= finiteBitSize s = 0
    | s <= -finiteBitSize s = if a >= 0 then 0 else -1
    | otherwise = shift a s

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
      symShiftUnsigned a s | s >= fromIntegral (finiteBitSize a) = 0
      symShiftUnsigned a s = shift a (fromIntegral s)

      symShiftSigned :: (Integral a, FiniteBits a) => a -> a -> a
      symShiftSigned a s | finiteBitSize s == 1 = a
      symShiftSigned a s
        | finiteBitSize s == 2 =
            if s == -2
              then if a < 0 then -1 else 0
              else shift a (fromIntegral s)
      symShiftSigned a s | s >= fromIntegral (finiteBitSize a) = 0
      symShiftSigned a s
        | s <= fromIntegral (-finiteBitSize a) =
            if a < 0 then -1 else 0
      symShiftSigned a s = shift a (fromIntegral s)

deriving via (DefaultFiniteBitsSymShift Int8) instance SymShift Int8

deriving via (DefaultFiniteBitsSymShift Int16) instance SymShift Int16

deriving via (DefaultFiniteBitsSymShift Int32) instance SymShift Int32

deriving via (DefaultFiniteBitsSymShift Int64) instance SymShift Int64

deriving via (DefaultFiniteBitsSymShift Word8) instance SymShift Word8

deriving via (DefaultFiniteBitsSymShift Word16) instance SymShift Word16

deriving via (DefaultFiniteBitsSymShift Word32) instance SymShift Word32

deriving via (DefaultFiniteBitsSymShift Word64) instance SymShift Word64

deriving via (DefaultFiniteBitsSymShift Word) instance SymShift Word
