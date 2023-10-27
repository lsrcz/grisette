{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Core.Data.Class.SymRotate
  ( SymRotate (..),
    DefaultFiniteBitsSymRotate (..),
  )
where

import Data.Bits (Bits (isSigned, rotate), FiniteBits (finiteBitSize))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)

class (Bits a) => SymRotate a where
  symRotate :: a -> a -> a

instance SymRotate Int where
  symRotate = rotate

newtype DefaultFiniteBitsSymRotate a = DefaultFiniteBitsSymRotate
  { unDefaultFiniteBitsSymRotate :: a
  }
  deriving newtype (Eq, Bits)

instance
  (Integral a, FiniteBits a) =>
  SymRotate (DefaultFiniteBitsSymRotate a)
  where
  symRotate (DefaultFiniteBitsSymRotate a) (DefaultFiniteBitsSymRotate s)
    | isSigned a = DefaultFiniteBitsSymRotate $ symRotateSigned a s
    | otherwise = DefaultFiniteBitsSymRotate $ symRotateUnsigned a s
    where
      symRotateUnsigned :: a -> a -> a
      symRotateUnsigned a s =
        rotate a (fromIntegral (s `mod` fromIntegral (finiteBitSize a)))
      symRotateSigned :: a -> a -> a
      symRotateSigned a s
        | finiteBitSize s == 1 = a
        | finiteBitSize s == 2 = rotate a (fromIntegral s)
        | otherwise =
            rotate a (fromIntegral (s `mod` fromIntegral (finiteBitSize a)))

deriving via (DefaultFiniteBitsSymRotate Int8) instance SymRotate Int8

deriving via (DefaultFiniteBitsSymRotate Int16) instance SymRotate Int16

deriving via (DefaultFiniteBitsSymRotate Int32) instance SymRotate Int32

deriving via (DefaultFiniteBitsSymRotate Int64) instance SymRotate Int64

deriving via (DefaultFiniteBitsSymRotate Word8) instance SymRotate Word8

deriving via (DefaultFiniteBitsSymRotate Word16) instance SymRotate Word16

deriving via (DefaultFiniteBitsSymRotate Word32) instance SymRotate Word32

deriving via (DefaultFiniteBitsSymRotate Word64) instance SymRotate Word64

deriving via (DefaultFiniteBitsSymRotate Word) instance SymRotate Word
