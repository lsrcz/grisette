{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.SymRotate
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.SymRotate
  ( SymRotate (..),
    DefaultFiniteBitsSymRotate (..),
  )
where

import Data.Bits (Bits (isSigned, rotate), FiniteBits (finiteBitSize))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)

-- | The `symRotate` is similar to `rotate`, but accepts the type itself instead
-- of `Int` for the rotate amount. The function works on all inputs, including
-- the rotate amounts that are beyond the bit width of the value.
--
-- The `symRotateNegated` function rotates to the opposite direction of
-- `symRotate`. This function is introduced to handle the asymmetry of the range
-- of values.
class (Bits a) => SymRotate a where
  symRotate :: a -> a -> a
  symRotateNegated :: a -> a -> a

instance SymRotate Int where
  symRotate = rotate
  symRotateNegated a s
    | s /= minBound = rotate a (-s)
    | otherwise = rotate a (-(s + finiteBitSize s))

-- | A newtype wrapper. Use this to derive `SymRotate` for types that have
-- `FiniteBits` instances.
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
  symRotateNegated (DefaultFiniteBitsSymRotate a) (DefaultFiniteBitsSymRotate s)
    | isSigned a = DefaultFiniteBitsSymRotate $ symRotateSigned a s
    | otherwise = DefaultFiniteBitsSymRotate $ symRotateUnsigned a s
    where
      bs = fromIntegral (finiteBitSize a)
      smodbs = s `mod` bs
      symRotateUnsigned :: a -> a -> a
      symRotateUnsigned a _ =
        rotate a (fromIntegral (bs - smodbs))
      symRotateSigned :: a -> a -> a
      symRotateSigned a s
        | finiteBitSize a == 1 = a
        | finiteBitSize a == 2 = rotate a (-fromIntegral s)
        | otherwise =
            if smodbs > 0
              then rotate a (fromIntegral (bs - smodbs))
              else rotate a (fromIntegral (-smodbs))

deriving via (DefaultFiniteBitsSymRotate Int8) instance SymRotate Int8

deriving via (DefaultFiniteBitsSymRotate Int16) instance SymRotate Int16

deriving via (DefaultFiniteBitsSymRotate Int32) instance SymRotate Int32

deriving via (DefaultFiniteBitsSymRotate Int64) instance SymRotate Int64

deriving via (DefaultFiniteBitsSymRotate Word8) instance SymRotate Word8

deriving via (DefaultFiniteBitsSymRotate Word16) instance SymRotate Word16

deriving via (DefaultFiniteBitsSymRotate Word32) instance SymRotate Word32

deriving via (DefaultFiniteBitsSymRotate Word64) instance SymRotate Word64

deriving via (DefaultFiniteBitsSymRotate Word) instance SymRotate Word
