{-# LANGUAGE FunctionalDependencies #-}

module Grisette.Core.Data.Class.SignConversion
  ( SignConversion (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)

-- | Convert values between signed and unsigned.
class SignConversion ubv sbv | ubv -> sbv, sbv -> ubv where
  -- | Convert unsigned value to the corresponding signed value.
  toSigned :: ubv -> sbv

  -- | Convert signed value to the corresponding unsigned value.
  toUnsigned :: sbv -> ubv

instance SignConversion Word8 Int8 where
  toSigned = fromIntegral
  toUnsigned = fromIntegral

instance SignConversion Word16 Int16 where
  toSigned = fromIntegral
  toUnsigned = fromIntegral

instance SignConversion Word32 Int32 where
  toSigned = fromIntegral
  toUnsigned = fromIntegral

instance SignConversion Word64 Int64 where
  toSigned = fromIntegral
  toUnsigned = fromIntegral

instance SignConversion Word Int where
  toSigned = fromIntegral
  toUnsigned = fromIntegral
