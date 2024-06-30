{-# LANGUAGE FunctionalDependencies #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.SignConversion
-- Copyright   :   (c) Sirui Lu 2023-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.SignConversion
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
