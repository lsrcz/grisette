{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.BitCast
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.BitCast (BitCast (..)) where

import Control.Monad.ST (ST, runST)
import Data.Array.ST (MArray (newArray), STUArray, readArray)
import Data.Array.Unsafe (castSTUArray)
import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64)

-- | Type class for bit-casting between types.
--
-- __Special Considerations for Floating-Point Types:__
--
-- Typically, bit-casting a value from type @a@ to type @b@ and then back to
-- type @a@ should result in the original value. However, this is not always
-- true for floating-point values. In SMT-LIB2, there is only one NaN value with
-- multiple bit representations.
--
-- Given this, we do not provide 'BitCast' for the 'FP' type, instead, we use
-- the 'bitCastOrCanonical' function to use a canonical representation for the
-- NaN values.
--
-- If your application requires distinguishing between different NaN values,
-- it is recommended to define your own floating-point type using bit-vectors.
-- This allows you to check for NaN values and perform operations by bitcasting
-- back to the provided floating-point types when they are not NaN values.
class BitCast from to where
  bitCast :: from -> to

#define BITCAST_WITH_MARRAY(from, to) \
instance BitCast from to where \
  bitCast x = runST $ bitcastWithMArray x; \
  {-# INLINE bitCast #-}

#if 1
BITCAST_WITH_MARRAY(Int64, Double)
BITCAST_WITH_MARRAY(Double, Int64)
BITCAST_WITH_MARRAY(Word64, Double)
BITCAST_WITH_MARRAY(Double, Word64)
BITCAST_WITH_MARRAY(Word64, Int64)
BITCAST_WITH_MARRAY(Int64, Word64)
BITCAST_WITH_MARRAY(Int32, Float)
BITCAST_WITH_MARRAY(Float, Int32)
BITCAST_WITH_MARRAY(Word32, Float)
BITCAST_WITH_MARRAY(Float, Word32)
BITCAST_WITH_MARRAY(Word32, Int32)
BITCAST_WITH_MARRAY(Int32, Word32)
#endif

{-# INLINE bitcastWithMArray #-}
bitcastWithMArray ::
  ( MArray (STUArray s) a (ST s),
    MArray (STUArray s) b (ST s)
  ) =>
  a ->
  ST s b
bitcastWithMArray x =
  newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0
