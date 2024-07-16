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
-- Special case regarding floating-point types:
--
-- Usually, bit-casting a value of type @a@ to type @b@ and then back to type
-- @a@ should yield the original value. However, this is not the case for
-- symbolic floating-point values. In SMT-LIB2, there is only one NaN value,
-- which have multiple bit representations. Then bit-casting a NaN value to
-- a bit-vector will yield one of the bit representations of NaN, which is
-- not necessarily the same as the original bit representation of the NaN value.
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
