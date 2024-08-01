{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.SymFiniteBits
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.SymFiniteBits
  ( SymFiniteBits (..),
    symBitBlast,
    symLsb,
    symMsb,
    symPopCount,
    symCountLeadingZeros,
    symCountTrailingZeros,
  )
where

import Data.Bits (Bits (clearBit, setBit, (.|.)), FiniteBits (finiteBitSize))
import GHC.TypeLits (KnownNat, type (<=))
import Grisette.Internal.Core.Data.Class.BitVector
  ( BV (bv, bvSelect),
  )
import Grisette.Internal.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Internal.Core.Data.Class.SymEq (SymEq ((.==)))
import Grisette.Internal.SymPrim.SomeBV (SomeBV (SomeBV), unsafeSomeBV)
import Grisette.Internal.SymPrim.SymBV (SymIntN, SymWordN)
import Grisette.Internal.SymPrim.SymBool (SymBool)

-- | A class for symbolic finite bit operations.
class (FiniteBits a, ITEOp a) => SymFiniteBits a where
  symTestBit :: a -> Int -> SymBool
  symSetBitTo :: a -> Int -> SymBool -> a
  symSetBitTo v i b = symIte b (setBit v i) (clearBit v i)
  symFromBits :: [SymBool] -> a

instance SymFiniteBits (SomeBV SymIntN) where
  symTestBit v i = bvSelect i 1 v .== bv 1 1
  symFromBits bits
    | null bits =
        error "Cannot create a SomeBV from an empty list of bits."
  symFromBits bits = unsafeSomeBV (length bits) $ \_ -> symFromBits bits

instance SymFiniteBits (SomeBV SymWordN) where
  symTestBit v i = bvSelect i 1 v .== bv 1 1
  symFromBits bits
    | null bits =
        error "Cannot create a SomeBV from an empty list of bits."
  symFromBits bits = unsafeSomeBV (length bits) $ \_ -> symFromBits bits

instance (KnownNat n, 1 <= n) => SymFiniteBits (SymIntN n) where
  symTestBit v = symTestBit (SomeBV v)
  symSetBitTo v i b = symIte b (setBit v i) (clearBit v i)
  symFromBits bits
    | length bits /= finiteBitSize (undefined :: SymWordN n) =
        error "symFromBits: length mismatch"
    | otherwise = foldl1 (.|.) lst
    where
      lst :: [SymIntN n]
      lst = (\(pos, b) -> symIte b (setBit 0 pos) 0) <$> zip [0 ..] bits

instance (KnownNat n, 1 <= n) => SymFiniteBits (SymWordN n) where
  symTestBit v = symTestBit (SomeBV v)
  symSetBitTo v i b = symIte b (setBit v i) (clearBit v i)
  symFromBits bits
    | length bits /= finiteBitSize (undefined :: SymWordN n) =
        error "symFromBits: length mismatch"
    | otherwise = foldl1 (.|.) lst
    where
      lst :: [SymWordN n]
      lst = (\(pos, b) -> symIte b (setBit 0 pos) 0) <$> zip [0 ..] bits

-- | Bit-blast a symbolic value into a list of symbolic bits. The first element
-- in the resulting list corresponds to the least significant bit.
symBitBlast :: (SymFiniteBits a) => a -> [SymBool]
symBitBlast x = map (symTestBit x) [0 .. finiteBitSize x - 1]

-- | Extract the least significant bit of a symbolic value.
symLsb :: (SymFiniteBits a) => a -> SymBool
symLsb x = symTestBit x 0

-- | Extract the most significant bit of a symbolic value.
symMsb :: (SymFiniteBits a) => a -> SymBool
symMsb x = symTestBit x (finiteBitSize x - 1)

-- | Count the number of set bits in a symbolic value.
symPopCount :: (Num b, ITEOp b, SymFiniteBits a) => a -> b
symPopCount v = sum $ (\b -> symIte b 1 0) <$> symBitBlast v

-- | Count the number of leading zeros in a symbolic value.
symCountLeadingZeros :: (Num b, ITEOp b, SymFiniteBits a) => a -> b
symCountLeadingZeros v = go bits rs
  where
    bits = reverse $ symBitBlast v
    rs = fromIntegral <$> [0 ..]
    go [] (r : _) = r
    go (b : bs) (r : rs) = symIte b r (go bs rs)
    go _ [] = error "Should not happen"

-- | Count the number of trailing zeros in a symbolic value.
symCountTrailingZeros :: (Num b, ITEOp b, SymFiniteBits a) => a -> b
symCountTrailingZeros v = go bits rs
  where
    bits = symBitBlast v
    rs = fromIntegral <$> [0 ..]
    go [] (r : _) = r
    go (b : bs) (r : rs) = symIte b r (go bs rs)
    go _ [] = error "Should not happen"
