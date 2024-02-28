{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :   Grisette.Core.Data.Class.BitVector
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.Class.BitVector
  ( -- * Bit vector operations
    BV (..),
    bvExtract,
    SizedBV (..),
    sizedBVExtract,
  )
where

import Data.Proxy (Proxy (Proxy))
import GHC.TypeNats (KnownNat, type (+), type (-), type (<=))
import Grisette.Utils.Parameterized
  ( KnownProof (KnownProof),
    LeqProof (LeqProof),
    addNat,
    hasRepr,
    natRepr,
    subNat,
    unsafeLeqProof,
  )

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> import Grisette.Utils.Parameterized
-- >>> :set -XDataKinds
-- >>> :set -XBinaryLiterals
-- >>> :set -XFlexibleContexts
-- >>> :set -XFlexibleInstances
-- >>> :set -XFunctionalDependencies

-- | Bit vector operations. Including concatenation ('bvConcat'),
-- extension ('bvZext', 'bvSext', 'bvExt'), and selection
-- ('bvSelect').
class BV bv where
  -- | Concatenation of two bit vectors.
  --
  -- >>> bvConcat (SomeSymWordN (0b101 :: SymWordN 3)) (SomeSymWordN (0b010 :: SymWordN 3))
  -- 0b101010
  bvConcat :: bv -> bv -> bv

  -- | Zero extension of a bit vector.
  --
  -- >>> bvZext 6 (SomeSymWordN (0b101 :: SymWordN 3))
  -- 0b000101
  bvZext ::
    -- | Desired output length
    Int ->
    -- | Bit vector to extend
    bv ->
    bv

  -- | Sign extension of a bit vector.
  --
  -- >>> bvSext 6 (SomeSymWordN (0b101 :: SymWordN 3))
  -- 0b111101
  bvSext ::
    -- | Desired output length
    Int ->
    -- | Bit vector to extend
    bv ->
    bv

  -- | Extension of a bit vector.
  -- Signedness is determined by the input bit vector type.
  --
  -- >>> bvExt 6 (SomeSymIntN (0b101 :: SymIntN 3))
  -- 0b111101
  -- >>> bvExt 6 (SomeSymIntN (0b001 :: SymIntN 3))
  -- 0b000001
  -- >>> bvExt 6 (SomeSymWordN (0b101 :: SymWordN 3))
  -- 0b000101
  -- >>> bvExt 6 (SomeSymWordN (0b001 :: SymWordN 3))
  -- 0b000001
  bvExt ::
    -- | Desired output length
    Int ->
    -- | Bit vector to extend
    bv ->
    bv

  -- | Slicing out a smaller bit vector from a larger one,
  -- selecting a slice with width @w@ starting from index @ix@.
  --
  -- The least significant bit is indexed as 0.
  --
  -- >>> bvSelect 1 3 (SomeSymIntN (0b001010 :: SymIntN 6))
  -- 0b101
  bvSelect ::
    -- | Index of the least significant bit of the slice
    Int ->
    -- | Desired output width, @ix + w <= n@ must hold where @n@ is
    -- the size of the input bit vector
    Int ->
    -- | Bit vector to select from
    bv ->
    bv

  -- | Create a bit vector from an integer. The bit-width is the first argument,
  -- which should not be zero.
  --
  -- >>> bv 12 21 :: SomeSymIntN
  -- 0x015
  bv :: Int -> Integer -> bv

-- | Slicing out a smaller bit vector from a larger one, extract a slice from
-- bit @i@ down to @j@.
--
-- The least significant bit is indexed as 0.
--
-- >>> bvExtract 4 2 (SomeSymIntN (0b010100 :: SymIntN 6))
-- 0b101
bvExtract ::
  (BV bv) =>
  -- | The start position to extract from, @i < n@ must hold where @n@ is
  -- the size of the output bit vector
  Int ->
  -- | The end position to extract from, @j <= i@ must hold
  Int ->
  -- | Bit vector to extract from
  bv ->
  bv
bvExtract i j = bvSelect j (i - j + 1)
{-# INLINE bvExtract #-}

-- | Sized bit vector operations. Including concatenation ('sizedBVConcat'),
-- extension ('sizedBVZext', 'sizedBVSext', 'sizedBVExt'), and selection
-- ('sizedBVSelect').
class SizedBV bv where
  -- | Concatenation of two bit vectors.
  --
  -- >>> sizedBVConcat (0b101 :: SymIntN 3) (0b010 :: SymIntN 3)
  -- 0b101010
  sizedBVConcat :: (KnownNat l, KnownNat r, 1 <= l, 1 <= r) => bv l -> bv r -> bv (l + r)

  -- | Zero extension of a bit vector.
  --
  -- >>> sizedBVZext (Proxy @6) (0b101 :: SymIntN 3)
  -- 0b000101
  sizedBVZext ::
    (KnownNat l, KnownNat r, 1 <= l, KnownNat r, l <= r) =>
    -- | Desired output width
    proxy r ->
    -- | Bit vector to extend
    bv l ->
    bv r

  -- | Signed extension of a bit vector.
  --
  -- >>> sizedBVSext (Proxy @6) (0b101 :: SymIntN 3)
  -- 0b111101
  sizedBVSext ::
    (KnownNat l, KnownNat r, 1 <= l, KnownNat r, l <= r) =>
    -- | Desired output width
    proxy r ->
    -- | Bit vector to extend
    bv l ->
    bv r

  -- | Extension of a bit vector.
  -- Signedness is determined by the input bit vector type.
  --
  -- >>> sizedBVExt (Proxy @6) (0b101 :: SymIntN 3)
  -- 0b111101
  -- >>> sizedBVExt (Proxy @6) (0b001 :: SymIntN 3)
  -- 0b000001
  -- >>> sizedBVExt (Proxy @6) (0b101 :: SymWordN 3)
  -- 0b000101
  -- >>> sizedBVExt (Proxy @6) (0b001 :: SymWordN 3)
  -- 0b000001
  sizedBVExt ::
    (KnownNat l, KnownNat r, 1 <= l, KnownNat r, l <= r) =>
    -- | Desired output width
    proxy r ->
    -- | Bit vector to extend
    bv l ->
    bv r

  -- | Slicing out a smaller bit vector from a larger one, selecting a slice with
  -- width @w@ starting from index @ix@.
  --
  -- The least significant bit is indexed as 0.
  --
  -- >>> sizedBVSelect (Proxy @2) (Proxy @3) (con 0b010100 :: SymIntN 6)
  -- 0b101
  sizedBVSelect ::
    (KnownNat n, KnownNat ix, KnownNat w, 1 <= n, 1 <= w, ix + w <= n) =>
    -- | Index of the least significant bit of the slice
    p ix ->
    -- | Desired output width, @ix + w <= n@ must hold where @n@ is
    -- the size of the input bit vector
    q w ->
    -- | Bit vector to select from
    bv n ->
    bv w

  -- Analogous to 'fromIntegral'.
  sizedBVFromIntegral :: (Integral a, KnownNat n, 1 <= n) => a -> bv n
  default sizedBVFromIntegral ::
    (Num (bv n), Integral a, KnownNat n, 1 <= n) => a -> bv n
  sizedBVFromIntegral = fromIntegral

-- | Slicing out a smaller bit vector from a larger one, extract a slice from
-- bit @i@ down to @j@.
--
-- The least significant bit is indexed as 0.
--
-- >>> sizedBVExtract (Proxy @4) (Proxy @2) (con 0b010100 :: SymIntN 6)
-- 0b101
sizedBVExtract ::
  forall p i q j n bv.
  (SizedBV bv, KnownNat n, KnownNat i, KnownNat j, 1 <= n, i + 1 <= n, j <= i) =>
  -- | The start position to extract from, @i < n@ must hold where @n@ is
  -- the size of the output bit vector
  p i ->
  -- | The end position to extract from, @j <= i@ must hold
  q j ->
  -- | Bit vector to extract from
  bv n ->
  bv (i - j + 1)
sizedBVExtract _ _ =
  case ( hasRepr (addNat (subNat (natRepr @i) (natRepr @j)) (natRepr @1)),
         unsafeLeqProof @(j + (i - j + 1)) @n,
         unsafeLeqProof @1 @(i - j + 1)
       ) of
    (KnownProof, LeqProof, LeqProof) ->
      sizedBVSelect (Proxy @j) (Proxy @(i - j + 1))
{-# INLINE sizedBVExtract #-}
