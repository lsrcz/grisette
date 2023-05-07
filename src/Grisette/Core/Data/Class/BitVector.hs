{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
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
    SomeBV (..),
    someBVZext',
    someBVSext',
    someBVExt',
    someBVSelect',
    someBVExtract,
    someBVExtract',
    SizedBV (..),
    sizedBVExtract,
    SizedBVSignPair (..),
  )
where

import Data.Proxy
import GHC.TypeNats
import Grisette.Utils.Parameterized

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> import Grisette.Utils.Parameterized
-- >>> :set -XDataKinds
-- >>> :set -XBinaryLiterals
-- >>> :set -XFlexibleContexts
-- >>> :set -XFlexibleInstances
-- >>> :set -XFunctionalDependencies

-- | Bit vector operations. Including concatenation ('someBVConcat'),
-- extension ('someBVZext', 'someBVSext', 'someBVExt'), and selection
-- ('someBVSelect').
class SomeBV bv where
  -- | Concatenation of two bit vectors.
  --
  -- >>> someBVConcat (SomeSymWordN (0b101 :: SymWordN 3)) (SomeSymWordN (0b010 :: SymWordN 3))
  -- 0b101010
  someBVConcat :: bv -> bv -> bv

  -- | Zero extension of a bit vector.
  --
  -- >>> someBVZext (Proxy @6) (SomeSymWordN (0b101 :: SymWordN 3))
  -- 0b000101
  someBVZext ::
    forall p l.
    (KnownNat l) =>
    -- | Desired output length
    p l ->
    -- | Bit vector to extend
    bv ->
    bv

  -- | Sign extension of a bit vector.
  --
  -- >>> someBVSext (Proxy @6) (SomeSymWordN (0b101 :: SymWordN 3))
  -- 0b111101
  someBVSext ::
    forall p l.
    (KnownNat l) =>
    -- | Desired output length
    p l ->
    -- | Bit vector to extend
    bv ->
    bv

  -- | Extension of a bit vector.
  -- Signedness is determined by the input bit vector type.
  --
  -- >>> someBVExt (Proxy @6) (SomeSymIntN (0b101 :: SymIntN 3))
  -- 0b111101
  -- >>> someBVExt (Proxy @6) (SomeSymIntN (0b001 :: SymIntN 3))
  -- 0b000001
  -- >>> someBVExt (Proxy @6) (SomeSymWordN (0b101 :: SymWordN 3))
  -- 0b000101
  -- >>> someBVExt (Proxy @6) (SomeSymWordN (0b001 :: SymWordN 3))
  -- 0b000001
  someBVExt ::
    forall p l.
    (KnownNat l) =>
    -- | Desired output length
    p l ->
    -- | Bit vector to extend
    bv ->
    bv

  -- | Slicing out a smaller bit vector from a larger one,
  -- selecting a slice with width @w@ starting from index @ix@.
  --
  -- The least significant bit is indexed as 0.
  --
  -- >>> someBVSelect (Proxy @1) (Proxy @3) (SomeSymIntN (0b001010 :: SymIntN 6))
  -- 0b101
  someBVSelect ::
    forall p ix q w.
    (KnownNat ix, KnownNat w) =>
    -- | Index of the least significant bit of the slice
    p ix ->
    -- | Desired output width, @ix + w <= n@ must hold where @n@ is
    -- the size of the input bit vector
    q w ->
    -- | Bit vector to select from
    bv ->
    bv

-- | Zero extension of a bit vector.
--
-- >>> someBVZext' (natRepr @6) (SomeSymWordN (0b101 :: SymWordN 3))
-- 0b000101
someBVZext' ::
  forall l bv.
  (SomeBV bv) =>
  -- | Desired output length
  NatRepr l ->
  -- | Bit vector to extend
  bv ->
  bv
someBVZext' p@(_ :: NatRepr l) = withKnownProof (hasRepr p) $ someBVZext (Proxy @l)
{-# INLINE someBVZext' #-}

-- | Sign extension of a bit vector.
--
-- >>> someBVSext' (natRepr @6) (SomeSymWordN (0b101 :: SymWordN 3))
-- 0b111101
someBVSext' ::
  forall l bv.
  (SomeBV bv) =>
  NatRepr l ->
  -- | Desired output length
  bv ->
  -- | Bit vector to extend
  bv
someBVSext' p@(_ :: NatRepr l) = withKnownProof (hasRepr p) $ someBVSext (Proxy @l)
{-# INLINE someBVSext' #-}

-- | Extension of a bit vector.
-- Signedness is determined by the input bit vector type.
--
-- >>> someBVExt' (natRepr @6) (SomeSymIntN (0b101 :: SymIntN 3))
-- 0b111101
-- >>> someBVExt' (natRepr @6) (SomeSymIntN (0b001 :: SymIntN 3))
-- 0b000001
-- >>> someBVExt' (natRepr @6) (SomeSymWordN (0b101 :: SymWordN 3))
-- 0b000101
-- >>> someBVExt' (natRepr @6) (SomeSymWordN (0b001 :: SymWordN 3))
-- 0b000001
someBVExt' ::
  forall l bv.
  (SomeBV bv) =>
  -- | Desired output length
  NatRepr l ->
  -- | Bit vector to extend
  bv ->
  bv
someBVExt' p@(_ :: NatRepr l) = withKnownProof (hasRepr p) $ someBVExt (Proxy @l)
{-# INLINE someBVExt' #-}

-- | Slicing out a smaller bit vector from a larger one,
-- selecting a slice with width @w@ starting from index @ix@.
--
-- The least significant bit is indexed as 0.
--
-- >>> someBVSelect' (natRepr @1) (natRepr @3) (SomeSymIntN (0b001010 :: SymIntN 6))
-- 0b101
someBVSelect' ::
  forall ix w bv.
  (SomeBV bv) =>
  -- | Index of the least significant bit of the slice
  NatRepr ix ->
  -- | Desired output width, @ix + w <= n@ must hold where @n@ is
  -- the size of the input bit vector
  NatRepr w ->
  -- | Bit vector to select from
  bv ->
  bv
someBVSelect' p@(_ :: NatRepr l) q@(_ :: NatRepr r) = withKnownProof (hasRepr p) $ withKnownProof (hasRepr q) $ someBVSelect p q
{-# INLINE someBVSelect' #-}

-- | Slicing out a smaller bit vector from a larger one, extract a slice from
-- bit @i@ down to @j@.
--
-- The least significant bit is indexed as 0.
--
-- >>> someBVExtract (Proxy @4) (Proxy @2) (SomeSymIntN (0b010100 :: SymIntN 6))
-- 0b101
someBVExtract ::
  forall p (i :: Nat) q (j :: Nat) bv.
  (SomeBV bv, KnownNat i, KnownNat j) =>
  -- | The start position to extract from, @i < n@ must hold where @n@ is
  -- the size of the output bit vector
  p i ->
  -- | The end position to extract from, @j <= i@ must hold
  q j ->
  -- | Bit vector to extract from
  bv ->
  bv
someBVExtract _ _ =
  withKnownProof (unsafeKnownProof @(i - j + 1) (fromIntegral (natVal (Proxy @i)) - fromIntegral (natVal (Proxy @j)) + 1)) $
    someBVSelect (Proxy @j) (Proxy @(i - j + 1))
{-# INLINE someBVExtract #-}

-- | Slicing out a smaller bit vector from a larger one, extract a slice from
-- bit @i@ down to @j@.
--
-- The least significant bit is indexed as 0.
--
-- >>> someBVExtract' (natRepr @4) (natRepr @2) (SomeSymIntN (0b010100 :: SymIntN 6))
-- 0b101
someBVExtract' ::
  forall (i :: Nat) (j :: Nat) bv.
  (SomeBV bv) =>
  -- | The start position to extract from, @i < n@ must hold where @n@ is
  -- the size of the output bit vector
  NatRepr i ->
  -- | The end position to extract from, @j <= i@ must hold
  NatRepr j ->
  -- | Bit vector to extract from
  bv ->
  bv
someBVExtract' p@(_ :: NatRepr l) q@(_ :: NatRepr r) = withKnownProof (hasRepr p) $ withKnownProof (hasRepr q) $ someBVExtract p q
{-# INLINE someBVExtract' #-}

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
    proxy ix ->
    -- | Desired output width, @ix + w <= n@ must hold where @n@ is
    -- the size of the input bit vector
    proxy w ->
    -- | Bit vector to select from
    bv n ->
    bv w

-- | Slicing out a smaller bit vector from a larger one, extract a slice from
-- bit @i@ down to @j@.
--
-- The least significant bit is indexed as 0.
--
-- >>> sizedBVExtract (Proxy @4) (Proxy @2) (con 0b010100 :: SymIntN 6)
-- 0b101
sizedBVExtract ::
  forall proxy i j n bv.
  (SizedBV bv, KnownNat n, KnownNat i, KnownNat j, 1 <= n, i + 1 <= n, j <= i) =>
  -- | The start position to extract from, @i < n@ must hold where @n@ is
  -- the size of the output bit vector
  proxy i ->
  -- | The end position to extract from, @j <= i@ must hold
  proxy j ->
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

class SizedBVSignPair sbv ubv | sbv -> ubv, ubv -> sbv where
  toSigned :: (KnownNat n, 1 <= n) => ubv n -> sbv n
  toUnsigned :: (KnownNat n, 1 <= n) => sbv n -> ubv n
