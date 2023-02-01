{-# LANGUAGE DataKinds #-}
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
    zextBV',
    sextBV',
    extBV',
    selectBV',
    extractBV,
    extractBV',
    SizedBV (..),
    extractSizedBV,
  )
where

import Data.Proxy
import GHC.TypeNats
import Grisette.IR.SymPrim.Data.Parameterized

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> import Grisette.IR.SymPrim.Data.Parameterized
-- >>> :set -XDataKinds
-- >>> :set -XBinaryLiterals
-- >>> :set -XFlexibleContexts
-- >>> :set -XFlexibleInstances
-- >>> :set -XFunctionalDependencies

-- | Bit vector operations. Including concatenation ('concatBV'),
-- extension ('zextBV', 'sextBV', 'extBV'), and selection ('selectBV').
class BV bv where
  -- | Concatenation of two bit vectors.
  --
  -- >>> concatBV (SomeSymWordN (0b101 :: SymWordN 3)) (SomeSymWordN (0b010 :: SymWordN 3))
  -- 0b101010
  concatBV :: bv -> bv -> bv

  -- | Zero extension of a bit vector.
  --
  -- >>> zextBV (Proxy @6) (SomeSymWordN (0b101 :: SymWordN 3))
  -- 0b000101
  zextBV ::
    forall p l.
    KnownNat l =>
    -- | Desired output length
    p l ->
    -- | Bit vector to extend
    bv ->
    bv

  -- | Sign extension of a bit vector.
  --
  -- >>> sextBV (Proxy @6) (SomeSymWordN (0b101 :: SymWordN 3))
  -- 0b111101
  sextBV ::
    forall p l.
    KnownNat l =>
    -- | Desired output length
    p l ->
    -- | Bit vector to extend
    bv ->
    bv

  -- | Extension of a bit vector.
  -- Signedness is determined by the input bit vector type.
  --
  -- >>> extBV (Proxy @6) (SomeSymIntN (0b101 :: SymIntN 3))
  -- 0b111101
  -- >>> extBV (Proxy @6) (SomeSymIntN (0b001 :: SymIntN 3))
  -- 0b000001
  -- >>> extBV (Proxy @6) (SomeSymWordN (0b101 :: SymWordN 3))
  -- 0b000101
  -- >>> extBV (Proxy @6) (SomeSymWordN (0b001 :: SymWordN 3))
  -- 0b000001
  extBV ::
    forall p l.
    KnownNat l =>
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
  -- >>> selectBV (Proxy @1) (Proxy @3) (SomeSymIntN (0b001010 :: SymIntN 6))
  -- 0b101
  selectBV ::
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
-- >>> zextBV' (natRepr @6) (SomeSymWordN (0b101 :: SymWordN 3))
-- 0b000101
zextBV' ::
  forall l bv.
  BV bv =>
  -- | Desired output length
  NatRepr l ->
  -- | Bit vector to extend
  bv ->
  bv
zextBV' p@(_ :: NatRepr l) = withKnownNat p $ zextBV (Proxy @l)
{-# INLINE zextBV' #-}

-- | Sign extension of a bit vector.
--
-- >>> sextBV' (natRepr @6) (SomeSymWordN (0b101 :: SymWordN 3))
-- 0b111101
sextBV' ::
  forall l bv.
  BV bv =>
  NatRepr l ->
  -- | Desired output length
  bv ->
  -- | Bit vector to extend
  bv
sextBV' p@(_ :: NatRepr l) = withKnownNat p $ sextBV (Proxy @l)
{-# INLINE sextBV' #-}

-- | Extension of a bit vector.
-- Signedness is determined by the input bit vector type.
--
-- >>> extBV' (natRepr @6) (SomeSymIntN (0b101 :: SymIntN 3))
-- 0b111101
-- >>> extBV' (natRepr @6) (SomeSymIntN (0b001 :: SymIntN 3))
-- 0b000001
-- >>> extBV' (natRepr @6) (SomeSymWordN (0b101 :: SymWordN 3))
-- 0b000101
-- >>> extBV' (natRepr @6) (SomeSymWordN (0b001 :: SymWordN 3))
-- 0b000001
extBV' ::
  forall l bv.
  BV bv =>
  -- | Desired output length
  NatRepr l ->
  -- | Bit vector to extend
  bv ->
  bv
extBV' p@(_ :: NatRepr l) = withKnownNat p $ extBV (Proxy @l)
{-# INLINE extBV' #-}


-- | Slicing out a smaller bit vector from a larger one,
-- selecting a slice with width @w@ starting from index @ix@.
--
-- The least significant bit is indexed as 0.
--
-- >>> selectBV' (natRepr @1) (natRepr @3) (SomeSymIntN (0b001010 :: SymIntN 6))
-- 0b101
selectBV' ::
  forall ix w bv.
  BV bv =>
  -- | Index of the least significant bit of the slice
  NatRepr ix ->
  -- | Desired output width, @ix + w <= n@ must hold where @n@ is
  -- the size of the input bit vector
  NatRepr w ->
  -- | Bit vector to select from
  bv ->
  bv
selectBV' p@(_ :: NatRepr l) q@(_ :: NatRepr r) = withKnownNat p $ withKnownNat q $ selectBV p q
{-# INLINE selectBV' #-}

-- | Slicing out a smaller bit vector from a larger one, extract a slice from
-- bit @i@ down to @j@.
--
-- The least significant bit is indexed as 0.
--
-- >>> extractBV (Proxy @4) (Proxy @2) (SomeSymIntN (0b010100 :: SymIntN 6))
-- 0b101
extractBV ::
  forall p (i :: Nat) q (j :: Nat) bv.
  (BV bv, KnownNat i, KnownNat j) =>
  -- | The start position to extract from, @i < n@ must hold where @n@ is
  -- the size of the output bit vector
  p i ->
  -- | The end position to extract from, @j <= i@ must hold
  q j ->
  -- | Bit vector to extract from
  bv ->
  bv
extractBV _ _ =
  withKnownNat (unsafeMkNatRepr @(i - j + 1) (fromIntegral (natVal (Proxy @i)) - fromIntegral (natVal (Proxy @j)) + 1)) $
    selectBV (Proxy @j) (Proxy @(i - j + 1))
{-# INLINE extractBV #-}

-- | Slicing out a smaller bit vector from a larger one, extract a slice from
-- bit @i@ down to @j@.
--
-- The least significant bit is indexed as 0.
--
-- >>> extractBV' (natVal @4) (natVal @2) (SomeSymIntN (0b010100 :: SymIntN 6))
-- 0b101
extractBV' ::
  forall (i :: Nat) (j :: Nat) bv.
  BV bv =>
  -- | The start position to extract from, @i < n@ must hold where @n@ is
  -- the size of the output bit vector
  NatRepr i ->
  -- | The end position to extract from, @j <= i@ must hold
  NatRepr j ->
  -- | Bit vector to extract from
  bv ->
  bv
extractBV' p@(_ :: NatRepr l) q@(_ :: NatRepr r) = withKnownNat p $ withKnownNat q $ extractBV p q
{-# INLINE extractBV' #-}

-- | Sized bit vector operations. Including concatenation ('concatSizedBV'),
-- extension ('zextSizedBV', 'sextSizedBV', 'extSizedBV'), and selection
-- ('selectSizedBV').
class SizedBV bv where
  -- | Concatenation of two bit vectors.
  --
  -- >>> concatSizedBV (0b101 :: SymIntN 3) (0b010 :: SymIntN 3)
  -- 0b101010
  concatSizedBV :: (KnownNat l, KnownNat r, 1 <= l, 1 <= r) => bv l -> bv r -> bv (l + r)

  -- | Zero extension of a bit vector.
  --
  -- >>> zextSizedBV (Proxy @6) (0b101 :: SymIntN 3)
  -- 0b000101
  zextSizedBV ::
    (KnownNat l, KnownNat r, 1 <= l, KnownNat r, l <= r) =>
    -- | Desired output width
    proxy r ->
    -- | Bit vector to extend
    bv l ->
    bv r

  -- | Signed extension of a bit vector.
  --
  -- >>> sextSizedBV (Proxy @6) (0b101 :: SymIntN 3)
  -- 0b111101
  sextSizedBV ::
    (KnownNat l, KnownNat r, 1 <= l, KnownNat r, l <= r) =>
    -- | Desired output width
    proxy r ->
    -- | Bit vector to extend
    bv l ->
    bv r

  -- | Extension of a bit vector.
  -- Signedness is determined by the input bit vector type.
  --
  -- >>> extSizedBV (Proxy @6) (0b101 :: SymIntN 3)
  -- 0b111101
  -- >>> extSizedBV (Proxy @6) (0b001 :: SymIntN 3)
  -- 0b000001
  -- >>> extSizedBV (Proxy @6) (0b101 :: SymWordN 3)
  -- 0b000101
  -- >>> extSizedBV (Proxy @6) (0b001 :: SymWordN 3)
  -- 0b000001
  extSizedBV ::
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
  -- >>> selectSizedBV (Proxy @2) (Proxy @3) (con 0b010100 :: SymIntN 6)
  -- 0b101
  selectSizedBV ::
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
-- >>> extractSizedBV (Proxy @4) (Proxy @2) (con 0b010100 :: SymIntN 6)
-- 0b101
extractSizedBV ::
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
extractSizedBV _ _ =
  case ( reprIsKnown (addNat (subNat (natRepr @i) (natRepr @j)) (natRepr @1)),
         unsafeLeqProof @(j + (i - j + 1)) @n,
         unsafeLeqProof @1 @(i - j + 1)
       ) of
    (KnownProof, LeqProof, LeqProof) ->
      selectSizedBV (Proxy @j) (Proxy @(i - j + 1))
{-# INLINE extractSizedBV #-}
