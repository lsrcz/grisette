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
    SizedBV (..),
    extractSizedBV,
  )
where

import Data.Proxy
import GHC.TypeNats

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> :set -XDataKinds
-- >>> :set -XBinaryLiterals
-- >>> :set -XFlexibleContexts
-- >>> :set -XFlexibleInstances
-- >>> :set -XFunctionalDependencies

-- | Sized bit vector operations. Including bitwise concatenation ('concatSizedBV'),
-- extension ('zextSizedBV', 'sextSizedBV', 'extSizedBV'), and selection
-- ('selectSizedBV').
class SizedBV bv where
  -- | Bitwise concatenation of the given bit vector values.
  --
  -- >>> concatSizedBV (0b101 :: SymIntN 3) (0b010 :: SymIntN 3)
  -- 0b101010
  concatSizedBV :: (KnownNat l, KnownNat r, 1 <= l, 1 <= r) => bv l -> bv r -> bv (l + r)

  -- | Bitwise zero extension of the given bit vector values.
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

  -- | Bitwise signed extension of the given bit vector values.
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

  -- | Bitwise extension of the given bit vector values.
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
  -- The indices are counting from zero from the least significant bit.
  --
  -- >>> selectSizedBV (Proxy @1) (Proxy @3) (con 0b001010 :: SymIntN 6)
  -- 0b101
  selectSizedBV ::
    (KnownNat n, KnownNat ix, KnownNat w, 1 <= n, 1 <= w, 0 <= ix, ix + w <= n) =>
    -- | Index to start selecting from
    proxy ix ->
    -- | Desired output width, @0 <= ix@ and @ix + w <= n@ must hold where @n@ is
    -- the size of the input bit vector
    proxy w ->
    -- | Bit vector to select from
    bv n ->
    bv w

-- | Extract a smaller bit vector from a larger one from bits @i@ down to @j@.
--
-- The indices are counting from zero from the least significant bit.
-- >>> extractSizedBV (Proxy @3) (Proxy @1) (con 0b001010 :: SymIntN 6)
-- 0b101
extractSizedBV ::
  forall proxy i j n bv.
  (SizedBV bv, KnownNat n, KnownNat ((i - j) + 1), KnownNat j, 1 <= n, 1 <= i - j + 1, j + (i - j + 1) <= n) =>
  -- | The start position to extract from, @0 <= i < n@ must hold where @n@ is
  -- the size of the output bit vector
  proxy i ->
  -- | The end position to extract from, @0 <= j <= i@ must hold
  proxy j ->
  -- | Bit vector to extract from
  bv n ->
  bv (i - j + 1)
extractSizedBV _ _ = selectSizedBV (Proxy @j) (Proxy @(i - j + 1))
{-# INLINE extractSizedBV #-}
