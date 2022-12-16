{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :   Grisette.Core.Data.Class.BitVector
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.Class.BitVector
  ( -- * Note for the examples

    --

    -- | This module does not contain the implementation for symbolic primitive
    -- types, and the examples in this module rely on the implementations in
    -- the [grisette-symir](https://hackage.haskell.org/package/grisette-symir) package.

    -- * Bit vector operations
    BVConcat (..),
    BVExtend (..),
    BVSelect (..),
    bvextract,
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

-- | Bitwise concatenation ('bvconcat') of the given bit vector values.
class BVConcat bv1 bv2 bv3 | bv1 bv2 -> bv3 where
  -- | Bitwise concatenation of the given bit vector values.
  --
  -- >>> bvconcat (0b101 :: SymIntN 3) (0b010 :: SymIntN 3)
  -- 0b101010
  bvconcat :: bv1 -> bv2 -> bv3

-- | Bitwise extension of the given bit vector values.
class BVExtend bv1 (n :: Nat) bv2 | bv1 n -> bv2 where
  -- | Bitwise zero extension of the given bit vector values.
  --
  -- >>> bvzeroExtend (Proxy @6) (0b101 :: SymIntN 3)
  -- 0b000101
  bvzeroExtend ::
    -- | Desired output width
    proxy n ->
    -- | Bit vector to extend
    bv1 ->
    bv2

  -- | Bitwise signed extension of the given bit vector values.
  --
  -- >>> bvsignExtend (Proxy @6) (0b101 :: SymIntN 3)
  -- 0b111101
  bvsignExtend ::
    -- | Desired output width
    proxy n ->
    -- | Bit vector to extend
    bv1 ->
    bv2

  -- | Bitwise extension of the given bit vector values.
  -- Signedness is determined by the input bit vector type.
  --
  -- >>> bvextend (Proxy @6) (0b101 :: SymIntN 3)
  -- 0b111101
  -- >>> bvextend (Proxy @6) (0b001 :: SymIntN 3)
  -- 0b000001
  -- >>> bvextend (Proxy @6) (0b101 :: SymWordN 3)
  -- 0b000101
  -- >>> bvextend (Proxy @6) (0b001 :: SymWordN 3)
  -- 0b000001
  bvextend ::
    -- | Desired output width
    proxy n ->
    -- | Bit vector to extend
    bv1 ->
    bv2

-- | Slicing out a smaller bit vector from a larger one, selecting a slice with
-- width @w@ starting from index @ix@.
class BVSelect bv1 (ix :: Nat) (w :: Nat) bv2 | bv1 w -> bv2 where
  -- | Slicing out a smaller bit vector from a larger one, selecting a slice with
  -- width @w@ starting from index @ix@.
  --
  -- The indices are counting from zero from the least significant bit.
  --
  -- >>> bvselect (Proxy @1) (Proxy @3) (conc 0b001010 :: SymIntN 6)
  -- 0b101
  bvselect ::
    -- | Index to start selecting from
    proxy ix ->
    -- | Desired output width, @0 <= ix@ and @ix + w < n@ must hold where @n@ is
    -- the size of the input bit vector
    proxy w ->
    -- | Bit vector to select from
    bv1 ->
    bv2

-- | Extract a smaller bit vector from a larger one from bits @i@ down to @j@.
--
-- The indices are counting from zero from the least significant bit.
-- >>> bvextract (Proxy @3) (Proxy @1) (conc 0b001010 :: SymIntN 6)
-- 0b101
bvextract ::
  forall proxy i j bv1 bv2.
  (BVSelect bv1 j (i - j + 1) bv2) =>
  -- | The start position to extract from, @0 <= i < n@ must hold where @n@ is
  -- the size of the output bit vector
  proxy i ->
  -- | The end position to extract from, @0 <= j <= i@ must hold
  proxy j ->
  -- | Bit vector to extract from
  bv1 ->
  bv2
bvextract _ _ = bvselect (Proxy @j) (Proxy @(i - j + 1))
{-# INLINE bvextract #-}
