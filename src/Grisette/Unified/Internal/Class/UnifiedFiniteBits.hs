{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Unified.Internal.Class.UnifiedFiniteBits
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Unified.Internal.Class.UnifiedFiniteBits
  ( UnifiedFiniteBits (..),
    symTestBit,
    symSetBitTo,
    symFromBits,
    symBitBlast,
    symLsb,
    symMsb,
    symPopCount,
    symCountLeadingZeros,
    symCountTrailingZeros,
  )
where

import Data.Bits
  ( Bits (popCount, testBit),
    FiniteBits (countLeadingZeros, countTrailingZeros),
  )
import Data.Data (Typeable)
import Data.Type.Bool (If)
import GHC.TypeLits (KnownNat, type (<=))
import Grisette.Internal.Core.Data.Class.SymFiniteBits
  ( FromBits,
    SymFiniteBits,
    setBitTo,
  )
import qualified Grisette.Internal.Core.Data.Class.SymFiniteBits as SymFiniteBits
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.SomeBV
  ( SomeIntN,
    SomeSymIntN,
    SomeSymWordN,
    SomeWordN,
  )
import Grisette.Internal.SymPrim.SymBV (SymIntN, SymWordN)
import Grisette.Unified.Internal.Class.UnifiedITEOp
  ( UnifiedITEOp (withBaseITEOp),
  )
import Grisette.Unified.Internal.EvalModeTag (EvalModeTag (Con, Sym), IsConMode)
import Grisette.Unified.Internal.UnifiedBool (UnifiedBool (GetBool))
import Grisette.Unified.Internal.Util (withMode)

-- | Unified `Grisette.Internal.Core.Data.Class.SymFiniteBits.symTestBit`.
symTestBit ::
  forall mode a.
  (Typeable mode, UnifiedFiniteBits mode a) =>
  a ->
  Int ->
  GetBool mode
symTestBit a i =
  withMode @mode
    (withBaseFiniteBits @mode @a (testBit a i))
    (withBaseFiniteBits @mode @a (SymFiniteBits.symTestBit a i))

-- | Unified `Grisette.Internal.Core.Data.Class.SymFiniteBits.symSetBitTo`.
symSetBitTo ::
  forall mode a.
  (Typeable mode, UnifiedFiniteBits mode a) =>
  a ->
  Int ->
  GetBool mode ->
  a
symSetBitTo a i b =
  withMode @mode
    (withBaseFiniteBits @mode @a (setBitTo a i b))
    (withBaseFiniteBits @mode @a (SymFiniteBits.symSetBitTo a i b))

-- | Unified `Grisette.Internal.Core.Data.Class.SymFiniteBits.symFromBits`.
symFromBits ::
  forall mode a.
  (Typeable mode, UnifiedFiniteBits mode a) =>
  [GetBool mode] ->
  a
symFromBits bits =
  withMode @mode
    (withBaseFiniteBits @mode @a (SymFiniteBits.fromBits bits))
    (withBaseFiniteBits @mode @a (SymFiniteBits.symFromBits bits))

-- | Unified `Grisette.Internal.Core.Data.Class.SymFiniteBits.symBitBlast`.
symBitBlast ::
  forall mode a.
  (Typeable mode, UnifiedFiniteBits mode a) =>
  a ->
  [GetBool mode]
symBitBlast a =
  withMode @mode
    (withBaseFiniteBits @mode @a (SymFiniteBits.bitBlast a))
    (withBaseFiniteBits @mode @a (SymFiniteBits.symBitBlast a))

-- | Unified `Grisette.Internal.Core.Data.Class.SymFiniteBits.symLsb`.
symLsb ::
  forall mode a.
  (Typeable mode, UnifiedFiniteBits mode a) =>
  a ->
  GetBool mode
symLsb a =
  withMode @mode
    (withBaseFiniteBits @mode @a (SymFiniteBits.lsb a))
    (withBaseFiniteBits @mode @a (SymFiniteBits.symLsb a))

-- | Unified `Grisette.Internal.Core.Data.Class.SymFiniteBits.symMsb`.
symMsb ::
  forall mode a.
  (Typeable mode, UnifiedFiniteBits mode a) =>
  a ->
  GetBool mode
symMsb a =
  withMode @mode
    (withBaseFiniteBits @mode @a (SymFiniteBits.msb a))
    (withBaseFiniteBits @mode @a (SymFiniteBits.symMsb a))

-- | Unified `Grisette.Internal.Core.Data.Class.SymFiniteBits.symPopCount`.
symPopCount ::
  forall mode a b.
  (Typeable mode, UnifiedFiniteBits mode a, Num b, UnifiedITEOp mode b) =>
  a ->
  b
symPopCount a =
  withMode @mode
    (withBaseFiniteBits @mode @a (fromIntegral $ popCount a))
    ( withBaseFiniteBits @mode @a $
        withBaseITEOp @mode @b (SymFiniteBits.symPopCount a)
    )

-- | Unified
-- `Grisette.Internal.Core.Data.Class.SymFiniteBits.symCountLeadingZeros`.
symCountLeadingZeros ::
  forall mode a b.
  (Typeable mode, UnifiedFiniteBits mode a, Num b, UnifiedITEOp mode b) =>
  a ->
  b
symCountLeadingZeros a =
  withMode @mode
    (withBaseFiniteBits @mode @a (fromIntegral $ countLeadingZeros a))
    ( withBaseFiniteBits @mode @a $
        withBaseITEOp @mode @b (SymFiniteBits.symCountLeadingZeros a)
    )

-- | Unified
-- `Grisette.Internal.Core.Data.Class.SymFiniteBits.symCountTrailingZeros`.
symCountTrailingZeros ::
  forall mode a b.
  (Typeable mode, UnifiedFiniteBits mode a, Num b, UnifiedITEOp mode b) =>
  a ->
  b
symCountTrailingZeros a =
  withMode @mode
    (withBaseFiniteBits @mode @a (fromIntegral $ countTrailingZeros a))
    ( withBaseFiniteBits @mode @a $
        withBaseITEOp @mode @b (SymFiniteBits.symCountTrailingZeros a)
    )

-- | A class that provides unified equality comparison.
--
-- We use this type class to help resolve the constraints for `FiniteBits`,
-- `FromBits` and `SymFiniteBits`.
class UnifiedFiniteBits mode a where
  withBaseFiniteBits ::
    ((If (IsConMode mode) (FiniteBits a, FromBits a) (SymFiniteBits a)) => r) ->
    r

instance (KnownNat n, 1 <= n) => UnifiedFiniteBits 'Con (WordN n) where
  withBaseFiniteBits r = r

instance (KnownNat n, 1 <= n) => UnifiedFiniteBits 'Con (IntN n) where
  withBaseFiniteBits r = r

instance UnifiedFiniteBits 'Con SomeWordN where
  withBaseFiniteBits r = r

instance UnifiedFiniteBits 'Con SomeIntN where
  withBaseFiniteBits r = r

instance (KnownNat n, 1 <= n) => UnifiedFiniteBits 'Sym (SymWordN n) where
  withBaseFiniteBits r = r

instance (KnownNat n, 1 <= n) => UnifiedFiniteBits 'Sym (SymIntN n) where
  withBaseFiniteBits r = r

instance UnifiedFiniteBits 'Sym SomeSymWordN where
  withBaseFiniteBits r = r

instance UnifiedFiniteBits 'Sym SomeSymIntN where
  withBaseFiniteBits r = r
