{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.SafeLinearArith
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.SafeLinearArith
  ( ArithException (..),
    SafeLinearArith (..),
  )
where

import Control.Exception (ArithException (DivideByZero, Overflow, Underflow))
import Control.Monad.Except (MonadError (throwError))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeNats (KnownNat, type (<=))
import Grisette.Internal.Core.Control.Monad.Class.Union (MonadUnion)
import Grisette.Internal.Core.Data.Class.AsKey (AsKey (AsKey))
import Grisette.Internal.Core.Data.Class.LogicalOp
  ( LogicalOp ((.&&), (.||)),
  )
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.SimpleMergeable
  ( mrgIf,
  )
import Grisette.Internal.Core.Data.Class.Solvable (Solvable (con))
import Grisette.Internal.Core.Data.Class.SymEq (SymEq ((./=), (.==)))
import Grisette.Internal.Core.Data.Class.SymOrd (SymOrd ((.<), (.>), (.>=)))
import Grisette.Internal.Core.Data.Class.TryMerge
  ( TryMerge,
    mrgSingle,
    tryMerge,
  )
import Grisette.Internal.SymPrim.BV
  ( IntN,
    WordN,
  )
import Grisette.Internal.SymPrim.SymBV
  ( SymIntN,
    SymWordN,
  )
import Grisette.Internal.SymPrim.SymInteger (SymInteger)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
-- >>> import Control.Monad.Except
-- >>> import Control.Exception

-- | Safe division with monadic error handling in multi-path
-- execution. These procedures throw an exception when overflow or underflow happens.
-- The result should be able to handle errors with `MonadError`.
class (MonadError e m, TryMerge m, Mergeable a) => SafeLinearArith e a m where
  -- | Safe '+' with monadic error handling in multi-path execution.
  -- Overflows or underflows are treated as errors.
  --
  -- >>> safeAdd (ssym "a") (ssym "b") :: ExceptT ArithException Union SymInteger
  -- ExceptT {Right (+ a b)}
  -- >>> safeAdd (ssym "a") (ssym "b") :: ExceptT ArithException Union (SymIntN 4)
  -- ExceptT {If (ite (< 0x0 a) (&& (< 0x0 b) (< (+ a b) 0x0)) (&& (< a 0x0) (&& (< b 0x0) (<= 0x0 (+ a b))))) (If (< 0x0 a) (Left arithmetic overflow) (Left arithmetic underflow)) (Right (+ a b))}
  safeAdd :: a -> a -> m a

  -- | Safe 'negate' with monadic error handling in multi-path execution.
  -- Overflows or underflows are treated as errors.
  --
  -- >>> safeNeg (ssym "a") :: ExceptT ArithException Union SymInteger
  -- ExceptT {Right (- a)}
  -- >>> safeNeg (ssym "a") :: ExceptT ArithException Union (SymIntN 4)
  -- ExceptT {If (= a 0x8) (Left arithmetic overflow) (Right (- a))}
  safeNeg :: a -> m a

  -- | Safe '-' with monadic error handling in multi-path execution.
  -- Overflows or underflows are treated as errors.
  --
  -- >>> safeSub (ssym "a") (ssym "b") :: ExceptT ArithException Union SymInteger
  -- ExceptT {Right (+ a (- b))}
  -- >>> safeSub (ssym "a") (ssym "b") :: ExceptT ArithException Union (SymIntN 4)
  -- ExceptT {If (ite (<= 0x0 a) (&& (< b 0x0) (< (+ a (- b)) 0x0)) (&& (< a 0x0) (&& (< 0x0 b) (< 0x0 (+ a (- b)))))) (If (<= 0x0 a) (Left arithmetic overflow) (Left arithmetic underflow)) (Right (+ a (- b)))}
  safeSub :: a -> a -> m a

instance
  (MonadError ArithException m, TryMerge m) =>
  SafeLinearArith ArithException Integer m
  where
  safeAdd l r = mrgSingle (l + r)
  safeNeg l = mrgSingle (-l)
  safeSub l r = mrgSingle (l - r)

#define SAFE_LINARITH_SIGNED_CONCRETE_BODY \
  safeAdd l r = let res = l + r in \
    if l > 0 && r > 0 && res < 0 \
      then tryMerge $ throwError Overflow \
      else if l < 0 && r < 0 && res >= 0 \
        then tryMerge $ throwError Underflow \
        else mrgSingle res;\
  safeSub l r = let res = l - r in \
    if l >= 0 && r < 0 && res < 0 \
      then tryMerge $ throwError Overflow \
      else if l < 0 && r > 0 && res > 0 \
        then tryMerge $ throwError Underflow \
        else mrgSingle res;\
  safeNeg v = \
    if v == minBound \
      then tryMerge $ throwError Overflow \
      else mrgSingle $ -v

#define SAFE_LINARITH_SIGNED_CONCRETE(type) \
instance \
  (MonadError ArithException m, TryMerge m) => \
  SafeLinearArith ArithException type m \
  where \
  SAFE_LINARITH_SIGNED_CONCRETE_BODY

#define SAFE_LINARITH_SIGNED_BV_CONCRETE(type) \
instance \
  (MonadError ArithException m, TryMerge m, KnownNat n, 1 <= n) => \
  SafeLinearArith ArithException (type n) m \
  where \
  SAFE_LINARITH_SIGNED_CONCRETE_BODY

#define SAFE_LINARITH_UNSIGNED_CONCRETE_BODY \
  safeAdd l r = let res = l + r in \
    if l > res || r > res \
      then tryMerge $ throwError Overflow \
      else mrgSingle res;\
  safeSub l r = \
    if r > l \
      then tryMerge $ throwError Underflow \
      else mrgSingle $ l - r;\
  safeNeg v = if v /= 0 then tryMerge $ throwError Underflow else mrgSingle $ -v

#define SAFE_LINARITH_UNSIGNED_CONCRETE(type) \
instance \
  (MonadError ArithException m, TryMerge m) => \
  SafeLinearArith ArithException type m \
  where \
  SAFE_LINARITH_UNSIGNED_CONCRETE_BODY

#define SAFE_LINARITH_UNSIGNED_BV_CONCRETE(type) \
instance \
  (MonadError ArithException m, TryMerge m, KnownNat n, 1 <= n) => \
  SafeLinearArith ArithException (type n) m \
  where \
  SAFE_LINARITH_UNSIGNED_CONCRETE_BODY

#if 1
SAFE_LINARITH_SIGNED_CONCRETE(Int8)
SAFE_LINARITH_SIGNED_CONCRETE(Int16)
SAFE_LINARITH_SIGNED_CONCRETE(Int32)
SAFE_LINARITH_SIGNED_CONCRETE(Int64)
SAFE_LINARITH_SIGNED_CONCRETE(Int)
SAFE_LINARITH_SIGNED_BV_CONCRETE(IntN)
SAFE_LINARITH_UNSIGNED_CONCRETE(Word8)
SAFE_LINARITH_UNSIGNED_CONCRETE(Word16)
SAFE_LINARITH_UNSIGNED_CONCRETE(Word32)
SAFE_LINARITH_UNSIGNED_CONCRETE(Word64)
SAFE_LINARITH_UNSIGNED_CONCRETE(Word)
SAFE_LINARITH_UNSIGNED_BV_CONCRETE(WordN)
#endif

instance
  (MonadError ArithException m, TryMerge m) =>
  SafeLinearArith ArithException SymInteger m
  where
  safeAdd ls rs = mrgSingle $ ls + rs
  safeNeg v = mrgSingle $ -v
  safeSub ls rs = mrgSingle $ ls - rs

instance
  (MonadError ArithException m, MonadUnion m, KnownNat n, 1 <= n) =>
  SafeLinearArith ArithException (SymIntN n) m
  where
  safeAdd ls rs =
    mrgIf
      (ls .> 0)
      (mrgIf (rs .> 0 .&& res .< 0) (throwError Overflow) (return res))
      ( mrgIf
          (ls .< 0 .&& rs .< 0 .&& res .>= 0)
          (throwError Underflow)
          (mrgSingle res)
      )
    where
      res = ls + rs
  safeNeg v = mrgIf (v .== con minBound) (throwError Overflow) (mrgSingle $ -v)
  safeSub ls rs =
    mrgIf
      (ls .>= 0)
      (mrgIf (rs .< 0 .&& res .< 0) (throwError Overflow) (return res))
      ( mrgIf
          (ls .< 0 .&& rs .> 0 .&& res .> 0)
          (throwError Underflow)
          (mrgSingle res)
      )
    where
      res = ls - rs

instance
  (MonadError ArithException m, MonadUnion m, KnownNat n, 1 <= n) =>
  SafeLinearArith ArithException (SymWordN n) m
  where
  safeAdd ls rs =
    mrgIf
      (ls .> res .|| rs .> res)
      (throwError Overflow)
      (mrgSingle res)
    where
      res = ls + rs
  safeNeg v = mrgIf (v ./= 0) (throwError Underflow) (mrgSingle v)
  safeSub ls rs =
    mrgIf
      (rs .> ls)
      (throwError Underflow)
      (mrgSingle res)
    where
      res = ls - rs

instance (SafeLinearArith e a m) => SafeLinearArith e (AsKey a) m where
  safeAdd (AsKey a) (AsKey b) = do
    r <- safeAdd a b
    mrgSingle $ AsKey r
  safeNeg (AsKey a) = do
    r <- safeNeg a
    mrgSingle $ AsKey r
  safeSub (AsKey a) (AsKey b) = do
    r <- safeSub a b
    mrgSingle $ AsKey r
