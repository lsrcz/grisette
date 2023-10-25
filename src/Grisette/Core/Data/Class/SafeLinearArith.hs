{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Core.Data.Class.SafeLinearArith
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.Class.SafeLinearArith
  ( ArithException (..),
    SafeLinearArith (..),
  )
where

import Control.Exception (ArithException (DivideByZero, Overflow, Underflow))
import Control.Monad.Except (MonadError (throwError))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Typeable (Proxy (Proxy), type (:~:) (Refl))
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeNats (KnownNat, sameNat, type (<=))
import Grisette.Core.Control.Monad.Union (MonadUnion)
import Grisette.Core.Data.BV
  ( BitwidthMismatch (BitwidthMismatch),
    IntN,
    SomeIntN (SomeIntN),
    SomeWordN (SomeWordN),
    WordN,
  )
import Grisette.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Core.Data.Class.SOrd (SOrd)
import Grisette.Core.Data.Class.SimpleMergeable
  ( merge,
    mrgIf,
    mrgSingle,
  )
import Grisette.Core.Data.Class.Solvable (Solvable (con))

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> import Control.Monad.Except

-- | Safe division with monadic error handling in multi-path
-- execution. These procedures throw an exception when overflow or underflow happens.
-- The result should be able to handle errors with `MonadError`.
class (SOrd a, Num a, Mergeable a, Mergeable e) => SafeLinearArith e a | a -> e where
  -- | Safe '+' with monadic error handling in multi-path execution.
  -- Overflows or underflows are treated as errors.
  --
  -- >>> safeAdd (ssym "a") (ssym "b") :: ExceptT ArithException UnionM SymInteger
  -- ExceptT {Right (+ a b)}
  -- >>> safeAdd (ssym "a") (ssym "b") :: ExceptT ArithException UnionM (SymIntN 4)
  -- ExceptT {If (ite (< 0x0 a) (&& (< 0x0 b) (< (+ a b) 0x0)) (&& (< a 0x0) (&& (< b 0x0) (<= 0x0 (+ a b))))) (If (< 0x0 a) (Left arithmetic overflow) (Left arithmetic underflow)) (Right (+ a b))}
  safeAdd :: (MonadError e uf, MonadUnion uf) => a -> a -> uf a

  -- | Safe 'negate' with monadic error handling in multi-path execution.
  -- Overflows or underflows are treated as errors.
  --
  -- >>> safeNeg (ssym "a") :: ExceptT ArithException UnionM SymInteger
  -- ExceptT {Right (- a)}
  -- >>> safeNeg (ssym "a") :: ExceptT ArithException UnionM (SymIntN 4)
  -- ExceptT {If (= a 0x8) (Left arithmetic overflow) (Right (- a))}
  safeNeg :: (MonadError e uf, MonadUnion uf) => a -> uf a

  -- | Safe '-' with monadic error handling in multi-path execution.
  -- Overflows or underflows are treated as errors.
  --
  -- >>> safeMinus (ssym "a") (ssym "b") :: ExceptT ArithException UnionM SymInteger
  -- ExceptT {Right (+ a (- b))}
  -- >>> safeMinus (ssym "a") (ssym "b") :: ExceptT ArithException UnionM (SymIntN 4)
  -- ExceptT {If (ite (<= 0x0 a) (&& (< b 0x0) (< (+ a (- b)) 0x0)) (&& (< a 0x0) (&& (< 0x0 b) (< 0x0 (+ a (- b)))))) (If (<= 0x0 a) (Left arithmetic overflow) (Left arithmetic underflow)) (Right (+ a (- b)))}
  safeMinus :: (MonadError e uf, MonadUnion uf) => a -> a -> uf a

  -- | Safe '+' with monadic error handling in multi-path execution.
  -- Overflows or underflows are treated as errors.
  -- The error is transformed.
  safeAdd' :: (MonadError e' uf, MonadUnion uf, Mergeable e') => (e -> e') -> a -> a -> uf a

  -- | Safe 'negate' with monadic error handling in multi-path execution.
  -- Overflows or underflows are treated as errors.
  -- The error is transformed.
  safeNeg' :: (MonadError e' uf, MonadUnion uf, Mergeable e') => (e -> e') -> a -> uf a

  -- | Safe '-' with monadic error handling in multi-path execution.
  -- Overflows or underflows are treated as errors.
  -- The error is transformed.
  safeMinus' :: (MonadError e' uf, MonadUnion uf, Mergeable e') => (e -> e') -> a -> a -> uf a

instance SafeLinearArith ArithException Integer where
  safeAdd l r = mrgSingle (l + r)
  safeNeg l = mrgSingle (-l)
  safeMinus l r = mrgSingle (l - r)
  safeAdd' _ l r = mrgSingle (l + r)
  safeNeg' _ l = mrgSingle (-l)
  safeMinus' _ l r = mrgSingle (l - r)

#define SAFE_LINARITH_SIGNED_CONCRETE_BODY \
  safeAdd l r = let res = l + r in \
    mrgIf (con $ l > 0 && r > 0 && res < 0) \
          (throwError Overflow) \
          (mrgIf (con $ l < 0 && r < 0 && res >= 0) \
                 (throwError Underflow) \
                 (return res));\
  safeAdd' t' l r = let res = l + r in \
    mrgIf (con $ l > 0 && r > 0 && res < 0) \
          (throwError (t' Overflow)) \
          (mrgIf (con $ l < 0 && r < 0 && res >= 0) \
                 (throwError (t' Underflow)) \
                 (return res)); \
  safeMinus l r = let res = l - r in \
    mrgIf (con $ l >= 0 && r < 0 && res < 0) \
          (throwError Overflow) \
          (mrgIf (con $ l < 0 && r > 0 && res > 0) \
                 (throwError Underflow) \
                 (return res));\
  safeMinus' t' l r = let res = l - r in \
    mrgIf (con $ l >= 0 && r < 0 && res < 0) \
          (throwError (t' Overflow)) \
          (mrgIf (con $ l < 0 && r > 0 && res > 0) \
                 (throwError (t' Underflow)) \
                 (return res)); \
  safeNeg v = mrgIf (con $ v == minBound) (throwError Overflow) (return $ -v);\
  safeNeg' t' v = mrgIf (con $ v == minBound) (throwError (t' Overflow)) (return $ -v)

#define SAFE_LINARITH_SIGNED_CONCRETE(type) \
instance SafeLinearArith ArithException type where \
  SAFE_LINARITH_SIGNED_CONCRETE_BODY

#define SAFE_LINARITH_SIGNED_BV_CONCRETE(type) \
instance (KnownNat n, 1 <= n) => SafeLinearArith ArithException (type n) where \
  SAFE_LINARITH_SIGNED_CONCRETE_BODY

#define SAFE_LINARITH_UNSIGNED_CONCRETE_BODY \
  safeAdd l r = let res = l + r in \
    mrgIf (con $ l > res || r > res) \
          (throwError Overflow) \
          (return res);\
  safeAdd' t' l r = let res = l + r in \
    mrgIf (con $ l > res || r > res) \
          (throwError (t' Overflow)) \
          (return res); \
  safeMinus l r = \
    mrgIf (con $ r > l) \
          (throwError Underflow) \
          (return $ l - r);\
  safeMinus' t' l r = \
    mrgIf (con $ r > l) \
          (throwError $ t' Underflow) \
          (return $ l - r);\
  safeNeg v = mrgIf (con $ v /= 0) (throwError Underflow) (return $ -v);\
  safeNeg' t' v = mrgIf (con $ v /= 0) (throwError (t' Underflow)) (return $ -v)

#define SAFE_LINARITH_UNSIGNED_CONCRETE(type) \
instance SafeLinearArith ArithException type where \
  SAFE_LINARITH_UNSIGNED_CONCRETE_BODY

#define SAFE_LINARITH_UNSIGNED_BV_CONCRETE(type) \
instance (KnownNat n, 1 <= n) => SafeLinearArith ArithException (type n) where \
  SAFE_LINARITH_UNSIGNED_CONCRETE_BODY

#define SAFE_LINARITH_SOME_CONCRETE(type, ctype) \
instance SafeLinearArith (Either BitwidthMismatch ArithException) type where \
  safeAdd (type (l :: ctype l)) (type (r :: ctype r)) = merge (\
    case sameNat (Proxy @l) (Proxy @r) of \
      Just Refl -> type <$> safeAdd' Right l r; \
      _ -> throwError $ Left BitwidthMismatch); \
  safeAdd' t (type (l :: ctype l)) (type (r :: ctype r)) = merge (\
    case sameNat (Proxy @l) (Proxy @r) of \
      Just Refl -> type <$> safeAdd' (t . Right) l r; \
      _ -> let t' = t; _ = t' in throwError $ t' $ Left BitwidthMismatch); \
  safeMinus (type (l :: ctype l)) (type (r :: ctype r)) = merge (\
    case sameNat (Proxy @l) (Proxy @r) of \
      Just Refl -> type <$> safeMinus' Right l r; \
      _ -> throwError $ Left BitwidthMismatch); \
  safeMinus' t (type (l :: ctype l)) (type (r :: ctype r)) = merge (\
    case sameNat (Proxy @l) (Proxy @r) of \
      Just Refl -> type <$> safeMinus' (t . Right) l r; \
      _ -> let t' = t; _ = t' in throwError $ t' $ Left BitwidthMismatch); \
  safeNeg (type l) = merge $ type <$> safeNeg' Right l; \
  safeNeg' t (type l) = merge $ type <$> safeNeg' (t . Right) l

#if 1
SAFE_LINARITH_SIGNED_CONCRETE(Int8)
SAFE_LINARITH_SIGNED_CONCRETE(Int16)
SAFE_LINARITH_SIGNED_CONCRETE(Int32)
SAFE_LINARITH_SIGNED_CONCRETE(Int64)
SAFE_LINARITH_SIGNED_CONCRETE(Int)
SAFE_LINARITH_SIGNED_BV_CONCRETE(IntN)
SAFE_LINARITH_SOME_CONCRETE(SomeIntN, IntN)
SAFE_LINARITH_UNSIGNED_CONCRETE(Word8)
SAFE_LINARITH_UNSIGNED_CONCRETE(Word16)
SAFE_LINARITH_UNSIGNED_CONCRETE(Word32)
SAFE_LINARITH_UNSIGNED_CONCRETE(Word64)
SAFE_LINARITH_UNSIGNED_CONCRETE(Word)
SAFE_LINARITH_UNSIGNED_BV_CONCRETE(WordN)
SAFE_LINARITH_SOME_CONCRETE(SomeWordN, WordN)
#endif
