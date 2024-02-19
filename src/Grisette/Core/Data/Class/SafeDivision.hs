{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
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
-- Module      :   Grisette.Core.Data.Class.SafeDivision
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.Class.SafeDivision
  ( ArithException (..),
    SafeDivision (..),
  )
where

import Control.Exception (ArithException (DivideByZero, Overflow, Underflow))
import Control.Monad.Except (MonadError (throwError))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeNats (KnownNat, type (<=))
import Grisette.Core.Control.Monad.Union (MonadUnion)
import Grisette.Core.Data.BV
  ( IntN,
    WordN,
  )
import Grisette.Core.Data.Class.LogicalOp (LogicalOp ((.&&)))
import Grisette.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Core.Data.Class.SEq (SEq ((.==)))
import Grisette.Core.Data.Class.SimpleMergeable
  ( mrgIf,
  )
import Grisette.Core.Data.Class.Solvable (Solvable (con))
import Grisette.Core.Data.Class.TryMerge
  ( TryMerge,
    mrgPure,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Integral
  ( pevalDivBoundedIntegralTerm,
    pevalDivIntegralTerm,
    pevalModBoundedIntegralTerm,
    pevalModIntegralTerm,
    pevalQuotBoundedIntegralTerm,
    pevalQuotIntegralTerm,
    pevalRemBoundedIntegralTerm,
    pevalRemIntegralTerm,
  )
import Grisette.IR.SymPrim.Data.SymPrim
  ( SymIntN (SymIntN),
    SymInteger (SymInteger),
    SymWordN (SymWordN),
  )
import Grisette.Lib.Control.Monad (mrgReturn)
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Grisette.Lib.Data.Functor (mrgFmap)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> import Control.Monad.Except

-- | Safe division with monadic error handling in multi-path
-- execution. These procedures throw an exception when the
-- divisor is zero. The result should be able to handle errors with
-- `MonadError`.
class (MonadError e m, TryMerge m, Mergeable a) => SafeDivision e a m where
  -- | Safe signed 'div' with monadic error handling in multi-path execution.
  --
  -- >>> safeDiv (ssym "a") (ssym "b") :: ExceptT ArithException UnionM SymInteger
  -- ExceptT {If (= b 0) (Left divide by zero) (Right (div a b))}
  safeDiv :: a -> a -> m a
  safeDiv l r = mrgFmap fst $ safeDivMod l r
  {-# INLINE safeDiv #-}

  -- | Safe signed 'mod' with monadic error handling in multi-path execution.
  --
  -- >>> safeMod (ssym "a") (ssym "b") :: ExceptT ArithException UnionM SymInteger
  -- ExceptT {If (= b 0) (Left divide by zero) (Right (mod a b))}
  safeMod :: a -> a -> m a
  safeMod l r = mrgFmap snd $ safeDivMod l r
  {-# INLINE safeMod #-}

  -- | Safe signed 'divMod' with monadic error handling in multi-path execution.
  --
  -- >>> safeDivMod (ssym "a") (ssym "b") :: ExceptT ArithException UnionM (SymInteger, SymInteger)
  -- ExceptT {If (= b 0) (Left divide by zero) (Right ((div a b),(mod a b)))}
  safeDivMod :: a -> a -> m (a, a)
  safeDivMod l r = do
    d <- safeDiv l r
    m <- safeMod l r
    mrgReturn (d, m)
  {-# INLINE safeDivMod #-}

  -- | Safe signed 'quot' with monadic error handling in multi-path execution.
  safeQuot :: a -> a -> m a
  safeQuot l r = mrgFmap fst $ safeQuotRem l r
  {-# INLINE safeQuot #-}

  -- | Safe signed 'rem' with monadic error handling in multi-path execution.
  safeRem :: a -> a -> m a
  safeRem l r = mrgFmap snd $ safeQuotRem l r
  {-# INLINE safeRem #-}

  -- | Safe signed 'quotRem' with monadic error handling in multi-path execution.
  safeQuotRem :: a -> a -> m (a, a)
  safeQuotRem l r = do
    q <- safeQuot l r
    m <- safeRem l r
    mrgReturn (q, m)
  {-# INLINE safeQuotRem #-}

  {-# MINIMAL
    ((safeDiv, safeMod) | safeDivMod),
    ((safeQuot, safeRem) | safeQuotRem)
    #-}

concreteSafeDivisionHelper ::
  (MonadError ArithException m, TryMerge m, Integral a, Mergeable r) =>
  (a -> a -> r) ->
  a ->
  a ->
  m r
concreteSafeDivisionHelper f l r
  | r == 0 = mrgThrowError DivideByZero
  | otherwise = mrgReturn $ f l r

concreteSignedBoundedSafeDivisionHelper ::
  ( MonadError ArithException m,
    TryMerge m,
    Integral a,
    Bounded a,
    Mergeable r
  ) =>
  (a -> a -> r) ->
  a ->
  a ->
  m r
concreteSignedBoundedSafeDivisionHelper f l r
  | r == 0 = mrgThrowError DivideByZero
  | l == minBound && r == -1 = mrgThrowError Overflow
  | otherwise = mrgReturn $ f l r

#define QUOTE() '
#define QID(a) a
#define QRIGHT(a) QID(a)'

#define QRIGHTT(a) QID(a)' t'
#define QRIGHTU(a) QID(a)' _'

#define SAFE_DIVISION_CONCRETE(type) \
instance (MonadError ArithException m, TryMerge m) => \
  SafeDivision ArithException type m where \
  safeDiv = concreteSafeDivisionHelper div; \
  safeMod = concreteSafeDivisionHelper mod; \
  safeDivMod = concreteSafeDivisionHelper divMod; \
  safeQuot = concreteSafeDivisionHelper quot; \
  safeRem = concreteSafeDivisionHelper rem; \
  safeQuotRem = concreteSafeDivisionHelper quotRem

#define SAFE_DIVISION_CONCRETE_SIGNED_BOUNDED(type) \
instance (MonadError ArithException m, TryMerge m) => \
  SafeDivision ArithException type m where \
  safeDiv = concreteSignedBoundedSafeDivisionHelper div; \
  safeMod = concreteSafeDivisionHelper mod; \
  safeDivMod = concreteSignedBoundedSafeDivisionHelper divMod; \
  safeQuot = concreteSignedBoundedSafeDivisionHelper quot; \
  safeRem = concreteSafeDivisionHelper rem; \
  safeQuotRem = concreteSignedBoundedSafeDivisionHelper quotRem

#define SAFE_DIVISION_CONCRETE_BV(type) \
instance \
  (MonadError ArithException m, TryMerge m, KnownNat n, 1 <= n) => \
  SafeDivision ArithException (type n) m where \
  safeDiv = concreteSafeDivisionHelper div; \
  safeMod = concreteSafeDivisionHelper mod; \
  safeDivMod = concreteSafeDivisionHelper divMod; \
  safeQuot = concreteSafeDivisionHelper quot; \
  safeRem = concreteSafeDivisionHelper rem; \
  safeQuotRem = concreteSafeDivisionHelper quotRem

#if 1
SAFE_DIVISION_CONCRETE(Integer)
SAFE_DIVISION_CONCRETE_SIGNED_BOUNDED(Int8)
SAFE_DIVISION_CONCRETE_SIGNED_BOUNDED(Int16)
SAFE_DIVISION_CONCRETE_SIGNED_BOUNDED(Int32)
SAFE_DIVISION_CONCRETE_SIGNED_BOUNDED(Int64)
SAFE_DIVISION_CONCRETE_SIGNED_BOUNDED(Int)
SAFE_DIVISION_CONCRETE(Word8)
SAFE_DIVISION_CONCRETE(Word16)
SAFE_DIVISION_CONCRETE(Word32)
SAFE_DIVISION_CONCRETE(Word64)
SAFE_DIVISION_CONCRETE(Word)

instance
  (MonadError ArithException m, TryMerge m, KnownNat n, 1 <= n) =>
  SafeDivision ArithException (IntN n) m where
  safeDiv = concreteSignedBoundedSafeDivisionHelper div
  safeMod = concreteSafeDivisionHelper mod
  safeDivMod = concreteSignedBoundedSafeDivisionHelper divMod
  safeQuot = concreteSignedBoundedSafeDivisionHelper quot
  safeRem = concreteSafeDivisionHelper rem
  safeQuotRem = concreteSignedBoundedSafeDivisionHelper quotRem

instance
  (MonadError ArithException m, TryMerge m, KnownNat n, 1 <= n) =>
  SafeDivision ArithException (WordN n) m where
  safeDiv = concreteSafeDivisionHelper div
  safeMod = concreteSafeDivisionHelper mod
  safeDivMod = concreteSafeDivisionHelper divMod
  safeQuot = concreteSafeDivisionHelper quot
  safeRem = concreteSafeDivisionHelper rem
  safeQuotRem = concreteSafeDivisionHelper quotRem
#endif

#define SAFE_DIVISION_SYMBOLIC_FUNC(name, type, op) \
name (type l) rs@(type r) = \
  mrgIf \
    (rs .== con 0) \
    (throwError DivideByZero) \
    (mrgPure $ type $ op l r); \

#define SAFE_DIVISION_SYMBOLIC_FUNC2(name, type, op1, op2) \
name (type l) rs@(type r) = \
  mrgIf \
    (rs .== con 0) \
    (throwError DivideByZero) \
    (mrgPure (type $ op1 l r, type $ op2 l r)); \

#if 1
instance
  (MonadUnion m, MonadError ArithException m) =>
  SafeDivision ArithException SymInteger m where
  SAFE_DIVISION_SYMBOLIC_FUNC(safeDiv, SymInteger, pevalDivIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC(safeMod, SymInteger, pevalModIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC(safeQuot, SymInteger, pevalQuotIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC(safeRem, SymInteger, pevalRemIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC2(safeDivMod, SymInteger, pevalDivIntegralTerm, pevalModIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC2(safeQuotRem, SymInteger, pevalQuotIntegralTerm, pevalRemIntegralTerm)
#endif

#define SAFE_DIVISION_SYMBOLIC_FUNC_BOUNDED_SIGNED(name, type, op) \
name ls@(type l) rs@(type r) = \
  mrgIf \
    (rs .== con 0) \
    (throwError DivideByZero) \
    (mrgIf (rs .== con (-1) .&& ls .== con minBound) \
      (throwError Overflow) \
      (mrgPure $ type $ op l r)); \

#define SAFE_DIVISION_SYMBOLIC_FUNC2_BOUNDED_SIGNED(name, type, op1, op2) \
name ls@(type l) rs@(type r) = \
  mrgIf \
    (rs .== con 0) \
    (throwError DivideByZero) \
    (mrgIf (rs .== con (-1) .&& ls .== con minBound) \
      (throwError Overflow) \
      (mrgPure (type $ op1 l r, type $ op2 l r))); \

#if 1
instance
  (MonadError ArithException m, MonadUnion m, KnownNat n, 1 <= n) =>
  SafeDivision ArithException (SymIntN n) m where
  SAFE_DIVISION_SYMBOLIC_FUNC_BOUNDED_SIGNED(safeDiv, SymIntN, pevalDivBoundedIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC(safeMod, SymIntN, pevalModBoundedIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC_BOUNDED_SIGNED(safeQuot, SymIntN, pevalQuotBoundedIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC(safeRem, SymIntN, pevalRemBoundedIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC2_BOUNDED_SIGNED(safeDivMod, SymIntN, pevalDivBoundedIntegralTerm, pevalModBoundedIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC2_BOUNDED_SIGNED(safeQuotRem, SymIntN, pevalQuotBoundedIntegralTerm, pevalRemBoundedIntegralTerm)
#endif

#if 1
instance
  (MonadError ArithException m, MonadUnion m, KnownNat n, 1 <= n) =>
  SafeDivision ArithException (SymWordN n) m where
  SAFE_DIVISION_SYMBOLIC_FUNC(safeDiv, SymWordN, pevalDivIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC(safeMod, SymWordN, pevalModIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC(safeQuot, SymWordN, pevalQuotIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC(safeRem, SymWordN, pevalRemIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC2(safeDivMod, SymWordN, pevalDivIntegralTerm, pevalModIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC2(safeQuotRem, SymWordN, pevalQuotIntegralTerm, pevalRemIntegralTerm)
#endif
