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
-- Module      :   Grisette.Internal.Core.Data.Class.SafeDivision
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.SafeDivision
  ( ArithException (..),
    SafeDivision (..),
    DivisionOr (..),
    divOrZero,
    modOrDividend,
    quotOrZero,
    remOrDividend,
    divModOrZeroDividend,
    quotRemOrZeroDividend,
  )
where

import Control.Exception (ArithException (DivideByZero, Overflow, Underflow))
import Control.Monad.Except (MonadError (throwError))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeNats (KnownNat, type (<=))
import Grisette.Internal.Core.Control.Monad.Class.Union (MonadUnion)
import Grisette.Internal.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Internal.Core.Data.Class.LogicalOp (LogicalOp ((.&&)))
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.SimpleMergeable
  ( mrgIf,
  )
import Grisette.Internal.Core.Data.Class.Solvable (Solvable (con))
import Grisette.Internal.Core.Data.Class.SymEq (SymEq ((.==)))
import Grisette.Internal.Core.Data.Class.TryMerge
  ( TryMerge,
    mrgSingle,
    tryMerge,
  )
import Grisette.Internal.SymPrim.BV
  ( IntN,
    WordN,
  )
import Grisette.Internal.SymPrim.Prim.Term
  ( PEvalDivModIntegralTerm
      ( pevalDivIntegralTerm,
        pevalModIntegralTerm,
        pevalQuotIntegralTerm,
        pevalRemIntegralTerm
      ),
  )
import Grisette.Internal.SymPrim.SymBV
  ( SymIntN (SymIntN),
    SymWordN (SymWordN),
  )
import Grisette.Internal.SymPrim.SymInteger (SymInteger (SymInteger))
import Grisette.Lib.Data.Functor (mrgFmap)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
-- >>> import Control.Monad.Except
-- >>> import Control.Exception

-- | Safe division handling with default values returned on exception.
class DivisionOr a where
  -- | Safe 'div' with default value returned on exception.
  --
  -- >>> divOr "d" "a" "b" :: SymInteger
  -- (ite (= b 0) d (div a b))
  divOr :: a -> a -> a -> a

  -- | Safe 'mod' with default value returned on exception.
  --
  -- >>> modOr "d" "a" "b" :: SymInteger
  -- (ite (= b 0) d (mod a b))
  modOr :: a -> a -> a -> a

  -- | Safe 'divMod' with default value returned on exception.
  --
  -- >>> divModOr ("d", "m") "a" "b" :: (SymInteger, SymInteger)
  -- ((ite (= b 0) d (div a b)),(ite (= b 0) m (mod a b)))
  divModOr :: (a, a) -> a -> a -> (a, a)

  -- | Safe 'quot' with default value returned on exception.
  quotOr :: a -> a -> a -> a

  -- | Safe 'rem' with default value returned on exception.
  remOr :: a -> a -> a -> a

  -- | Safe 'quotRem' with default value returned on exception.
  quotRemOr :: (a, a) -> a -> a -> (a, a)

-- | Safe 'div' with 0 returned on exception.
divOrZero :: (DivisionOr a, Num a) => a -> a -> a
divOrZero l = divOr (l - l) l
{-# INLINE divOrZero #-}

-- | Safe 'mod' with dividend returned on exception.
modOrDividend :: (DivisionOr a, Num a) => a -> a -> a
modOrDividend l = modOr l l
{-# INLINE modOrDividend #-}

-- | Safe 'quot' with 0 returned on exception.
quotOrZero :: (DivisionOr a, Num a) => a -> a -> a
quotOrZero l = quotOr (l - l) l
{-# INLINE quotOrZero #-}

-- | Safe 'rem' with dividend returned on exception.
remOrDividend :: (DivisionOr a, Num a) => a -> a -> a
remOrDividend l = remOr l l
{-# INLINE remOrDividend #-}

-- | Safe 'divMod' with 0 returned on exception.
divModOrZeroDividend :: (DivisionOr a, Num a) => a -> a -> (a, a)
divModOrZeroDividend l = divModOr (l - l, l) l
{-# INLINE divModOrZeroDividend #-}

-- | Safe 'quotRem' with 0 returned on exception.
quotRemOrZeroDividend :: (DivisionOr a, Num a) => a -> a -> (a, a)
quotRemOrZeroDividend l = quotRemOr (l - l, l) l
{-# INLINE quotRemOrZeroDividend #-}

-- | Safe division with monadic error handling in multi-path
-- execution. These procedures throw an exception when the
-- divisor is zero. The result should be able to handle errors with
-- `MonadError`.
class (MonadError e m, TryMerge m, Mergeable a, DivisionOr a) => SafeDivision e a m where
  -- | Safe 'div' with monadic error handling in multi-path execution.
  --
  -- >>> safeDiv "a" "b" :: ExceptT ArithException Union SymInteger
  -- ExceptT {If (= b 0) (Left divide by zero) (Right (div a b))}
  safeDiv :: a -> a -> m a
  safeDiv l r = mrgFmap fst $ safeDivMod l r
  {-# INLINE safeDiv #-}

  -- | Safe 'mod' with monadic error handling in multi-path execution.
  --
  -- >>> safeMod "a" "b" :: ExceptT ArithException Union SymInteger
  -- ExceptT {If (= b 0) (Left divide by zero) (Right (mod a b))}
  safeMod :: a -> a -> m a
  safeMod l r = mrgFmap snd $ safeDivMod l r
  {-# INLINE safeMod #-}

  -- | Safe 'divMod' with monadic error handling in multi-path execution.
  --
  -- >>> safeDivMod "a" "b" :: ExceptT ArithException Union (SymInteger, SymInteger)
  -- ExceptT {If (= b 0) (Left divide by zero) (Right ((div a b),(mod a b)))}
  safeDivMod :: a -> a -> m (a, a)
  safeDivMod l r = do
    d <- safeDiv l r
    m <- safeMod l r
    mrgSingle (d, m)
  {-# INLINE safeDivMod #-}

  -- | Safe 'quot' with monadic error handling in multi-path execution.
  safeQuot :: a -> a -> m a
  safeQuot l r = mrgFmap fst $ safeQuotRem l r
  {-# INLINE safeQuot #-}

  -- | Safe 'rem' with monadic error handling in multi-path execution.
  safeRem :: a -> a -> m a
  safeRem l r = mrgFmap snd $ safeQuotRem l r
  {-# INLINE safeRem #-}

  -- | Safe 'quotRem' with monadic error handling in multi-path execution.
  safeQuotRem :: a -> a -> m (a, a)
  safeQuotRem l r = do
    q <- safeQuot l r
    m <- safeRem l r
    mrgSingle (q, m)
  {-# INLINE safeQuotRem #-}

  {-# MINIMAL
    ((safeDiv, safeMod) | safeDivMod),
    ((safeQuot, safeRem) | safeQuotRem)
    #-}

concreteDivisionOrHelper ::
  (Integral a) =>
  (a -> a -> r) ->
  r ->
  a ->
  a ->
  r
concreteDivisionOrHelper f d l r
  | r == 0 = d
  | otherwise = f l r

concreteSafeDivisionHelper ::
  (MonadError ArithException m, TryMerge m, Integral a, Mergeable r) =>
  (a -> a -> r) ->
  a ->
  a ->
  m r
concreteSafeDivisionHelper f l r
  | r == 0 = tryMerge $ throwError DivideByZero
  | otherwise = mrgSingle $ f l r

concreteSignedBoundedDivisionOrHelper ::
  ( Integral a,
    Bounded a,
    Mergeable r
  ) =>
  (a -> a -> r) ->
  r ->
  a ->
  a ->
  r
concreteSignedBoundedDivisionOrHelper f d l r
  | r == 0 = d
  | l == minBound && r == -1 = d
  | otherwise = f l r

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
  | r == 0 = tryMerge $ throwError DivideByZero
  | l == minBound && r == -1 = tryMerge $ throwError Overflow
  | otherwise = mrgSingle $ f l r

#define QUOTE() '
#define QID(a) a
#define QRIGHT(a) QID(a)'

#define QRIGHTT(a) QID(a)' t'
#define QRIGHTU(a) QID(a)' _'

#define DIVISION_OR_CONCRETE(type) \
instance DivisionOr type where \
  divOr = concreteDivisionOrHelper div; \
  modOr = concreteDivisionOrHelper mod; \
  divModOr = concreteDivisionOrHelper divMod; \
  quotOr = concreteDivisionOrHelper quot; \
  remOr = concreteDivisionOrHelper rem; \
  quotRemOr = concreteDivisionOrHelper quotRem

#define SAFE_DIVISION_CONCRETE(type) \
instance (MonadError ArithException m, TryMerge m) => \
  SafeDivision ArithException type m where \
  safeDiv = concreteSafeDivisionHelper div; \
  safeMod = concreteSafeDivisionHelper mod; \
  safeDivMod = concreteSafeDivisionHelper divMod; \
  safeQuot = concreteSafeDivisionHelper quot; \
  safeRem = concreteSafeDivisionHelper rem; \
  safeQuotRem = concreteSafeDivisionHelper quotRem

#define DIVISION_OR_CONCRETE_SIGNED_BOUNDED(type) \
instance DivisionOr type where \
  divOr = concreteSignedBoundedDivisionOrHelper div; \
  modOr = concreteDivisionOrHelper mod; \
  divModOr = concreteSignedBoundedDivisionOrHelper divMod; \
  quotOr = concreteSignedBoundedDivisionOrHelper quot; \
  remOr = concreteDivisionOrHelper rem; \
  quotRemOr = concreteSignedBoundedDivisionOrHelper quotRem

#define SAFE_DIVISION_CONCRETE_SIGNED_BOUNDED(type) \
instance (MonadError ArithException m, TryMerge m) => \
  SafeDivision ArithException type m where \
  safeDiv = concreteSignedBoundedSafeDivisionHelper div; \
  safeMod = concreteSafeDivisionHelper mod; \
  safeDivMod = concreteSignedBoundedSafeDivisionHelper divMod; \
  safeQuot = concreteSignedBoundedSafeDivisionHelper quot; \
  safeRem = concreteSafeDivisionHelper rem; \
  safeQuotRem = concreteSignedBoundedSafeDivisionHelper quotRem

#if 1
DIVISION_OR_CONCRETE(Integer)
SAFE_DIVISION_CONCRETE(Integer)
DIVISION_OR_CONCRETE_SIGNED_BOUNDED(Int8)
SAFE_DIVISION_CONCRETE_SIGNED_BOUNDED(Int8)
DIVISION_OR_CONCRETE_SIGNED_BOUNDED(Int16)
SAFE_DIVISION_CONCRETE_SIGNED_BOUNDED(Int16)
DIVISION_OR_CONCRETE_SIGNED_BOUNDED(Int32)
SAFE_DIVISION_CONCRETE_SIGNED_BOUNDED(Int32)
DIVISION_OR_CONCRETE_SIGNED_BOUNDED(Int64)
SAFE_DIVISION_CONCRETE_SIGNED_BOUNDED(Int64)
DIVISION_OR_CONCRETE_SIGNED_BOUNDED(Int)
SAFE_DIVISION_CONCRETE_SIGNED_BOUNDED(Int)
DIVISION_OR_CONCRETE(Word8)
SAFE_DIVISION_CONCRETE(Word8)
DIVISION_OR_CONCRETE(Word16)
SAFE_DIVISION_CONCRETE(Word16)
DIVISION_OR_CONCRETE(Word32)
SAFE_DIVISION_CONCRETE(Word32)
DIVISION_OR_CONCRETE(Word64)
SAFE_DIVISION_CONCRETE(Word64)
DIVISION_OR_CONCRETE(Word)
SAFE_DIVISION_CONCRETE(Word)


#endif

instance (KnownNat n, 1 <= n) => DivisionOr (IntN n) where
  divOr = concreteSignedBoundedDivisionOrHelper div
  modOr = concreteDivisionOrHelper mod
  divModOr = concreteSignedBoundedDivisionOrHelper divMod
  quotOr = concreteSignedBoundedDivisionOrHelper quot
  remOr = concreteDivisionOrHelper rem
  quotRemOr = concreteSignedBoundedDivisionOrHelper quotRem

instance
  (MonadError ArithException m, TryMerge m, KnownNat n, 1 <= n) =>
  SafeDivision ArithException (IntN n) m
  where
  safeDiv = concreteSignedBoundedSafeDivisionHelper div
  safeMod = concreteSafeDivisionHelper mod
  safeDivMod = concreteSignedBoundedSafeDivisionHelper divMod
  safeQuot = concreteSignedBoundedSafeDivisionHelper quot
  safeRem = concreteSafeDivisionHelper rem
  safeQuotRem = concreteSignedBoundedSafeDivisionHelper quotRem

instance (KnownNat n, 1 <= n) => DivisionOr (WordN n) where
  divOr = concreteDivisionOrHelper div
  modOr = concreteDivisionOrHelper mod
  divModOr = concreteDivisionOrHelper divMod
  quotOr = concreteDivisionOrHelper quot
  remOr = concreteDivisionOrHelper rem
  quotRemOr = concreteDivisionOrHelper quotRem

instance
  (MonadError ArithException m, TryMerge m, KnownNat n, 1 <= n) =>
  SafeDivision ArithException (WordN n) m
  where
  safeDiv = concreteSafeDivisionHelper div
  safeMod = concreteSafeDivisionHelper mod
  safeDivMod = concreteSafeDivisionHelper divMod
  safeQuot = concreteSafeDivisionHelper quot
  safeRem = concreteSafeDivisionHelper rem
  safeQuotRem = concreteSafeDivisionHelper quotRem

#define DIVISION_OR_SYMBOLIC_FUNC(name, type, op) \
name d (type l) rs@(type r) = \
  symIte (rs .== con 0) d (type $ op l r)

#define DIVISION_OR_SYMBOLIC_FUNC2(name, type, op1, op2) \
name (dd, dm) (type l) rs@(type r) = \
  (symIte (rs .== con 0) dd (type $ op1 l r), \
   symIte (rs .== con 0) dm (type $ op2 l r))

#define SAFE_DIVISION_SYMBOLIC_FUNC(name, type, op) \
name (type l) rs@(type r) = \
  mrgIf \
    (rs .== con 0) \
    (throwError DivideByZero) \
    (mrgSingle $ type $ op l r); \

#define SAFE_DIVISION_SYMBOLIC_FUNC2(name, type, op1, op2) \
name (type l) rs@(type r) = \
  mrgIf \
    (rs .== con 0) \
    (throwError DivideByZero) \
    (mrgSingle (type $ op1 l r, type $ op2 l r)); \

#if 1
instance DivisionOr SymInteger where
  DIVISION_OR_SYMBOLIC_FUNC(divOr, SymInteger, pevalDivIntegralTerm)
  DIVISION_OR_SYMBOLIC_FUNC(modOr, SymInteger, pevalModIntegralTerm)
  DIVISION_OR_SYMBOLIC_FUNC(quotOr, SymInteger, pevalQuotIntegralTerm)
  DIVISION_OR_SYMBOLIC_FUNC(remOr, SymInteger, pevalRemIntegralTerm)
  DIVISION_OR_SYMBOLIC_FUNC2(divModOr, SymInteger, pevalDivIntegralTerm, pevalModIntegralTerm)
  DIVISION_OR_SYMBOLIC_FUNC2(quotRemOr, SymInteger, pevalQuotIntegralTerm, pevalRemIntegralTerm)
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

#define DIVISION_OR_SYMBOLIC_FUNC_BOUNDED_SIGNED(name, type, op) \
name d ls@(type l) rs@(type r) = \
  symIte \
    (rs .== con 0) \
    d \
    (symIte (rs .== con (-1) .&& ls .== con minBound) \
      d \
      (type $ op l r)) \

#define DIVISION_OR_SYMBOLIC_FUNC2_BOUNDED_SIGNED(name, type, op1, op2) \
name (dd, dr) ls@(type l) rs@(type r) = \
  (symIte \
    (rs .== con 0) \
    dd \
    (symIte (rs .== con (-1) .&& ls .== con minBound) \
      dd \
      (type $ op1 l r)), \
  symIte \
    (rs .== con 0) \
    dr \
    (symIte (rs .== con (-1) .&& ls .== con minBound) \
      dr \
      (type $ op2 l r))) \

#define SAFE_DIVISION_SYMBOLIC_FUNC_BOUNDED_SIGNED(name, type, op) \
name ls@(type l) rs@(type r) = \
  mrgIf \
    (rs .== con 0) \
    (throwError DivideByZero) \
    (mrgIf (rs .== con (-1) .&& ls .== con minBound) \
      (throwError Overflow) \
      (mrgSingle $ type $ op l r)); \

#define SAFE_DIVISION_SYMBOLIC_FUNC2_BOUNDED_SIGNED(name, type, op1, op2) \
name ls@(type l) rs@(type r) = \
  mrgIf \
    (rs .== con 0) \
    (throwError DivideByZero) \
    (mrgIf (rs .== con (-1) .&& ls .== con minBound) \
      (throwError Overflow) \
      (mrgSingle (type $ op1 l r, type $ op2 l r))); \

#if 1
instance (KnownNat n, 1 <= n) => DivisionOr (SymIntN n) where
  DIVISION_OR_SYMBOLIC_FUNC_BOUNDED_SIGNED(divOr, SymIntN, pevalDivIntegralTerm)
  DIVISION_OR_SYMBOLIC_FUNC(modOr, SymIntN, pevalModIntegralTerm)
  DIVISION_OR_SYMBOLIC_FUNC_BOUNDED_SIGNED(quotOr, SymIntN, pevalQuotIntegralTerm)
  DIVISION_OR_SYMBOLIC_FUNC(remOr, SymIntN, pevalRemIntegralTerm)
  DIVISION_OR_SYMBOLIC_FUNC2_BOUNDED_SIGNED(divModOr, SymIntN, pevalDivIntegralTerm, pevalModIntegralTerm)
  DIVISION_OR_SYMBOLIC_FUNC2_BOUNDED_SIGNED(quotRemOr, SymIntN, pevalQuotIntegralTerm, pevalRemIntegralTerm)
instance
  (MonadError ArithException m, MonadUnion m, KnownNat n, 1 <= n) =>
  SafeDivision ArithException (SymIntN n) m where
  SAFE_DIVISION_SYMBOLIC_FUNC_BOUNDED_SIGNED(safeDiv, SymIntN, pevalDivIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC(safeMod, SymIntN, pevalModIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC_BOUNDED_SIGNED(safeQuot, SymIntN, pevalQuotIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC(safeRem, SymIntN, pevalRemIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC2_BOUNDED_SIGNED(safeDivMod, SymIntN, pevalDivIntegralTerm, pevalModIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC2_BOUNDED_SIGNED(safeQuotRem, SymIntN, pevalQuotIntegralTerm, pevalRemIntegralTerm)
#endif

#if 1
instance (KnownNat n, 1 <= n) => DivisionOr (SymWordN n) where
  DIVISION_OR_SYMBOLIC_FUNC(divOr, SymWordN, pevalDivIntegralTerm)
  DIVISION_OR_SYMBOLIC_FUNC(modOr, SymWordN, pevalModIntegralTerm)
  DIVISION_OR_SYMBOLIC_FUNC(quotOr, SymWordN, pevalQuotIntegralTerm)
  DIVISION_OR_SYMBOLIC_FUNC(remOr, SymWordN, pevalRemIntegralTerm)
  DIVISION_OR_SYMBOLIC_FUNC2(divModOr, SymWordN, pevalDivIntegralTerm, pevalModIntegralTerm)
  DIVISION_OR_SYMBOLIC_FUNC2(quotRemOr, SymWordN, pevalQuotIntegralTerm, pevalRemIntegralTerm)
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
