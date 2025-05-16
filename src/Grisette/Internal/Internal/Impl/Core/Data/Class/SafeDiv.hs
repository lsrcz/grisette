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
-- Module      :   Grisette.Internal.Internal.Impl.Core.Data.Class.SafeDiv
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Impl.Core.Data.Class.SafeDiv () where

import Control.Exception (ArithException (DivideByZero, Overflow))
import Control.Monad.Except (MonadError (throwError))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeNats (KnownNat, type (<=))
import Grisette.Internal.Core.Control.Monad.Class.Union (MonadUnion)
import Grisette.Internal.Core.Data.Class.AsKey (AsKey (AsKey))
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
import Grisette.Internal.Internal.Decl.Core.Data.Class.SafeDiv
  ( DivOr (divModOr, divOr, modOr, quotOr, quotRemOr, remOr),
    SafeDiv (safeDiv, safeDivMod, safeMod, safeQuot, safeQuotRem, safeRem),
  )
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.SymBV (SymIntN, SymWordN)
import Grisette.Internal.SymPrim.SymInteger (SymInteger)

concreteDivOrHelper ::
  (Integral a) =>
  (a -> a -> r) ->
  r ->
  a ->
  a ->
  r
concreteDivOrHelper f d l r
  | r == 0 = d
  | otherwise = f l r

concreteSafeDivHelper ::
  (MonadError ArithException m, TryMerge m, Integral a, Mergeable r) =>
  (a -> a -> r) ->
  a ->
  a ->
  m r
concreteSafeDivHelper f l r
  | r == 0 = tryMerge $ throwError DivideByZero
  | otherwise = mrgSingle $ f l r

concreteSignedBoundedDivOrHelper ::
  ( Integral a,
    Bounded a,
    Mergeable r
  ) =>
  (a -> a -> r) ->
  r ->
  a ->
  a ->
  r
concreteSignedBoundedDivOrHelper f d l r
  | r == 0 = d
  | l == minBound && r == -1 = d
  | otherwise = f l r

concreteSignedBoundedSafeDivHelper ::
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
concreteSignedBoundedSafeDivHelper f l r
  | r == 0 = tryMerge $ throwError DivideByZero
  | l == minBound && r == -1 = tryMerge $ throwError Overflow
  | otherwise = mrgSingle $ f l r

#define QUOTE() '
#define QID(a) a
#define QRIGHT(a) QID(a)'

#define QRIGHTT(a) QID(a)' t'
#define QRIGHTU(a) QID(a)' _'

#define DIVISION_OR_CONCRETE(type) \
instance DivOr type where \
  divOr = concreteDivOrHelper div; \
  modOr = concreteDivOrHelper mod; \
  divModOr = concreteDivOrHelper divMod; \
  quotOr = concreteDivOrHelper quot; \
  remOr = concreteDivOrHelper rem; \
  quotRemOr = concreteDivOrHelper quotRem

#define SAFE_DIVISION_CONCRETE(type) \
instance (MonadError ArithException m, TryMerge m) => \
  SafeDiv ArithException type m where \
  safeDiv = concreteSafeDivHelper div; \
  safeMod = concreteSafeDivHelper mod; \
  safeDivMod = concreteSafeDivHelper divMod; \
  safeQuot = concreteSafeDivHelper quot; \
  safeRem = concreteSafeDivHelper rem; \
  safeQuotRem = concreteSafeDivHelper quotRem

#define DIVISION_OR_CONCRETE_SIGNED_BOUNDED(type) \
instance DivOr type where \
  divOr = concreteSignedBoundedDivOrHelper div; \
  modOr = concreteDivOrHelper mod; \
  divModOr = concreteSignedBoundedDivOrHelper divMod; \
  quotOr = concreteSignedBoundedDivOrHelper quot; \
  remOr = concreteDivOrHelper rem; \
  quotRemOr = concreteSignedBoundedDivOrHelper quotRem

#define SAFE_DIVISION_CONCRETE_SIGNED_BOUNDED(type) \
instance (MonadError ArithException m, TryMerge m) => \
  SafeDiv ArithException type m where \
  safeDiv = concreteSignedBoundedSafeDivHelper div; \
  safeMod = concreteSafeDivHelper mod; \
  safeDivMod = concreteSignedBoundedSafeDivHelper divMod; \
  safeQuot = concreteSignedBoundedSafeDivHelper quot; \
  safeRem = concreteSafeDivHelper rem; \
  safeQuotRem = concreteSignedBoundedSafeDivHelper quotRem

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

instance (KnownNat n, 1 <= n) => DivOr (IntN n) where
  divOr = concreteSignedBoundedDivOrHelper div
  modOr = concreteDivOrHelper mod
  divModOr = concreteSignedBoundedDivOrHelper divMod
  quotOr = concreteSignedBoundedDivOrHelper quot
  remOr = concreteDivOrHelper rem
  quotRemOr = concreteSignedBoundedDivOrHelper quotRem

instance
  (MonadError ArithException m, TryMerge m, KnownNat n, 1 <= n) =>
  SafeDiv ArithException (IntN n) m
  where
  safeDiv = concreteSignedBoundedSafeDivHelper div
  safeMod = concreteSafeDivHelper mod
  safeDivMod = concreteSignedBoundedSafeDivHelper divMod
  safeQuot = concreteSignedBoundedSafeDivHelper quot
  safeRem = concreteSafeDivHelper rem
  safeQuotRem = concreteSignedBoundedSafeDivHelper quotRem

instance (KnownNat n, 1 <= n) => DivOr (WordN n) where
  divOr = concreteDivOrHelper div
  modOr = concreteDivOrHelper mod
  divModOr = concreteDivOrHelper divMod
  quotOr = concreteDivOrHelper quot
  remOr = concreteDivOrHelper rem
  quotRemOr = concreteDivOrHelper quotRem

instance
  (MonadError ArithException m, TryMerge m, KnownNat n, 1 <= n) =>
  SafeDiv ArithException (WordN n) m
  where
  safeDiv = concreteSafeDivHelper div
  safeMod = concreteSafeDivHelper mod
  safeDivMod = concreteSafeDivHelper divMod
  safeQuot = concreteSafeDivHelper quot
  safeRem = concreteSafeDivHelper rem
  safeQuotRem = concreteSafeDivHelper quotRem

#define DIVISION_OR_SYMBOLIC_FUNC(name, op) \
name d l r = symIte (r .== con 0) d (op l r)

#define DIVISION_OR_SYMBOLIC_FUNC2(name, op1, op2) \
name (dd, dm) l r = \
  (symIte (r .== con 0) dd (op1 l r), symIte (r .== con 0) dm (op2 l r))

#define SAFE_DIVISION_SYMBOLIC_FUNC(name, op) \
name l r = mrgIf (r .== con 0) (throwError DivideByZero) (mrgSingle $ op l r)

#define SAFE_DIVISION_SYMBOLIC_FUNC2(name, op1, op2) \
name l r = mrgIf (r .== con 0) (throwError DivideByZero) (mrgSingle (op1 l r, op2 l r))

#if 1
instance DivOr SymInteger where
  DIVISION_OR_SYMBOLIC_FUNC(divOr, div)
  DIVISION_OR_SYMBOLIC_FUNC(modOr, mod)
  DIVISION_OR_SYMBOLIC_FUNC(quotOr, quot)
  DIVISION_OR_SYMBOLIC_FUNC(remOr, rem)
  DIVISION_OR_SYMBOLIC_FUNC2(divModOr, div, mod)
  DIVISION_OR_SYMBOLIC_FUNC2(quotRemOr, quot, rem)
instance
  (MonadUnion m, MonadError ArithException m) =>
  SafeDiv ArithException SymInteger m where
  SAFE_DIVISION_SYMBOLIC_FUNC(safeDiv, div)
  SAFE_DIVISION_SYMBOLIC_FUNC(safeMod, mod)
  SAFE_DIVISION_SYMBOLIC_FUNC(safeQuot, quot)
  SAFE_DIVISION_SYMBOLIC_FUNC(safeRem, rem)
  SAFE_DIVISION_SYMBOLIC_FUNC2(safeDivMod, div, mod)
  SAFE_DIVISION_SYMBOLIC_FUNC2(safeQuotRem, quot, rem)
#endif

#define DIVISION_OR_SYMBOLIC_FUNC_BOUNDED_SIGNED(name, op) \
name d l r = \
  symIte (r .== con 0) d $ \
    symIte (r .== con (-1) .&& l .== con minBound) d (op l r)

#define DIVISION_OR_SYMBOLIC_FUNC2_BOUNDED_SIGNED(name, op1, op2) \
name (dd, dr) l r = \
  ( symIte (r .== con 0) dd $ \
      symIte (r .== con (-1) .&& l .== con minBound) dd (op1 l r), \
    symIte (r .== con 0) dr $ \
      symIte (r .== con (-1) .&& l .== con minBound) dr (op2 l r) \
  )

#define SAFE_DIVISION_SYMBOLIC_FUNC_BOUNDED_SIGNED(name, op) \
name l r = \
  mrgIf (r .== con 0) (throwError DivideByZero) $ \
    mrgIf (r .== con (-1) .&& l .== con minBound) \
      (throwError Overflow) \
      (mrgSingle $ op l r)

#define SAFE_DIVISION_SYMBOLIC_FUNC2_BOUNDED_SIGNED(name, op1, op2) \
name l r = \
  mrgIf (r .== con 0) (throwError DivideByZero) $ \
    mrgIf (r .== con (-1) .&& l .== con minBound) \
      (throwError Overflow) \
      (mrgSingle (op1 l r, op2 l r))

#if 1
instance (KnownNat n, 1 <= n) => DivOr (SymIntN n) where
  DIVISION_OR_SYMBOLIC_FUNC_BOUNDED_SIGNED(divOr, div)
  DIVISION_OR_SYMBOLIC_FUNC(modOr, mod)
  DIVISION_OR_SYMBOLIC_FUNC_BOUNDED_SIGNED(quotOr, quot)
  DIVISION_OR_SYMBOLIC_FUNC(remOr, rem)
  DIVISION_OR_SYMBOLIC_FUNC2_BOUNDED_SIGNED(divModOr, div, mod)
  DIVISION_OR_SYMBOLIC_FUNC2_BOUNDED_SIGNED(quotRemOr, quot, rem)
instance
  (MonadError ArithException m, MonadUnion m, KnownNat n, 1 <= n) =>
  SafeDiv ArithException (SymIntN n) m where
  SAFE_DIVISION_SYMBOLIC_FUNC_BOUNDED_SIGNED(safeDiv, div)
  SAFE_DIVISION_SYMBOLIC_FUNC(safeMod, mod)
  SAFE_DIVISION_SYMBOLIC_FUNC_BOUNDED_SIGNED(safeQuot, quot)
  SAFE_DIVISION_SYMBOLIC_FUNC(safeRem, rem)
  SAFE_DIVISION_SYMBOLIC_FUNC2_BOUNDED_SIGNED(safeDivMod, div, mod)
  SAFE_DIVISION_SYMBOLIC_FUNC2_BOUNDED_SIGNED(safeQuotRem, quot, rem)
#endif

#if 1
instance (KnownNat n, 1 <= n) => DivOr (SymWordN n) where
  DIVISION_OR_SYMBOLIC_FUNC(divOr, div)
  DIVISION_OR_SYMBOLIC_FUNC(modOr, mod)
  DIVISION_OR_SYMBOLIC_FUNC(quotOr, quot)
  DIVISION_OR_SYMBOLIC_FUNC(remOr, rem)
  DIVISION_OR_SYMBOLIC_FUNC2(divModOr, div, mod)
  DIVISION_OR_SYMBOLIC_FUNC2(quotRemOr, quot, rem)
instance
  (MonadError ArithException m, MonadUnion m, KnownNat n, 1 <= n) =>
  SafeDiv ArithException (SymWordN n) m where
  SAFE_DIVISION_SYMBOLIC_FUNC(safeDiv, div)
  SAFE_DIVISION_SYMBOLIC_FUNC(safeMod, mod)
  SAFE_DIVISION_SYMBOLIC_FUNC(safeQuot, quot)
  SAFE_DIVISION_SYMBOLIC_FUNC(safeRem, rem)
  SAFE_DIVISION_SYMBOLIC_FUNC2(safeDivMod, div, mod)
  SAFE_DIVISION_SYMBOLIC_FUNC2(safeQuotRem, quot, rem)
#endif

instance (DivOr a) => DivOr (AsKey a) where
  divOr (AsKey def) (AsKey n) (AsKey d) = AsKey $ divOr def n d
  modOr (AsKey def) (AsKey n) (AsKey d) = AsKey $ modOr def n d
  divModOr (AsKey defDiv, AsKey defMod) (AsKey n) (AsKey d) =
    bimap AsKey AsKey $ divModOr (defDiv, defMod) n d
  quotOr (AsKey def) (AsKey n) (AsKey d) = AsKey $ quotOr def n d
  remOr (AsKey def) (AsKey n) (AsKey d) = AsKey $ remOr def n d
  quotRemOr (AsKey defDiv, AsKey defRem) (AsKey n) (AsKey d) =
    bimap AsKey AsKey $ quotRemOr (defDiv, defRem) n d

instance (SafeDiv e a m) => SafeDiv e (AsKey a) m where
  safeDiv (AsKey n) (AsKey d) = do
    r <- safeDiv n d
    mrgSingle $ AsKey r
  safeMod (AsKey n) (AsKey d) = do
    r <- safeMod n d
    mrgSingle $ AsKey r
  safeDivMod (AsKey n) (AsKey d) = do
    (rd, rm) <- safeDivMod n d
    mrgSingle (AsKey rd, AsKey rm)
  safeQuot (AsKey n) (AsKey d) = do
    r <- safeQuot n d
    mrgSingle $ AsKey r
  safeRem (AsKey n) (AsKey d) = do
    r <- safeRem n d
    mrgSingle $ AsKey r
  safeQuotRem (AsKey n) (AsKey d) = do
    (rq, rr) <- safeQuotRem n d
    mrgSingle (AsKey rq, AsKey rr)
