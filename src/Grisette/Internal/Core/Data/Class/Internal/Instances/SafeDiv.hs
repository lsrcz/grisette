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

module Grisette.Internal.Core.Data.Class.Internal.Instances.SafeDiv () where

import Control.Exception (ArithException (DivideByZero, Overflow))
import Control.Monad.Except (MonadError (throwError))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeNats (KnownNat, type (<=))
import Grisette.Internal.Core.Control.Monad.Class.Union (MonadUnion)
import Grisette.Internal.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Internal.Core.Data.Class.Internal.SafeDiv
  ( DivOr (divModOr, divOr, modOr, quotOr, quotRemOr, remOr),
    SafeDiv (safeDiv, safeDivMod, safeMod, safeQuot, safeQuotRem, safeRem),
  )
import Grisette.Internal.Core.Data.Class.SymEq (SymEq ((.==)))
import Grisette.Internal.Core.Data.Class.LogicalOp (LogicalOp ((.&&)))
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.SimpleMergeable
  ( mrgIf,
  )
import Grisette.Internal.Core.Data.Class.Solvable (Solvable (con))
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
instance DivOr SymInteger where
  DIVISION_OR_SYMBOLIC_FUNC(divOr, SymInteger, pevalDivIntegralTerm)
  DIVISION_OR_SYMBOLIC_FUNC(modOr, SymInteger, pevalModIntegralTerm)
  DIVISION_OR_SYMBOLIC_FUNC(quotOr, SymInteger, pevalQuotIntegralTerm)
  DIVISION_OR_SYMBOLIC_FUNC(remOr, SymInteger, pevalRemIntegralTerm)
  DIVISION_OR_SYMBOLIC_FUNC2(divModOr, SymInteger, pevalDivIntegralTerm, pevalModIntegralTerm)
  DIVISION_OR_SYMBOLIC_FUNC2(quotRemOr, SymInteger, pevalQuotIntegralTerm, pevalRemIntegralTerm)
instance
  (MonadUnion m, MonadError ArithException m) =>
  SafeDiv ArithException SymInteger m where
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
instance (KnownNat n, 1 <= n) => DivOr (SymIntN n) where
  DIVISION_OR_SYMBOLIC_FUNC_BOUNDED_SIGNED(divOr, SymIntN, pevalDivIntegralTerm)
  DIVISION_OR_SYMBOLIC_FUNC(modOr, SymIntN, pevalModIntegralTerm)
  DIVISION_OR_SYMBOLIC_FUNC_BOUNDED_SIGNED(quotOr, SymIntN, pevalQuotIntegralTerm)
  DIVISION_OR_SYMBOLIC_FUNC(remOr, SymIntN, pevalRemIntegralTerm)
  DIVISION_OR_SYMBOLIC_FUNC2_BOUNDED_SIGNED(divModOr, SymIntN, pevalDivIntegralTerm, pevalModIntegralTerm)
  DIVISION_OR_SYMBOLIC_FUNC2_BOUNDED_SIGNED(quotRemOr, SymIntN, pevalQuotIntegralTerm, pevalRemIntegralTerm)
instance
  (MonadError ArithException m, MonadUnion m, KnownNat n, 1 <= n) =>
  SafeDiv ArithException (SymIntN n) m where
  SAFE_DIVISION_SYMBOLIC_FUNC_BOUNDED_SIGNED(safeDiv, SymIntN, pevalDivIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC(safeMod, SymIntN, pevalModIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC_BOUNDED_SIGNED(safeQuot, SymIntN, pevalQuotIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC(safeRem, SymIntN, pevalRemIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC2_BOUNDED_SIGNED(safeDivMod, SymIntN, pevalDivIntegralTerm, pevalModIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC2_BOUNDED_SIGNED(safeQuotRem, SymIntN, pevalQuotIntegralTerm, pevalRemIntegralTerm)
#endif

#if 1
instance (KnownNat n, 1 <= n) => DivOr (SymWordN n) where
  DIVISION_OR_SYMBOLIC_FUNC(divOr, SymWordN, pevalDivIntegralTerm)
  DIVISION_OR_SYMBOLIC_FUNC(modOr, SymWordN, pevalModIntegralTerm)
  DIVISION_OR_SYMBOLIC_FUNC(quotOr, SymWordN, pevalQuotIntegralTerm)
  DIVISION_OR_SYMBOLIC_FUNC(remOr, SymWordN, pevalRemIntegralTerm)
  DIVISION_OR_SYMBOLIC_FUNC2(divModOr, SymWordN, pevalDivIntegralTerm, pevalModIntegralTerm)
  DIVISION_OR_SYMBOLIC_FUNC2(quotRemOr, SymWordN, pevalQuotIntegralTerm, pevalRemIntegralTerm)
instance
  (MonadError ArithException m, MonadUnion m, KnownNat n, 1 <= n) =>
  SafeDiv ArithException (SymWordN n) m where
  SAFE_DIVISION_SYMBOLIC_FUNC(safeDiv, SymWordN, pevalDivIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC(safeMod, SymWordN, pevalModIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC(safeQuot, SymWordN, pevalQuotIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC(safeRem, SymWordN, pevalRemIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC2(safeDivMod, SymWordN, pevalDivIntegralTerm, pevalModIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC2(safeQuotRem, SymWordN, pevalQuotIntegralTerm, pevalRemIntegralTerm)
#endif
