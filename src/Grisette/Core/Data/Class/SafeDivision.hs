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
import Grisette.Core.Data.Class.LogicalOp (LogicalOp ((.&&)))
import Grisette.Core.Data.Class.SEq (SEq ((.==)))
import Grisette.Core.Data.Class.SimpleMergeable
  ( mrgIf,
  )
import Grisette.Core.Data.Class.Solvable (Solvable (con))
import Grisette.Core.Data.Class.TryMerge
  ( mrgPure,
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

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> import Control.Monad.Except

-- | Safe division with monadic error handling in multi-path
-- execution. These procedures throw an exception when the
-- divisor is zero. The result should be able to handle errors with
-- `MonadError`.
class SafeDivision a m where
  -- | Safe signed 'div' with monadic error handling in multi-path execution.
  --
  -- >>> safeDiv (ssym "a") (ssym "b") :: ExceptT ArithException UnionM SymInteger
  -- ExceptT {If (= b 0) (Left divide by zero) (Right (div a b))}
  safeDiv :: a -> a -> m a

  -- | Safe signed 'mod' with monadic error handling in multi-path execution.
  --
  -- >>> safeMod (ssym "a") (ssym "b") :: ExceptT ArithException UnionM SymInteger
  -- ExceptT {If (= b 0) (Left divide by zero) (Right (mod a b))}
  safeMod :: a -> a -> m a

  -- | Safe signed 'divMod' with monadic error handling in multi-path execution.
  --
  -- >>> safeDivMod (ssym "a") (ssym "b") :: ExceptT ArithException UnionM (SymInteger, SymInteger)
  -- ExceptT {If (= b 0) (Left divide by zero) (Right ((div a b),(mod a b)))}
  safeDivMod :: a -> a -> m (a, a)

  -- | Safe signed 'quot' with monadic error handling in multi-path execution.
  safeQuot :: a -> a -> m a

  -- | Safe signed 'rem' with monadic error handling in multi-path execution.
  safeRem :: a -> a -> m a

  -- | Safe signed 'quotRem' with monadic error handling in multi-path execution.
  safeQuotRem :: a -> a -> m (a, a)

#define QUOTE() '
#define QID(a) a
#define QRIGHT(a) QID(a)'

#define QRIGHTT(a) QID(a)' t'
#define QRIGHTU(a) QID(a)' _'

#define SAFE_DIVISION_CONCRETE_FUNC(name, op) \
name _ r \
  | r == 0 = throwError DivideByZero; \
name l r = return $ l `op` r;

#define SAFE_DIVISION_CONCRETE_FUNC_SIGNED_BOUNDED(name, op) \
name _ r | r == 0 = throwError DivideByZero; \
name l r | l == minBound && r == -1 = throwError Overflow; \
name l r = return $ l `op` r;

#define SAFE_DIVISION_CONCRETE(type) \
instance MonadError ArithException m => SafeDivision type m where \
  SAFE_DIVISION_CONCRETE_FUNC(safeDiv, div); \
  SAFE_DIVISION_CONCRETE_FUNC(safeMod, mod); \
  SAFE_DIVISION_CONCRETE_FUNC(safeDivMod, divMod); \
  SAFE_DIVISION_CONCRETE_FUNC(safeQuot, quot); \
  SAFE_DIVISION_CONCRETE_FUNC(safeRem, rem); \
  SAFE_DIVISION_CONCRETE_FUNC(safeQuotRem, quotRem)

#define SAFE_DIVISION_CONCRETE_SIGNED_BOUNDED(type) \
instance MonadError ArithException m => SafeDivision type m where \
  SAFE_DIVISION_CONCRETE_FUNC_SIGNED_BOUNDED(safeDiv, div); \
  SAFE_DIVISION_CONCRETE_FUNC_SIGNED_BOUNDED(safeMod, mod); \
  SAFE_DIVISION_CONCRETE_FUNC_SIGNED_BOUNDED(safeDivMod, divMod); \
  SAFE_DIVISION_CONCRETE_FUNC_SIGNED_BOUNDED(safeQuot, quot); \
  SAFE_DIVISION_CONCRETE_FUNC_SIGNED_BOUNDED(safeRem, rem); \
  SAFE_DIVISION_CONCRETE_FUNC_SIGNED_BOUNDED(safeQuotRem, quotRem)

#define SAFE_DIVISION_CONCRETE_BV(type) \
instance \
  (MonadError ArithException m, KnownNat n, 1 <= n) => \
  SafeDivision (type n) m where \
  SAFE_DIVISION_CONCRETE_FUNC(safeDiv, div); \
  SAFE_DIVISION_CONCRETE_FUNC(safeMod, mod); \
  SAFE_DIVISION_CONCRETE_FUNC(safeDivMod, divMod); \
  SAFE_DIVISION_CONCRETE_FUNC(safeQuot, quot); \
  SAFE_DIVISION_CONCRETE_FUNC(safeRem, rem); \
  SAFE_DIVISION_CONCRETE_FUNC(safeQuotRem, quotRem)

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
  (MonadError ArithException m, KnownNat n, 1 <= n) =>
  SafeDivision (IntN n) m where
  SAFE_DIVISION_CONCRETE_FUNC_SIGNED_BOUNDED(safeDiv, div)
  SAFE_DIVISION_CONCRETE_FUNC_SIGNED_BOUNDED(safeMod, mod)
  SAFE_DIVISION_CONCRETE_FUNC_SIGNED_BOUNDED(safeDivMod, divMod)
  SAFE_DIVISION_CONCRETE_FUNC_SIGNED_BOUNDED(safeQuot, quot)
  SAFE_DIVISION_CONCRETE_FUNC_SIGNED_BOUNDED(safeRem, rem)
  SAFE_DIVISION_CONCRETE_FUNC_SIGNED_BOUNDED(safeQuotRem, quotRem)

instance
  (MonadError ArithException m, KnownNat n, 1 <= n) =>
  SafeDivision (WordN n) m where
  SAFE_DIVISION_CONCRETE_FUNC(safeDiv, div)
  SAFE_DIVISION_CONCRETE_FUNC(safeMod, mod)
  SAFE_DIVISION_CONCRETE_FUNC(safeDivMod, divMod)
  SAFE_DIVISION_CONCRETE_FUNC(safeQuot, quot)
  SAFE_DIVISION_CONCRETE_FUNC(safeRem, rem)
  SAFE_DIVISION_CONCRETE_FUNC(safeQuotRem, quotRem)
#endif

#define SAFE_DIVISION_CONCRETE_FUNC_SOME(stype, type, name, op) \
  name (stype (l :: type l)) (stype (r :: type r)) = \
    (case sameNat (Proxy @l) (Proxy @r) of \
      Just Refl -> \
        (case name l r of \
          Left err -> throwError $ Right err; \
          Right value -> return $ stype value); \
      Nothing -> throwError $ Left BitwidthMismatch); \
  
#define SAFE_DIVISION_CONCRETE_FUNC_SOME_DIVMOD(stype, type, name, op) \
  name (stype (l :: type l)) (stype (r :: type r)) = \
    (case sameNat (Proxy @l) (Proxy @r) of \
      Just Refl -> \
        (case name l r of \
          Left err -> throwError $ Right err; \
          Right (value1, value2) -> return (stype value1, stype value2)); \
      Nothing -> throwError $ Left BitwidthMismatch); \

#if 1
instance
  MonadError (Either BitwidthMismatch ArithException) m =>
  SafeDivision SomeIntN m where
  SAFE_DIVISION_CONCRETE_FUNC_SOME(SomeIntN, IntN, safeDiv, div)
  SAFE_DIVISION_CONCRETE_FUNC_SOME(SomeIntN, IntN, safeMod, mod)
  SAFE_DIVISION_CONCRETE_FUNC_SOME_DIVMOD(SomeIntN, IntN, safeDivMod, divMod)
  SAFE_DIVISION_CONCRETE_FUNC_SOME(SomeIntN, IntN, safeQuot, quot)
  SAFE_DIVISION_CONCRETE_FUNC_SOME(SomeIntN, IntN, safeRem, rem)
  SAFE_DIVISION_CONCRETE_FUNC_SOME_DIVMOD(SomeIntN, IntN, safeQuotRem, quotRem)

instance
  MonadError (Either BitwidthMismatch ArithException) m =>
  SafeDivision SomeWordN m where
  SAFE_DIVISION_CONCRETE_FUNC_SOME(SomeWordN, WordN, safeDiv, div)
  SAFE_DIVISION_CONCRETE_FUNC_SOME(SomeWordN, WordN, safeMod, mod)
  SAFE_DIVISION_CONCRETE_FUNC_SOME_DIVMOD(SomeWordN, WordN, safeDivMod, divMod)
  SAFE_DIVISION_CONCRETE_FUNC_SOME(SomeWordN, WordN, safeQuot, quot)
  SAFE_DIVISION_CONCRETE_FUNC_SOME(SomeWordN, WordN, safeRem, rem)
  SAFE_DIVISION_CONCRETE_FUNC_SOME_DIVMOD(SomeWordN, WordN, safeQuotRem, quotRem)
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
  SafeDivision SymInteger m where
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
  SafeDivision (SymIntN n) m where
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
  SafeDivision (SymWordN n) m where
  SAFE_DIVISION_SYMBOLIC_FUNC(safeDiv, SymWordN, pevalDivIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC(safeMod, SymWordN, pevalModIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC(safeQuot, SymWordN, pevalQuotIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC(safeRem, SymWordN, pevalRemIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC2(safeDivMod, SymWordN, pevalDivIntegralTerm, pevalModIntegralTerm)
  SAFE_DIVISION_SYMBOLIC_FUNC2(safeQuotRem, SymWordN, pevalQuotIntegralTerm, pevalRemIntegralTerm)
#endif
