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

{- HLINT ignore "Redundant bracket" -}

-- |
-- Module      :   Grisette.Core.Data.Class.SafeArith
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.Class.SafeArith
  ( -- * Symbolic integer operations
    ArithException (..),
    SafeDivision (..),
    SafeLinearArith (..),
    SymIntegerOp,
  )
where

import Control.Exception
import Control.Monad.Except
import Data.Int
import Data.Typeable
import Data.Word
import GHC.TypeNats
import Grisette.Core.Control.Monad.Union
import Grisette.Core.Data.BV
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Error
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Data.Class.SOrd
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Core.Data.Class.Solvable

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim

-- | Safe division with monadic error handling in multi-path
-- execution. These procedures throw an exception when the
-- divisor is zero. The result should be able to handle errors with
-- `MonadError`.
class (SOrd a, Num a, Mergeable a, Mergeable e) => SafeDivision e a | a -> e where
  -- | Safe signed 'div' with monadic error handling in multi-path execution.
  --
  -- >>> safeDiv (ssym "a") (ssym "b") :: ExceptT ArithException UnionM SymInteger
  -- ExceptT {If (= b 0) (Left divide by zero) (Right (div a b))}
  safeDiv :: (MonadError e uf, MonadUnion uf) => a -> a -> uf a
  safeDiv l r = do
    (d, _) <- safeDivMod l r
    mrgSingle d

  -- | Safe signed 'mod' with monadic error handling in multi-path execution.
  --
  -- >>> safeMod (ssym "a") (ssym "b") :: ExceptT ArithException UnionM SymInteger
  -- ExceptT {If (= b 0) (Left divide by zero) (Right (mod a b))}
  safeMod :: (MonadError e uf, MonadUnion uf) => a -> a -> uf a
  safeMod l r = do
    (_, m) <- safeDivMod l r
    mrgSingle m

  -- | Safe signed 'divMod' with monadic error handling in multi-path execution.
  --
  -- >>> safeDivMod (ssym "a") (ssym "b") :: ExceptT ArithException UnionM (SymInteger, SymInteger)
  -- ExceptT {If (= b 0) (Left divide by zero) (Right ((div a b),(mod a b)))}
  safeDivMod :: (MonadError e uf, MonadUnion uf) => a -> a -> uf (a, a)
  safeDivMod l r = do
    d <- safeDiv l r
    m <- safeMod l r
    mrgSingle (d, m)

  -- | Safe signed 'quot' with monadic error handling in multi-path execution.
  safeQuot :: (MonadError e uf, MonadUnion uf) => a -> a -> uf a
  safeQuot l r = do
    (d, m) <- safeDivMod l r
    mrgIf
      ((l >=~ 0 &&~ r >~ 0) ||~ (l <=~ 0 &&~ r <~ 0) ||~ m ==~ 0)
      (mrgSingle d)
      (mrgSingle $ d + 1)

  -- | Safe signed 'rem' with monadic error handling in multi-path execution.
  safeRem :: (MonadError e uf, MonadUnion uf) => a -> a -> uf a
  safeRem l r = do
    (d, m) <- safeDivMod l r
    mrgIf
      ((l >=~ 0 &&~ r >~ 0) ||~ (l <=~ 0 &&~ r <~ 0) ||~ m ==~ 0)
      (mrgSingle m)
      (mrgSingle $ m - r)

  -- | Safe signed 'quotRem' with monadic error handling in multi-path execution.
  safeQuotRem :: (MonadError e uf, MonadUnion uf) => a -> a -> uf (a, a)
  safeQuotRem l r = do
    (d, m) <- safeDivMod l r
    mrgIf
      ((l >=~ 0 &&~ r >~ 0) ||~ (l <=~ 0 &&~ r <~ 0) ||~ m ==~ 0)
      (mrgSingle (d, m))
      (mrgSingle (d + 1, m - r))

  -- | Safe signed 'div' with monadic error handling in multi-path execution.
  -- The error is transformed.
  --
  -- >>> safeDiv' (const ()) (ssym "a") (ssym "b") :: ExceptT () UnionM SymInteger
  -- ExceptT {If (= b 0) (Left ()) (Right (div a b))}
  safeDiv' :: (MonadError e' uf, MonadUnion uf, Mergeable e') => (e -> e') -> a -> a -> uf a
  safeDiv' t l r = do
    (d, _) <- safeDivMod' t l r
    mrgSingle d

  -- | Safe signed 'mod' with monadic error handling in multi-path execution.
  -- The error is transformed.
  --
  -- >>> safeMod' (const ()) (ssym "a") (ssym "b") :: ExceptT () UnionM SymInteger
  -- ExceptT {If (= b 0) (Left ()) (Right (mod a b))}
  safeMod' :: (MonadError e' uf, MonadUnion uf, Mergeable e') => (e -> e') -> a -> a -> uf a
  safeMod' t l r = do
    (_, m) <- safeDivMod' t l r
    mrgSingle m

  -- | Safe signed 'divMod' with monadic error handling in multi-path execution.
  -- The error is transformed.
  --
  -- >>> safeDivMod' (const ()) (ssym "a") (ssym "b") :: ExceptT () UnionM (SymInteger, SymInteger)
  -- ExceptT {If (= b 0) (Left ()) (Right ((div a b),(mod a b)))}
  safeDivMod' :: (MonadError e' uf, MonadUnion uf, Mergeable e') => (e -> e') -> a -> a -> uf (a, a)
  safeDivMod' t l r = do
    d <- safeDiv' t l r
    m <- safeMod' t l r
    mrgSingle (d, m)

  -- | Safe signed 'quot' with monadic error handling in multi-path execution.
  -- The error is transformed.
  safeQuot' :: (MonadError e' uf, MonadUnion uf, Mergeable e') => (e -> e') -> a -> a -> uf a
  safeQuot' t l r = do
    (d, m) <- safeDivMod' t l r
    mrgIf
      ((l >=~ 0 &&~ r >~ 0) ||~ (l <=~ 0 &&~ r <~ 0) ||~ m ==~ 0)
      (mrgSingle d)
      (mrgSingle $ d + 1)

  -- | Safe signed 'rem' with monadic error handling in multi-path execution.
  -- The error is transformed.
  safeRem' :: (MonadError e' uf, MonadUnion uf, Mergeable e') => (e -> e') -> a -> a -> uf a
  safeRem' t l r = do
    (d, m) <- safeDivMod' t l r
    mrgIf
      ((l >=~ 0 &&~ r >~ 0) ||~ (l <=~ 0 &&~ r <~ 0) ||~ m ==~ 0)
      (mrgSingle m)
      (mrgSingle $ m - r)

  -- | Safe signed 'quotRem' with monadic error handling in multi-path execution.
  -- The error is transformed.
  safeQuotRem' :: (MonadError e' uf, MonadUnion uf, Mergeable e') => (e -> e') -> a -> a -> uf (a, a)
  safeQuotRem' t l r = do
    (d, m) <- safeDivMod' t l r
    mrgIf
      ((l >=~ 0 &&~ r >~ 0) ||~ (l <=~ 0 &&~ r <~ 0) ||~ m ==~ 0)
      (mrgSingle (d, m))
      (mrgSingle (d + 1, m - r))

  {-# MINIMAL (safeDivMod | (safeDiv, safeMod)), (safeDivMod' | (safeDiv', safeMod')) #-}

#define QUOTE() '
#define QID(a) a
#define QRIGHT(a) QID(a)'

#define QRIGHTT(a) QID(a)' t'

#define SAFE_DIVISION_FUNC(name, op) \
name _ r | r == 0 = merge $ throwError DivideByZero; \
name l r = mrgSingle $ l `op` r; \
QRIGHTT(name) _ r | r == 0 = let t1 = t' in merge $ throwError (t' DivideByZero); \
QRIGHTT(name) l r = mrgSingle $ l `op` r

#define SAFE_DIVISION_CONCRETE(type) \
instance SafeDivision ArithException type where \
  SAFE_DIVISION_FUNC(safeDiv, div); \
  SAFE_DIVISION_FUNC(safeMod, mod); \
  SAFE_DIVISION_FUNC(safeDivMod, divMod); \
  SAFE_DIVISION_FUNC(safeQuot, quot); \
  SAFE_DIVISION_FUNC(safeRem, rem); \
  SAFE_DIVISION_FUNC(safeQuotRem, quotRem)

#define SAFE_DIVISION_CONCRETE_BV(type) \
instance (KnownNat n, 1 <= n) => SafeDivision ArithException (type n) where \
  SAFE_DIVISION_FUNC(safeDiv, div); \
  SAFE_DIVISION_FUNC(safeMod, mod); \
  SAFE_DIVISION_FUNC(safeDivMod, divMod); \
  SAFE_DIVISION_FUNC(safeQuot, quot); \
  SAFE_DIVISION_FUNC(safeRem, rem); \
  SAFE_DIVISION_FUNC(safeQuotRem, quotRem)

#if 1
SAFE_DIVISION_CONCRETE(Integer)
SAFE_DIVISION_CONCRETE(Int8)
SAFE_DIVISION_CONCRETE(Int16)
SAFE_DIVISION_CONCRETE(Int32)
SAFE_DIVISION_CONCRETE(Int64)
SAFE_DIVISION_CONCRETE(Int)
SAFE_DIVISION_CONCRETE(Word8)
SAFE_DIVISION_CONCRETE(Word16)
SAFE_DIVISION_CONCRETE(Word32)
SAFE_DIVISION_CONCRETE(Word64)
SAFE_DIVISION_CONCRETE(Word)
#endif

#define SAFE_DIVISION_FUNC_SOME(stype, type, name, op) \
  name (stype (l :: type l)) (stype (r :: type r)) = \
    (case sameNat (Proxy @l) (Proxy @r) of \
      Just Refl -> \
        if r == 0 \
          then merge $ throwError $ Right DivideByZero \
          else mrgSingle $ stype $ l `op` r; \
      Nothing -> merge $ throwError $ Left BitwidthMismatch); \
  QRIGHT(name) t (stype (l :: type l)) (stype (r :: type r)) = \
    (case sameNat (Proxy @l) (Proxy @r) of \
      Just Refl -> \
        if r == 0 \
          then merge $ throwError $ t (Right DivideByZero) \
          else mrgSingle $ stype $ l `op` r; \
      Nothing -> merge $ throwError $ t (Left BitwidthMismatch))

#define SAFE_DIVISION_FUNC_SOME_DIVMOD(stype, type, name, op) \
  name (stype (l :: type l)) (stype (r :: type r)) = \
    (case sameNat (Proxy @l) (Proxy @r) of \
      Just Refl -> \
        if r == 0 \
          then merge $ throwError $ Right DivideByZero \
          else (case l `op` r of (d, m) -> mrgSingle (stype d, stype m)); \
      Nothing -> merge $ throwError $ Left BitwidthMismatch); \
  QRIGHT(name) t (stype (l :: type l)) (stype (r :: type r)) = \
    (case sameNat (Proxy @l) (Proxy @r) of \
      Just Refl -> \
        if r == 0 \
          then merge $ throwError $ t (Right DivideByZero) \
          else (case l `op` r of (d, m) -> mrgSingle (stype d, stype m)); \
      Nothing -> merge $ throwError $ t (Left BitwidthMismatch))

#if 1
SAFE_DIVISION_CONCRETE_BV(IntN)
SAFE_DIVISION_CONCRETE_BV(WordN)
instance SafeDivision (Either BitwidthMismatch ArithException) SomeIntN where
  SAFE_DIVISION_FUNC_SOME(SomeIntN, IntN, safeDiv, div)
  SAFE_DIVISION_FUNC_SOME(SomeIntN, IntN, safeMod, mod)
  SAFE_DIVISION_FUNC_SOME_DIVMOD(SomeIntN, IntN, safeDivMod, divMod)
  SAFE_DIVISION_FUNC_SOME(SomeIntN, IntN, safeQuot, quot)
  SAFE_DIVISION_FUNC_SOME(SomeIntN, IntN, safeRem, rem)
  SAFE_DIVISION_FUNC_SOME_DIVMOD(SomeIntN, IntN, safeQuotRem, quotRem)

instance SafeDivision (Either BitwidthMismatch ArithException) SomeWordN where
  SAFE_DIVISION_FUNC_SOME(SomeWordN, WordN, safeDiv, div)
  SAFE_DIVISION_FUNC_SOME(SomeWordN, WordN, safeMod, mod)
  SAFE_DIVISION_FUNC_SOME_DIVMOD(SomeWordN, WordN, safeDivMod, divMod)
  SAFE_DIVISION_FUNC_SOME(SomeWordN, WordN, safeQuot, quot)
  SAFE_DIVISION_FUNC_SOME(SomeWordN, WordN, safeRem, rem)
  SAFE_DIVISION_FUNC_SOME_DIVMOD(SomeWordN, WordN, safeQuotRem, quotRem)
#endif

class SafeLinearArith a where
  -- | Safe signed '+' with monadic error handling in multi-path execution.
  -- Overflows are treated as errors.
  --
  -- >>> safeAdd AssertionError (ssym "a") (ssym "b") :: ExceptT AssertionError UnionM SymInteger
  -- ExceptT {Right (+ a b)}
  -- >>> safeAdd AssertionError (ssym "a") (ssym "b") :: ExceptT AssertionError UnionM (SymIntN 4)
  -- ExceptT {If (|| (&& (< 0x0 a) (&& (< 0x0 b) (< (+ a b) 0x0))) (&& (< a 0x0) (&& (< b 0x0) (<= 0x0 (+ a b))))) (Left AssertionError) (Right (+ a b))}
  safeAdd :: (MonadError e uf, MonadUnion uf, Mergeable e) => e -> a -> a -> uf a

  -- | Safe signed 'negate' with monadic error handling in multi-path execution.
  -- Overflows are treated as errors.
  --
  -- >>> safeNeg AssertionError (ssym "a") :: ExceptT AssertionError UnionM SymInteger
  -- ExceptT {Right (- a)}
  -- >>> safeNeg AssertionError (ssym "a") :: ExceptT AssertionError UnionM (SymIntN 4)
  -- ExceptT {If (= a 0x8) (Left AssertionError) (Right (- a))}
  safeNeg :: (MonadError e uf, MonadUnion uf, Mergeable e) => e -> a -> uf a

  -- | Safe signed '-' with monadic error handling in multi-path execution.
  -- Overflows are treated as errors.
  --
  -- >>> safeMinus AssertionError (ssym "a") (ssym "b") :: ExceptT AssertionError UnionM SymInteger
  -- ExceptT {Right (+ a (- b))}
  -- >>> safeMinus AssertionError (ssym "a") (ssym "b") :: ExceptT AssertionError UnionM (SymIntN 4)
  -- ExceptT {If (|| (&& (<= 0x0 a) (&& (< b 0x0) (< (+ a (- b)) 0x0))) (&& (< a 0x0) (&& (< 0x0 b) (< 0x0 (+ a (- b)))))) (Left AssertionError) (Right (+ a (- b)))}
  safeMinus :: (MonadError e uf, MonadUnion uf, Mergeable e) => e -> a -> a -> uf a

-- | Aggregation for the operations on symbolic integer types
class (Num a, SEq a, SOrd a, Solvable Integer a) => SymIntegerOp a
