{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- |
-- Module      :   Grisette.Core.Data.Class.Integer
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.Class.Integer
  ( -- * Symbolic integer operations
    ArithException (..),
    SafeDivision (..),
    SafeLinearArith (..),
    SymIntegerOp,
  )
where

import Control.Exception
import Control.Monad.Except
import Grisette.Core.Control.Monad.Union
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Error
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Data.Class.SOrd
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Core.Data.Class.Solvable
import Data.Int
import Data.Word
import GHC.TypeNats
import Grisette.Core.Data.BV

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim

-- | Safe division with monadic error handling in multi-path
-- execution. These procedures throw an exception when the
-- divisor is zero. The result should be able to handle errors with
-- `MonadError`.
class (SOrd a, Num a, Mergeable a) => SafeDivision a where
  -- | Safe signed 'div' with monadic error handling in multi-path execution.
  --
  -- >>> safeDiv AssertionError (ssym "a") (ssym "b") :: ExceptT AssertionError UnionM SymInteger
  -- ExceptT {If (= b 0) (Left AssertionError) (Right (div a b))}
  safeDiv :: (MonadError e uf, MonadUnion uf, Mergeable e) => e -> a -> a -> uf a
  safeDiv e l r = do
    (d, _) <- safeDivMod e l r
    mrgSingle d

  -- | Safe signed 'mod' with monadic error handling in multi-path execution.
  --
  -- >>> safeMod AssertionError (ssym "a") (ssym "b") :: ExceptT AssertionError UnionM SymInteger
  -- ExceptT {If (= b 0) (Left AssertionError) (Right (mod a b))}
  safeMod :: (MonadError e uf, MonadUnion uf, Mergeable e) => e -> a -> a -> uf a
  safeMod e l r = do
    (_, m) <- safeDivMod e l r
    mrgSingle m

  -- | Safe signed 'div' with monadic error handling in multi-path execution.
  --
  -- >>> safeDivMod AssertionError (ssym "a") (ssym "b") :: ExceptT AssertionError UnionM (SymInteger, SymInteger)
  -- ExceptT {If (= b 0) (Left AssertionError) (Right ((div a b),(mod a b)))}
  safeDivMod :: (MonadError e uf, MonadUnion uf, Mergeable e) => e -> a -> a -> uf (a, a)
  safeDivMod e l r = do
    d <- safeDiv e l r
    m <- safeMod e l r
    mrgSingle (d, m)

  -- | Safe signed 'quot' with monadic error handling in multi-path execution.
  safeQuot :: (MonadError e uf, MonadUnion uf, Mergeable e) => e -> a -> a -> uf a
  safeQuot e l r = do
    (d, m) <- safeDivMod e l r
    mrgIf
      ((l >=~ 0 &&~ r >~ 0) ||~ (l <=~ 0 &&~ r <~ 0) ||~ m ==~ 0)
      (mrgSingle d)
      (mrgSingle $ d + 1)

  -- | Safe signed 'rem' with monadic error handling in multi-path execution.
  safeRem :: (MonadError e uf, MonadUnion uf, Mergeable e) => e -> a -> a -> uf a
  safeRem e l r = do
    (d, m) <- safeDivMod e l r
    mrgIf
      ((l >=~ 0 &&~ r >~ 0) ||~ (l <=~ 0 &&~ r <~ 0) ||~ m ==~ 0)
      (mrgSingle m)
      (mrgSingle $ m - r)

  -- | Safe signed 'quotRem' with monadic error handling in multi-path execution.
  safeQuotRem :: (MonadError e uf, MonadUnion uf, Mergeable e) => e -> a -> a -> uf (a, a)
  safeQuotRem e l r = do
    (d, m) <- safeDivMod e l r
    mrgIf
      ((l >=~ 0 &&~ r >~ 0) ||~ (l <=~ 0 &&~ r <~ 0) ||~ m ==~ 0)
      (mrgSingle (d, m))
      (mrgSingle (d + 1, m - r))

  {-# MINIMAL (safeDivMod | (safeDiv, safeMod)) #-}

#define SAFE_DIVISION_CONCRETE(type) \
instance SafeDivision type where \
  safeDiv e _ r | r == 0 = merge $ throwError e; \
  safeDiv _ l r = mrgSingle $ l `div` r; \
  safeMod e _ r | r == 0 = merge $ throwError e; \
  safeMod _ l r = mrgSingle $ l `mod` r; \
  safeDivMod e _ r | r == 0 = merge $ throwError e; \
  safeDivMod _ l r = mrgSingle $ l `divMod` r; \
  safeQuot e _ r | r == 0 = merge $ throwError e; \
  safeQuot _ l r = mrgSingle $ l `quot` r; \
  safeRem e _ r | r == 0 = merge $ throwError e; \
  safeRem _ l r = mrgSingle $ l `rem` r; \
  safeQuotRem e _ r | r == 0 = merge $ throwError e; \
  safeQuotRem _ l r = mrgSingle $ l `quotRem` r

#define SAFE_DIVISION_CONCRETE_BV(type) \
instance (KnownNat n, 1 <= n) => SafeDivision (type n) where \
  safeDiv e _ r | r == 0 = merge $ throwError e; \
  safeDiv _ l r = mrgSingle $ l `div` r; \
  safeMod e _ r | r == 0 = merge $ throwError e; \
  safeMod _ l r = mrgSingle $ l `mod` r; \
  safeDivMod e _ r | r == 0 = merge $ throwError e; \
  safeDivMod _ l r = mrgSingle $ l `divMod` r; \
  safeQuot e _ r | r == 0 = merge $ throwError e; \
  safeQuot _ l r = mrgSingle $ l `quot` r; \
  safeRem e _ r | r == 0 = merge $ throwError e; \
  safeRem _ l r = mrgSingle $ l `rem` r; \
  safeQuotRem e _ r | r == 0 = merge $ throwError e; \
  safeQuotRem _ l r = mrgSingle $ l `quotRem` r

#if 1
SAFE_DIVISION_CONCRETE(Integer)
SAFE_DIVISION_CONCRETE(Int8)
SAFE_DIVISION_CONCRETE(Int16)
SAFE_DIVISION_CONCRETE(Int32)
SAFE_DIVISION_CONCRETE(Int64)
SAFE_DIVISION_CONCRETE(Int)
SAFE_DIVISION_CONCRETE(SomeIntN)
SAFE_DIVISION_CONCRETE(Word8)
SAFE_DIVISION_CONCRETE(Word16)
SAFE_DIVISION_CONCRETE(Word32)
SAFE_DIVISION_CONCRETE(Word64)
SAFE_DIVISION_CONCRETE(Word)
SAFE_DIVISION_CONCRETE(SomeWordN)
#endif

#if 1
SAFE_DIVISION_CONCRETE_BV(IntN)
SAFE_DIVISION_CONCRETE_BV(WordN)
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
class (Num a, SEq a, SOrd a, Solvable Integer a, SafeDivision a, SafeLinearArith a) => SymIntegerOp a
