{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.Core.Data.Class.Integer
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.Class.Integer
  ( -- * Note for the examples

    --

    -- | This module does not contain the implementation for solvable (see "Grisette.Core#solvable")
    -- types, and the examples in this module rely on the implementations in
    -- the [grisette-symir](https://hackage.haskell.org/package/grisette-symir) package.

    -- * Symbolic integer operations
    ArithException (..),
    SignedDivMod (..),
    UnsignedDivMod (..),
    SignedQuotRem (..),
    GSymIntegerOp,
  )
where

import Control.Exception
import Control.Monad.Except
import Grisette.Core.Control.Monad.Union
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Error
import Grisette.Core.Data.Class.SOrd
import Grisette.Core.Data.Class.Solvable

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim

-- | Safe signed 'div' and 'mod' with monadic error handling in multi-path
-- execution. These procedures show throw 'DivideByZero' exception when the
-- divisor is zero. The result should be able to handle errors with
-- `MonadError`, and the error type should be compatible with 'ArithException'
-- (see 'TransformError' for more details).
class SignedDivMod bool a where
  -- | Safe signed 'div' with monadic error handling in multi-path execution.
  --
  -- >>> divs (ssym "a") (ssym "b") :: ExceptT AssertionError UnionM SymInteger
  -- ExceptT {If (= b 0) (Left AssertionError) (Right (div a b))}
  divs :: (MonadError e uf, GMonadUnion bool uf, TransformError ArithException e) => a -> a -> uf a

  -- | Safe signed 'mod' with monadic error handling in multi-path execution.
  --
  -- >>> mods (ssym "a") (ssym "b") :: ExceptT AssertionError UnionM SymInteger
  -- ExceptT {If (= b 0) (Left AssertionError) (Right (mod a b))}
  mods :: (MonadError e uf, GMonadUnion bool uf, TransformError ArithException e) => a -> a -> uf a

-- | Safe unsigned 'div' and 'mod' with monadic error handling in multi-path
-- execution. These procedures show throw 'DivideByZero' exception when the
-- divisor is zero. The result should be able to handle errors with
-- `MonadError`, and the error type should be compatible with 'ArithException'
-- (see 'TransformError' for more details).
class UnsignedDivMod bool a where
  udivs :: (MonadError e uf, GMonadUnion bool uf, TransformError ArithException e) => a -> a -> uf a
  umods :: (MonadError e uf, GMonadUnion bool uf, TransformError ArithException e) => a -> a -> uf a

-- | Safe signed 'quot' and 'rem' with monadic error handling in multi-path
-- execution. These procedures show throw 'DivideByZero' exception when the
-- divisor is zero. The result should be able to handle errors with
-- `MonadError`, and the error type should be compatible with 'ArithException'
-- (see 'TransformError' for more details).
class SignedQuotRem bool a where
  quots :: (MonadError e uf, GMonadUnion bool uf, TransformError ArithException e) => a -> a -> uf a
  rems :: (MonadError e uf, GMonadUnion bool uf, TransformError ArithException e) => a -> a -> uf a

-- | Aggregation for the operations on symbolic integer types
class (Num a, GSEq bool a, GSOrd bool a, Solvable Integer a) => GSymIntegerOp bool a
