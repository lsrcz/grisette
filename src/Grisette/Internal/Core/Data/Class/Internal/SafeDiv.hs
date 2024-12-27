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
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Internal.Core.Data.Class.Internal.SafeDiv
  ( ArithException (..),
    SafeDiv (..),
    DivOr (..),
    divOrZero,
    modOrDividend,
    quotOrZero,
    remOrDividend,
    divModOrZeroDividend,
    quotRemOrZeroDividend,
  )
where

import Control.Exception (ArithException (DivideByZero, Overflow, Underflow))
import Control.Monad.Except (MonadError)
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.TryMerge
  ( TryMerge,
    mrgSingle,
  )
import Grisette.Lib.Data.Functor (mrgFmap)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
-- >>> import Control.Monad.Except
-- >>> import Control.Exception

-- | Safe division handling with default values returned on exception.
class DivOr a where
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
divOrZero :: (DivOr a, Num a) => a -> a -> a
divOrZero l = divOr (l - l) l
{-# INLINE divOrZero #-}

-- | Safe 'mod' with dividend returned on exception.
modOrDividend :: (DivOr a, Num a) => a -> a -> a
modOrDividend l = modOr l l
{-# INLINE modOrDividend #-}

-- | Safe 'quot' with 0 returned on exception.
quotOrZero :: (DivOr a, Num a) => a -> a -> a
quotOrZero l = quotOr (l - l) l
{-# INLINE quotOrZero #-}

-- | Safe 'rem' with dividend returned on exception.
remOrDividend :: (DivOr a, Num a) => a -> a -> a
remOrDividend l = remOr l l
{-# INLINE remOrDividend #-}

-- | Safe 'divMod' with 0 returned on exception.
divModOrZeroDividend :: (DivOr a, Num a) => a -> a -> (a, a)
divModOrZeroDividend l = divModOr (l - l, l) l
{-# INLINE divModOrZeroDividend #-}

-- | Safe 'quotRem' with 0 returned on exception.
quotRemOrZeroDividend :: (DivOr a, Num a) => a -> a -> (a, a)
quotRemOrZeroDividend l = quotRemOr (l - l, l) l
{-# INLINE quotRemOrZeroDividend #-}

-- | Safe division with monadic error handling in multi-path
-- execution. These procedures throw an exception when the
-- divisor is zero. The result should be able to handle errors with
-- `MonadError`.
class (MonadError e m, TryMerge m, Mergeable a, DivOr a) => SafeDiv e a m where
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
