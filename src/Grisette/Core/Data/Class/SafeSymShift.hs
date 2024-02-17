{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Core.Data.Class.SafeSymShift
  ( SafeSymShift (..),
  )
where

import Control.Exception (ArithException (Overflow))
import Control.Monad.Error.Class (MonadError)
import Data.Bits (Bits (shiftL, shiftR), FiniteBits (finiteBitSize))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeLits (KnownNat, type (<=))
import Grisette.Core.Control.Monad.Union (MonadUnion)
import Grisette.Core.Data.BV (IntN, WordN)
import Grisette.Core.Data.Class.LogicalOp
  ( LogicalOp ((.&&), (.||)),
  )
import Grisette.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Core.Data.Class.SOrd
  ( SOrd ((.<), (.>=)),
  )
import Grisette.Core.Data.Class.SimpleMergeable
  ( mrgIf,
  )
import Grisette.Core.Data.Class.SymShift (SymShift)
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bits
  ( pevalShiftLeftTerm,
    pevalShiftRightTerm,
  )
import Grisette.IR.SymPrim.Data.SymPrim (SymIntN (SymIntN), SymWordN (SymWordN))
import Grisette.Lib.Control.Monad (mrgReturn)
import Grisette.Lib.Control.Monad.Except (mrgThrowError)

-- | Safe version for `shiftL` or `shiftR`.
--
-- The `safeSymShiftL` and `safeSymShiftR` and their primed versions are defined
-- for all non-negative shift amounts.
--
-- * Shifting by negative shift amounts is an error.
-- * The result is defined to be 0 when shifting left by more than or equal to
-- the  bit size of the number.
-- * The result is defined to be 0 when shifting right by more than or equal to
-- the bit size of the number and the number is unsigned or signed non-negative.
-- * The result is defined to be -1 when shifting right by more than or equal to
-- the bit size of the number and the number is signed negative.
--
-- The `safeSymStrictShiftL` and `safeSymStrictShiftR` and their primed versions
-- are defined for all non-negative shift amounts that is less than the bit
-- size. Shifting by more than or equal to the bit size is an error, otherwise
-- they are the same as the non-strict versions.
class (SymShift a) => SafeSymShift e a | a -> e where
  safeSymShiftL :: (MonadError e m, MonadUnion m) => a -> a -> m a
  safeSymShiftL = safeSymShiftL' id
  safeSymShiftR :: (MonadError e m, MonadUnion m) => a -> a -> m a
  safeSymShiftR = safeSymShiftR' id
  safeSymShiftL' ::
    (MonadError e' m, MonadUnion m) => (e -> e') -> a -> a -> m a
  safeSymShiftR' ::
    (MonadError e' m, MonadUnion m) => (e -> e') -> a -> a -> m a
  safeSymStrictShiftL :: (MonadError e m, MonadUnion m) => a -> a -> m a
  safeSymStrictShiftL = safeSymStrictShiftL' id
  safeSymStrictShiftR :: (MonadError e m, MonadUnion m) => a -> a -> m a
  safeSymStrictShiftR = safeSymStrictShiftR' id
  safeSymStrictShiftL' ::
    (MonadError e' m, MonadUnion m) => (e -> e') -> a -> a -> m a
  safeSymStrictShiftR' ::
    (MonadError e' m, MonadUnion m) => (e -> e') -> a -> a -> m a
  {-# MINIMAL
    safeSymShiftL',
    safeSymShiftR',
    safeSymStrictShiftL',
    safeSymStrictShiftR'
    #-}

-- | This function handles the case when the shift amount is out the range of
-- `Int` correctly.
safeSymShiftLConcreteNum ::
  (MonadError e m, MonadUnion m, Integral a, FiniteBits a, Mergeable a) =>
  e ->
  Bool ->
  a ->
  a ->
  m a
safeSymShiftLConcreteNum e _ _ s | s < 0 = mrgThrowError e
safeSymShiftLConcreteNum e allowLargeShiftAmount a s
  | (fromIntegral s :: Integer) >= fromIntegral (finiteBitSize a) =
      if allowLargeShiftAmount then mrgReturn 0 else mrgThrowError e
safeSymShiftLConcreteNum _ _ a s = mrgReturn $ shiftL a (fromIntegral s)

-- | This function handles the case when the shift amount is out the range of
-- `Int` correctly.
safeSymShiftRConcreteNum ::
  (MonadError e m, MonadUnion m, Integral a, FiniteBits a, Mergeable a) =>
  e ->
  Bool ->
  a ->
  a ->
  m a
safeSymShiftRConcreteNum e _ _ s | s < 0 = mrgThrowError e
safeSymShiftRConcreteNum e allowLargeShiftAmount a s
  | (fromIntegral s :: Integer) >= fromIntegral (finiteBitSize a) =
      if allowLargeShiftAmount then mrgReturn 0 else mrgThrowError e
safeSymShiftRConcreteNum _ _ a s = mrgReturn $ shiftR a (fromIntegral s)

#define SAFE_SYM_SHIFT_CONCRETE(T) \
  instance SafeSymShift ArithException T where \
    safeSymShiftL' f = safeSymShiftLConcreteNum (f Overflow) True; \
    safeSymShiftR' f = safeSymShiftRConcreteNum (f Overflow) True; \
    safeSymStrictShiftL' f = safeSymShiftLConcreteNum (f Overflow) False; \
    safeSymStrictShiftR' f = safeSymShiftRConcreteNum (f Overflow) False

#if 1
SAFE_SYM_SHIFT_CONCRETE(Word8)
SAFE_SYM_SHIFT_CONCRETE(Word16)
SAFE_SYM_SHIFT_CONCRETE(Word32)
SAFE_SYM_SHIFT_CONCRETE(Word64)
SAFE_SYM_SHIFT_CONCRETE(Word)
SAFE_SYM_SHIFT_CONCRETE(Int8)
SAFE_SYM_SHIFT_CONCRETE(Int16)
SAFE_SYM_SHIFT_CONCRETE(Int32)
SAFE_SYM_SHIFT_CONCRETE(Int64)
SAFE_SYM_SHIFT_CONCRETE(Int)
#endif

instance (KnownNat n, 1 <= n) => SafeSymShift ArithException (WordN n) where
  safeSymShiftL' f = safeSymShiftLConcreteNum (f Overflow) True
  safeSymShiftR' f = safeSymShiftRConcreteNum (f Overflow) True
  safeSymStrictShiftL' f = safeSymShiftLConcreteNum (f Overflow) False
  safeSymStrictShiftR' f = safeSymShiftRConcreteNum (f Overflow) False

instance (KnownNat n, 1 <= n) => SafeSymShift ArithException (IntN n) where
  safeSymShiftL' f = safeSymShiftLConcreteNum (f Overflow) True
  safeSymShiftR' f = safeSymShiftRConcreteNum (f Overflow) True
  safeSymStrictShiftL' f = safeSymShiftLConcreteNum (f Overflow) False
  safeSymStrictShiftR' f = safeSymShiftRConcreteNum (f Overflow) False

instance (KnownNat n, 1 <= n) => SafeSymShift ArithException (SymWordN n) where
  safeSymShiftL' _ (SymWordN a) (SymWordN s) =
    return $ SymWordN $ pevalShiftLeftTerm a s
  safeSymShiftR' _ (SymWordN a) (SymWordN s) =
    return $ SymWordN $ pevalShiftRightTerm a s
  safeSymStrictShiftL' f a@(SymWordN ta) s@(SymWordN ts) =
    mrgIf
      (s .>= fromIntegral (finiteBitSize a))
      (mrgThrowError $ f Overflow)
      (return $ SymWordN $ pevalShiftLeftTerm ta ts)
  safeSymStrictShiftR' f a@(SymWordN ta) s@(SymWordN ts) =
    mrgIf
      (s .>= fromIntegral (finiteBitSize a))
      (mrgThrowError $ f Overflow)
      (return $ SymWordN $ pevalShiftRightTerm ta ts)

instance (KnownNat n, 1 <= n) => SafeSymShift ArithException (SymIntN n) where
  safeSymShiftL' f (SymIntN a) ss@(SymIntN s) =
    mrgIf
      (ss .< 0)
      (mrgThrowError $ f Overflow)
      (return $ SymIntN $ pevalShiftLeftTerm a s)
  safeSymShiftR' f (SymIntN a) ss@(SymIntN s) =
    mrgIf
      (ss .< 0)
      (mrgThrowError $ f Overflow)
      (return $ SymIntN $ pevalShiftRightTerm a s)
  safeSymStrictShiftL' f a@(SymIntN ta) s@(SymIntN ts) =
    mrgIf
      (s .< 0 .|| (bs .>= 0 .&& s .>= bs))
      (mrgThrowError $ f Overflow)
      (return $ SymIntN $ pevalShiftLeftTerm ta ts)
    where
      bs = fromIntegral (finiteBitSize a)
  safeSymStrictShiftR' f a@(SymIntN ta) s@(SymIntN ts) =
    mrgIf
      (s .< 0 .|| (bs .>= 0 .&& s .>= bs))
      (mrgThrowError $ f Overflow)
      (return $ SymIntN $ pevalShiftRightTerm ta ts)
    where
      bs = fromIntegral (finiteBitSize a)
