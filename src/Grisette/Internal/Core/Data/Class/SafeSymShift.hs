{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Internal.Core.Data.Class.SafeSymShift
  ( SafeSymShift (..),
  )
where

import Control.Exception (ArithException (Overflow))
import Control.Monad.Error.Class (MonadError)
import Data.Bits (Bits (shiftL, shiftR), FiniteBits (finiteBitSize))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeLits (KnownNat, type (<=))
import Grisette.Internal.Core.Control.Monad.Union (MonadUnion)
import Grisette.Internal.Core.Data.Class.LogicalOp
  ( LogicalOp ((.&&), (.||)),
  )
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.SOrd
  ( SOrd ((.<), (.>=)),
  )
import Grisette.Internal.Core.Data.Class.SimpleMergeable
  ( mrgIf,
  )
import Grisette.Internal.Core.Data.Class.TryMerge (TryMerge)
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.Prim.Term
  ( PEvalShiftTerm
      ( pevalShiftLeftTerm,
        pevalShiftRightTerm
      ),
  )
import Grisette.Internal.SymPrim.SymBV (SymIntN (SymIntN), SymWordN (SymWordN))
import Grisette.Lib.Control.Monad (mrgReturn)
import Grisette.Lib.Control.Monad.Except (mrgThrowError)

-- | Safe version for `shiftL` or `shiftR`.
--
-- The `safeSymShiftL` and `safeSymShiftR` are defined for all non-negative
-- shift amounts.
--
-- * Shifting by negative shift amounts is an error.
-- * The result is defined to be 0 when shifting left by more than or equal to
-- the  bit size of the number.
-- * The result is defined to be 0 when shifting right by more than or equal to
-- the bit size of the number and the number is unsigned or signed non-negative.
-- * The result is defined to be -1 when shifting right by more than or equal to
-- the bit size of the number and the number is signed negative.
--
-- The `safeSymStrictShiftL` and `safeSymStrictShiftR` are defined for all
-- non-negative shift amounts that is less than the bit size. Shifting by more
-- than or equal to the bit size is an error, otherwise they are the same as the
-- non-strict versions.
class (MonadError e m, TryMerge m, Mergeable a) => SafeSymShift e a m where
  safeSymShiftL :: a -> a -> m a
  safeSymShiftR :: a -> a -> m a
  safeSymStrictShiftL :: a -> a -> m a
  safeSymStrictShiftR :: a -> a -> m a

-- | This function handles the case when the shift amount is out the range of
-- `Int` correctly.
safeSymShiftLConcreteNum ::
  (MonadError ArithException m, TryMerge m, Integral a, FiniteBits a, Mergeable a) =>
  Bool ->
  a ->
  a ->
  m a
safeSymShiftLConcreteNum _ _ s | s < 0 = mrgThrowError Overflow
safeSymShiftLConcreteNum allowLargeShiftAmount a s
  | (fromIntegral s :: Integer) >= fromIntegral (finiteBitSize a) =
      if allowLargeShiftAmount then mrgReturn 0 else mrgThrowError Overflow
safeSymShiftLConcreteNum _ a s = mrgReturn $ shiftL a (fromIntegral s)

-- | This function handles the case when the shift amount is out the range of
-- `Int` correctly.
safeSymShiftRConcreteNum ::
  ( MonadError ArithException m,
    TryMerge m,
    Integral a,
    FiniteBits a,
    Mergeable a
  ) =>
  Bool ->
  a ->
  a ->
  m a
safeSymShiftRConcreteNum _ _ s | s < 0 = mrgThrowError Overflow
safeSymShiftRConcreteNum allowLargeShiftAmount a s
  | (fromIntegral s :: Integer) >= fromIntegral (finiteBitSize a) =
      if allowLargeShiftAmount then mrgReturn 0 else mrgThrowError Overflow
safeSymShiftRConcreteNum _ a s = mrgReturn $ shiftR a (fromIntegral s)

#define SAFE_SYM_SHIFT_CONCRETE(T) \
  instance (MonadError ArithException m, TryMerge m) => \
    SafeSymShift ArithException T m where \
    safeSymShiftL = safeSymShiftLConcreteNum True; \
    safeSymShiftR = safeSymShiftRConcreteNum True; \
    safeSymStrictShiftL = safeSymShiftLConcreteNum False; \
    safeSymStrictShiftR = safeSymShiftRConcreteNum False

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

instance
  (MonadError ArithException m, TryMerge m, KnownNat n, 1 <= n) =>
  SafeSymShift ArithException (WordN n) m
  where
  safeSymShiftL = safeSymShiftLConcreteNum True
  safeSymShiftR = safeSymShiftRConcreteNum True
  safeSymStrictShiftL = safeSymShiftLConcreteNum False
  safeSymStrictShiftR = safeSymShiftRConcreteNum False

instance
  (MonadError ArithException m, TryMerge m, KnownNat n, 1 <= n) =>
  SafeSymShift ArithException (IntN n) m
  where
  safeSymShiftL = safeSymShiftLConcreteNum True
  safeSymShiftR = safeSymShiftRConcreteNum True
  safeSymStrictShiftL = safeSymShiftLConcreteNum False
  safeSymStrictShiftR = safeSymShiftRConcreteNum False

instance
  (MonadError ArithException m, MonadUnion m, KnownNat n, 1 <= n) =>
  SafeSymShift ArithException (SymWordN n) m
  where
  safeSymShiftL (SymWordN a) (SymWordN s) =
    return $ SymWordN $ pevalShiftLeftTerm a s
  safeSymShiftR (SymWordN a) (SymWordN s) =
    return $ SymWordN $ pevalShiftRightTerm a s
  safeSymStrictShiftL a@(SymWordN ta) s@(SymWordN ts) =
    mrgIf
      (s .>= fromIntegral (finiteBitSize a))
      (mrgThrowError Overflow)
      (return $ SymWordN $ pevalShiftLeftTerm ta ts)
  safeSymStrictShiftR a@(SymWordN ta) s@(SymWordN ts) =
    mrgIf
      (s .>= fromIntegral (finiteBitSize a))
      (mrgThrowError Overflow)
      (return $ SymWordN $ pevalShiftRightTerm ta ts)

instance
  (MonadError ArithException m, MonadUnion m, KnownNat n, 1 <= n) =>
  SafeSymShift ArithException (SymIntN n) m
  where
  safeSymShiftL (SymIntN a) ss@(SymIntN s) =
    mrgIf
      (ss .< 0)
      (mrgThrowError Overflow)
      (return $ SymIntN $ pevalShiftLeftTerm a s)
  safeSymShiftR (SymIntN a) ss@(SymIntN s) =
    mrgIf
      (ss .< 0)
      (mrgThrowError Overflow)
      (return $ SymIntN $ pevalShiftRightTerm a s)
  safeSymStrictShiftL a@(SymIntN ta) s@(SymIntN ts) =
    mrgIf
      (s .< 0 .|| (bs .>= 0 .&& s .>= bs))
      (mrgThrowError Overflow)
      (return $ SymIntN $ pevalShiftLeftTerm ta ts)
    where
      bs = fromIntegral (finiteBitSize a)
  safeSymStrictShiftR a@(SymIntN ta) s@(SymIntN ts) =
    mrgIf
      (s .< 0 .|| (bs .>= 0 .&& s .>= bs))
      (mrgThrowError Overflow)
      (return $ SymIntN $ pevalShiftRightTerm ta ts)
    where
      bs = fromIntegral (finiteBitSize a)
