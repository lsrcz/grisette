{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Core.Data.Class.SafeSymRotate (SafeSymRotate (..)) where

import Control.Exception (ArithException (Overflow))
import Control.Monad.Error.Class (MonadError)
import Data.Bits (Bits (rotateL, rotateR), FiniteBits (finiteBitSize))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeLits (KnownNat, type (<=))
import Grisette.Core.Control.Monad.Union (MonadUnion)
import Grisette.Core.Data.BV (IntN, WordN)
import Grisette.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Core.Data.Class.SOrd (SOrd ((<~)))
import Grisette.Core.Data.Class.SimpleMergeable (UnionLike, mrgIf)
import Grisette.Core.Data.Class.SymRotate (SymRotate)
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bits
  ( pevalRotateLeftTerm,
    pevalRotateRightTerm,
  )
import Grisette.IR.SymPrim.Data.SymPrim
  ( SymIntN (SymIntN),
    SymWordN (SymWordN),
  )
import Grisette.Lib.Control.Monad (mrgReturn)
import Grisette.Lib.Control.Monad.Except (mrgThrowError)

class (SymRotate a) => SafeSymRotate e a | a -> e where
  safeSymRotateL :: (MonadError e m, UnionLike m) => a -> a -> m a
  safeSymRotateL = safeSymRotateL' id
  safeSymRotateR :: (MonadError e m, UnionLike m) => a -> a -> m a
  safeSymRotateR = safeSymRotateR' id
  safeSymRotateL' ::
    (MonadError e' m, UnionLike m) => (e -> e') -> a -> a -> m a
  safeSymRotateR' ::
    (MonadError e' m, UnionLike m) => (e -> e') -> a -> a -> m a
  {-# MINIMAL safeSymRotateL', safeSymRotateR' #-}

-- | This function handles the case when the shift amount is out the range of
-- `Int` correctly.
safeSymRotateLConcreteNum ::
  (MonadError e m, MonadUnion m, Integral a, FiniteBits a, Mergeable a) =>
  e ->
  a ->
  a ->
  m a
safeSymRotateLConcreteNum e _ s | s < 0 = mrgThrowError e
safeSymRotateLConcreteNum _ a s =
  mrgReturn $ rotateL a (fromIntegral $ s `rem` fromIntegral (finiteBitSize s))

-- | This function handles the case when the shift amount is out the range of
-- `Int` correctly.
safeSymRotateRConcreteNum ::
  (MonadError e m, MonadUnion m, Integral a, FiniteBits a, Mergeable a) =>
  e ->
  a ->
  a ->
  m a
safeSymRotateRConcreteNum e _ s | s < 0 = mrgThrowError e
safeSymRotateRConcreteNum _ a s =
  mrgReturn $ rotateR a (fromIntegral $ s `rem` fromIntegral (finiteBitSize s))

#define SAFE_SYM_ROTATE_CONCRETE(T) \
  instance SafeSymRotate ArithException T where \
    safeSymRotateL' f = safeSymRotateLConcreteNum (f Overflow); \
    safeSymRotateR' f = safeSymRotateRConcreteNum (f Overflow) \

#if 1
SAFE_SYM_ROTATE_CONCRETE(Word8)
SAFE_SYM_ROTATE_CONCRETE(Word16)
SAFE_SYM_ROTATE_CONCRETE(Word32)
SAFE_SYM_ROTATE_CONCRETE(Word64)
SAFE_SYM_ROTATE_CONCRETE(Word)
SAFE_SYM_ROTATE_CONCRETE(Int8)
SAFE_SYM_ROTATE_CONCRETE(Int16)
SAFE_SYM_ROTATE_CONCRETE(Int32)
SAFE_SYM_ROTATE_CONCRETE(Int64)
SAFE_SYM_ROTATE_CONCRETE(Int)
#endif

instance (KnownNat n, 1 <= n) => SafeSymRotate ArithException (WordN n) where
  safeSymRotateL' f = safeSymRotateLConcreteNum (f Overflow)
  safeSymRotateR' f = safeSymRotateRConcreteNum (f Overflow)

instance (KnownNat n, 1 <= n) => SafeSymRotate ArithException (IntN n) where
  safeSymRotateL' f = safeSymRotateLConcreteNum (f Overflow)
  safeSymRotateR' f = safeSymRotateRConcreteNum (f Overflow)

instance (KnownNat n, 1 <= n) => SafeSymRotate ArithException (SymWordN n) where
  safeSymRotateL' _ (SymWordN ta) (SymWordN tr) =
    mrgReturn $ SymWordN $ pevalRotateLeftTerm ta tr
  safeSymRotateR' _ (SymWordN ta) (SymWordN tr) =
    mrgReturn $ SymWordN $ pevalRotateRightTerm ta tr

instance (KnownNat n, 1 <= n) => SafeSymRotate ArithException (SymIntN n) where
  safeSymRotateL' f (SymIntN ta) r@(SymIntN tr) =
    mrgIf
      (r <~ 0)
      (mrgThrowError $ f Overflow)
      (mrgReturn $ SymIntN $ pevalRotateLeftTerm ta tr)
  safeSymRotateR' f (SymIntN ta) r@(SymIntN tr) =
    mrgIf
      (r <~ 0)
      (mrgThrowError $ f Overflow)
      (mrgReturn $ SymIntN $ pevalRotateRightTerm ta tr)
