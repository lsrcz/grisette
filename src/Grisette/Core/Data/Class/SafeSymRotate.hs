{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Core.Data.Class.SafeSymRotate
-- Copyright   :   (c) Sirui Lu 2023-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
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
import Grisette.Core.Data.Class.SOrd (SOrd ((.<)))
import Grisette.Core.Data.Class.SimpleMergeable (mrgIf)
import Grisette.Core.Data.Class.TryMerge (TryMerge)
import Grisette.Lib.Control.Monad (mrgReturn)
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Grisette.SymPrim.Prim.Term
  ( PEvalRotateTerm
      ( pevalRotateLeftTerm,
        pevalRotateRightTerm
      ),
  )
import Grisette.SymPrim.SymBV
  ( SymIntN (SymIntN),
    SymWordN (SymWordN),
  )

-- | Safe rotation operations. The operators will reject negative shift amounts.
class (MonadError e m, TryMerge m, Mergeable a) => SafeSymRotate e a m where
  safeSymRotateL :: a -> a -> m a
  safeSymRotateR :: a -> a -> m a

-- | This function handles the case when the shift amount is out the range of
-- `Int` correctly.
safeSymRotateLConcreteNum ::
  ( MonadError ArithException m,
    TryMerge m,
    Integral a,
    FiniteBits a,
    Mergeable a
  ) =>
  a ->
  a ->
  m a
safeSymRotateLConcreteNum _ s | s < 0 = mrgThrowError Overflow
safeSymRotateLConcreteNum a s =
  mrgReturn $ rotateL a (fromIntegral $ s `rem` fromIntegral (finiteBitSize s))

-- | This function handles the case when the shift amount is out the range of
-- `Int` correctly.
safeSymRotateRConcreteNum ::
  ( MonadError ArithException m,
    TryMerge m,
    Integral a,
    FiniteBits a,
    Mergeable a
  ) =>
  a ->
  a ->
  m a
safeSymRotateRConcreteNum _ s | s < 0 = mrgThrowError Overflow
safeSymRotateRConcreteNum a s =
  mrgReturn $ rotateR a (fromIntegral $ s `rem` fromIntegral (finiteBitSize s))

#define SAFE_SYM_ROTATE_CONCRETE(T) \
  instance (MonadError ArithException m, TryMerge m) => \
    SafeSymRotate ArithException T m where \
    safeSymRotateL = safeSymRotateLConcreteNum; \
    safeSymRotateR = safeSymRotateRConcreteNum \

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

instance
  (MonadError ArithException m, TryMerge m, KnownNat n, 1 <= n) =>
  SafeSymRotate ArithException (WordN n) m
  where
  safeSymRotateL = safeSymRotateLConcreteNum
  safeSymRotateR = safeSymRotateRConcreteNum

instance
  (MonadError ArithException m, TryMerge m, KnownNat n, 1 <= n) =>
  SafeSymRotate ArithException (IntN n) m
  where
  safeSymRotateL = safeSymRotateLConcreteNum
  safeSymRotateR = safeSymRotateRConcreteNum

instance
  (MonadError ArithException m, TryMerge m, KnownNat n, 1 <= n) =>
  SafeSymRotate ArithException (SymWordN n) m
  where
  safeSymRotateL (SymWordN ta) (SymWordN tr) =
    mrgReturn $ SymWordN $ pevalRotateLeftTerm ta tr
  safeSymRotateR (SymWordN ta) (SymWordN tr) =
    mrgReturn $ SymWordN $ pevalRotateRightTerm ta tr

instance
  (MonadError ArithException m, MonadUnion m, KnownNat n, 1 <= n) =>
  SafeSymRotate ArithException (SymIntN n) m
  where
  safeSymRotateL (SymIntN ta) r@(SymIntN tr) =
    mrgIf
      (r .< 0)
      (mrgThrowError Overflow)
      (mrgReturn $ SymIntN $ pevalRotateLeftTerm ta tr)
  safeSymRotateR (SymIntN ta) r@(SymIntN tr) =
    mrgIf
      (r .< 0)
      (mrgThrowError Overflow)
      (mrgReturn $ SymIntN $ pevalRotateRightTerm ta tr)
