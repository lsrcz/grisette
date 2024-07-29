{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.IEEEFP
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.IEEEFP
  ( fpIsNaN,
    fpIsPositiveZero,
    fpIsNegativeZero,
    fpIsPositiveInfinite,
    fpIsNegativeInfinite,
    fpIsPositive,
    fpIsNegative,
    fpIsInfinite,
    fpIsZero,
    fpIsNormal,
    fpIsSubnormal,
    fpIsPoint,
    IEEEFPConstants (..),
    IEEEFPRoundingMode (..),
    IEEEFPOp (..),
    IEEEFPRoundingOp (..),
    IEEEFPConvertible (..),
  )
where

-- | Check if a floating-point number is not-a-number.
fpIsNaN :: (RealFloat a) => a -> Bool
fpIsNaN = isNaN
{-# INLINE fpIsNaN #-}

-- | Check if a floating-point number is positive zero.
fpIsPositiveZero :: (RealFloat a) => a -> Bool
fpIsPositiveZero x = x == 0 && not (fpIsNegativeZero x)
{-# INLINE fpIsPositiveZero #-}

-- | Check if a floating-point number is negative zero.
fpIsNegativeZero :: (RealFloat a) => a -> Bool
fpIsNegativeZero = isNegativeZero
{-# INLINE fpIsNegativeZero #-}

-- | Check if a floating-point number is positive infinite.
fpIsPositiveInfinite :: (RealFloat a) => a -> Bool
fpIsPositiveInfinite x = isInfinite x && x > 0
{-# INLINE fpIsPositiveInfinite #-}

-- | Check if a floating-point number is negative infinite.
fpIsNegativeInfinite :: (RealFloat a) => a -> Bool
fpIsNegativeInfinite x = isInfinite x && x < 0
{-# INLINE fpIsNegativeInfinite #-}

-- | Check if a floating-point number is positive.
-- +0, +inf are considered positive. nan, -0, -inf are not positive.
fpIsPositive :: (RealFloat a) => a -> Bool
fpIsPositive x = not (fpIsNaN x) && (x > 0 || fpIsPositiveZero x)
{-# INLINE fpIsPositive #-}

-- | Check if a floating-point number is negative.
-- -0, -inf are considered negative. nan, +0, +inf are not negative.
fpIsNegative :: (RealFloat a) => a -> Bool
fpIsNegative x = not (fpIsNaN x) && (x < 0 || isNegativeZero x)
{-# INLINE fpIsNegative #-}

-- | Check if a floating-point number is infinite.
fpIsInfinite :: (RealFloat a) => a -> Bool
fpIsInfinite x = fpIsPositiveInfinite x || fpIsNegativeInfinite x
{-# INLINE fpIsInfinite #-}

-- | Check if a floating-point number is zero.
fpIsZero :: (RealFloat a) => a -> Bool
fpIsZero x = fpIsPositiveZero x || fpIsNegativeZero x
{-# INLINE fpIsZero #-}

-- | Check if a floating-point number is normal, i.e., not 0, not inf, not
-- nan, and not denormalized.
fpIsNormal :: (RealFloat a) => a -> Bool
fpIsNormal x =
  not (fpIsZero x)
    && not (fpIsSubnormal x)
    && not (fpIsInfinite x)
    && not (fpIsNaN x)
{-# INLINE fpIsNormal #-}

-- | Check if a floating-point number is subnormal, i.e., denormalized. 0,
-- inf, or nan are not subnormal.
fpIsSubnormal :: (RealFloat a) => a -> Bool
fpIsSubnormal = isDenormalized
{-# INLINE fpIsSubnormal #-}

-- | Check if a floating-point number is a point, i.e., not inf, not nan.
fpIsPoint :: (RealFloat a) => a -> Bool
fpIsPoint x = not (fpIsInfinite x) && not (fpIsNaN x)
{-# INLINE fpIsPoint #-}

-- | Constants for IEEE floating-point numbers.
class IEEEFPConstants a where
  -- | Positive infinity.
  fpPositiveInfinite :: a

  -- | Negative infinity.
  fpNegativeInfinite :: a

  -- | Not-a-number.
  fpNaN :: a

  -- | Negative zero.
  fpNegativeZero :: a

  -- | Positive zero.
  fpPositiveZero :: a

-- | Operations on IEEE floating-point numbers, without rounding mode.
class IEEEFPOp a where
  -- | IEEE754-2019 abs operation.
  fpAbs :: a -> a

  -- | IEEE754-2019 negate operation.
  fpNeg :: a -> a

  -- | IEEE754-2019 remainder operation.
  fpRem :: a -> a -> a

  -- | IEEE754-2019 minimum operation.
  --
  -- * The comparison for zeros follows -0 < 0
  -- * Returns NaN if one operand is NaN.
  fpMinimum :: a -> a -> a

  -- | IEEE754-2019 minimumNumber operation.
  --
  -- * The comparison for zeros follows -0 < 0
  -- * Returns the other operand if one operand is NaN.
  fpMinimumNumber :: a -> a -> a

  -- | IEEE754-2019 maximum operation.
  --
  -- * The comparison for zeros follows -0 < 0
  -- * Returns NaN if one operand is NaN.
  fpMaximum :: a -> a -> a

  -- | IEEE754-2019 maximumNumber operation.
  --
  -- * The comparison for zeros follows -0 < 0
  -- * Returns the other operand if one operand is NaN.
  fpMaximumNumber :: a -> a -> a

-- | Rounding modes for floating-point operations.
class IEEEFPRoundingMode mode where
  -- | Round to nearest, ties to even.
  rne :: mode

  -- | Round to nearest, ties to away from zero.
  rna :: mode

  -- | Round towards positive infinity.
  rtp :: mode

  -- | Round towards negative infinity.
  rtn :: mode

  -- | Round towards zero.
  rtz :: mode

-- | Operations on IEEE floating-point numbers, with rounding mode.
class (IEEEFPRoundingMode mode) => IEEEFPRoundingOp a mode | a -> mode where
  fpAdd :: mode -> a -> a -> a
  fpSub :: mode -> a -> a -> a
  fpMul :: mode -> a -> a -> a
  fpDiv :: mode -> a -> a -> a
  fpFMA :: mode -> a -> a -> a -> a
  fpSqrt :: mode -> a -> a
  fpRoundToIntegral :: mode -> a -> a

class IEEEFPConvertible a fp mode | fp -> mode where
  fromFPOr :: a -> mode -> fp -> a
  toFP :: mode -> a -> fp
