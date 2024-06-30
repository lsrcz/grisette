{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
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
    SymIEEEFPTraits (..),
    IEEEConstants (..),
    IEEEFPOp (..),
    IEEEFPRoundingOp (..),
  )
where

import Data.SBV (infinity, nan)
import Grisette.Internal.Core.Data.Class.Solvable (Solvable (con))
import Grisette.Internal.SymPrim.FP (FP (FP), ValidFP)
import Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalFP
  ( pevalFPBinaryTerm,
    pevalFPFMATerm,
    pevalFPRoundingBinaryTerm,
    pevalFPRoundingUnaryTerm,
    pevalFPTraitTerm,
    pevalFPUnaryTerm,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( FPBinaryOp (FPMax, FPMin, FPRem),
    FPRoundingBinaryOp (FPAdd, FPDiv, FPMul, FPSub),
    FPRoundingUnaryOp (FPRoundToIntegral, FPSqrt),
    FPTrait
      ( FPIsInfinite,
        FPIsNaN,
        FPIsNegative,
        FPIsNegativeInfinite,
        FPIsNegativeZero,
        FPIsNormal,
        FPIsPoint,
        FPIsPositive,
        FPIsPositiveInfinite,
        FPIsPositiveZero,
        FPIsSubnormal,
        FPIsZero
      ),
    FPUnaryOp (FPAbs, FPNeg),
  )
import Grisette.Internal.SymPrim.SymBool (SymBool (SymBool))
import Grisette.Internal.SymPrim.SymFP (SymFP (SymFP), SymFPRoundingMode (SymFPRoundingMode))

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
fpIsNegative = isNegativeZero
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

-- | A class for symbolic traits of IEEE floating-point numbers.
class SymIEEEFPTraits a where
  -- | Check if a symbolic floating-point number is not-a-number.
  symFpIsNaN :: a -> SymBool

  -- | Check if a symbolic floating-point number is positive.
  -- +0, +inf are considered positive. nan, -0, -inf are not positive.
  symFpIsPositive :: a -> SymBool

  -- | Check if a symbolic floating-point number is negative.
  -- -0, -inf are considered negative. nan, +0, +inf are not negative.
  symFpIsNegative :: a -> SymBool

  -- | Check if a symbolic floating-point number is positive infinite.
  symFpIsPositiveInfinite :: a -> SymBool

  -- | Check if a symbolic floating-point number is negative infinite.
  symFpIsNegativeInfinite :: a -> SymBool

  -- | Check if a symbolic floating-point number is infinite.
  symFpIsInfinite :: a -> SymBool

  -- | Check if a symbolic floating-point number is positive zero.
  symFpIsPositiveZero :: a -> SymBool

  -- | Check if a symbolic floating-point number is negative zero.
  symFpIsNegativeZero :: a -> SymBool

  -- | Check if a symbolic floating-point number is zero.
  symFpIsZero :: a -> SymBool

  -- | Check if a symbolic floating-point number is normal, i.e., not 0, not
  -- inf, not nan, and not denormalized.
  symFpIsNormal :: a -> SymBool

  -- | Check if a symbolic floating-point number is subnormal, i.e.,
  -- denormalized. 0, inf, or nan are not subnormal.
  symFpIsSubnormal :: a -> SymBool

  -- | Check if a symbolic floating-point number is a point, i.e., not inf, not
  -- nan.
  symFpIsPoint :: a -> SymBool

newtype ConcreteFloat f = ConcreteFloat f

instance (RealFloat f) => SymIEEEFPTraits (ConcreteFloat f) where
  symFpIsNaN (ConcreteFloat x) = con $ fpIsNaN x
  {-# INLINE symFpIsNaN #-}

  symFpIsPositive (ConcreteFloat x) = con $ fpIsPositive x
  {-# INLINE symFpIsPositive #-}

  symFpIsNegative (ConcreteFloat x) = con $ fpIsNegative x
  {-# INLINE symFpIsNegative #-}

  symFpIsInfinite (ConcreteFloat x) = con $ fpIsInfinite x
  {-# INLINE symFpIsInfinite #-}

  symFpIsPositiveInfinite (ConcreteFloat x) = con $ fpIsPositiveInfinite x
  {-# INLINE symFpIsPositiveInfinite #-}

  symFpIsNegativeInfinite (ConcreteFloat x) = con $ fpIsNegativeInfinite x
  {-# INLINE symFpIsNegativeInfinite #-}

  symFpIsPositiveZero (ConcreteFloat x) = con $ fpIsPositiveZero x
  {-# INLINE symFpIsPositiveZero #-}

  symFpIsNegativeZero (ConcreteFloat x) = con $ fpIsNegativeZero x
  {-# INLINE symFpIsNegativeZero #-}

  symFpIsZero (ConcreteFloat x) = con $ fpIsZero x
  {-# INLINE symFpIsZero #-}

  symFpIsNormal (ConcreteFloat x) = con $ fpIsNormal x
  {-# INLINE symFpIsNormal #-}

  symFpIsSubnormal (ConcreteFloat x) = con $ fpIsSubnormal x
  {-# INLINE symFpIsSubnormal #-}

  symFpIsPoint (ConcreteFloat x) = con $ fpIsPoint x
  {-# INLINE symFpIsPoint #-}

deriving via (ConcreteFloat Float) instance SymIEEEFPTraits Float

deriving via (ConcreteFloat Double) instance SymIEEEFPTraits Double

deriving via
  (ConcreteFloat (FP eb sb))
  instance
    (ValidFP eb sb) => SymIEEEFPTraits (FP eb sb)

instance (ValidFP eb sb) => SymIEEEFPTraits (SymFP eb sb) where
  symFpIsNaN (SymFP x) = SymBool $ pevalFPTraitTerm FPIsNaN x
  {-# INLINE symFpIsNaN #-}
  symFpIsPositive (SymFP x) = SymBool $ pevalFPTraitTerm FPIsPositive x
  {-# INLINE symFpIsPositive #-}
  symFpIsNegative (SymFP x) = SymBool $ pevalFPTraitTerm FPIsNegative x
  {-# INLINE symFpIsNegative #-}
  symFpIsInfinite (SymFP x) = SymBool $ pevalFPTraitTerm FPIsInfinite x
  {-# INLINE symFpIsInfinite #-}
  symFpIsPositiveInfinite (SymFP x) =
    SymBool $ pevalFPTraitTerm FPIsPositiveInfinite x
  {-# INLINE symFpIsPositiveInfinite #-}
  symFpIsNegativeInfinite (SymFP x) =
    SymBool $ pevalFPTraitTerm FPIsNegativeInfinite x
  {-# INLINE symFpIsNegativeInfinite #-}
  symFpIsPositiveZero (SymFP x) = SymBool $ pevalFPTraitTerm FPIsPositiveZero x
  {-# INLINE symFpIsPositiveZero #-}
  symFpIsNegativeZero (SymFP x) = SymBool $ pevalFPTraitTerm FPIsNegativeZero x
  {-# INLINE symFpIsNegativeZero #-}
  symFpIsZero (SymFP x) = SymBool $ pevalFPTraitTerm FPIsZero x
  {-# INLINE symFpIsZero #-}
  symFpIsNormal (SymFP x) = SymBool $ pevalFPTraitTerm FPIsNormal x
  {-# INLINE symFpIsNormal #-}
  symFpIsSubnormal (SymFP x) = SymBool $ pevalFPTraitTerm FPIsSubnormal x
  {-# INLINE symFpIsSubnormal #-}
  symFpIsPoint (SymFP x) = SymBool $ pevalFPTraitTerm FPIsPoint x
  {-# INLINE symFpIsPoint #-}

-- | Constants for IEEE floating-point numbers.
class IEEEConstants a where
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

instance (ValidFP eb sb) => IEEEConstants (FP eb sb) where
  fpPositiveInfinite = FP infinity
  {-# INLINE fpPositiveInfinite #-}
  fpNegativeInfinite = FP $ -infinity
  {-# INLINE fpNegativeInfinite #-}
  fpNaN = FP nan
  {-# INLINE fpNaN #-}
  fpNegativeZero = FP $ -0
  {-# INLINE fpNegativeZero #-}
  fpPositiveZero = FP 0
  {-# INLINE fpPositiveZero #-}

instance (ValidFP eb sb) => IEEEConstants (SymFP eb sb) where
  fpPositiveInfinite = con fpPositiveInfinite
  {-# INLINE fpPositiveInfinite #-}
  fpNegativeInfinite = con fpNegativeInfinite
  {-# INLINE fpNegativeInfinite #-}
  fpNaN = con fpNaN
  {-# INLINE fpNaN #-}
  fpNegativeZero = con fpNegativeZero
  {-# INLINE fpNegativeZero #-}
  fpPositiveZero = con fpPositiveZero
  {-# INLINE fpPositiveZero #-}

-- | Operations on IEEE floating-point numbers, without rounding mode.
class IEEEFPOp a where
  symFpAbs :: a -> a
  symFpNeg :: a -> a
  symFpRem :: a -> a -> a
  symFpMin :: a -> a -> a
  symFpMax :: a -> a -> a

instance (ValidFP eb sb) => IEEEFPOp (SymFP eb sb) where
  symFpAbs (SymFP l) = SymFP $ pevalFPUnaryTerm FPAbs l
  {-# INLINE symFpAbs #-}
  symFpNeg (SymFP l) = SymFP $ pevalFPUnaryTerm FPNeg l
  {-# INLINE symFpNeg #-}
  symFpRem (SymFP l) (SymFP r) = SymFP $ pevalFPBinaryTerm FPRem l r
  {-# INLINE symFpRem #-}
  symFpMin (SymFP l) (SymFP r) = SymFP $ pevalFPBinaryTerm FPMin l r
  {-# INLINE symFpMin #-}
  symFpMax (SymFP l) (SymFP r) = SymFP $ pevalFPBinaryTerm FPMax l r
  {-# INLINE symFpMax #-}

-- | Operations on IEEE floating-point numbers, with rounding mode.
class IEEEFPRoundingOp a mode where
  symFpAdd :: mode -> a -> a -> a
  symFpSub :: mode -> a -> a -> a
  symFpMul :: mode -> a -> a -> a
  symFpDiv :: mode -> a -> a -> a
  symFpFMA :: mode -> a -> a -> a -> a
  symFpSqrt :: mode -> a -> a
  symFpRoundToIntegral :: mode -> a -> a

instance (ValidFP eb sb) => IEEEFPRoundingOp (SymFP eb sb) SymFPRoundingMode where
  symFpAdd (SymFPRoundingMode mode) (SymFP l) (SymFP r) =
    SymFP $ pevalFPRoundingBinaryTerm FPAdd mode l r
  {-# INLINE symFpAdd #-}
  symFpSub (SymFPRoundingMode mode) (SymFP l) (SymFP r) =
    SymFP $ pevalFPRoundingBinaryTerm FPSub mode l r
  {-# INLINE symFpSub #-}
  symFpMul (SymFPRoundingMode mode) (SymFP l) (SymFP r) =
    SymFP $ pevalFPRoundingBinaryTerm FPMul mode l r
  {-# INLINE symFpMul #-}
  symFpDiv (SymFPRoundingMode mode) (SymFP l) (SymFP r) =
    SymFP $ pevalFPRoundingBinaryTerm FPDiv mode l r
  {-# INLINE symFpDiv #-}
  symFpFMA (SymFPRoundingMode mode) (SymFP l) (SymFP m) (SymFP r) =
    SymFP $ pevalFPFMATerm mode l m r
  {-# INLINE symFpFMA #-}
  symFpSqrt (SymFPRoundingMode mode) (SymFP v) =
    SymFP $ pevalFPRoundingUnaryTerm FPSqrt mode v
  {-# INLINE symFpSqrt #-}
  symFpRoundToIntegral (SymFPRoundingMode mode) (SymFP v) =
    SymFP $ pevalFPRoundingUnaryTerm FPRoundToIntegral mode v
  {-# INLINE symFpRoundToIntegral #-}
