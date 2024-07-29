{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.SymIEEEFP
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.SymIEEEFP
  ( SymIEEEFPTraits (..),
  )
where

import Grisette.Internal.Core.Data.Class.IEEEFP
  ( fpIsInfinite,
    fpIsNaN,
    fpIsNegative,
    fpIsNegativeInfinite,
    fpIsNegativeZero,
    fpIsNormal,
    fpIsPoint,
    fpIsPositive,
    fpIsPositiveInfinite,
    fpIsPositiveZero,
    fpIsSubnormal,
    fpIsZero,
  )
import Grisette.Internal.Core.Data.Class.Solvable (Solvable (con))
import Grisette.Internal.SymPrim.FP (FP, ValidFP)
import Grisette.Internal.SymPrim.SymBool (SymBool)

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
