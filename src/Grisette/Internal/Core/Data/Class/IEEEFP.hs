{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Internal.Core.Data.Class.IEEEFP
  ( IEEEFPTraits (..),
    SymIEEEFPTraits (..),
    IEEEConstants (..),
  )
where

import Data.SBV (infinity, nan)
import Grisette.Internal.Core.Data.Class.Solvable (Solvable (con))
import Grisette.Internal.SymPrim.FP (FP (FP), ValidFP)
import Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalFP
  ( pevalFPTraitTerm,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Term (FPTrait (FPIsNaN))
import Grisette.Internal.SymPrim.SymBool (SymBool (SymBool))
import Grisette.Internal.SymPrim.SymFP (SymFP (SymFP))

-- | A class for traits of IEEE floating-point numbers.
class IEEEFPTraits a where
  -- | Check if a floating-point number is not-a-number.
  fpIsNaN :: a -> Bool

-- | A class for symbolic traits of IEEE floating-point numbers.
class SymIEEEFPTraits a where
  -- | Check if a symbolic floating-point number is not-a-number.
  symFpIsNaN :: a -> SymBool

-- | Constants for IEEE floating-point numbers.
class IEEEConstants a where
  -- | Positive infinity.
  fpPosInf :: a

  -- | Negative infinity.
  fpNegInf :: a

  -- | Not-a-number.
  fpNaN :: a

  -- | Negative zero.
  fpNegZero :: a

  -- | Positive zero.
  fpPosZero :: a

instance (ValidFP eb sb) => IEEEFPTraits (FP eb sb) where
  fpIsNaN (FP x) = isNaN x
  {-# INLINE fpIsNaN #-}

instance (ValidFP eb sb) => SymIEEEFPTraits (FP eb sb) where
  symFpIsNaN x = con $ fpIsNaN x
  {-# INLINE symFpIsNaN #-}

instance (ValidFP eb sb) => IEEEConstants (FP eb sb) where
  fpPosInf = FP infinity
  {-# INLINE fpPosInf #-}
  fpNegInf = FP $ -infinity
  {-# INLINE fpNegInf #-}
  fpNaN = FP nan
  {-# INLINE fpNaN #-}
  fpNegZero = FP $ -0
  {-# INLINE fpNegZero #-}
  fpPosZero = FP 0
  {-# INLINE fpPosZero #-}

instance (ValidFP eb sb) => SymIEEEFPTraits (SymFP eb sb) where
  symFpIsNaN (SymFP x) = SymBool $ pevalFPTraitTerm FPIsNaN x
  {-# INLINE symFpIsNaN #-}

instance (ValidFP eb sb) => IEEEConstants (SymFP eb sb) where
  fpPosInf = con fpPosInf
  fpNegInf = con fpNegInf
  fpNaN = con fpNaN
  fpNegZero = con fpNegZero
  fpPosZero = con fpPosZero
  {-# INLINE fpPosInf #-}
  {-# INLINE fpNegInf #-}
  {-# INLINE fpNaN #-}
  {-# INLINE fpNegZero #-}
  {-# INLINE fpPosZero #-}
