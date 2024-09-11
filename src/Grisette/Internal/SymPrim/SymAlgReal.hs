{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.SymAlgReal
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.SymAlgReal (SymAlgReal (SymAlgReal)) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable (hashWithSalt))
import Data.Serialize (Serialize (get, put))
import Data.String (IsString (fromString))
import GHC.Generics (Generic)
import Grisette.Internal.Core.Data.Class.Function (Apply (FunType, apply))
import Grisette.Internal.Core.Data.Class.Solvable
  ( Solvable (con, conView, ssym, sym),
  )
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.AllSyms (AllSyms (allSymsS), SomeSym (SomeSym))
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( FloatingUnaryOp
      ( FloatingAcos,
        FloatingAsin,
        FloatingAtan,
        FloatingCos,
        FloatingCosh,
        FloatingExp,
        FloatingLog,
        FloatingSin,
        FloatingSinh,
        FloatingSqrt,
        FloatingTan,
        FloatingTanh
      ),
    PEvalFloatingTerm (pevalFloatingUnaryTerm, pevalPowerTerm),
    PEvalNumTerm
      ( pevalAbsNumTerm,
        pevalMulNumTerm,
        pevalNegNumTerm,
        pevalSignumNumTerm
      ),
    pevalSubNumTerm,
    typedConstantSymbol,
  )
import Grisette.Internal.SymPrim.Prim.Term
  ( ConRep (ConType),
    LinkedRep (underlyingTerm, wrapTerm),
    PEvalNumTerm (pevalAddNumTerm),
    SymRep (SymType),
    Term (ConTerm),
    conTerm,
    pformatTerm,
    symTerm,
  )
import Language.Haskell.TH.Syntax (Lift)

-- | Symbolic representation of algebraic real numbers.
newtype SymAlgReal = SymAlgReal {underlyingAlgRealTerm :: Term AlgReal}
  deriving (Lift, Generic)
  deriving anyclass (NFData)

instance ConRep SymAlgReal where
  type ConType SymAlgReal = AlgReal

instance SymRep AlgReal where
  type SymType AlgReal = SymAlgReal

instance LinkedRep AlgReal SymAlgReal where
  underlyingTerm = underlyingAlgRealTerm
  wrapTerm = SymAlgReal

instance Apply SymAlgReal where
  type FunType SymAlgReal = SymAlgReal
  apply = id

instance Eq SymAlgReal where
  SymAlgReal a == SymAlgReal b = a == b

instance Hashable SymAlgReal where
  hashWithSalt s (SymAlgReal a) = hashWithSalt s a

instance IsString SymAlgReal where
  fromString = ssym . fromString

instance Solvable AlgReal SymAlgReal where
  con = SymAlgReal . conTerm
  sym = SymAlgReal . symTerm . typedConstantSymbol
  conView (SymAlgReal (ConTerm _ _ _ _ t)) = Just t
  conView _ = Nothing

instance Show SymAlgReal where
  show (SymAlgReal t) = pformatTerm t

instance AllSyms SymAlgReal where
  allSymsS v = (SomeSym v :)

instance Num SymAlgReal where
  (SymAlgReal l) + (SymAlgReal r) = SymAlgReal $ pevalAddNumTerm l r
  (SymAlgReal l) - (SymAlgReal r) = SymAlgReal $ pevalSubNumTerm l r
  (SymAlgReal l) * (SymAlgReal r) = SymAlgReal $ pevalMulNumTerm l r
  negate (SymAlgReal v) = SymAlgReal $ pevalNegNumTerm v
  abs (SymAlgReal v) = SymAlgReal $ pevalAbsNumTerm v
  signum (SymAlgReal v) = SymAlgReal $ pevalSignumNumTerm v
  fromInteger = con . fromInteger

instance Fractional SymAlgReal where
  fromRational = con . fromRational
  (/) = error "consider using safeFdiv instead of (/) for SymAlgReal"
  recip = error "consider using safeRecip instead of recip for SymAlgReal"

instance Floating SymAlgReal where
  pi = fromRational $ toRational pi
  exp (SymAlgReal v) = SymAlgReal $ pevalFloatingUnaryTerm FloatingExp v
  log (SymAlgReal v) = SymAlgReal $ pevalFloatingUnaryTerm FloatingLog v
  sqrt (SymAlgReal v) = SymAlgReal $ pevalFloatingUnaryTerm FloatingSqrt v
  sin (SymAlgReal v) = SymAlgReal $ pevalFloatingUnaryTerm FloatingSin v
  cos (SymAlgReal v) = SymAlgReal $ pevalFloatingUnaryTerm FloatingCos v
  tan (SymAlgReal v) = SymAlgReal $ pevalFloatingUnaryTerm FloatingTan v
  sinh (SymAlgReal v) = SymAlgReal $ pevalFloatingUnaryTerm FloatingSinh v
  cosh (SymAlgReal v) = SymAlgReal $ pevalFloatingUnaryTerm FloatingCosh v
  tanh (SymAlgReal v) = SymAlgReal $ pevalFloatingUnaryTerm FloatingTanh v
  asin (SymAlgReal v) = SymAlgReal $ pevalFloatingUnaryTerm FloatingAsin v
  acos (SymAlgReal v) = SymAlgReal $ pevalFloatingUnaryTerm FloatingAcos v
  atan (SymAlgReal v) = SymAlgReal $ pevalFloatingUnaryTerm FloatingAtan v
  asinh = error "asinh isn't supported by the underlying sbv library"
  acosh = error "acosh isn't supported by the underlying sbv library"
  atanh = error "atanh isn't supported by the underlying sbv library"
  SymAlgReal l ** SymAlgReal r = SymAlgReal $ pevalPowerTerm l r
  logBase = error "consider using safeLogBase instead of logBase for AlgReal"

instance Serialize SymAlgReal where
  put = put . underlyingAlgRealTerm
  get = SymAlgReal <$> get
