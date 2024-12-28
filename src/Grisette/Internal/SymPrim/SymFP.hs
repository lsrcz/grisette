{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.SymFP
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.SymFP
  ( SymFP (SymFP),
    SymFP16,
    SymFP32,
    SymFP64,
    SymFPRoundingMode (SymFPRoundingMode),
  )
where

import Control.DeepSeq (NFData)
import qualified Data.Binary as Binary
import Data.Bytes.Serial (Serial (deserialize, serialize))
import Data.Hashable (Hashable (hashWithSalt))
import Data.Proxy (Proxy (Proxy))
import qualified Data.Serialize as Cereal
import Data.String (IsString (fromString))
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, type (+), type (<=))
import Grisette.Internal.Core.Data.Class.BitCast
  ( BitCast (bitCast),
    BitCastCanonical (bitCastCanonicalValue),
    BitCastOr (bitCastOr),
  )
import Grisette.Internal.Core.Data.Class.Function (Apply (FunType, apply))
import Grisette.Internal.Core.Data.Class.IEEEFP
  ( IEEEFPConstants
      ( fpMaxNormalized,
        fpMaxSubnormal,
        fpMinNormalized,
        fpMinSubnormal,
        fpNaN,
        fpNegativeInfinite,
        fpNegativeZero,
        fpPositiveInfinite,
        fpPositiveZero
      ),
    IEEEFPConvertible (fromFPOr, toFP),
    IEEEFPOp
      ( fpAbs,
        fpMaximum,
        fpMaximumNumber,
        fpMinimum,
        fpMinimumNumber,
        fpNeg,
        fpRem
      ),
    IEEEFPRoundingMode (rna, rne, rtn, rtp, rtz),
    IEEEFPRoundingOp
      ( fpAdd,
        fpDiv,
        fpFMA,
        fpMul,
        fpRoundToIntegral,
        fpSqrt,
        fpSub
      ),
    IEEEFPToAlgReal,
  )
import Grisette.Internal.Core.Data.Class.Solvable
  ( Solvable (con, conView, ssym, sym),
  )
import Grisette.Internal.Core.Data.Class.SymIEEEFP
  ( SymIEEEFPTraits
      ( symFpIsInfinite,
        symFpIsNaN,
        symFpIsNegative,
        symFpIsNegativeInfinite,
        symFpIsNegativeZero,
        symFpIsNormal,
        symFpIsPoint,
        symFpIsPositive,
        symFpIsPositiveInfinite,
        symFpIsPositiveZero,
        symFpIsSubnormal,
        symFpIsZero
      ),
  )
import Grisette.Internal.Internal.Decl.SymPrim.AllSyms
  ( AllSyms (allSymsS),
    SomeSym (SomeSym),
  )
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP
  ( FP,
    FPRoundingMode (RNA, RNE, RTN, RTP, RTZ),
    ValidFP,
    withValidFPProofs,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalFP
  ( pevalFPBinaryTerm,
    pevalFPFMATerm,
    pevalFPRoundingBinaryTerm,
    pevalFPRoundingUnaryTerm,
    pevalFPTraitTerm,
    pevalFPUnaryTerm,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( ConRep (ConType),
    FPBinaryOp (FPMaximum, FPMaximumNumber, FPMinimum, FPMinimumNumber, FPRem),
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
    FloatingUnaryOp (FloatingSqrt),
    LinkedRep (underlyingTerm, wrapTerm),
    PEvalBitCastOrTerm (pevalBitCastOrTerm),
    PEvalBitCastTerm (pevalBitCastTerm),
    PEvalFloatingTerm (pevalFloatingUnaryTerm),
    PEvalFractionalTerm (pevalFdivTerm, pevalRecipTerm),
    PEvalIEEEFPConvertibleTerm (pevalFromFPOrTerm, pevalToFPTerm),
    PEvalNumTerm
      ( pevalAbsNumTerm,
        pevalAddNumTerm,
        pevalMulNumTerm,
        pevalNegNumTerm,
        pevalSignumNumTerm
      ),
    SymRep (SymType),
    Term (ConTerm),
    conTerm,
    pevalSubNumTerm,
    pformatTerm,
    symTerm,
    typedConstantSymbol,
  )
import Grisette.Internal.SymPrim.SymAlgReal (SymAlgReal (SymAlgReal))
import Grisette.Internal.SymPrim.SymBV (SymIntN (SymIntN), SymWordN (SymWordN))
import Grisette.Internal.SymPrim.SymBool (SymBool (SymBool))
import Grisette.Internal.SymPrim.SymInteger (SymInteger (SymInteger))
import Language.Haskell.TH.Syntax (Lift)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
-- >>> import Grisette.Backend
-- >>> import Data.Proxy

-- | Symbolic IEEE 754 floating-point number with @eb@ exponent bits and @sb@
-- significand bits.
--
-- >>> "a" + 2.0 :: SymFP 11 53
-- (+ a 2.0)
-- >>> fpAdd rne "a" 2.0 :: SymFP 11 53
-- (fp.add rne a 2.0)
--
-- More operations are available. Please refer to "Grisette.Core#g:symops" for
-- more information.
newtype SymFP eb sb = SymFP {underlyingFPTerm :: Term (FP eb sb)}
  deriving (Lift, Generic)
  deriving anyclass (NFData)

-- | Symbolic IEEE 754 half-precision floating-point number.
type SymFP16 = SymFP 5 11

-- | Symbolic IEEE 754 single-precision floating-point number.
type SymFP32 = SymFP 8 24

-- | Symbolic IEEE 754 double-precision floating-point number.
type SymFP64 = SymFP 11 53

instance ConRep (SymFP eb sb) where
  type ConType (SymFP eb sb) = FP eb sb

instance (ValidFP eb sb) => SymRep (FP eb sb) where
  type SymType (FP eb sb) = SymFP eb sb

instance (ValidFP eb sb) => LinkedRep (FP eb sb) (SymFP eb sb) where
  underlyingTerm (SymFP a) = a
  wrapTerm = SymFP

instance (ValidFP eb sb) => Apply (SymFP eb sb) where
  type FunType (SymFP eb sb) = SymFP eb sb
  apply = id

instance (ValidFP eb sb) => Eq (SymFP eb sb) where
  SymFP a == SymFP b = a == b

instance (ValidFP eb sb) => Hashable (SymFP eb sb) where
  hashWithSalt s (SymFP a) = hashWithSalt s a

instance (ValidFP eb sb) => IsString (SymFP eb sb) where
  fromString = ssym . fromString

instance (ValidFP eb sb) => Solvable (FP eb sb) (SymFP eb sb) where
  con = SymFP . conTerm
  sym = SymFP . symTerm . typedConstantSymbol
  conView (SymFP (ConTerm _ _ _ _ t)) = Just t
  conView _ = Nothing

instance (ValidFP eb sb) => Show (SymFP eb sb) where
  show (SymFP a) = pformatTerm a

instance (ValidFP eb sb) => AllSyms (SymFP eb sb) where
  allSymsS v = (SomeSym v :)

instance (ValidFP eb sb) => Num (SymFP eb sb) where
  (SymFP l) + (SymFP r) = SymFP $ pevalAddNumTerm l r
  (SymFP l) - (SymFP r) = SymFP $ pevalSubNumTerm l r
  (SymFP l) * (SymFP r) = SymFP $ pevalMulNumTerm l r
  negate (SymFP v) = SymFP $ pevalNegNumTerm v
  abs (SymFP v) = SymFP $ pevalAbsNumTerm v
  signum (SymFP v) = SymFP $ pevalSignumNumTerm v
  fromInteger = con . fromInteger

instance (ValidFP eb sb) => Fractional (SymFP eb sb) where
  (SymFP l) / (SymFP r) = SymFP $ pevalFdivTerm l r
  recip (SymFP v) = SymFP $ pevalRecipTerm v
  fromRational = con . fromRational

instance (ValidFP eb sb) => Floating (SymFP eb sb) where
  pi = error "pi isn't supported by the underlying sbv library"
  exp = error "exp isn't supported by the underlying sbv library"
  log = error "log isn't supported by the underlying sbv library"
  sqrt (SymFP v) = SymFP $ pevalFloatingUnaryTerm FloatingSqrt v
  (**) = error "(**) isn't supported by the underlying sbv library"
  logBase = error "logBase isn't supported by the underlying sbv library"
  sin = error "sin isn't supported by the underlying sbv library"
  cos = error "cos isn't supported by the underlying sbv library"
  asin = error "asin isn't supported by the underlying sbv library"
  acos = error "acos isn't supported by the underlying sbv library"
  atan = error "atan isn't supported by the underlying sbv library"
  sinh = error "sinh isn't supported by the underlying sbv library"
  cosh = error "cosh isn't supported by the underlying sbv library"
  asinh = error "asinh isn't supported by the underlying sbv library"
  acosh = error "acosh isn't supported by the underlying sbv library"
  atanh = error "atanh isn't supported by the underlying sbv library"

-- | Symbolic floating-point rounding mode.
newtype SymFPRoundingMode = SymFPRoundingMode (Term FPRoundingMode)
  deriving (Lift, Generic)
  deriving anyclass (NFData)

instance ConRep SymFPRoundingMode where
  type ConType SymFPRoundingMode = FPRoundingMode

instance SymRep FPRoundingMode where
  type SymType FPRoundingMode = SymFPRoundingMode

instance LinkedRep FPRoundingMode SymFPRoundingMode where
  underlyingTerm (SymFPRoundingMode a) = a
  wrapTerm = SymFPRoundingMode

instance Apply SymFPRoundingMode where
  type FunType SymFPRoundingMode = SymFPRoundingMode
  apply = id

instance Eq SymFPRoundingMode where
  SymFPRoundingMode a == SymFPRoundingMode b = a == b

instance Hashable SymFPRoundingMode where
  hashWithSalt s (SymFPRoundingMode a) = hashWithSalt s a

instance IsString SymFPRoundingMode where
  fromString = ssym . fromString

instance Solvable FPRoundingMode SymFPRoundingMode where
  con = SymFPRoundingMode . conTerm
  sym = SymFPRoundingMode . symTerm . typedConstantSymbol
  conView (SymFPRoundingMode (ConTerm _ _ _ _ t)) = Just t
  conView _ = Nothing

instance Show SymFPRoundingMode where
  show (SymFPRoundingMode a) = pformatTerm a

instance AllSyms SymFPRoundingMode where
  allSymsS v = (SomeSym v :)

instance
  (ValidFP eb sb, r ~ (eb + sb)) =>
  BitCastCanonical (SymFP eb sb) (SymWordN r)
  where
  bitCastCanonicalValue _ =
    withValidFPProofs @eb @sb $
      con (bitCastCanonicalValue (Proxy @(FP eb sb)) :: WordN r)

instance
  (ValidFP eb sb, r ~ (eb + sb)) =>
  BitCastCanonical (SymFP eb sb) (SymIntN r)
  where
  bitCastCanonicalValue _ =
    withValidFPProofs @eb @sb $
      con (bitCastCanonicalValue (Proxy @(FP eb sb)) :: IntN r)

instance
  (ValidFP eb sb, r ~ (eb + sb)) =>
  BitCastOr (SymFP eb sb) (SymWordN r)
  where
  bitCastOr (SymWordN d) (SymFP a) =
    withValidFPProofs @eb @sb $ SymWordN (pevalBitCastOrTerm d a)

instance
  (ValidFP eb sb, r ~ (eb + sb)) =>
  BitCastOr (SymFP eb sb) (SymIntN r)
  where
  bitCastOr (SymIntN d) (SymFP a) =
    withValidFPProofs @eb @sb $ SymIntN (pevalBitCastOrTerm d a)

#define BIT_CAST_CANONICAL_VIA_INTERMEDIATE(from, to, intermediate) \
  instance BitCastCanonical (from) (to) where \
    bitCastCanonicalValue x = bitCast (bitCastCanonicalValue x :: intermediate)

instance
  (ValidFP eb sb, r ~ (eb + sb)) =>
  BitCast (SymIntN r) (SymFP eb sb)
  where
  bitCast (SymIntN a) =
    withValidFPProofs @eb @sb $ SymFP $ pevalBitCastTerm a

instance
  (ValidFP eb sb, r ~ (eb + sb)) =>
  BitCast (SymWordN r) (SymFP eb sb)
  where
  bitCast (SymWordN a) =
    withValidFPProofs @eb @sb $ SymFP $ pevalBitCastTerm a

instance (ValidFP eb sb) => IEEEFPConstants (SymFP eb sb) where
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
  fpMinNormalized = con fpMinNormalized
  {-# INLINE fpMinNormalized #-}
  fpMinSubnormal = con fpMinSubnormal
  {-# INLINE fpMinSubnormal #-}
  fpMaxNormalized = con fpMaxNormalized
  {-# INLINE fpMaxNormalized #-}
  fpMaxSubnormal = con fpMaxSubnormal
  {-# INLINE fpMaxSubnormal #-}

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

instance (ValidFP eb sb) => IEEEFPOp (SymFP eb sb) where
  fpAbs (SymFP l) = SymFP $ pevalFPUnaryTerm FPAbs l
  {-# INLINE fpAbs #-}
  fpNeg (SymFP l) = SymFP $ pevalFPUnaryTerm FPNeg l
  {-# INLINE fpNeg #-}
  fpRem (SymFP l) (SymFP r) = SymFP $ pevalFPBinaryTerm FPRem l r
  {-# INLINE fpRem #-}
  fpMinimum (SymFP l) (SymFP r) = SymFP $ pevalFPBinaryTerm FPMinimum l r
  {-# INLINE fpMinimum #-}
  fpMinimumNumber (SymFP l) (SymFP r) =
    SymFP $ pevalFPBinaryTerm FPMinimumNumber l r
  {-# INLINE fpMinimumNumber #-}
  fpMaximum (SymFP l) (SymFP r) = SymFP $ pevalFPBinaryTerm FPMaximum l r
  {-# INLINE fpMaximum #-}
  fpMaximumNumber (SymFP l) (SymFP r) =
    SymFP $ pevalFPBinaryTerm FPMaximumNumber l r
  {-# INLINE fpMaximumNumber #-}

instance IEEEFPRoundingMode SymFPRoundingMode where
  rne = con RNE
  {-# INLINE rne #-}
  rna = con RNA
  {-# INLINE rna #-}
  rtp = con RTP
  {-# INLINE rtp #-}
  rtn = con RTN
  {-# INLINE rtn #-}
  rtz = con RTZ
  {-# INLINE rtz #-}

instance (ValidFP eb sb) => IEEEFPRoundingOp (SymFP eb sb) SymFPRoundingMode where
  fpAdd (SymFPRoundingMode mode) (SymFP l) (SymFP r) =
    SymFP $ pevalFPRoundingBinaryTerm FPAdd mode l r
  {-# INLINE fpAdd #-}
  fpSub (SymFPRoundingMode mode) (SymFP l) (SymFP r) =
    SymFP $ pevalFPRoundingBinaryTerm FPSub mode l r
  {-# INLINE fpSub #-}
  fpMul (SymFPRoundingMode mode) (SymFP l) (SymFP r) =
    SymFP $ pevalFPRoundingBinaryTerm FPMul mode l r
  {-# INLINE fpMul #-}
  fpDiv (SymFPRoundingMode mode) (SymFP l) (SymFP r) =
    SymFP $ pevalFPRoundingBinaryTerm FPDiv mode l r
  {-# INLINE fpDiv #-}
  fpFMA (SymFPRoundingMode mode) (SymFP l) (SymFP m) (SymFP r) =
    SymFP $ pevalFPFMATerm mode l m r
  {-# INLINE fpFMA #-}
  fpSqrt (SymFPRoundingMode mode) (SymFP v) =
    SymFP $ pevalFPRoundingUnaryTerm FPSqrt mode v
  {-# INLINE fpSqrt #-}
  fpRoundToIntegral (SymFPRoundingMode mode) (SymFP v) =
    SymFP $ pevalFPRoundingUnaryTerm FPRoundToIntegral mode v
  {-# INLINE fpRoundToIntegral #-}

instance
  (ValidFP eb sb) =>
  IEEEFPConvertible SymInteger (SymFP eb sb) SymFPRoundingMode
  where
  fromFPOr (SymInteger d) (SymFPRoundingMode mode) (SymFP fp) =
    SymInteger $ pevalFromFPOrTerm d mode fp
  toFP (SymFPRoundingMode mode) (SymInteger v) = SymFP $ pevalToFPTerm mode v

instance
  (ValidFP eb sb) =>
  IEEEFPConvertible SymAlgReal (SymFP eb sb) SymFPRoundingMode
  where
  fromFPOr (SymAlgReal d) (SymFPRoundingMode mode) (SymFP fp) =
    SymAlgReal $ pevalFromFPOrTerm d mode fp
  toFP (SymFPRoundingMode mode) (SymAlgReal v) = SymFP $ pevalToFPTerm mode v

instance
  (ValidFP eb sb) =>
  IEEEFPToAlgReal SymAlgReal (SymFP eb sb) SymFPRoundingMode

instance
  (ValidFP eb sb, KnownNat n, 1 <= n) =>
  IEEEFPConvertible (SymWordN n) (SymFP eb sb) SymFPRoundingMode
  where
  fromFPOr (SymWordN d) (SymFPRoundingMode mode) (SymFP fp) =
    SymWordN $ pevalFromFPOrTerm d mode fp
  toFP (SymFPRoundingMode mode) (SymWordN v) = SymFP $ pevalToFPTerm mode v

instance
  (ValidFP eb sb, KnownNat n, 1 <= n) =>
  IEEEFPConvertible (SymIntN n) (SymFP eb sb) SymFPRoundingMode
  where
  fromFPOr (SymIntN d) (SymFPRoundingMode mode) (SymFP fp) =
    SymIntN $ pevalFromFPOrTerm d mode fp
  toFP (SymFPRoundingMode mode) (SymIntN v) = SymFP $ pevalToFPTerm mode v

instance
  (ValidFP eb sb, ValidFP eb' sb') =>
  IEEEFPConvertible (SymFP eb' sb') (SymFP eb sb) SymFPRoundingMode
  where
  fromFPOr (SymFP d) (SymFPRoundingMode mode) (SymFP fp) =
    SymFP $ pevalFromFPOrTerm d mode fp
  toFP (SymFPRoundingMode mode) (SymFP v) = SymFP $ pevalToFPTerm mode v

instance (ValidFP eb sb) => Serial (SymFP eb sb) where
  serialize = serialize . underlyingFPTerm
  deserialize = SymFP <$> deserialize

instance (ValidFP eb sb) => Cereal.Serialize (SymFP eb sb) where
  put = serialize
  get = deserialize

instance (ValidFP eb sb) => Binary.Binary (SymFP eb sb) where
  put = serialize
  get = deserialize
