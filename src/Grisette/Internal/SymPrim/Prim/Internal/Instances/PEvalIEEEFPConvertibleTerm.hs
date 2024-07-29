{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalIEEEFPConvertibleTerm () where

import Data.Proxy (Proxy (Proxy))
import qualified Data.SBV as SBV
import qualified Data.SBV.Internals as SBVI
import GHC.TypeLits (KnownNat, type (<=))
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP
  ( FP,
    FPRoundingMode,
    ValidFP,
    withValidFPProofs,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Instances.SupportedPrim
  ( bvIsNonZeroFromGEq1,
  )
import Grisette.Internal.SymPrim.Prim.Internal.IsZero
  ( IsZeroCases (IsZeroEvidence, NonZeroEvidence),
    KnownIsZero (isZero),
  )
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( PEvalIEEEFPConvertibleTerm
      ( pevalFromFPOrTerm,
        pevalToFPTerm,
        sbvFromFPOrTerm,
        sbvToFPTerm
      ),
    SBVRep (SBVType),
    fromFPOrTerm,
    toFPTerm,
  )

genericFPCast ::
  forall a r.
  (SBV.HasKind a, SBV.HasKind r) =>
  SBV.SRoundingMode ->
  SBV.SBV a ->
  SBV.SBV r
genericFPCast rm f = SBVI.SBV (SBVI.SVal kTo (Right (SBVI.cache r)))
  where
    kFrom = SBVI.kindOf f
    kTo = SBVI.kindOf (Proxy @r)
    r st = do
      msv <- SBVI.sbvToSV st rm
      xsv <- SBVI.sbvToSV st f
      SBVI.newExpr st kTo $
        SBVI.SBVApp
          (SBVI.IEEEFP (SBVI.FP_Cast kFrom kTo msv))
          [xsv]

boundedSBVFromFPTerm ::
  forall m eb sb p r.
  (KnownIsZero m, ValidFP eb sb, SBVI.HasKind r, SBVI.SymVal r, Bounded r) =>
  p m ->
  SBVI.SBV r ->
  SBVType m FPRoundingMode ->
  SBVType m (FP eb sb) ->
  SBVI.SBV r
boundedSBVFromFPTerm _ d mode l =
  SBV.ite
    ( SBV.fpIsNaN l
        SBV..|| SBV.fpIsInfinite l
        SBV..|| l SBV..< lb
        SBV..|| l SBV..> rb
    )
    d
    $ genericFPCast mode l
  where
    lb :: SBVType m (FP eb sb)
    lb = withValidFPProofs @eb @sb $ genericFPCast mode (minBound :: SBVI.SBV r)
    rb :: SBVType m (FP eb sb)
    rb = withValidFPProofs @eb @sb $ genericFPCast mode (maxBound :: SBVI.SBV r)

instance PEvalIEEEFPConvertibleTerm Integer where
  pevalFromFPOrTerm = fromFPOrTerm
  pevalToFPTerm = toFPTerm
  sbvFromFPOrTerm (p :: p n) d mode l =
    case isZero p of
      IsZeroEvidence ->
        SBV.ite (SBV.fpIsNaN l SBV..|| SBV.fpIsInfinite l) d $
          SBV.fromSFloatingPoint mode l
      NonZeroEvidence -> boundedSBVFromFPTerm p d mode l
  sbvToFPTerm p mode l =
    case isZero p of
      IsZeroEvidence -> SBV.toSFloatingPoint mode l
      NonZeroEvidence -> genericFPCast mode l

instance PEvalIEEEFPConvertibleTerm AlgReal where
  pevalFromFPOrTerm = fromFPOrTerm
  pevalToFPTerm = toFPTerm
  sbvFromFPOrTerm _ d mode l =
    SBV.ite (SBV.fpIsNaN l SBV..|| SBV.fpIsInfinite l) d $
      SBV.fromSFloatingPoint mode l
  sbvToFPTerm _ = SBV.toSFloatingPoint

instance (KnownNat n, 1 <= n) => PEvalIEEEFPConvertibleTerm (WordN n) where
  pevalFromFPOrTerm = fromFPOrTerm
  pevalToFPTerm = toFPTerm
  sbvFromFPOrTerm = bvIsNonZeroFromGEq1 (Proxy @n) boundedSBVFromFPTerm
  sbvToFPTerm _ mode l = bvIsNonZeroFromGEq1 (Proxy @n) $ genericFPCast mode l

instance (KnownNat n, 1 <= n) => PEvalIEEEFPConvertibleTerm (IntN n) where
  pevalFromFPOrTerm = fromFPOrTerm
  pevalToFPTerm = toFPTerm
  sbvFromFPOrTerm = bvIsNonZeroFromGEq1 (Proxy @n) boundedSBVFromFPTerm
  sbvToFPTerm _ mode l = bvIsNonZeroFromGEq1 (Proxy @n) $ genericFPCast mode l
