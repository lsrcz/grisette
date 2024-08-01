{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalIEEEFPConvertibleTerm
  ( genericFPCast,
  )
where

import Data.Proxy (Proxy (Proxy))
import qualified Data.SBV as SBV
import qualified Data.SBV.Internals as SBVI
import GHC.TypeLits (KnownNat, Nat, type (<=))
import Grisette.Internal.Core.Data.Class.IEEEFP
  ( IEEEFPConvertible (fromFPOr, toFP),
  )
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP
  ( ConvertibleBound (convertibleLowerBound, convertibleUpperBound),
    FP,
    FPRoundingMode (RNA, RNE, RTN, RTP, RTZ),
    ValidFP,
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
    SupportedPrim (conSBVTerm),
    Term (ConTerm),
    conTerm,
    fromFPOrTerm,
    toFPTerm,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Unfold (binaryUnfoldOnce)

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
  forall bv (n :: Nat) eb sb m p r.
  ( KnownIsZero m,
    ValidFP eb sb,
    SBVI.HasKind r,
    SBVI.SymVal r,
    Bounded r,
    Num r,
    Ord r,
    SBVI.SBV r ~ SBVType m (bv n),
    KnownNat n,
    1 <= n,
    ConvertibleBound bv
  ) =>
  p m ->
  SBVI.SBV r ->
  SBVType m FPRoundingMode ->
  SBVType m (FP eb sb) ->
  SBVI.SBV r
boundedSBVFromFPTerm p d mode l =
  SBV.ite
    ( SBV.fpIsNaN l
        SBV..|| SBV.fpIsInfinite l
        SBV..|| l SBV..< bound convertibleLowerBound
        SBV..|| l SBV..> bound convertibleUpperBound
    )
    d
    $ genericFPCast mode l
  where
    lst =
      [ (SBV.sRTP, RTP),
        (SBV.sRTN, RTN),
        (SBV.sRTZ, RTZ),
        (SBV.sRNE, RNE),
        (SBV.sRNA, RNA)
      ]
    bound f =
      foldl
        ( \acc (srm, rm) ->
            SBV.ite
              (mode SBV..== srm)
              ( conSBVTerm
                  p
                  (f (undefined :: bv n) rm :: FP eb sb)
              )
              acc
        )
        (0 :: SBVType m (FP eb sb))
        lst

generalPevalFromFPOrTerm ::
  ( PEvalIEEEFPConvertibleTerm a,
    ValidFP eb sb,
    IEEEFPConvertible a (FP eb sb) FPRoundingMode
  ) =>
  Term a ->
  Term FPRoundingMode ->
  Term (FP eb sb) ->
  Term a
generalPevalFromFPOrTerm (ConTerm _ d) (ConTerm _ rd) (ConTerm _ f) =
  conTerm $ fromFPOr d rd f
generalPevalFromFPOrTerm d rd f = fromFPOrTerm d rd f

algRealPevalFromFPOrTerm ::
  ( PEvalIEEEFPConvertibleTerm a,
    ValidFP eb sb,
    IEEEFPConvertible a (FP eb sb) FPRoundingMode
  ) =>
  Term a ->
  Term FPRoundingMode ->
  Term (FP eb sb) ->
  Term a
algRealPevalFromFPOrTerm (ConTerm _ d) _ (ConTerm _ f) =
  conTerm $ fromFPOr d RNE f
algRealPevalFromFPOrTerm d _ f = fromFPOrTerm d (conTerm RNE) f

generalDoPevalToFPTerm ::
  ( PEvalIEEEFPConvertibleTerm a,
    ValidFP eb sb,
    IEEEFPConvertible a (FP eb sb) FPRoundingMode
  ) =>
  Term FPRoundingMode ->
  Term a ->
  Maybe (Term (FP eb sb))
generalDoPevalToFPTerm (ConTerm _ rd) (ConTerm _ f) =
  Just $ conTerm $ toFP rd f
generalDoPevalToFPTerm _ _ = Nothing

generalPevalToFPTerm ::
  ( PEvalIEEEFPConvertibleTerm a,
    ValidFP eb sb,
    IEEEFPConvertible a (FP eb sb) FPRoundingMode
  ) =>
  Term FPRoundingMode ->
  Term a ->
  Term (FP eb sb)
generalPevalToFPTerm = binaryUnfoldOnce generalDoPevalToFPTerm toFPTerm

instance PEvalIEEEFPConvertibleTerm Integer where
  pevalFromFPOrTerm = generalPevalFromFPOrTerm
  pevalToFPTerm = generalPevalToFPTerm
  sbvFromFPOrTerm (p :: p n) d mode l =
    case isZero p of
      IsZeroEvidence ->
        SBV.ite (SBV.fpIsNaN l SBV..|| SBV.fpIsInfinite l) d $
          let r = SBV.fromSFloatingPoint mode l :: SBV.SReal
              ifloor = SBV.sRealToSInteger r
              diff = r - SBV.sFromIntegral ifloor
           in SBV.ite (diff SBV..== 0) ifloor
                $ SBV.ite (mode SBV..== SBV.sRTN) ifloor
                $ SBV.ite (mode SBV..== SBV.sRTP) (ifloor + 1)
                $ SBV.ite
                  (mode SBV..== SBV.sRTZ)
                  (SBV.ite (ifloor SBV..< 0) (ifloor + 1) ifloor)
                $ SBV.ite
                  (diff SBV..== 0.5)
                  ( SBV.ite
                      (mode SBV..== SBV.sRNE)
                      ( SBV.ite
                          (SBV.sMod ifloor 2 SBV..== 0)
                          ifloor
                          (ifloor + 1)
                      )
                      (SBV.ite (ifloor SBV..< 0) ifloor (ifloor + 1))
                  )
                $ SBV.ite
                  (diff SBV..< 0.5)
                  ifloor
                  (ifloor + 1)
      NonZeroEvidence -> boundedSBVFromFPTerm @IntN p d mode l
  sbvToFPTerm p mode l =
    case isZero p of
      IsZeroEvidence -> SBV.toSFloatingPoint mode l
      NonZeroEvidence -> genericFPCast mode l

instance PEvalIEEEFPConvertibleTerm AlgReal where
  pevalFromFPOrTerm = algRealPevalFromFPOrTerm
  pevalToFPTerm = generalPevalToFPTerm
  sbvFromFPOrTerm _ d mode l =
    SBV.ite (SBV.fpIsNaN l SBV..|| SBV.fpIsInfinite l) d $
      SBV.fromSFloatingPoint mode l
  sbvToFPTerm _ v =
    case SBV.unliteral v of
      Nothing -> SBV.toSFloatingPoint v
      Just _ ->
        error $
          "SBV is buggy on converting literal AlgReal to an FP. "
            ++ "This should never be called. "
            ++ "https://github.com/LeventErkok/sbv/pull/718"

instance (KnownNat n, 1 <= n) => PEvalIEEEFPConvertibleTerm (WordN n) where
  pevalFromFPOrTerm = generalPevalFromFPOrTerm
  pevalToFPTerm = generalPevalToFPTerm
  sbvFromFPOrTerm = bvIsNonZeroFromGEq1 (Proxy @n) $ boundedSBVFromFPTerm @WordN
  sbvToFPTerm _ mode l = bvIsNonZeroFromGEq1 (Proxy @n) $ genericFPCast mode l

instance (KnownNat n, 1 <= n) => PEvalIEEEFPConvertibleTerm (IntN n) where
  pevalFromFPOrTerm = generalPevalFromFPOrTerm
  pevalToFPTerm = generalPevalToFPTerm
  sbvFromFPOrTerm = bvIsNonZeroFromGEq1 (Proxy @n) $ boundedSBVFromFPTerm @IntN
  sbvToFPTerm _ mode l = bvIsNonZeroFromGEq1 (Proxy @n) $ genericFPCast mode l

instance (ValidFP eb sb) => PEvalIEEEFPConvertibleTerm (FP eb sb) where
  pevalFromFPOrTerm _ = generalPevalToFPTerm
  pevalToFPTerm = generalPevalToFPTerm
  sbvFromFPOrTerm _ _ v =
    case SBV.unliteral v of
      Nothing -> SBV.toSFloatingPoint v
      Just _ ->
        error $
          "SBV is buggy on converting literal FP to another FP with different "
            ++ "precision. This should never be called. "
            ++ "https://github.com/LeventErkok/sbv/pull/717"
  sbvToFPTerm _ v = case SBV.unliteral v of
    Nothing -> SBV.toSFloatingPoint v
    Just _ ->
      error $
        "SBV is buggy on converting literal FP to another FP with different "
          ++ "precision. This should never be called. "
          ++ "https://github.com/LeventErkok/sbv/pull/717"