{-# LANGUAGE FlexibleContexts #-}

module Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalFP
  ( pevalFPTraitTerm,
    sbvFPTraitTerm,
    pevalFPUnaryTerm,
    sbvFPUnaryTerm,
    pevalFPBinaryTerm,
    sbvFPBinaryTerm,
    pevalFPRoundingUnaryTerm,
    sbvFPRoundingUnaryTerm,
    pevalFPRoundingBinaryTerm,
    sbvFPRoundingBinaryTerm,
    pevalFPFMATerm,
    sbvFPFMATerm,
  )
where

import qualified Data.SBV as SBV
import Grisette.Internal.SymPrim.FP (FP, FPRoundingMode, ValidFP)
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
    SupportedPrim,
    Term (ConTerm),
    conTerm,
    fpBinaryTerm,
    fpFMATerm,
    fpRoundingBinaryTerm,
    fpRoundingUnaryTerm,
    fpTraitTerm,
    fpUnaryTerm,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Unfold (unaryUnfoldOnce)

pevalFPTraitTerm ::
  (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
  FPTrait ->
  Term (FP eb sb) ->
  Term Bool
pevalFPTraitTerm trait = unaryUnfoldOnce doPevalFPTraitTerm (fpTraitTerm trait)
  where
    doPevalFPTraitTerm (ConTerm _ a) = case trait of
      FPIsNaN -> Just $ conTerm $ isNaN a
      FPIsPositive ->
        Just $
          conTerm $
            not (isNaN a) && a >= 0 && not (isNegativeZero a)
      FPIsNegative ->
        Just $ conTerm $ not (isNaN a) && (a < 0 || isNegativeZero a)
      FPIsInfinite -> Just $ conTerm $ isInfinite a
      FPIsPositiveInfinite -> Just $ conTerm $ isInfinite a && a > 0
      FPIsNegativeInfinite -> Just $ conTerm $ isInfinite a && a < 0
      FPIsPositiveZero ->
        Just $ conTerm $ a == 0 && not (isNegativeZero a)
      FPIsNegativeZero -> Just $ conTerm $ isNegativeZero a
      FPIsZero -> Just $ conTerm $ a == 0
      FPIsNormal ->
        Just $
          conTerm $
            not (a == 0 || isNaN a || isInfinite a || isDenormalized a)
      FPIsSubnormal -> Just $ conTerm $ isDenormalized a
      FPIsPoint -> Just $ conTerm $ not (isNaN a || isInfinite a)
    doPevalFPTraitTerm _ = Nothing

-- Workaround for https://github.com/GaloisInc/libBF-hs/pull/32, which affects
-- the correctness of the Ord instance for 'Data.SBV.FloatingPoint'.
goodFpIsPositive ::
  (ValidFP eb sb) =>
  SBV.SFloatingPoint eb sb ->
  SBV.SBool
goodFpIsPositive x =
  SBV.sNot (SBV.fpIsNaN x) SBV..&& SBV.fpIsPositive x
{-# INLINE goodFpIsPositive #-}

goodFpIsNegative ::
  (ValidFP eb sb) =>
  SBV.SFloatingPoint eb sb ->
  SBV.SBool
goodFpIsNegative x = SBV.sNot (SBV.fpIsNaN x) SBV..&& SBV.fpIsNegative x
{-# INLINE goodFpIsNegative #-}

sbvFPTraitTerm ::
  (ValidFP eb sb) =>
  FPTrait ->
  SBV.SFloatingPoint eb sb ->
  SBV.SBool
sbvFPTraitTerm FPIsNaN = SBV.fpIsNaN
sbvFPTraitTerm FPIsPositive = goodFpIsPositive
sbvFPTraitTerm FPIsNegative = goodFpIsNegative
sbvFPTraitTerm FPIsInfinite = SBV.fpIsInfinite
sbvFPTraitTerm FPIsPositiveInfinite = \f ->
  SBV.fpIsInfinite f SBV..&& goodFpIsPositive f
sbvFPTraitTerm FPIsNegativeInfinite = \f ->
  SBV.fpIsInfinite f SBV..&& goodFpIsNegative f
sbvFPTraitTerm FPIsPositiveZero =
  \f -> SBV.fpIsZero f SBV..&& goodFpIsPositive f
sbvFPTraitTerm FPIsNegativeZero =
  \f -> SBV.fpIsZero f SBV..&& goodFpIsNegative f
sbvFPTraitTerm FPIsZero = SBV.fpIsZero
sbvFPTraitTerm FPIsNormal = SBV.fpIsNormal
sbvFPTraitTerm FPIsSubnormal = SBV.fpIsSubnormal
sbvFPTraitTerm FPIsPoint = SBV.fpIsPoint

pevalFPUnaryTerm ::
  (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
  FPUnaryOp ->
  Term (FP eb sb) ->
  Term (FP eb sb)
pevalFPUnaryTerm = fpUnaryTerm
{-# INLINE pevalFPUnaryTerm #-}

sbvFPUnaryTerm ::
  (ValidFP eb sb) =>
  FPUnaryOp ->
  SBV.SFloatingPoint eb sb ->
  SBV.SFloatingPoint eb sb
sbvFPUnaryTerm FPAbs = SBV.fpAbs
sbvFPUnaryTerm FPNeg = SBV.fpNeg
{-# INLINE sbvFPUnaryTerm #-}

pevalFPBinaryTerm ::
  (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
  FPBinaryOp ->
  Term (FP eb sb) ->
  Term (FP eb sb) ->
  Term (FP eb sb)
pevalFPBinaryTerm = fpBinaryTerm
{-# INLINE pevalFPBinaryTerm #-}

sbvFPBinaryTerm ::
  (ValidFP eb sb) =>
  FPBinaryOp ->
  SBV.SFloatingPoint eb sb ->
  SBV.SFloatingPoint eb sb ->
  SBV.SFloatingPoint eb sb
sbvFPBinaryTerm FPRem = SBV.fpRem
sbvFPBinaryTerm FPMin = SBV.fpMin
sbvFPBinaryTerm FPMax = SBV.fpMax
{-# INLINE sbvFPBinaryTerm #-}

pevalFPRoundingUnaryTerm ::
  (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
  FPRoundingUnaryOp ->
  Term FPRoundingMode ->
  Term (FP eb sb) ->
  Term (FP eb sb)
pevalFPRoundingUnaryTerm = fpRoundingUnaryTerm
{-# INLINE pevalFPRoundingUnaryTerm #-}

sbvFPRoundingUnaryTerm ::
  (ValidFP eb sb) =>
  FPRoundingUnaryOp ->
  SBV.SRoundingMode ->
  SBV.SFloatingPoint eb sb ->
  SBV.SFloatingPoint eb sb
sbvFPRoundingUnaryTerm FPSqrt = SBV.fpSqrt
sbvFPRoundingUnaryTerm FPRoundToIntegral = SBV.fpRoundToIntegral
{-# INLINE sbvFPRoundingUnaryTerm #-}

pevalFPRoundingBinaryTerm ::
  (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
  FPRoundingBinaryOp ->
  Term FPRoundingMode ->
  Term (FP eb sb) ->
  Term (FP eb sb) ->
  Term (FP eb sb)
pevalFPRoundingBinaryTerm = fpRoundingBinaryTerm
{-# INLINE pevalFPRoundingBinaryTerm #-}

sbvFPRoundingBinaryTerm ::
  (ValidFP eb sb) =>
  FPRoundingBinaryOp ->
  SBV.SRoundingMode ->
  SBV.SFloatingPoint eb sb ->
  SBV.SFloatingPoint eb sb ->
  SBV.SFloatingPoint eb sb
sbvFPRoundingBinaryTerm FPAdd = SBV.fpAdd
sbvFPRoundingBinaryTerm FPSub = SBV.fpSub
sbvFPRoundingBinaryTerm FPMul = SBV.fpMul
sbvFPRoundingBinaryTerm FPDiv = SBV.fpDiv
{-# INLINE sbvFPRoundingBinaryTerm #-}

pevalFPFMATerm ::
  (ValidFP eb sb, SupportedPrim (FP eb sb), SupportedPrim FPRoundingMode) =>
  Term FPRoundingMode ->
  Term (FP eb sb) ->
  Term (FP eb sb) ->
  Term (FP eb sb) ->
  Term (FP eb sb)
pevalFPFMATerm = fpFMATerm
{-# INLINE pevalFPFMATerm #-}

sbvFPFMATerm ::
  (ValidFP eb sb) =>
  SBV.SRoundingMode ->
  SBV.SFloatingPoint eb sb ->
  SBV.SFloatingPoint eb sb ->
  SBV.SFloatingPoint eb sb ->
  SBV.SFloatingPoint eb sb
sbvFPFMATerm = SBV.fpFMA
{-# INLINE sbvFPFMATerm #-}
