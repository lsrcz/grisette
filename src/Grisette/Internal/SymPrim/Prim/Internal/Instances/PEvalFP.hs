{-# LANGUAGE FlexibleContexts #-}

module Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalFP
  ( pevalFPTraitTerm,
    sbvFPTraitTerm,
  )
where

import qualified Data.SBV as SBV
import Grisette.Internal.SymPrim.FP (FP, ValidFP)
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( FPTrait
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
    SupportedPrim,
    Term (ConTerm),
    conTerm,
    fpTraitTerm,
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
goodFpIsPositive x = SBV.sNot (SBV.fpIsNaN x) SBV..&& SBV.fpIsPositive x
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
