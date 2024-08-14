{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalFP
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
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
import Grisette.Internal.Core.Data.Class.IEEEFP
  ( IEEEFPOp
      ( fpMaximum,
        fpMaximumNumber,
        fpMinimum,
        fpMinimumNumber,
        fpRem
      ),
    IEEEFPRoundingOp
      ( fpAdd,
        fpDiv,
        fpFMA,
        fpMul,
        fpRoundToIntegral,
        fpSqrt,
        fpSub
      ),
  )
import Grisette.Internal.SymPrim.FP (FP, FPRoundingMode, ValidFP)
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( FPBinaryOp (FPMaximum, FPMaximumNumber, FPMinimum, FPMinimumNumber, FPRem),
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

-- | Partially evaluate a floating-point trait term.
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

-- | Lowering an floating-point trait term to sbv.
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

-- | Partially evaluate a floating-point unary term.
pevalFPUnaryTerm ::
  (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
  FPUnaryOp ->
  Term (FP eb sb) ->
  Term (FP eb sb)
pevalFPUnaryTerm = fpUnaryTerm
{-# INLINE pevalFPUnaryTerm #-}

-- | Lowering an floating-point unary term to sbv.
sbvFPUnaryTerm ::
  (ValidFP eb sb) =>
  FPUnaryOp ->
  SBV.SFloatingPoint eb sb ->
  SBV.SFloatingPoint eb sb
sbvFPUnaryTerm FPAbs = SBV.fpAbs
sbvFPUnaryTerm FPNeg = SBV.fpNeg
{-# INLINE sbvFPUnaryTerm #-}

-- | Partially evaluate a floating-point binary term.
pevalFPBinaryTerm ::
  (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
  FPBinaryOp ->
  Term (FP eb sb) ->
  Term (FP eb sb) ->
  Term (FP eb sb)
pevalFPBinaryTerm bop (ConTerm _ l) (ConTerm _ r) =
  case bop of
    FPMaximum -> conTerm $ fpMaximum l r
    FPMaximumNumber -> conTerm $ fpMaximumNumber l r
    FPMinimum -> conTerm $ fpMinimum l r
    FPMinimumNumber -> conTerm $ fpMinimumNumber l r
    FPRem -> conTerm $ fpRem l r
pevalFPBinaryTerm FPMaximum l r | l == r = l
pevalFPBinaryTerm FPMaximumNumber l r | l == r = l
pevalFPBinaryTerm FPMinimum l r | l == r = l
pevalFPBinaryTerm FPMinimumNumber l r | l == r = l
pevalFPBinaryTerm bop l r = fpBinaryTerm bop l r
{-# INLINE pevalFPBinaryTerm #-}

sbvCmpHandleNegZero ::
  (ValidFP eb sb) =>
  SBV.SFloatingPoint eb sb ->
  SBV.SFloatingPoint eb sb ->
  SBV.SBool
sbvCmpHandleNegZero x y =
  SBV.ite
    (SBV.fpIsZero x SBV..&& SBV.fpIsZero y)
    (SBV.fpIsNegativeZero x)
    (x SBV..< y)

-- | Lowering an floating-point binary term to sbv.
sbvFPBinaryTerm ::
  (ValidFP eb sb) =>
  FPBinaryOp ->
  SBV.SFloatingPoint eb sb ->
  SBV.SFloatingPoint eb sb ->
  SBV.SFloatingPoint eb sb
sbvFPBinaryTerm FPRem x y = SBV.fpRem x y
sbvFPBinaryTerm FPMinimum x y =
  SBV.ite (SBV.fpIsNaN x SBV..|| SBV.fpIsNaN y) SBV.nan $
    SBV.ite (sbvCmpHandleNegZero x y) x y
sbvFPBinaryTerm FPMinimumNumber x y =
  SBV.ite (SBV.fpIsNaN x) y $
    SBV.ite (SBV.fpIsNaN y) x $
      SBV.ite (sbvCmpHandleNegZero x y) x y
sbvFPBinaryTerm FPMaximum x y =
  SBV.ite (SBV.fpIsNaN x SBV..|| SBV.fpIsNaN y) SBV.nan $
    SBV.ite (sbvCmpHandleNegZero x y) y x
sbvFPBinaryTerm FPMaximumNumber x y =
  SBV.ite (SBV.fpIsNaN x) y $
    SBV.ite (SBV.fpIsNaN y) x $
      SBV.ite (sbvCmpHandleNegZero x y) y x
{-# INLINE sbvFPBinaryTerm #-}

-- | Partially evaluate a floating-point rounding unary term.
pevalFPRoundingUnaryTerm ::
  (ValidFP eb sb, SupportedPrim (FP eb sb), SupportedPrim FPRoundingMode) =>
  FPRoundingUnaryOp ->
  Term FPRoundingMode ->
  Term (FP eb sb) ->
  Term (FP eb sb)
pevalFPRoundingUnaryTerm uop (ConTerm _ rd) (ConTerm _ l) =
  case uop of
    FPSqrt -> conTerm $ fpSqrt rd l
    FPRoundToIntegral -> conTerm $ fpRoundToIntegral rd l
pevalFPRoundingUnaryTerm uop rd l = fpRoundingUnaryTerm uop rd l
{-# INLINE pevalFPRoundingUnaryTerm #-}

-- | Lowering an floating-point rounding unary term to sbv.
sbvFPRoundingUnaryTerm ::
  (ValidFP eb sb) =>
  FPRoundingUnaryOp ->
  SBV.SRoundingMode ->
  SBV.SFloatingPoint eb sb ->
  SBV.SFloatingPoint eb sb
sbvFPRoundingUnaryTerm FPSqrt = SBV.fpSqrt
sbvFPRoundingUnaryTerm FPRoundToIntegral = SBV.fpRoundToIntegral
{-# INLINE sbvFPRoundingUnaryTerm #-}

-- | Partially evaluate a floating-point rounding binary term.
pevalFPRoundingBinaryTerm ::
  (ValidFP eb sb, SupportedPrim (FP eb sb), SupportedPrim FPRoundingMode) =>
  FPRoundingBinaryOp ->
  Term FPRoundingMode ->
  Term (FP eb sb) ->
  Term (FP eb sb) ->
  Term (FP eb sb)
pevalFPRoundingBinaryTerm bop (ConTerm _ rd) (ConTerm _ l) (ConTerm _ r) =
  case bop of
    FPAdd -> conTerm $ fpAdd rd l r
    FPSub -> conTerm $ fpSub rd l r
    FPMul -> conTerm $ fpMul rd l r
    FPDiv -> conTerm $ fpDiv rd l r
pevalFPRoundingBinaryTerm bop rd l r = fpRoundingBinaryTerm bop rd l r
{-# INLINE pevalFPRoundingBinaryTerm #-}

-- | Lowering an floating-point rounding binary term to sbv.
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

-- | Partially evaluate a floating-point fused multiply-add term.
pevalFPFMATerm ::
  (ValidFP eb sb, SupportedPrim (FP eb sb), SupportedPrim FPRoundingMode) =>
  Term FPRoundingMode ->
  Term (FP eb sb) ->
  Term (FP eb sb) ->
  Term (FP eb sb) ->
  Term (FP eb sb)
pevalFPFMATerm (ConTerm _ rd) (ConTerm _ x) (ConTerm _ y) (ConTerm _ z) =
  conTerm $ fpFMA rd x y z
pevalFPFMATerm rd x y z = fpFMATerm rd x y z
{-# INLINE pevalFPFMATerm #-}

-- | Lowering an floating-point fused multiply-add term to sbv.
sbvFPFMATerm ::
  (ValidFP eb sb) =>
  SBV.SRoundingMode ->
  SBV.SFloatingPoint eb sb ->
  SBV.SFloatingPoint eb sb ->
  SBV.SFloatingPoint eb sb ->
  SBV.SFloatingPoint eb sb
sbvFPFMATerm = SBV.fpFMA
{-# INLINE sbvFPFMATerm #-}
