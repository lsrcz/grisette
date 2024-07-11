{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Prim.Internal.Instances.SupportedPrim
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.Prim.Internal.Instances.SupportedPrim
  ( bvIsNonZeroFromGEq1,
  )
where

import Data.Coerce (coerce)
import Data.Proxy (Proxy (Proxy))
import Data.SBV (BVIsNonZero)
import qualified Data.SBV as SBV
import Data.Type.Bool (If)
import Data.Type.Equality ((:~:) (Refl), type (:~~:) (HRefl))
import GHC.TypeNats (KnownNat, type (<=))
import Grisette.Internal.SymPrim.AlgReal (AlgReal, fromSBVAlgReal, toSBVAlgReal)
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP
  ( FP (FP),
    FPRoundingMode (RNA, RNE, RTN, RTP, RTZ),
    ValidFP,
  )
import Grisette.Internal.SymPrim.Prim.Internal.IsZero
  ( IsZero,
    IsZeroCases (IsZeroEvidence, NonZeroEvidence),
    KnownIsZero (isZero),
  )
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( IsSymbolKind (decideSymbolKind),
    NonFuncSBVRep (NonFuncSBVBaseType),
    SBVRep
      ( SBVType
      ),
    SupportedNonFuncPrim (conNonFuncSBVTerm, symNonFuncSBVTerm, withNonFuncPrim),
    SupportedPrim
      ( castTypedSymbol,
        conSBVTerm,
        defaultValue,
        defaultValueDynamic,
        funcDummyConstraint,
        isFuncType,
        parseSMTModelResult,
        pevalEqTerm,
        pevalITETerm,
        pformatCon,
        sbvIte,
        symSBVName,
        symSBVTerm,
        withPrim
      ),
    SupportedPrimConstraint
      ( PrimConstraint
      ),
    Term (ConTerm),
    TypedSymbol (TypedSymbol),
    conTerm,
    eqTerm,
    parseScalarSMTModelResult,
    pevalDefaultEqTerm,
    pevalITEBasicTerm,
    sbvFresh,
  )
import Grisette.Internal.SymPrim.Prim.ModelValue (ModelValue, toModelValue)
import Grisette.Internal.Utils.Parameterized (unsafeAxiom)

defaultValueForInteger :: Integer
defaultValueForInteger = 0

defaultValueForIntegerDyn :: ModelValue
defaultValueForIntegerDyn = toModelValue defaultValueForInteger

-- Basic Integer
instance SBVRep Integer where
  type SBVType n Integer = SBV.SBV (If (IsZero n) (Integer) (SBV.IntN n))

instance SupportedPrimConstraint Integer

instance SupportedPrim Integer where
  pformatCon = show
  defaultValue = defaultValueForInteger
  defaultValueDynamic _ = defaultValueForIntegerDyn
  pevalITETerm = pevalITEBasicTerm
  pevalEqTerm = pevalDefaultEqTerm
  conSBVTerm p n = case isZero p of
    IsZeroEvidence -> fromInteger n
    NonZeroEvidence -> fromInteger n
  symSBVName symbol _ = show symbol
  symSBVTerm p name = case isZero p of
    IsZeroEvidence -> sbvFresh name
    NonZeroEvidence -> sbvFresh name
  withPrim p r = case isZero p of
    IsZeroEvidence -> r
    NonZeroEvidence -> r
  parseSMTModelResult _ = parseScalarSMTModelResult id
  castTypedSymbol ::
    forall knd knd'.
    (IsSymbolKind knd') =>
    TypedSymbol knd Integer ->
    Maybe (TypedSymbol knd' Integer)
  castTypedSymbol (TypedSymbol s) =
    case decideSymbolKind @knd' of
      Left HRefl -> Just $ TypedSymbol s
      Right HRefl -> Just $ TypedSymbol s
  isFuncType = False
  funcDummyConstraint _ _ = SBV.sTrue

instance NonFuncSBVRep Integer where
  type NonFuncSBVBaseType n Integer = If (IsZero n) Integer (SBV.IntN n)

instance SupportedNonFuncPrim Integer where
  conNonFuncSBVTerm = conSBVTerm
  symNonFuncSBVTerm = symSBVTerm @Integer
  withNonFuncPrim p r = case isZero p of
    IsZeroEvidence -> r
    NonZeroEvidence -> bvIsNonZeroFromGEq1 p r

-- Signed BV
instance (KnownNat w, 1 <= w) => SupportedPrimConstraint (IntN w) where
  type PrimConstraint _ (IntN w) = (KnownNat w, 1 <= w, BVIsNonZero w)

instance (KnownNat w, 1 <= w) => SBVRep (IntN w) where
  type SBVType _ (IntN w) = SBV.SBV (SBV.IntN w)

instance (KnownNat w, 1 <= w) => SupportedPrim (IntN w) where
  pformatCon = show
  defaultValue = 0
  pevalITETerm = pevalITEBasicTerm
  pevalEqTerm = pevalDefaultEqTerm
  conSBVTerm _ n = bvIsNonZeroFromGEq1 (Proxy @w) $ fromIntegral n
  symSBVName symbol _ = show symbol
  symSBVTerm _ name = bvIsNonZeroFromGEq1 (Proxy @w) $ sbvFresh name
  withPrim _ r = bvIsNonZeroFromGEq1 (Proxy @w) r
  parseSMTModelResult _ cv =
    withPrim @(IntN w) (Proxy @0) $
      parseScalarSMTModelResult (\(x :: SBV.IntN w) -> fromIntegral x) cv
  castTypedSymbol ::
    forall knd knd'.
    (IsSymbolKind knd') =>
    TypedSymbol knd (IntN w) ->
    Maybe (TypedSymbol knd' (IntN w))
  castTypedSymbol (TypedSymbol s) =
    case decideSymbolKind @knd' of
      Left HRefl -> Just $ TypedSymbol s
      Right HRefl -> Just $ TypedSymbol s
  isFuncType = False
  funcDummyConstraint _ _ = SBV.sTrue

bvIsNonZeroFromGEq1 ::
  forall w r proxy.
  (1 <= w) =>
  proxy w ->
  ((SBV.BVIsNonZero w) => r) ->
  r
bvIsNonZeroFromGEq1 _ r1 = case unsafeAxiom :: w :~: 1 of
  Refl -> r1

instance (KnownNat w, 1 <= w) => NonFuncSBVRep (IntN w) where
  type NonFuncSBVBaseType _ (IntN w) = SBV.IntN w

instance (KnownNat w, 1 <= w) => SupportedNonFuncPrim (IntN w) where
  conNonFuncSBVTerm = conSBVTerm
  symNonFuncSBVTerm = symSBVTerm @(IntN w)
  withNonFuncPrim _ r = bvIsNonZeroFromGEq1 (Proxy @w) r

-- Unsigned BV
instance (KnownNat w, 1 <= w) => SupportedPrimConstraint (WordN w) where
  type PrimConstraint _ (WordN w) = (KnownNat w, 1 <= w, BVIsNonZero w)

instance (KnownNat w, 1 <= w) => SBVRep (WordN w) where
  type SBVType _ (WordN w) = SBV.SBV (SBV.WordN w)

instance (KnownNat w, 1 <= w) => SupportedPrim (WordN w) where
  pformatCon = show
  defaultValue = 0
  pevalITETerm = pevalITEBasicTerm
  pevalEqTerm = pevalDefaultEqTerm
  conSBVTerm _ n = bvIsNonZeroFromGEq1 (Proxy @w) $ fromIntegral n
  symSBVName symbol _ = show symbol
  symSBVTerm _ name = bvIsNonZeroFromGEq1 (Proxy @w) $ sbvFresh name
  withPrim _ r = bvIsNonZeroFromGEq1 (Proxy @w) r
  parseSMTModelResult _ cv =
    withPrim @(IntN w) (Proxy @0) $
      parseScalarSMTModelResult (\(x :: SBV.WordN w) -> fromIntegral x) cv
  castTypedSymbol ::
    forall knd knd'.
    (IsSymbolKind knd') =>
    TypedSymbol knd (WordN w) ->
    Maybe (TypedSymbol knd' (WordN w))
  castTypedSymbol (TypedSymbol s) =
    case decideSymbolKind @knd' of
      Left HRefl -> Just $ TypedSymbol s
      Right HRefl -> Just $ TypedSymbol s
  isFuncType = False
  funcDummyConstraint _ _ = SBV.sTrue

instance (KnownNat w, 1 <= w) => NonFuncSBVRep (WordN w) where
  type NonFuncSBVBaseType _ (WordN w) = SBV.WordN w

instance (KnownNat w, 1 <= w) => SupportedNonFuncPrim (WordN w) where
  conNonFuncSBVTerm = conSBVTerm
  symNonFuncSBVTerm = symSBVTerm @(WordN w)
  withNonFuncPrim _ r = bvIsNonZeroFromGEq1 (Proxy @w) r

-- FP
instance (ValidFP eb sb) => SupportedPrimConstraint (FP eb sb) where
  type PrimConstraint _ (FP eb sb) = ValidFP eb sb

instance (ValidFP eb sb) => SBVRep (FP eb sb) where
  type SBVType _ (FP eb sb) = SBV.SBV (SBV.FloatingPoint eb sb)

instance (ValidFP eb sb) => SupportedPrim (FP eb sb) where
  defaultValue = 0
  pevalITETerm = pevalITEBasicTerm
  pevalEqTerm (ConTerm _ l) (ConTerm _ r) = conTerm $ l == r
  pevalEqTerm l@ConTerm {} r = pevalEqTerm r l
  pevalEqTerm l r = eqTerm l r
  conSBVTerm _ (FP fp) = SBV.literal fp
  symSBVName symbol _ = show symbol
  symSBVTerm _ name = sbvFresh name
  withPrim _ r = r
  parseSMTModelResult _ cv =
    withPrim @(FP eb sb) (Proxy @0) $
      parseScalarSMTModelResult (\(x :: SBV.FloatingPoint eb sb) -> coerce x) cv
  isFuncType = False
  funcDummyConstraint _ _ = SBV.sTrue

  -- Workaround for sbv#702.
  sbvIte p = withPrim @(FP eb sb) p $ \c a b ->
    case (SBV.unliteral a, SBV.unliteral b) of
      (Just a', Just b')
        | isInfinite a' && isInfinite b' ->
            let correspondingZero x = if x > 0 then 0 else -0
             in 1
                  / sbvIte @(FP eb sb)
                    p
                    c
                    (conSBVTerm @(FP eb sb) p $ correspondingZero a')
                    (conSBVTerm @(FP eb sb) p $ correspondingZero b')
      _ -> SBV.ite c a b
  castTypedSymbol ::
    forall knd knd'.
    (IsSymbolKind knd') =>
    TypedSymbol knd (FP eb sb) ->
    Maybe (TypedSymbol knd' (FP eb sb))
  castTypedSymbol (TypedSymbol s) =
    case decideSymbolKind @knd' of
      Left HRefl -> Just $ TypedSymbol s
      Right HRefl -> Just $ TypedSymbol s

instance (ValidFP eb sb) => NonFuncSBVRep (FP eb sb) where
  type NonFuncSBVBaseType _ (FP eb sb) = SBV.FloatingPoint eb sb

instance (ValidFP eb sb) => SupportedNonFuncPrim (FP eb sb) where
  conNonFuncSBVTerm = conSBVTerm
  symNonFuncSBVTerm = symSBVTerm @(FP eb sb)
  withNonFuncPrim _ r = r

-- FPRoundingMode
instance SupportedPrimConstraint FPRoundingMode

instance SBVRep FPRoundingMode where
  type SBVType _ FPRoundingMode = SBV.SBV SBV.RoundingMode

instance SupportedPrim FPRoundingMode where
  defaultValue = RNE
  pevalITETerm = pevalITEBasicTerm
  pevalEqTerm (ConTerm _ l) (ConTerm _ r) = conTerm $ l == r
  pevalEqTerm l@ConTerm {} r = pevalEqTerm r l
  pevalEqTerm l r = eqTerm l r
  conSBVTerm _ RNE = SBV.sRNE
  conSBVTerm _ RNA = SBV.sRNA
  conSBVTerm _ RTP = SBV.sRTP
  conSBVTerm _ RTN = SBV.sRTN
  conSBVTerm _ RTZ = SBV.sRTZ
  symSBVName symbol _ = show symbol
  symSBVTerm _ name = sbvFresh name
  withPrim _ r = r
  parseSMTModelResult _ cv =
    withPrim @(FPRoundingMode) (Proxy @0) $
      parseScalarSMTModelResult
        ( \(x :: SBV.RoundingMode) -> case x of
            SBV.RoundNearestTiesToEven -> RNE
            SBV.RoundNearestTiesToAway -> RNA
            SBV.RoundTowardPositive -> RTP
            SBV.RoundTowardNegative -> RTN
            SBV.RoundTowardZero -> RTZ
        )
        cv
  castTypedSymbol ::
    forall knd knd'.
    (IsSymbolKind knd') =>
    TypedSymbol knd FPRoundingMode ->
    Maybe (TypedSymbol knd' FPRoundingMode)
  castTypedSymbol (TypedSymbol s) =
    case decideSymbolKind @knd' of
      Left HRefl -> Just $ TypedSymbol s
      Right HRefl -> Just $ TypedSymbol s
  isFuncType = False
  funcDummyConstraint _ _ = SBV.sTrue

instance NonFuncSBVRep FPRoundingMode where
  type NonFuncSBVBaseType _ FPRoundingMode = SBV.RoundingMode

instance SupportedNonFuncPrim FPRoundingMode where
  conNonFuncSBVTerm = conSBVTerm
  symNonFuncSBVTerm = symSBVTerm @FPRoundingMode
  withNonFuncPrim _ r = r

-- AlgReal

instance SupportedPrimConstraint AlgReal

instance SBVRep AlgReal where
  type SBVType _ AlgReal = SBV.SBV SBV.AlgReal

instance SupportedPrim AlgReal where
  defaultValue = 0
  pevalITETerm = pevalITEBasicTerm
  pevalEqTerm (ConTerm _ l) (ConTerm _ r) = conTerm $ l == r
  pevalEqTerm l@ConTerm {} r = pevalEqTerm r l
  pevalEqTerm l r = eqTerm l r
  conSBVTerm _ = SBV.literal . toSBVAlgReal
  symSBVName symbol _ = show symbol
  symSBVTerm _ name = sbvFresh name
  withPrim _ r = r
  parseSMTModelResult _ cv =
    withPrim @(AlgReal) (Proxy @0) $
      parseScalarSMTModelResult fromSBVAlgReal cv
  castTypedSymbol ::
    forall knd knd'.
    (IsSymbolKind knd') =>
    TypedSymbol knd AlgReal ->
    Maybe (TypedSymbol knd' AlgReal)
  castTypedSymbol (TypedSymbol s) =
    case decideSymbolKind @knd' of
      Left HRefl -> Just $ TypedSymbol s
      Right HRefl -> Just $ TypedSymbol s
  isFuncType = False
  funcDummyConstraint _ _ = SBV.sTrue

instance NonFuncSBVRep AlgReal where
  type NonFuncSBVBaseType _ AlgReal = SBV.AlgReal

instance SupportedNonFuncPrim AlgReal where
  conNonFuncSBVTerm = conSBVTerm
  symNonFuncSBVTerm = symSBVTerm @AlgReal
  withNonFuncPrim _ r = r
