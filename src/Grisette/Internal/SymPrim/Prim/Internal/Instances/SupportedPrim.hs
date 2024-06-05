{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE InstanceSigs #-}
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

import Data.Proxy (Proxy (Proxy))
import Data.SBV (BVIsNonZero, FiniteBits (finiteBitSize))
import qualified Data.SBV as SBV
import qualified Data.SBV.Dynamic as SBVD
import Data.Type.Bool (If)
import Data.Type.Equality ((:~:) (Refl))
import Debug.Trace (trace)
import GHC.TypeNats (KnownNat, natVal, type (<=))
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP (FP (FP), ValidFP)
import Grisette.Internal.SymPrim.Prim.Internal.IsZero
  ( IsZero,
    IsZeroCases (IsZeroEvidence, NonZeroEvidence),
    KnownIsZero (isZero),
  )
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( NonFuncSBVRep (NonFuncSBVBaseType),
    SBVRep
      ( SBVType
      ),
    SupportedNonFuncPrim (conNonFuncSBVTerm, symNonFuncSBVTerm, withNonFuncPrim),
    SupportedPrim
      ( conSBVTerm,
        defaultValue,
        defaultValueDynamic,
        parseSMTModelResult,
        pevalEqTerm,
        pevalITETerm,
        pformatCon,
        symSBVName,
        symSBVTerm,
        withPrim
      ),
    SupportedPrimConstraint
      ( PrimConstraint
      ),
    parseSMTModelResultError,
    pevalDefaultEqTerm,
    pevalITEBasicTerm,
    sbvFresh,
  )
import Grisette.Internal.SymPrim.Prim.ModelValue (ModelValue, toModelValue)
import Grisette.Internal.Utils.Parameterized (unsafeAxiom)
import Type.Reflection (typeRep)
import Unsafe.Coerce (unsafeCoerce)

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
  parseSMTModelResult :: Int -> ([([SBVD.CV], SBVD.CV)], SBVD.CV) -> Integer
  parseSMTModelResult _ ([], SBVD.CV SBVD.KUnbounded (SBVD.CInteger i)) = i
  parseSMTModelResult _ ([([], SBVD.CV SBVD.KUnbounded (SBVD.CInteger i))], _) = i
  parseSMTModelResult _ cv = trace (show cv) $ parseSMTModelResultError (typeRep @Integer) cv

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
  parseSMTModelResult :: Int -> ([([SBVD.CV], SBVD.CV)], SBVD.CV) -> IntN w
  parseSMTModelResult
    _
    ([], SBVD.CV (SBVD.KBounded _ bitWidth) (SBVD.CInteger i))
      | bitWidth == finiteBitSize (undefined :: IntN w) = fromIntegral i
  parseSMTModelResult
    _
    ([([], SBVD.CV (SBVD.KBounded _ bitWidth) (SBVD.CInteger i))], _)
      | bitWidth == finiteBitSize (undefined :: IntN w) = fromIntegral i
  parseSMTModelResult _ cv = parseSMTModelResultError (typeRep @(IntN w)) cv

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
  parseSMTModelResult
    _
    ([], SBVD.CV (SBVD.KBounded _ bitWidth) (SBVD.CInteger i))
      | bitWidth == finiteBitSize (undefined :: WordN w) = fromIntegral i
  parseSMTModelResult
    _
    ([([], SBVD.CV (SBVD.KBounded _ bitWidth) (SBVD.CInteger i))], _)
      | bitWidth == finiteBitSize (undefined :: WordN w) = fromIntegral i
  parseSMTModelResult _ cv = parseSMTModelResultError (typeRep @(WordN w)) cv

instance (ValidFP eb sb) => SupportedPrimConstraint (FP eb sb) where
  type PrimConstraint _ (FP eb sb) = ValidFP eb sb

instance (ValidFP eb sb) => SBVRep (FP eb sb) where
  type SBVType _ (FP eb sb) = SBV.SBV (SBV.FloatingPoint eb sb)

instance (ValidFP eb sb) => SupportedPrim (FP eb sb) where
  defaultValue = 0
  pevalITETerm = pevalITEBasicTerm
  pevalEqTerm = pevalDefaultEqTerm
  conSBVTerm _ (FP fp) = SBV.literal fp
  symSBVName symbol _ = show symbol
  symSBVTerm _ name = sbvFresh name
  withPrim _ r = r
  parseSMTModelResult
    _
    ([], SBVD.CV (SBVD.KFP eb sb) (SBVD.CFP fp))
      | eb == fromIntegral (natVal (Proxy @eb))
          && sb == fromIntegral (natVal (Proxy @sb)) =
          -- Assumes that in SBV, FloatingPoint is a newtype of FP as the
          -- constructor isn't exposed
          fromIntegral $ unsafeCoerce fp
  parseSMTModelResult _ cv = parseSMTModelResultError (typeRep @(FP eb sb)) cv

instance (KnownNat w, 1 <= w) => NonFuncSBVRep (WordN w) where
  type NonFuncSBVBaseType _ (WordN w) = SBV.WordN w

instance (KnownNat w, 1 <= w) => SupportedNonFuncPrim (WordN w) where
  conNonFuncSBVTerm = conSBVTerm
  symNonFuncSBVTerm = symSBVTerm @(WordN w)
  withNonFuncPrim _ r = bvIsNonZeroFromGEq1 (Proxy @w) r
