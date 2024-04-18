{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.Prim.Internal.Instances.SupportedPrim
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.Prim.Internal.Instances.SupportedPrim
  ( bvIsNonZeroFromGEq1,
  )
where

import Data.Proxy (Proxy (Proxy))
import Data.SBV (BVIsNonZero)
import qualified Data.SBV as SBV
import Data.Type.Bool (If)
import Data.Type.Equality ((:~:) (Refl))
import GHC.TypeNats (KnownNat, type (<=))
import Grisette.Core.Data.BV (IntN, WordN)
import Grisette.IR.SymPrim.Data.Prim.Internal.IsZero
  ( IsZero,
    IsZeroCases (IsZeroEvidence, NonZeroEvidence),
    KnownIsZero (isZero),
  )
import Grisette.IR.SymPrim.Data.Prim.Internal.Term
  ( NonFuncSBVRep (NonFuncSBVBaseType),
    SBVRep
      ( SBVType
      ),
    SupportedNonFuncPrim (conNonFuncSBVTerm, symNonFuncSBVTerm, withNonFuncPrim),
    SupportedPrim
      ( conSBVTerm,
        defaultValue,
        defaultValueDynamic,
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
    pevalDefaultEqTerm,
    pevalITEBasicTerm,
    sbvFresh,
  )
import Grisette.IR.SymPrim.Data.Prim.ModelValue (ModelValue, toModelValue)
import Grisette.Utils.Parameterized (unsafeAxiom)

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

instance NonFuncSBVRep Integer where
  type NonFuncSBVBaseType n Integer = If (IsZero n) Integer (SBV.IntN n)

instance SupportedNonFuncPrim Integer where
  conNonFuncSBVTerm = conSBVTerm
  symNonFuncSBVTerm = symSBVTerm @Integer
  withNonFuncPrim p r = case isZero p of
    IsZeroEvidence -> r
    NonZeroEvidence -> r

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

instance (KnownNat w, 1 <= w) => NonFuncSBVRep (WordN w) where
  type NonFuncSBVBaseType _ (WordN w) = SBV.WordN w

instance (KnownNat w, 1 <= w) => SupportedNonFuncPrim (WordN w) where
  conNonFuncSBVTerm = conSBVTerm
  symNonFuncSBVTerm = symSBVTerm @(WordN w)
  withNonFuncPrim _ r = bvIsNonZeroFromGEq1 (Proxy @w) r
