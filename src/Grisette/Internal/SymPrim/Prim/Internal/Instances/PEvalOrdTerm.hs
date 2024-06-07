{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalOrdTerm
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalOrdTerm
  ( pevalGeneralLtOrdTerm,
    pevalGeneralLeOrdTerm,
  )
where

import Control.Monad (msum)
import Data.Foldable (Foldable (foldl'))
import Data.Proxy (Proxy (Proxy))
import qualified Data.SBV as SBV
import GHC.TypeNats (KnownNat, type (<=))
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP (FP, FPRoundingMode, ValidFP, allFPRoundingMode)
import Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalNumTerm ()
import Grisette.Internal.SymPrim.Prim.Internal.IsZero
  ( IsZeroCases (IsZeroEvidence, NonZeroEvidence),
    KnownIsZero (isZero),
  )
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( PEvalNumTerm (pevalNegNumTerm),
    PEvalOrdTerm
      ( pevalLeOrdTerm,
        pevalLtOrdTerm,
        sbvLeOrdTerm,
        sbvLtOrdTerm,
        withSbvOrdTermConstraint
      ),
    SupportedPrim (conSBVTerm, withPrim),
    Term (AddNumTerm, ConTerm),
    conTerm,
    leOrdTerm,
    ltOrdTerm,
    pevalSubNumTerm,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Unfold (binaryUnfoldOnce)

-- Lt
pevalGeneralLtOrdTerm :: (PEvalOrdTerm a) => Term a -> Term a -> Term Bool
pevalGeneralLtOrdTerm = binaryUnfoldOnce doPevalGeneralLtOrdTerm ltOrdTerm

doPevalGeneralLtOrdTerm ::
  (PEvalOrdTerm a) => Term a -> Term a -> Maybe (Term Bool)
doPevalGeneralLtOrdTerm (ConTerm _ a) (ConTerm _ b) = Just $ conTerm $ a < b
doPevalGeneralLtOrdTerm _ _ = Nothing

-- Le
pevalGeneralLeOrdTerm :: (PEvalOrdTerm a) => Term a -> Term a -> Term Bool
pevalGeneralLeOrdTerm = binaryUnfoldOnce doPevalGeneralLeOrdTerm leOrdTerm

doPevalGeneralLeOrdTerm ::
  (PEvalOrdTerm a) => Term a -> Term a -> Maybe (Term Bool)
doPevalGeneralLeOrdTerm (ConTerm _ a) (ConTerm _ b) = Just $ conTerm $ a <= b
doPevalGeneralLeOrdTerm _ _ = Nothing

instance PEvalOrdTerm Integer where
  pevalLtOrdTerm = binaryUnfoldOnce doPevalLtOrdTerm ltOrdTerm
    where
      doPevalLtOrdTerm l r =
        msum
          [ doPevalGeneralLtOrdTerm l r,
            case (l, r) of
              (ConTerm _ l, AddNumTerm _ (ConTerm _ j) k) ->
                Just $ pevalLtOrdTerm (conTerm $ l - j) k
              (AddNumTerm _ (ConTerm _ i) j, ConTerm _ k) ->
                Just $ pevalLtOrdTerm j (conTerm $ k - i)
              ((AddNumTerm _ (ConTerm _ j) k), l) ->
                Just $
                  pevalLtOrdTerm
                    (conTerm j)
                    (pevalSubNumTerm l k)
              (j, (AddNumTerm _ (ConTerm _ k) l)) ->
                Just $ pevalLtOrdTerm (conTerm $ -k) (pevalSubNumTerm l j)
              (l, ConTerm _ r) ->
                Just $ pevalLtOrdTerm (conTerm $ -r) (pevalNegNumTerm l)
              _ -> Nothing
          ]
  pevalLeOrdTerm = binaryUnfoldOnce doPevalLeOrdTerm leOrdTerm
    where
      doPevalLeOrdTerm l r =
        msum
          [ doPevalGeneralLeOrdTerm l r,
            case (l, r) of
              (ConTerm _ l, AddNumTerm _ (ConTerm _ j) k) ->
                Just $ pevalLeOrdTerm (conTerm $ l - j) k
              (AddNumTerm _ (ConTerm _ i) j, ConTerm _ k) ->
                Just $ pevalLeOrdTerm j (conTerm $ k - i)
              (AddNumTerm _ (ConTerm _ j) k, l) ->
                Just $ pevalLeOrdTerm (conTerm j) (pevalSubNumTerm l k)
              (j, AddNumTerm _ (ConTerm _ k) l) ->
                Just $ pevalLeOrdTerm (conTerm $ -k) (pevalSubNumTerm l j)
              (l, ConTerm _ r) ->
                Just $ pevalLeOrdTerm (conTerm $ -r) (pevalNegNumTerm l)
              _ -> Nothing
          ]
  withSbvOrdTermConstraint p r = case isZero p of
    IsZeroEvidence -> r
    NonZeroEvidence -> r

instance (KnownNat n, 1 <= n) => PEvalOrdTerm (WordN n) where
  pevalLtOrdTerm = pevalGeneralLtOrdTerm
  pevalLeOrdTerm = pevalGeneralLeOrdTerm
  withSbvOrdTermConstraint p r = withPrim @(WordN n) p r

instance (KnownNat n, 1 <= n) => PEvalOrdTerm (IntN n) where
  pevalLtOrdTerm = pevalGeneralLtOrdTerm
  pevalLeOrdTerm = pevalGeneralLeOrdTerm
  withSbvOrdTermConstraint p r = withPrim @(IntN n) p r

instance (ValidFP eb sb) => PEvalOrdTerm (FP eb sb) where
  pevalLtOrdTerm = pevalGeneralLtOrdTerm
  pevalLeOrdTerm = pevalGeneralLeOrdTerm
  withSbvOrdTermConstraint p r = withPrim @(FP eb sb) p r
  sbvLeOrdTerm _ x y =
    (SBV.sNot (SBV.fpIsNaN x) SBV..&& SBV.sNot (SBV.fpIsNaN y))
      SBV..&& (x SBV..<= y)

-- Use this table to avoid accidental breakage introduced by sbv.
fpRoundingModeLtTable :: [(SBV.SRoundingMode, SBV.SRoundingMode)]
fpRoundingModeLtTable =
  [ ( conSBVTerm @FPRoundingMode (Proxy @0) a,
      conSBVTerm @FPRoundingMode (Proxy @0) b
    )
    | a <- allFPRoundingMode,
      b <- allFPRoundingMode,
      a < b
  ]

fpRoundingModeLeTable :: [(SBV.SRoundingMode, SBV.SRoundingMode)]
fpRoundingModeLeTable =
  [ ( conSBVTerm @FPRoundingMode (Proxy @0) a,
      conSBVTerm @FPRoundingMode (Proxy @0) b
    )
    | a <- allFPRoundingMode,
      b <- allFPRoundingMode,
      a <= b
  ]

sbvTableLookup ::
  [(SBV.SRoundingMode, SBV.SRoundingMode)] ->
  SBV.SRoundingMode ->
  SBV.SRoundingMode ->
  SBV.SBV Bool
sbvTableLookup tbl lhs rhs =
  foldl'
    (\acc (a, b) -> acc SBV..|| ((lhs SBV..== a) SBV..&& (rhs SBV..== b)))
    SBV.sFalse
    tbl

instance PEvalOrdTerm FPRoundingMode where
  pevalLtOrdTerm = pevalGeneralLtOrdTerm
  pevalLeOrdTerm = pevalGeneralLeOrdTerm
  withSbvOrdTermConstraint p r = withPrim @FPRoundingMode p r
  sbvLtOrdTerm _ = sbvTableLookup fpRoundingModeLtTable
  sbvLeOrdTerm _ = sbvTableLookup fpRoundingModeLeTable
