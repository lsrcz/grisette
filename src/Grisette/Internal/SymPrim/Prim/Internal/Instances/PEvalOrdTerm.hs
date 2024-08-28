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
import qualified Data.SBV as SBV
import GHC.TypeNats (KnownNat, type (<=))
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP
  ( FP,
    FPRoundingMode,
    ValidFP,
    allFPRoundingMode,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalNumTerm ()
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

-- | General partially evaluation of less than operation.
pevalGeneralLtOrdTerm :: (PEvalOrdTerm a) => Term a -> Term a -> Term Bool
pevalGeneralLtOrdTerm = binaryUnfoldOnce doPevalGeneralLtOrdTerm ltOrdTerm

doPevalGeneralLtOrdTerm ::
  (PEvalOrdTerm a) => Term a -> Term a -> Maybe (Term Bool)
doPevalGeneralLtOrdTerm (ConTerm _ _ a) (ConTerm _ _ b) = Just $ conTerm $ a < b
doPevalGeneralLtOrdTerm _ _ = Nothing

-- | General partially evaluation of less than or equal to operation.
pevalGeneralLeOrdTerm :: (PEvalOrdTerm a) => Term a -> Term a -> Term Bool
pevalGeneralLeOrdTerm = binaryUnfoldOnce doPevalGeneralLeOrdTerm leOrdTerm

doPevalGeneralLeOrdTerm ::
  (PEvalOrdTerm a) => Term a -> Term a -> Maybe (Term Bool)
doPevalGeneralLeOrdTerm (ConTerm _ _ a) (ConTerm _ _ b) = Just $ conTerm $ a <= b
doPevalGeneralLeOrdTerm _ _ = Nothing

instance PEvalOrdTerm Integer where
  pevalLtOrdTerm = binaryUnfoldOnce doPevalLtOrdTerm ltOrdTerm
    where
      doPevalLtOrdTerm l r =
        msum
          [ doPevalGeneralLtOrdTerm l r,
            case (l, r) of
              (ConTerm _ _ l, AddNumTerm _ _ (ConTerm _ _ j) k) ->
                Just $ pevalLtOrdTerm (conTerm $ l - j) k
              (AddNumTerm _ _ (ConTerm _ _ i) j, ConTerm _ _ k) ->
                Just $ pevalLtOrdTerm j (conTerm $ k - i)
              ((AddNumTerm _ _ (ConTerm _ _ j) k), l) ->
                Just $
                  pevalLtOrdTerm
                    (conTerm j)
                    (pevalSubNumTerm l k)
              (j, (AddNumTerm _ _ (ConTerm _ _ k) l)) ->
                Just $ pevalLtOrdTerm (conTerm $ -k) (pevalSubNumTerm l j)
              (l, ConTerm _ _ r) ->
                Just $ pevalLtOrdTerm (conTerm $ -r) (pevalNegNumTerm l)
              _ -> Nothing
          ]
  pevalLeOrdTerm = binaryUnfoldOnce doPevalLeOrdTerm leOrdTerm
    where
      doPevalLeOrdTerm l r =
        msum
          [ doPevalGeneralLeOrdTerm l r,
            case (l, r) of
              (ConTerm _ _ l, AddNumTerm _ _ (ConTerm _ _ j) k) ->
                Just $ pevalLeOrdTerm (conTerm $ l - j) k
              (AddNumTerm _ _ (ConTerm _ _ i) j, ConTerm _ _ k) ->
                Just $ pevalLeOrdTerm j (conTerm $ k - i)
              (AddNumTerm _ _ (ConTerm _ _ j) k, l) ->
                Just $ pevalLeOrdTerm (conTerm j) (pevalSubNumTerm l k)
              (j, AddNumTerm _ _ (ConTerm _ _ k) l) ->
                Just $ pevalLeOrdTerm (conTerm $ -k) (pevalSubNumTerm l j)
              (l, ConTerm _ _ r) ->
                Just $ pevalLeOrdTerm (conTerm $ -r) (pevalNegNumTerm l)
              _ -> Nothing
          ]
  withSbvOrdTermConstraint r = r

instance (KnownNat n, 1 <= n) => PEvalOrdTerm (WordN n) where
  pevalLtOrdTerm = pevalGeneralLtOrdTerm
  pevalLeOrdTerm = pevalGeneralLeOrdTerm
  withSbvOrdTermConstraint r = withPrim @(WordN n) r

instance (KnownNat n, 1 <= n) => PEvalOrdTerm (IntN n) where
  pevalLtOrdTerm = pevalGeneralLtOrdTerm
  pevalLeOrdTerm = pevalGeneralLeOrdTerm
  withSbvOrdTermConstraint r = withPrim @(IntN n) r

instance (ValidFP eb sb) => PEvalOrdTerm (FP eb sb) where
  pevalLtOrdTerm = pevalGeneralLtOrdTerm
  pevalLeOrdTerm = pevalGeneralLeOrdTerm
  withSbvOrdTermConstraint r = withPrim @(FP eb sb) r
  sbvLeOrdTerm x y =
    (SBV.sNot (SBV.fpIsNaN x) SBV..&& SBV.sNot (SBV.fpIsNaN y))
      SBV..&& (x SBV..<= y)

-- Use this table to avoid accidental breakage introduced by sbv.
fpRoundingModeLtTable :: [(SBV.SRoundingMode, SBV.SRoundingMode)]
fpRoundingModeLtTable =
  [ ( conSBVTerm @FPRoundingMode a,
      conSBVTerm @FPRoundingMode b
    )
    | a <- allFPRoundingMode,
      b <- allFPRoundingMode,
      a < b
  ]

fpRoundingModeLeTable :: [(SBV.SRoundingMode, SBV.SRoundingMode)]
fpRoundingModeLeTable =
  [ ( conSBVTerm @FPRoundingMode a,
      conSBVTerm @FPRoundingMode b
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
  withSbvOrdTermConstraint r = withPrim @FPRoundingMode r
  sbvLtOrdTerm = sbvTableLookup fpRoundingModeLtTable
  sbvLeOrdTerm = sbvTableLookup fpRoundingModeLeTable

instance PEvalOrdTerm AlgReal where
  pevalLtOrdTerm = pevalGeneralLtOrdTerm
  pevalLeOrdTerm = pevalGeneralLeOrdTerm
  withSbvOrdTermConstraint r = withPrim @AlgReal r
