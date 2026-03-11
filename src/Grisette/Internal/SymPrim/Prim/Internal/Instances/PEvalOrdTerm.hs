{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
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
import qualified Data.SBV as SBV
import GHC.TypeNats (KnownNat, type (<=))
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP
  ( FP,
    ValidFP,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalNumTerm ()
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( PEvalNumTerm (pevalNegNumTerm),
    PEvalOrdTerm
      ( pevalLeOrdTerm,
        pevalLtOrdTerm,
        sbvLeOrdTerm,
        withSbvOrdTermConstraint
      ),
    SupportedPrim (withPrim),
    Term,
    conTerm,
    leOrdTerm,
    ltOrdTerm,
    pevalSubNumTerm,
    pattern AddNumTerm,
    pattern ConTerm,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Unfold (binaryUnfoldOnce)

-- | General partially evaluation of less than operation.
pevalGeneralLtOrdTerm :: (PEvalOrdTerm a, Ord a) => Term a -> Term a -> Term Bool
pevalGeneralLtOrdTerm = binaryUnfoldOnce doPevalGeneralLtOrdTerm ltOrdTerm

doPevalGeneralLtOrdTerm ::
  (PEvalOrdTerm a, Ord a) => Term a -> Term a -> Maybe (Term Bool)
doPevalGeneralLtOrdTerm (ConTerm a) (ConTerm b) = Just $ conTerm $ a < b
doPevalGeneralLtOrdTerm _ _ = Nothing

-- | General partially evaluation of less than or equal to operation.
pevalGeneralLeOrdTerm :: (PEvalOrdTerm a, Ord a) => Term a -> Term a -> Term Bool
pevalGeneralLeOrdTerm = binaryUnfoldOnce doPevalGeneralLeOrdTerm leOrdTerm

doPevalGeneralLeOrdTerm ::
  (PEvalOrdTerm a, Ord a) => Term a -> Term a -> Maybe (Term Bool)
doPevalGeneralLeOrdTerm (ConTerm a) (ConTerm b) = Just $ conTerm $ a <= b
doPevalGeneralLeOrdTerm _ _ = Nothing

instance PEvalOrdTerm Integer where
  pevalLtOrdTerm = binaryUnfoldOnce doPevalLtOrdTerm ltOrdTerm
    where
      doPevalLtOrdTerm l r =
        msum
          [ doPevalGeneralLtOrdTerm l r,
            case (l, r) of
              (ConTerm l, AddNumTerm (ConTerm j) k) ->
                Just $ pevalLtOrdTerm (conTerm $ l - j) k
              (AddNumTerm (ConTerm i) j, ConTerm k) ->
                Just $ pevalLtOrdTerm j (conTerm $ k - i)
              ((AddNumTerm (ConTerm j) k), l) ->
                Just $
                  pevalLtOrdTerm
                    (conTerm j)
                    (pevalSubNumTerm l k)
              (j, (AddNumTerm (ConTerm k) l)) ->
                Just $ pevalLtOrdTerm (conTerm $ -k) (pevalSubNumTerm l j)
              (l, ConTerm r) ->
                Just $ pevalLtOrdTerm (conTerm $ -r) (pevalNegNumTerm l)
              _ -> Nothing
          ]
  pevalLeOrdTerm = binaryUnfoldOnce doPevalLeOrdTerm leOrdTerm
    where
      doPevalLeOrdTerm l r =
        msum
          [ doPevalGeneralLeOrdTerm l r,
            case (l, r) of
              (ConTerm l, AddNumTerm (ConTerm j) k) ->
                Just $ pevalLeOrdTerm (conTerm $ l - j) k
              (AddNumTerm (ConTerm i) j, ConTerm k) ->
                Just $ pevalLeOrdTerm j (conTerm $ k - i)
              (AddNumTerm (ConTerm j) k, l) ->
                Just $ pevalLeOrdTerm (conTerm j) (pevalSubNumTerm l k)
              (j, AddNumTerm (ConTerm k) l) ->
                Just $ pevalLeOrdTerm (conTerm $ -k) (pevalSubNumTerm l j)
              (l, ConTerm r) ->
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

instance PEvalOrdTerm AlgReal where
  pevalLtOrdTerm = pevalGeneralLtOrdTerm
  pevalLeOrdTerm = pevalGeneralLeOrdTerm
  withSbvOrdTermConstraint r = withPrim @AlgReal r
