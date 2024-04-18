{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.Prim.Internal.Instances.PEvalOrdTerm
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.Prim.Internal.Instances.PEvalOrdTerm
  ( pevalGeneralLtOrdTerm,
    pevalGeneralLeOrdTerm,
  )
where

import Control.Monad (msum)
import GHC.TypeNats (KnownNat, type (<=))
import Grisette.Core.Data.BV (IntN, WordN)
import Grisette.IR.SymPrim.Data.Prim.Internal.Instances.PEvalNumTerm ()
import Grisette.IR.SymPrim.Data.Prim.Internal.IsZero (IsZeroCases (IsZeroEvidence, NonZeroEvidence), KnownIsZero (isZero))
import Grisette.IR.SymPrim.Data.Prim.Internal.Term
  ( PEvalNumTerm (pevalNegNumTerm),
    PEvalOrdTerm (pevalLeOrdTerm, pevalLtOrdTerm, withSbvOrdTermConstraint),
    Term (AddNumTerm, ConTerm),
    conTerm,
    leOrdTerm,
    ltOrdTerm,
    pevalSubNumTerm, SupportedPrim (withPrim),
  )
import Grisette.IR.SymPrim.Data.Prim.Internal.Unfold (binaryUnfoldOnce)

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
