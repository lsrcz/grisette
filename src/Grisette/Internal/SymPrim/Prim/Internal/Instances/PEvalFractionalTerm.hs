{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalFractionalTerm
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalFractionalTerm () where

import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.FP (FP, ValidFP)
import Grisette.Internal.SymPrim.Prim.Internal.Instances.SupportedPrim ()
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( PEvalFractionalTerm
      ( pevalFdivTerm,
        pevalRecipTerm,
        withSbvFractionalTermConstraint
      ),
    SupportedPrim (withPrim),
    Term (ConTerm),
    conTerm,
    fdivTerm,
    introSupportedPrimConstraint,
    recipTerm,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Unfold
  ( binaryUnfoldOnce,
    generalBinaryUnfolded,
    generalUnaryUnfolded,
    unaryUnfoldOnce,
  )

instance (ValidFP eb sb) => PEvalFractionalTerm (FP eb sb) where
  pevalFdivTerm = generalBinaryUnfolded (/) fdivTerm
  pevalRecipTerm = generalUnaryUnfolded recip recipTerm
  withSbvFractionalTermConstraint r = withPrim @(FP eb sb) r

pevalDefaultFdivTerm ::
  (PEvalFractionalTerm a, Eq a) => Term a -> Term a -> Term a
pevalDefaultFdivTerm l r =
  introSupportedPrimConstraint l $
    binaryUnfoldOnce doPevalDefaultFdivTerm fdivTerm l r

doPevalDefaultFdivTerm ::
  (PEvalFractionalTerm a, Eq a) => Term a -> Term a -> Maybe (Term a)
doPevalDefaultFdivTerm (ConTerm _ _ _ a) (ConTerm _ _ _ b)
  | b /= 0 = Just $ conTerm $ a / b
doPevalDefaultFdivTerm a (ConTerm _ _ _ 1) = Just a
doPevalDefaultFdivTerm _ _ = Nothing

pevalDefaultRecipTerm ::
  (PEvalFractionalTerm a, Eq a) => Term a -> Term a
pevalDefaultRecipTerm l =
  introSupportedPrimConstraint l $
    unaryUnfoldOnce doPevalDefaultRecipTerm recipTerm l

doPevalDefaultRecipTerm ::
  (PEvalFractionalTerm a, Eq a) => Term a -> Maybe (Term a)
doPevalDefaultRecipTerm (ConTerm _ _ _ n) | n /= 0 = Just $ conTerm $ recip n
doPevalDefaultRecipTerm _ = Nothing

instance PEvalFractionalTerm AlgReal where
  pevalFdivTerm = pevalDefaultFdivTerm
  pevalRecipTerm = pevalDefaultRecipTerm
  withSbvFractionalTermConstraint r = withPrim @AlgReal r
