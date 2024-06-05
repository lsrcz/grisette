{-# LANGUAGE FlexibleContexts #-}

module Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalFP
  ( pevalFPTraitTerm,
    sbvFPTraitTerm,
  )
where

import qualified Data.SBV as SBV
import Grisette.Internal.SymPrim.FP (FP, ValidFP)
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( FPTrait (FPIsNaN),
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
    doPevalFPTraitTerm (ConTerm _ a) = Just $ conTerm $ case trait of
      FPIsNaN -> isNaN a
    doPevalFPTraitTerm _ = Nothing

sbvFPTraitTerm :: (ValidFP eb sb) => FPTrait -> SBV.SFloatingPoint eb sb -> SBV.SBool
sbvFPTraitTerm FPIsNaN = SBV.fpIsNaN
