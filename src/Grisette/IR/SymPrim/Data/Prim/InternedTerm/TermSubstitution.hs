{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.Prim.InternedTerm.TermSubstitution
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.Prim.InternedTerm.TermSubstitution
  ( substTerm,
  )
where

import Grisette.Core.Data.MemoUtils (htmemo)
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
  ( conTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.SomeTerm
  ( SomeTerm (SomeTerm),
  )
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
  ( BinaryOp (partialEvalBinary),
    SupportedPrim,
    Term
      ( AbsNumTerm,
        AddNumTerm,
        AndBitsTerm,
        AndTerm,
        BVConcatTerm,
        BVExtendTerm,
        BVSelectTerm,
        BinaryTerm,
        ComplementBitsTerm,
        ConTerm,
        DivBoundedIntegralTerm,
        DivIntegralTerm,
        EqvTerm,
        GeneralFunApplyTerm,
        ITETerm,
        LENumTerm,
        LTNumTerm,
        ModBoundedIntegralTerm,
        ModIntegralTerm,
        NotTerm,
        OrBitsTerm,
        OrTerm,
        QuotBoundedIntegralTerm,
        QuotIntegralTerm,
        RemBoundedIntegralTerm,
        RemIntegralTerm,
        RotateLeftTerm,
        RotateRightTerm,
        ShiftLeftTerm,
        ShiftRightTerm,
        SignumNumTerm,
        SymTerm,
        TabularFunApplyTerm,
        TernaryTerm,
        TimesNumTerm,
        ToSignedTerm,
        ToUnsignedTerm,
        UMinusNumTerm,
        UnaryTerm,
        XorBitsTerm
      ),
    TernaryOp (partialEvalTernary),
    TypedSymbol,
    UnaryOp (partialEvalUnary),
    someTypedSymbol,
    type (-->) (GeneralFun),
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.BV
  ( pevalBVConcatTerm,
    pevalBVExtendTerm,
    pevalBVSelectTerm,
    pevalToSignedTerm,
    pevalToUnsignedTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bits
  ( pevalAndBitsTerm,
    pevalComplementBitsTerm,
    pevalOrBitsTerm,
    pevalRotateLeftTerm,
    pevalRotateRightTerm,
    pevalShiftLeftTerm,
    pevalShiftRightTerm,
    pevalXorBitsTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bool
  ( pevalAndTerm,
    pevalEqvTerm,
    pevalITETerm,
    pevalNotTerm,
    pevalOrTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.GeneralFun
  ( pevalGeneralFunApplyTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Integral
  ( pevalDivBoundedIntegralTerm,
    pevalDivIntegralTerm,
    pevalModBoundedIntegralTerm,
    pevalModIntegralTerm,
    pevalQuotBoundedIntegralTerm,
    pevalQuotIntegralTerm,
    pevalRemBoundedIntegralTerm,
    pevalRemIntegralTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Num
  ( pevalAbsNumTerm,
    pevalAddNumTerm,
    pevalLeNumTerm,
    pevalLtNumTerm,
    pevalSignumNumTerm,
    pevalTimesNumTerm,
    pevalUMinusNumTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.TabularFun
  ( pevalTabularFunApplyTerm,
  )
import Type.Reflection
  ( TypeRep,
    eqTypeRep,
    typeRep,
    pattern App,
    type (:~~:) (HRefl),
  )
import Unsafe.Coerce (unsafeCoerce)

substTerm :: forall a b. (SupportedPrim a, SupportedPrim b) => TypedSymbol a -> Term a -> Term b -> Term b
substTerm sym term = gov
  where
    gov :: (SupportedPrim x) => Term x -> Term x
    gov b = case go (SomeTerm b) of
      SomeTerm v -> unsafeCoerce v
    go :: SomeTerm -> SomeTerm
    go = htmemo $ \stm@(SomeTerm (tm :: Term v)) ->
      case tm of
        ConTerm _ cv -> case (typeRep :: TypeRep v) of
          App (App gf _) _ ->
            case eqTypeRep gf (typeRep @(-->)) of
              Just HRefl -> case cv of
                GeneralFun sym1 tm1 ->
                  if someTypedSymbol sym1 == someTypedSymbol sym
                    then stm
                    else SomeTerm $ conTerm $ GeneralFun sym1 (gov tm1)
              Nothing -> stm
          _ -> stm
        SymTerm _ ts -> SomeTerm $ if someTypedSymbol ts == someTypedSymbol sym then unsafeCoerce term else tm
        UnaryTerm _ tag te -> SomeTerm $ partialEvalUnary tag (gov te)
        BinaryTerm _ tag te te' -> SomeTerm $ partialEvalBinary tag (gov te) (gov te')
        TernaryTerm _ tag op1 op2 op3 -> SomeTerm $ partialEvalTernary tag (gov op1) (gov op2) (gov op3)
        NotTerm _ op -> SomeTerm $ pevalNotTerm (gov op)
        OrTerm _ op1 op2 -> SomeTerm $ pevalOrTerm (gov op1) (gov op2)
        AndTerm _ op1 op2 -> SomeTerm $ pevalAndTerm (gov op1) (gov op2)
        EqvTerm _ op1 op2 -> SomeTerm $ pevalEqvTerm (gov op1) (gov op2)
        ITETerm _ c op1 op2 -> SomeTerm $ pevalITETerm (gov c) (gov op1) (gov op2)
        AddNumTerm _ op1 op2 -> SomeTerm $ pevalAddNumTerm (gov op1) (gov op2)
        UMinusNumTerm _ op -> SomeTerm $ pevalUMinusNumTerm (gov op)
        TimesNumTerm _ op1 op2 -> SomeTerm $ pevalTimesNumTerm (gov op1) (gov op2)
        AbsNumTerm _ op -> SomeTerm $ pevalAbsNumTerm (gov op)
        SignumNumTerm _ op -> SomeTerm $ pevalSignumNumTerm (gov op)
        LTNumTerm _ op1 op2 -> SomeTerm $ pevalLtNumTerm (gov op1) (gov op2)
        LENumTerm _ op1 op2 -> SomeTerm $ pevalLeNumTerm (gov op1) (gov op2)
        AndBitsTerm _ op1 op2 -> SomeTerm $ pevalAndBitsTerm (gov op1) (gov op2)
        OrBitsTerm _ op1 op2 -> SomeTerm $ pevalOrBitsTerm (gov op1) (gov op2)
        XorBitsTerm _ op1 op2 -> SomeTerm $ pevalXorBitsTerm (gov op1) (gov op2)
        ComplementBitsTerm _ op -> SomeTerm $ pevalComplementBitsTerm (gov op)
        ShiftLeftTerm _ op n -> SomeTerm $ pevalShiftLeftTerm (gov op) (gov n)
        RotateLeftTerm _ op n -> SomeTerm $ pevalRotateLeftTerm (gov op) (gov n)
        ShiftRightTerm _ op n -> SomeTerm $ pevalShiftRightTerm (gov op) (gov n)
        RotateRightTerm _ op n -> SomeTerm $ pevalRotateRightTerm (gov op) (gov n)
        ToSignedTerm _ op -> SomeTerm $ pevalToSignedTerm op
        ToUnsignedTerm _ op -> SomeTerm $ pevalToUnsignedTerm op
        BVConcatTerm _ op1 op2 -> SomeTerm $ pevalBVConcatTerm (gov op1) (gov op2)
        BVSelectTerm _ ix w op -> SomeTerm $ pevalBVSelectTerm ix w (gov op)
        BVExtendTerm _ n signed op -> SomeTerm $ pevalBVExtendTerm n signed (gov op)
        TabularFunApplyTerm _ f op -> SomeTerm $ pevalTabularFunApplyTerm (gov f) (gov op)
        GeneralFunApplyTerm _ f op -> SomeTerm $ pevalGeneralFunApplyTerm (gov f) (gov op)
        DivIntegralTerm _ op1 op2 -> SomeTerm $ pevalDivIntegralTerm (gov op1) (gov op2)
        ModIntegralTerm _ op1 op2 -> SomeTerm $ pevalModIntegralTerm (gov op1) (gov op2)
        QuotIntegralTerm _ op1 op2 -> SomeTerm $ pevalQuotIntegralTerm (gov op1) (gov op2)
        RemIntegralTerm _ op1 op2 -> SomeTerm $ pevalRemIntegralTerm (gov op1) (gov op2)
        DivBoundedIntegralTerm _ op1 op2 -> SomeTerm $ pevalDivBoundedIntegralTerm (gov op1) (gov op2)
        ModBoundedIntegralTerm _ op1 op2 -> SomeTerm $ pevalModBoundedIntegralTerm (gov op1) (gov op2)
        QuotBoundedIntegralTerm _ op1 op2 -> SomeTerm $ pevalQuotBoundedIntegralTerm (gov op1) (gov op2)
        RemBoundedIntegralTerm _ op1 op2 -> SomeTerm $ pevalRemBoundedIntegralTerm (gov op1) (gov op2)
