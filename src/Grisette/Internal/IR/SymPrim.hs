{-# LANGUAGE PatternSynonyms #-}
-- Disable this warning because we are re-exporting things.
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- |
-- Module      :   Grisette.Internal.IR.SymPrim
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.IR.SymPrim
  ( -- Sym (..),
    UnaryOp (..),
    BinaryOp (..),
    TernaryOp (..),
    Term (..),
    showUntyped,
    withSymbolSupported,
    SomeTypedSymbol (..),
    someTypedSymbol,
    evaluateTerm,
    introSupportedPrimConstraint,
    SomeTerm (..),
    SupportedPrim (..),
    castTerm,
    identity,
    identityWithTypeRep,
    pformat,
    constructUnary,
    constructBinary,
    constructTernary,
    conTerm,
    symTerm,
    ssymTerm,
    isymTerm,
    termSize,
    termsSize,
    extractSymbolicsTerm,
    trueTerm,
    falseTerm,
    pattern BoolConTerm,
    pattern TrueTerm,
    pattern FalseTerm,
    pattern BoolTerm,
    pevalNotTerm,
    pevalEqvTerm,
    pevalNotEqvTerm,
    pevalOrTerm,
    pevalAndTerm,
    pevalITETerm,
    pevalImplyTerm,
    pevalXorTerm,
    unaryUnfoldOnce,
    binaryUnfoldOnce,
    pattern UnaryTermPatt,
    pattern BinaryTermPatt,
    pattern TernaryTermPatt,
    PartialFun,
    PartialRuleUnary,
    TotalRuleUnary,
    PartialRuleBinary,
    TotalRuleBinary,
    totalize,
    totalize2,
    UnaryPartialStrategy (..),
    unaryPartial,
    BinaryCommPartialStrategy (..),
    BinaryPartialStrategy (..),
    binaryPartial,
    pattern NumConTerm,
    pattern NumOrdConTerm,
    pevalAddNumTerm,
    pevalMinusNumTerm,
    pevalUMinusNumTerm,
    pevalAbsNumTerm,
    pevalSignumNumTerm,
    pevalTimesNumTerm,
    pevalLtNumTerm,
    pevalLeNumTerm,
    pevalGtNumTerm,
    pevalGeNumTerm,
    pevalTabularFunApplyTerm,
    PEvalApplyTerm (..),
    pevalDivIntegralTerm,
    pevalModIntegralTerm,
    pevalQuotIntegralTerm,
    pevalRemIntegralTerm,
  )
where

import Grisette.IR.SymPrim.Data.Prim.Helpers
  ( pattern BinaryTermPatt,
    pattern TernaryTermPatt,
    pattern UnaryTermPatt,
  )
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.SomeTerm
  ( SomeTerm (..),
  )
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
  ( BinaryOp (..),
    PEvalApplyTerm (pevalApplyTerm),
    SomeTypedSymbol (..),
    SupportedPrim (..),
    Term (..),
    TernaryOp (..),
    UnaryOp (..),
    conTerm,
    constructBinary,
    constructTernary,
    constructUnary,
    identity,
    identityWithTypeRep,
    introSupportedPrimConstraint,
    isymTerm,
    pformat,
    showUntyped,
    someTypedSymbol,
    ssymTerm,
    symTerm,
    withSymbolSupported,
  )
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.TermUtils
  ( castTerm,
    extractSymbolicsTerm,
    termSize,
    termsSize,
  )
import Grisette.IR.SymPrim.Data.Prim.Model (evaluateTerm)
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bool
  ( falseTerm,
    pevalAndTerm,
    pevalEqvTerm,
    pevalITETerm,
    pevalImplyTerm,
    pevalNotEqvTerm,
    pevalNotTerm,
    pevalOrTerm,
    pevalXorTerm,
    trueTerm,
    pattern BoolConTerm,
    pattern BoolTerm,
    pattern FalseTerm,
    pattern TrueTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Integral
  ( pevalDivIntegralTerm,
    pevalModIntegralTerm,
    pevalQuotIntegralTerm,
    pevalRemIntegralTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Num
  ( pevalAbsNumTerm,
    pevalAddNumTerm,
    pevalGeNumTerm,
    pevalGtNumTerm,
    pevalLeNumTerm,
    pevalLtNumTerm,
    pevalMinusNumTerm,
    pevalSignumNumTerm,
    pevalTimesNumTerm,
    pevalUMinusNumTerm,
    pattern NumConTerm,
    pattern NumOrdConTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.PartialEval
  ( BinaryCommPartialStrategy (..),
    BinaryPartialStrategy (..),
    PartialFun,
    PartialRuleBinary,
    PartialRuleUnary,
    TotalRuleBinary,
    TotalRuleUnary,
    UnaryPartialStrategy (..),
    binaryPartial,
    totalize,
    totalize2,
    unaryPartial,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.TabularFun
  ( pevalTabularFunApplyTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Unfold
  ( binaryUnfoldOnce,
    unaryUnfoldOnce,
  )
