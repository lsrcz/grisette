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
  ( Term (..),
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
    -- pattern NumConTerm,
    -- pattern NumOrdConTerm,
    pevalDivIntegralTerm,
    pevalModIntegralTerm,
    pevalQuotIntegralTerm,
    pevalRemIntegralTerm,

    -- * Partial evaluation for the terms
    UnaryOp (..),
    BinaryOp (..),
    TernaryOp (..),
    PEvalApplyTerm (..),
    PEvalBitwiseTerm (..),
    PEvalShiftTerm (..),
    PEvalRotateTerm (..),
    PEvalNumTerm (..),
    pevalSubNumTerm,
    PEvalOrdTerm (..),
    pevalGtOrdTerm,
    pevalGeOrdTerm,
  )
where

import Grisette.IR.SymPrim.Data.Prim.Helpers
  ( pattern BinaryTermPatt,
    pattern TernaryTermPatt,
    pattern UnaryTermPatt,
  )
import Grisette.IR.SymPrim.Data.Prim.Model (evaluateTerm)
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Integral
  ( pevalDivIntegralTerm,
    pevalModIntegralTerm,
    pevalQuotIntegralTerm,
    pevalRemIntegralTerm,
  )
-- import Grisette.IR.SymPrim.Data.Prim.PartialEval.Num
--   ( pattern NumConTerm,
--     pattern NumOrdConTerm,
--   )
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
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Unfold
  ( binaryUnfoldOnce,
    unaryUnfoldOnce,
  )
import Grisette.IR.SymPrim.Data.Prim.SomeTerm
  ( SomeTerm (..),
  )
import Grisette.IR.SymPrim.Data.Prim.Term
  ( BinaryOp (..),
    PEvalApplyTerm (..),
    PEvalBitwiseTerm (..),
    PEvalNumTerm (..),
    PEvalOrdTerm (..),
    PEvalRotateTerm (..),
    PEvalShiftTerm (..),
    SomeTypedSymbol (..),
    SupportedPrim (..),
    Term (..),
    TernaryOp (..),
    UnaryOp (..),
    conTerm,
    constructBinary,
    constructTernary,
    constructUnary,
    falseTerm,
    identity,
    identityWithTypeRep,
    introSupportedPrimConstraint,
    isymTerm,
    pevalAndTerm,
    pevalEqvTerm,
    pevalGeOrdTerm,
    pevalGtOrdTerm,
    pevalImplyTerm,
    pevalNotEqvTerm,
    pevalNotTerm,
    pevalOrTerm,
    pevalSubNumTerm,
    pevalXorTerm,
    pformat,
    showUntyped,
    someTypedSymbol,
    ssymTerm,
    symTerm,
    trueTerm,
    withSymbolSupported,
    pattern BoolConTerm,
    pattern BoolTerm,
    pattern FalseTerm,
    pattern TrueTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.TermUtils
  ( castTerm,
    extractSymbolicsTerm,
    termSize,
    termsSize,
  )
