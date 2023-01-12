{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      :   Grisette.Internal.IR.SymPrim
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.IR.SymPrim
  ( FunArg (..),
    Sym (..),
    Union,
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
    sinfosymTerm,
    iinfosymTerm,
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
    pevalGeneralFunApplyTerm,
    pevalDivIntegerTerm,
    pevalModIntegerTerm,
  )
where

import Grisette.IR.SymPrim.Data.Prim.Helpers
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.SomeTerm
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.TermUtils
import Grisette.IR.SymPrim.Data.Prim.Model
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bool
import Grisette.IR.SymPrim.Data.Prim.PartialEval.GeneralFun
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Integer
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Num
import Grisette.IR.SymPrim.Data.Prim.PartialEval.PartialEval
import Grisette.IR.SymPrim.Data.Prim.PartialEval.TabularFun
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Unfold
import Grisette.IR.SymPrim.Data.SymPrim
import Grisette.IR.SymPrim.Data.Union
