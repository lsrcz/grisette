{-# LANGUAGE PatternSynonyms #-}

module Grisette.Internal.IR.SymPrim
  ( Union,
    UnaryOp (..),
    BinaryOp (..),
    TernaryOp (..),
    Term (..),
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
    concTerm,
    symbTerm,
    ssymbTerm,
    isymbTerm,
    sinfosymbTerm,
    iinfosymbTerm,
    termSize,
    termsSize,
    extractSymbolicsTerm,
    trueTerm,
    falseTerm,
    pattern BoolConcTerm,
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
    PartialFunc,
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
    pattern NumConcTerm,
    pattern NumOrdConcTerm,
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
    pevalTabularFuncApplyTerm,
    pevalGeneralFuncApplyTerm,
    pevalDivIntegerTerm,
    pevalModIntegerTerm,
  )
where

import Grisette.IR.SymPrim.Data.Prim.Helpers
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.SomeTerm
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.TermUtils
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bool
import Grisette.IR.SymPrim.Data.Prim.PartialEval.GeneralFunc
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Integer
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Num
import Grisette.IR.SymPrim.Data.Prim.PartialEval.PartialEval
import Grisette.IR.SymPrim.Data.Prim.PartialEval.TabularFunc
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Unfold
import Grisette.IR.SymPrim.Data.Union
