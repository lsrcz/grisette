{-# LANGUAGE PatternSynonyms #-}

module Pizza.Internal.IR.SymPrim
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

import Pizza.IR.SymPrim.Data.Prim.Helpers
import Pizza.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Pizza.IR.SymPrim.Data.Prim.InternedTerm.SomeTerm
import Pizza.IR.SymPrim.Data.Prim.InternedTerm.Term
import Pizza.IR.SymPrim.Data.Prim.InternedTerm.TermUtils
import Pizza.IR.SymPrim.Data.Prim.PartialEval.Bool
import Pizza.IR.SymPrim.Data.Prim.PartialEval.GeneralFunc
import Pizza.IR.SymPrim.Data.Prim.PartialEval.Integer
import Pizza.IR.SymPrim.Data.Prim.PartialEval.Num
import Pizza.IR.SymPrim.Data.Prim.PartialEval.PartialEval
import Pizza.IR.SymPrim.Data.Prim.PartialEval.TabularFunc
import Pizza.IR.SymPrim.Data.Prim.PartialEval.Unfold
import Pizza.IR.SymPrim.Data.Union
