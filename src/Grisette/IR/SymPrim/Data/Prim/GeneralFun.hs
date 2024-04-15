{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.Prim.GeneralFun
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.Prim.GeneralFun
  ( type (-->) (..),
    buildGeneralFun,
    substTerm,
  )
where

import Control.DeepSeq (NFData (rnf))
import Data.Hashable (Hashable (hashWithSalt))
import GHC.Generics (Generic)
import Grisette.Core.Data.Class.Function (Function ((#)))
import Grisette.Core.Data.MemoUtils (htmemo)
import Grisette.Core.Data.Symbol
  ( Symbol (IndexedSymbol, SimpleSymbol),
    withInfo,
  )
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.SomeTerm (SomeTerm (SomeTerm))
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
  ( BinaryOp (partialEvalBinary),
    LinkedRep (underlyingTerm, wrapTerm),
    PEvalApplyTerm (pevalApplyTerm),
    SupportedPrim (PrimConstraint, defaultValue),
    Term
      ( AbsNumTerm,
        AddNumTerm,
        AndBitsTerm,
        AndTerm,
        ApplyTerm,
        BVConcatTerm,
        BVExtendTerm,
        BVSelectTerm,
        BinaryTerm,
        ComplementBitsTerm,
        ConTerm,
        DivBoundedIntegralTerm,
        DivIntegralTerm,
        EqvTerm,
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
    TypedSymbol (TypedSymbol, unTypedSymbol),
    UnaryOp (partialEvalUnary),
    applyTerm,
    conTerm,
    pformat,
    someTypedSymbol,
    symTerm,
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
import Grisette.IR.SymPrim.Data.Prim.PartialEval.PartialEval (totalize2)
import Grisette.IR.SymPrim.Data.Prim.PartialEval.TabularFun
  ( pevalTabularFunApplyTerm,
  )
import Language.Haskell.TH.Syntax (Lift (liftTyped))
import Type.Reflection (TypeRep, eqTypeRep, typeRep, pattern App, type (:~~:) (HRefl))
import Unsafe.Coerce (unsafeCoerce)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim

-- | General symbolic function type. Use the '#' operator to apply the function.
-- Note that this function should be applied to symbolic values only. It is by
-- itself already a symbolic value, but can be considered partially concrete
-- as the function body is specified. Use 'Grisette.IR.SymPrim.Data.SymPrim.-~>'
-- for uninterpreted general symbolic functions.
--
-- The result would be partially evaluated.
--
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeOperators
-- >>> let f = ("x" :: TypedSymbol Integer) --> ("x" + 1 + "y" :: SymInteger) :: Integer --> Integer
-- >>> f # 1    -- 1 has the type SymInteger
-- (+ 2 y)
-- >>> f # "a"  -- "a" has the type SymInteger
-- (+ 1 (+ a y))
data (-->) a b where
  GeneralFun ::
    (SupportedPrim a, SupportedPrim b) =>
    TypedSymbol a ->
    Term b ->
    a --> b

instance (LinkedRep a sa, LinkedRep b sb) => Function (a --> b) sa sb where
  (GeneralFun s t) # x = wrapTerm $ substTerm s (underlyingTerm x) t

infixr 0 -->

buildGeneralFun ::
  (SupportedPrim a, SupportedPrim b) => TypedSymbol a -> Term b -> a --> b
buildGeneralFun arg v =
  GeneralFun
    (TypedSymbol newarg)
    (substTerm arg (symTerm newarg) v)
  where
    newarg = case unTypedSymbol arg of
      SimpleSymbol s -> SimpleSymbol (withInfo s ARG)
      IndexedSymbol s i -> IndexedSymbol (withInfo s ARG) i

data ARG = ARG
  deriving (Eq, Ord, Lift, Show, Generic)

instance NFData ARG where
  rnf ARG = ()

instance Hashable ARG where
  hashWithSalt s ARG = s `hashWithSalt` (0 :: Int)

instance Eq (a --> b) where
  GeneralFun sym1 tm1 == GeneralFun sym2 tm2 = sym1 == sym2 && tm1 == tm2

instance Show (a --> b) where
  show (GeneralFun sym tm) = "\\(" ++ show sym ++ ") -> " ++ pformat tm

instance Lift (a --> b) where
  liftTyped (GeneralFun sym tm) = [||GeneralFun sym tm||]

instance Hashable (a --> b) where
  s `hashWithSalt` (GeneralFun sym tm) = s `hashWithSalt` sym `hashWithSalt` tm

instance NFData (a --> b) where
  rnf (GeneralFun sym tm) = rnf sym `seq` rnf tm

instance (SupportedPrim a, SupportedPrim b) => SupportedPrim (a --> b) where
  type PrimConstraint (a --> b) = (SupportedPrim a, SupportedPrim b)
  defaultValue = buildGeneralFun (TypedSymbol "a") (conTerm defaultValue)

instance
  (SupportedPrim a, SupportedPrim b) =>
  PEvalApplyTerm (a --> b) a b
  where
  pevalApplyTerm = totalize2 doPevalApplyTerm applyTerm
    where
      doPevalApplyTerm ::
        (SupportedPrim a, SupportedPrim b) =>
        Term (a --> b) ->
        Term a ->
        Maybe (Term b)
      doPevalApplyTerm (ConTerm _ (GeneralFun arg tm)) v =
        Just $ substTerm arg v tm
      doPevalApplyTerm (ITETerm _ c l r) v =
        return $ pevalITETerm c (pevalApplyTerm l v) (pevalApplyTerm r v)
      doPevalApplyTerm _ _ = Nothing

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
        ApplyTerm _ f op -> SomeTerm $ pevalApplyTerm (gov f) (gov op)
        TabularFunApplyTerm _ f op -> SomeTerm $ pevalTabularFunApplyTerm (gov f) (gov op)
        DivIntegralTerm _ op1 op2 -> SomeTerm $ pevalDivIntegralTerm (gov op1) (gov op2)
        ModIntegralTerm _ op1 op2 -> SomeTerm $ pevalModIntegralTerm (gov op1) (gov op2)
        QuotIntegralTerm _ op1 op2 -> SomeTerm $ pevalQuotIntegralTerm (gov op1) (gov op2)
        RemIntegralTerm _ op1 op2 -> SomeTerm $ pevalRemIntegralTerm (gov op1) (gov op2)
        DivBoundedIntegralTerm _ op1 op2 -> SomeTerm $ pevalDivBoundedIntegralTerm (gov op1) (gov op2)
        ModBoundedIntegralTerm _ op1 op2 -> SomeTerm $ pevalModBoundedIntegralTerm (gov op1) (gov op2)
        QuotBoundedIntegralTerm _ op1 op2 -> SomeTerm $ pevalQuotBoundedIntegralTerm (gov op1) (gov op2)
        RemBoundedIntegralTerm _ op1 op2 -> SomeTerm $ pevalRemBoundedIntegralTerm (gov op1) (gov op2)
