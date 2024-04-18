{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.GeneralFun
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.GeneralFun
  ( type (-->) (..),
    buildGeneralFun,
    substTerm,
  )
where

import Control.DeepSeq (NFData (rnf))
import Data.Bifunctor (Bifunctor (second))
import Data.Foldable (Foldable (foldl'))
import Data.Hashable (Hashable (hashWithSalt))
import qualified Data.SBV as SBV
import qualified Data.SBV.Dynamic as SBVD
import GHC.Generics (Generic)
import Grisette.Core.Data.Class.Function (Function ((#)))
import Grisette.Core.Data.MemoUtils (htmemo)
import Grisette.Core.Data.Symbol
  ( Symbol (IndexedSymbol, SimpleSymbol),
    withInfo,
  )
import Grisette.IR.SymPrim.Data.Prim.Internal.PartialEval (totalize2)
import Grisette.IR.SymPrim.Data.Prim.Internal.Term
  ( SBVRep,
    SupportedPrim (parseSMTModelResult, sbvEq),
    partitionCVArg,
  )
import Grisette.IR.SymPrim.Data.Prim.SomeTerm (SomeTerm (SomeTerm))
import Grisette.IR.SymPrim.Data.Prim.Term
  ( BinaryOp (pevalBinary),
    LinkedRep (underlyingTerm, wrapTerm),
    NonFuncSBVBaseType,
    PEvalApplyTerm (pevalApplyTerm, sbvApplyTerm),
    PEvalBVSignConversionTerm (pevalBVToSignedTerm, pevalBVToUnsignedTerm),
    PEvalBVTerm (pevalBVConcatTerm, pevalBVExtendTerm, pevalBVSelectTerm),
    PEvalBitwiseTerm
      ( pevalAndBitsTerm,
        pevalComplementBitsTerm,
        pevalOrBitsTerm,
        pevalXorBitsTerm
      ),
    PEvalDivModIntegralTerm
      ( pevalDivIntegralTerm,
        pevalModIntegralTerm
      ),
    PEvalNumTerm
      ( pevalAbsNumTerm,
        pevalAddNumTerm,
        pevalMulNumTerm,
        pevalNegNumTerm,
        pevalSignumNumTerm
      ),
    PEvalOrdTerm (pevalLeOrdTerm, pevalLtOrdTerm),
    PEvalRotateTerm (pevalRotateRightTerm),
    PEvalShiftTerm (pevalShiftLeftTerm, pevalShiftRightTerm),
    SBVType,
    SupportedNonFuncPrim (withNonFuncPrim),
    SupportedPrim
      ( conSBVTerm,
        defaultValue,
        pevalITETerm,
        symSBVName,
        symSBVTerm,
        withPrim
      ),
    SupportedPrimConstraint (PrimConstraint),
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
        DivIntegralTerm,
        EqTerm,
        ITETerm,
        LeOrdTerm,
        LtOrdTerm,
        ModIntegralTerm,
        MulNumTerm,
        NegNumTerm,
        NotTerm,
        OrBitsTerm,
        OrTerm,
        QuotIntegralTerm,
        RemIntegralTerm,
        RotateLeftTerm,
        RotateRightTerm,
        ShiftLeftTerm,
        ShiftRightTerm,
        SignumNumTerm,
        SymTerm,
        TernaryTerm,
        ToSignedTerm,
        ToUnsignedTerm,
        UnaryTerm,
        XorBitsTerm
      ),
    TernaryOp (pevalTernary),
    TypedSymbol (TypedSymbol, unTypedSymbol),
    UnaryOp (pevalUnary),
    applyTerm,
    conTerm,
    pevalAndTerm,
    pevalDefaultEqTerm,
    pevalEqTerm,
    pevalITEBasicTerm,
    pevalNotTerm,
    pevalOrTerm,
    pevalQuotIntegralTerm,
    pevalRemIntegralTerm,
    pevalRotateLeftTerm,
    pformat,
    someTypedSymbol,
    symTerm,
    translateTypeError,
  )
import Language.Haskell.TH.Syntax (Lift (liftTyped))
import Type.Reflection
  ( TypeRep,
    eqTypeRep,
    typeRep,
    pattern App,
    type (:~~:) (HRefl),
  )
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

instance
  (SupportedNonFuncPrim a, SupportedPrim b) =>
  SupportedPrimConstraint (a --> b)
  where
  type
    PrimConstraint n (a --> b) =
      ( SupportedNonFuncPrim a,
        SupportedPrim b,
        PrimConstraint n b,
        SBVType n (a --> b) ~ (SBV.SBV (NonFuncSBVBaseType n a) -> SBVType n b)
      )

instance
  (SupportedNonFuncPrim a, SupportedPrim b) =>
  SBVRep (a --> b)
  where
  type
    SBVType n (a --> b) =
      SBV.SBV (NonFuncSBVBaseType n a) ->
      SBVType n b

parseGeneralFunSMTModelResult ::
  forall a b.
  (SupportedNonFuncPrim a, SupportedPrim b) =>
  Int ->
  ([([SBVD.CV], SBVD.CV)], SBVD.CV) ->
  a --> b
parseGeneralFunSMTModelResult level (l, s) =
  let sym = IndexedSymbol "arg" level
      funs =
        second
          (\r -> parseSMTModelResult (level + 1) (r, s))
          <$> partitionCVArg @a l
      def = parseSMTModelResult (level + 1) ([], s)
      body =
        foldl'
          ( \acc (v, f) ->
              pevalITETerm
                (pevalEqTerm (symTerm sym) (conTerm v))
                (conTerm f)
                acc
          )
          (conTerm def)
          funs
   in buildGeneralFun (TypedSymbol sym) body

instance
  (SupportedNonFuncPrim a, SupportedNonFuncPrim b) =>
  SupportedPrim (a --> b)
  where
  defaultValue = buildGeneralFun (TypedSymbol "a") (conTerm defaultValue)
  pevalITETerm = pevalITEBasicTerm
  pevalEqTerm = pevalDefaultEqTerm
  conSBVTerm _ _ =
    translateTypeError
      ( Just $
          "BUG. Please send a bug report. GeneralFun must have already been "
            <> "partial evaluated away before reaching this point."
      )
      (typeRep @(a --> b))
  symSBVName _ num = "gfunc2_" <> show num
  symSBVTerm (p :: proxy n) name =
    withNonFuncPrim @a p $
      withNonFuncPrim @b p $
        return $
          SBV.uninterpret name
  withPrim p r = withNonFuncPrim @a p $ withNonFuncPrim @b p r
  sbvEq _ _ =
    translateTypeError
      ( Just $
          "BUG. Please send a bug report. GeneralFun is not supported for "
            <> "equality comparison."
      )
      (typeRep @(a --> b))
  parseSMTModelResult = parseGeneralFunSMTModelResult

instance
  {-# OVERLAPPING #-}
  ( SupportedNonFuncPrim a,
    SupportedNonFuncPrim b,
    SupportedNonFuncPrim c,
    SupportedPrim a,
    SupportedPrim b,
    SupportedPrim c
  ) =>
  SupportedPrim (a --> b --> c)
  where
  defaultValue = buildGeneralFun (TypedSymbol "a") (conTerm defaultValue)
  pevalITETerm = pevalITEBasicTerm
  pevalEqTerm = pevalDefaultEqTerm
  conSBVTerm _ _ =
    translateTypeError
      ( Just $
          "BUG. Please send a bug report. GeneralFun must have already been "
            <> "partial evaluated away before reaching this point."
      )
      (typeRep @(a --> b --> c))
  symSBVName _ num = "gfunc3_" <> show num
  symSBVTerm (p :: proxy n) name =
    withNonFuncPrim @a p $
      withNonFuncPrim @b p $
        withNonFuncPrim @c p $
          return $
            SBV.uninterpret name
  withPrim p r =
    withNonFuncPrim @a p $
      withNonFuncPrim @b p $
        withNonFuncPrim @c p r
  sbvEq _ _ =
    translateTypeError
      ( Just $
          "BUG. Please send a bug report. GeneralFun is not supported for "
            <> "equality comparison."
      )
      (typeRep @(a --> b --> c))
  parseSMTModelResult = parseGeneralFunSMTModelResult

instance
  {-# OVERLAPPING #-}
  ( SupportedNonFuncPrim a,
    SupportedNonFuncPrim b,
    SupportedNonFuncPrim c,
    SupportedNonFuncPrim d,
    SupportedPrim a,
    SupportedPrim b,
    SupportedPrim c,
    SupportedPrim d
  ) =>
  SupportedPrim (a --> b --> c --> d)
  where
  defaultValue = buildGeneralFun (TypedSymbol "a") (conTerm defaultValue)
  pevalITETerm = pevalITEBasicTerm
  pevalEqTerm = pevalDefaultEqTerm
  conSBVTerm _ _ =
    translateTypeError
      ( Just $
          "BUG. Please send a bug report. GeneralFun must have already been "
            <> "partial evaluated away before reaching this point."
      )
      (typeRep @(a --> b --> c --> d))
  symSBVName _ num = "gfunc4_" <> show num
  symSBVTerm (p :: proxy n) name =
    withNonFuncPrim @a p $
      withNonFuncPrim @b p $
        withNonFuncPrim @c p $
          withNonFuncPrim @d p $
            return $
              SBV.uninterpret name
  withPrim p r =
    withNonFuncPrim @a p $
      withNonFuncPrim @b p $
        withNonFuncPrim @c p $
          withNonFuncPrim @d p r
  sbvEq _ _ =
    translateTypeError
      ( Just $
          "BUG. Please send a bug report. GeneralFun is not supported for "
            <> "equality comparison."
      )
      (typeRep @(a --> b --> c --> d))
  parseSMTModelResult = parseGeneralFunSMTModelResult

instance
  {-# OVERLAPPING #-}
  ( SupportedNonFuncPrim a,
    SupportedNonFuncPrim b,
    SupportedNonFuncPrim c,
    SupportedNonFuncPrim d,
    SupportedNonFuncPrim e,
    SupportedPrim a,
    SupportedPrim b,
    SupportedPrim c,
    SupportedPrim d,
    SupportedPrim e
  ) =>
  SupportedPrim (a --> b --> c --> d --> e)
  where
  defaultValue = buildGeneralFun (TypedSymbol "a") (conTerm defaultValue)
  pevalITETerm = pevalITEBasicTerm
  pevalEqTerm = pevalDefaultEqTerm
  conSBVTerm _ _ =
    translateTypeError
      ( Just $
          "BUG. Please send a bug report. GeneralFun must have already been "
            <> "partial evaluated away before reaching this point."
      )
      (typeRep @(a --> b --> c --> d --> e))
  symSBVName _ num = "gfunc5_" <> show num
  symSBVTerm (p :: proxy n) name =
    withNonFuncPrim @a p $
      withNonFuncPrim @b p $
        withNonFuncPrim @c p $
          withNonFuncPrim @d p $
            withNonFuncPrim @e p $
              return $
                SBV.uninterpret name
  withPrim p r =
    withNonFuncPrim @a p $
      withNonFuncPrim @b p $
        withNonFuncPrim @c p $
          withNonFuncPrim @d p $
            withNonFuncPrim @e p r
  sbvEq _ _ =
    translateTypeError
      ( Just $
          "BUG. Please send a bug report. GeneralFun is not supported for "
            <> "equality comparison."
      )
      (typeRep @(a --> b --> c --> d --> e))
  parseSMTModelResult = parseGeneralFunSMTModelResult

instance
  {-# OVERLAPPING #-}
  ( SupportedNonFuncPrim a,
    SupportedNonFuncPrim b,
    SupportedNonFuncPrim c,
    SupportedNonFuncPrim d,
    SupportedNonFuncPrim e,
    SupportedNonFuncPrim f,
    SupportedPrim a,
    SupportedPrim b,
    SupportedPrim c,
    SupportedPrim d,
    SupportedPrim e,
    SupportedPrim f
  ) =>
  SupportedPrim (a --> b --> c --> d --> e --> f)
  where
  defaultValue = buildGeneralFun (TypedSymbol "a") (conTerm defaultValue)
  pevalITETerm = pevalITEBasicTerm
  pevalEqTerm = pevalDefaultEqTerm
  conSBVTerm _ _ =
    translateTypeError
      ( Just $
          "BUG. Please send a bug report. GeneralFun must have already been "
            <> "partial evaluated away before reaching this point."
      )
      (typeRep @(a --> b --> c --> d --> e --> f))
  symSBVName _ num = "gfunc6_" <> show num
  symSBVTerm (p :: proxy n) name =
    withNonFuncPrim @a p $
      withNonFuncPrim @b p $
        withNonFuncPrim @c p $
          withNonFuncPrim @d p $
            withNonFuncPrim @e p $
              withNonFuncPrim @f p $
                return $
                  SBV.uninterpret name
  withPrim p r =
    withNonFuncPrim @a p $
      withNonFuncPrim @b p $
        withNonFuncPrim @c p $
          withNonFuncPrim @d p $
            withNonFuncPrim @e p $
              withNonFuncPrim @f p r
  sbvEq _ _ =
    translateTypeError
      ( Just $
          "BUG. Please send a bug report. GeneralFun is not supported for "
            <> "equality comparison."
      )
      (typeRep @(a --> b --> c --> d --> e --> f))
  parseSMTModelResult = parseGeneralFunSMTModelResult

instance
  {-# OVERLAPPING #-}
  ( SupportedNonFuncPrim a,
    SupportedNonFuncPrim b,
    SupportedNonFuncPrim c,
    SupportedNonFuncPrim d,
    SupportedNonFuncPrim e,
    SupportedNonFuncPrim f,
    SupportedNonFuncPrim g,
    SupportedPrim a,
    SupportedPrim b,
    SupportedPrim c,
    SupportedPrim d,
    SupportedPrim e,
    SupportedPrim f,
    SupportedPrim g
  ) =>
  SupportedPrim (a --> b --> c --> d --> e --> f --> g)
  where
  defaultValue = buildGeneralFun (TypedSymbol "a") (conTerm defaultValue)
  pevalITETerm = pevalITEBasicTerm
  pevalEqTerm = pevalDefaultEqTerm
  conSBVTerm _ _ =
    translateTypeError
      ( Just $
          "BUG. Please send a bug report. GeneralFun must have already been "
            <> "partial evaluated away before reaching this point."
      )
      (typeRep @(a --> b --> c --> d --> e --> f --> g))
  symSBVName _ num = "gfunc7_" <> show num
  symSBVTerm (p :: proxy n) name =
    withNonFuncPrim @a p $
      withNonFuncPrim @b p $
        withNonFuncPrim @c p $
          withNonFuncPrim @d p $
            withNonFuncPrim @e p $
              withNonFuncPrim @f p $
                withNonFuncPrim @g p $
                  return $
                    SBV.uninterpret name
  withPrim p r =
    withNonFuncPrim @a p $
      withNonFuncPrim @b p $
        withNonFuncPrim @c p $
          withNonFuncPrim @d p $
            withNonFuncPrim @e p $
              withNonFuncPrim @f p $
                withNonFuncPrim @g p r
  sbvEq _ _ =
    translateTypeError
      ( Just $
          "BUG. Please send a bug report. GeneralFun is not supported for "
            <> "equality comparison."
      )
      (typeRep @(a --> b --> c --> d --> e --> f --> g))
  parseSMTModelResult = parseGeneralFunSMTModelResult

instance
  {-# OVERLAPPING #-}
  ( SupportedNonFuncPrim a,
    SupportedNonFuncPrim b,
    SupportedNonFuncPrim c,
    SupportedNonFuncPrim d,
    SupportedNonFuncPrim e,
    SupportedNonFuncPrim f,
    SupportedNonFuncPrim g,
    SupportedNonFuncPrim h,
    SupportedPrim a,
    SupportedPrim b,
    SupportedPrim c,
    SupportedPrim d,
    SupportedPrim e,
    SupportedPrim f,
    SupportedPrim g,
    SupportedPrim h
  ) =>
  SupportedPrim (a --> b --> c --> d --> e --> f --> g --> h)
  where
  defaultValue = buildGeneralFun (TypedSymbol "a") (conTerm defaultValue)
  pevalITETerm = pevalITEBasicTerm
  pevalEqTerm = pevalDefaultEqTerm
  conSBVTerm _ _ =
    translateTypeError
      ( Just $
          "BUG. Please send a bug report. GeneralFun must have already been "
            <> "partial evaluated away before reaching this point."
      )
      (typeRep @(a --> b --> c --> d --> e --> f --> g --> h))
  symSBVName _ num = "gfunc8_" <> show num
  symSBVTerm (p :: proxy n) name =
    withNonFuncPrim @a p $
      withNonFuncPrim @b p $
        withNonFuncPrim @c p $
          withNonFuncPrim @d p $
            withNonFuncPrim @e p $
              withNonFuncPrim @f p $
                withNonFuncPrim @g p $
                  withNonFuncPrim @h p $
                    return $
                      SBV.uninterpret name
  withPrim p r =
    withNonFuncPrim @a p $
      withNonFuncPrim @b p $
        withNonFuncPrim @c p $
          withNonFuncPrim @d p $
            withNonFuncPrim @e p $
              withNonFuncPrim @f p $
                withNonFuncPrim @g p $
                  withNonFuncPrim @h p r
  sbvEq _ _ =
    translateTypeError
      ( Just $
          "BUG. Please send a bug report. GeneralFun is not supported for "
            <> "equality comparison."
      )
      (typeRep @(a --> b --> c --> d --> e --> f --> g --> h))
  parseSMTModelResult = parseGeneralFunSMTModelResult

pevalGeneralFunApplyTerm ::
  ( SupportedNonFuncPrim a,
    SupportedPrim b,
    SupportedPrim (a --> b)
  ) =>
  Term (a --> b) ->
  Term a ->
  Term b
pevalGeneralFunApplyTerm = totalize2 doPevalApplyTerm applyTerm
  where
    doPevalApplyTerm (ConTerm _ (GeneralFun arg tm)) v =
      Just $ substTerm arg v tm
    doPevalApplyTerm (ITETerm _ c l r) v =
      return $ pevalITETerm c (pevalApplyTerm l v) (pevalApplyTerm r v)
    doPevalApplyTerm _ _ = Nothing

instance
  ( SupportedPrim (a --> b),
    SupportedNonFuncPrim a,
    SupportedPrim b
  ) =>
  PEvalApplyTerm (a --> b) a b
  where
  pevalApplyTerm = pevalGeneralFunApplyTerm
  sbvApplyTerm p f a =
    withPrim @(a --> b) p $ withNonFuncPrim @a p $ f a

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
        UnaryTerm _ tag te -> SomeTerm $ pevalUnary tag (gov te)
        BinaryTerm _ tag te te' -> SomeTerm $ pevalBinary tag (gov te) (gov te')
        TernaryTerm _ tag op1 op2 op3 -> SomeTerm $ pevalTernary tag (gov op1) (gov op2) (gov op3)
        NotTerm _ op -> SomeTerm $ pevalNotTerm (gov op)
        OrTerm _ op1 op2 -> SomeTerm $ pevalOrTerm (gov op1) (gov op2)
        AndTerm _ op1 op2 -> SomeTerm $ pevalAndTerm (gov op1) (gov op2)
        EqTerm _ op1 op2 -> SomeTerm $ pevalEqTerm (gov op1) (gov op2)
        ITETerm _ c op1 op2 -> SomeTerm $ pevalITETerm (gov c) (gov op1) (gov op2)
        AddNumTerm _ op1 op2 -> SomeTerm $ pevalAddNumTerm (gov op1) (gov op2)
        NegNumTerm _ op -> SomeTerm $ pevalNegNumTerm (gov op)
        MulNumTerm _ op1 op2 -> SomeTerm $ pevalMulNumTerm (gov op1) (gov op2)
        AbsNumTerm _ op -> SomeTerm $ pevalAbsNumTerm (gov op)
        SignumNumTerm _ op -> SomeTerm $ pevalSignumNumTerm (gov op)
        LtOrdTerm _ op1 op2 -> SomeTerm $ pevalLtOrdTerm (gov op1) (gov op2)
        LeOrdTerm _ op1 op2 -> SomeTerm $ pevalLeOrdTerm (gov op1) (gov op2)
        AndBitsTerm _ op1 op2 -> SomeTerm $ pevalAndBitsTerm (gov op1) (gov op2)
        OrBitsTerm _ op1 op2 -> SomeTerm $ pevalOrBitsTerm (gov op1) (gov op2)
        XorBitsTerm _ op1 op2 -> SomeTerm $ pevalXorBitsTerm (gov op1) (gov op2)
        ComplementBitsTerm _ op -> SomeTerm $ pevalComplementBitsTerm (gov op)
        ShiftLeftTerm _ op n -> SomeTerm $ pevalShiftLeftTerm (gov op) (gov n)
        RotateLeftTerm _ op n -> SomeTerm $ pevalRotateLeftTerm (gov op) (gov n)
        ShiftRightTerm _ op n -> SomeTerm $ pevalShiftRightTerm (gov op) (gov n)
        RotateRightTerm _ op n -> SomeTerm $ pevalRotateRightTerm (gov op) (gov n)
        ToSignedTerm _ op -> SomeTerm $ pevalBVToSignedTerm op
        ToUnsignedTerm _ op -> SomeTerm $ pevalBVToUnsignedTerm op
        BVConcatTerm _ op1 op2 -> SomeTerm $ pevalBVConcatTerm (gov op1) (gov op2)
        BVSelectTerm _ ix w op -> SomeTerm $ pevalBVSelectTerm ix w (gov op)
        BVExtendTerm _ n signed op -> SomeTerm $ pevalBVExtendTerm n signed (gov op)
        ApplyTerm _ f op -> SomeTerm $ pevalApplyTerm (gov f) (gov op)
        DivIntegralTerm _ op1 op2 -> SomeTerm $ pevalDivIntegralTerm (gov op1) (gov op2)
        ModIntegralTerm _ op1 op2 -> SomeTerm $ pevalModIntegralTerm (gov op1) (gov op2)
        QuotIntegralTerm _ op1 op2 -> SomeTerm $ pevalQuotIntegralTerm (gov op1) (gov op2)
        RemIntegralTerm _ op1 op2 -> SomeTerm $ pevalRemIntegralTerm (gov op1) (gov op2)
