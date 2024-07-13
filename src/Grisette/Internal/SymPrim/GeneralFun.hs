{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE DataKinds #-}
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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.GeneralFun
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.GeneralFun
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
import Grisette.Internal.Core.Data.Class.Function (Function ((#)))
import Grisette.Internal.Core.Data.MemoUtils (htmemo)
import Grisette.Internal.Core.Data.Symbol
  ( Symbol (IndexedSymbol, SimpleSymbol),
    withInfo,
  )
import Grisette.Internal.SymPrim.FunInstanceGen (supportedPrimFunUpTo)
import Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalFP
  ( pevalFPBinaryTerm,
    pevalFPFMATerm,
    pevalFPRoundingBinaryTerm,
    pevalFPRoundingUnaryTerm,
    pevalFPTraitTerm,
    pevalFPUnaryTerm,
  )
import Grisette.Internal.SymPrim.Prim.Internal.PartialEval (totalize2)
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( BinaryOp (pevalBinary),
    IsSymbolKind,
    LinkedRep (underlyingTerm, wrapTerm),
    NonFuncPrimConstraint,
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
    PEvalFloatingTerm (pevalFloatingUnaryTerm, pevalPowerTerm),
    PEvalFractionalTerm (pevalFdivTerm, pevalRecipTerm),
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
    SBVRep (SBVType),
    SupportedNonFuncPrim (withNonFuncPrim),
    SupportedPrim
      ( defaultValue,
        parseSMTModelResult,
        pevalITETerm,
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
        ExistsTerm,
        FPBinaryTerm,
        FPFMATerm,
        FPRoundingBinaryTerm,
        FPRoundingUnaryTerm,
        FPTraitTerm,
        FPUnaryTerm,
        FdivTerm,
        FloatingUnaryTerm,
        ForallTerm,
        ITETerm,
        LeOrdTerm,
        LtOrdTerm,
        ModIntegralTerm,
        MulNumTerm,
        NegNumTerm,
        NotTerm,
        OrBitsTerm,
        OrTerm,
        PowerTerm,
        QuotIntegralTerm,
        RecipTerm,
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
    TypedConstantSymbol,
    TypedSymbol (TypedSymbol, unTypedSymbol),
    UnaryOp (pevalUnary),
    applyTerm,
    conTerm,
    eqHeteroSymbol,
    existsTerm,
    forallTerm,
    partitionCVArg,
    pevalAndTerm,
    pevalEqTerm,
    pevalNotTerm,
    pevalOrTerm,
    pevalQuotIntegralTerm,
    pevalRemIntegralTerm,
    pevalRotateLeftTerm,
    pformat,
    symTerm,
    translateTypeError,
  )
import Grisette.Internal.SymPrim.Prim.SomeTerm (SomeTerm (SomeTerm))
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
-- >>> import Grisette.SymPrim

-- | General symbolic function type. Use the '#' operator to apply the function.
-- Note that this function should be applied to symbolic values only. It is by
-- itself already a symbolic value, but can be considered partially concrete
-- as the function body is specified. Use 'Grisette.SymPrim.SymPrim.-~>'
-- for uninterpreted general symbolic functions.
--
-- The result would be partially evaluated.
--
-- >>> let f = ("x" :: TypedConstantSymbol Integer) --> ("x" + 1 + "y" :: SymInteger) :: Integer --> Integer
-- >>> f # 1    -- 1 has the type SymInteger
-- (+ 2 y)
-- >>> f # "a"  -- "a" has the type SymInteger
-- (+ 1 (+ a y))
data (-->) a b where
  GeneralFun ::
    (SupportedPrim a, SupportedPrim b) =>
    TypedConstantSymbol a ->
    Term b ->
    a --> b

instance (LinkedRep a sa, LinkedRep b sb) => Function (a --> b) sa sb where
  (GeneralFun s t) # x = wrapTerm $ substTerm s (underlyingTerm x) t

infixr 0 -->

-- | Build a general symbolic function with a bounded symbol and a term.
buildGeneralFun ::
  (SupportedNonFuncPrim a, SupportedPrim b) =>
  TypedConstantSymbol a ->
  Term b ->
  a --> b
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
        NonFuncPrimConstraint n a,
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

-- | Substitute a term for a symbol in a term.
substTerm ::
  forall knd a b.
  (SupportedPrim a, SupportedPrim b, IsSymbolKind knd) =>
  TypedSymbol knd a ->
  Term a ->
  Term b ->
  Term b
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
                  if eqHeteroSymbol sym1 sym
                    then stm
                    else SomeTerm $ conTerm $ GeneralFun sym1 (gov tm1)
              Nothing -> stm
          _ -> stm
        SymTerm _ ts -> SomeTerm $ if eqHeteroSymbol ts sym then unsafeCoerce term else tm
        ForallTerm _ ts a ->
          if eqHeteroSymbol ts sym
            then stm
            else SomeTerm $ forallTerm ts (gov a)
        ExistsTerm _ ts a ->
          if eqHeteroSymbol ts sym
            then stm
            else SomeTerm $ existsTerm ts (gov a)
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
        FPTraitTerm _ trait op -> SomeTerm $ pevalFPTraitTerm trait (gov op)
        FdivTerm _ op1 op2 -> SomeTerm $ pevalFdivTerm (gov op1) (gov op2)
        RecipTerm _ op -> SomeTerm $ pevalRecipTerm (gov op)
        FloatingUnaryTerm _ fop op -> SomeTerm $ pevalFloatingUnaryTerm fop (gov op)
        PowerTerm _ op1 op2 -> SomeTerm $ pevalPowerTerm (gov op1) (gov op2)
        FPUnaryTerm _ uop op -> SomeTerm $ pevalFPUnaryTerm uop (gov op)
        FPBinaryTerm _ bop op1 op2 -> SomeTerm $ pevalFPBinaryTerm bop (gov op1) (gov op2)
        FPRoundingUnaryTerm _ uop mode op -> SomeTerm $ pevalFPRoundingUnaryTerm uop mode (gov op)
        FPRoundingBinaryTerm _ bop mode op1 op2 -> SomeTerm $ pevalFPRoundingBinaryTerm bop mode (gov op1) (gov op2)
        FPFMATerm _ mode op1 op2 op3 -> SomeTerm $ pevalFPFMATerm (gov mode) (gov op1) (gov op2) (gov op3)

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
          ( \r ->
              case r of
                [([], v)] -> parseSMTModelResult (level + 1) ([], v)
                _ -> parseSMTModelResult (level + 1) (r, s)
          )
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

supportedPrimFunUpTo
  [|buildGeneralFun (TypedSymbol "a") (conTerm defaultValue)|]
  [|parseGeneralFunSMTModelResult|]
  ( \tyVars ->
      [|
        translateTypeError
          (Just "x")
          ( typeRep ::
              TypeRep
                $( foldl1 (\fty ty -> [t|$ty --> $fty|])
                     . reverse
                     $ tyVars
                 )
          )
        |]
  )
  "GeneralFun"
  "gfunc"
  ''(-->)
  8
