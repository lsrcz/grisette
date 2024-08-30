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
{-# LANGUAGE RankNTypes #-}
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
    generalSubstSomeTerm,
    substTerm,
    freshArgSymbol,
  )
where

import Control.DeepSeq (NFData (rnf))
import Data.Bifunctor (Bifunctor (second))
import Data.Foldable (Foldable (foldl', toList))
import qualified Data.HashSet as HS
import Data.Hashable (Hashable (hashWithSalt))
import Data.Maybe (fromJust)
import qualified Data.SBV as SBV
import qualified Data.SBV.Dynamic as SBVD
import Grisette.Internal.Core.Data.Class.Function
  ( Apply (FunType, apply),
    Function ((#)),
  )
import Grisette.Internal.Core.Data.MemoUtils (htmemo)
import Grisette.Internal.Core.Data.Symbol
  ( Symbol (IndexedSymbol),
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
  ( IsSymbolKind,
    LinkedRep (underlyingTerm, wrapTerm),
    NonFuncPrimConstraint,
    NonFuncSBVBaseType,
    PEvalApplyTerm (pevalApplyTerm, sbvApplyTerm),
    PEvalBVTerm (pevalBVConcatTerm, pevalBVExtendTerm, pevalBVSelectTerm),
    PEvalBitCastOrTerm (pevalBitCastOrTerm),
    PEvalBitCastTerm (pevalBitCastTerm),
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
    PEvalFromIntegralTerm (pevalFromIntegralTerm),
    PEvalIEEEFPConvertibleTerm (pevalFromFPOrTerm, pevalToFPTerm),
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
    SomeTypedAnySymbol,
    SomeTypedConstantSymbol,
    SupportedNonFuncPrim (withNonFuncPrim),
    SupportedPrim
      ( castTypedSymbol,
        defaultValue,
        parseSMTModelResult,
        pevalDistinctTerm,
        pevalITETerm,
        withPrim
      ),
    SupportedPrimConstraint (PrimConstraint),
    SymRep (SymType),
    SymbolKind (AnyKind),
    Term
      ( AbsNumTerm,
        AddNumTerm,
        AndBitsTerm,
        AndTerm,
        ApplyTerm,
        BVConcatTerm,
        BVExtendTerm,
        BVSelectTerm,
        BitCastOrTerm,
        BitCastTerm,
        ComplementBitsTerm,
        ConTerm,
        DistinctTerm,
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
        FromFPOrTerm,
        FromIntegralTerm,
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
        ToFPTerm,
        XorBitsTerm
      ),
    TypedAnySymbol,
    TypedConstantSymbol,
    TypedSymbol (TypedSymbol, unTypedSymbol),
    applyTerm,
    conTerm,
    eqHeteroSymbol,
    existsTerm,
    forallTerm,
    partitionCVArg,
    pevalAndTerm,
    pevalEqTerm,
    pevalITEBasicTerm,
    pevalNotTerm,
    pevalOrTerm,
    pevalQuotIntegralTerm,
    pevalRemIntegralTerm,
    pevalRotateLeftTerm,
    pformatTerm,
    someTypedSymbol,
    symTerm,
    translateTypeError,
  )
import Grisette.Internal.SymPrim.Prim.SomeTerm (SomeTerm (SomeTerm), someTerm)
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
    (SupportedNonFuncPrim a, SupportedPrim b) =>
    TypedConstantSymbol a ->
    Term b ->
    a --> b

instance (LinkedRep a sa, LinkedRep b sb) => Function (a --> b) sa sb where
  (GeneralFun s t) # x = wrapTerm $ substTerm s (underlyingTerm x) HS.empty t

infixr 0 -->

extractSymSomeTermIncludeBoundedVars ::
  SomeTerm -> HS.HashSet SomeTypedAnySymbol
extractSymSomeTermIncludeBoundedVars = htmemo go
  where
    goTyped ::
      (SupportedPrim a) =>
      Term a ->
      HS.HashSet SomeTypedAnySymbol
    goTyped = go . SomeTerm

    go :: SomeTerm -> HS.HashSet SomeTypedAnySymbol
    go (SomeTerm (SymTerm _ _ _ (sym :: TypedAnySymbol a))) =
      HS.singleton $ someTypedSymbol sym
    go (SomeTerm (ConTerm _ _ _ cv :: Term v)) =
      case (typeRep :: TypeRep v) of
        App (App gf _) _ ->
          case eqTypeRep (typeRep @(-->)) gf of
            Just HRefl ->
              case cv of
                GeneralFun (tsym :: TypedConstantSymbol x) tm ->
                  HS.union
                    ( HS.singleton
                        (someTypedSymbol $ fromJust $ castTypedSymbol tsym)
                    )
                    $ go (SomeTerm tm)
            Nothing -> HS.empty
        _ -> HS.empty
    go (SomeTerm (ForallTerm _ _ _ sym arg)) =
      HS.insert (someTypedSymbol $ fromJust $ castTypedSymbol sym) $ goUnary arg
    go (SomeTerm (ExistsTerm _ _ _ sym arg)) =
      HS.insert (someTypedSymbol $ fromJust $ castTypedSymbol sym) $ goUnary arg
    go (SomeTerm (NotTerm _ _ _ arg)) = goUnary arg
    go (SomeTerm (OrTerm _ _ _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (AndTerm _ _ _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (EqTerm _ _ _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (DistinctTerm _ _ _ args)) =
      mconcat <$> map goTyped $ toList args
    go (SomeTerm (ITETerm _ _ _ cond arg1 arg2)) = goTernary cond arg1 arg2
    go (SomeTerm (AddNumTerm _ _ _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (NegNumTerm _ _ _ arg)) = goUnary arg
    go (SomeTerm (MulNumTerm _ _ _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (AbsNumTerm _ _ _ arg)) = goUnary arg
    go (SomeTerm (SignumNumTerm _ _ _ arg)) = goUnary arg
    go (SomeTerm (LtOrdTerm _ _ _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (LeOrdTerm _ _ _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (AndBitsTerm _ _ _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (OrBitsTerm _ _ _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (XorBitsTerm _ _ _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (ComplementBitsTerm _ _ _ arg)) = goUnary arg
    go (SomeTerm (ShiftLeftTerm _ _ _ arg n)) = goBinary arg n
    go (SomeTerm (ShiftRightTerm _ _ _ arg n)) = goBinary arg n
    go (SomeTerm (RotateLeftTerm _ _ _ arg n)) = goBinary arg n
    go (SomeTerm (RotateRightTerm _ _ _ arg n)) = goBinary arg n
    go (SomeTerm (BitCastTerm _ _ _ arg)) = goUnary arg
    go (SomeTerm (BitCastOrTerm _ _ _ d arg)) = goBinary d arg
    go (SomeTerm (BVConcatTerm _ _ _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (BVSelectTerm _ _ _ _ _ arg)) = goUnary arg
    go (SomeTerm (BVExtendTerm _ _ _ _ _ arg)) = goUnary arg
    go (SomeTerm (ApplyTerm _ _ _ func arg)) = goBinary func arg
    go (SomeTerm (DivIntegralTerm _ _ _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (ModIntegralTerm _ _ _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (QuotIntegralTerm _ _ _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (RemIntegralTerm _ _ _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (FPTraitTerm _ _ _ _ arg)) = goUnary arg
    go (SomeTerm (FdivTerm _ _ _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (RecipTerm _ _ _ arg)) = goUnary arg
    go (SomeTerm (FloatingUnaryTerm _ _ _ _ arg)) = goUnary arg
    go (SomeTerm (PowerTerm _ _ _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (FPUnaryTerm _ _ _ _ arg)) = goUnary arg
    go (SomeTerm (FPBinaryTerm _ _ _ _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (FPRoundingUnaryTerm _ _ _ _ _ arg)) = goUnary arg
    go (SomeTerm (FPRoundingBinaryTerm _ _ _ _ _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (FPFMATerm _ _ _ mode arg1 arg2 arg3)) =
      mconcat
        [ goTyped mode,
          goTyped arg1,
          goTyped arg2,
          goTyped arg3
        ]
    go (SomeTerm (FromIntegralTerm _ _ _ arg)) = goUnary arg
    go (SomeTerm (FromFPOrTerm _ _ _ d mode arg)) = goTernary d mode arg
    go (SomeTerm (ToFPTerm _ _ _ mode arg _ _)) = goBinary mode arg
    goUnary :: (SupportedPrim a) => Term a -> HS.HashSet SomeTypedAnySymbol
    goUnary = goTyped
    goBinary ::
      (SupportedPrim a, SupportedPrim b) =>
      Term a ->
      Term b ->
      HS.HashSet SomeTypedAnySymbol
    goBinary a b = goTyped a <> goTyped b
    goTernary ::
      (SupportedPrim a, SupportedPrim b, SupportedPrim c) =>
      Term a ->
      Term b ->
      Term c ->
      HS.HashSet SomeTypedAnySymbol
    goTernary a b c = goTyped a <> goTyped b <> goTyped c

-- | Generate a fresh argument symbol that is not used as bounded or unbounded
-- variables in the function body for a general symbolic function.
freshArgSymbol ::
  forall a. (SupportedNonFuncPrim a) => [SomeTerm] -> TypedConstantSymbol a
freshArgSymbol terms = TypedSymbol $ go 0
  where
    allSymbols = mconcat $ extractSymSomeTermIncludeBoundedVars <$> terms
    go :: Int -> Symbol
    go n =
      let currentSymbol = IndexedSymbol "arg" n
          currentTypedSymbol =
            someTypedSymbol (TypedSymbol currentSymbol :: TypedAnySymbol a)
       in if HS.member currentTypedSymbol allSymbols
            then go (n + 1)
            else currentSymbol

-- | Build a general symbolic function with a bounded symbol and a term.
buildGeneralFun ::
  forall a b.
  (SupportedNonFuncPrim a, SupportedPrim b) =>
  TypedConstantSymbol a ->
  Term b ->
  a --> b
buildGeneralFun arg v =
  GeneralFun
    argSymbol
    (substTerm arg (symTerm $ unTypedSymbol argSymbol) HS.empty v)
  where
    argSymbol = freshArgSymbol [SomeTerm v]

instance Eq (a --> b) where
  GeneralFun sym1 tm1 == GeneralFun sym2 tm2 = sym1 == sym2 && tm1 == tm2

instance Show (a --> b) where
  show (GeneralFun sym tm) = "\\(" ++ show sym ++ ") -> " ++ pformatTerm tm

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
    PrimConstraint (a --> b) =
      ( SupportedNonFuncPrim a,
        SupportedPrim b,
        NonFuncPrimConstraint a,
        PrimConstraint b,
        SBVType (a --> b) ~ (SBV.SBV (NonFuncSBVBaseType a) -> SBVType b)
      )

instance
  (SupportedNonFuncPrim a, SupportedPrim b) =>
  SBVRep (a --> b)
  where
  type
    SBVType (a --> b) =
      SBV.SBV (NonFuncSBVBaseType a) ->
      SBVType b

instance (Apply st, LinkedRep ca sa, LinkedRep ct st) => Apply (ca --> ct) where
  type FunType (ca --> ct) = SymType ca -> FunType (SymType ct)
  apply uf a = apply (uf # a)

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
    doPevalApplyTerm (ConTerm _ _ _ (GeneralFun arg tm)) v =
      Just $ substTerm arg v HS.empty tm
    doPevalApplyTerm (ITETerm _ _ _ c l r) v =
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
  sbvApplyTerm f a =
    withPrim @(a --> b) $ withNonFuncPrim @a $ f a

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

-- | General procedure for substituting symbols in a term.
{-# NOINLINE generalSubstSomeTerm #-}
generalSubstSomeTerm ::
  forall v.
  (forall a. TypedSymbol 'AnyKind a -> Term a) ->
  HS.HashSet SomeTypedConstantSymbol ->
  Term v ->
  Term v
generalSubstSomeTerm subst initialBoundedSymbols = go initialMemo
  where
    go :: forall a. (SomeTerm -> SomeTerm) -> Term a -> Term a
    go memo a = case memo $ someTerm a of
      SomeTerm v -> unsafeCoerce v
    initialMemo :: SomeTerm -> SomeTerm
    initialMemo = htmemo (goSome initialMemo initialBoundedSymbols)
    {-# NOINLINE initialMemo #-}
    goSome ::
      (SomeTerm -> SomeTerm) ->
      HS.HashSet SomeTypedConstantSymbol ->
      SomeTerm ->
      SomeTerm
    goSome _ bs c@(SomeTerm (ConTerm _ _ _ cv :: Term x)) =
      case (typeRep :: TypeRep x) of
        App (App gf _) _ ->
          case eqTypeRep gf (typeRep @(-->)) of
            Just HRefl -> case cv of
              GeneralFun sym (tm :: Term r) ->
                let newmemo =
                      htmemo
                        ( goSome
                            newmemo
                            (HS.union (HS.singleton (someTypedSymbol sym)) bs)
                        )
                    {-# NOINLINE newmemo #-}
                 in SomeTerm $ conTerm $ GeneralFun sym (go newmemo tm)
            Nothing -> c
        _ -> c
    goSome _ bs c@(SomeTerm ((SymTerm _ _ _ sym) :: Term a)) =
      case castTypedSymbol sym of
        Just sym' | HS.member (someTypedSymbol sym') bs -> c
        _ -> SomeTerm $ subst sym
    goSome _ bs (SomeTerm (ForallTerm _ _ _ tsym b)) =
      let newmemo =
            htmemo (goSome newmemo (HS.insert (someTypedSymbol tsym) bs))
          {-# NOINLINE newmemo #-}
       in goUnary newmemo (forallTerm tsym) b
    goSome _ bs (SomeTerm (ExistsTerm _ _ _ tsym b)) =
      let newmemo =
            htmemo (goSome newmemo (HS.insert (someTypedSymbol tsym) bs))
          {-# NOINLINE newmemo #-}
       in goUnary newmemo (existsTerm tsym) b
    goSome memo _ (SomeTerm (NotTerm _ _ _ arg)) =
      goUnary memo pevalNotTerm arg
    goSome memo _ (SomeTerm (OrTerm _ _ _ arg1 arg2)) =
      goBinary memo pevalOrTerm arg1 arg2
    goSome memo _ (SomeTerm (AndTerm _ _ _ arg1 arg2)) =
      goBinary memo pevalAndTerm arg1 arg2
    goSome memo _ (SomeTerm (EqTerm _ _ _ arg1 arg2)) =
      goBinary memo pevalEqTerm arg1 arg2
    goSome memo _ (SomeTerm (DistinctTerm _ _ _ args)) =
      SomeTerm $ pevalDistinctTerm (fmap (go memo) args)
    goSome memo _ (SomeTerm (ITETerm _ _ _ cond arg1 arg2)) =
      goTernary memo pevalITETerm cond arg1 arg2
    goSome memo _ (SomeTerm (AddNumTerm _ _ _ arg1 arg2)) =
      goBinary memo pevalAddNumTerm arg1 arg2
    goSome memo _ (SomeTerm (NegNumTerm _ _ _ arg)) =
      goUnary memo pevalNegNumTerm arg
    goSome memo _ (SomeTerm (MulNumTerm _ _ _ arg1 arg2)) =
      goBinary memo pevalMulNumTerm arg1 arg2
    goSome memo _ (SomeTerm (AbsNumTerm _ _ _ arg)) =
      goUnary memo pevalAbsNumTerm arg
    goSome memo _ (SomeTerm (SignumNumTerm _ _ _ arg)) =
      goUnary memo pevalSignumNumTerm arg
    goSome memo _ (SomeTerm (LtOrdTerm _ _ _ arg1 arg2)) =
      goBinary memo pevalLtOrdTerm arg1 arg2
    goSome memo _ (SomeTerm (LeOrdTerm _ _ _ arg1 arg2)) =
      goBinary memo pevalLeOrdTerm arg1 arg2
    goSome memo _ (SomeTerm (AndBitsTerm _ _ _ arg1 arg2)) =
      goBinary memo pevalAndBitsTerm arg1 arg2
    goSome memo _ (SomeTerm (OrBitsTerm _ _ _ arg1 arg2)) =
      goBinary memo pevalOrBitsTerm arg1 arg2
    goSome memo _ (SomeTerm (XorBitsTerm _ _ _ arg1 arg2)) =
      goBinary memo pevalXorBitsTerm arg1 arg2
    goSome memo _ (SomeTerm (ComplementBitsTerm _ _ _ arg)) =
      goUnary memo pevalComplementBitsTerm arg
    goSome memo _ (SomeTerm (ShiftLeftTerm _ _ _ arg n)) =
      goBinary memo pevalShiftLeftTerm arg n
    goSome memo _ (SomeTerm (RotateLeftTerm _ _ _ arg n)) =
      goBinary memo pevalRotateLeftTerm arg n
    goSome memo _ (SomeTerm (ShiftRightTerm _ _ _ arg n)) =
      goBinary memo pevalShiftRightTerm arg n
    goSome memo _ (SomeTerm (RotateRightTerm _ _ _ arg n)) =
      goBinary memo pevalRotateRightTerm arg n
    goSome memo _ (SomeTerm (BitCastTerm _ _ _ (arg :: Term a) :: Term r)) =
      goUnary memo (pevalBitCastTerm @a @r) arg
    goSome memo _ (SomeTerm (BitCastOrTerm _ _ _ (d :: term r) (arg :: Term a) :: Term r)) =
      goBinary memo (pevalBitCastOrTerm @a @r) d arg
    goSome memo _ (SomeTerm (BVConcatTerm _ _ _ arg1 arg2)) =
      goBinary memo pevalBVConcatTerm arg1 arg2
    goSome memo _ (SomeTerm (BVSelectTerm _ _ _ ix w arg)) =
      goUnary memo (pevalBVSelectTerm ix w) arg
    goSome memo _ (SomeTerm (BVExtendTerm _ _ _ n signed arg)) =
      goUnary memo (pevalBVExtendTerm n signed) arg
    goSome memo _ (SomeTerm (ApplyTerm _ _ _ f arg)) =
      goBinary memo pevalApplyTerm f arg
    goSome memo _ (SomeTerm (DivIntegralTerm _ _ _ arg1 arg2)) =
      goBinary memo pevalDivIntegralTerm arg1 arg2
    goSome memo _ (SomeTerm (ModIntegralTerm _ _ _ arg1 arg2)) =
      goBinary memo pevalModIntegralTerm arg1 arg2
    goSome memo _ (SomeTerm (QuotIntegralTerm _ _ _ arg1 arg2)) =
      goBinary memo pevalQuotIntegralTerm arg1 arg2
    goSome memo _ (SomeTerm (RemIntegralTerm _ _ _ arg1 arg2)) =
      goBinary memo pevalRemIntegralTerm arg1 arg2
    goSome memo _ (SomeTerm (FPTraitTerm _ _ _ trait arg)) =
      goUnary memo (pevalFPTraitTerm trait) arg
    goSome memo _ (SomeTerm (FdivTerm _ _ _ arg1 arg2)) =
      goBinary memo pevalFdivTerm arg1 arg2
    goSome memo _ (SomeTerm (RecipTerm _ _ _ arg)) =
      goUnary memo pevalRecipTerm arg
    goSome memo _ (SomeTerm (FloatingUnaryTerm _ _ _ op arg)) =
      goUnary memo (pevalFloatingUnaryTerm op) arg
    goSome memo _ (SomeTerm (PowerTerm _ _ _ arg1 arg2)) =
      goBinary memo pevalPowerTerm arg1 arg2
    goSome memo _ (SomeTerm (FPUnaryTerm _ _ _ op arg)) =
      goUnary memo (pevalFPUnaryTerm op) arg
    goSome memo _ (SomeTerm (FPBinaryTerm _ _ _ op arg1 arg2)) =
      goBinary memo (pevalFPBinaryTerm op) arg1 arg2
    goSome memo _ (SomeTerm (FPRoundingUnaryTerm _ _ _ op mode arg)) =
      goUnary memo (pevalFPRoundingUnaryTerm op mode) arg
    goSome memo _ (SomeTerm (FPRoundingBinaryTerm _ _ _ op mode arg1 arg2)) =
      goBinary memo (pevalFPRoundingBinaryTerm op mode) arg1 arg2
    goSome memo _ (SomeTerm (FPFMATerm _ _ _ mode arg1 arg2 arg3)) =
      SomeTerm $
        pevalFPFMATerm
          (go memo mode)
          (go memo arg1)
          (go memo arg2)
          (go memo arg3)
    goSome memo _ (SomeTerm (FromIntegralTerm _ _ _ (arg :: Term a) :: Term b)) =
      goUnary memo (pevalFromIntegralTerm @a @b) arg
    goSome memo _ (SomeTerm (FromFPOrTerm _ _ _ d mode arg)) =
      goTernary memo pevalFromFPOrTerm d mode arg
    goSome
      memo
      _
      (SomeTerm (ToFPTerm _ _ _ mode (arg :: Term a) (_ :: p eb) (_ :: q sb))) =
        goBinary memo (pevalToFPTerm @a @eb @sb) mode arg
    goUnary memo f a = SomeTerm $ f (go memo a)
    goBinary memo f a b = SomeTerm $ f (go memo a) (go memo b)
    goTernary memo f a b c =
      SomeTerm $ f (go memo a) (go memo b) (go memo c)

-- | Substitute a term for a symbol in a term.
substTerm ::
  forall knd a b.
  (SupportedPrim a, SupportedPrim b, IsSymbolKind knd) =>
  TypedSymbol knd a ->
  Term a ->
  HS.HashSet SomeTypedConstantSymbol ->
  Term b ->
  Term b
substTerm sym a =
  generalSubstSomeTerm
    ( \t@(TypedSymbol t') ->
        if eqHeteroSymbol sym t then unsafeCoerce a else symTerm t'
    )

supportedPrimFunUpTo
  [|buildGeneralFun (TypedSymbol "a") (conTerm defaultValue)|]
  [|
    \c t f -> case (t, f) of
      ( ConTerm _ _ _ (GeneralFun (ta :: TypedConstantSymbol a) a),
        ConTerm _ _ _ (GeneralFun tb b)
        ) ->
          conTerm $
            GeneralFun argSymbol $
              pevalITETerm
                c
                (substTerm ta (symTerm $ unTypedSymbol argSymbol) HS.empty a)
                (substTerm tb (symTerm $ unTypedSymbol argSymbol) HS.empty b)
          where
            argSymbol :: TypedConstantSymbol a
            argSymbol = freshArgSymbol [SomeTerm a, SomeTerm b]
      _ -> pevalITEBasicTerm c t f
    |]
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
