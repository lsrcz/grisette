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
  )
where

import Control.DeepSeq (NFData (rnf))
import Data.Bifunctor (Bifunctor (second))
import Data.Foldable (Foldable (foldl'))
import qualified Data.HashSet as HS
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
    SomeTypedConstantSymbol,
    SupportedNonFuncPrim (withNonFuncPrim),
    SupportedPrim
      ( castTypedSymbol,
        defaultValue,
        parseSMTModelResult,
        pevalITETerm,
        withPrim
      ),
    SupportedPrimConstraint (PrimConstraint),
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
        BinaryTerm,
        BitCastOrTerm,
        BitCastTerm,
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
        TernaryTerm,
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
    goSome _ bs c@(SomeTerm (ConTerm _ cv :: Term x)) =
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
    goSome _ bs c@(SomeTerm ((SymTerm _ sym) :: Term a)) =
      case castTypedSymbol sym of
        Just sym' | HS.member (someTypedSymbol sym') bs -> c
        _ -> SomeTerm $ subst sym
    goSome _ bs (SomeTerm (ForallTerm _ tsym b)) =
      let newmemo =
            htmemo (goSome newmemo (HS.insert (someTypedSymbol tsym) bs))
          {-# NOINLINE newmemo #-}
       in goUnary newmemo (forallTerm tsym) b
    goSome _ bs (SomeTerm (ExistsTerm _ tsym b)) =
      let newmemo =
            htmemo (goSome newmemo (HS.insert (someTypedSymbol tsym) bs))
          {-# NOINLINE newmemo #-}
       in goUnary newmemo (existsTerm tsym) b
    goSome memo _ (SomeTerm (UnaryTerm _ tag (arg :: Term a))) =
      goUnary memo (pevalUnary tag) arg
    goSome
      memo
      _
      (SomeTerm (BinaryTerm _ tag (arg1 :: Term a1) (arg2 :: Term a2))) =
        goBinary memo (pevalBinary tag) arg1 arg2
    goSome
      memo
      _
      ( SomeTerm
          ( TernaryTerm
              _
              tag
              (arg1 :: Term a1)
              (arg2 :: Term a2)
              (arg3 :: Term a3)
            )
        ) = do
        goTernary memo (pevalTernary tag) arg1 arg2 arg3
    goSome memo _ (SomeTerm (NotTerm _ arg)) =
      goUnary memo pevalNotTerm arg
    goSome memo _ (SomeTerm (OrTerm _ arg1 arg2)) =
      goBinary memo pevalOrTerm arg1 arg2
    goSome memo _ (SomeTerm (AndTerm _ arg1 arg2)) =
      goBinary memo pevalAndTerm arg1 arg2
    goSome memo _ (SomeTerm (EqTerm _ arg1 arg2)) =
      goBinary memo pevalEqTerm arg1 arg2
    goSome memo _ (SomeTerm (ITETerm _ cond arg1 arg2)) =
      goTernary memo pevalITETerm cond arg1 arg2
    goSome memo _ (SomeTerm (AddNumTerm _ arg1 arg2)) =
      goBinary memo pevalAddNumTerm arg1 arg2
    goSome memo _ (SomeTerm (NegNumTerm _ arg)) =
      goUnary memo pevalNegNumTerm arg
    goSome memo _ (SomeTerm (MulNumTerm _ arg1 arg2)) =
      goBinary memo pevalMulNumTerm arg1 arg2
    goSome memo _ (SomeTerm (AbsNumTerm _ arg)) =
      goUnary memo pevalAbsNumTerm arg
    goSome memo _ (SomeTerm (SignumNumTerm _ arg)) =
      goUnary memo pevalSignumNumTerm arg
    goSome memo _ (SomeTerm (LtOrdTerm _ arg1 arg2)) =
      goBinary memo pevalLtOrdTerm arg1 arg2
    goSome memo _ (SomeTerm (LeOrdTerm _ arg1 arg2)) =
      goBinary memo pevalLeOrdTerm arg1 arg2
    goSome memo _ (SomeTerm (AndBitsTerm _ arg1 arg2)) =
      goBinary memo pevalAndBitsTerm arg1 arg2
    goSome memo _ (SomeTerm (OrBitsTerm _ arg1 arg2)) =
      goBinary memo pevalOrBitsTerm arg1 arg2
    goSome memo _ (SomeTerm (XorBitsTerm _ arg1 arg2)) =
      goBinary memo pevalXorBitsTerm arg1 arg2
    goSome memo _ (SomeTerm (ComplementBitsTerm _ arg)) =
      goUnary memo pevalComplementBitsTerm arg
    goSome memo _ (SomeTerm (ShiftLeftTerm _ arg n)) =
      goBinary memo pevalShiftLeftTerm arg n
    goSome memo _ (SomeTerm (RotateLeftTerm _ arg n)) =
      goBinary memo pevalRotateLeftTerm arg n
    goSome memo _ (SomeTerm (ShiftRightTerm _ arg n)) =
      goBinary memo pevalShiftRightTerm arg n
    goSome memo _ (SomeTerm (RotateRightTerm _ arg n)) =
      goBinary memo pevalRotateRightTerm arg n
    goSome memo _ (SomeTerm (BitCastTerm _ (arg :: Term a) :: Term r)) =
      goUnary memo (pevalBitCastTerm @a @r) arg
    goSome memo _ (SomeTerm (BitCastOrTerm _ (d :: term r) (arg :: Term a) :: Term r)) =
      goBinary memo (pevalBitCastOrTerm @a @r) d arg
    goSome memo _ (SomeTerm (BVConcatTerm _ arg1 arg2)) =
      goBinary memo pevalBVConcatTerm arg1 arg2
    goSome memo _ (SomeTerm (BVSelectTerm _ ix w arg)) =
      goUnary memo (pevalBVSelectTerm ix w) arg
    goSome memo _ (SomeTerm (BVExtendTerm _ n signed arg)) =
      goUnary memo (pevalBVExtendTerm n signed) arg
    goSome memo _ (SomeTerm (ApplyTerm _ f arg)) =
      goBinary memo pevalApplyTerm f arg
    goSome memo _ (SomeTerm (DivIntegralTerm _ arg1 arg2)) =
      goBinary memo pevalDivIntegralTerm arg1 arg2
    goSome memo _ (SomeTerm (ModIntegralTerm _ arg1 arg2)) =
      goBinary memo pevalModIntegralTerm arg1 arg2
    goSome memo _ (SomeTerm (QuotIntegralTerm _ arg1 arg2)) =
      goBinary memo pevalQuotIntegralTerm arg1 arg2
    goSome memo _ (SomeTerm (RemIntegralTerm _ arg1 arg2)) =
      goBinary memo pevalRemIntegralTerm arg1 arg2
    goSome memo _ (SomeTerm (FPTraitTerm _ trait arg)) =
      goUnary memo (pevalFPTraitTerm trait) arg
    goSome memo _ (SomeTerm (FdivTerm _ arg1 arg2)) =
      goBinary memo pevalFdivTerm arg1 arg2
    goSome memo _ (SomeTerm (RecipTerm _ arg)) =
      goUnary memo pevalRecipTerm arg
    goSome memo _ (SomeTerm (FloatingUnaryTerm _ op arg)) =
      goUnary memo (pevalFloatingUnaryTerm op) arg
    goSome memo _ (SomeTerm (PowerTerm _ arg1 arg2)) =
      goBinary memo pevalPowerTerm arg1 arg2
    goSome memo _ (SomeTerm (FPUnaryTerm _ op arg)) =
      goUnary memo (pevalFPUnaryTerm op) arg
    goSome memo _ (SomeTerm (FPBinaryTerm _ op arg1 arg2)) =
      goBinary memo (pevalFPBinaryTerm op) arg1 arg2
    goSome memo _ (SomeTerm (FPRoundingUnaryTerm _ op mode arg)) =
      goUnary memo (pevalFPRoundingUnaryTerm op mode) arg
    goSome memo _ (SomeTerm (FPRoundingBinaryTerm _ op mode arg1 arg2)) =
      goBinary memo (pevalFPRoundingBinaryTerm op mode) arg1 arg2
    goSome memo _ (SomeTerm (FPFMATerm _ mode arg1 arg2 arg3)) =
      SomeTerm $
        pevalFPFMATerm
          (go memo mode)
          (go memo arg1)
          (go memo arg2)
          (go memo arg3)
    goSome memo _ (SomeTerm (FromIntegralTerm _ (arg :: Term a) :: Term b)) =
      goUnary memo (pevalFromIntegralTerm @a @b) arg
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
  Term b ->
  Term b
substTerm sym a =
  generalSubstSomeTerm
    ( \t@(TypedSymbol t') ->
        if eqHeteroSymbol sym t then unsafeCoerce a else symTerm t'
    )
    HS.empty

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
