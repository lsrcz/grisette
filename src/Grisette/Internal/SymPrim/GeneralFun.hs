{-# LANGUAGE CPP #-}
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

#if MIN_VERSION_base(4,20,0)
#else
import Data.Foldable (Foldable (foldl'))
#endif

import Control.DeepSeq (NFData (rnf))
import Data.Bifunctor (Bifunctor (second))
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
import Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalFP ()
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
    PEvalFPTerm
      ( pevalFPBinaryTerm,
        pevalFPFMATerm,
        pevalFPRoundingBinaryTerm,
        pevalFPRoundingUnaryTerm,
        pevalFPTraitTerm,
        pevalFPUnaryTerm
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
    pevalSelectTerm,
    pevalStoreTerm,
    pevalConstArrayTerm,
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
        primTypeRep,
        withPrim
      ),
    SupportedPrimConstraint (PrimConstraint),
    SymRep (SymType),
    SymbolKind (AnyKind),
    Term,
    TypedAnySymbol,
    TypedConstantSymbol,
    TypedSymbol,
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
    typedAnySymbol,
    typedConstantSymbol,
    pattern AbsNumTerm,
    pattern AddNumTerm,
    pattern AndBitsTerm,
    pattern AndTerm,
    pattern ApplyTerm,
    pattern BVConcatTerm,
    pattern BVExtendTerm,
    pattern BVSelectTerm,
    pattern BitCastOrTerm,
    pattern BitCastTerm,
    pattern ComplementBitsTerm,
    pattern ConTerm,
    pattern DistinctTerm,
    pattern DivIntegralTerm,
    pattern EqTerm,
    pattern ExistsTerm,
    pattern FPBinaryTerm,
    pattern FPFMATerm,
    pattern FPRoundingBinaryTerm,
    pattern FPRoundingUnaryTerm,
    pattern FPTraitTerm,
    pattern FPUnaryTerm,
    pattern FdivTerm,
    pattern FloatingUnaryTerm,
    pattern ForallTerm,
    pattern FromFPOrTerm,
    pattern FromIntegralTerm,
    pattern ITETerm,
    pattern LeOrdTerm,
    pattern LtOrdTerm,
    pattern ModIntegralTerm,
    pattern MulNumTerm,
    pattern NegNumTerm,
    pattern NotTerm,
    pattern OrBitsTerm,
    pattern OrTerm,
    pattern PowerTerm,
    pattern QuotIntegralTerm,
    pattern RecipTerm,
    pattern RemIntegralTerm,
    pattern RotateLeftTerm,
    pattern RotateRightTerm,
    pattern ShiftLeftTerm,
    pattern ShiftRightTerm,
    pattern SignumNumTerm,
    pattern SupportedTypedSymbol,
    pattern SymTerm,
    pattern ToFPTerm,
    pattern XorBitsTerm,
    pattern SelectTerm,
    pattern StoreTerm,
    pattern ConstArrayTerm,
  )
import Grisette.Internal.SymPrim.Prim.Pattern (pattern SubTerms)
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
    goTyped :: Term a -> HS.HashSet SomeTypedAnySymbol
    goTyped = go . someTerm

    go :: SomeTerm -> HS.HashSet SomeTypedAnySymbol
    go (SomeTerm (SymTerm (sym :: TypedAnySymbol a))) =
      HS.singleton $ someTypedSymbol sym
    go (SomeTerm (ConTerm cv :: Term v)) =
      case (primTypeRep :: TypeRep v) of
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
    go (SomeTerm (ForallTerm sym arg)) =
      HS.insert (someTypedSymbol $ fromJust $ castTypedSymbol sym) $ goTyped arg
    go (SomeTerm (ExistsTerm sym arg)) =
      HS.insert (someTypedSymbol $ fromJust $ castTypedSymbol sym) $ goTyped arg
    go (SomeTerm (SubTerms tms)) = mconcat <$> map go $ tms

-- | Generate a fresh argument symbol that is not used as bounded or unbounded
-- variables in the function body for a general symbolic function.
freshArgSymbol ::
  forall a. (SupportedNonFuncPrim a) => [SomeTerm] -> TypedConstantSymbol a
freshArgSymbol terms = typedConstantSymbol $ go 0
  where
    allSymbols = mconcat $ extractSymSomeTermIncludeBoundedVars <$> terms
    go :: Int -> Symbol
    go n =
      let currentSymbol = IndexedSymbol "arg" n
          currentTypedSymbol =
            someTypedSymbol (typedAnySymbol currentSymbol :: TypedAnySymbol a)
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
    (substTerm arg (symTerm argSymbol) HS.empty v)
  where
    argSymbol = freshArgSymbol [SomeTerm v]

-- | Checks if two formulas are the same. Not building the actual symbolic
-- equality formula.
--
-- The reason why we choose this behavior is to allow symbolic variables to be
-- used as keys in hash maps, which can be useful for memoization.
--
-- Use with caution. Usually you should use t'Grisette.Core.SymEq' instead.
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
    doPevalApplyTerm (ConTerm (GeneralFun arg tm)) v =
      Just $ substTerm arg v HS.empty tm
    doPevalApplyTerm (ITETerm c l r) v =
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
  let sym = typedConstantSymbol $ IndexedSymbol "arg" level
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
   in buildGeneralFun sym body

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
    goSome _ bs c@(SomeTerm (ConTerm cv :: Term x)) =
      case (primTypeRep :: TypeRep x) of
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
    goSome _ bs c@(SomeTerm ((SymTerm sym) :: Term a)) =
      case castTypedSymbol sym of
        Just sym' | HS.member (someTypedSymbol sym') bs -> c
        _ -> SomeTerm $ subst sym
    goSome _ bs (SomeTerm (ForallTerm tsym b)) =
      let newmemo =
            htmemo (goSome newmemo (HS.insert (someTypedSymbol tsym) bs))
          {-# NOINLINE newmemo #-}
       in goUnary newmemo (forallTerm tsym) b
    goSome _ bs (SomeTerm (ExistsTerm tsym b)) =
      let newmemo =
            htmemo (goSome newmemo (HS.insert (someTypedSymbol tsym) bs))
          {-# NOINLINE newmemo #-}
       in goUnary newmemo (existsTerm tsym) b
    goSome memo _ (SomeTerm (NotTerm arg)) =
      goUnary memo pevalNotTerm arg
    goSome memo _ (SomeTerm (OrTerm arg1 arg2)) =
      goBinary memo pevalOrTerm arg1 arg2
    goSome memo _ (SomeTerm (AndTerm arg1 arg2)) =
      goBinary memo pevalAndTerm arg1 arg2
    goSome memo _ (SomeTerm (EqTerm arg1 arg2)) =
      goBinary memo pevalEqTerm arg1 arg2
    goSome memo _ (SomeTerm (DistinctTerm args)) =
      SomeTerm $ pevalDistinctTerm (fmap (go memo) args)
    goSome memo _ (SomeTerm (ITETerm cond arg1 arg2)) =
      goTernary memo pevalITETerm cond arg1 arg2
    goSome memo _ (SomeTerm (AddNumTerm arg1 arg2)) =
      goBinary memo pevalAddNumTerm arg1 arg2
    goSome memo _ (SomeTerm (NegNumTerm arg)) =
      goUnary memo pevalNegNumTerm arg
    goSome memo _ (SomeTerm (MulNumTerm arg1 arg2)) =
      goBinary memo pevalMulNumTerm arg1 arg2
    goSome memo _ (SomeTerm (AbsNumTerm arg)) =
      goUnary memo pevalAbsNumTerm arg
    goSome memo _ (SomeTerm (SignumNumTerm arg)) =
      goUnary memo pevalSignumNumTerm arg
    goSome memo _ (SomeTerm (LtOrdTerm arg1 arg2)) =
      goBinary memo pevalLtOrdTerm arg1 arg2
    goSome memo _ (SomeTerm (LeOrdTerm arg1 arg2)) =
      goBinary memo pevalLeOrdTerm arg1 arg2
    goSome memo _ (SomeTerm (AndBitsTerm arg1 arg2)) =
      goBinary memo pevalAndBitsTerm arg1 arg2
    goSome memo _ (SomeTerm (OrBitsTerm arg1 arg2)) =
      goBinary memo pevalOrBitsTerm arg1 arg2
    goSome memo _ (SomeTerm (XorBitsTerm arg1 arg2)) =
      goBinary memo pevalXorBitsTerm arg1 arg2
    goSome memo _ (SomeTerm (ComplementBitsTerm arg)) =
      goUnary memo pevalComplementBitsTerm arg
    goSome memo _ (SomeTerm (ShiftLeftTerm arg n)) =
      goBinary memo pevalShiftLeftTerm arg n
    goSome memo _ (SomeTerm (RotateLeftTerm arg n)) =
      goBinary memo pevalRotateLeftTerm arg n
    goSome memo _ (SomeTerm (ShiftRightTerm arg n)) =
      goBinary memo pevalShiftRightTerm arg n
    goSome memo _ (SomeTerm (RotateRightTerm arg n)) =
      goBinary memo pevalRotateRightTerm arg n
    goSome memo _ (SomeTerm (BitCastTerm (arg :: Term a) :: Term r)) =
      goUnary memo (pevalBitCastTerm @a @r) arg
    goSome memo _ (SomeTerm (BitCastOrTerm (d :: term r) (arg :: Term a) :: Term r)) =
      goBinary memo (pevalBitCastOrTerm @a @r) d arg
    goSome memo _ (SomeTerm (BVConcatTerm arg1 arg2)) =
      goBinary memo pevalBVConcatTerm arg1 arg2
    goSome memo _ (SomeTerm (BVSelectTerm ix w arg)) =
      goUnary memo (pevalBVSelectTerm ix w) arg
    goSome memo _ (SomeTerm (BVExtendTerm n signed arg)) =
      goUnary memo (pevalBVExtendTerm n signed) arg
    goSome memo _ (SomeTerm (ApplyTerm f arg)) =
      goBinary memo pevalApplyTerm f arg
    goSome memo _ (SomeTerm (DivIntegralTerm arg1 arg2)) =
      goBinary memo pevalDivIntegralTerm arg1 arg2
    goSome memo _ (SomeTerm (ModIntegralTerm arg1 arg2)) =
      goBinary memo pevalModIntegralTerm arg1 arg2
    goSome memo _ (SomeTerm (QuotIntegralTerm arg1 arg2)) =
      goBinary memo pevalQuotIntegralTerm arg1 arg2
    goSome memo _ (SomeTerm (RemIntegralTerm arg1 arg2)) =
      goBinary memo pevalRemIntegralTerm arg1 arg2
    goSome memo _ (SomeTerm (FPTraitTerm trait arg)) =
      goUnary memo (pevalFPTraitTerm trait) arg
    goSome memo _ (SomeTerm (FdivTerm arg1 arg2)) =
      goBinary memo pevalFdivTerm arg1 arg2
    goSome memo _ (SomeTerm (RecipTerm arg)) =
      goUnary memo pevalRecipTerm arg
    goSome memo _ (SomeTerm (FloatingUnaryTerm op arg)) =
      goUnary memo (pevalFloatingUnaryTerm op) arg
    goSome memo _ (SomeTerm (PowerTerm arg1 arg2)) =
      goBinary memo pevalPowerTerm arg1 arg2
    goSome memo _ (SomeTerm (FPUnaryTerm op arg)) =
      goUnary memo (pevalFPUnaryTerm op) arg
    goSome memo _ (SomeTerm (FPBinaryTerm op arg1 arg2)) =
      goBinary memo (pevalFPBinaryTerm op) arg1 arg2
    goSome memo _ (SomeTerm (FPRoundingUnaryTerm op mode arg)) =
      goUnary memo (pevalFPRoundingUnaryTerm op mode) arg
    goSome memo _ (SomeTerm (FPRoundingBinaryTerm op mode arg1 arg2)) =
      goBinary memo (pevalFPRoundingBinaryTerm op mode) arg1 arg2
    goSome memo _ (SomeTerm (FPFMATerm mode arg1 arg2 arg3)) =
      SomeTerm $
        pevalFPFMATerm
          (go memo mode)
          (go memo arg1)
          (go memo arg2)
          (go memo arg3)
    goSome memo _ (SomeTerm (FromIntegralTerm (arg :: Term a) :: Term b)) =
      goUnary memo (pevalFromIntegralTerm @a @b) arg
    goSome memo _ (SomeTerm (FromFPOrTerm d mode arg)) =
      goTernary memo pevalFromFPOrTerm d mode arg
    goSome
      memo
      _
      (SomeTerm (ToFPTerm mode (arg :: Term a) (_ :: p eb) (_ :: q sb))) =
        goBinary memo (pevalToFPTerm @a @eb @sb) mode arg
    goSome  memo _ (SomeTerm (SelectTerm arr key)) =
      goBinary memo pevalSelectTerm arr key
    goSome  memo _ (SomeTerm (StoreTerm arr key val)) =
      goTernary memo pevalStoreTerm arr key val
    goSome  memo _ (SomeTerm (ConstArrayTerm pkey val)) =
      goUnary memo (pevalConstArrayTerm pkey) val
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
substTerm sym@SupportedTypedSymbol a =
  generalSubstSomeTerm
    ( \t ->
        if eqHeteroSymbol sym t
          then unsafeCoerce a
          else symTerm t
    )

supportedPrimFunUpTo
  [|buildGeneralFun (typedConstantSymbol "a") (conTerm defaultValue)|]
  [|
    \c t f -> case (t, f) of
      ( ConTerm (GeneralFun (ta :: TypedConstantSymbol a) a),
        ConTerm (GeneralFun tb b)
        ) ->
          conTerm $
            GeneralFun argSymbol $
              pevalITETerm
                c
                (substTerm ta (symTerm argSymbol) HS.empty a)
                (substTerm tb (symTerm argSymbol) HS.empty b)
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
