{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Prim.TermUtils
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.Prim.TermUtils
  ( extractTerm,
    castTerm,
    someTermsSize,
    someTermSize,
    termSize,
    termsSize,
  )
where

import Control.Monad.State
  ( State,
    execState,
    gets,
    modify',
  )
import Data.Data (cast)
import qualified Data.HashSet as S
import Grisette.Internal.Core.Data.MemoUtils (htmemo2)
import Grisette.Internal.SymPrim.GeneralFun (type (-->) (GeneralFun))
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( IsSymbolKind (SymbolKindConstraint),
    SomeTypedConstantSymbol,
    SomeTypedSymbol (SomeTypedSymbol),
    SupportedPrim (castTypedSymbol),
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
    TypedAnySymbol,
    introSupportedPrimConstraint,
    someTypedSymbol,
  )
import Grisette.Internal.SymPrim.Prim.SomeTerm
  ( SomeTerm (SomeTerm),
  )
import Type.Reflection
  ( TypeRep,
    Typeable,
    eqTypeRep,
    typeRep,
    pattern App,
    type (:~~:) (HRefl),
  )
import qualified Type.Reflection as R

extractSymSomeTerm ::
  forall knd.
  (IsSymbolKind knd) =>
  S.HashSet (SomeTypedConstantSymbol) ->
  SomeTerm ->
  Maybe (S.HashSet (SomeTypedSymbol knd))
extractSymSomeTerm = go initialMemo
  where
    gotyped ::
      (SupportedPrim a) =>
      (S.HashSet (SomeTypedConstantSymbol) -> SomeTerm -> Maybe (S.HashSet (SomeTypedSymbol knd))) ->
      S.HashSet (SomeTypedConstantSymbol) ->
      Term a ->
      Maybe (S.HashSet (SomeTypedSymbol knd))
    gotyped memo boundedSymbols a = memo boundedSymbols (SomeTerm a)
    initialMemo :: S.HashSet (SomeTypedConstantSymbol) -> SomeTerm -> Maybe (S.HashSet (SomeTypedSymbol knd))
    initialMemo = htmemo2 (go initialMemo)
    {-# NOINLINE initialMemo #-}

    go ::
      (S.HashSet (SomeTypedConstantSymbol) -> SomeTerm -> Maybe (S.HashSet (SomeTypedSymbol knd))) ->
      S.HashSet (SomeTypedConstantSymbol) ->
      SomeTerm ->
      Maybe (S.HashSet (SomeTypedSymbol knd))
    go _ bs (SomeTerm (SymTerm _ (sym :: TypedAnySymbol a))) =
      case (castTypedSymbol sym, castTypedSymbol sym) of
        (Just sym', _) | S.member (someTypedSymbol sym') bs -> return S.empty
        (_, Just sym') -> return $ S.singleton $ SomeTypedSymbol (R.typeRep @a) sym'
        _ -> Nothing
    go _ bs (SomeTerm (ConTerm _ cv :: Term v)) =
      case (typeRep :: TypeRep v) of
        App (App gf _) _ ->
          case eqTypeRep (typeRep @(-->)) gf of
            Just HRefl -> case cv of
              GeneralFun sym (tm :: Term r) ->
                let newmemo = htmemo2 (go newmemo)
                    {-# NOINLINE newmemo #-}
                 in gotyped
                      newmemo
                      (S.union (S.singleton (someTypedSymbol sym)) bs)
                      tm
            Nothing -> return S.empty
        _ -> return S.empty
    go _ bs (SomeTerm (ForallTerm _ sym arg)) =
      let newmemo = htmemo2 (go newmemo)
          {-# NOINLINE newmemo #-}
       in goUnary newmemo (S.insert (someTypedSymbol sym) bs) arg
    go _ bs (SomeTerm (ExistsTerm _ sym arg)) =
      let newmemo = htmemo2 (go newmemo)
          {-# NOINLINE newmemo #-}
       in goUnary newmemo (S.insert (someTypedSymbol sym) bs) arg
    go memo bs (SomeTerm (UnaryTerm _ _ arg)) = goUnary memo bs arg
    go memo bs (SomeTerm (BinaryTerm _ _ arg1 arg2)) =
      goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (TernaryTerm _ _ arg1 arg2 arg3)) =
      goTernary memo bs arg1 arg2 arg3
    go memo bs (SomeTerm (NotTerm _ arg)) = goUnary memo bs arg
    go memo bs (SomeTerm (OrTerm _ arg1 arg2)) = goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (AndTerm _ arg1 arg2)) = goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (EqTerm _ arg1 arg2)) = goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (ITETerm _ cond arg1 arg2)) =
      goTernary memo bs cond arg1 arg2
    go memo bs (SomeTerm (AddNumTerm _ arg1 arg2)) = goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (NegNumTerm _ arg)) = goUnary memo bs arg
    go memo bs (SomeTerm (MulNumTerm _ arg1 arg2)) = goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (AbsNumTerm _ arg)) = goUnary memo bs arg
    go memo bs (SomeTerm (SignumNumTerm _ arg)) = goUnary memo bs arg
    go memo bs (SomeTerm (LtOrdTerm _ arg1 arg2)) = goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (LeOrdTerm _ arg1 arg2)) = goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (AndBitsTerm _ arg1 arg2)) = goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (OrBitsTerm _ arg1 arg2)) = goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (XorBitsTerm _ arg1 arg2)) = goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (ComplementBitsTerm _ arg)) = goUnary memo bs arg
    go memo bs (SomeTerm (ShiftLeftTerm _ arg n1)) = goBinary memo bs arg n1
    go memo bs (SomeTerm (ShiftRightTerm _ arg n1)) = goBinary memo bs arg n1
    go memo bs (SomeTerm (RotateLeftTerm _ arg n1)) = goBinary memo bs arg n1
    go memo bs (SomeTerm (RotateRightTerm _ arg n1)) = goBinary memo bs arg n1
    go memo bs (SomeTerm (ToSignedTerm _ arg)) = goUnary memo bs arg
    go memo bs (SomeTerm (ToUnsignedTerm _ arg)) = goUnary memo bs arg
    go memo bs (SomeTerm (BVConcatTerm _ arg1 arg2)) = goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (BVSelectTerm _ _ _ arg)) = goUnary memo bs arg
    go memo bs (SomeTerm (BVExtendTerm _ _ _ arg)) = goUnary memo bs arg
    go memo bs (SomeTerm (ApplyTerm _ func arg)) = goBinary memo bs func arg
    go memo bs (SomeTerm (DivIntegralTerm _ arg1 arg2)) = goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (ModIntegralTerm _ arg1 arg2)) = goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (QuotIntegralTerm _ arg1 arg2)) = goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (RemIntegralTerm _ arg1 arg2)) = goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (FPTraitTerm _ _ arg)) = goUnary memo bs arg
    go memo bs (SomeTerm (FdivTerm _ arg1 arg2)) = goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (RecipTerm _ arg)) = goUnary memo bs arg
    go memo bs (SomeTerm (FloatingUnaryTerm _ _ arg)) = goUnary memo bs arg
    go memo bs (SomeTerm (PowerTerm _ arg1 arg2)) = goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (FPUnaryTerm _ _ arg)) = goUnary memo bs arg
    go memo bs (SomeTerm (FPBinaryTerm _ _ arg1 arg2)) = goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (FPRoundingUnaryTerm _ _ _ arg)) = goUnary memo bs arg
    go memo bs (SomeTerm (FPRoundingBinaryTerm _ _ _ arg1 arg2)) = goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (FPFMATerm _ mode arg1 arg2 arg3)) =
      gotyped memo bs mode
        <> gotyped memo bs arg1
        <> gotyped memo bs arg2
        <> gotyped memo bs arg3
    goUnary ::
      (SupportedPrim a) =>
      (S.HashSet (SomeTypedConstantSymbol) -> SomeTerm -> Maybe (S.HashSet (SomeTypedSymbol knd))) ->
      S.HashSet (SomeTypedConstantSymbol) ->
      Term a ->
      Maybe (S.HashSet (SomeTypedSymbol knd))
    goUnary = gotyped
    goBinary ::
      (SupportedPrim a, SupportedPrim b) =>
      (S.HashSet (SomeTypedConstantSymbol) -> SomeTerm -> Maybe (S.HashSet (SomeTypedSymbol knd))) ->
      S.HashSet (SomeTypedConstantSymbol) ->
      Term a ->
      Term b ->
      Maybe (S.HashSet (SomeTypedSymbol knd))
    goBinary memo bs arg1 arg2 = gotyped memo bs arg1 <> gotyped memo bs arg2
    goTernary ::
      (SupportedPrim a, SupportedPrim b, SupportedPrim c) =>
      (S.HashSet (SomeTypedConstantSymbol) -> SomeTerm -> Maybe (S.HashSet (SomeTypedSymbol knd))) ->
      S.HashSet (SomeTypedConstantSymbol) ->
      Term a ->
      Term b ->
      Term c ->
      Maybe (S.HashSet (SomeTypedSymbol knd))
    goTernary memo bs arg1 arg2 arg3 =
      gotyped memo bs arg1 <> gotyped memo bs arg2 <> gotyped memo bs arg3
{-# INLINEABLE extractSymSomeTerm #-}

-- | Extract all the symbols in a term.
extractTerm ::
  (IsSymbolKind knd, SymbolKindConstraint knd a, SupportedPrim a) =>
  S.HashSet (SomeTypedConstantSymbol) ->
  Term a ->
  Maybe (S.HashSet (SomeTypedSymbol knd))
extractTerm initialBoundedSymbols t =
  extractSymSomeTerm initialBoundedSymbols (SomeTerm t)
{-# INLINE extractTerm #-}

-- | Cast a term to another type.
castTerm :: forall a b. (Typeable b) => Term a -> Maybe (Term b)
castTerm t@ConTerm {} = cast t
castTerm t@SymTerm {} = cast t
castTerm t@ForallTerm {} = cast t
castTerm t@ExistsTerm {} = cast t
castTerm t@UnaryTerm {} = cast t
castTerm t@BinaryTerm {} = cast t
castTerm t@TernaryTerm {} = cast t
castTerm t@NotTerm {} = cast t
castTerm t@OrTerm {} = cast t
castTerm t@AndTerm {} = cast t
castTerm t@EqTerm {} = cast t
castTerm t@ITETerm {} = cast t
castTerm t@AddNumTerm {} = cast t
castTerm t@NegNumTerm {} = cast t
castTerm t@MulNumTerm {} = cast t
castTerm t@AbsNumTerm {} = cast t
castTerm t@SignumNumTerm {} = cast t
castTerm t@LtOrdTerm {} = cast t
castTerm t@LeOrdTerm {} = cast t
castTerm t@AndBitsTerm {} = cast t
castTerm t@OrBitsTerm {} = cast t
castTerm t@XorBitsTerm {} = cast t
castTerm t@ComplementBitsTerm {} = cast t
castTerm t@ShiftLeftTerm {} = cast t
castTerm t@ShiftRightTerm {} = cast t
castTerm t@RotateLeftTerm {} = cast t
castTerm t@RotateRightTerm {} = cast t
castTerm t@ToSignedTerm {} = cast t
castTerm t@ToUnsignedTerm {} = cast t
castTerm t@BVConcatTerm {} = cast t
castTerm t@BVSelectTerm {} = cast t
castTerm t@BVExtendTerm {} = cast t
castTerm t@ApplyTerm {} = cast t
castTerm t@DivIntegralTerm {} = cast t
castTerm t@ModIntegralTerm {} = cast t
castTerm t@QuotIntegralTerm {} = cast t
castTerm t@RemIntegralTerm {} = cast t
castTerm t@FPTraitTerm {} = cast t
castTerm t@FdivTerm {} = cast t
castTerm t@RecipTerm {} = cast t
castTerm t@FloatingUnaryTerm {} = cast t
castTerm t@PowerTerm {} = cast t
castTerm t@FPUnaryTerm {} = cast t
castTerm t@FPBinaryTerm {} = cast t
castTerm t@FPRoundingUnaryTerm {} = cast t
castTerm t@FPRoundingBinaryTerm {} = cast t
castTerm t@FPFMATerm {} = cast t
{-# INLINE castTerm #-}

-- | Compute the size of a list of terms. Do not count the same term twice.
someTermsSize :: [SomeTerm] -> Int
someTermsSize terms = S.size $ execState (traverse goSome terms) S.empty
  where
    exists t = gets (S.member (SomeTerm t))
    add t = modify' (S.insert (SomeTerm t))
    goSome :: SomeTerm -> State (S.HashSet SomeTerm) ()
    goSome (SomeTerm b) = go b
    go :: forall b. Term b -> State (S.HashSet SomeTerm) ()
    go t@ConTerm {} = add t
    go t@SymTerm {} = add t
    go t@(ForallTerm _ _ arg) = goUnary t arg
    go t@(ExistsTerm _ _ arg) = goUnary t arg
    go t@(UnaryTerm _ _ arg) = goUnary t arg
    go t@(BinaryTerm _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(TernaryTerm _ _ arg1 arg2 arg3) = goTernary t arg1 arg2 arg3
    go t@(NotTerm _ arg) = goUnary t arg
    go t@(OrTerm _ arg1 arg2) = goBinary t arg1 arg2
    go t@(AndTerm _ arg1 arg2) = goBinary t arg1 arg2
    go t@(EqTerm _ arg1 arg2) = goBinary t arg1 arg2
    go t@(ITETerm _ cond arg1 arg2) = goTernary t cond arg1 arg2
    go t@(AddNumTerm _ arg1 arg2) = goBinary t arg1 arg2
    go t@(NegNumTerm _ arg) = goUnary t arg
    go t@(MulNumTerm _ arg1 arg2) = goBinary t arg1 arg2
    go t@(AbsNumTerm _ arg) = goUnary t arg
    go t@(SignumNumTerm _ arg) = goUnary t arg
    go t@(LtOrdTerm _ arg1 arg2) = goBinary t arg1 arg2
    go t@(LeOrdTerm _ arg1 arg2) = goBinary t arg1 arg2
    go t@(AndBitsTerm _ arg1 arg2) = goBinary t arg1 arg2
    go t@(OrBitsTerm _ arg1 arg2) = goBinary t arg1 arg2
    go t@(XorBitsTerm _ arg1 arg2) = goBinary t arg1 arg2
    go t@(ComplementBitsTerm _ arg) = goUnary t arg
    go t@(ShiftLeftTerm _ arg n) = goBinary t arg n
    go t@(ShiftRightTerm _ arg n) = goBinary t arg n
    go t@(RotateLeftTerm _ arg n) = goBinary t arg n
    go t@(RotateRightTerm _ arg n) = goBinary t arg n
    go t@(ToSignedTerm _ arg) = goUnary t arg
    go t@(ToUnsignedTerm _ arg) = goUnary t arg
    go t@(BVConcatTerm _ arg1 arg2) = goBinary t arg1 arg2
    go t@(BVSelectTerm _ _ _ arg) = goUnary t arg
    go t@(BVExtendTerm _ _ _ arg) = goUnary t arg
    go t@(ApplyTerm _ func arg) = goBinary t func arg
    go t@(DivIntegralTerm _ arg1 arg2) = goBinary t arg1 arg2
    go t@(ModIntegralTerm _ arg1 arg2) = goBinary t arg1 arg2
    go t@(QuotIntegralTerm _ arg1 arg2) = goBinary t arg1 arg2
    go t@(RemIntegralTerm _ arg1 arg2) = goBinary t arg1 arg2
    go t@(FPTraitTerm _ _ arg) = goUnary t arg
    go t@(FdivTerm _ arg1 arg2) = goBinary t arg1 arg2
    go t@(RecipTerm _ arg) = goUnary t arg
    go t@(FloatingUnaryTerm _ _ arg) = goUnary t arg
    go t@(PowerTerm _ arg1 arg2) = goBinary t arg1 arg2
    go t@(FPUnaryTerm _ _ arg) = goUnary t arg
    go t@(FPBinaryTerm _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(FPRoundingUnaryTerm _ _ _ arg) = goUnary t arg
    go t@(FPRoundingBinaryTerm _ _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(FPFMATerm _ _ arg1 arg2 arg3) = goTernary t arg1 arg2 arg3
    goUnary :: forall a b. (SupportedPrim a) => Term a -> Term b -> State (S.HashSet SomeTerm) ()
    goUnary t arg = do
      b <- exists t
      if b
        then return ()
        else do
          add t
          go arg
    goBinary ::
      forall a b c.
      (SupportedPrim a, SupportedPrim b) =>
      Term a ->
      Term b ->
      Term c ->
      State (S.HashSet SomeTerm) ()
    goBinary t arg1 arg2 = do
      b <- exists t
      if b
        then return ()
        else do
          add t
          go arg1
          go arg2
    goTernary ::
      forall a b c d.
      (SupportedPrim a, SupportedPrim b, SupportedPrim c) =>
      Term a ->
      Term b ->
      Term c ->
      Term d ->
      State (S.HashSet SomeTerm) ()
    goTernary t arg1 arg2 arg3 = do
      b <- exists t
      if b
        then return ()
        else do
          add t
          go arg1
          go arg2
          go arg3
{-# INLINEABLE someTermsSize #-}

-- | Compute the size of a list of terms. Do not count the same term twice.
someTermSize :: SomeTerm -> Int
someTermSize term = someTermsSize [term]
{-# INLINE someTermSize #-}

-- | Compute the size of a list of terms. Do not count the same term twice.
termsSize :: [Term a] -> Int
termsSize terms = someTermsSize $ (\x -> introSupportedPrimConstraint x $ SomeTerm x) <$> terms
{-# INLINEABLE termsSize #-}

-- | Compute the size of a term.
termSize :: Term a -> Int
termSize term = termsSize [term]
{-# INLINE termSize #-}
