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
import Data.Foldable (Foldable (toList), traverse_)
import qualified Data.HashSet as HS
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
        TernaryTerm,
        ToFPTerm,
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
  HS.HashSet (SomeTypedConstantSymbol) ->
  SomeTerm ->
  Maybe (HS.HashSet (SomeTypedSymbol knd))
extractSymSomeTerm = go initialMemo
  where
    gotyped ::
      (SupportedPrim a) =>
      ( HS.HashSet (SomeTypedConstantSymbol) ->
        SomeTerm ->
        Maybe (HS.HashSet (SomeTypedSymbol knd))
      ) ->
      HS.HashSet (SomeTypedConstantSymbol) ->
      Term a ->
      Maybe (HS.HashSet (SomeTypedSymbol knd))
    gotyped memo boundedSymbols a = memo boundedSymbols (SomeTerm a)
    initialMemo ::
      HS.HashSet (SomeTypedConstantSymbol) ->
      SomeTerm ->
      Maybe (HS.HashSet (SomeTypedSymbol knd))
    initialMemo = htmemo2 (go initialMemo)
    {-# NOINLINE initialMemo #-}

    go ::
      ( HS.HashSet (SomeTypedConstantSymbol) ->
        SomeTerm ->
        Maybe (HS.HashSet (SomeTypedSymbol knd))
      ) ->
      HS.HashSet (SomeTypedConstantSymbol) ->
      SomeTerm ->
      Maybe (HS.HashSet (SomeTypedSymbol knd))
    go _ bs (SomeTerm (SymTerm _ _ (sym :: TypedAnySymbol a))) =
      case (castTypedSymbol sym, castTypedSymbol sym) of
        (Just sym', _) | HS.member (someTypedSymbol sym') bs -> return HS.empty
        (_, Just sym') ->
          return $ HS.singleton $ SomeTypedSymbol (R.typeRep @a) sym'
        _ -> Nothing
    go _ bs (SomeTerm (ConTerm _ _ cv :: Term v)) =
      case (typeRep :: TypeRep v) of
        App (App gf _) _ ->
          case eqTypeRep (typeRep @(-->)) gf of
            Just HRefl -> case cv of
              GeneralFun sym (tm :: Term r) ->
                let newmemo = htmemo2 (go newmemo)
                    {-# NOINLINE newmemo #-}
                 in gotyped
                      newmemo
                      (HS.union (HS.singleton (someTypedSymbol sym)) bs)
                      tm
            Nothing -> return HS.empty
        _ -> return HS.empty
    go _ bs (SomeTerm (ForallTerm _ _ sym arg)) =
      let newmemo = htmemo2 (go newmemo)
          {-# NOINLINE newmemo #-}
       in goUnary newmemo (HS.insert (someTypedSymbol sym) bs) arg
    go _ bs (SomeTerm (ExistsTerm _ _ sym arg)) =
      let newmemo = htmemo2 (go newmemo)
          {-# NOINLINE newmemo #-}
       in goUnary newmemo (HS.insert (someTypedSymbol sym) bs) arg
    go memo bs (SomeTerm (UnaryTerm _ _ _ arg)) = goUnary memo bs arg
    go memo bs (SomeTerm (BinaryTerm _ _ _ arg1 arg2)) =
      goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (TernaryTerm _ _ _ arg1 arg2 arg3)) =
      goTernary memo bs arg1 arg2 arg3
    go memo bs (SomeTerm (NotTerm _ _ arg)) = goUnary memo bs arg
    go memo bs (SomeTerm (OrTerm _ _ arg1 arg2)) = goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (AndTerm _ _ arg1 arg2)) = goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (EqTerm _ _ arg1 arg2)) = goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (DistinctTerm _ _ args)) =
      combineAllSets $ map (gotyped memo bs) $ toList args
    go memo bs (SomeTerm (ITETerm _ _ cond arg1 arg2)) =
      goTernary memo bs cond arg1 arg2
    go memo bs (SomeTerm (AddNumTerm _ _ arg1 arg2)) = goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (NegNumTerm _ _ arg)) = goUnary memo bs arg
    go memo bs (SomeTerm (MulNumTerm _ _ arg1 arg2)) = goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (AbsNumTerm _ _ arg)) = goUnary memo bs arg
    go memo bs (SomeTerm (SignumNumTerm _ _ arg)) = goUnary memo bs arg
    go memo bs (SomeTerm (LtOrdTerm _ _ arg1 arg2)) = goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (LeOrdTerm _ _ arg1 arg2)) = goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (AndBitsTerm _ _ arg1 arg2)) = goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (OrBitsTerm _ _ arg1 arg2)) = goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (XorBitsTerm _ _ arg1 arg2)) = goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (ComplementBitsTerm _ _ arg)) = goUnary memo bs arg
    go memo bs (SomeTerm (ShiftLeftTerm _ _ arg n1)) = goBinary memo bs arg n1
    go memo bs (SomeTerm (ShiftRightTerm _ _ arg n1)) = goBinary memo bs arg n1
    go memo bs (SomeTerm (RotateLeftTerm _ _ arg n1)) = goBinary memo bs arg n1
    go memo bs (SomeTerm (RotateRightTerm _ _ arg n1)) = goBinary memo bs arg n1
    go memo bs (SomeTerm (BitCastTerm _ _ arg)) = goUnary memo bs arg
    go memo bs (SomeTerm (BitCastOrTerm _ _ d arg)) = goBinary memo bs d arg
    go memo bs (SomeTerm (BVConcatTerm _ _ arg1 arg2)) =
      goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (BVSelectTerm _ _ _ _ arg)) = goUnary memo bs arg
    go memo bs (SomeTerm (BVExtendTerm _ _ _ _ arg)) = goUnary memo bs arg
    go memo bs (SomeTerm (ApplyTerm _ _ func arg)) = goBinary memo bs func arg
    go memo bs (SomeTerm (DivIntegralTerm _ _ arg1 arg2)) =
      goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (ModIntegralTerm _ _ arg1 arg2)) =
      goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (QuotIntegralTerm _ _ arg1 arg2)) =
      goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (RemIntegralTerm _ _ arg1 arg2)) =
      goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (FPTraitTerm _ _ _ arg)) = goUnary memo bs arg
    go memo bs (SomeTerm (FdivTerm _ _ arg1 arg2)) = goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (RecipTerm _ _ arg)) = goUnary memo bs arg
    go memo bs (SomeTerm (FloatingUnaryTerm _ _ _ arg)) = goUnary memo bs arg
    go memo bs (SomeTerm (PowerTerm _ _ arg1 arg2)) = goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (FPUnaryTerm _ _ _ arg)) = goUnary memo bs arg
    go memo bs (SomeTerm (FPBinaryTerm _ _ _ arg1 arg2)) =
      goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (FPRoundingUnaryTerm _ _ _ _ arg)) = goUnary memo bs arg
    go memo bs (SomeTerm (FPRoundingBinaryTerm _ _ _ _ arg1 arg2)) =
      goBinary memo bs arg1 arg2
    go memo bs (SomeTerm (FPFMATerm _ _ mode arg1 arg2 arg3)) =
      combineAllSets
        [ gotyped memo bs mode,
          gotyped memo bs arg1,
          gotyped memo bs arg2,
          gotyped memo bs arg3
        ]
    go memo bs (SomeTerm (FromIntegralTerm _ _ arg)) = goUnary memo bs arg
    go memo bs (SomeTerm (FromFPOrTerm _ _ d mode arg)) =
      goTernary memo bs d mode arg
    go memo bs (SomeTerm (ToFPTerm _ _ mode arg _ _)) = goBinary memo bs mode arg
    goUnary ::
      (SupportedPrim a) =>
      (HS.HashSet (SomeTypedConstantSymbol) -> SomeTerm -> Maybe (HS.HashSet (SomeTypedSymbol knd))) ->
      HS.HashSet (SomeTypedConstantSymbol) ->
      Term a ->
      Maybe (HS.HashSet (SomeTypedSymbol knd))
    goUnary = gotyped
    goBinary ::
      (SupportedPrim a, SupportedPrim b) =>
      (HS.HashSet (SomeTypedConstantSymbol) -> SomeTerm -> Maybe (HS.HashSet (SomeTypedSymbol knd))) ->
      HS.HashSet (SomeTypedConstantSymbol) ->
      Term a ->
      Term b ->
      Maybe (HS.HashSet (SomeTypedSymbol knd))
    goBinary memo bs arg1 arg2 =
      combineSet (gotyped memo bs arg1) (gotyped memo bs arg2)
    goTernary ::
      (SupportedPrim a, SupportedPrim b, SupportedPrim c) =>
      (HS.HashSet (SomeTypedConstantSymbol) -> SomeTerm -> Maybe (HS.HashSet (SomeTypedSymbol knd))) ->
      HS.HashSet (SomeTypedConstantSymbol) ->
      Term a ->
      Term b ->
      Term c ->
      Maybe (HS.HashSet (SomeTypedSymbol knd))
    goTernary memo bs arg1 arg2 arg3 =
      combineAllSets
        [ gotyped memo bs arg1,
          gotyped memo bs arg2,
          gotyped memo bs arg3
        ]
    combineSet (Just a) (Just b) = Just $ HS.union a b
    combineSet _ _ = Nothing
    combineAllSets = foldl1 combineSet
{-# INLINEABLE extractSymSomeTerm #-}

-- | Extract all the symbols in a term.
extractTerm ::
  (IsSymbolKind knd, SymbolKindConstraint knd a, SupportedPrim a) =>
  HS.HashSet (SomeTypedConstantSymbol) ->
  Term a ->
  Maybe (HS.HashSet (SomeTypedSymbol knd))
extractTerm initialBoundedSymbols t =
  extractSymSomeTerm initialBoundedSymbols (SomeTerm t)
{-# INLINE extractTerm #-}

-- | Cast a term to another type.
castTerm :: forall a b. (Typeable b) => Term a -> Maybe (Term b)
castTerm t = introSupportedPrimConstraint t $ cast t
{-# INLINE castTerm #-}

-- | Compute the size of a list of terms. Do not count the same term twice.
someTermsSize :: [SomeTerm] -> Int
someTermsSize terms = HS.size $ execState (traverse goSome terms) HS.empty
  where
    exists t = gets (HS.member (SomeTerm t))
    add t = modify' (HS.insert (SomeTerm t))
    goSome :: SomeTerm -> State (HS.HashSet SomeTerm) ()
    goSome (SomeTerm b) = go b
    go :: forall b. Term b -> State (HS.HashSet SomeTerm) ()
    go t@ConTerm {} = add t
    go t@SymTerm {} = add t
    go t@(ForallTerm _ _ _ arg) = goUnary t arg
    go t@(ExistsTerm _ _ _ arg) = goUnary t arg
    go t@(UnaryTerm _ _ _ arg) = goUnary t arg
    go t@(BinaryTerm _ _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(TernaryTerm _ _ _ arg1 arg2 arg3) = goTernary t arg1 arg2 arg3
    go t@(NotTerm _ _ arg) = goUnary t arg
    go t@(OrTerm _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(AndTerm _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(EqTerm _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(DistinctTerm _ _ args) = do
      b <- exists t
      if b
        then return ()
        else do
          add t
          traverse_ go args
    go t@(ITETerm _ _ cond arg1 arg2) = goTernary t cond arg1 arg2
    go t@(AddNumTerm _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(NegNumTerm _ _ arg) = goUnary t arg
    go t@(MulNumTerm _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(AbsNumTerm _ _ arg) = goUnary t arg
    go t@(SignumNumTerm _ _ arg) = goUnary t arg
    go t@(LtOrdTerm _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(LeOrdTerm _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(AndBitsTerm _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(OrBitsTerm _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(XorBitsTerm _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(ComplementBitsTerm _ _ arg) = goUnary t arg
    go t@(ShiftLeftTerm _ _ arg n) = goBinary t arg n
    go t@(ShiftRightTerm _ _ arg n) = goBinary t arg n
    go t@(RotateLeftTerm _ _ arg n) = goBinary t arg n
    go t@(RotateRightTerm _ _ arg n) = goBinary t arg n
    go t@(BitCastTerm _ _ arg) = goUnary t arg
    go t@(BitCastOrTerm _ _ d arg) = goBinary t d arg
    go t@(BVConcatTerm _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(BVSelectTerm _ _ _ _ arg) = goUnary t arg
    go t@(BVExtendTerm _ _ _ _ arg) = goUnary t arg
    go t@(ApplyTerm _ _ func arg) = goBinary t func arg
    go t@(DivIntegralTerm _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(ModIntegralTerm _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(QuotIntegralTerm _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(RemIntegralTerm _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(FPTraitTerm _ _ _ arg) = goUnary t arg
    go t@(FdivTerm _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(RecipTerm _ _ arg) = goUnary t arg
    go t@(FloatingUnaryTerm _ _ _ arg) = goUnary t arg
    go t@(PowerTerm _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(FPUnaryTerm _ _ _ arg) = goUnary t arg
    go t@(FPBinaryTerm _ _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(FPRoundingUnaryTerm _ _ _ _ arg) = goUnary t arg
    go t@(FPRoundingBinaryTerm _ _ _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(FPFMATerm _ _ _ arg1 arg2 arg3) = goTernary t arg1 arg2 arg3
    go t@(FromIntegralTerm _ _ arg) = goUnary t arg
    go t@(FromFPOrTerm _ _ d mode arg) =
      goTernary t d mode arg
    go t@(ToFPTerm _ _ mode arg _ _) = goBinary t mode arg
    goUnary :: forall a b. (SupportedPrim a) => Term a -> Term b -> State (HS.HashSet SomeTerm) ()
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
      State (HS.HashSet SomeTerm) ()
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
      State (HS.HashSet SomeTerm) ()
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
termsSize terms =
  someTermsSize $
    (\x -> introSupportedPrimConstraint x $ SomeTerm x) <$> terms
{-# INLINEABLE termsSize #-}

-- | Compute the size of a term.
termSize :: Term a -> Int
termSize term = termsSize [term]
{-# INLINE termSize #-}
