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
import Grisette.Internal.Core.Data.MemoUtils (htmemo)
import Grisette.Internal.SymPrim.GeneralFun (type (-->) (GeneralFun))
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( IsSymbolKind (SymbolKindConstraint),
    SomeTypedConstantSymbol,
    SomeTypedSymbol (SomeTypedSymbol),
    SupportedPrim (castTypedSymbol, primTypeRep),
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
    introSupportedPrimConstraint,
    someTypedSymbol,
  )
import Grisette.Internal.SymPrim.Prim.SomeTerm
  ( SomeTerm (SomeTerm),
    someTerm,
  )
import Type.Reflection
  ( TypeRep,
    Typeable,
    eqTypeRep,
    typeRep,
    pattern App,
    type (:~~:) (HRefl),
  )

{-# NOINLINE extractSymSomeTerm #-}
extractSymSomeTerm ::
  forall knd.
  (IsSymbolKind knd) =>
  HS.HashSet (SomeTypedConstantSymbol) ->
  SomeTerm ->
  Maybe (HS.HashSet (SomeTypedSymbol knd))
extractSymSomeTerm initialBounded = go initialMemo initialBounded
  where
    gotyped ::
      ( SomeTerm ->
        Maybe (HS.HashSet (SomeTypedSymbol knd))
      ) ->
      Term a ->
      Maybe (HS.HashSet (SomeTypedSymbol knd))
    gotyped memo a =
      introSupportedPrimConstraint a $ memo (SomeTerm a)
    initialMemo ::
      SomeTerm ->
      Maybe (HS.HashSet (SomeTypedSymbol knd))
    initialMemo = htmemo (go initialMemo initialBounded)
    {-# NOINLINE initialMemo #-}

    go ::
      ( SomeTerm ->
        Maybe (HS.HashSet (SomeTypedSymbol knd))
      ) ->
      HS.HashSet (SomeTypedConstantSymbol) ->
      SomeTerm ->
      Maybe (HS.HashSet (SomeTypedSymbol knd))
    go _ bs (SomeTerm (SymTerm _ _ _ _ (sym :: TypedAnySymbol a))) =
      case (castTypedSymbol sym, castTypedSymbol sym) of
        (Just sym', _) | HS.member (someTypedSymbol sym') bs -> return HS.empty
        (_, Just sym') ->
          return $ HS.singleton $ SomeTypedSymbol sym'
        _ -> Nothing
    go _ bs (SomeTerm (ConTerm _ _ _ _ cv :: Term v)) =
      case (primTypeRep :: TypeRep v) of
        App (App gf _) _ ->
          case eqTypeRep (typeRep @(-->)) gf of
            Just HRefl -> case cv of
              GeneralFun sym (tm :: Term r) ->
                let newBounded = HS.union (HS.singleton (someTypedSymbol sym)) bs
                    newmemo = htmemo (go newmemo newBounded)
                    {-# NOINLINE newmemo #-}
                 in gotyped newmemo tm
            Nothing -> return HS.empty
        _ -> return HS.empty
    go _ bs (SomeTerm (ForallTerm _ _ _ _ sym arg)) =
      let newBounded = HS.insert (someTypedSymbol sym) bs
          newmemo = htmemo (go newmemo newBounded)
          {-# NOINLINE newmemo #-}
       in goUnary newmemo arg
    go _ bs (SomeTerm (ExistsTerm _ _ _ _ sym arg)) =
      let newBounded = HS.insert (someTypedSymbol sym) bs
          newmemo = htmemo (go newmemo newBounded)
          {-# NOINLINE newmemo #-}
       in goUnary newmemo arg
    go memo _ (SomeTerm (NotTerm _ _ _ _ arg)) = goUnary memo arg
    go memo _ (SomeTerm (OrTerm _ _ _ _ arg1 arg2)) = goBinary memo arg1 arg2
    go memo _ (SomeTerm (AndTerm _ _ _ _ arg1 arg2)) = goBinary memo arg1 arg2
    go memo _ (SomeTerm (EqTerm _ _ _ _ arg1 arg2)) = goBinary memo arg1 arg2
    go memo _ (SomeTerm (DistinctTerm _ _ _ _ args)) =
      combineAllSets $ map (gotyped memo) $ toList args
    go memo _ (SomeTerm (ITETerm _ _ _ _ cond arg1 arg2)) =
      goTernary memo cond arg1 arg2
    go memo _ (SomeTerm (AddNumTerm _ _ _ _ arg1 arg2)) = goBinary memo arg1 arg2
    go memo _ (SomeTerm (NegNumTerm _ _ _ _ arg)) = goUnary memo arg
    go memo _ (SomeTerm (MulNumTerm _ _ _ _ arg1 arg2)) = goBinary memo arg1 arg2
    go memo _ (SomeTerm (AbsNumTerm _ _ _ _ arg)) = goUnary memo arg
    go memo _ (SomeTerm (SignumNumTerm _ _ _ _ arg)) = goUnary memo arg
    go memo _ (SomeTerm (LtOrdTerm _ _ _ _ arg1 arg2)) = goBinary memo arg1 arg2
    go memo _ (SomeTerm (LeOrdTerm _ _ _ _ arg1 arg2)) = goBinary memo arg1 arg2
    go memo _ (SomeTerm (AndBitsTerm _ _ _ _ arg1 arg2)) = goBinary memo arg1 arg2
    go memo _ (SomeTerm (OrBitsTerm _ _ _ _ arg1 arg2)) = goBinary memo arg1 arg2
    go memo _ (SomeTerm (XorBitsTerm _ _ _ _ arg1 arg2)) = goBinary memo arg1 arg2
    go memo _ (SomeTerm (ComplementBitsTerm _ _ _ _ arg)) = goUnary memo arg
    go memo _ (SomeTerm (ShiftLeftTerm _ _ _ _ arg n1)) = goBinary memo arg n1
    go memo _ (SomeTerm (ShiftRightTerm _ _ _ _ arg n1)) = goBinary memo arg n1
    go memo _ (SomeTerm (RotateLeftTerm _ _ _ _ arg n1)) = goBinary memo arg n1
    go memo _ (SomeTerm (RotateRightTerm _ _ _ _ arg n1)) = goBinary memo arg n1
    go memo _ (SomeTerm (BitCastTerm _ _ _ _ arg)) = goUnary memo arg
    go memo _ (SomeTerm (BitCastOrTerm _ _ _ _ d arg)) = goBinary memo d arg
    go memo _ (SomeTerm (BVConcatTerm _ _ _ _ arg1 arg2)) =
      goBinary memo arg1 arg2
    go memo _ (SomeTerm (BVSelectTerm _ _ _ _ _ _ arg)) = goUnary memo arg
    go memo _ (SomeTerm (BVExtendTerm _ _ _ _ _ _ arg)) = goUnary memo arg
    go memo _ (SomeTerm (ApplyTerm _ _ _ _ func arg)) = goBinary memo func arg
    go memo _ (SomeTerm (DivIntegralTerm _ _ _ _ arg1 arg2)) =
      goBinary memo arg1 arg2
    go memo _ (SomeTerm (ModIntegralTerm _ _ _ _ arg1 arg2)) =
      goBinary memo arg1 arg2
    go memo _ (SomeTerm (QuotIntegralTerm _ _ _ _ arg1 arg2)) =
      goBinary memo arg1 arg2
    go memo _ (SomeTerm (RemIntegralTerm _ _ _ _ arg1 arg2)) =
      goBinary memo arg1 arg2
    go memo _ (SomeTerm (FPTraitTerm _ _ _ _ _ arg)) = goUnary memo arg
    go memo _ (SomeTerm (FdivTerm _ _ _ _ arg1 arg2)) = goBinary memo arg1 arg2
    go memo _ (SomeTerm (RecipTerm _ _ _ _ arg)) = goUnary memo arg
    go memo _ (SomeTerm (FloatingUnaryTerm _ _ _ _ _ arg)) = goUnary memo arg
    go memo _ (SomeTerm (PowerTerm _ _ _ _ arg1 arg2)) = goBinary memo arg1 arg2
    go memo _ (SomeTerm (FPUnaryTerm _ _ _ _ _ arg)) = goUnary memo arg
    go memo _ (SomeTerm (FPBinaryTerm _ _ _ _ _ arg1 arg2)) =
      goBinary memo arg1 arg2
    go memo _ (SomeTerm (FPRoundingUnaryTerm _ _ _ _ _ _ arg)) = goUnary memo arg
    go memo _ (SomeTerm (FPRoundingBinaryTerm _ _ _ _ _ _ arg1 arg2)) =
      goBinary memo arg1 arg2
    go memo _ (SomeTerm (FPFMATerm _ _ _ _ mode arg1 arg2 arg3)) =
      combineAllSets
        [ gotyped memo mode,
          gotyped memo arg1,
          gotyped memo arg2,
          gotyped memo arg3
        ]
    go memo _ (SomeTerm (FromIntegralTerm _ _ _ _ arg)) = goUnary memo arg
    go memo _ (SomeTerm (FromFPOrTerm _ _ _ _ d mode arg)) =
      goTernary memo d mode arg
    go memo _ (SomeTerm (ToFPTerm _ _ _ _ mode arg _ _)) = goBinary memo mode arg
    goUnary ::
      (SomeTerm -> Maybe (HS.HashSet (SomeTypedSymbol knd))) ->
      Term a ->
      Maybe (HS.HashSet (SomeTypedSymbol knd))
    goUnary = gotyped
    goBinary ::
      (SomeTerm -> Maybe (HS.HashSet (SomeTypedSymbol knd))) ->
      Term a ->
      Term b ->
      Maybe (HS.HashSet (SomeTypedSymbol knd))
    goBinary memo arg1 arg2 =
      combineSet (gotyped memo arg1) (gotyped memo arg2)
    goTernary ::
      (SomeTerm -> Maybe (HS.HashSet (SomeTypedSymbol knd))) ->
      Term a ->
      Term b ->
      Term c ->
      Maybe (HS.HashSet (SomeTypedSymbol knd))
    goTernary memo arg1 arg2 arg3 =
      combineAllSets
        [ gotyped memo arg1,
          gotyped memo arg2,
          gotyped memo arg3
        ]
    combineSet (Just a) (Just b) = Just $ HS.union a b
    combineSet _ _ = Nothing
    combineAllSets = foldl1 combineSet

-- | Extract all the symbols in a term.
extractTerm ::
  (IsSymbolKind knd, SymbolKindConstraint knd a, SupportedPrim a) =>
  HS.HashSet (SomeTypedConstantSymbol) ->
  Term a ->
  Maybe (HS.HashSet (SomeTypedSymbol knd))
extractTerm initialBoundedSymbols t =
  extractSymSomeTerm initialBoundedSymbols (SomeTerm t)
{-# NOINLINE extractTerm #-}

-- | Cast a term to another type.
castTerm :: forall a b. (Typeable b) => Term a -> Maybe (Term b)
castTerm t = introSupportedPrimConstraint t $ cast t
{-# INLINE castTerm #-}

-- | Compute the size of a list of terms. Do not count the same term twice.
someTermsSize :: [SomeTerm] -> Int
someTermsSize terms = HS.size $ execState (traverse goSome terms) HS.empty
  where
    exists t = gets (HS.member (someTerm t))
    add t = modify' (HS.insert (someTerm t))
    goSome :: SomeTerm -> State (HS.HashSet SomeTerm) ()
    goSome (SomeTerm b) = go b
    go :: forall b. Term b -> State (HS.HashSet SomeTerm) ()
    go t@ConTerm {} = add t
    go t@SymTerm {} = add t
    go t@(ForallTerm _ _ _ _ _ arg) = goUnary t arg
    go t@(ExistsTerm _ _ _ _ _ arg) = goUnary t arg
    go t@(NotTerm _ _ _ _ arg) = goUnary t arg
    go t@(OrTerm _ _ _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(AndTerm _ _ _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(EqTerm _ _ _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(DistinctTerm _ _ _ _ args) = do
      b <- exists t
      if b
        then return ()
        else do
          add t
          traverse_ go args
    go t@(ITETerm _ _ _ _ cond arg1 arg2) = goTernary t cond arg1 arg2
    go t@(AddNumTerm _ _ _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(NegNumTerm _ _ _ _ arg) = goUnary t arg
    go t@(MulNumTerm _ _ _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(AbsNumTerm _ _ _ _ arg) = goUnary t arg
    go t@(SignumNumTerm _ _ _ _ arg) = goUnary t arg
    go t@(LtOrdTerm _ _ _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(LeOrdTerm _ _ _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(AndBitsTerm _ _ _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(OrBitsTerm _ _ _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(XorBitsTerm _ _ _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(ComplementBitsTerm _ _ _ _ arg) = goUnary t arg
    go t@(ShiftLeftTerm _ _ _ _ arg n) = goBinary t arg n
    go t@(ShiftRightTerm _ _ _ _ arg n) = goBinary t arg n
    go t@(RotateLeftTerm _ _ _ _ arg n) = goBinary t arg n
    go t@(RotateRightTerm _ _ _ _ arg n) = goBinary t arg n
    go t@(BitCastTerm _ _ _ _ arg) = goUnary t arg
    go t@(BitCastOrTerm _ _ _ _ d arg) = goBinary t d arg
    go t@(BVConcatTerm _ _ _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(BVSelectTerm _ _ _ _ _ _ arg) = goUnary t arg
    go t@(BVExtendTerm _ _ _ _ _ _ arg) = goUnary t arg
    go t@(ApplyTerm _ _ _ _ func arg) = goBinary t func arg
    go t@(DivIntegralTerm _ _ _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(ModIntegralTerm _ _ _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(QuotIntegralTerm _ _ _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(RemIntegralTerm _ _ _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(FPTraitTerm _ _ _ _ _ arg) = goUnary t arg
    go t@(FdivTerm _ _ _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(RecipTerm _ _ _ _ arg) = goUnary t arg
    go t@(FloatingUnaryTerm _ _ _ _ _ arg) = goUnary t arg
    go t@(PowerTerm _ _ _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(FPUnaryTerm _ _ _ _ _ arg) = goUnary t arg
    go t@(FPBinaryTerm _ _ _ _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(FPRoundingUnaryTerm _ _ _ _ _ _ arg) = goUnary t arg
    go t@(FPRoundingBinaryTerm _ _ _ _ _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(FPFMATerm _ _ _ _ _ arg1 arg2 arg3) = goTernary t arg1 arg2 arg3
    go t@(FromIntegralTerm _ _ _ _ arg) = goUnary t arg
    go t@(FromFPOrTerm _ _ _ _ d mode arg) =
      goTernary t d mode arg
    go t@(ToFPTerm _ _ _ _ mode arg _ _) = goBinary t mode arg
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
