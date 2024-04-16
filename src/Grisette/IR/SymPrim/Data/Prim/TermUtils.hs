{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.Prim.TermUtils
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.Prim.TermUtils
  ( extractSymbolicsTerm,
    castTerm,
    someTermsSize,
    someTermSize,
    termSize,
    termsSize,
  )
where

import Control.Monad.State
  ( MonadState (get, put),
    State,
    evalState,
    execState,
    gets,
    modify',
  )
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.Typeable
  ( Typeable,
    cast,
  )
import Grisette.IR.SymPrim.Data.Prim.SomeTerm
  ( SomeTerm (SomeTerm),
  )
import Grisette.IR.SymPrim.Data.Prim.Term
  ( SomeTypedSymbol (SomeTypedSymbol),
    SupportedPrim,
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
        TernaryTerm,
        TimesNumTerm,
        ToSignedTerm,
        ToUnsignedTerm,
        UMinusNumTerm,
        UnaryTerm,
        XorBitsTerm
      ),
    TypedSymbol,
    introSupportedPrimConstraint,
  )
import qualified Type.Reflection as R

extractSymbolicsSomeTerm :: SomeTerm -> S.HashSet SomeTypedSymbol
extractSymbolicsSomeTerm t1 = evalState (gocached t1) M.empty
  where
    gocached :: SomeTerm -> State (M.HashMap SomeTerm (S.HashSet SomeTypedSymbol)) (S.HashSet SomeTypedSymbol)
    gocached t = do
      v <- gets (M.lookup t)
      case v of
        Just x -> return x
        Nothing -> do
          res <- go t
          st <- get
          put $ M.insert t res st
          return res
    go :: SomeTerm -> State (M.HashMap SomeTerm (S.HashSet SomeTypedSymbol)) (S.HashSet SomeTypedSymbol)
    go (SomeTerm ConTerm {}) = return S.empty
    go (SomeTerm (SymTerm _ (sym :: TypedSymbol a))) = return $ S.singleton $ SomeTypedSymbol (R.typeRep @a) sym
    go (SomeTerm (UnaryTerm _ _ arg)) = goUnary arg
    go (SomeTerm (BinaryTerm _ _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (TernaryTerm _ _ arg1 arg2 arg3)) = goTernary arg1 arg2 arg3
    go (SomeTerm (NotTerm _ arg)) = goUnary arg
    go (SomeTerm (OrTerm _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (AndTerm _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (EqvTerm _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (ITETerm _ cond arg1 arg2)) = goTernary cond arg1 arg2
    go (SomeTerm (AddNumTerm _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (UMinusNumTerm _ arg)) = goUnary arg
    go (SomeTerm (TimesNumTerm _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (AbsNumTerm _ arg)) = goUnary arg
    go (SomeTerm (SignumNumTerm _ arg)) = goUnary arg
    go (SomeTerm (LTNumTerm _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (LENumTerm _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (AndBitsTerm _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (OrBitsTerm _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (XorBitsTerm _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (ComplementBitsTerm _ arg)) = goUnary arg
    go (SomeTerm (ShiftLeftTerm _ arg n1)) = goBinary arg n1
    go (SomeTerm (ShiftRightTerm _ arg n1)) = goBinary arg n1
    go (SomeTerm (RotateLeftTerm _ arg n1)) = goBinary arg n1
    go (SomeTerm (RotateRightTerm _ arg n1)) = goBinary arg n1
    go (SomeTerm (ToSignedTerm _ arg)) = goUnary arg
    go (SomeTerm (ToUnsignedTerm _ arg)) = goUnary arg
    go (SomeTerm (BVConcatTerm _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (BVSelectTerm _ _ _ arg)) = goUnary arg
    go (SomeTerm (BVExtendTerm _ _ _ arg)) = goUnary arg
    go (SomeTerm (ApplyTerm _ func arg)) = goBinary func arg
    go (SomeTerm (DivIntegralTerm _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (ModIntegralTerm _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (QuotIntegralTerm _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (RemIntegralTerm _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (DivBoundedIntegralTerm _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (ModBoundedIntegralTerm _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (QuotBoundedIntegralTerm _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (RemBoundedIntegralTerm _ arg1 arg2)) = goBinary arg1 arg2
    goUnary arg = gocached (SomeTerm arg)
    goBinary arg1 arg2 = do
      r1 <- gocached (SomeTerm arg1)
      r2 <- gocached (SomeTerm arg2)
      return $ r1 <> r2
    goTernary arg1 arg2 arg3 = do
      r1 <- gocached (SomeTerm arg1)
      r2 <- gocached (SomeTerm arg2)
      r3 <- gocached (SomeTerm arg3)
      return $ r1 <> r2 <> r3
{-# INLINEABLE extractSymbolicsSomeTerm #-}

extractSymbolicsTerm :: (SupportedPrim a) => Term a -> S.HashSet SomeTypedSymbol
extractSymbolicsTerm t = extractSymbolicsSomeTerm (SomeTerm t)
{-# INLINE extractSymbolicsTerm #-}

castTerm :: forall a b. (Typeable b) => Term a -> Maybe (Term b)
castTerm t@ConTerm {} = cast t
castTerm t@SymTerm {} = cast t
castTerm t@UnaryTerm {} = cast t
castTerm t@BinaryTerm {} = cast t
castTerm t@TernaryTerm {} = cast t
castTerm t@NotTerm {} = cast t
castTerm t@OrTerm {} = cast t
castTerm t@AndTerm {} = cast t
castTerm t@EqvTerm {} = cast t
castTerm t@ITETerm {} = cast t
castTerm t@AddNumTerm {} = cast t
castTerm t@UMinusNumTerm {} = cast t
castTerm t@TimesNumTerm {} = cast t
castTerm t@AbsNumTerm {} = cast t
castTerm t@SignumNumTerm {} = cast t
castTerm t@LTNumTerm {} = cast t
castTerm t@LENumTerm {} = cast t
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
castTerm t@DivBoundedIntegralTerm {} = cast t
castTerm t@ModBoundedIntegralTerm {} = cast t
castTerm t@QuotBoundedIntegralTerm {} = cast t
castTerm t@RemBoundedIntegralTerm {} = cast t
{-# INLINE castTerm #-}

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
    go t@(UnaryTerm _ _ arg) = goUnary t arg
    go t@(BinaryTerm _ _ arg1 arg2) = goBinary t arg1 arg2
    go t@(TernaryTerm _ _ arg1 arg2 arg3) = goTernary t arg1 arg2 arg3
    go t@(NotTerm _ arg) = goUnary t arg
    go t@(OrTerm _ arg1 arg2) = goBinary t arg1 arg2
    go t@(AndTerm _ arg1 arg2) = goBinary t arg1 arg2
    go t@(EqvTerm _ arg1 arg2) = goBinary t arg1 arg2
    go t@(ITETerm _ cond arg1 arg2) = goTernary t cond arg1 arg2
    go t@(AddNumTerm _ arg1 arg2) = goBinary t arg1 arg2
    go t@(UMinusNumTerm _ arg) = goUnary t arg
    go t@(TimesNumTerm _ arg1 arg2) = goBinary t arg1 arg2
    go t@(AbsNumTerm _ arg) = goUnary t arg
    go t@(SignumNumTerm _ arg) = goUnary t arg
    go t@(LTNumTerm _ arg1 arg2) = goBinary t arg1 arg2
    go t@(LENumTerm _ arg1 arg2) = goBinary t arg1 arg2
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
    go t@(DivBoundedIntegralTerm _ arg1 arg2) = goBinary t arg1 arg2
    go t@(ModBoundedIntegralTerm _ arg1 arg2) = goBinary t arg1 arg2
    go t@(QuotBoundedIntegralTerm _ arg1 arg2) = goBinary t arg1 arg2
    go t@(RemBoundedIntegralTerm _ arg1 arg2) = goBinary t arg1 arg2
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

someTermSize :: SomeTerm -> Int
someTermSize term = someTermsSize [term]
{-# INLINE someTermSize #-}

termsSize :: [Term a] -> Int
termsSize terms = someTermsSize $ (\x -> introSupportedPrimConstraint x $ SomeTerm x) <$> terms
{-# INLINEABLE termsSize #-}

termSize :: Term a -> Int
termSize term = termsSize [term]
{-# INLINE termSize #-}
