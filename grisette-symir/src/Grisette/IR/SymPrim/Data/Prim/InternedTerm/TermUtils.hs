{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.IR.SymPrim.Data.Prim.InternedTerm.TermUtils
  ( identity,
    identityWithTypeRep,
    introSupportedPrimConstraint,
    extractSymbolicsTerm,
    castTerm,
    pformat,
    termSize,
    termsSize,
  )
where

import Control.Monad.State
import Data.HashMap.Strict as M
import Data.HashSet as S
import Data.Interned
import Data.Typeable
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.SomeTerm
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Grisette.IR.SymPrim.Data.TabularFunc ()

identity :: Term t -> Id
identity (ConcTerm i _) = i
identity (SymbTerm i _) = i
identity (UnaryTerm i _ _) = i
identity (BinaryTerm i _ _ _) = i
identity (TernaryTerm i _ _ _ _) = i
identity (NotTerm i _) = i
identity (OrTerm i _ _) = i
identity (AndTerm i _ _) = i
identity (EqvTerm i _ _) = i
identity (ITETerm i _ _ _) = i
identity (AddNumTerm i _ _) = i
identity (UMinusNumTerm i _) = i
identity (TimesNumTerm i _ _) = i
identity (AbsNumTerm i _) = i
identity (SignumNumTerm i _) = i
identity (LTNumTerm i _ _) = i
identity (LENumTerm i _ _) = i
identity (AndBitsTerm i _ _) = i
identity (OrBitsTerm i _ _) = i
identity (XorBitsTerm i _ _) = i
identity (ComplementBitsTerm i _) = i
identity (ShiftBitsTerm i _ _) = i
identity (RotateBitsTerm i _ _) = i
identity (BVConcatTerm i _ _) = i
identity (BVSelectTerm i _ _ _) = i
identity (BVExtendTerm i _ _ _) = i
identity (TabularFuncApplyTerm i _ _) = i
identity (GeneralFuncApplyTerm i _ _) = i
identity (DivIntegerTerm i _ _) = i
identity (ModIntegerTerm i _ _) = i
{-# INLINE identity #-}

identityWithTypeRep :: forall t. Term t -> (TypeRep, Id)
identityWithTypeRep (ConcTerm i _) = (typeRep (Proxy @t), i)
identityWithTypeRep (SymbTerm i _) = (typeRep (Proxy @t), i)
identityWithTypeRep (UnaryTerm i _ _) = (typeRep (Proxy @t), i)
identityWithTypeRep (BinaryTerm i _ _ _) = (typeRep (Proxy @t), i)
identityWithTypeRep (TernaryTerm i _ _ _ _) = (typeRep (Proxy @t), i)
identityWithTypeRep (NotTerm i _) = (typeRep (Proxy @t), i)
identityWithTypeRep (OrTerm i _ _) = (typeRep (Proxy @t), i)
identityWithTypeRep (AndTerm i _ _) = (typeRep (Proxy @t), i)
identityWithTypeRep (EqvTerm i _ _) = (typeRep (Proxy @t), i)
identityWithTypeRep (ITETerm i _ _ _) = (typeRep (Proxy @t), i)
identityWithTypeRep (AddNumTerm i _ _) = (typeRep (Proxy @t), i)
identityWithTypeRep (UMinusNumTerm i _) = (typeRep (Proxy @t), i)
identityWithTypeRep (TimesNumTerm i _ _) = (typeRep (Proxy @t), i)
identityWithTypeRep (AbsNumTerm i _) = (typeRep (Proxy @t), i)
identityWithTypeRep (SignumNumTerm i _) = (typeRep (Proxy @t), i)
identityWithTypeRep (LTNumTerm i _ _) = (typeRep (Proxy @t), i)
identityWithTypeRep (LENumTerm i _ _) = (typeRep (Proxy @t), i)
identityWithTypeRep (AndBitsTerm i _ _) = (typeRep (Proxy @t), i)
identityWithTypeRep (OrBitsTerm i _ _) = (typeRep (Proxy @t), i)
identityWithTypeRep (XorBitsTerm i _ _) = (typeRep (Proxy @t), i)
identityWithTypeRep (ComplementBitsTerm i _) = (typeRep (Proxy @t), i)
identityWithTypeRep (ShiftBitsTerm i _ _) = (typeRep (Proxy @t), i)
identityWithTypeRep (RotateBitsTerm i _ _) = (typeRep (Proxy @t), i)
identityWithTypeRep (BVConcatTerm i _ _) = (typeRep (Proxy @t), i)
identityWithTypeRep (BVSelectTerm i _ _ _) = (typeRep (Proxy @t), i)
identityWithTypeRep (BVExtendTerm i _ _ _) = (typeRep (Proxy @t), i)
identityWithTypeRep (TabularFuncApplyTerm i _ _) = (typeRep (Proxy @t), i)
identityWithTypeRep (GeneralFuncApplyTerm i _ _) = (typeRep (Proxy @t), i)
identityWithTypeRep (DivIntegerTerm i _ _) = (typeRep (Proxy @t), i)
identityWithTypeRep (ModIntegerTerm i _ _) = (typeRep (Proxy @t), i)
{-# INLINE identityWithTypeRep #-}

introSupportedPrimConstraint :: forall t a. Term t -> ((SupportedPrim t) => a) -> a
introSupportedPrimConstraint ConcTerm {} x = x
introSupportedPrimConstraint SymbTerm {} x = x
introSupportedPrimConstraint UnaryTerm {} x = x
introSupportedPrimConstraint BinaryTerm {} x = x
introSupportedPrimConstraint TernaryTerm {} x = x
introSupportedPrimConstraint NotTerm {} x = x
introSupportedPrimConstraint OrTerm {} x = x
introSupportedPrimConstraint AndTerm {} x = x
introSupportedPrimConstraint EqvTerm {} x = x
introSupportedPrimConstraint ITETerm {} x = x
introSupportedPrimConstraint AddNumTerm {} x = x
introSupportedPrimConstraint UMinusNumTerm {} x = x
introSupportedPrimConstraint TimesNumTerm {} x = x
introSupportedPrimConstraint AbsNumTerm {} x = x
introSupportedPrimConstraint SignumNumTerm {} x = x
introSupportedPrimConstraint LTNumTerm {} x = x
introSupportedPrimConstraint LENumTerm {} x = x
introSupportedPrimConstraint AndBitsTerm {} x = x
introSupportedPrimConstraint OrBitsTerm {} x = x
introSupportedPrimConstraint XorBitsTerm {} x = x
introSupportedPrimConstraint ComplementBitsTerm {} x = x
introSupportedPrimConstraint ShiftBitsTerm {} x = x
introSupportedPrimConstraint RotateBitsTerm {} x = x
introSupportedPrimConstraint BVConcatTerm {} x = x
introSupportedPrimConstraint BVSelectTerm {} x = x
introSupportedPrimConstraint BVExtendTerm {} x = x
introSupportedPrimConstraint TabularFuncApplyTerm {} x = x
introSupportedPrimConstraint GeneralFuncApplyTerm {} x = x
introSupportedPrimConstraint DivIntegerTerm {} x = x
introSupportedPrimConstraint ModIntegerTerm {} x = x
{-# INLINE introSupportedPrimConstraint #-}

extractSymbolicsSomeTerm :: SomeTerm -> S.HashSet TermSymbol
extractSymbolicsSomeTerm t1 = evalState (gocached t1) M.empty
  where
    gocached :: SomeTerm -> State (M.HashMap SomeTerm (S.HashSet TermSymbol)) (S.HashSet TermSymbol)
    gocached t = do
      v <- gets (M.lookup t)
      case v of
        Just x -> return x
        Nothing -> do
          res <- go t
          st <- get
          put $ M.insert t res st
          return res
    go :: SomeTerm -> State (M.HashMap SomeTerm (S.HashSet TermSymbol)) (S.HashSet TermSymbol)
    go (SomeTerm ConcTerm {}) = return S.empty
    go (SomeTerm (SymbTerm _ symb)) = return $ S.singleton symb
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
    go (SomeTerm (ShiftBitsTerm _ arg _)) = goUnary arg
    go (SomeTerm (RotateBitsTerm _ arg _)) = goUnary arg
    go (SomeTerm (BVConcatTerm _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (BVSelectTerm _ _ _ arg)) = goUnary arg
    go (SomeTerm (BVExtendTerm _ _ _ arg)) = goUnary arg
    go (SomeTerm (TabularFuncApplyTerm _ func arg)) = goBinary func arg
    go (SomeTerm (GeneralFuncApplyTerm _ func arg)) = goBinary func arg
    go (SomeTerm (DivIntegerTerm _ arg1 arg2)) = goBinary arg1 arg2
    go (SomeTerm (ModIntegerTerm _ arg1 arg2)) = goBinary arg1 arg2
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

extractSymbolicsTerm :: (SupportedPrim a) => Term a -> S.HashSet TermSymbol
extractSymbolicsTerm t = extractSymbolicsSomeTerm (SomeTerm t)
{-# INLINE extractSymbolicsTerm #-}

castTerm :: forall a b. (Typeable b) => Term a -> Maybe (Term b)
castTerm t@ConcTerm {} = cast t
castTerm t@SymbTerm {} = cast t
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
castTerm t@ShiftBitsTerm {} = cast t
castTerm t@RotateBitsTerm {} = cast t
castTerm t@BVConcatTerm {} = cast t
castTerm t@BVSelectTerm {} = cast t
castTerm t@BVExtendTerm {} = cast t
castTerm t@TabularFuncApplyTerm {} = cast t
castTerm t@GeneralFuncApplyTerm {} = cast t
castTerm t@DivIntegerTerm {} = cast t
castTerm t@ModIntegerTerm {} = cast t
{-# INLINE castTerm #-}

pformat :: forall t. (SupportedPrim t) => Term t -> String
pformat (ConcTerm _ t) = pformatConc t
pformat (SymbTerm _ (TermSymbol _ symb)) = pformatSymb (Proxy @t) symb
pformat (UnaryTerm _ tag arg1) = pformatUnary tag arg1
pformat (BinaryTerm _ tag arg1 arg2) = pformatBinary tag arg1 arg2
pformat (TernaryTerm _ tag arg1 arg2 arg3) = pformatTernary tag arg1 arg2 arg3
pformat (NotTerm _ arg) = "(! " ++ pformat arg ++ ")"
pformat (OrTerm _ arg1 arg2) = "(|| " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (AndTerm _ arg1 arg2) = "(&& " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (EqvTerm _ arg1 arg2) = "(= " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (ITETerm _ cond arg1 arg2) = "(ite " ++ pformat cond ++ " " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (AddNumTerm _ arg1 arg2) = "(+ " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (UMinusNumTerm _ arg) = "(- " ++ pformat arg ++ ")"
pformat (TimesNumTerm _ arg1 arg2) = "(* " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (AbsNumTerm _ arg) = "(abs " ++ pformat arg ++ ")"
pformat (SignumNumTerm _ arg) = "(signum " ++ pformat arg ++ ")"
pformat (LTNumTerm _ arg1 arg2) = "(< " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (LENumTerm _ arg1 arg2) = "(<= " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (AndBitsTerm _ arg1 arg2) = "(& " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (OrBitsTerm _ arg1 arg2) = "(| " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (XorBitsTerm _ arg1 arg2) = "(^ " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (ComplementBitsTerm _ arg) = "(~ " ++ pformat arg ++ ")"
pformat (ShiftBitsTerm _ arg n) = "(shift " ++ pformat arg ++ " " ++ show n ++ ")"
pformat (RotateBitsTerm _ arg n) = "(rotate " ++ pformat arg ++ " " ++ show n ++ ")"
pformat (BVConcatTerm _ arg1 arg2) = "(bvconcat " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (BVSelectTerm _ ix w arg) = "(bvselect " ++ show ix ++ " " ++ show w ++ " " ++ pformat arg ++ ")"
pformat (BVExtendTerm _ signed n arg) =
  (if signed then "(bvsext " else "(bvzext") ++ show n ++ " " ++ pformat arg ++ ")"
pformat (TabularFuncApplyTerm _ func arg) = "(apply " ++ pformat func ++ " " ++ pformat arg ++ ")"
pformat (GeneralFuncApplyTerm _ func arg) = "(apply " ++ pformat func ++ " " ++ pformat arg ++ ")"
pformat (DivIntegerTerm _ arg1 arg2) = "(div " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (ModIntegerTerm _ arg1 arg2) = "(mod " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
{-# INLINE pformat #-}

termsSize :: [Term a] -> Int
termsSize terms = S.size $ execState (traverse go terms) S.empty
  where
    exists t = gets (S.member (SomeTerm t))
    add t = modify' (S.insert (SomeTerm t))
    go :: forall b. Term b -> State (S.HashSet SomeTerm) ()
    go t@ConcTerm {} = add t
    go t@SymbTerm {} = add t
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
    go t@(ShiftBitsTerm _ arg _) = goUnary t arg
    go t@(RotateBitsTerm _ arg _) = goUnary t arg
    go t@(BVConcatTerm _ arg1 arg2) = goBinary t arg1 arg2
    go t@(BVSelectTerm _ _ _ arg) = goUnary t arg
    go t@(BVExtendTerm _ _ _ arg) = goUnary t arg
    go t@(TabularFuncApplyTerm _ func arg) = goBinary t func arg
    go t@(GeneralFuncApplyTerm _ func arg) = goBinary t func arg
    go t@(DivIntegerTerm _ arg1 arg2) = goBinary t arg1 arg2
    go t@(ModIntegerTerm _ arg1 arg2) = goBinary t arg1 arg2
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
{-# INLINEABLE termsSize #-}

termSize :: Term a -> Int
termSize term = termsSize [term]
{-# INLINE termSize #-}
