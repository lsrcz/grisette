{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Pizza.IR.SymPrim.Data.Prim.Model
  ( Model (..),
    equation,
    empty,
    valueOf,
    exceptFor,
    restrictTo,
    extendTo,
    exact,
    insert,
    evaluateTerm,
  )
where

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.Hashable
import Data.Proxy
import GHC.Generics
import Pizza.Core.Data.MemoUtils
import Pizza.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Pizza.IR.SymPrim.Data.Prim.InternedTerm.SomeTerm
import Pizza.IR.SymPrim.Data.Prim.InternedTerm.Term
import Pizza.IR.SymPrim.Data.Prim.ModelValue
import Pizza.IR.SymPrim.Data.Prim.PartialEval.BV
import Pizza.IR.SymPrim.Data.Prim.PartialEval.Bits
import Pizza.IR.SymPrim.Data.Prim.PartialEval.Bool
import Pizza.IR.SymPrim.Data.Prim.PartialEval.GeneralFunc
import Pizza.IR.SymPrim.Data.Prim.PartialEval.Integer
import Pizza.IR.SymPrim.Data.Prim.PartialEval.Num
import Pizza.IR.SymPrim.Data.Prim.PartialEval.TabularFunc
import Type.Reflection
import Unsafe.Coerce

newtype Model = Model (M.HashMap TermSymbol ModelValue) deriving (Show, Eq, Generic, Hashable)

equation :: Model -> TermSymbol -> Maybe (Term Bool)
equation m tsym@(TermSymbol (_ :: TypeRep a) sym) =
  case valueOf m tsym of
    Just (v :: a) -> Just $ pevalEqvTerm (symbTerm sym) (concTerm v)
    Nothing -> Nothing

empty :: Model
empty = Model M.empty

valueOf :: forall t. (Typeable t) => Model -> TermSymbol -> Maybe t
valueOf (Model m) sym =
  (unsafeFromModelValue @t)
    <$> M.lookup sym m

exceptFor :: Model -> S.HashSet TermSymbol -> Model
exceptFor (Model m) s =
  Model $ S.foldl' (flip M.delete) m s

restrictTo :: Model -> S.HashSet TermSymbol -> Model
restrictTo (Model m) s =
  Model $
    S.foldl'
      ( \acc sym -> case M.lookup sym m of
          Just v -> M.insert sym v acc
          Nothing -> acc
      )
      M.empty
      s

extendTo :: Model -> S.HashSet TermSymbol -> Model
extendTo (Model m) s =
  Model $
    S.foldl'
      ( \acc sym@(TermSymbol (_ :: TypeRep t) _) -> case M.lookup sym acc of
          Just _ -> acc
          Nothing -> M.insert sym (defaultValueDynamic (Proxy @t)) acc
      )
      m
      s

exact :: Model -> S.HashSet TermSymbol -> Model
exact m s = restrictTo (extendTo m s) s

insert :: (Eq a, Show a, Hashable a, Typeable a) => Model -> TermSymbol -> a -> Model
insert (Model m) sym@(TermSymbol p _) v =
  case eqTypeRep p (typeOf v) of
    Just HRefl -> Model $ M.insert sym (toModelValue v) m
    _ -> error "Bad value type"

evaluateSomeTerm :: Bool -> Model -> SomeTerm -> SomeTerm
evaluateSomeTerm fillDefault (Model ma) = gomemo
  where
    gomemo = htmemo go
    gotyped :: (SupportedPrim a) => Term a -> Term a
    gotyped a = case gomemo (SomeTerm a) of
      SomeTerm v -> unsafeCoerce v
    go c@(SomeTerm ConcTerm {}) = c
    go c@(SomeTerm ((SymbTerm _ sym@(TermSymbol (_ :: TypeRep t) _)) :: Term a)) = case M.lookup sym ma of
      Nothing -> if fillDefault then SomeTerm $ concTerm (defaultValue @t) else c
      Just dy -> SomeTerm $ concTerm (unsafeFromModelValue @a dy)
    go (SomeTerm (UnaryTerm _ tag (arg :: Term a))) = goUnary (partialEvalUnary tag) arg
    go (SomeTerm (BinaryTerm _ tag (arg1 :: Term a1) (arg2 :: Term a2))) =
      goBinary (partialEvalBinary tag) arg1 arg2
    go (SomeTerm (TernaryTerm _ tag (arg1 :: Term a1) (arg2 :: Term a2) (arg3 :: Term a3))) = do
      goTernary (partialEvalTernary tag) arg1 arg2 arg3
    go (SomeTerm (NotTerm _ arg)) = goUnary pevalNotTerm arg
    go (SomeTerm (OrTerm _ arg1 arg2)) =
      goBinary pevalOrTerm arg1 arg2
    go (SomeTerm (AndTerm _ arg1 arg2)) =
      goBinary pevalAndTerm arg1 arg2
    go (SomeTerm (EqvTerm _ arg1 arg2)) =
      goBinary pevalEqvTerm arg1 arg2
    go (SomeTerm (ITETerm _ cond arg1 arg2)) =
      goTernary pevalITETerm cond arg1 arg2
    go (SomeTerm (AddNumTerm _ arg1 arg2)) =
      goBinary pevalAddNumTerm arg1 arg2
    go (SomeTerm (UMinusNumTerm _ arg)) = goUnary pevalUMinusNumTerm arg
    go (SomeTerm (TimesNumTerm _ arg1 arg2)) =
      goBinary pevalTimesNumTerm arg1 arg2
    go (SomeTerm (AbsNumTerm _ arg)) = goUnary pevalAbsNumTerm arg
    go (SomeTerm (SignumNumTerm _ arg)) = goUnary pevalSignumNumTerm arg
    go (SomeTerm (LTNumTerm _ arg1 arg2)) =
      goBinary pevalLtNumTerm arg1 arg2
    go (SomeTerm (LENumTerm _ arg1 arg2)) =
      goBinary pevalLeNumTerm arg1 arg2
    go (SomeTerm (AndBitsTerm _ arg1 arg2)) =
      goBinary pevalAndBitsTerm arg1 arg2
    go (SomeTerm (OrBitsTerm _ arg1 arg2)) =
      goBinary pevalOrBitsTerm arg1 arg2
    go (SomeTerm (XorBitsTerm _ arg1 arg2)) =
      goBinary pevalXorBitsTerm arg1 arg2
    go (SomeTerm (ComplementBitsTerm _ arg)) = goUnary pevalComplementBitsTerm arg
    go (SomeTerm (ShiftBitsTerm _ arg n)) =
      goUnary (`pevalShiftBitsTerm` n) arg
    go (SomeTerm (RotateBitsTerm _ arg n)) =
      goUnary (`pevalRotateBitsTerm` n) arg
    go (SomeTerm (BVConcatTerm _ arg1 arg2)) =
      goBinary pevalBVConcatTerm arg1 arg2
    go (SomeTerm (BVSelectTerm _ ix w arg)) =
      goUnary (pevalBVSelectTerm ix w) arg
    go (SomeTerm (BVExtendTerm _ n signed arg)) =
      goUnary (pevalBVExtendTerm n signed) arg
    go (SomeTerm (TabularFuncApplyTerm _ f arg)) =
      goBinary pevalTabularFuncApplyTerm f arg
    go (SomeTerm (GeneralFuncApplyTerm _ f arg)) =
      goBinary pevalGeneralFuncApplyTerm f arg
    go (SomeTerm (DivIntegerTerm _ arg1 arg2)) =
      goBinary pevalDivIntegerTerm arg1 arg2
    go (SomeTerm (ModIntegerTerm _ arg1 arg2)) =
      goBinary pevalModIntegerTerm arg1 arg2
    goUnary :: (SupportedPrim a, SupportedPrim b) => (Term a -> Term b) -> Term a -> SomeTerm
    goUnary f a = SomeTerm $ f (gotyped a)
    goBinary ::
      (SupportedPrim a, SupportedPrim b, SupportedPrim c) =>
      (Term a -> Term b -> Term c) ->
      Term a ->
      Term b ->
      SomeTerm
    goBinary f a b = SomeTerm $ f (gotyped a) (gotyped b)
    goTernary ::
      (SupportedPrim a, SupportedPrim b, SupportedPrim c, SupportedPrim d) =>
      (Term a -> Term b -> Term c -> Term d) ->
      Term a ->
      Term b ->
      Term c ->
      SomeTerm
    goTernary f a b c = SomeTerm $ f (gotyped a) (gotyped b) (gotyped c)

evaluateTerm :: forall a. (SupportedPrim a) => Bool -> Model -> Term a -> Term a
evaluateTerm fillDefault m t = case evaluateSomeTerm fillDefault m $ SomeTerm t of
  SomeTerm (t1 :: Term b) -> unsafeCoerce @(Term b) @(Term a) t1
