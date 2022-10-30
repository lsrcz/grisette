{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Grisette.IR.SymPrim.Data.Prim.Model
  ( SymbolSet (..),
    Model (..),
    equation,
    evaluateTerm,
  )
where

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.Hashable
import Data.Proxy
import GHC.Generics
import Grisette.Core.Data.Class.ExtractSymbolics
import Grisette.Core.Data.Class.ModelOps
import Grisette.Core.Data.MemoUtils
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.SomeTerm
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Grisette.IR.SymPrim.Data.Prim.ModelValue
import Grisette.IR.SymPrim.Data.Prim.PartialEval.BV
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bits
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bool
import Grisette.IR.SymPrim.Data.Prim.PartialEval.GeneralFunc
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Integer
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Num
import Grisette.IR.SymPrim.Data.Prim.PartialEval.TabularFunc
import Type.Reflection
import Unsafe.Coerce

newtype SymbolSet = SymbolSet {unSymbolSet :: S.HashSet SomeTypedSymbol}
  deriving (Eq, Show, Generic, Hashable, Semigroup, Monoid)

newtype Model = Model {unModel :: M.HashMap SomeTypedSymbol ModelValue} deriving (Show, Eq, Generic, Hashable)

equation :: TypedSymbol a -> Model -> Maybe (Term Bool)
equation tsym@(TypedSymbol sym) m =
  case valueOf tsym m of
    Just v -> Just $ pevalEqvTerm (symbTerm sym) (concTerm v)
    Nothing -> Nothing

instance SymbolSetOps SymbolSet TypedSymbol where
  emptySet = SymbolSet S.empty
  containsSymbol s@(TypedSymbol _ :: TypedSymbol a) =
    S.member (someTypedSymbol s) . unSymbolSet
  insertSymbol s@(TypedSymbol _ :: TypedSymbol a) = SymbolSet . S.insert (someTypedSymbol s) . unSymbolSet
  intersectionSet (SymbolSet s1) (SymbolSet s2) = SymbolSet $ S.intersection s1 s2
  unionSet (SymbolSet s1) (SymbolSet s2) = SymbolSet $ S.union s1 s2
  differenceSet (SymbolSet s1) (SymbolSet s2) = SymbolSet $ S.difference s1 s2

instance ExtractSymbolics SymbolSet SymbolSet where
  extractSymbolics = id

instance ModelOps Model SymbolSet TypedSymbol where
  emptyModel = Model M.empty
  valueOf :: forall t. TypedSymbol t -> Model -> Maybe t
  valueOf sym@(TypedSymbol _) (Model m) =
    (unsafeFromModelValue @t)
      <$> M.lookup (someTypedSymbol sym) m
  exceptFor (SymbolSet s) (Model m) = Model $ S.foldl' (flip M.delete) m s
  restrictTo (SymbolSet s) (Model m) =
    Model $
      S.foldl'
        ( \acc sym -> case M.lookup sym m of
            Just v -> M.insert sym v acc
            Nothing -> acc
        )
        M.empty
        s
  extendTo (SymbolSet s) (Model m) =
    Model $
      S.foldl'
        ( \acc sym@(SomeTypedSymbol _ (TypedSymbol _ :: TypedSymbol t)) -> case M.lookup sym acc of
            Just _ -> acc
            Nothing -> M.insert sym (defaultValueDynamic (Proxy @t)) acc
        )
        m
        s
  insertValue sym@(TypedSymbol _) (v :: t) (Model m) =
    Model $ M.insert (someTypedSymbol sym) (toModelValue v) m

evaluateSomeTerm :: Bool -> Model -> SomeTerm -> SomeTerm
evaluateSomeTerm fillDefault (Model ma) = gomemo
  where
    gomemo = htmemo go
    gotyped :: (SupportedPrim a) => Term a -> Term a
    gotyped a = case gomemo (SomeTerm a) of
      SomeTerm v -> unsafeCoerce v
    go c@(SomeTerm ConcTerm {}) = c
    go c@(SomeTerm ((SymbTerm _ sym@(TypedSymbol _ :: TypedSymbol t)) :: Term a)) =
      case M.lookup (SomeTypedSymbol (typeRep @t) sym) ma of
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
