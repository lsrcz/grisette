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
    ModelValuePair (..),
    equation,
    evaluateTerm,
  )
where

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.Hashable
import Data.List
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
  deriving (Eq, Generic, Hashable, Semigroup, Monoid)

instance Show SymbolSet where
  showsPrec prec (SymbolSet s) = showParen (prec >= 10) $ \x ->
    "SymbolSet {"
      ++ go0 (sort $ show <$> S.toList s)
      ++ "}"
      ++ x
    where
      go0 [] = ""
      go0 [x] = x
      go0 (x : xs) = x ++ ", " ++ go0 xs

newtype Model = Model {unModel :: M.HashMap SomeTypedSymbol ModelValue} deriving (Eq, Generic, Hashable)

instance Show Model where
  showsPrec prec (Model m) = showParen (prec >= 10) $ \x ->
    "Model {"
      ++ go0 (sortOn (\(x, _) -> show x) $ M.toList m)
      ++ "}"
      ++ x
    where
      go0 [] = ""
      go0 [(SomeTypedSymbol _ s, v)] = showUntyped s ++ " -> " ++ show v
      go0 ((SomeTypedSymbol _ s, v) : xs) = showUntyped s ++ " -> " ++ show v ++ ", " ++ go0 xs

equation :: TypedSymbol a -> Model -> Maybe (Term Bool)
equation tsym m = withSymbolSupported tsym $
  case valueOf tsym m of
    Just v -> Just $ pevalEqvTerm (symbTerm tsym) (concTerm v)
    Nothing -> Nothing

instance SymbolSetOps SymbolSet TypedSymbol where
  emptySet = SymbolSet S.empty
  containsSymbol s =
    S.member (someTypedSymbol s) . unSymbolSet
  insertSymbol s = SymbolSet . S.insert (someTypedSymbol s) . unSymbolSet
  intersectionSet (SymbolSet s1) (SymbolSet s2) = SymbolSet $ S.intersection s1 s2
  unionSet (SymbolSet s1) (SymbolSet s2) = SymbolSet $ S.union s1 s2
  differenceSet (SymbolSet s1) (SymbolSet s2) = SymbolSet $ S.difference s1 s2

instance SymbolSetRep (TypedSymbol t) SymbolSet TypedSymbol where
  buildSymbolSet sym = insertSymbol sym emptySet

instance
  SymbolSetRep
    ( TypedSymbol a,
      TypedSymbol b
    )
    SymbolSet
    TypedSymbol
  where
  buildSymbolSet (sym1, sym2) =
    insertSymbol sym2
      . insertSymbol sym1
      $ emptySet

instance
  SymbolSetRep
    ( TypedSymbol a,
      TypedSymbol b,
      TypedSymbol c
    )
    SymbolSet
    TypedSymbol
  where
  buildSymbolSet (sym1, sym2, sym3) =
    insertSymbol sym3
      . insertSymbol sym2
      . insertSymbol sym1
      $ emptySet

instance
  SymbolSetRep
    ( TypedSymbol a,
      TypedSymbol b,
      TypedSymbol c,
      TypedSymbol d
    )
    SymbolSet
    TypedSymbol
  where
  buildSymbolSet (sym1, sym2, sym3, sym4) =
    insertSymbol sym4
      . insertSymbol sym3
      . insertSymbol sym2
      . insertSymbol sym1
      $ emptySet

instance
  SymbolSetRep
    ( TypedSymbol a,
      TypedSymbol b,
      TypedSymbol c,
      TypedSymbol d,
      TypedSymbol e
    )
    SymbolSet
    TypedSymbol
  where
  buildSymbolSet (sym1, sym2, sym3, sym4, sym5) =
    insertSymbol sym5
      . insertSymbol sym4
      . insertSymbol sym3
      . insertSymbol sym2
      . insertSymbol sym1
      $ emptySet

instance
  SymbolSetRep
    ( TypedSymbol a,
      TypedSymbol b,
      TypedSymbol c,
      TypedSymbol d,
      TypedSymbol e,
      TypedSymbol f
    )
    SymbolSet
    TypedSymbol
  where
  buildSymbolSet (sym1, sym2, sym3, sym4, sym5, sym6) =
    insertSymbol sym6
      . insertSymbol sym5
      . insertSymbol sym4
      . insertSymbol sym3
      . insertSymbol sym2
      . insertSymbol sym1
      $ emptySet

instance
  SymbolSetRep
    ( TypedSymbol a,
      TypedSymbol b,
      TypedSymbol c,
      TypedSymbol d,
      TypedSymbol e,
      TypedSymbol f,
      TypedSymbol g
    )
    SymbolSet
    TypedSymbol
  where
  buildSymbolSet (sym1, sym2, sym3, sym4, sym5, sym6, sym7) =
    insertSymbol sym7
      . insertSymbol sym6
      . insertSymbol sym5
      . insertSymbol sym4
      . insertSymbol sym3
      . insertSymbol sym2
      . insertSymbol sym1
      $ emptySet

instance
  SymbolSetRep
    ( TypedSymbol a,
      TypedSymbol b,
      TypedSymbol c,
      TypedSymbol d,
      TypedSymbol e,
      TypedSymbol f,
      TypedSymbol g,
      TypedSymbol h
    )
    SymbolSet
    TypedSymbol
  where
  buildSymbolSet (sym1, sym2, sym3, sym4, sym5, sym6, sym7, sym8) =
    insertSymbol sym8
      . insertSymbol sym7
      . insertSymbol sym6
      . insertSymbol sym5
      . insertSymbol sym4
      . insertSymbol sym3
      . insertSymbol sym2
      . insertSymbol sym1
      $ emptySet

instance GExtractSymbolics SymbolSet SymbolSet where
  gextractSymbolics = id

instance ModelOps Model SymbolSet TypedSymbol where
  emptyModel = Model M.empty
  valueOf :: forall t. TypedSymbol t -> Model -> Maybe t
  valueOf sym (Model m) =
    withSymbolSupported sym $
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
        ( \acc sym@(SomeTypedSymbol _ (tsym :: TypedSymbol t)) -> case M.lookup sym acc of
            Just _ -> acc
            Nothing -> withSymbolSupported tsym $ M.insert sym (defaultValueDynamic (Proxy @t)) acc
        )
        m
        s
  insertValue sym (v :: t) (Model m) =
    withSymbolSupported sym $
      Model $
        M.insert (someTypedSymbol sym) (toModelValue v) m

evaluateSomeTerm :: Bool -> Model -> SomeTerm -> SomeTerm
evaluateSomeTerm fillDefault (Model ma) = gomemo
  where
    gomemo = htmemo go
    gotyped :: (SupportedPrim a) => Term a -> Term a
    gotyped a = case gomemo (SomeTerm a) of
      SomeTerm v -> unsafeCoerce v
    go c@(SomeTerm ConcTerm {}) = c
    go c@(SomeTerm ((SymbTerm _ sym) :: Term a)) =
      case M.lookup (someTypedSymbol sym) ma of
        Nothing -> if fillDefault then SomeTerm $ concTerm (defaultValue @a) else c
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

data ModelValuePair t = (TypedSymbol t) ::= t deriving (Show)

instance ModelRep (ModelValuePair t) Model SymbolSet TypedSymbol where
  buildModel (sym ::= val) = insertValue sym val emptyModel

instance
  ModelRep
    ( ModelValuePair a,
      ModelValuePair b
    )
    Model
    SymbolSet
    TypedSymbol
  where
  buildModel
    ( sym1 ::= val1,
      sym2 ::= val2
      ) =
      insertValue sym2 val2
        . insertValue sym1 val1
        $ emptyModel

instance
  ModelRep
    ( ModelValuePair a,
      ModelValuePair b,
      ModelValuePair c
    )
    Model
    SymbolSet
    TypedSymbol
  where
  buildModel
    ( sym1 ::= val1,
      sym2 ::= val2,
      sym3 ::= val3
      ) =
      insertValue sym3 val3
        . insertValue sym2 val2
        . insertValue sym1 val1
        $ emptyModel

instance
  ModelRep
    ( ModelValuePair a,
      ModelValuePair b,
      ModelValuePair c,
      ModelValuePair d
    )
    Model
    SymbolSet
    TypedSymbol
  where
  buildModel
    ( sym1 ::= val1,
      sym2 ::= val2,
      sym3 ::= val3,
      sym4 ::= val4
      ) =
      insertValue sym4 val4
        . insertValue sym3 val3
        . insertValue sym2 val2
        . insertValue sym1 val1
        $ emptyModel

instance
  ModelRep
    ( ModelValuePair a,
      ModelValuePair b,
      ModelValuePair c,
      ModelValuePair d,
      ModelValuePair e
    )
    Model
    SymbolSet
    TypedSymbol
  where
  buildModel
    ( sym1 ::= val1,
      sym2 ::= val2,
      sym3 ::= val3,
      sym4 ::= val4,
      sym5 ::= val5
      ) =
      insertValue sym5 val5
        . insertValue sym4 val4
        . insertValue sym3 val3
        . insertValue sym2 val2
        . insertValue sym1 val1
        $ emptyModel

instance
  ModelRep
    ( ModelValuePair a,
      ModelValuePair b,
      ModelValuePair c,
      ModelValuePair d,
      ModelValuePair e,
      ModelValuePair f
    )
    Model
    SymbolSet
    TypedSymbol
  where
  buildModel
    ( sym1 ::= val1,
      sym2 ::= val2,
      sym3 ::= val3,
      sym4 ::= val4,
      sym5 ::= val5,
      sym6 ::= val6
      ) =
      insertValue sym6 val6
        . insertValue sym5 val5
        . insertValue sym4 val4
        . insertValue sym3 val3
        . insertValue sym2 val2
        . insertValue sym1 val1
        $ emptyModel

instance
  ModelRep
    ( ModelValuePair a,
      ModelValuePair b,
      ModelValuePair c,
      ModelValuePair d,
      ModelValuePair e,
      ModelValuePair f,
      ModelValuePair g
    )
    Model
    SymbolSet
    TypedSymbol
  where
  buildModel
    ( sym1 ::= val1,
      sym2 ::= val2,
      sym3 ::= val3,
      sym4 ::= val4,
      sym5 ::= val5,
      sym6 ::= val6,
      sym7 ::= val7
      ) =
      insertValue sym7 val7
        . insertValue sym6 val6
        . insertValue sym5 val5
        . insertValue sym4 val4
        . insertValue sym3 val3
        . insertValue sym2 val2
        . insertValue sym1 val1
        $ emptyModel

instance
  ModelRep
    ( ModelValuePair a,
      ModelValuePair b,
      ModelValuePair c,
      ModelValuePair d,
      ModelValuePair e,
      ModelValuePair f,
      ModelValuePair g,
      ModelValuePair h
    )
    Model
    SymbolSet
    TypedSymbol
  where
  buildModel
    ( sym1 ::= val1,
      sym2 ::= val2,
      sym3 ::= val3,
      sym4 ::= val4,
      sym5 ::= val5,
      sym6 ::= val6,
      sym7 ::= val7,
      sym8 ::= val8
      ) =
      insertValue sym8 val8
        . insertValue sym7 val7
        . insertValue sym6 val6
        . insertValue sym5 val5
        . insertValue sym4 val4
        . insertValue sym3 val3
        . insertValue sym2 val2
        . insertValue sym1 val1
        $ emptyModel
