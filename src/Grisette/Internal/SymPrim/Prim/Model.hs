{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Prim.Model
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.Prim.Model
  ( SymbolSet (..),
    Model (..),
    ModelValuePair (..),
    equation,
    evaluateTerm,
  )
where

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.Hashable (Hashable)
import Data.List (sort, sortOn)
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import Grisette.Internal.Core.Data.Class.ModelOps
  ( ModelOps
      ( emptyModel,
        exceptFor,
        exceptFor',
        extendTo,
        insertValue,
        isEmptyModel,
        modelContains,
        restrictTo,
        valueOf
      ),
    ModelRep (buildModel),
    SymbolSetOps
      ( containsSymbol,
        differenceSet,
        emptySet,
        insertSymbol,
        intersectionSet,
        isEmptySet,
        unionSet
      ),
    SymbolSetRep (buildSymbolSet),
  )
import Grisette.Internal.Core.Data.MemoUtils (htmemo)
import Grisette.Internal.SymPrim.GeneralFun (type (-->) (GeneralFun))
import Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalFP
  ( pevalFPBinaryTerm,
    pevalFPFMATerm,
    pevalFPRoundingBinaryTerm,
    pevalFPRoundingUnaryTerm,
    pevalFPTraitTerm,
    pevalFPUnaryTerm,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Term (Term (FPFMATerm))
import Grisette.Internal.SymPrim.Prim.ModelValue
  ( ModelValue,
    toModelValue,
    unsafeFromModelValue,
  )
import Grisette.Internal.SymPrim.Prim.SomeTerm
  ( SomeTerm (SomeTerm),
  )
import Grisette.Internal.SymPrim.Prim.Term
  ( BinaryOp (pevalBinary),
    PEvalApplyTerm (pevalApplyTerm),
    PEvalBVSignConversionTerm (pevalBVToSignedTerm, pevalBVToUnsignedTerm),
    PEvalBVTerm (pevalBVConcatTerm, pevalBVExtendTerm, pevalBVSelectTerm),
    PEvalBitwiseTerm
      ( pevalAndBitsTerm,
        pevalComplementBitsTerm,
        pevalOrBitsTerm,
        pevalXorBitsTerm
      ),
    PEvalDivModIntegralTerm
      ( pevalDivIntegralTerm,
        pevalModIntegralTerm,
        pevalQuotIntegralTerm,
        pevalRemIntegralTerm
      ),
    PEvalFloatingTerm (pevalSqrtTerm),
    PEvalFractionalTerm (pevalFdivTerm, pevalRecipTerm),
    PEvalNumTerm
      ( pevalAbsNumTerm,
        pevalAddNumTerm,
        pevalMulNumTerm,
        pevalNegNumTerm,
        pevalSignumNumTerm
      ),
    PEvalOrdTerm (pevalLeOrdTerm, pevalLtOrdTerm),
    PEvalRotateTerm
      ( pevalRotateLeftTerm,
        pevalRotateRightTerm
      ),
    PEvalShiftTerm (pevalShiftLeftTerm, pevalShiftRightTerm),
    SomeTypedSymbol (SomeTypedSymbol),
    SupportedPrim (defaultValue, defaultValueDynamic, pevalITETerm),
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
        FPBinaryTerm,
        FPRoundingBinaryTerm,
        FPRoundingUnaryTerm,
        FPTraitTerm,
        FPUnaryTerm,
        FdivTerm,
        ITETerm,
        LeOrdTerm,
        LtOrdTerm,
        ModIntegralTerm,
        MulNumTerm,
        NegNumTerm,
        NotTerm,
        OrBitsTerm,
        OrTerm,
        QuotIntegralTerm,
        RecipTerm,
        RemIntegralTerm,
        RotateLeftTerm,
        RotateRightTerm,
        ShiftLeftTerm,
        ShiftRightTerm,
        SignumNumTerm,
        SqrtTerm,
        SymTerm,
        TernaryTerm,
        ToSignedTerm,
        ToUnsignedTerm,
        UnaryTerm,
        XorBitsTerm
      ),
    TernaryOp (pevalTernary),
    TypedSymbol (TypedSymbol, unTypedSymbol),
    UnaryOp (pevalUnary),
    conTerm,
    pevalAndTerm,
    pevalEqTerm,
    pevalNotTerm,
    pevalOrTerm,
    showUntyped,
    someTypedSymbol,
    symTerm,
    withSymbolSupported,
  )
import Grisette.Internal.Utils.Parameterized (unsafeAxiom)
import Type.Reflection
  ( TypeRep,
    eqTypeRep,
    typeRep,
    pattern App,
    type (:~:) (Refl),
    type (:~~:) (HRefl),
  )
import Unsafe.Coerce (unsafeCoerce)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
-- >>> :set -XFlexibleContexts

-- | Set of symbols.
--
-- Check 'Grisette.Core.SymbolSetOps' for operations, and
-- 'Grisette.Core.SymbolSetRep' for manual constructions.
newtype SymbolSet = SymbolSet {unSymbolSet :: S.HashSet SomeTypedSymbol}
  deriving (Eq, Generic, Hashable)

instance Semigroup SymbolSet where
  SymbolSet s1 <> SymbolSet s2 = SymbolSet $ S.union s1 s2

instance Monoid SymbolSet where
  mempty = emptySet

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

-- | Model returned by the solver.
--
-- Check 'Grisette.Core.ModelOps' for operations, and 'Grisette.Core.ModelRep'
-- for manual constructions.
newtype Model = Model {unModel :: M.HashMap SomeTypedSymbol ModelValue}
  deriving (Eq, Generic, Hashable)

instance Semigroup Model where
  Model m1 <> Model m2 = Model $ M.union m1 m2

instance Monoid Model where
  mempty = emptyModel

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

-- | Given a typed symbol and a model, return the equation (symbol = value)
-- encoded in the model.
equation :: TypedSymbol a -> Model -> Maybe (Term Bool)
equation tsym@(TypedSymbol {}) m = withSymbolSupported tsym $
  case valueOf tsym m of
    Just v -> Just $ pevalEqTerm (symTerm $ unTypedSymbol tsym) (conTerm v)
    Nothing -> Nothing

instance SymbolSetOps SymbolSet TypedSymbol where
  emptySet = SymbolSet S.empty
  isEmptySet (SymbolSet s) = S.null s
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

instance ModelOps Model SymbolSet TypedSymbol where
  emptyModel = Model M.empty
  isEmptyModel (Model m) = M.null m
  valueOf :: forall t. TypedSymbol t -> Model -> Maybe t
  valueOf sym (Model m) =
    withSymbolSupported sym $
      (unsafeFromModelValue @t)
        <$> M.lookup (someTypedSymbol sym) m
  modelContains sym (Model m) = M.member (someTypedSymbol sym) m
  exceptFor (SymbolSet s) (Model m) = Model $ S.foldl' (flip M.delete) m s
  exceptFor' s (Model m) = Model $ M.delete (someTypedSymbol s) m
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
evaluateSomeTerm fillDefault m@(Model ma) = gomemo
  where
    gomemo = htmemo go
    gotyped :: (SupportedPrim a) => Term a -> Term a
    gotyped a = case gomemo (SomeTerm a) of
      SomeTerm v -> unsafeCoerce v
    go c@(SomeTerm (ConTerm _ cv :: Term v)) =
      case (typeRep :: TypeRep v) of
        App (App gf _) _ ->
          case eqTypeRep gf (typeRep @(-->)) of
            Just HRefl -> case cv of
              GeneralFun sym (tm :: Term r) ->
                if modelContains sym m -- someTypedSymbol sym1 == someTypedSymbol sym
                  then case evaluateSomeTerm fillDefault (exceptFor' sym m) (SomeTerm tm) of
                    SomeTerm (tm' :: Term r1) ->
                      case unsafeAxiom @r @r1 of
                        Refl -> SomeTerm $ conTerm $ GeneralFun sym tm' -- stm
                  else SomeTerm $ conTerm $ GeneralFun sym (gotyped tm)
            Nothing -> c
        _ -> c
    go c@(SomeTerm ((SymTerm _ sym) :: Term a)) =
      case M.lookup (someTypedSymbol sym) ma of
        Nothing -> if fillDefault then SomeTerm $ conTerm (defaultValue @a) else c
        Just dy -> SomeTerm $ conTerm (unsafeFromModelValue @a dy)
    go (SomeTerm (UnaryTerm _ tag (arg :: Term a))) = goUnary (pevalUnary tag) arg
    go (SomeTerm (BinaryTerm _ tag (arg1 :: Term a1) (arg2 :: Term a2))) =
      goBinary (pevalBinary tag) arg1 arg2
    go (SomeTerm (TernaryTerm _ tag (arg1 :: Term a1) (arg2 :: Term a2) (arg3 :: Term a3))) = do
      goTernary (pevalTernary tag) arg1 arg2 arg3
    go (SomeTerm (NotTerm _ arg)) = goUnary pevalNotTerm arg
    go (SomeTerm (OrTerm _ arg1 arg2)) =
      goBinary pevalOrTerm arg1 arg2
    go (SomeTerm (AndTerm _ arg1 arg2)) =
      goBinary pevalAndTerm arg1 arg2
    go (SomeTerm (EqTerm _ arg1 arg2)) =
      goBinary pevalEqTerm arg1 arg2
    go (SomeTerm (ITETerm _ cond arg1 arg2)) =
      goTernary pevalITETerm cond arg1 arg2
    go (SomeTerm (AddNumTerm _ arg1 arg2)) =
      goBinary pevalAddNumTerm arg1 arg2
    go (SomeTerm (NegNumTerm _ arg)) = goUnary pevalNegNumTerm arg
    go (SomeTerm (MulNumTerm _ arg1 arg2)) =
      goBinary pevalMulNumTerm arg1 arg2
    go (SomeTerm (AbsNumTerm _ arg)) = goUnary pevalAbsNumTerm arg
    go (SomeTerm (SignumNumTerm _ arg)) = goUnary pevalSignumNumTerm arg
    go (SomeTerm (LtOrdTerm _ arg1 arg2)) =
      goBinary pevalLtOrdTerm arg1 arg2
    go (SomeTerm (LeOrdTerm _ arg1 arg2)) =
      goBinary pevalLeOrdTerm arg1 arg2
    go (SomeTerm (AndBitsTerm _ arg1 arg2)) =
      goBinary pevalAndBitsTerm arg1 arg2
    go (SomeTerm (OrBitsTerm _ arg1 arg2)) =
      goBinary pevalOrBitsTerm arg1 arg2
    go (SomeTerm (XorBitsTerm _ arg1 arg2)) =
      goBinary pevalXorBitsTerm arg1 arg2
    go (SomeTerm (ComplementBitsTerm _ arg)) = goUnary pevalComplementBitsTerm arg
    go (SomeTerm (ShiftLeftTerm _ arg n)) = goBinary pevalShiftLeftTerm arg n
    go (SomeTerm (RotateLeftTerm _ arg n)) = goBinary pevalRotateLeftTerm arg n
    go (SomeTerm (ShiftRightTerm _ arg n)) = goBinary pevalShiftRightTerm arg n
    go (SomeTerm (RotateRightTerm _ arg n)) = goBinary pevalRotateRightTerm arg n
    go (SomeTerm (ToSignedTerm _ arg)) =
      goUnary pevalBVToSignedTerm arg
    go (SomeTerm (ToUnsignedTerm _ arg)) =
      goUnary pevalBVToUnsignedTerm arg
    go (SomeTerm (BVConcatTerm _ arg1 arg2)) =
      goBinary pevalBVConcatTerm arg1 arg2
    go (SomeTerm (BVSelectTerm _ ix w arg)) =
      goUnary (pevalBVSelectTerm ix w) arg
    go (SomeTerm (BVExtendTerm _ n signed arg)) =
      goUnary (pevalBVExtendTerm n signed) arg
    go (SomeTerm (ApplyTerm _ f arg)) =
      goBinary pevalApplyTerm f arg
    go (SomeTerm (DivIntegralTerm _ arg1 arg2)) =
      goBinary pevalDivIntegralTerm arg1 arg2
    go (SomeTerm (ModIntegralTerm _ arg1 arg2)) =
      goBinary pevalModIntegralTerm arg1 arg2
    go (SomeTerm (QuotIntegralTerm _ arg1 arg2)) =
      goBinary pevalQuotIntegralTerm arg1 arg2
    go (SomeTerm (RemIntegralTerm _ arg1 arg2)) =
      goBinary pevalRemIntegralTerm arg1 arg2
    go (SomeTerm (FPTraitTerm _ trait arg)) =
      goUnary (pevalFPTraitTerm trait) arg
    go (SomeTerm (FdivTerm _ arg1 arg2)) = goBinary pevalFdivTerm arg1 arg2
    go (SomeTerm (RecipTerm _ arg)) = goUnary pevalRecipTerm arg
    go (SomeTerm (SqrtTerm _ arg)) = goUnary pevalSqrtTerm arg
    go (SomeTerm (FPUnaryTerm _ op arg)) = goUnary (pevalFPUnaryTerm op) arg
    go (SomeTerm (FPBinaryTerm _ op arg1 arg2)) =
      goBinary (pevalFPBinaryTerm op) arg1 arg2
    go (SomeTerm (FPRoundingUnaryTerm _ op mode arg)) =
      goUnary (pevalFPRoundingUnaryTerm op mode) arg
    go (SomeTerm (FPRoundingBinaryTerm _ op mode arg1 arg2)) =
      goBinary (pevalFPRoundingBinaryTerm op mode) arg1 arg2
    go (SomeTerm (FPFMATerm _ mode arg1 arg2 arg3)) =
      SomeTerm $ pevalFPFMATerm (gotyped mode) (gotyped arg1) (gotyped arg2) (gotyped arg3)
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

-- | Evaluate a term in the given model.
evaluateTerm :: forall a. (SupportedPrim a) => Bool -> Model -> Term a -> Term a
evaluateTerm fillDefault m t = case evaluateSomeTerm fillDefault m $ SomeTerm t of
  SomeTerm (t1 :: Term b) -> unsafeCoerce @(Term b) @(Term a) t1

-- |
-- A type used for building a model by hand.
--
-- >>> buildModel ("x" ::= (1 :: Integer), "y" ::= True) :: Model
-- Model {x -> 1 :: Integer, y -> True :: Bool}
data ModelValuePair t = (TypedSymbol t) ::= t deriving (Show)

instance ModelRep (ModelValuePair t) Model where
  buildModel (sym ::= val) = insertValue sym val emptyModel

instance (ModelRep a Model, ModelRep b Model) => ModelRep (a, b) Model where
  buildModel (a, b) = buildModel a <> buildModel b

instance
  ( ModelRep a Model,
    ModelRep b Model,
    ModelRep c Model
  ) =>
  ModelRep (a, b, c) Model
  where
  buildModel (a, b, c) = buildModel a <> buildModel b <> buildModel c

instance
  ( ModelRep a Model,
    ModelRep b Model,
    ModelRep c Model,
    ModelRep d Model
  ) =>
  ModelRep (a, b, c, d) Model
  where
  buildModel (a, b, c, d) =
    buildModel a <> buildModel b <> buildModel c <> buildModel d

instance
  ( ModelRep a Model,
    ModelRep b Model,
    ModelRep c Model,
    ModelRep d Model,
    ModelRep e Model
  ) =>
  ModelRep (a, b, c, d, e) Model
  where
  buildModel (a, b, c, d, e) =
    buildModel a <> buildModel b <> buildModel c <> buildModel d <> buildModel e

instance
  ( ModelRep a Model,
    ModelRep b Model,
    ModelRep c Model,
    ModelRep d Model,
    ModelRep e Model,
    ModelRep f Model
  ) =>
  ModelRep (a, b, c, d, e, f) Model
  where
  buildModel (a, b, c, d, e, f) =
    buildModel a
      <> buildModel b
      <> buildModel c
      <> buildModel d
      <> buildModel e
      <> buildModel f

instance
  ( ModelRep a Model,
    ModelRep b Model,
    ModelRep c Model,
    ModelRep d Model,
    ModelRep e Model,
    ModelRep f Model,
    ModelRep g Model
  ) =>
  ModelRep (a, b, c, d, e, f, g) Model
  where
  buildModel (a, b, c, d, e, f, g) =
    buildModel a
      <> buildModel b
      <> buildModel c
      <> buildModel d
      <> buildModel e
      <> buildModel f
      <> buildModel g

instance
  ( ModelRep a Model,
    ModelRep b Model,
    ModelRep c Model,
    ModelRep d Model,
    ModelRep e Model,
    ModelRep f Model,
    ModelRep g Model,
    ModelRep h Model
  ) =>
  ModelRep (a, b, c, d, e, f, g, h) Model
  where
  buildModel (a, b, c, d, e, f, g, h) =
    buildModel a
      <> buildModel b
      <> buildModel c
      <> buildModel d
      <> buildModel e
      <> buildModel f
      <> buildModel g
      <> buildModel h
