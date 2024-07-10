{-# LANGUAGE DataKinds #-}
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
    ConstantSymbolSet,
    AnySymbolSet,
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
import Grisette.Internal.Core.Data.MemoUtils (htmemo2)
import Grisette.Internal.SymPrim.GeneralFun (type (-->) (GeneralFun))
import Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalFP
  ( pevalFPBinaryTerm,
    pevalFPFMATerm,
    pevalFPRoundingBinaryTerm,
    pevalFPRoundingUnaryTerm,
    pevalFPTraitTerm,
    pevalFPUnaryTerm,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( SomeTypedAnySymbol,
    SomeTypedConstantSymbol,
    SupportedPrim (castTypedSymbol),
    SymbolKind (AnyKind, ConstantKind),
    Term (ExistsTerm, FPFMATerm),
    TypedAnySymbol,
    existsTerm,
    forallTerm,
  )
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
    PEvalFloatingTerm (pevalFloatingUnaryTerm, pevalPowerTerm),
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
-- >>> :set -XFlexibleContexts

-- | Set of symbols.
--
-- Check 'Grisette.Core.SymbolSetOps' for operations, and
-- 'Grisette.Core.SymbolSetRep' for manual constructions.
newtype SymbolSet knd = SymbolSet
  { unSymbolSet :: S.HashSet (SomeTypedSymbol knd)
  }
  deriving (Eq, Generic, Hashable)

type ConstantSymbolSet = SymbolSet 'ConstantKind

type AnySymbolSet = SymbolSet 'AnyKind

instance Semigroup (SymbolSet knd) where
  SymbolSet s1 <> SymbolSet s2 = SymbolSet $ S.union s1 s2

instance Monoid (SymbolSet knd) where
  mempty = emptySet

instance Show (SymbolSet knd) where
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
newtype Model = Model
  { unModel :: M.HashMap SomeTypedAnySymbol ModelValue
  }
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
equation :: TypedAnySymbol a -> Model -> Maybe (Term Bool)
equation tsym@(TypedSymbol {}) m = withSymbolSupported tsym $
  case valueOf tsym m of
    Just v -> Just $ pevalEqTerm (symTerm $ unTypedSymbol tsym) (conTerm v)
    Nothing -> Nothing

instance SymbolSetOps (SymbolSet knd) (TypedSymbol knd) where
  emptySet = SymbolSet S.empty
  isEmptySet (SymbolSet s) = S.null s
  containsSymbol s =
    S.member (someTypedSymbol s) . unSymbolSet
  insertSymbol s = SymbolSet . S.insert (someTypedSymbol s) . unSymbolSet
  intersectionSet (SymbolSet s1) (SymbolSet s2) = SymbolSet $ S.intersection s1 s2
  unionSet (SymbolSet s1) (SymbolSet s2) = SymbolSet $ S.union s1 s2
  differenceSet (SymbolSet s1) (SymbolSet s2) = SymbolSet $ S.difference s1 s2

instance SymbolSetRep (TypedSymbol knd t) (SymbolSet knd) (TypedSymbol knd) where
  buildSymbolSet sym = insertSymbol sym emptySet

instance
  SymbolSetRep
    ( TypedSymbol knd a,
      TypedSymbol knd b
    )
    (SymbolSet knd)
    (TypedSymbol knd)
  where
  buildSymbolSet (sym1, sym2) =
    insertSymbol sym2
      . insertSymbol sym1
      $ emptySet

instance
  SymbolSetRep
    ( TypedSymbol knd a,
      TypedSymbol knd b,
      TypedSymbol knd c
    )
    (SymbolSet knd)
    (TypedSymbol knd)
  where
  buildSymbolSet (sym1, sym2, sym3) =
    insertSymbol sym3
      . insertSymbol sym2
      . insertSymbol sym1
      $ emptySet

instance
  SymbolSetRep
    ( TypedSymbol knd a,
      TypedSymbol knd b,
      TypedSymbol knd c,
      TypedSymbol knd d
    )
    (SymbolSet knd)
    (TypedSymbol knd)
  where
  buildSymbolSet (sym1, sym2, sym3, sym4) =
    insertSymbol sym4
      . insertSymbol sym3
      . insertSymbol sym2
      . insertSymbol sym1
      $ emptySet

instance
  SymbolSetRep
    ( TypedSymbol knd a,
      TypedSymbol knd b,
      TypedSymbol knd c,
      TypedSymbol knd d,
      TypedSymbol knd e
    )
    (SymbolSet knd)
    (TypedSymbol knd)
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
    ( TypedSymbol knd a,
      TypedSymbol knd b,
      TypedSymbol knd c,
      TypedSymbol knd d,
      TypedSymbol knd e,
      TypedSymbol knd f
    )
    (SymbolSet knd)
    (TypedSymbol knd)
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
    ( TypedSymbol knd a,
      TypedSymbol knd b,
      TypedSymbol knd c,
      TypedSymbol knd d,
      TypedSymbol knd e,
      TypedSymbol knd f,
      TypedSymbol knd g
    )
    (SymbolSet knd)
    (TypedSymbol knd)
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
    ( TypedSymbol knd a,
      TypedSymbol knd b,
      TypedSymbol knd c,
      TypedSymbol knd d,
      TypedSymbol knd e,
      TypedSymbol knd f,
      TypedSymbol knd g,
      TypedSymbol knd h
    )
    (SymbolSet knd)
    (TypedSymbol knd)
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

instance ModelOps Model AnySymbolSet TypedAnySymbol where
  emptyModel = Model M.empty
  isEmptyModel (Model m) = M.null m
  valueOf :: forall t. TypedAnySymbol t -> Model -> Maybe t
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
        ( \acc sym@(SomeTypedSymbol _ (tsym :: TypedAnySymbol t)) -> case M.lookup sym acc of
            Just _ -> acc
            Nothing -> withSymbolSupported tsym $ M.insert sym (defaultValueDynamic (Proxy @t)) acc
        )
        m
        s
  insertValue sym (v :: t) (Model m) =
    withSymbolSupported sym $
      Model $
        M.insert (someTypedSymbol sym) (toModelValue v) m

evaluateSomeTerm ::
  Bool -> S.HashSet SomeTypedConstantSymbol -> Model -> SomeTerm -> SomeTerm
evaluateSomeTerm fillDefault initialBoundedSymbols (Model ma) =
  go initialMemo initialBoundedSymbols
  where
    gotyped ::
      (SupportedPrim a) =>
      (S.HashSet SomeTypedConstantSymbol -> SomeTerm -> SomeTerm) ->
      S.HashSet SomeTypedConstantSymbol ->
      Term a ->
      Term a
    gotyped memo boundedSymbols a = case memo boundedSymbols (SomeTerm a) of
      SomeTerm v -> unsafeCoerce v
    initialMemo :: S.HashSet SomeTypedConstantSymbol -> SomeTerm -> SomeTerm
    initialMemo = htmemo2 (go initialMemo)
    {-# NOINLINE initialMemo #-}
    go _ bs c@(SomeTerm (ConTerm _ cv :: Term v)) =
      case (typeRep :: TypeRep v) of
        App (App gf _) _ ->
          case eqTypeRep gf (typeRep @(-->)) of
            Just HRefl -> case cv of
              GeneralFun sym (tm :: Term r) -> do
                let newmemo = htmemo2 (go newmemo)
                    {-# NOINLINE newmemo #-}
                SomeTerm $
                  conTerm $
                    GeneralFun
                      sym
                      ( gotyped
                          newmemo
                          (S.union (S.singleton (someTypedSymbol sym)) bs)
                          tm
                      )
            Nothing -> c
        _ -> c
    go _ bs c@(SomeTerm ((SymTerm _ sym) :: Term a)) =
      case castTypedSymbol sym of
        Just sym' | S.member (someTypedSymbol sym') bs -> c
        _ ->
          case (M.lookup (someTypedSymbol sym) ma) of
            Nothing -> if fillDefault then SomeTerm $ conTerm (defaultValue @a) else c
            Just dy -> SomeTerm $ conTerm (unsafeFromModelValue @a dy)
    go _ bs (SomeTerm (ForallTerm _ tsym b)) =
      let newmemo = htmemo2 (go newmemo)
          {-# NOINLINE newmemo #-}
       in goUnary
            newmemo
            (S.insert (someTypedSymbol tsym) bs)
            (forallTerm tsym)
            b
    go _ bs (SomeTerm (ExistsTerm _ tsym b)) =
      let newmemo = htmemo2 (go newmemo)
          {-# NOINLINE newmemo #-}
       in goUnary
            newmemo
            (S.insert (someTypedSymbol tsym) bs)
            (existsTerm tsym)
            b
    go memo bs (SomeTerm (UnaryTerm _ tag (arg :: Term a))) =
      goUnary memo bs (pevalUnary tag) arg
    go memo bs (SomeTerm (BinaryTerm _ tag (arg1 :: Term a1) (arg2 :: Term a2))) =
      goBinary memo bs (pevalBinary tag) arg1 arg2
    go memo bs (SomeTerm (TernaryTerm _ tag (arg1 :: Term a1) (arg2 :: Term a2) (arg3 :: Term a3))) = do
      goTernary memo bs (pevalTernary tag) arg1 arg2 arg3
    go memo bs (SomeTerm (NotTerm _ arg)) = goUnary memo bs pevalNotTerm arg
    go memo bs (SomeTerm (OrTerm _ arg1 arg2)) =
      goBinary memo bs pevalOrTerm arg1 arg2
    go memo bs (SomeTerm (AndTerm _ arg1 arg2)) =
      goBinary memo bs pevalAndTerm arg1 arg2
    go memo bs (SomeTerm (EqTerm _ arg1 arg2)) =
      goBinary memo bs pevalEqTerm arg1 arg2
    go memo bs (SomeTerm (ITETerm _ cond arg1 arg2)) =
      goTernary memo bs pevalITETerm cond arg1 arg2
    go memo bs (SomeTerm (AddNumTerm _ arg1 arg2)) =
      goBinary memo bs pevalAddNumTerm arg1 arg2
    go memo bs (SomeTerm (NegNumTerm _ arg)) = goUnary memo bs pevalNegNumTerm arg
    go memo bs (SomeTerm (MulNumTerm _ arg1 arg2)) =
      goBinary memo bs pevalMulNumTerm arg1 arg2
    go memo bs (SomeTerm (AbsNumTerm _ arg)) = goUnary memo bs pevalAbsNumTerm arg
    go memo bs (SomeTerm (SignumNumTerm _ arg)) = goUnary memo bs pevalSignumNumTerm arg
    go memo bs (SomeTerm (LtOrdTerm _ arg1 arg2)) =
      goBinary memo bs pevalLtOrdTerm arg1 arg2
    go memo bs (SomeTerm (LeOrdTerm _ arg1 arg2)) =
      goBinary memo bs pevalLeOrdTerm arg1 arg2
    go memo bs (SomeTerm (AndBitsTerm _ arg1 arg2)) =
      goBinary memo bs pevalAndBitsTerm arg1 arg2
    go memo bs (SomeTerm (OrBitsTerm _ arg1 arg2)) =
      goBinary memo bs pevalOrBitsTerm arg1 arg2
    go memo bs (SomeTerm (XorBitsTerm _ arg1 arg2)) =
      goBinary memo bs pevalXorBitsTerm arg1 arg2
    go memo bs (SomeTerm (ComplementBitsTerm _ arg)) =
      goUnary memo bs pevalComplementBitsTerm arg
    go memo bs (SomeTerm (ShiftLeftTerm _ arg n)) =
      goBinary memo bs pevalShiftLeftTerm arg n
    go memo bs (SomeTerm (RotateLeftTerm _ arg n)) =
      goBinary memo bs pevalRotateLeftTerm arg n
    go memo bs (SomeTerm (ShiftRightTerm _ arg n)) =
      goBinary memo bs pevalShiftRightTerm arg n
    go memo bs (SomeTerm (RotateRightTerm _ arg n)) =
      goBinary memo bs pevalRotateRightTerm arg n
    go memo bs (SomeTerm (ToSignedTerm _ arg)) =
      goUnary memo bs pevalBVToSignedTerm arg
    go memo bs (SomeTerm (ToUnsignedTerm _ arg)) =
      goUnary memo bs pevalBVToUnsignedTerm arg
    go memo bs (SomeTerm (BVConcatTerm _ arg1 arg2)) =
      goBinary memo bs pevalBVConcatTerm arg1 arg2
    go memo bs (SomeTerm (BVSelectTerm _ ix w arg)) =
      goUnary memo bs (pevalBVSelectTerm ix w) arg
    go memo bs (SomeTerm (BVExtendTerm _ n signed arg)) =
      goUnary memo bs (pevalBVExtendTerm n signed) arg
    go memo bs (SomeTerm (ApplyTerm _ f arg)) =
      goBinary memo bs pevalApplyTerm f arg
    go memo bs (SomeTerm (DivIntegralTerm _ arg1 arg2)) =
      goBinary memo bs pevalDivIntegralTerm arg1 arg2
    go memo bs (SomeTerm (ModIntegralTerm _ arg1 arg2)) =
      goBinary memo bs pevalModIntegralTerm arg1 arg2
    go memo bs (SomeTerm (QuotIntegralTerm _ arg1 arg2)) =
      goBinary memo bs pevalQuotIntegralTerm arg1 arg2
    go memo bs (SomeTerm (RemIntegralTerm _ arg1 arg2)) =
      goBinary memo bs pevalRemIntegralTerm arg1 arg2
    go memo bs (SomeTerm (FPTraitTerm _ trait arg)) =
      goUnary memo bs (pevalFPTraitTerm trait) arg
    go memo bs (SomeTerm (FdivTerm _ arg1 arg2)) = goBinary memo bs pevalFdivTerm arg1 arg2
    go memo bs (SomeTerm (RecipTerm _ arg)) = goUnary memo bs pevalRecipTerm arg
    go memo bs (SomeTerm (FloatingUnaryTerm _ op arg)) =
      goUnary memo bs (pevalFloatingUnaryTerm op) arg
    go memo bs (SomeTerm (PowerTerm _ arg1 arg2)) =
      goBinary memo bs pevalPowerTerm arg1 arg2
    go memo bs (SomeTerm (FPUnaryTerm _ op arg)) = goUnary memo bs (pevalFPUnaryTerm op) arg
    go memo bs (SomeTerm (FPBinaryTerm _ op arg1 arg2)) =
      goBinary memo bs (pevalFPBinaryTerm op) arg1 arg2
    go memo bs (SomeTerm (FPRoundingUnaryTerm _ op mode arg)) =
      goUnary memo bs (pevalFPRoundingUnaryTerm op mode) arg
    go memo bs (SomeTerm (FPRoundingBinaryTerm _ op mode arg1 arg2)) =
      goBinary memo bs (pevalFPRoundingBinaryTerm op mode) arg1 arg2
    go memo bs (SomeTerm (FPFMATerm _ mode arg1 arg2 arg3)) =
      SomeTerm $
        pevalFPFMATerm
          (gotyped memo bs mode)
          (gotyped memo bs arg1)
          (gotyped memo bs arg2)
          (gotyped memo bs arg3)
    goUnary ::
      (SupportedPrim a, SupportedPrim b) =>
      (S.HashSet SomeTypedConstantSymbol -> SomeTerm -> SomeTerm) ->
      S.HashSet SomeTypedConstantSymbol ->
      (Term a -> Term b) ->
      Term a ->
      SomeTerm
    goUnary memo bs f a = SomeTerm $ f (gotyped memo bs a)
    goBinary ::
      (SupportedPrim a, SupportedPrim b, SupportedPrim c) =>
      (S.HashSet SomeTypedConstantSymbol -> SomeTerm -> SomeTerm) ->
      S.HashSet SomeTypedConstantSymbol ->
      (Term a -> Term b -> Term c) ->
      Term a ->
      Term b ->
      SomeTerm
    goBinary memo bs f a b = SomeTerm $ f (gotyped memo bs a) (gotyped memo bs b)
    goTernary ::
      (SupportedPrim a, SupportedPrim b, SupportedPrim c, SupportedPrim d) =>
      (S.HashSet SomeTypedConstantSymbol -> SomeTerm -> SomeTerm) ->
      S.HashSet SomeTypedConstantSymbol ->
      (Term a -> Term b -> Term c -> Term d) ->
      Term a ->
      Term b ->
      Term c ->
      SomeTerm
    goTernary memo bs f a b c =
      SomeTerm $ f (gotyped memo bs a) (gotyped memo bs b) (gotyped memo bs c)

-- | Evaluate a term in the given model.
evaluateTerm ::
  forall a.
  (SupportedPrim a) =>
  Bool ->
  S.HashSet SomeTypedConstantSymbol ->
  Model ->
  Term a ->
  Term a
evaluateTerm fillDefault boundedSymbols m t =
  case evaluateSomeTerm fillDefault boundedSymbols m $ SomeTerm t of
    SomeTerm (t1 :: Term b) -> unsafeCoerce @(Term b) @(Term a) t1

-- |
-- A type used for building a model by hand.
--
-- >>> buildModel ("x" ::= (1 :: Integer), "y" ::= True) :: Model
-- Model {x -> 1 :: Integer, y -> True :: Bool}
data ModelValuePair t = (TypedAnySymbol t) ::= t deriving (Show)

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
