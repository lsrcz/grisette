{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Grisette.IR.SymPrim.Data.Prim.PartialEval.Num
  ( pattern NumConcTerm,
    pattern NumOrdConcTerm,
    pevalAddNumTerm,
    pevalMinusNumTerm,
    pevalTimesNumTerm,
    pevalUMinusNumTerm,
    pevalAbsNumTerm,
    pevalSignumNumTerm,
    pevalLtNumTerm,
    pevalLeNumTerm,
    pevalGtNumTerm,
    pevalGeNumTerm,
  )
where

import Data.Typeable
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Unfold
import Grisette.IR.SymPrim.Data.Prim.Utils
import Unsafe.Coerce

numConcTermView :: (Num b, Typeable b) => Term a -> Maybe b
numConcTermView (ConcTerm _ b) = cast b
numConcTermView _ = Nothing

pattern NumConcTerm :: forall b a. (Num b, Typeable b) => b -> Term a
pattern NumConcTerm b <- (numConcTermView -> Just b)

numOrdConcTermView :: (Num b, Ord b, Typeable b) => Term a -> Maybe b
numOrdConcTermView (ConcTerm _ b) = cast b
numOrdConcTermView _ = Nothing

pattern NumOrdConcTerm :: forall b a. (Num b, Ord b, Typeable b) => b -> Term a
pattern NumOrdConcTerm b <- (numOrdConcTermView -> Just b)

-- add
pevalAddNumTerm :: forall a. (Num a, SupportedPrim a) => Term a -> Term a -> Term a
pevalAddNumTerm = binaryUnfoldOnce doPevalAddNumTerm (\a b -> normalizeAddNum $ addNumTerm a b)

doPevalAddNumTerm :: forall a. (Num a, SupportedPrim a) => Term a -> Term a -> Maybe (Term a)
doPevalAddNumTerm (ConcTerm _ a) (ConcTerm _ b) = Just $ concTerm $ a + b
doPevalAddNumTerm l@(ConcTerm _ a) b = case (a, b) of
  (0, k) -> Just k
  (l1, AddNumTerm _ (ConcTerm _ j) k) -> Just $ pevalAddNumTerm (concTerm $ l1 + j) k
  _ -> doPevalAddNumTermNoConc l b
doPevalAddNumTerm a r@(ConcTerm _ _) = doPevalAddNumTerm r a
doPevalAddNumTerm l r = doPevalAddNumTermNoConc l r

doPevalAddNumTermNoConc :: forall a. (Num a, SupportedPrim a) => Term a -> Term a -> Maybe (Term a)
doPevalAddNumTermNoConc (AddNumTerm _ i@ConcTerm {} j) k = Just $ pevalAddNumTerm i $ pevalAddNumTerm j k
doPevalAddNumTermNoConc i (AddNumTerm _ j@ConcTerm {} k) = Just $ pevalAddNumTerm j $ pevalAddNumTerm i k
doPevalAddNumTermNoConc (UMinusNumTerm _ i) (UMinusNumTerm _ j) = Just $ pevalUMinusNumTerm $ pevalAddNumTerm i j
doPevalAddNumTermNoConc (TimesNumTerm _ (ConcTerm _ i) j) (TimesNumTerm _ (ConcTerm _ k) l)
  | j == l = Just $ pevalTimesNumTerm (concTerm $ i + k) j
doPevalAddNumTermNoConc (TimesNumTerm _ i@ConcTerm {} j) (TimesNumTerm _ k@(ConcTerm _ _) l)
  | i == k = Just $ pevalTimesNumTerm i (pevalAddNumTerm j l)
doPevalAddNumTermNoConc _ _ = Nothing

normalizeAddNum :: forall a. (Num a, Typeable a) => Term a -> Term a
normalizeAddNum (AddNumTerm _ l r@(ConcTerm _ _)) = addNumTerm r l
normalizeAddNum v = v

pevalMinusNumTerm :: (Num a, SupportedPrim a) => Term a -> Term a -> Term a
pevalMinusNumTerm l r = pevalAddNumTerm l (pevalUMinusNumTerm r)

-- uminus
pevalUMinusNumTerm :: (Num a, SupportedPrim a) => Term a -> Term a
pevalUMinusNumTerm = unaryUnfoldOnce doPevalUMinusNumTerm uminusNumTerm

doPevalUMinusNumTerm :: forall a. (Num a, SupportedPrim a) => Term a -> Maybe (Term a)
doPevalUMinusNumTerm (ConcTerm _ a) = Just $ concTerm $ -a
doPevalUMinusNumTerm (UMinusNumTerm _ v) = Just v
doPevalUMinusNumTerm (AddNumTerm _ (NumConcTerm l) r) = Just $ pevalMinusNumTerm (concTerm $ -l) r
doPevalUMinusNumTerm (AddNumTerm _ (UMinusNumTerm _ l) r) = Just $ pevalAddNumTerm l (pevalUMinusNumTerm r)
doPevalUMinusNumTerm (AddNumTerm _ l (UMinusNumTerm _ r)) = Just $ pevalAddNumTerm (pevalUMinusNumTerm l) r
doPevalUMinusNumTerm (TimesNumTerm _ (NumConcTerm l) r) = Just $ pevalTimesNumTerm (concTerm $ -l) r
doPevalUMinusNumTerm (TimesNumTerm _ (UMinusNumTerm _ _ :: Term a) (_ :: Term a)) = error "Should not happen"
doPevalUMinusNumTerm (TimesNumTerm _ (_ :: Term a) (UMinusNumTerm _ (_ :: Term a))) = error "Should not happen"
doPevalUMinusNumTerm (AddNumTerm _ (_ :: Term a) ConcTerm {}) = error "Should not happen"
doPevalUMinusNumTerm _ = Nothing

-- times
pevalTimesNumTerm :: forall a. (Num a, SupportedPrim a) => Term a -> Term a -> Term a
pevalTimesNumTerm = binaryUnfoldOnce doPevalTimesNumTerm (\a b -> normalizeTimesNum $ timesNumTerm a b)

normalizeTimesNum :: forall a. (Num a, Typeable a) => Term a -> Term a
normalizeTimesNum (TimesNumTerm _ l r@(ConcTerm _ _)) = timesNumTerm r l
normalizeTimesNum v = v

doPevalTimesNumTerm :: forall a. (Num a, SupportedPrim a) => Term a -> Term a -> Maybe (Term a)
doPevalTimesNumTerm (ConcTerm _ a) (ConcTerm _ b) = Just $ concTerm $ a * b
doPevalTimesNumTerm l@(ConcTerm _ a) b = case (a, b) of
  (0, _) -> Just $ concTerm 0
  (1, k) -> Just k
  (-1, k) -> Just $ pevalUMinusNumTerm k
  (l1, TimesNumTerm _ (NumConcTerm j) k) -> Just $ pevalTimesNumTerm (concTerm $ l1 * j) k
  (l1, AddNumTerm _ (NumConcTerm j) k) -> Just $ pevalAddNumTerm (concTerm $ l1 * j) (pevalTimesNumTerm (concTerm l1) k)
  (l1, UMinusNumTerm _ j) -> Just (pevalTimesNumTerm (concTerm $ -l1) j)
  (_, TimesNumTerm _ (_ :: Term a) ConcTerm {}) -> error "Should not happen"
  (_, AddNumTerm _ (_ :: Term a) ConcTerm {}) -> error "Should not happen"
  _ -> doPevalTimesNumTermNoConc l b
doPevalTimesNumTerm a r@(ConcTerm _ _) = doPevalTimesNumTerm r a
doPevalTimesNumTerm l r = doPevalTimesNumTermNoConc l r

doPevalTimesNumTermNoConc :: forall a. (Num a, SupportedPrim a) => Term a -> Term a -> Maybe (Term a)
doPevalTimesNumTermNoConc (TimesNumTerm _ i@ConcTerm {} j) k = Just $ pevalTimesNumTerm i $ pevalTimesNumTerm j k
doPevalTimesNumTermNoConc i (TimesNumTerm _ j@ConcTerm {} k) = Just $ pevalTimesNumTerm j $ pevalTimesNumTerm i k
doPevalTimesNumTermNoConc (UMinusNumTerm _ i) j = Just $ pevalUMinusNumTerm $ pevalTimesNumTerm i j
doPevalTimesNumTermNoConc i (UMinusNumTerm _ j) = Just $ pevalUMinusNumTerm $ pevalTimesNumTerm i j
doPevalTimesNumTermNoConc i j@ConcTerm {} = Just $ pevalTimesNumTerm j i
doPevalTimesNumTermNoConc (TimesNumTerm _ (_ :: Term a) ConcTerm {}) _ = error "Should not happen"
doPevalTimesNumTermNoConc _ (TimesNumTerm _ (_ :: Term a) ConcTerm {}) = error "Should not happen"
doPevalTimesNumTermNoConc _ _ = Nothing

-- abs
pevalAbsNumTerm :: (SupportedPrim a, Num a) => Term a -> Term a
pevalAbsNumTerm = unaryUnfoldOnce doPevalAbsNumTerm absNumTerm

doPevalAbsNumTerm :: forall a. (Num a, SupportedPrim a) => Term a -> Maybe (Term a)
doPevalAbsNumTerm (ConcTerm _ a) = Just $ concTerm $ abs a
doPevalAbsNumTerm (UMinusNumTerm _ v) = Just $ pevalAbsNumTerm v
doPevalAbsNumTerm t@(AbsNumTerm _ (_ :: Term a)) = Just t
doPevalAbsNumTerm (TimesNumTerm _ (Dyn (l :: Term Integer)) r) =
  Just $ pevalTimesNumTerm (pevalAbsNumTerm $ unsafeCoerce l :: Term a) $ pevalAbsNumTerm (unsafeCoerce r)
doPevalAbsNumTerm _ = Nothing

-- signum
pevalSignumNumTerm :: (Num a, SupportedPrim a) => Term a -> Term a
pevalSignumNumTerm = unaryUnfoldOnce doPevalSignumNumTerm signumNumTerm

doPevalSignumNumTerm :: forall a. (Num a, SupportedPrim a) => Term a -> Maybe (Term a)
doPevalSignumNumTerm (ConcTerm _ a) = Just $ concTerm $ signum a
doPevalSignumNumTerm (UMinusNumTerm _ (Dyn (v :: Term Integer))) = Just $ pevalUMinusNumTerm $ pevalSignumNumTerm $ unsafeCoerce v
doPevalSignumNumTerm (TimesNumTerm _ (Dyn (l :: Term Integer)) r) =
  Just $ pevalTimesNumTerm (pevalSignumNumTerm $ unsafeCoerce l :: Term a) $ pevalSignumNumTerm (unsafeCoerce r)
doPevalSignumNumTerm _ = Nothing

-- lt
pevalLtNumTerm :: (Num a, Ord a, SupportedPrim a) => Term a -> Term a -> Term Bool
pevalLtNumTerm = binaryUnfoldOnce doPevalLtNumTerm ltNumTerm

doPevalLtNumTerm :: forall a. (Num a, Ord a, SupportedPrim a) => Term a -> Term a -> Maybe (Term Bool)
doPevalLtNumTerm (ConcTerm _ a) (ConcTerm _ b) = Just $ concTerm $ a < b
doPevalLtNumTerm (ConcTerm _ l) (AddNumTerm _ (ConcTerm _ (Dyn (j :: Integer))) k) =
  Just $ pevalLtNumTerm (concTerm $ unsafeCoerce l - j) (unsafeCoerce k)
doPevalLtNumTerm (AddNumTerm _ (ConcTerm _ (Dyn (i :: Integer))) j) (ConcTerm _ k) =
  Just $ pevalLtNumTerm (unsafeCoerce j) (concTerm $ unsafeCoerce k - i)
doPevalLtNumTerm (AddNumTerm _ (ConcTerm _ (Dyn (j :: Integer))) k) l =
  Just $ pevalLtNumTerm (concTerm j) (pevalMinusNumTerm (unsafeCoerce l) (unsafeCoerce k))
doPevalLtNumTerm j (AddNumTerm _ (ConcTerm _ (Dyn (k :: Integer))) l) =
  Just $ pevalLtNumTerm (concTerm $ -k) (pevalMinusNumTerm (unsafeCoerce l) (unsafeCoerce j))
doPevalLtNumTerm l (ConcTerm _ r) =
  case eqT @a @Integer of
    Just Refl ->
      Just $ pevalLtNumTerm (concTerm $ -r) (pevalUMinusNumTerm l)
    _ -> Nothing
doPevalLtNumTerm _ _ = Nothing

-- le
pevalLeNumTerm :: (Num a, Ord a, SupportedPrim a) => Term a -> Term a -> Term Bool
pevalLeNumTerm = binaryUnfoldOnce doPevalLeNumTerm leNumTerm

doPevalLeNumTerm :: forall a. (Num a, Ord a, SupportedPrim a) => Term a -> Term a -> Maybe (Term Bool)
doPevalLeNumTerm (ConcTerm _ a) (ConcTerm _ b) = Just $ concTerm $ a <= b
doPevalLeNumTerm (ConcTerm _ l) (AddNumTerm _ (ConcTerm _ (Dyn (j :: Integer))) k) =
  Just $ pevalLeNumTerm (concTerm $ unsafeCoerce l - j) (unsafeCoerce k)
doPevalLeNumTerm (AddNumTerm _ (ConcTerm _ (Dyn (i :: Integer))) j) (ConcTerm _ k) =
  Just $ pevalLeNumTerm (unsafeCoerce j) (concTerm $ unsafeCoerce k - i)
doPevalLeNumTerm (AddNumTerm _ (ConcTerm _ (Dyn (j :: Integer))) k) l =
  Just $ pevalLeNumTerm (concTerm j) (pevalMinusNumTerm (unsafeCoerce l) (unsafeCoerce k))
doPevalLeNumTerm j (AddNumTerm _ (ConcTerm _ (Dyn (k :: Integer))) l) =
  Just $ pevalLeNumTerm (concTerm $ -k) (pevalMinusNumTerm (unsafeCoerce l) (unsafeCoerce j))
doPevalLeNumTerm l (ConcTerm _ r) =
  case eqT @a @Integer of
    Just Refl ->
      Just $ pevalLeNumTerm (concTerm $ -r) (pevalUMinusNumTerm l)
    _ -> Nothing
doPevalLeNumTerm _ _ = Nothing

pevalGtNumTerm :: (Num a, Ord a, SupportedPrim a) => Term a -> Term a -> Term Bool
pevalGtNumTerm = flip pevalLtNumTerm

pevalGeNumTerm :: (Num a, Ord a, SupportedPrim a) => Term a -> Term a -> Term Bool
pevalGeNumTerm = flip pevalLeNumTerm
