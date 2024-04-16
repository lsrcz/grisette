{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.Prim.PartialEval.Num
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.Prim.PartialEval.Num
  ( pattern NumConTerm,
    pattern NumOrdConTerm,
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

import Data.Typeable (Typeable, cast, eqT, type (:~:) (Refl))
import Grisette.Core.Data.BV (WordN)
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Unfold
  ( binaryUnfoldOnce,
    unaryUnfoldOnce,
  )
import Grisette.IR.SymPrim.Data.Prim.Term
  ( SupportedPrim,
    Term
      ( AbsNumTerm,
        AddNumTerm,
        ConTerm,
        TimesNumTerm,
        UMinusNumTerm
      ),
    absNumTerm,
    addNumTerm,
    conTerm,
    leNumTerm,
    ltNumTerm,
    signumNumTerm,
    timesNumTerm,
    uminusNumTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.Utils (pattern Dyn)
import qualified Type.Reflection as R
import Unsafe.Coerce (unsafeCoerce)

numConTermView :: (Num b, Typeable b) => Term a -> Maybe b
numConTermView (ConTerm _ b) = cast b
numConTermView _ = Nothing

pattern NumConTerm :: forall b a. (Num b, Typeable b) => b -> Term a
pattern NumConTerm b <- (numConTermView -> Just b)

numOrdConTermView :: (Num b, Ord b, Typeable b) => Term a -> Maybe b
numOrdConTermView (ConTerm _ b) = cast b
numOrdConTermView _ = Nothing

pattern NumOrdConTerm :: forall b a. (Num b, Ord b, Typeable b) => b -> Term a
pattern NumOrdConTerm b <- (numOrdConTermView -> Just b)

-- add
pevalAddNumTerm :: forall a. (SupportedPrim a, Num a, SupportedPrim a) => Term a -> Term a -> Term a
pevalAddNumTerm = binaryUnfoldOnce doPevalAddNumTerm (\a b -> normalizeAddNum $ addNumTerm a b)

doPevalAddNumTerm :: forall a. (Num a, SupportedPrim a) => Term a -> Term a -> Maybe (Term a)
doPevalAddNumTerm (ConTerm _ a) (ConTerm _ b) = Just $ conTerm $ a + b
doPevalAddNumTerm l@(ConTerm _ a) b = case (a, b) of
  (0, k) -> Just k
  (l1, AddNumTerm _ (ConTerm _ j) k) -> Just $ pevalAddNumTerm (conTerm $ l1 + j) k
  _ -> doPevalAddNumTermNoConc l b
doPevalAddNumTerm a r@(ConTerm _ _) = doPevalAddNumTerm r a
doPevalAddNumTerm l r = doPevalAddNumTermNoConc l r

doPevalAddNumTermNoConc :: forall a. (Num a, SupportedPrim a) => Term a -> Term a -> Maybe (Term a)
doPevalAddNumTermNoConc (AddNumTerm _ i@ConTerm {} j) k = Just $ pevalAddNumTerm i $ pevalAddNumTerm j k
doPevalAddNumTermNoConc i (AddNumTerm _ j@ConTerm {} k) = Just $ pevalAddNumTerm j $ pevalAddNumTerm i k
doPevalAddNumTermNoConc (UMinusNumTerm _ i) (UMinusNumTerm _ j) = Just $ pevalUMinusNumTerm $ pevalAddNumTerm i j
doPevalAddNumTermNoConc (TimesNumTerm _ (ConTerm _ i) j) (TimesNumTerm _ (ConTerm _ k) l)
  | j == l = Just $ pevalTimesNumTerm (conTerm $ i + k) j
doPevalAddNumTermNoConc (TimesNumTerm _ i@ConTerm {} j) (TimesNumTerm _ k@(ConTerm _ _) l)
  | i == k = Just $ pevalTimesNumTerm i (pevalAddNumTerm j l)
doPevalAddNumTermNoConc _ _ = Nothing

normalizeAddNum :: forall a. (Num a, Typeable a) => Term a -> Term a
normalizeAddNum (AddNumTerm _ l r@(ConTerm _ _)) = addNumTerm r l
normalizeAddNum v = v

pevalMinusNumTerm :: (Num a, SupportedPrim a) => Term a -> Term a -> Term a
pevalMinusNumTerm l r = pevalAddNumTerm l (pevalUMinusNumTerm r)

-- uminus
pevalUMinusNumTerm :: (Num a, SupportedPrim a) => Term a -> Term a
pevalUMinusNumTerm = unaryUnfoldOnce doPevalUMinusNumTerm uminusNumTerm

doPevalUMinusNumTerm :: forall a. (Num a, SupportedPrim a) => Term a -> Maybe (Term a)
doPevalUMinusNumTerm (ConTerm _ a) = Just $ conTerm $ -a
doPevalUMinusNumTerm (UMinusNumTerm _ v) = Just v
doPevalUMinusNumTerm (AddNumTerm _ (NumConTerm l) r) = Just $ pevalMinusNumTerm (conTerm $ -l) r
doPevalUMinusNumTerm (AddNumTerm _ (UMinusNumTerm _ l) r) = Just $ pevalAddNumTerm l (pevalUMinusNumTerm r)
doPevalUMinusNumTerm (AddNumTerm _ l (UMinusNumTerm _ r)) = Just $ pevalAddNumTerm (pevalUMinusNumTerm l) r
doPevalUMinusNumTerm (TimesNumTerm _ (NumConTerm l) r) = Just $ pevalTimesNumTerm (conTerm $ -l) r
doPevalUMinusNumTerm (TimesNumTerm _ (UMinusNumTerm _ _ :: Term a) (_ :: Term a)) = error "Should not happen"
doPevalUMinusNumTerm (TimesNumTerm _ (_ :: Term a) (UMinusNumTerm _ (_ :: Term a))) = error "Should not happen"
doPevalUMinusNumTerm (AddNumTerm _ (_ :: Term a) ConTerm {}) = error "Should not happen"
doPevalUMinusNumTerm _ = Nothing

-- times
pevalTimesNumTerm :: forall a. (Num a, SupportedPrim a) => Term a -> Term a -> Term a
pevalTimesNumTerm = binaryUnfoldOnce doPevalTimesNumTerm (\a b -> normalizeTimesNum $ timesNumTerm a b)

normalizeTimesNum :: forall a. (Num a, Typeable a) => Term a -> Term a
normalizeTimesNum (TimesNumTerm _ l r@(ConTerm _ _)) = timesNumTerm r l
normalizeTimesNum v = v

doPevalTimesNumTerm :: forall a. (Num a, SupportedPrim a) => Term a -> Term a -> Maybe (Term a)
doPevalTimesNumTerm (ConTerm _ a) (ConTerm _ b) = Just $ conTerm $ a * b
doPevalTimesNumTerm l@(ConTerm _ a) b = case (a, b) of
  (0, _) -> Just $ conTerm 0
  (1, k) -> Just k
  (-1, k) -> Just $ pevalUMinusNumTerm k
  (l1, TimesNumTerm _ (NumConTerm j) k) -> Just $ pevalTimesNumTerm (conTerm $ l1 * j) k
  (l1, AddNumTerm _ (NumConTerm j) k) -> Just $ pevalAddNumTerm (conTerm $ l1 * j) (pevalTimesNumTerm (conTerm l1) k)
  (l1, UMinusNumTerm _ j) -> Just (pevalTimesNumTerm (conTerm $ -l1) j)
  (_, TimesNumTerm _ (_ :: Term a) ConTerm {}) -> error "Should not happen"
  (_, AddNumTerm _ (_ :: Term a) ConTerm {}) -> error "Should not happen"
  _ -> doPevalTimesNumTermNoConc l b
doPevalTimesNumTerm a r@(ConTerm _ _) = doPevalTimesNumTerm r a
doPevalTimesNumTerm l r = doPevalTimesNumTermNoConc l r

doPevalTimesNumTermNoConc :: forall a. (Num a, SupportedPrim a) => Term a -> Term a -> Maybe (Term a)
doPevalTimesNumTermNoConc (TimesNumTerm _ i@ConTerm {} j) k = Just $ pevalTimesNumTerm i $ pevalTimesNumTerm j k
doPevalTimesNumTermNoConc i (TimesNumTerm _ j@ConTerm {} k) = Just $ pevalTimesNumTerm j $ pevalTimesNumTerm i k
doPevalTimesNumTermNoConc (UMinusNumTerm _ i) j = Just $ pevalUMinusNumTerm $ pevalTimesNumTerm i j
doPevalTimesNumTermNoConc i (UMinusNumTerm _ j) = Just $ pevalUMinusNumTerm $ pevalTimesNumTerm i j
doPevalTimesNumTermNoConc i j@ConTerm {} = Just $ pevalTimesNumTerm j i
doPevalTimesNumTermNoConc (TimesNumTerm _ (_ :: Term a) ConTerm {}) _ = error "Should not happen"
doPevalTimesNumTermNoConc _ (TimesNumTerm _ (_ :: Term a) ConTerm {}) = error "Should not happen"
doPevalTimesNumTermNoConc _ _ = Nothing

-- abs
pevalAbsNumTerm :: (Num a, SupportedPrim a) => Term a -> Term a
pevalAbsNumTerm = unaryUnfoldOnce doPevalAbsNumTerm absNumTerm

isUnsignedBV :: R.TypeRep a -> Bool
isUnsignedBV (R.App s _) =
  case R.eqTypeRep s $ R.typeRep @WordN of
    Just R.HRefl -> True
    _ -> False
isUnsignedBV _ = False

doPevalAbsNumTerm :: forall a. (Num a, SupportedPrim a) => Term a -> Maybe (Term a)
doPevalAbsNumTerm x | isUnsignedBV (R.typeRep @a) = Just x
doPevalAbsNumTerm (ConTerm _ a) = Just $ conTerm $ abs a
doPevalAbsNumTerm (UMinusNumTerm _ v) = Just $ pevalAbsNumTerm v
doPevalAbsNumTerm t@(AbsNumTerm _ (_ :: Term a)) = Just t
doPevalAbsNumTerm (TimesNumTerm _ (Dyn (l :: Term Integer)) r) =
  Just $ pevalTimesNumTerm (pevalAbsNumTerm $ unsafeCoerce l :: Term a) $ pevalAbsNumTerm (unsafeCoerce r)
doPevalAbsNumTerm _ = Nothing

-- signum
pevalSignumNumTerm :: (Num a, SupportedPrim a) => Term a -> Term a
pevalSignumNumTerm = unaryUnfoldOnce doPevalSignumNumTerm signumNumTerm

doPevalSignumNumTerm :: forall a. (Num a, SupportedPrim a) => Term a -> Maybe (Term a)
doPevalSignumNumTerm (ConTerm _ a) = Just $ conTerm $ signum a
doPevalSignumNumTerm (UMinusNumTerm _ (Dyn (v :: Term Integer))) = Just $ pevalUMinusNumTerm $ pevalSignumNumTerm $ unsafeCoerce v
doPevalSignumNumTerm (TimesNumTerm _ (Dyn (l :: Term Integer)) r) =
  Just $ pevalTimesNumTerm (pevalSignumNumTerm $ unsafeCoerce l :: Term a) $ pevalSignumNumTerm (unsafeCoerce r)
doPevalSignumNumTerm _ = Nothing

-- lt
pevalLtNumTerm :: (Num a, Ord a, SupportedPrim a, SupportedPrim Bool, SupportedPrim Integer) => Term a -> Term a -> Term Bool
pevalLtNumTerm = binaryUnfoldOnce doPevalLtNumTerm ltNumTerm

doPevalLtNumTerm :: forall a. (Num a, Ord a, SupportedPrim a, SupportedPrim Bool, SupportedPrim Integer) => Term a -> Term a -> Maybe (Term Bool)
doPevalLtNumTerm (ConTerm _ a) (ConTerm _ b) = Just $ conTerm $ a < b
doPevalLtNumTerm (ConTerm _ l) (AddNumTerm _ (ConTerm _ (Dyn (j :: Integer))) k) =
  Just $ pevalLtNumTerm (conTerm $ unsafeCoerce l - j) (unsafeCoerce k)
doPevalLtNumTerm (AddNumTerm _ (ConTerm _ (Dyn (i :: Integer))) j) (ConTerm _ k) =
  Just $ pevalLtNumTerm (unsafeCoerce j) (conTerm $ unsafeCoerce k - i)
doPevalLtNumTerm (AddNumTerm _ (ConTerm _ (Dyn (j :: Integer))) k) l =
  Just $ pevalLtNumTerm (conTerm j) (pevalMinusNumTerm (unsafeCoerce l) (unsafeCoerce k))
doPevalLtNumTerm j (AddNumTerm _ (ConTerm _ (Dyn (k :: Integer))) l) =
  Just $ pevalLtNumTerm (conTerm $ -k) (pevalMinusNumTerm (unsafeCoerce l) (unsafeCoerce j))
doPevalLtNumTerm l (ConTerm _ r) =
  case eqT @a @Integer of
    Just Refl ->
      Just $ pevalLtNumTerm (conTerm $ -r) (pevalUMinusNumTerm l)
    _ -> Nothing
doPevalLtNumTerm _ _ = Nothing

-- le
pevalLeNumTerm :: (Num a, Ord a, SupportedPrim a, SupportedPrim Bool, SupportedPrim Integer) => Term a -> Term a -> Term Bool
pevalLeNumTerm = binaryUnfoldOnce doPevalLeNumTerm leNumTerm

doPevalLeNumTerm :: forall a. (Num a, Ord a, SupportedPrim a, SupportedPrim Bool, SupportedPrim Integer) => Term a -> Term a -> Maybe (Term Bool)
doPevalLeNumTerm (ConTerm _ a) (ConTerm _ b) = Just $ conTerm $ a <= b
doPevalLeNumTerm (ConTerm _ l) (AddNumTerm _ (ConTerm _ (Dyn (j :: Integer))) k) =
  Just $ pevalLeNumTerm (conTerm $ unsafeCoerce l - j) (unsafeCoerce k)
doPevalLeNumTerm (AddNumTerm _ (ConTerm _ (Dyn (i :: Integer))) j) (ConTerm _ k) =
  Just $ pevalLeNumTerm (unsafeCoerce j) (conTerm $ unsafeCoerce k - i)
doPevalLeNumTerm (AddNumTerm _ (ConTerm _ (Dyn (j :: Integer))) k) l =
  Just $ pevalLeNumTerm (conTerm j) (pevalMinusNumTerm (unsafeCoerce l) (unsafeCoerce k))
doPevalLeNumTerm j (AddNumTerm _ (ConTerm _ (Dyn (k :: Integer))) l) =
  Just $ pevalLeNumTerm (conTerm $ -k) (pevalMinusNumTerm (unsafeCoerce l) (unsafeCoerce j))
doPevalLeNumTerm l (ConTerm _ r) =
  case eqT @a @Integer of
    Just Refl ->
      Just $ pevalLeNumTerm (conTerm $ -r) (pevalUMinusNumTerm l)
    _ -> Nothing
doPevalLeNumTerm _ _ = Nothing

pevalGtNumTerm :: (Num a, Ord a, SupportedPrim a, SupportedPrim Bool, SupportedPrim Integer) => Term a -> Term a -> Term Bool
pevalGtNumTerm = flip pevalLtNumTerm

pevalGeNumTerm :: (Num a, Ord a, SupportedPrim a, SupportedPrim Bool, SupportedPrim Integer) => Term a -> Term a -> Term Bool
pevalGeNumTerm = flip pevalLeNumTerm
