{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Grisette.Core.Data.UnionBase
  ( UnionBase (..),
    ifWithLeftMost,
    ifWithStrategy,
    fullReconstruct,
  )
where

import Control.DeepSeq
import Data.Functor.Classes
import Data.Hashable
import GHC.Generics
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Data.Class.PrimWrapper
import Grisette.Core.Data.Class.SimpleMergeable
import Language.Haskell.TH.Syntax

-- | The default union implementation.
data UnionBase b a
  = -- | A single value
    Single a
  | -- | A if value
    If
      a
      -- ^ Cached leftmost value
      !Bool
      -- ^ Is merged invariant already maintained?
      !b
      -- ^ If condition
      (UnionBase b a)
      -- ^ True branch
      (UnionBase b a)
      -- ^ False branch
  deriving (Generic, Eq, Lift, Generic1)

instance (Eq b) => Eq1 (UnionBase b) where
  liftEq e (Single a) (Single b) = e a b
  liftEq e (If l1 i1 c1 t1 f1) (If l2 i2 c2 t2 f2) =
    e l1 l2 && i1 == i2 && c1 == c2 && liftEq e t1 t2 && liftEq e f1 f2
  liftEq _ _ _ = False

instance (NFData b, NFData a) => NFData (UnionBase b a) where
  rnf = rnf1

instance (NFData b) => NFData1 (UnionBase b) where
  liftRnf = liftRnf2 rnf

instance NFData2 UnionBase where
  liftRnf2 _b _a (Single a) = _a a
  liftRnf2 _b _a (If a bo b l r) = _a a `seq` rnf bo `seq` _b b `seq` liftRnf2 _b _a l `seq` liftRnf2 _b _a r

-- | Build 'If' with leftmost cache correctly maintained.
--
-- Usually you should never directly try to build a 'If' with its constructor.
ifWithLeftMost :: (SymBoolOp b) => Bool -> b -> UnionBase b a -> UnionBase b a -> UnionBase b a
ifWithLeftMost _ (Conc c) t f
  | c = t
  | otherwise = f
ifWithLeftMost inv cond t f = If (leftMost t) inv cond t f
{-# INLINE ifWithLeftMost #-}

instance SymBoolOp bool => UnionPrjOp bool (UnionBase bool) where
  singleView (Single a) = Just a
  singleView _ = Nothing
  {-# INLINE singleView #-}
  ifView (If _ _ cond ifTrue ifFalse) = Just (cond, ifTrue, ifFalse)
  ifView _ = Nothing
  {-# INLINE ifView #-}
  leftMost (Single a) = a
  leftMost (If a _ _ _ _) = a
  {-# INLINE leftMost #-}

instance (SymBoolOp bool, GMergeable bool a) => GMergeable bool (UnionBase bool a) where
  gmergingStrategy = SimpleStrategy $ ifWithStrategy gmergingStrategy
  {-# INLINE gmergingStrategy #-}

instance (SymBoolOp bool) => GMergeable1 bool (UnionBase bool) where
  liftGMergingStrategy ms = SimpleStrategy $ ifWithStrategy ms
  {-# INLINE liftGMergingStrategy #-}

instance (SymBoolOp bool, GMergeable bool a) => SimpleMergeable bool (UnionBase bool a) where
  mrgIte = mrgIf

instance (SymBoolOp bool) => SimpleMergeable1 bool (UnionBase bool) where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)

instance (SymBoolOp bool) => UnionLike bool (UnionBase bool) where
  mergeWithStrategy = fullReconstruct
  {-# INLINE mergeWithStrategy #-}
  single = Single
  {-# INLINE single #-}
  unionIf = ifWithLeftMost False
  {-# INLINE unionIf #-}
  mrgIfWithStrategy = ifWithStrategy
  {-# INLINE mrgIfWithStrategy #-}
  mrgSingleWithStrategy _ = Single
  {-# INLINE mrgSingleWithStrategy #-}

instance (Show b) => Show1 (UnionBase b) where
  liftShowsPrec sp _ i (Single a) = showsUnaryWith sp "Single" i a
  liftShowsPrec sp sl i (If _ _ cond t f) =
    showParen (i > 10) $
      showString "If" . showChar ' ' . showsPrec 11 cond . showChar ' ' . sp1 11 t . showChar ' ' . sp1 11 f
    where
      sp1 = liftShowsPrec sp sl

instance (Show b, Show a) => Show (UnionBase b a) where
  showsPrec = showsPrec1

instance (Hashable b, Hashable a) => Hashable (UnionBase b a) where
  s `hashWithSalt` (Single a) = s `hashWithSalt` (0 :: Int) `hashWithSalt` a
  s `hashWithSalt` (If _ _ c l r) = s `hashWithSalt` (1 :: Int) `hashWithSalt` c `hashWithSalt` l `hashWithSalt` r

-- | Fully reconstruct a 'UnionBase' to maintain the merged invariant.
fullReconstruct :: (SymBoolOp bool) => GMergingStrategy bool a -> UnionBase bool a -> UnionBase bool a
fullReconstruct strategy (If _ False cond t f) =
  ifWithStrategyInv strategy cond (fullReconstruct strategy t) (fullReconstruct strategy f)
fullReconstruct _ u = u
{-# INLINE fullReconstruct #-}

-- | Use a specific strategy to build a 'If' value.
--
-- The merged invariant will be maintained in the result.
ifWithStrategy ::
  (SymBoolOp bool) =>
  GMergingStrategy bool a ->
  bool ->
  UnionBase bool a ->
  UnionBase bool a ->
  UnionBase bool a
ifWithStrategy strategy cond t@(If _ False _ _ _) f = ifWithStrategy strategy cond (fullReconstruct strategy t) f
ifWithStrategy strategy cond t f@(If _ False _ _ _) = ifWithStrategy strategy cond t (fullReconstruct strategy f)
ifWithStrategy strategy cond t f = ifWithStrategyInv strategy cond t f
{-# INLINE ifWithStrategy #-}

ifWithStrategyInv ::
  (SymBoolOp bool) =>
  GMergingStrategy bool a ->
  bool ->
  UnionBase bool a ->
  UnionBase bool a ->
  UnionBase bool a
ifWithStrategyInv _ (Conc v) t f
  | v = t
  | otherwise = f
ifWithStrategyInv strategy cond (If _ True condTrue tt _) f
  | cond == condTrue = ifWithStrategyInv strategy cond tt f
-- {| nots cond == condTrue || cond == nots condTrue = ifWithStrategyInv strategy cond ft f
ifWithStrategyInv strategy cond t (If _ True condFalse _ ff)
  | cond == condFalse = ifWithStrategyInv strategy cond t ff
-- {| nots cond == condTrue || cond == nots condTrue = ifWithStrategyInv strategy cond t tf -- buggy here condTrue
ifWithStrategyInv (SimpleStrategy m) cond (Single l) (Single r) = Single $ m cond l r
ifWithStrategyInv strategy@(SortedStrategy idxFun substrategy) cond ifTrue ifFalse = case (ifTrue, ifFalse) of
  (Single _, Single _) -> ssIf cond ifTrue ifFalse
  (Single _, If {}) -> sgIf cond ifTrue ifFalse
  (If {}, Single _) -> gsIf cond ifTrue ifFalse
  _ -> ggIf cond ifTrue ifFalse
  where
    ssIf cond' ifTrue' ifFalse'
      | idxt < idxf = ifWithLeftMost True cond' ifTrue' ifFalse'
      | idxt == idxf = ifWithStrategyInv (substrategy idxt) cond' ifTrue' ifFalse'
      | otherwise = ifWithLeftMost True (nots cond') ifFalse' ifTrue'
      where
        idxt = idxFun $ leftMost ifTrue'
        idxf = idxFun $ leftMost ifFalse'
    {-# INLINE ssIf #-}
    sgIf cond' ifTrue' ifFalse'@(If _ True condf ft ff)
      | idxft == idxff = ssIf cond' ifTrue' ifFalse'
      | idxt < idxft = ifWithLeftMost True cond' ifTrue' ifFalse'
      | idxt == idxft = ifWithLeftMost True (cond' ||~ condf) (ifWithStrategyInv (substrategy idxt) cond' ifTrue' ft) ff
      | otherwise = ifWithLeftMost True (nots cond' &&~ condf) ft (ifWithStrategyInv strategy cond' ifTrue' ff)
      where
        idxft = idxFun $ leftMost ft
        idxff = idxFun $ leftMost ff
        idxt = idxFun $ leftMost ifTrue'
    sgIf _ _ _ = undefined
    {-# INLINE sgIf #-}
    gsIf cond' ifTrue'@(If _ True condt tt tf) ifFalse'
      | idxtt == idxtf = ssIf cond' ifTrue' ifFalse'
      | idxtt < idxf = ifWithLeftMost True (cond' &&~ condt) tt $ ifWithStrategyInv strategy cond' tf ifFalse'
      | idxtt == idxf = ifWithLeftMost True (nots cond' ||~ condt) (ifWithStrategyInv (substrategy idxf) cond' tt ifFalse') tf
      | otherwise = ifWithLeftMost True (nots cond') ifFalse' ifTrue'
      where
        idxtt = idxFun $ leftMost tt
        idxtf = idxFun $ leftMost tf
        idxf = idxFun $ leftMost ifFalse'
    gsIf _ _ _ = undefined
    {-# INLINE gsIf #-}
    ggIf cond' ifTrue'@(If _ True condt tt tf) ifFalse'@(If _ True condf ft ff)
      | idxtt == idxtf = sgIf cond' ifTrue' ifFalse'
      | idxft == idxff = gsIf cond' ifTrue' ifFalse'
      | idxtt < idxft = ifWithLeftMost True (cond' &&~ condt) tt $ ifWithStrategyInv strategy cond' tf ifFalse'
      | idxtt == idxft =
          let newCond = ites cond' condt condf
              newIfTrue = ifWithStrategyInv (substrategy idxtt) cond' tt ft
              newIfFalse = ifWithStrategyInv strategy cond' tf ff
           in ifWithLeftMost True newCond newIfTrue newIfFalse
      | otherwise = ifWithLeftMost True (nots cond' &&~ condf) ft $ ifWithStrategyInv strategy cond' ifTrue' ff
      where
        idxtt = idxFun $ leftMost tt
        idxtf = idxFun $ leftMost tf
        idxft = idxFun $ leftMost ft
        idxff = idxFun $ leftMost ff
    ggIf _ _ _ = undefined
    {-# INLINE ggIf #-}
ifWithStrategyInv NoStrategy cond ifTrue ifFalse = ifWithLeftMost True cond ifTrue ifFalse
ifWithStrategyInv _ _ _ _ = error "Invariant violated"
{-# INLINE ifWithStrategyInv #-}
