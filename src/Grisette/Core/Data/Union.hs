{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :   Grisette.Core.Data.Union
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.Union
  ( -- * The union data structure.

    -- | Please consider using 'Grisette.Core.Control.Monad.UnionM' instead.
    Union (..),
    ifWithLeftMost,
    ifWithStrategy,
    fullReconstruct,
  )
where

import Control.DeepSeq (NFData (rnf), NFData1 (liftRnf), rnf1)
import Control.Monad (ap)
import Data.Functor.Classes
  ( Eq1 (liftEq),
    Show1 (liftShowsPrec),
    showsPrec1,
    showsUnaryWith,
  )
import Data.Hashable (Hashable (hashWithSalt))
import GHC.Generics (Generic, Generic1)
import Grisette.Core.Data.Class.GPretty
  ( GPretty (gprettyPrec),
    condEnclose,
  )
import Grisette.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Core.Data.Class.LogicalOp (LogicalOp (symNot, (.&&), (.||)))
import Grisette.Core.Data.Class.Mergeable
  ( Mergeable (rootStrategy),
    Mergeable1 (liftRootStrategy),
    MergingStrategy (NoStrategy, SimpleStrategy, SortedStrategy),
  )
import Grisette.Core.Data.Class.PlainUnion
  ( PlainUnion (ifView, singleView),
  )
import Grisette.Core.Data.Class.SimpleMergeable
  ( SimpleMergeable (mrgIte),
    SimpleMergeable1 (liftMrgIte),
    UnionMergeable1 (mrgIfPropagatedStrategy, mrgIfWithStrategy),
    mrgIf,
  )
import Grisette.Core.Data.Class.Solvable (pattern Con)
import Grisette.Core.Data.Class.TryMerge (TryMerge (tryMergeWithStrategy))
import Grisette.IR.SymPrim.Data.AllSyms
  ( AllSyms (allSymsS),
    SomeSym (SomeSym),
  )
import Grisette.IR.SymPrim.Data.SymBool (SymBool)
import Language.Haskell.TH.Syntax (Lift)

#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter (align, group, nest, vsep)
#else
import Data.Text.Prettyprint.Doc (align, group, nest, vsep)
#endif

-- | The default union implementation.
data Union a where
  -- | A single value
  UnionSingle :: a -> Union a
  -- | A if value
  UnionIf ::
    -- | Cached leftmost value
    a ->
    -- | Is merged invariant already maintained?
    !Bool ->
    -- | If condition
    !SymBool ->
    -- | True branch
    Union a ->
    -- | False branch
    Union a ->
    Union a
  deriving (Generic, Eq, Lift, Generic1)
  deriving (Functor)

instance Applicative Union where
  pure = UnionSingle
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad Union where
  return = pure
  {-# INLINE return #-}
  UnionSingle a >>= f = f a
  UnionIf _ _ c t f >>= f' = ifWithLeftMost False c (t >>= f') (f >>= f')
  {-# INLINE (>>=) #-}

instance Eq1 Union where
  liftEq e (UnionSingle a) (UnionSingle b) = e a b
  liftEq e (UnionIf l1 i1 c1 t1 f1) (UnionIf l2 i2 c2 t2 f2) =
    e l1 l2 && i1 == i2 && c1 == c2 && liftEq e t1 t2 && liftEq e f1 f2
  liftEq _ _ _ = False

instance (NFData a) => NFData (Union a) where
  rnf = rnf1

instance NFData1 Union where
  liftRnf _a (UnionSingle a) = _a a
  liftRnf _a (UnionIf a bo b l r) =
    _a a `seq`
      rnf bo `seq`
        rnf b `seq`
          liftRnf _a l `seq`
            liftRnf _a r

-- | Build 'UnionIf' with leftmost cache correctly maintained.
--
-- Usually you should never directly try to build a 'UnionIf' with its
-- constructor.
ifWithLeftMost :: Bool -> SymBool -> Union a -> Union a -> Union a
ifWithLeftMost _ (Con c) t f
  | c = t
  | otherwise = f
ifWithLeftMost inv cond t f = UnionIf (leftMost t) inv cond t f
{-# INLINE ifWithLeftMost #-}

instance PlainUnion Union where
  singleView (UnionSingle a) = Just a
  singleView _ = Nothing
  {-# INLINE singleView #-}
  ifView (UnionIf _ _ cond ifTrue ifFalse) = Just (cond, ifTrue, ifFalse)
  ifView _ = Nothing
  {-# INLINE ifView #-}

leftMost :: Union a -> a
leftMost (UnionSingle a) = a
leftMost (UnionIf a _ _ _ _) = a
{-# INLINE leftMost #-}

instance (Mergeable a) => Mergeable (Union a) where
  rootStrategy = SimpleStrategy $ ifWithStrategy rootStrategy
  {-# INLINE rootStrategy #-}

instance Mergeable1 Union where
  liftRootStrategy ms = SimpleStrategy $ ifWithStrategy ms
  {-# INLINE liftRootStrategy #-}

instance (Mergeable a) => SimpleMergeable (Union a) where
  mrgIte = mrgIf

instance SimpleMergeable1 Union where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)

instance TryMerge Union where
  tryMergeWithStrategy = fullReconstruct
  {-# INLINE tryMergeWithStrategy #-}

instance UnionMergeable1 Union where
  mrgIfWithStrategy = ifWithStrategy
  {-# INLINE mrgIfWithStrategy #-}

  mrgIfPropagatedStrategy = ifWithLeftMost False
  {-# INLINE mrgIfPropagatedStrategy #-}

instance Show1 Union where
  liftShowsPrec sp _ i (UnionSingle a) = showsUnaryWith sp "Single" i a
  liftShowsPrec sp sl i (UnionIf _ _ cond t f) =
    showParen (i > 10) $
      showString "If"
        . showChar ' '
        . showsPrec 11 cond
        . showChar ' '
        . sp1 11 t
        . showChar ' '
        . sp1 11 f
    where
      sp1 = liftShowsPrec sp sl

instance (Show a) => Show (Union a) where
  showsPrec = showsPrec1

instance (GPretty a) => GPretty (Union a) where
  gprettyPrec n (UnionSingle a) = gprettyPrec n a
  gprettyPrec n (UnionIf _ _ cond t f) =
    group $
      condEnclose (n > 10) "(" ")" $
        align $
          nest 2 $
            vsep
              [ "If",
                gprettyPrec 11 cond,
                gprettyPrec 11 t,
                gprettyPrec 11 f
              ]

instance (Hashable a) => Hashable (Union a) where
  s `hashWithSalt` (UnionSingle a) =
    s `hashWithSalt` (0 :: Int) `hashWithSalt` a
  s `hashWithSalt` (UnionIf _ _ c l r) =
    s
      `hashWithSalt` (1 :: Int)
      `hashWithSalt` c
      `hashWithSalt` l
      `hashWithSalt` r

instance (AllSyms a) => AllSyms (Union a) where
  allSymsS (UnionSingle v) = allSymsS v
  allSymsS (UnionIf _ _ c t f) = \l -> SomeSym c : (allSymsS t . allSymsS f $ l)

-- | Fully reconstruct a 'Union' to maintain the merged invariant.
fullReconstruct :: MergingStrategy a -> Union a -> Union a
fullReconstruct strategy (UnionIf _ False cond t f) =
  ifWithStrategyInv
    strategy
    cond
    (fullReconstruct strategy t)
    (fullReconstruct strategy f)
fullReconstruct _ u = u
{-# INLINE fullReconstruct #-}

-- | Use a specific strategy to build a 'UnionIf' value.
--
-- The merged invariant will be maintained in the result.
ifWithStrategy ::
  MergingStrategy a ->
  SymBool ->
  Union a ->
  Union a ->
  Union a
ifWithStrategy strategy cond t@(UnionIf _ False _ _ _) f =
  ifWithStrategy strategy cond (fullReconstruct strategy t) f
ifWithStrategy strategy cond t f@(UnionIf _ False _ _ _) =
  ifWithStrategy strategy cond t (fullReconstruct strategy f)
ifWithStrategy strategy cond t f = ifWithStrategyInv strategy cond t f
{-# INLINE ifWithStrategy #-}

ifWithStrategyInv ::
  MergingStrategy a ->
  SymBool ->
  Union a ->
  Union a ->
  Union a
ifWithStrategyInv _ (Con v) t f
  | v = t
  | otherwise = f
ifWithStrategyInv strategy cond (UnionIf _ True condTrue tt _) f
  | cond == condTrue = ifWithStrategyInv strategy cond tt f
-- {| symNot cond == condTrue || cond == symNot condTrue = ifWithStrategyInv strategy cond ft f
ifWithStrategyInv strategy cond t (UnionIf _ True condFalse _ ff)
  | cond == condFalse = ifWithStrategyInv strategy cond t ff
-- {| symNot cond == condTrue || cond == symNot condTrue = ifWithStrategyInv strategy cond t tf -- buggy here condTrue
ifWithStrategyInv (SimpleStrategy m) cond (UnionSingle l) (UnionSingle r) = UnionSingle $ m cond l r
ifWithStrategyInv strategy@(SortedStrategy idxFun substrategy) cond ifTrue ifFalse = case (ifTrue, ifFalse) of
  (UnionSingle _, UnionSingle _) -> ssUnionIf cond ifTrue ifFalse
  (UnionSingle _, UnionIf {}) -> sgUnionIf cond ifTrue ifFalse
  (UnionIf {}, UnionSingle _) -> gsUnionIf cond ifTrue ifFalse
  _ -> ggUnionIf cond ifTrue ifFalse
  where
    ssUnionIf cond' ifTrue' ifFalse'
      | idxt < idxf = ifWithLeftMost True cond' ifTrue' ifFalse'
      | idxt == idxf = ifWithStrategyInv (substrategy idxt) cond' ifTrue' ifFalse'
      | otherwise = ifWithLeftMost True (symNot cond') ifFalse' ifTrue'
      where
        idxt = idxFun $ leftMost ifTrue'
        idxf = idxFun $ leftMost ifFalse'
    {-# INLINE ssUnionIf #-}
    sgUnionIf cond' ifTrue' ifFalse'@(UnionIf _ True condf ft ff)
      | idxft == idxff = ssUnionIf cond' ifTrue' ifFalse'
      | idxt < idxft = ifWithLeftMost True cond' ifTrue' ifFalse'
      | idxt == idxft = ifWithLeftMost True (cond' .|| condf) (ifWithStrategyInv (substrategy idxt) cond' ifTrue' ft) ff
      | otherwise = ifWithLeftMost True (symNot cond' .&& condf) ft (ifWithStrategyInv strategy cond' ifTrue' ff)
      where
        idxft = idxFun $ leftMost ft
        idxff = idxFun $ leftMost ff
        idxt = idxFun $ leftMost ifTrue'
    sgUnionIf _ _ _ = undefined
    {-# INLINE sgUnionIf #-}
    gsUnionIf cond' ifTrue'@(UnionIf _ True condt tt tf) ifFalse'
      | idxtt == idxtf = ssUnionIf cond' ifTrue' ifFalse'
      | idxtt < idxf = ifWithLeftMost True (cond' .&& condt) tt $ ifWithStrategyInv strategy cond' tf ifFalse'
      | idxtt == idxf = ifWithLeftMost True (symNot cond' .|| condt) (ifWithStrategyInv (substrategy idxf) cond' tt ifFalse') tf
      | otherwise = ifWithLeftMost True (symNot cond') ifFalse' ifTrue'
      where
        idxtt = idxFun $ leftMost tt
        idxtf = idxFun $ leftMost tf
        idxf = idxFun $ leftMost ifFalse'
    gsUnionIf _ _ _ = undefined
    {-# INLINE gsUnionIf #-}
    ggUnionIf cond' ifTrue'@(UnionIf _ True condt tt tf) ifFalse'@(UnionIf _ True condf ft ff)
      | idxtt == idxtf = sgUnionIf cond' ifTrue' ifFalse'
      | idxft == idxff = gsUnionIf cond' ifTrue' ifFalse'
      | idxtt < idxft = ifWithLeftMost True (cond' .&& condt) tt $ ifWithStrategyInv strategy cond' tf ifFalse'
      | idxtt == idxft =
          let newCond = symIte cond' condt condf
              newUnionIfTrue = ifWithStrategyInv (substrategy idxtt) cond' tt ft
              newUnionIfFalse = ifWithStrategyInv strategy cond' tf ff
           in ifWithLeftMost True newCond newUnionIfTrue newUnionIfFalse
      | otherwise = ifWithLeftMost True (symNot cond' .&& condf) ft $ ifWithStrategyInv strategy cond' ifTrue' ff
      where
        idxtt = idxFun $ leftMost tt
        idxtf = idxFun $ leftMost tf
        idxft = idxFun $ leftMost ft
        idxff = idxFun $ leftMost ff
    ggUnionIf _ _ _ = undefined
    {-# INLINE ggUnionIf #-}
ifWithStrategyInv NoStrategy cond ifTrue ifFalse = ifWithLeftMost True cond ifTrue ifFalse
ifWithStrategyInv _ _ _ _ = error "Invariant violated"
{-# INLINE ifWithStrategyInv #-}
