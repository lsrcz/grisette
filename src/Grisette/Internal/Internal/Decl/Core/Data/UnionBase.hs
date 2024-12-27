{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :   Grisette.Internal.Internal.Decl.Core.Data.UnionBase
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Decl.Core.Data.UnionBase
  ( -- * The union data structure.

    -- | Please consider using 'Grisette.Core.Union' instead.
    UnionBase (..),
    ifWithLeftMost,
    ifWithStrategy,
    fullReconstruct,
  )
where

import Control.Monad (ap)
import GHC.Generics (Generic, Generic1)
import Grisette.Internal.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Internal.Core.Data.Class.LogicalOp
  ( LogicalOp (symNot, (.&&), (.||)),
  )
import Grisette.Internal.Core.Data.Class.Mergeable
  ( Mergeable (rootStrategy),
    Mergeable1 (liftRootStrategy),
    MergingStrategy (NoStrategy, SimpleStrategy, SortedStrategy),
  )
import Grisette.Internal.Core.Data.Class.PlainUnion
  ( PlainUnion (ifView, singleView),
  )
import Grisette.Internal.Core.Data.Class.SimpleMergeable
  ( SimpleMergeable (mrgIte),
    SimpleMergeable1 (liftMrgIte),
    SymBranching (mrgIfPropagatedStrategy, mrgIfWithStrategy),
    mrgIf,
  )
import Grisette.Internal.Core.Data.Class.Solvable (pattern Con)
import Grisette.Internal.Core.Data.Class.TryMerge
  ( TryMerge (tryMergeWithStrategy),
  )
import Grisette.Internal.SymPrim.SymBool (SymBool)
import Language.Haskell.TH.Syntax (Lift)

-- | The base union implementation, which is an if-then-else tree structure.
data UnionBase a where
  -- | A single value
  UnionSingle :: a -> UnionBase a
  -- | A if value
  UnionIf ::
    -- | Cached leftmost value
    a ->
    -- | Is merged invariant already maintained?
    !Bool ->
    -- | If condition
    !SymBool ->
    -- | True branch
    UnionBase a ->
    -- | False branch
    UnionBase a ->
    UnionBase a
  deriving (Generic, Eq, Lift, Generic1)
  deriving (Functor)

instance Applicative UnionBase where
  pure = UnionSingle
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad UnionBase where
  return = pure
  {-# INLINE return #-}
  UnionSingle a >>= f = f a
  UnionIf _ _ c t f >>= f' = ifWithLeftMost False c (t >>= f') (f >>= f')
  {-# INLINE (>>=) #-}

instance TryMerge UnionBase where
  tryMergeWithStrategy = fullReconstruct
  {-# INLINE tryMergeWithStrategy #-}

-- | Fully reconstruct a 'Grisette.Core.Union' to maintain the merged invariant.
fullReconstruct :: MergingStrategy a -> UnionBase a -> UnionBase a
fullReconstruct strategy (UnionIf _ False cond t f) =
  ifWithStrategyInv
    strategy
    cond
    (fullReconstruct strategy t)
    (fullReconstruct strategy f)
fullReconstruct _ u = u
{-# INLINE fullReconstruct #-}

leftMost :: UnionBase a -> a
leftMost (UnionSingle a) = a
leftMost (UnionIf a _ _ _ _) = a
{-# INLINE leftMost #-}

-- | Build 'UnionIf' with leftmost cache correctly maintained.
--
-- Usually you should never directly try to build a 'UnionIf' with its
-- constructor.
ifWithLeftMost :: Bool -> SymBool -> UnionBase a -> UnionBase a -> UnionBase a
ifWithLeftMost _ (Con c) t f
  | c = t
  | otherwise = f
ifWithLeftMost inv cond t f = UnionIf (leftMost t) inv cond t f
{-# INLINE ifWithLeftMost #-}

-- | Use a specific strategy to build a 'UnionIf' value.
--
-- The merged invariant will be maintained in the result.
ifWithStrategy ::
  MergingStrategy a ->
  SymBool ->
  UnionBase a ->
  UnionBase a ->
  UnionBase a
ifWithStrategy strategy cond t@(UnionIf _ False _ _ _) f =
  ifWithStrategy strategy cond (fullReconstruct strategy t) f
ifWithStrategy strategy cond t f@(UnionIf _ False _ _ _) =
  ifWithStrategy strategy cond t (fullReconstruct strategy f)
ifWithStrategy strategy cond t f = ifWithStrategyInv strategy cond t f
{-# INLINE ifWithStrategy #-}

ifWithStrategyInv ::
  MergingStrategy a ->
  SymBool ->
  UnionBase a ->
  UnionBase a ->
  UnionBase a
ifWithStrategyInv _ (Con v) t f
  | v = t
  | otherwise = f
ifWithStrategyInv strategy cond (UnionIf _ True condTrue tt _) f
  | cond == condTrue = ifWithStrategyInv strategy cond tt f
-- {| symNot cond == condTrue || cond == symNot condTrue = ifWithStrategyInv strategy cond ft f
ifWithStrategyInv strategy cond t (UnionIf _ True condFalse _ ff)
  | cond == condFalse = ifWithStrategyInv strategy cond t ff
-- {| symNot cond == condTrue || cond == symNot condTrue = ifWithStrategyInv strategy cond t tf -- buggy here condTrue
ifWithStrategyInv (SimpleStrategy m) cond (UnionSingle l) (UnionSingle r) =
  UnionSingle $ m cond l r
ifWithStrategyInv
  strategy@(SortedStrategy idxFun substrategy)
  cond
  ifTrue
  ifFalse = case (ifTrue, ifFalse) of
    (UnionSingle _, UnionSingle _) -> ssUnionIf cond ifTrue ifFalse
    (UnionSingle _, UnionIf {}) -> sgUnionIf cond ifTrue ifFalse
    (UnionIf {}, UnionSingle _) -> gsUnionIf cond ifTrue ifFalse
    _ -> ggUnionIf cond ifTrue ifFalse
    where
      ssUnionIf cond' ifTrue' ifFalse'
        | idxt < idxf = ifWithLeftMost True cond' ifTrue' ifFalse'
        | idxt == idxf =
            ifWithStrategyInv (substrategy idxt) cond' ifTrue' ifFalse'
        | otherwise = ifWithLeftMost True (symNot cond') ifFalse' ifTrue'
        where
          idxt = idxFun $ leftMost ifTrue'
          idxf = idxFun $ leftMost ifFalse'
      {-# INLINE ssUnionIf #-}
      sgUnionIf cond' ifTrue' ifFalse'@(UnionIf _ True condf ft ff)
        | idxft == idxff = ssUnionIf cond' ifTrue' ifFalse'
        | idxt < idxft = ifWithLeftMost True cond' ifTrue' ifFalse'
        | idxt == idxft =
            ifWithLeftMost
              True
              (cond' .|| condf)
              (ifWithStrategyInv (substrategy idxt) cond' ifTrue' ft)
              ff
        | otherwise =
            ifWithLeftMost
              True
              (symNot cond' .&& condf)
              ft
              (ifWithStrategyInv strategy cond' ifTrue' ff)
        where
          idxft = idxFun $ leftMost ft
          idxff = idxFun $ leftMost ff
          idxt = idxFun $ leftMost ifTrue'
      sgUnionIf _ _ _ = undefined
      {-# INLINE sgUnionIf #-}
      gsUnionIf cond' ifTrue'@(UnionIf _ True condt tt tf) ifFalse'
        | idxtt == idxtf = ssUnionIf cond' ifTrue' ifFalse'
        | idxtt < idxf =
            ifWithLeftMost True (cond' .&& condt) tt $
              ifWithStrategyInv strategy cond' tf ifFalse'
        | idxtt == idxf =
            ifWithLeftMost
              True
              (symNot cond' .|| condt)
              (ifWithStrategyInv (substrategy idxf) cond' tt ifFalse')
              tf
        | otherwise = ifWithLeftMost True (symNot cond') ifFalse' ifTrue'
        where
          idxtt = idxFun $ leftMost tt
          idxtf = idxFun $ leftMost tf
          idxf = idxFun $ leftMost ifFalse'
      gsUnionIf _ _ _ = undefined
      {-# INLINE gsUnionIf #-}
      ggUnionIf
        cond'
        ifTrue'@(UnionIf _ True condt tt tf)
        ifFalse'@(UnionIf _ True condf ft ff)
          | idxtt == idxtf = sgUnionIf cond' ifTrue' ifFalse'
          | idxft == idxff = gsUnionIf cond' ifTrue' ifFalse'
          | idxtt < idxft =
              ifWithLeftMost True (cond' .&& condt) tt $
                ifWithStrategyInv strategy cond' tf ifFalse'
          | idxtt == idxft =
              let newCond = symIte cond' condt condf
                  newUnionIfTrue =
                    ifWithStrategyInv (substrategy idxtt) cond' tt ft
                  newUnionIfFalse = ifWithStrategyInv strategy cond' tf ff
               in ifWithLeftMost True newCond newUnionIfTrue newUnionIfFalse
          | otherwise =
              ifWithLeftMost True (symNot cond' .&& condf) ft $
                ifWithStrategyInv strategy cond' ifTrue' ff
          where
            idxtt = idxFun $ leftMost tt
            idxtf = idxFun $ leftMost tf
            idxft = idxFun $ leftMost ft
            idxff = idxFun $ leftMost ff
      ggUnionIf _ _ _ = undefined
      {-# INLINE ggUnionIf #-}
ifWithStrategyInv NoStrategy cond ifTrue ifFalse =
  ifWithLeftMost True cond ifTrue ifFalse
ifWithStrategyInv _ _ _ _ = error "Invariant violated"
{-# INLINE ifWithStrategyInv #-}

instance (Mergeable a) => Mergeable (UnionBase a) where
  rootStrategy = SimpleStrategy $ ifWithStrategy rootStrategy
  {-# INLINE rootStrategy #-}

instance Mergeable1 UnionBase where
  liftRootStrategy ms = SimpleStrategy $ ifWithStrategy ms
  {-# INLINE liftRootStrategy #-}

instance (Mergeable a) => SimpleMergeable (UnionBase a) where
  mrgIte = mrgIf

instance SimpleMergeable1 UnionBase where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)

instance SymBranching UnionBase where
  mrgIfWithStrategy = ifWithStrategy
  {-# INLINE mrgIfWithStrategy #-}

  mrgIfPropagatedStrategy = ifWithLeftMost False
  {-# INLINE mrgIfPropagatedStrategy #-}

instance PlainUnion UnionBase where
  singleView (UnionSingle a) = Just a
  singleView _ = Nothing
  {-# INLINE singleView #-}
  ifView (UnionIf _ _ cond ifTrue ifFalse) = Just (cond, ifTrue, ifFalse)
  ifView _ = Nothing
  {-# INLINE ifView #-}
