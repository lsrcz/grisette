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
-- Module      :   Grisette.Internal.Core.Data.UnionBase
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.UnionBase
  ( -- * The union data structure.

    -- | Please consider using 'Grisette.Core.Union' instead.
    UnionBase (..),
    ifWithLeftMost,
    ifWithStrategy,
    fullReconstruct,
  )
where

#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter (align, group, nest, vsep)
#else
import Data.Text.Prettyprint.Doc (align, group, nest, vsep)
#endif

import Control.DeepSeq (NFData (rnf), NFData1 (liftRnf), rnf1)
import Control.Monad (ap)
import qualified Data.Binary as Binary
import Data.Bytes.Get (MonadGet (getWord8))
import Data.Bytes.Put (MonadPut (putWord8))
import Data.Bytes.Serial (Serial (deserialize, serialize))
import Data.Functor.Classes
  ( Eq1 (liftEq),
    Show1 (liftShowsPrec),
    showsPrec1,
    showsUnaryWith,
  )
import Data.Hashable (Hashable (hashWithSalt))
import qualified Data.Serialize as Cereal
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
import Grisette.Internal.Core.Data.Class.PPrint
  ( PPrint (pformatPrec),
    PPrint1 (liftPFormatPrec),
    condEnclose,
    pformatPrec1,
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
import Grisette.Internal.SymPrim.AllSyms
  ( AllSyms (allSymsS),
    AllSyms1 (liftAllSymsS),
    SomeSym (SomeSym),
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

instance Eq1 UnionBase where
  liftEq e (UnionSingle a) (UnionSingle b) = e a b
  liftEq e (UnionIf l1 i1 c1 t1 f1) (UnionIf l2 i2 c2 t2 f2) =
    e l1 l2 && i1 == i2 && c1 == c2 && liftEq e t1 t2 && liftEq e f1 f2
  liftEq _ _ _ = False

instance (NFData a) => NFData (UnionBase a) where
  rnf = rnf1

instance NFData1 UnionBase where
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
ifWithLeftMost :: Bool -> SymBool -> UnionBase a -> UnionBase a -> UnionBase a
ifWithLeftMost _ (Con c) t f
  | c = t
  | otherwise = f
ifWithLeftMost inv cond t f = UnionIf (leftMost t) inv cond t f
{-# INLINE ifWithLeftMost #-}

instance PlainUnion UnionBase where
  singleView (UnionSingle a) = Just a
  singleView _ = Nothing
  {-# INLINE singleView #-}
  ifView (UnionIf _ _ cond ifTrue ifFalse) = Just (cond, ifTrue, ifFalse)
  ifView _ = Nothing
  {-# INLINE ifView #-}

leftMost :: UnionBase a -> a
leftMost (UnionSingle a) = a
leftMost (UnionIf a _ _ _ _) = a
{-# INLINE leftMost #-}

instance (Mergeable a) => Mergeable (UnionBase a) where
  rootStrategy = SimpleStrategy $ ifWithStrategy rootStrategy
  {-# INLINE rootStrategy #-}

instance (Mergeable a, Serial a) => Serial (UnionBase a) where
  serialize (UnionSingle a) = putWord8 0 >> serialize a
  serialize (UnionIf _ _ c a b) =
    putWord8 1 >> serialize c >> serialize a >> serialize b
  deserialize = do
    tag <- getWord8
    case tag of
      0 -> UnionSingle <$> deserialize
      1 ->
        ifWithStrategy rootStrategy
          <$> deserialize
          <*> deserialize
          <*> deserialize
      _ -> fail "Invalid tag"

instance (Mergeable a, Serial a) => Cereal.Serialize (UnionBase a) where
  put = serialize
  get = deserialize

instance (Mergeable a, Serial a) => Binary.Binary (UnionBase a) where
  put = serialize
  get = deserialize

instance Mergeable1 UnionBase where
  liftRootStrategy ms = SimpleStrategy $ ifWithStrategy ms
  {-# INLINE liftRootStrategy #-}

instance (Mergeable a) => SimpleMergeable (UnionBase a) where
  mrgIte = mrgIf

instance SimpleMergeable1 UnionBase where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)

instance TryMerge UnionBase where
  tryMergeWithStrategy = fullReconstruct
  {-# INLINE tryMergeWithStrategy #-}

instance SymBranching UnionBase where
  mrgIfWithStrategy = ifWithStrategy
  {-# INLINE mrgIfWithStrategy #-}

  mrgIfPropagatedStrategy = ifWithLeftMost False
  {-# INLINE mrgIfPropagatedStrategy #-}

instance Show1 UnionBase where
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

instance (Show a) => Show (UnionBase a) where
  showsPrec = showsPrec1

instance (PPrint a) => PPrint (UnionBase a) where
  pformatPrec = pformatPrec1

instance PPrint1 UnionBase where
  liftPFormatPrec fa _ n (UnionSingle a) = fa n a
  liftPFormatPrec fa fl n (UnionIf _ _ cond t f) =
    group $
      condEnclose (n > 10) "(" ")" $
        align $
          nest 2 $
            vsep
              [ "If",
                pformatPrec 11 cond,
                liftPFormatPrec fa fl 11 t,
                liftPFormatPrec fa fl 11 f
              ]

instance (Hashable a) => Hashable (UnionBase a) where
  s `hashWithSalt` (UnionSingle a) =
    s `hashWithSalt` (0 :: Int) `hashWithSalt` a
  s `hashWithSalt` (UnionIf _ _ c l r) =
    s
      `hashWithSalt` (1 :: Int)
      `hashWithSalt` c
      `hashWithSalt` l
      `hashWithSalt` r

instance (AllSyms a) => AllSyms (UnionBase a) where
  allSymsS (UnionSingle v) = allSymsS v
  allSymsS (UnionIf _ _ c t f) = \l -> SomeSym c : (allSymsS t . allSymsS f $ l)

instance AllSyms1 UnionBase where
  liftAllSymsS fa (UnionSingle v) = fa v
  liftAllSymsS fa (UnionIf _ _ c t f) =
    \l -> SomeSym c : (liftAllSymsS fa t . liftAllSymsS fa f $ l)

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
