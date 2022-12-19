{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.Class.Mergeable
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.Class.Mergeable
  ( MergingStrategy,
    Mergeable,
    Mergeable',
    rootStrategy,
    Mergeable1,
    liftRootStrategy,
    rootStrategy1,
    Mergeable2,
    liftRootStrategy2,
    rootStrategy2,
    Mergeable3,
    liftRootStrategy3,
    rootStrategy3,
    derivedRootStrategy,
    wrapStrategy,
    product2Strategy,
    buildStrategyList,
    resolveStrategy,
    resolveStrategy',
  )
where

import GHC.Generics
import Grisette.Core.Data.Class.Mergeable
import Grisette.IR.SymPrim.Data.SymPrim

-- | 'GMergingStrategy' specialized with 'SymBool'
type MergingStrategy a = GMergingStrategy SymBool a

-- | 'GMergeable' specialized with 'SymBool'
type Mergeable a = GMergeable SymBool a

-- | 'GMergeable'' specialized with 'SymBool'
type Mergeable' a = GMergeable' SymBool a

-- | 'grootStrategy' specialized with 'SymBool'
rootStrategy :: Mergeable a => MergingStrategy a
rootStrategy = grootStrategy
{-# INLINE rootStrategy #-}

-- | 'GMergeable1' specialized with 'SymBool'
type Mergeable1 f = GMergeable1 SymBool f

-- | 'liftGRootStrategy' specialized with 'SymBool'
liftRootStrategy :: (Mergeable1 f) => MergingStrategy b -> MergingStrategy (f b)
liftRootStrategy = liftGRootStrategy
{-# INLINE liftRootStrategy #-}

-- | 'grootStrategy1' specialized with 'SymBool'
rootStrategy1 :: (Mergeable1 f, Mergeable b) => MergingStrategy (f b)
rootStrategy1 = grootStrategy1
{-# INLINE rootStrategy1 #-}

-- | 'GMergeable2' specialized with 'SymBool'
type Mergeable2 f = GMergeable2 SymBool f

-- | 'liftGRootStrategy2' specialized with 'SymBool'
liftRootStrategy2 ::
  (Mergeable2 f) =>
  MergingStrategy a ->
  MergingStrategy b ->
  MergingStrategy (f a b)
liftRootStrategy2 = liftGRootStrategy2
{-# INLINE liftRootStrategy2 #-}

-- | 'grootStrategy2' specialized with 'SymBool'
rootStrategy2 :: (Mergeable2 f, Mergeable a, Mergeable b) => MergingStrategy (f a b)
rootStrategy2 = grootStrategy2
{-# INLINE rootStrategy2 #-}

-- | 'GMergeable3' specialized with 'SymBool'
type Mergeable3 f = GMergeable3 SymBool f

-- | 'liftGRootStrategy3' specialized with 'SymBool'
liftRootStrategy3 ::
  (Mergeable3 f) =>
  MergingStrategy a ->
  MergingStrategy b ->
  MergingStrategy c ->
  MergingStrategy (f a b c)
liftRootStrategy3 = liftGRootStrategy3
{-# INLINE liftRootStrategy3 #-}

-- | 'grootStrategy3' specialized with 'SymBool'
rootStrategy3 ::
  (Mergeable3 f, Mergeable a, Mergeable b, Mergeable c) =>
  MergingStrategy (f a b c)
rootStrategy3 = grootStrategy3
{-# INLINE rootStrategy3 #-}

-- | 'derivedGMergingStrategy' specialized with 'SymBool'
derivedRootStrategy :: (Generic a, Mergeable' (Rep a)) => MergingStrategy a
derivedRootStrategy = derivedGRootStrategy
{-# INLINE derivedRootStrategy #-}

-- | 'gwrapStrategy' specialized with 'SymBool'
wrapStrategy ::
  -- | The merge strategy to be wrapped
  MergingStrategy a ->
  -- | The wrap function
  (a -> b) ->
  -- | The unwrap function, which does not have to be defined for every value
  (b -> a) ->
  MergingStrategy b
wrapStrategy = gwrapStrategy
{-# INLINE wrapStrategy #-}

-- | 'gproduct2Strategy' specialized with 'SymBool'
product2Strategy ::
  (a -> b -> r) ->
  (r -> (a, b)) ->
  MergingStrategy a ->
  MergingStrategy b ->
  MergingStrategy r
product2Strategy = gproduct2Strategy
{-# INLINE product2Strategy #-}

-- | 'gbuildStrategyList' specialized with 'SymBool'
buildStrategyList ::
  (Functor container) =>
  MergingStrategy a ->
  container a ->
  StrategyList container
buildStrategyList = gbuildStrategyList
{-# INLINE buildStrategyList #-}

-- | 'gresolveStrategy' specialized with 'SymBool'
resolveStrategy :: MergingStrategy x -> x -> ([DynamicSortedIdx], MergingStrategy x)
resolveStrategy = gresolveStrategy
{-# INLINE resolveStrategy #-}

-- | 'gresolveStrategy'' specialized with 'SymBool'
resolveStrategy' :: x -> MergingStrategy x -> ([DynamicSortedIdx], MergingStrategy x)
resolveStrategy' = gresolveStrategy'
{-# INLINE resolveStrategy' #-}
