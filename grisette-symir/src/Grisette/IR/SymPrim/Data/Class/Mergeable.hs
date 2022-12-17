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
    mergingStrategy,
    Mergeable1,
    liftMergingStrategy,
    mergingStrategy1,
    Mergeable2,
    liftMergingStrategy2,
    mergingStrategy2,
    Mergeable3,
    liftMergingStrategy3,
    mergingStrategy3,
    derivedMergingStrategy,
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

-- | 'gmergingStrategy' specialized with 'SymBool'
mergingStrategy :: Mergeable a => MergingStrategy a
mergingStrategy = gmergingStrategy
{-# INLINE mergingStrategy #-}

-- | 'GMergeable1' specialized with 'SymBool'
type Mergeable1 f = GMergeable1 SymBool f

-- | 'liftGMergingStrategy' specialized with 'SymBool'
liftMergingStrategy :: (Mergeable1 f) => MergingStrategy b -> MergingStrategy (f b)
liftMergingStrategy = liftGMergingStrategy
{-# INLINE liftMergingStrategy #-}

-- | 'gmergingStrategy1' specialized with 'SymBool'
mergingStrategy1 :: (Mergeable1 f, Mergeable b) => MergingStrategy (f b)
mergingStrategy1 = gmergingStrategy1
{-# INLINE mergingStrategy1 #-}

-- | 'GMergeable2' specialized with 'SymBool'
type Mergeable2 f = GMergeable2 SymBool f

-- | 'liftGMergingStrategy2' specialized with 'SymBool'
liftMergingStrategy2 ::
  (Mergeable2 f) =>
  MergingStrategy a ->
  MergingStrategy b ->
  MergingStrategy (f a b)
liftMergingStrategy2 = liftGMergingStrategy2
{-# INLINE liftMergingStrategy2 #-}

-- | 'gmergingStrategy2' specialized with 'SymBool'
mergingStrategy2 :: (Mergeable2 f, Mergeable a, Mergeable b) => MergingStrategy (f a b)
mergingStrategy2 = gmergingStrategy2
{-# INLINE mergingStrategy2 #-}

-- | 'GMergeable3' specialized with 'SymBool'
type Mergeable3 f = GMergeable3 SymBool f

-- | 'liftGMergingStrategy3' specialized with 'SymBool'
liftMergingStrategy3 ::
  (Mergeable3 f) =>
  MergingStrategy a ->
  MergingStrategy b ->
  MergingStrategy c ->
  MergingStrategy (f a b c)
liftMergingStrategy3 = liftGMergingStrategy3
{-# INLINE liftMergingStrategy3 #-}

-- | 'gmergingStrategy3' specialized with 'SymBool'
mergingStrategy3 ::
  (Mergeable3 f, Mergeable a, Mergeable b, Mergeable c) =>
  MergingStrategy (f a b c)
mergingStrategy3 = gmergingStrategy3
{-# INLINE mergingStrategy3 #-}

-- | 'derivedGMergingStrategy' specialized with 'SymBool'
derivedMergingStrategy :: (Generic a, Mergeable' (Rep a)) => MergingStrategy a
derivedMergingStrategy = derivedGMergingStrategy
{-# INLINE derivedMergingStrategy #-}

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
