{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

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

type MergingStrategy a = GMergingStrategy SymBool a

type Mergeable a = GMergeable SymBool a

type Mergeable' a = GMergeable' SymBool a

rootStrategy :: Mergeable a => MergingStrategy a
rootStrategy = grootStrategy
{-# INLINE rootStrategy #-}

type Mergeable1 f = GMergeable1 SymBool f

liftRootStrategy :: (Mergeable1 f) => MergingStrategy b -> MergingStrategy (f b)
liftRootStrategy = liftGRootStrategy
{-# INLINE liftRootStrategy #-}

rootStrategy1 :: (Mergeable1 f, Mergeable b) => MergingStrategy (f b)
rootStrategy1 = grootStrategy1
{-# INLINE rootStrategy1 #-}

type Mergeable2 f = GMergeable2 SymBool f

liftRootStrategy2 ::
  (Mergeable2 f) =>
  MergingStrategy a ->
  MergingStrategy b ->
  MergingStrategy (f a b)
liftRootStrategy2 = liftGRootStrategy2
{-# INLINE liftRootStrategy2 #-}

rootStrategy2 :: (Mergeable2 f, Mergeable a, Mergeable b) => MergingStrategy (f a b)
rootStrategy2 = grootStrategy2
{-# INLINE rootStrategy2 #-}

type Mergeable3 f = GMergeable3 SymBool f

liftRootStrategy3 ::
  (Mergeable3 f) =>
  MergingStrategy a ->
  MergingStrategy b ->
  MergingStrategy c ->
  MergingStrategy (f a b c)
liftRootStrategy3 = liftGRootStrategy3
{-# INLINE liftRootStrategy3 #-}

rootStrategy3 ::
  (Mergeable3 f, Mergeable a, Mergeable b, Mergeable c) =>
  MergingStrategy (f a b c)
rootStrategy3 = grootStrategy3
{-# INLINE rootStrategy3 #-}

derivedRootStrategy :: (Generic a, Mergeable' (Rep a)) => MergingStrategy a
derivedRootStrategy = derivedGRootStrategy
{-# INLINE derivedRootStrategy #-}

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

product2Strategy ::
  (a -> b -> r) ->
  (r -> (a, b)) ->
  MergingStrategy a ->
  MergingStrategy b ->
  MergingStrategy r
product2Strategy = gproduct2Strategy
{-# INLINE product2Strategy #-}

buildStrategyList ::
  (Functor container) =>
  MergingStrategy a ->
  container a ->
  StrategyList container
buildStrategyList = gbuildStrategyList
{-# INLINE buildStrategyList #-}

resolveStrategy :: MergingStrategy x -> x -> ([DynamicSortedIdx], MergingStrategy x)
resolveStrategy = gresolveStrategy
{-# INLINE resolveStrategy #-}

resolveStrategy' :: x -> MergingStrategy x -> ([DynamicSortedIdx], MergingStrategy x)
resolveStrategy' = gresolveStrategy'
{-# INLINE resolveStrategy' #-}
