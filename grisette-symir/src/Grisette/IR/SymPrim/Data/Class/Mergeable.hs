{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

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

type MergingStrategy a = GMergingStrategy SymBool a

type Mergeable a = GMergeable SymBool a

type Mergeable' a = GMergeable' SymBool a

mergingStrategy :: Mergeable a => MergingStrategy a
mergingStrategy = gmergingStrategy
{-# INLINE mergingStrategy #-}

type Mergeable1 f = GMergeable1 SymBool f

liftMergingStrategy :: (Mergeable1 f) => MergingStrategy b -> MergingStrategy (f b)
liftMergingStrategy = liftGMergingStrategy
{-# INLINE liftMergingStrategy #-}

mergingStrategy1 :: (Mergeable1 f, Mergeable b) => MergingStrategy (f b)
mergingStrategy1 = gmergingStrategy1
{-# INLINE mergingStrategy1 #-}

type Mergeable2 f = GMergeable2 SymBool f

liftMergingStrategy2 ::
  (Mergeable2 f) =>
  MergingStrategy a ->
  MergingStrategy b ->
  MergingStrategy (f a b)
liftMergingStrategy2 = liftGMergingStrategy2
{-# INLINE liftMergingStrategy2 #-}

mergingStrategy2 :: (Mergeable2 f, Mergeable a, Mergeable b) => MergingStrategy (f a b)
mergingStrategy2 = gmergingStrategy2
{-# INLINE mergingStrategy2 #-}

type Mergeable3 f = GMergeable3 SymBool f

liftMergingStrategy3 ::
  (Mergeable3 f) =>
  MergingStrategy a ->
  MergingStrategy b ->
  MergingStrategy c ->
  MergingStrategy (f a b c)
liftMergingStrategy3 = liftGMergingStrategy3
{-# INLINE liftMergingStrategy3 #-}

mergingStrategy3 ::
  (Mergeable3 f, Mergeable a, Mergeable b, Mergeable c) =>
  MergingStrategy (f a b c)
mergingStrategy3 = gmergingStrategy3
{-# INLINE mergingStrategy3 #-}

derivedMergingStrategy :: (Generic a, Mergeable' (Rep a)) => MergingStrategy a
derivedMergingStrategy = derivedGMergingStrategy
{-# INLINE derivedMergingStrategy #-}

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
