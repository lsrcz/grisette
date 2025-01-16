{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Internal.Decl.Core.Data.Class.Mergeable
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Decl.Core.Data.Class.Mergeable
  ( -- * Merging strategy
    MergingStrategy (..),

    -- * Mergeable
    Mergeable (..),
    Mergeable1 (..),
    rootStrategy1,
    Mergeable2 (..),
    rootStrategy2,
    Mergeable3 (..),
    rootStrategy3,

    -- * Generic 'Mergeable'
    MergeableArgs (..),
    GMergeable (..),
    genericRootStrategy,
    genericLiftRootStrategy,

    -- * Combinators for manually building merging strategies
    wrapStrategy,
    product2Strategy,
    DynamicSortedIdx (..),
    StrategyList (..),
    buildStrategyList,
    resolveStrategy,
    resolveStrategy',
    resolveMergeable1,
  )
where

import Data.Functor.Classes
  ( Eq1,
    Ord1,
    Show1,
    compare1,
    eq1,
    showsPrec1,
  )
import Data.Kind (Type)
import Data.Typeable
  ( Typeable,
    eqT,
    type (:~:) (Refl),
  )
import Generics.Deriving
  ( Default,
    Default1,
    Generic (Rep, from, to),
    Generic1 (Rep1, from1, to1),
    K1 (K1, unK1),
    M1 (M1, unM1),
    Par1 (Par1, unPar1),
    Rec1 (Rec1, unRec1),
    U1,
    V1,
    (:.:) (Comp1, unComp1),
    type (:*:) ((:*:)),
    type (:+:) (L1, R1),
  )
import Grisette.Internal.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Internal.SymPrim.SymBool (SymBool)
import Grisette.Internal.Utils.Derive (Arity0, Arity1)
import Unsafe.Coerce (unsafeCoerce)

-- | Merging strategies.
--
-- __You probably do not need to know the details of this type if you are only__
-- __going to use algebraic data types. You can get merging strategies for__
-- __them with type derivation.__
--
-- In Grisette, a merged union (if-then-else tree) follows the
-- __/hierarchical sorted representation invariant/__ with regards to some
-- merging strategy.
--
-- A merging strategy encodes how to merge a __/subset/__ of the values of a
-- given type. We have three types of merging strategies:
--
-- * Simple strategy
-- * Sorted strategy
-- * No strategy
--
-- The 'SimpleStrategy' merges values with a simple merge function.
-- For example,
--
--    * the symbolic boolean values can be directly merged with 'symIte'.
--
--    * the set @{1}@, which is a subset of the values of the type @Integer@,
--        can be simply merged as the set contains only a single value.
--
--    * all the 'Just' values of the type @Maybe SymBool@ can be simply merged
--        by merging the wrapped symbolic boolean with 'symIte'.
--
-- The 'SortedStrategy' merges values by first grouping the values with an
-- indexing function, and the values with the same index will be organized as
-- a sub-tree in the if-then-else structure of
-- 'Grisette.Core.Data.UnionBase.UnionBase'. Each group (sub-tree) will be
-- further merged with a sub-strategy for the index.
-- The index type should be a totally ordered type (with the 'Ord'
-- type class). Grisette will use the indexing function to partition the values
-- into sub-trees, and organize them in a sorted way. The sub-trees will further
-- be merged with the sub-strategies. For example,
--
--    * all the integers can be merged with 'SortedStrategy' by indexing with
--      the identity function and use the 'SimpleStrategy' shown before as the
--      sub-strategies.
--
--    * all the @Maybe SymBool@ values can be merged with 'SortedStrategy' by
--      indexing with 'Data.Maybe.isJust', the 'Nothing' and 'Just' values can
--      then be merged with different simple strategies as sub-strategies.
--
-- The 'NoStrategy' does not perform any merging.
-- For example, we cannot merge values with function types that returns concrete
-- lists.
--
-- For ADTs, we can automatically derive the 'Mergeable' type class, which
-- provides a merging strategy.
--
-- If the derived version does not work for you, you should determine
-- if your type can be directly merged with a merging function. If so, you can
-- implement the merging strategy as a 'SimpleStrategy'.
-- If the type cannot be directly merged with a merging function, but could be
-- partitioned into subsets of values that can be simply merged with a function,
-- you should implement the merging strategy as a 'SortedStrategy'.
-- For easier building of the merging strategies, check out the combinators
-- like `wrapStrategy`.
--
-- For more details, please see the documents of the constructors, or refer to
-- [Grisette's paper](https://lsrcz.github.io/files/POPL23.pdf).
data MergingStrategy a where
  -- | Simple mergeable strategy.
  --
  -- For symbolic booleans, we can implement its merge strategy as follows:
  --
  -- > SimpleStrategy symIte :: MergingStrategy SymBool
  SimpleStrategy ::
    -- | Merge function.
    (SymBool -> a -> a -> a) ->
    MergingStrategy a
  -- | Sorted mergeable strategy.
  --
  -- For Integers, we can implement its merge strategy as follows:
  --
  -- > SortedStrategy id (\_ -> SimpleStrategy $ \_ t _ -> t)
  --
  -- For @Maybe SymBool@, we can implement its merge strategy as follows:
  --
  -- > SortedStrategy
  -- >   (\case; Nothing -> False; Just _ -> True)
  -- >   (\idx ->
  -- >      if idx
  -- >        then SimpleStrategy $ \_ t _ -> t
  -- >        else SimpleStrategy $ \cond (Just l) (Just r) -> Just $ symIte cond l r)
  SortedStrategy ::
    (Ord idx, Typeable idx, Show idx) =>
    -- | Indexing function
    (a -> idx) ->
    -- | Sub-strategy function
    (idx -> MergingStrategy a) ->
    MergingStrategy a
  -- | For preventing the merging intentionally. This could be
  -- useful for keeping some value concrete and may help generate more efficient
  -- formulas.
  --
  -- See [Grisette's paper](https://lsrcz.github.io/files/POPL23.pdf) for
  -- details.
  NoStrategy :: MergingStrategy a

-- | Each type is associated with a root merge strategy given by 'rootStrategy'.
-- The root merge strategy should be able to merge every value of the type.
-- Grisette will use the root merge strategy to merge the values of the type in
-- a union.
--
-- __Note 1:__ This type class can be derived for algebraic data types.
-- You may need the @DerivingVia@ and @DerivingStrategies@ extensions.
--
-- > data X = ... deriving Generic deriving Mergeable via (Default X)
class Mergeable a where
  -- | The root merging strategy for the type.
  rootStrategy :: MergingStrategy a

  sortIndices :: a -> [DynamicSortedIdx]
  sortIndices = fst . resolveStrategy rootStrategy

-- | Lifting of the 'Mergeable' class to unary type constructors.
class
  (forall a. (Mergeable a) => Mergeable (u a)) =>
  Mergeable1 (u :: Type -> Type)
  where
  -- | Lift merge strategy through the type constructor.
  liftRootStrategy :: MergingStrategy a -> MergingStrategy (u a)

-- | Lift the root merge strategy through the unary type constructor.
rootStrategy1 :: (Mergeable a, Mergeable1 u) => MergingStrategy (u a)
rootStrategy1 = liftRootStrategy rootStrategy
{-# INLINE rootStrategy1 #-}

-- | Workaround as GHC prior to 9.6 doesn't support quantified constraints
-- reliably.
--
-- Similar to https://github.com/haskell/core-libraries-committee/issues/10,
-- which is only available with 9.6 or higher.
resolveMergeable1 ::
  forall f a r. (Mergeable1 f, Mergeable a) => ((Mergeable (f a)) => r) -> r
resolveMergeable1 v = v

-- | Lifting of the 'Mergeable' class to binary type constructors.
class
  (forall a. (Mergeable a) => Mergeable1 (u a)) =>
  Mergeable2 (u :: Type -> Type -> Type)
  where
  -- | Lift merge strategy through the type constructor.
  liftRootStrategy2 ::
    MergingStrategy a ->
    MergingStrategy b ->
    MergingStrategy (u a b)

-- | Lift the root merge strategy through the binary type constructor.
rootStrategy2 ::
  (Mergeable a, Mergeable b, Mergeable2 u) =>
  MergingStrategy (u a b)
rootStrategy2 = liftRootStrategy2 rootStrategy rootStrategy
{-# INLINE rootStrategy2 #-}

-- | Lifting of the 'Mergeable' class to ternary type constructors.
class
  (forall a. (Mergeable a) => Mergeable2 (u a)) =>
  Mergeable3 (u :: Type -> Type -> Type -> Type)
  where
  -- | Lift merge strategy through the type constructor.
  liftRootStrategy3 ::
    MergingStrategy a ->
    MergingStrategy b ->
    MergingStrategy c ->
    MergingStrategy (u a b c)

-- | Lift the root merge strategy through the binary type constructor.
rootStrategy3 ::
  (Mergeable a, Mergeable b, Mergeable c, Mergeable3 u) =>
  MergingStrategy (u a b c)
rootStrategy3 = liftRootStrategy3 rootStrategy rootStrategy rootStrategy
{-# INLINE rootStrategy3 #-}

-- | Useful utility function for building merge strategies manually.
--
-- For example, to build the merge strategy for the just branch of @Maybe a@,
-- one could write
--
-- > wrapStrategy Just fromMaybe rootStrategy :: MergingStrategy (Maybe a)
wrapStrategy ::
  -- | The merge strategy to be wrapped
  MergingStrategy a ->
  -- | The wrap function
  (a -> b) ->
  -- | The unwrap function, which does not have to be defined for every value
  (b -> a) ->
  MergingStrategy b
wrapStrategy (SimpleStrategy m) wrap unwrap =
  SimpleStrategy
    ( \cond ifTrue ifFalse ->
        wrap $ m cond (unwrap ifTrue) (unwrap ifFalse)
    )
wrapStrategy (SortedStrategy idxFun substrategy) wrap unwrap =
  SortedStrategy
    (idxFun . unwrap)
    (\idx -> wrapStrategy (substrategy idx) wrap unwrap)
wrapStrategy NoStrategy _ _ = NoStrategy
{-# INLINE wrapStrategy #-}

-- | Useful utility function for building merge strategies for product types
-- manually.
--
-- For example, to build the merge strategy for the following product type,
-- one could write
--
-- > data X = X { x1 :: Int, x2 :: Bool }
-- > product2Strategy X (\(X a b) -> (a, b)) rootStrategy rootStrategy
-- >   :: MergingStrategy X
product2Strategy ::
  -- | The wrap function
  (a -> b -> r) ->
  -- | The unwrap function, which does not have to be defined for every value
  (r -> (a, b)) ->
  -- | The first merge strategy to be wrapped
  MergingStrategy a ->
  -- | The second merge strategy to be wrapped
  MergingStrategy b ->
  MergingStrategy r
product2Strategy wrap unwrap strategy1 strategy2 =
  case (strategy1, strategy2) of
    (NoStrategy, _) -> NoStrategy
    (_, NoStrategy) -> NoStrategy
    (SimpleStrategy m1, SimpleStrategy m2) ->
      SimpleStrategy $ \cond t f -> case (unwrap t, unwrap f) of
        ((hdt, tlt), (hdf, tlf)) ->
          wrap (m1 cond hdt hdf) (m2 cond tlt tlf)
    (s1@(SimpleStrategy _), SortedStrategy idxf subf) ->
      SortedStrategy
        (idxf . snd . unwrap)
        (product2Strategy wrap unwrap s1 . subf)
    (SortedStrategy idxf subf, s2) ->
      SortedStrategy
        (idxf . fst . unwrap)
        (\idx -> product2Strategy wrap unwrap (subf idx) s2)
{-# INLINE product2Strategy #-}

-- Derivations

-- | The arguments to the generic merging strategy function.
data family MergeableArgs arity a :: Type

data instance MergeableArgs Arity0 _ = MergeableArgs0

newtype instance MergeableArgs Arity1 a = MergeableArgs1 (MergingStrategy a)

-- | The class of types that can be generically merged.
class GMergeable arity f where
  grootStrategy :: MergeableArgs arity a -> MergingStrategy (f a)

instance GMergeable arity V1 where
  grootStrategy _ = SimpleStrategy (\_ t _ -> t)
  {-# INLINE grootStrategy #-}

instance GMergeable arity U1 where
  grootStrategy _ = SimpleStrategy (\_ t _ -> t)
  {-# INLINE grootStrategy #-}

instance
  (GMergeable arity a, GMergeable arity b) =>
  GMergeable arity (a :*: b)
  where
  grootStrategy args =
    product2Strategy
      (:*:)
      (\(a :*: b) -> (a, b))
      (grootStrategy args)
      (grootStrategy args)
  {-# INLINE grootStrategy #-}

instance
  (GMergeable arity a, GMergeable arity b) =>
  GMergeable arity (a :+: b)
  where
  grootStrategy args =
    SortedStrategy
      ( \case
          L1 _ -> False
          R1 _ -> True
      )
      ( \idx ->
          if not idx
            then
              wrapStrategy
                (grootStrategy args)
                L1
                (\case (L1 v) -> v; _ -> error "Should not happen")
            else
              wrapStrategy
                (grootStrategy args)
                R1
                (\case (R1 v) -> v; _ -> error "Should not happen")
      )
  {-# INLINE grootStrategy #-}

instance (GMergeable arity a) => GMergeable arity (M1 i c a) where
  grootStrategy arg = wrapStrategy (grootStrategy arg) M1 unM1
  {-# INLINE grootStrategy #-}

instance (Mergeable c) => GMergeable arity (K1 i c) where
  grootStrategy _ = wrapStrategy rootStrategy K1 unK1
  {-# INLINE grootStrategy #-}

instance GMergeable Arity1 Par1 where
  grootStrategy (MergeableArgs1 strategy) = wrapStrategy strategy Par1 unPar1
  {-# INLINE grootStrategy #-}

instance (Mergeable1 f) => GMergeable Arity1 (Rec1 f) where
  grootStrategy (MergeableArgs1 m) =
    wrapStrategy (liftRootStrategy m) Rec1 unRec1
  {-# INLINE grootStrategy #-}

instance
  (Mergeable1 f, GMergeable Arity1 g) =>
  GMergeable Arity1 (f :.: g)
  where
  grootStrategy targs =
    wrapStrategy (liftRootStrategy (grootStrategy targs)) Comp1 unComp1
  {-# INLINE grootStrategy #-}

instance (Generic a, GMergeable Arity0 (Rep a)) => Mergeable (Default a) where
  rootStrategy = unsafeCoerce (genericRootStrategy :: MergingStrategy a)
  {-# INLINE rootStrategy #-}

-- | Generic 'rootStrategy'.
genericRootStrategy ::
  (Generic a, GMergeable Arity0 (Rep a)) => MergingStrategy a
genericRootStrategy = wrapStrategy (grootStrategy MergeableArgs0) to from
{-# INLINE genericRootStrategy #-}

instance
  (Generic1 f, GMergeable Arity1 (Rep1 f), Mergeable a) =>
  Mergeable (Default1 f a)
  where
  rootStrategy = rootStrategy1
  {-# INLINE rootStrategy #-}

instance (Generic1 f, GMergeable Arity1 (Rep1 f)) => Mergeable1 (Default1 f) where
  liftRootStrategy (m :: MergingStrategy a) =
    unsafeCoerce (genericLiftRootStrategy m :: MergingStrategy (f a))
  {-# INLINE liftRootStrategy #-}

-- | Generic 'liftRootStrategy'.
genericLiftRootStrategy ::
  (Generic1 f, GMergeable Arity1 (Rep1 f)) =>
  MergingStrategy a ->
  MergingStrategy (f a)
genericLiftRootStrategy m =
  wrapStrategy (grootStrategy $ MergeableArgs1 m) to1 from1
{-# INLINE genericLiftRootStrategy #-}

-- | Helper type for combining arbitrary number of indices into one.
-- Useful when trying to write efficient merge strategy for lists/vectors.
data DynamicSortedIdx where
  DynamicSortedIdx :: forall idx. (Show idx, Ord idx, Typeable idx) => idx -> DynamicSortedIdx

instance Eq DynamicSortedIdx where
  (DynamicSortedIdx (a :: a)) == (DynamicSortedIdx (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    _ -> False
  {-# INLINE (==) #-}

instance Ord DynamicSortedIdx where
  compare (DynamicSortedIdx (a :: a)) (DynamicSortedIdx (b :: b)) = case eqT @a @b of
    Just Refl -> compare a b
    _ -> error "This Ord is incomplete"
  {-# INLINE compare #-}

instance Show DynamicSortedIdx where
  show (DynamicSortedIdx a) = show a

-- | Resolves the indices and the terminal merge strategy for a value of some
-- 'Mergeable' type.
resolveStrategy ::
  forall x.
  MergingStrategy x ->
  x ->
  ([DynamicSortedIdx], MergingStrategy x)
resolveStrategy s x = resolveStrategy' x s
{-# INLINE resolveStrategy #-}

-- | Resolves the indices and the terminal merge strategy for a value given a
-- merge strategy for its type.
resolveStrategy' ::
  forall x. x -> MergingStrategy x -> ([DynamicSortedIdx], MergingStrategy x)
resolveStrategy' x = go
  where
    go :: MergingStrategy x -> ([DynamicSortedIdx], MergingStrategy x)
    go (SortedStrategy idxFun subStrategy) = case go ss of
      (idxs, r) -> (DynamicSortedIdx idx : idxs, r)
      where
        idx = idxFun x
        ss = subStrategy idx
    go s = ([], s)
{-# INLINE resolveStrategy' #-}

-- | Helper type for building efficient merge strategy for list-like containers.
data StrategyList container where
  StrategyList ::
    forall a container.
    container [DynamicSortedIdx] ->
    container (MergingStrategy a) ->
    StrategyList container

-- | Helper function for building efficient merge strategy for list-like
-- containers.
buildStrategyList ::
  forall a container.
  (Functor container) =>
  MergingStrategy a ->
  container a ->
  StrategyList container
buildStrategyList s l = StrategyList idxs strategies
  where
    r = resolveStrategy s <$> l
    idxs = fst <$> r
    strategies = snd <$> r
{-# INLINE buildStrategyList #-}

instance (Eq1 container) => Eq (StrategyList container) where
  (StrategyList idxs1 _) == (StrategyList idxs2 _) = eq1 idxs1 idxs2
  {-# INLINE (==) #-}

instance (Ord1 container) => Ord (StrategyList container) where
  compare (StrategyList idxs1 _) (StrategyList idxs2 _) = compare1 idxs1 idxs2
  {-# INLINE compare #-}

instance (Show1 container) => Show (StrategyList container) where
  showsPrec i (StrategyList idxs1 _) = showsPrec1 i idxs1
  {-# INLINE showsPrec #-}

instance Mergeable SymBool where
  rootStrategy = SimpleStrategy symIte

instance Mergeable Ordering where
  rootStrategy =
    let sub = SimpleStrategy $ \_ t _ -> t
     in SortedStrategy id $ const sub
