{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Core.Data.Class.Mergeable
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.Class.Mergeable
  ( -- * Note for the examples

    --

    -- | This module does not contain the implementation for solvable (see "Grisette.Core#solvable")
    -- types, and the examples in this module rely on the implementations in
    -- the [grisette-symir](https://hackage.haskell.org/package/grisette-symir) package.

    -- * Merging strategy
    GMergingStrategy (..),

    -- * Mergeable
    GMergeable (..),
    GMergeable1 (..),
    gmergingStrategy1,
    GMergeable2 (..),
    gmergingStrategy2,
    GMergeable3 (..),
    gmergingStrategy3,
    GMergeable' (..),
    derivedGMergingStrategy,

    -- * Combinators for manually building merging strategies
    gwrapStrategy,
    gproduct2Strategy,
    DynamicSortedIdx (..),
    StrategyList (..),
    gbuildStrategyList,
    gresolveStrategy,
    gresolveStrategy',
  )
where

import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Identity
import qualified Control.Monad.RWS.Lazy as RWSLazy
import qualified Control.Monad.RWS.Strict as RWSStrict
import Control.Monad.Reader
import qualified Control.Monad.State.Lazy as StateLazy
import qualified Control.Monad.State.Strict as StateStrict
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.ByteString as B
import Data.Functor.Classes
import Data.Functor.Sum
import Data.Int
import Data.Kind
import qualified Data.Monoid as Monoid
import Data.Typeable
import Data.Word
import Generics.Deriving
import Grisette.Core.Data.Class.Bool
import Unsafe.Coerce

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

-- | Resolves the indices and the terminal merge strategy for a value of some 'GMergeable' type.
gresolveStrategy :: forall bool x. GMergingStrategy bool x -> x -> ([DynamicSortedIdx], GMergingStrategy bool x)
gresolveStrategy s x = gresolveStrategy' x s
{-# INLINE gresolveStrategy #-}

-- | Resolves the indices and the terminal merge strategy for a value given a merge strategy for its type.
gresolveStrategy' :: forall bool x. x -> GMergingStrategy bool x -> ([DynamicSortedIdx], GMergingStrategy bool x)
gresolveStrategy' x = go
  where
    go :: GMergingStrategy bool x -> ([DynamicSortedIdx], GMergingStrategy bool x)
    go (SortedStrategy idxFun subStrategy) = case go ss of
      (idxs, r) -> (DynamicSortedIdx idx : idxs, r)
      where
        idx = idxFun x
        ss = subStrategy idx
    go s = ([], s)
{-# INLINE gresolveStrategy' #-}

-- | Merging strategies.
--
-- __You probably do not need to know the details of this type if you are only going__
-- __to use algebraic data types. You can get merging strategies for them with type__
-- __derivation.__
--
-- In Grisette, a merged union (if-then-else tree) follows the __/hierarchical/__
-- __/sorted representation invariant/__ with regards to some merging strategy.
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
--    * the symbolic boolean values can be directly merged with 'ites'.
--
--    * the set @{1}@, which is a subset of the values of the type @Integer@,
--        can be simply merged as the set contains only a single value.
--
--    * all the 'Just' values of the type @Maybe SymBool@ can be simply merged
--        by merging the wrapped symbolic boolean with 'ites'.
--
-- The 'SortedStrategy' merges values by first grouping the values with an
-- indexing function, and the values with the same index will be organized as
-- a sub-tree in the if-then-else structure of 'Grisette.Core.Data.UnionBase.UnionBase'.
-- Each group (sub-tree) will be further merged with a sub-strategy for the
-- index.
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
--      indexing with 'Data.Maybe.isJust', the 'None' and 'Just' values can then
--      then be merged with different simple strategies as sub-strategies.
--
-- The 'NoStrategy' does not perform any merging.
-- For example, we cannot merge values with function types that returns concrete
-- lists.
--
-- For ADTs, we can automatically derive the 'GMergeable' type class, which
-- provides a merging strategy.
--
-- If the derived version does not work for you, you should determine
-- if your type can be directly merged with a merging function. If so, you can
-- implement the merging strategy as a 'SimpleStrategy'.
-- If the type cannot be directly merged with a merging function, but could be
-- partitioned into subsets of values that can be simply merged with a function,
-- you should implement the merging strategy as a 'SortedStrategy'.
-- For easier building of the merging strategies, check out the combinators
-- like `gwrapStrategy`.
--
-- For more details, please see the documents of the constructors, or refer to
-- [Grisette's paper](https://lsrcz.github.io/files/POPL23.pdf).
--
-- __Note:__ The @bool@ type is the symbolic boolean type to use. It should
-- be an instance of `SymBoolOp`. If you do not need to use an alternative
-- symbolic Boolean type, and will use the 'SymBool' type provided by the
-- `grisette-symir` package, you can use the specialized `MergingStrategy` type
-- instead. The specialized versions for the combinators for building
-- `MergingStrategy` are also provided.
data GMergingStrategy bool a where
  -- | Simple mergeable strategy.
  --
  -- For symbolic booleans, we can implement its merge strategy as follows:
  --
  -- > SimpleStrategy ites :: GMergingStrategy SymBool SymBool
  SimpleStrategy ::
    -- | Merge function.
    (bool -> a -> a -> a) ->
    GMergingStrategy bool a
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
  -- >        else SimpleStrategy $ \cond (Just l) (Just r) -> Just $ ites cond l r)
  SortedStrategy ::
    (Ord idx, Typeable idx, Show idx) =>
    -- | Indexing function
    (a -> idx) ->
    -- | Sub-strategy function
    (idx -> GMergingStrategy bool a) ->
    GMergingStrategy bool a
  -- | For preventing the merging intentionally. This could be
  -- useful for keeping some value concrete and may help generate more efficient
  -- formulas.
  --
  -- See [Grisette's paper](https://lsrcz.github.io/files/POPL23.pdf) for
  -- details.
  NoStrategy :: GMergingStrategy bool a

-- | Useful utility function for building merge strategies manually.
--
-- For example, to build the merge strategy for the just branch of @Maybe a@,
-- one could write
--
-- > gwrapStrategy Just fromMaybe gmergingStrategy :: GMergingStrategy (Maybe a)
gwrapStrategy ::
  -- | The merge strategy to be wrapped
  GMergingStrategy bool a ->
  -- | The wrap function
  (a -> b) ->
  -- | The unwrap function, which does not have to be defined for every value
  (b -> a) ->
  GMergingStrategy bool b
gwrapStrategy (SimpleStrategy m) wrap unwrap =
  SimpleStrategy
    ( \cond ifTrue ifFalse ->
        wrap $ m cond (unwrap ifTrue) (unwrap ifFalse)
    )
gwrapStrategy (SortedStrategy idxFun substrategy) wrap unwrap =
  SortedStrategy
    (idxFun . unwrap)
    (\idx -> gwrapStrategy (substrategy idx) wrap unwrap)
gwrapStrategy NoStrategy _ _ = NoStrategy
{-# INLINE gwrapStrategy #-}

-- | Each type is associated with a root merge strategy given by 'gmergingStrategy'.
-- The root merge strategy should be able to merge every value of the type.
-- Grisette will use the root merge strategy to merge the values of the type in
-- a union.
--
-- __Note 1:__ This type class can be derived for algebraic data types.
-- You may need the @DerivingVia@ and @DerivingStrategies@ extensions.
--
-- > data X = ... deriving Generic deriving (GMergeable SymBool) via (Default X)
--
-- __Note 2:__ The @bool@ type is the symbolic boolean type to use. It should
-- be an instance of `SymBoolOp`. If you do not need to use an alternative
-- symbolic Boolean type, and will use the 'SymBool' type provided by the
-- [grisette-symir](https://hackage.haskell.org/package/grisette-symir) package, you can use the specialized `Mergeable` type
-- synonym for constraints.
-- You still need @'GMergeable' SymBool@ for implementing or deriving the
-- type class due to GHC's limitation.
class GMergeable bool a where
  -- | The root merging strategy for the type.
  gmergingStrategy :: GMergingStrategy bool a

instance (Generic a, GMergeable' bool (Rep a)) => GMergeable bool (Default a) where
  gmergingStrategy = unsafeCoerce (derivedGMergingStrategy :: GMergingStrategy bool a)
  {-# NOINLINE gmergingStrategy #-}

-- | Generic derivation for the 'GMergeable' class.
--
-- Usually you can derive the merging strategy with the @DerivingVia@ and
-- @DerivingStrategies@ extension.
--
-- > data X = ... deriving (Generic) deriving (GMergeable SymBool) via (Default X)
derivedGMergingStrategy :: (Generic a, GMergeable' bool (Rep a)) => GMergingStrategy bool a
derivedGMergingStrategy = gwrapStrategy gmergingStrategy' to from
{-# INLINE derivedGMergingStrategy #-}

-- | Lifting of the 'GMergeable' class to unary type constructors.
class GMergeable1 bool (u :: Type -> Type) where
  -- | Lift merge strategy through the type constructor.
  liftGMergingStrategy :: GMergingStrategy bool a -> GMergingStrategy bool (u a)

-- | Lift the root merge strategy through the unary type constructor.
gmergingStrategy1 :: (GMergeable bool a, GMergeable1 bool u) => GMergingStrategy bool (u a)
gmergingStrategy1 = liftGMergingStrategy gmergingStrategy
{-# INLINE gmergingStrategy1 #-}

-- | Lifting of the 'GMergeable' class to binary type constructors.
class GMergeable2 bool (u :: Type -> Type -> Type) where
  -- | Lift merge strategy through the type constructor.
  liftGMergingStrategy2 :: GMergingStrategy bool a -> GMergingStrategy bool b -> GMergingStrategy bool (u a b)

-- | Lift the root merge strategy through the binary type constructor.
gmergingStrategy2 :: (GMergeable bool a, GMergeable bool b, GMergeable2 bool u) => GMergingStrategy bool (u a b)
gmergingStrategy2 = liftGMergingStrategy2 gmergingStrategy gmergingStrategy
{-# INLINE gmergingStrategy2 #-}

-- | Lifting of the 'GMergeable' class to ternary type constructors.
class GMergeable3 bool (u :: Type -> Type -> Type -> Type) where
  -- | Lift merge strategy through the type constructor.
  liftGMergingStrategy3 :: GMergingStrategy bool a -> GMergingStrategy bool b -> GMergingStrategy bool c -> GMergingStrategy bool (u a b c)

-- | Lift the root merge strategy through the ternary type constructor.
gmergingStrategy3 :: (GMergeable bool a, GMergeable bool b, GMergeable bool c, GMergeable3 bool u) => GMergingStrategy bool (u a b c)
gmergingStrategy3 = liftGMergingStrategy3 gmergingStrategy gmergingStrategy gmergingStrategy
{-# INLINE gmergingStrategy3 #-}

instance (Generic1 u, GMergeable1' bool (Rep1 u)) => GMergeable1 bool (Default1 u) where
  liftGMergingStrategy = unsafeCoerce (derivedLiftGMergingStrategy :: GMergingStrategy bool a -> GMergingStrategy bool (u a))
  {-# NOINLINE liftGMergingStrategy #-}

class GMergeable1' bool (u :: Type -> Type) where
  liftGMergingStrategy' :: GMergingStrategy bool a -> GMergingStrategy bool (u a)

instance GMergeable1' bool U1 where
  liftGMergingStrategy' _ = SimpleStrategy (\_ t _ -> t)
  {-# INLINE liftGMergingStrategy' #-}

instance GMergeable1' bool V1 where
  liftGMergingStrategy' _ = SimpleStrategy (\_ t _ -> t)
  {-# INLINE liftGMergingStrategy' #-}

instance GMergeable1' bool Par1 where
  liftGMergingStrategy' m = gwrapStrategy m Par1 unPar1
  {-# INLINE liftGMergingStrategy' #-}

instance GMergeable1 bool f => GMergeable1' bool (Rec1 f) where
  liftGMergingStrategy' m = gwrapStrategy (liftGMergingStrategy m) Rec1 unRec1
  {-# INLINE liftGMergingStrategy' #-}

instance GMergeable bool c => GMergeable1' bool (K1 i c) where
  liftGMergingStrategy' _ = gwrapStrategy gmergingStrategy K1 unK1
  {-# INLINE liftGMergingStrategy' #-}

instance GMergeable1' bool a => GMergeable1' bool (M1 i c a) where
  liftGMergingStrategy' m = gwrapStrategy (liftGMergingStrategy' m) M1 unM1
  {-# INLINE liftGMergingStrategy' #-}

instance (GMergeable1' bool a, GMergeable1' bool b) => GMergeable1' bool (a :+: b) where
  liftGMergingStrategy' m =
    SortedStrategy
      ( \case
          L1 _ -> False
          R1 _ -> True
      )
      ( \idx ->
          if not idx
            then gwrapStrategy (liftGMergingStrategy' m) L1 (\case (L1 v) -> v; _ -> error "impossible")
            else gwrapStrategy (liftGMergingStrategy' m) R1 (\case (R1 v) -> v; _ -> error "impossible")
      )
  {-# INLINE liftGMergingStrategy' #-}

instance (GMergeable1' bool a, GMergeable1' bool b) => GMergeable1' bool (a :*: b) where
  liftGMergingStrategy' m = gproduct2Strategy (:*:) (\(a :*: b) -> (a, b)) (liftGMergingStrategy' m) (liftGMergingStrategy' m)
  {-# INLINE liftGMergingStrategy' #-}

-- | Generic derivation for the 'GMergeable' class.
derivedLiftGMergingStrategy :: (Generic1 u, GMergeable1' bool (Rep1 u)) => GMergingStrategy bool a -> GMergingStrategy bool (u a)
derivedLiftGMergingStrategy m = gwrapStrategy (liftGMergingStrategy' m) to1 from1
{-# INLINE derivedLiftGMergingStrategy #-}

{-
-- | Resolves the 'GMergeable' constraint through a 'GMergeable1' type constructor.
withGMergeable :: forall bool u a b. (GMergeable1 bool u, GMergeable bool a) => (GMergeable bool (u a) => b) -> b
withGMergeable v = unCConst $ withGMergeableT @bool @u @a @(CConst (GMergeable bool (u a)) b) $ CConst v
-}

-- | Auxiliary class for the generic derivation for the 'GMergeable' class.
class GMergeable' bool f where
  gmergingStrategy' :: GMergingStrategy bool (f a)

instance GMergeable' bool U1 where
  gmergingStrategy' = SimpleStrategy (\_ t _ -> t)
  {-# INLINE gmergingStrategy' #-}

instance GMergeable' bool V1 where
  gmergingStrategy' = SimpleStrategy (\_ t _ -> t)
  {-# INLINE gmergingStrategy' #-}

instance (GMergeable bool c) => GMergeable' bool (K1 i c) where
  gmergingStrategy' = gwrapStrategy gmergingStrategy K1 unK1
  {-# INLINE gmergingStrategy' #-}

instance (GMergeable' bool a) => GMergeable' bool (M1 i c a) where
  gmergingStrategy' = gwrapStrategy gmergingStrategy' M1 unM1
  {-# INLINE gmergingStrategy' #-}

instance (GMergeable' bool a, GMergeable' bool b) => GMergeable' bool (a :+: b) where
  gmergingStrategy' =
    SortedStrategy
      ( \case
          L1 _ -> False
          R1 _ -> True
      )
      ( \idx ->
          if not idx
            then gwrapStrategy gmergingStrategy' L1 (\case (L1 v) -> v; _ -> undefined)
            else gwrapStrategy gmergingStrategy' R1 (\case (R1 v) -> v; _ -> undefined)
      )
  {-# INLINE gmergingStrategy' #-}

-- | Useful utility function for building merge strategies for product types
-- manually.
--
-- For example, to build the merge strategy for the following product type,
-- one could write
--
-- > data X = X { x1 :: Int, x2 :: Bool }
-- > gproduct2Strategy X (\(X a b) -> (a, b)) gmergingStrategy gmergingStrategy
-- >   :: GMergingStrategy X
gproduct2Strategy ::
  -- | The wrap function
  (a -> b -> r) ->
  -- | The unwrap function, which does not have to be defined for every value
  (r -> (a, b)) ->
  -- | The first merge strategy to be wrapped
  GMergingStrategy bool a ->
  -- | The second merge strategy to be wrapped
  GMergingStrategy bool b ->
  GMergingStrategy bool r
gproduct2Strategy wrap unwrap strategy1 strategy2 =
  case (strategy1, strategy2) of
    (NoStrategy, _) -> NoStrategy
    (_, NoStrategy) -> NoStrategy
    (SimpleStrategy m1, SimpleStrategy m2) ->
      SimpleStrategy $ \cond t f -> case (unwrap t, unwrap f) of
        ((hdt, tlt), (hdf, tlf)) ->
          wrap (m1 cond hdt hdf) (m2 cond tlt tlf)
    (s1@(SimpleStrategy _), SortedStrategy idxf subf) ->
      SortedStrategy (idxf . snd . unwrap) (gproduct2Strategy wrap unwrap s1 . subf)
    (SortedStrategy idxf subf, s2) ->
      SortedStrategy (idxf . fst . unwrap) (\idx -> gproduct2Strategy wrap unwrap (subf idx) s2)
{-# INLINE gproduct2Strategy #-}

instance (GMergeable' bool a, GMergeable' bool b) => GMergeable' bool (a :*: b) where
  gmergingStrategy' = gproduct2Strategy (:*:) (\(a :*: b) -> (a, b)) gmergingStrategy' gmergingStrategy'
  {-# INLINE gmergingStrategy' #-}

-- instances

#define CONCRETE_ORD_MERGEABLE(type) \
instance (SymBoolOp bool) => GMergeable bool type where \
  gmergingStrategy = \
    let sub = SimpleStrategy $ \_ t _ -> t \
     in SortedStrategy id $ const sub; \
  {-# INLINE gmergingStrategy #-}

CONCRETE_ORD_MERGEABLE (Bool)
CONCRETE_ORD_MERGEABLE (Integer)
CONCRETE_ORD_MERGEABLE (Char)
CONCRETE_ORD_MERGEABLE (Int)
CONCRETE_ORD_MERGEABLE (Int8)
CONCRETE_ORD_MERGEABLE (Int16)
CONCRETE_ORD_MERGEABLE (Int32)
CONCRETE_ORD_MERGEABLE (Int64)
CONCRETE_ORD_MERGEABLE (Word)
CONCRETE_ORD_MERGEABLE (Word8)
CONCRETE_ORD_MERGEABLE (Word16)
CONCRETE_ORD_MERGEABLE (Word32)
CONCRETE_ORD_MERGEABLE (Word64)
CONCRETE_ORD_MERGEABLE (B.ByteString)

-- ()
deriving via (Default ()) instance (SymBoolOp bool) => GMergeable bool ()

-- Either
deriving via (Default (Either e a)) instance (SymBoolOp bool, GMergeable bool e, GMergeable bool a) => GMergeable bool (Either e a)

deriving via (Default1 (Either e)) instance (SymBoolOp bool, GMergeable bool e) => GMergeable1 bool (Either e)

instance (SymBoolOp bool) => GMergeable2 bool Either where
  liftGMergingStrategy2 m1 m2 =
    SortedStrategy
      ( \case
          Left _ -> False
          Right _ -> True
      )
      ( \case
          False -> gwrapStrategy m1 Left (\case (Left v) -> v; _ -> undefined)
          True -> gwrapStrategy m2 Right (\case (Right v) -> v; _ -> undefined)
      )
  {-# INLINE liftGMergingStrategy2 #-}

-- Maybe
deriving via (Default (Maybe a)) instance (SymBoolOp bool, GMergeable bool a) => GMergeable bool (Maybe a)

deriving via (Default1 Maybe) instance (SymBoolOp bool) => GMergeable1 bool Maybe

-- | Helper type for building efficient merge strategy for list-like containers.
data StrategyList container where
  StrategyList ::
    forall bool a container.
    container [DynamicSortedIdx] ->
    container (GMergingStrategy bool a) ->
    StrategyList container

-- | Helper function for building efficient merge strategy for list-like containers.
gbuildStrategyList ::
  forall bool a container.
  (Functor container) =>
  GMergingStrategy bool a ->
  container a ->
  StrategyList container
gbuildStrategyList s l = StrategyList idxs strategies
  where
    r = gresolveStrategy @bool s <$> l
    idxs = fst <$> r
    strategies = snd <$> r
{-# INLINE gbuildStrategyList #-}

instance Eq1 container => Eq (StrategyList container) where
  (StrategyList idxs1 _) == (StrategyList idxs2 _) = eq1 idxs1 idxs2
  {-# INLINE (==) #-}

instance Ord1 container => Ord (StrategyList container) where
  compare (StrategyList idxs1 _) (StrategyList idxs2 _) = compare1 idxs1 idxs2
  {-# INLINE compare #-}

instance Show1 container => Show (StrategyList container) where
  showsPrec i (StrategyList idxs1 _) = showsPrec1 i idxs1

-- List
instance (SymBoolOp bool, GMergeable bool a) => GMergeable bool [a] where
  gmergingStrategy = case gmergingStrategy :: GMergingStrategy bool a of
    SimpleStrategy m ->
      SortedStrategy length $ \_ ->
        SimpleStrategy $ \cond -> zipWith (m cond)
    NoStrategy ->
      SortedStrategy length $ const NoStrategy
    _ -> SortedStrategy length $ \_ ->
      SortedStrategy (gbuildStrategyList @bool gmergingStrategy) $ \(StrategyList _ strategies) ->
        let s :: [GMergingStrategy bool a] = unsafeCoerce strategies
            allSimple = all (\case SimpleStrategy _ -> True; _ -> False) s
         in if allSimple
              then SimpleStrategy $ \cond l r ->
                (\case (SimpleStrategy f, l1, r1) -> f cond l1 r1; _ -> error "impossible") <$> zip3 s l r
              else NoStrategy
  {-# INLINE gmergingStrategy #-}

instance (SymBoolOp bool) => GMergeable1 bool [] where
  liftGMergingStrategy (ms :: GMergingStrategy bool a) = case ms of
    SimpleStrategy m ->
      SortedStrategy length $ \_ ->
        SimpleStrategy $ \cond -> zipWith (m cond)
    NoStrategy ->
      SortedStrategy length $ const NoStrategy
    _ -> SortedStrategy length $ \_ ->
      SortedStrategy (gbuildStrategyList @bool ms) $ \(StrategyList _ strategies) ->
        let s :: [GMergingStrategy bool a] = unsafeCoerce strategies
            allSimple = all (\case SimpleStrategy _ -> True; _ -> False) s
         in if allSimple
              then SimpleStrategy $ \cond l r ->
                (\case (SimpleStrategy f, l1, r1) -> f cond l1 r1; _ -> error "impossible") <$> zip3 s l r
              else NoStrategy
  {-# INLINE liftGMergingStrategy #-}

-- (,)
deriving via (Default (a, b)) instance (SymBoolOp bool, GMergeable bool a, GMergeable bool b) => GMergeable bool (a, b)

deriving via (Default1 ((,) a)) instance (SymBoolOp bool, GMergeable bool a) => GMergeable1 bool ((,) a)

instance SymBoolOp bool => GMergeable2 bool (,) where
  liftGMergingStrategy2 = gproduct2Strategy (,) id
  {-# INLINE liftGMergingStrategy2 #-}

-- (,,)
deriving via
  (Default (a, b, c))
  instance
    (SymBoolOp bool, GMergeable bool a, GMergeable bool b, GMergeable bool c) => GMergeable bool (a, b, c)

deriving via
  (Default1 ((,,) a b))
  instance
    (SymBoolOp bool, GMergeable bool a, GMergeable bool b) => GMergeable1 bool ((,,) a b)

instance (SymBoolOp bool, GMergeable bool a) => GMergeable2 bool ((,,) a) where
  liftGMergingStrategy2 = liftGMergingStrategy3 gmergingStrategy
  {-# INLINE liftGMergingStrategy2 #-}

instance SymBoolOp bool => GMergeable3 bool (,,) where
  liftGMergingStrategy3 m1 m2 m3 =
    gproduct2Strategy
      (\a (b, c) -> (a, b, c))
      (\(a, b, c) -> (a, (b, c)))
      m1
      (liftGMergingStrategy2 m2 m3)
  {-# INLINE liftGMergingStrategy3 #-}

-- (,,,)
deriving via
  (Default (a, b, c, d))
  instance
    (SymBoolOp bool, GMergeable bool a, GMergeable bool b, GMergeable bool c, GMergeable bool d) =>
    GMergeable bool (a, b, c, d)

deriving via
  (Default1 ((,,,) a b c))
  instance
    (SymBoolOp bool, GMergeable bool a, GMergeable bool b, GMergeable bool c) =>
    GMergeable1 bool ((,,,) a b c)

-- (,,,,)
deriving via
  (Default (a, b, c, d, e))
  instance
    (SymBoolOp bool, GMergeable bool a, GMergeable bool b, GMergeable bool c, GMergeable bool d, GMergeable bool e) =>
    GMergeable bool (a, b, c, d, e)

deriving via
  (Default1 ((,,,,) a b c d))
  instance
    (SymBoolOp bool, GMergeable bool a, GMergeable bool b, GMergeable bool c, GMergeable bool d) =>
    GMergeable1 bool ((,,,,) a b c d)

-- (,,,,,)
deriving via
  (Default (a, b, c, d, e, f))
  instance
    ( SymBoolOp bool,
      GMergeable bool a,
      GMergeable bool b,
      GMergeable bool c,
      GMergeable bool d,
      GMergeable bool e,
      GMergeable bool f
    ) =>
    GMergeable bool (a, b, c, d, e, f)

deriving via
  (Default1 ((,,,,,) a b c d e))
  instance
    (SymBoolOp bool, GMergeable bool a, GMergeable bool b, GMergeable bool c, GMergeable bool d, GMergeable bool e) =>
    GMergeable1 bool ((,,,,,) a b c d e)

-- (,,,,,,)
deriving via
  (Default (a, b, c, d, e, f, g))
  instance
    ( SymBoolOp bool,
      GMergeable bool a,
      GMergeable bool b,
      GMergeable bool c,
      GMergeable bool d,
      GMergeable bool e,
      GMergeable bool f,
      GMergeable bool g
    ) =>
    GMergeable bool (a, b, c, d, e, f, g)

deriving via
  (Default1 ((,,,,,,) a b c d e f))
  instance
    ( SymBoolOp bool,
      GMergeable bool a,
      GMergeable bool b,
      GMergeable bool c,
      GMergeable bool d,
      GMergeable bool e,
      GMergeable bool f
    ) =>
    GMergeable1 bool ((,,,,,,) a b c d e f)

-- (,,,,,,,)
deriving via
  (Default (a, b, c, d, e, f, g, h))
  instance
    ( SymBoolOp bool,
      GMergeable bool a,
      GMergeable bool b,
      GMergeable bool c,
      GMergeable bool d,
      GMergeable bool e,
      GMergeable bool f,
      GMergeable bool g,
      GMergeable bool h
    ) =>
    GMergeable bool (a, b, c, d, e, f, g, h)

deriving via
  (Default1 ((,,,,,,,) a b c d e f g))
  instance
    ( SymBoolOp bool,
      GMergeable bool a,
      GMergeable bool b,
      GMergeable bool c,
      GMergeable bool d,
      GMergeable bool e,
      GMergeable bool f,
      GMergeable bool g
    ) =>
    GMergeable1 bool ((,,,,,,,) a b c d e f g)

-- function
instance (SymBoolOp bool, GMergeable bool b) => GMergeable bool (a -> b) where
  gmergingStrategy = case gmergingStrategy @bool @b of
    SimpleStrategy m -> SimpleStrategy $ \cond t f v -> m cond (t v) (f v)
    _ -> NoStrategy
  {-# INLINE gmergingStrategy #-}

instance (SymBoolOp bool) => GMergeable1 bool ((->) a) where
  liftGMergingStrategy ms = case ms of
    SimpleStrategy m -> SimpleStrategy $ \cond t f v -> m cond (t v) (f v)
    _ -> NoStrategy
  {-# INLINE liftGMergingStrategy #-}

-- MaybeT
instance (SymBoolOp bool, GMergeable1 bool m, GMergeable bool a) => GMergeable bool (MaybeT m a) where
  gmergingStrategy = gwrapStrategy gmergingStrategy1 MaybeT runMaybeT
  {-# INLINE gmergingStrategy #-}

instance (SymBoolOp bool, GMergeable1 bool m) => GMergeable1 bool (MaybeT m) where
  liftGMergingStrategy m = gwrapStrategy (liftGMergingStrategy (liftGMergingStrategy m)) MaybeT runMaybeT
  {-# INLINE liftGMergingStrategy #-}

-- ExceptT
instance
  (SymBoolOp bool, GMergeable1 bool m, GMergeable bool e, GMergeable bool a) =>
  GMergeable bool (ExceptT e m a)
  where
  gmergingStrategy = gwrapStrategy gmergingStrategy1 ExceptT runExceptT
  {-# INLINE gmergingStrategy #-}

instance (SymBoolOp bool, GMergeable1 bool m, GMergeable bool e) => GMergeable1 bool (ExceptT e m) where
  liftGMergingStrategy m = gwrapStrategy (liftGMergingStrategy (liftGMergingStrategy m)) ExceptT runExceptT
  {-# INLINE liftGMergingStrategy #-}

-- state
instance
  (SymBoolOp bool, GMergeable bool s, GMergeable bool a, GMergeable1 bool m) =>
  GMergeable bool (StateLazy.StateT s m a)
  where
  gmergingStrategy = gwrapStrategy (liftGMergingStrategy gmergingStrategy1) StateLazy.StateT StateLazy.runStateT
  {-# INLINE gmergingStrategy #-}

instance (SymBoolOp bool, GMergeable bool s, GMergeable1 bool m) => GMergeable1 bool (StateLazy.StateT s m) where
  liftGMergingStrategy m =
    gwrapStrategy
      (liftGMergingStrategy (liftGMergingStrategy (liftGMergingStrategy2 m gmergingStrategy)))
      StateLazy.StateT
      StateLazy.runStateT
  {-# INLINE liftGMergingStrategy #-}

instance
  (SymBoolOp bool, GMergeable bool s, GMergeable bool a, GMergeable1 bool m) =>
  GMergeable bool (StateStrict.StateT s m a)
  where
  gmergingStrategy =
    gwrapStrategy (liftGMergingStrategy gmergingStrategy1) StateStrict.StateT StateStrict.runStateT
  {-# INLINE gmergingStrategy #-}

instance (SymBoolOp bool, GMergeable bool s, GMergeable1 bool m) => GMergeable1 bool (StateStrict.StateT s m) where
  liftGMergingStrategy m =
    gwrapStrategy
      (liftGMergingStrategy (liftGMergingStrategy (liftGMergingStrategy2 m gmergingStrategy)))
      StateStrict.StateT
      StateStrict.runStateT
  {-# INLINE liftGMergingStrategy #-}

-- writer
instance
  (SymBoolOp bool, GMergeable bool s, GMergeable bool a, GMergeable1 bool m) =>
  GMergeable bool (WriterLazy.WriterT s m a)
  where
  gmergingStrategy = gwrapStrategy (liftGMergingStrategy gmergingStrategy1) WriterLazy.WriterT WriterLazy.runWriterT
  {-# INLINE gmergingStrategy #-}

instance (SymBoolOp bool, GMergeable bool s, GMergeable1 bool m) => GMergeable1 bool (WriterLazy.WriterT s m) where
  liftGMergingStrategy m =
    gwrapStrategy
      (liftGMergingStrategy (liftGMergingStrategy2 m gmergingStrategy))
      WriterLazy.WriterT
      WriterLazy.runWriterT
  {-# INLINE liftGMergingStrategy #-}

instance
  (SymBoolOp bool, GMergeable bool s, GMergeable bool a, GMergeable1 bool m) =>
  GMergeable bool (WriterStrict.WriterT s m a)
  where
  gmergingStrategy = gwrapStrategy (liftGMergingStrategy gmergingStrategy1) WriterStrict.WriterT WriterStrict.runWriterT
  {-# INLINE gmergingStrategy #-}

instance (SymBoolOp bool, GMergeable bool s, GMergeable1 bool m) => GMergeable1 bool (WriterStrict.WriterT s m) where
  liftGMergingStrategy m =
    gwrapStrategy
      (liftGMergingStrategy (liftGMergingStrategy2 m gmergingStrategy))
      WriterStrict.WriterT
      WriterStrict.runWriterT
  {-# INLINE liftGMergingStrategy #-}

-- reader
instance
  (SymBoolOp bool, GMergeable bool a, GMergeable1 bool m) =>
  GMergeable bool (ReaderT s m a)
  where
  gmergingStrategy = gwrapStrategy (liftGMergingStrategy gmergingStrategy1) ReaderT runReaderT
  {-# INLINE gmergingStrategy #-}

instance (SymBoolOp bool, GMergeable1 bool m) => GMergeable1 bool (ReaderT s m) where
  liftGMergingStrategy m =
    gwrapStrategy
      (liftGMergingStrategy (liftGMergingStrategy m))
      ReaderT
      runReaderT
  {-# INLINE liftGMergingStrategy #-}

-- Sum
instance
  (SymBoolOp bool, GMergeable1 bool l, GMergeable1 bool r, GMergeable bool x) =>
  GMergeable bool (Sum l r x)
  where
  gmergingStrategy =
    SortedStrategy
      ( \case
          InL _ -> False
          InR _ -> True
      )
      ( \case
          False -> gwrapStrategy gmergingStrategy1 InL (\case (InL v) -> v; _ -> error "impossible")
          True -> gwrapStrategy gmergingStrategy1 InR (\case (InR v) -> v; _ -> error "impossible")
      )
  {-# INLINE gmergingStrategy #-}

instance (SymBoolOp bool, GMergeable1 bool l, GMergeable1 bool r) => GMergeable1 bool (Sum l r) where
  liftGMergingStrategy m =
    SortedStrategy
      ( \case
          InL _ -> False
          InR _ -> True
      )
      ( \case
          False -> gwrapStrategy (liftGMergingStrategy m) InL (\case (InL v) -> v; _ -> error "impossible")
          True -> gwrapStrategy (liftGMergingStrategy m) InR (\case (InR v) -> v; _ -> error "impossible")
      )
  {-# INLINE liftGMergingStrategy #-}

-- Ordering
deriving via
  (Default Ordering)
  instance
    (SymBoolOp bool) => GMergeable bool Ordering

-- Generic
deriving via
  (Default (U1 x))
  instance
    (SymBoolOp bool) => GMergeable bool (U1 x)

deriving via
  (Default (V1 x))
  instance
    (SymBoolOp bool) => GMergeable bool (V1 x)

deriving via
  (Default (K1 i c x))
  instance
    (SymBoolOp bool, GMergeable bool c) => GMergeable bool (K1 i c x)

deriving via
  (Default (M1 i c a x))
  instance
    (SymBoolOp bool, GMergeable bool (a x)) => GMergeable bool (M1 i c a x)

deriving via
  (Default ((a :+: b) x))
  instance
    (SymBoolOp bool, GMergeable bool (a x), GMergeable bool (b x)) => GMergeable bool ((a :+: b) x)

deriving via
  (Default ((a :*: b) x))
  instance
    (SymBoolOp bool, GMergeable bool (a x), GMergeable bool (b x)) => GMergeable bool ((a :*: b) x)

-- Identity
instance (SymBoolOp bool, GMergeable bool a) => GMergeable bool (Identity a) where
  gmergingStrategy = gwrapStrategy gmergingStrategy Identity runIdentity
  {-# INLINE gmergingStrategy #-}

instance SymBoolOp bool => GMergeable1 bool Identity where
  liftGMergingStrategy m = gwrapStrategy m Identity runIdentity
  {-# INLINE liftGMergingStrategy #-}

-- IdentityT
instance (SymBoolOp bool, GMergeable1 bool m, GMergeable bool a) => GMergeable bool (IdentityT m a) where
  gmergingStrategy = gwrapStrategy gmergingStrategy1 IdentityT runIdentityT
  {-# INLINE gmergingStrategy #-}

instance (SymBoolOp bool, GMergeable1 bool m) => GMergeable1 bool (IdentityT m) where
  liftGMergingStrategy m = gwrapStrategy (liftGMergingStrategy m) IdentityT runIdentityT
  {-# INLINE liftGMergingStrategy #-}

-- ContT
instance (SymBoolOp bool, GMergeable1 bool m, GMergeable bool r) => GMergeable bool (ContT r m a) where
  gmergingStrategy =
    gwrapStrategy
      (liftGMergingStrategy gmergingStrategy1)
      ContT
      (\(ContT v) -> v)
  {-# INLINE gmergingStrategy #-}

instance (SymBoolOp bool, GMergeable1 bool m, GMergeable bool r) => GMergeable1 bool (ContT r m) where
  liftGMergingStrategy _ =
    gwrapStrategy
      (liftGMergingStrategy gmergingStrategy1)
      ContT
      (\(ContT v) -> v)
  {-# INLINE liftGMergingStrategy #-}

-- RWS
instance
  (SymBoolOp bool, GMergeable bool s, GMergeable bool w, GMergeable bool a, GMergeable1 bool m) =>
  GMergeable bool (RWSLazy.RWST r w s m a)
  where
  gmergingStrategy = gwrapStrategy (liftGMergingStrategy (liftGMergingStrategy gmergingStrategy1)) RWSLazy.RWST (\(RWSLazy.RWST m) -> m)
  {-# INLINE gmergingStrategy #-}

instance
  (SymBoolOp bool, GMergeable bool s, GMergeable bool w, GMergeable1 bool m) =>
  GMergeable1 bool (RWSLazy.RWST r w s m)
  where
  liftGMergingStrategy m =
    gwrapStrategy
      (liftGMergingStrategy (liftGMergingStrategy (liftGMergingStrategy (liftGMergingStrategy3 m gmergingStrategy gmergingStrategy))))
      RWSLazy.RWST
      (\(RWSLazy.RWST rws) -> rws)
  {-# INLINE liftGMergingStrategy #-}

instance
  (SymBoolOp bool, GMergeable bool s, GMergeable bool w, GMergeable bool a, GMergeable1 bool m) =>
  GMergeable bool (RWSStrict.RWST r w s m a)
  where
  gmergingStrategy = gwrapStrategy (liftGMergingStrategy (liftGMergingStrategy gmergingStrategy1)) RWSStrict.RWST (\(RWSStrict.RWST m) -> m)
  {-# INLINE gmergingStrategy #-}

instance
  (SymBoolOp bool, GMergeable bool s, GMergeable bool w, GMergeable1 bool m) =>
  GMergeable1 bool (RWSStrict.RWST r w s m)
  where
  liftGMergingStrategy m =
    gwrapStrategy
      (liftGMergingStrategy (liftGMergingStrategy (liftGMergingStrategy (liftGMergingStrategy3 m gmergingStrategy gmergingStrategy))))
      RWSStrict.RWST
      (\(RWSStrict.RWST rws) -> rws)
  {-# INLINE liftGMergingStrategy #-}

-- Data.Monoid module
deriving via
  (Default (Monoid.Sum a))
  instance
    (GMergeable bool a) => GMergeable bool (Monoid.Sum a)

deriving via (Default1 Monoid.Sum) instance GMergeable1 bool Monoid.Sum
