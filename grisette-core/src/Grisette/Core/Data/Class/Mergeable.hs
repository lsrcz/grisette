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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Core.Data.Class.Mergeable
  ( GMergingStrategy (..),
    GMergeable (..),
    GMergeable' (..),
    GMergeable1 (..),
    grootStrategy1,
    GMergeable2 (..),
    grootStrategy2,
    GMergeable3 (..),
    grootStrategy3,
    derivedGRootStrategy,
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
-- Useful when trying to write efficient merge strategy for lists / vectors.
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

-- Resolves the indices and the terminal merge strategy for a value of some 'GMergeable' type.
gresolveStrategy :: forall bool x. GMergingStrategy bool x -> x -> ([DynamicSortedIdx], GMergingStrategy bool x)
gresolveStrategy s x = gresolveStrategy' x s
{-# INLINE gresolveStrategy #-}

-- Resolves the indices and the terminal merge strategy for a value given a merge strategy for its type.
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

-- | Merge strategy types.
--
-- A merge strategy encodes how to merge a __/subset/__ of the values of a given type.
--
-- The 'SimpleStrategy' merges values with a simple merge function.
-- For example,
--
--    (1) the symbolic boolean values can be directly merged with 'ites'.
--
--    (2) the set @{1}@, which is a subset of the values of the type @Integer@,
--        can be simply merged as the set contains only a single value.
--
--    (3) all the 'Just' values of the type @Maybe SymBool@ can be simply merged
--        by merging the wrapped symbolic boolean with ites.
--
-- The 'SortedStrategy' merges values by first grouping the values with an indexing
-- function. Each group with be merged in a subtree with a sub-strategy for the index.
-- Grisette will use these information to generate efficient SMT formula.
-- For example,
--
--    (1) all the integers can be merged with 'SortedStrategy' by indexing with identity map
--        and use the 'SimpleStrategy' shown before as the sub-strategies.
--
--    (2) all the @Maybe SymBool@ values can be merged with 'SortedStrategy' by
--        indexing with 'Data.Maybe.isJust'.
--
-- The 'NoStrategy' does not perform any merging.
-- For example, we cannot merge functions that returns concrete lists.
--
-- Usually the user does not have to implement 'GMergingStrategy' manually,
-- and the derived 'GMergeable' type class for ADTs is sufficient.
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
  NoStrategy :: GMergingStrategy bool a

-- | Useful utility function for building merge strategies manually.
--
-- For example, to build the merge strategy for the just branch of 'Maybe a',
-- one could write
--
-- > gwrapStrategy Just fromMaybe grootStrategy :: GMergingStrategy (Maybe a)
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

-- | Each type is associated with a root merge strategy given by 'grootStrategy'.
-- The root merge strategy should be able to merge every value of the type.
-- Grisette will use the root merge strategy to merge the values of the type.
class GMergeable bool a where
  grootStrategy :: GMergingStrategy bool a

instance (Generic a, GMergeable' bool (Rep a)) => GMergeable bool (Default a) where
  grootStrategy = unsafeCoerce (derivedGRootStrategy :: GMergingStrategy bool a)
  {-# NOINLINE grootStrategy #-}

-- | Generic derivation for the 'GMergeable' class.
derivedGRootStrategy :: (Generic a, GMergeable' bool (Rep a)) => GMergingStrategy bool a
derivedGRootStrategy = gwrapStrategy grootStrategy' to from
{-# INLINE derivedGRootStrategy #-}

-- | Lifting of the 'GMergeable' class to unary type constructors.
class GMergeable1 bool (u :: Type -> Type) where
  -- | Lift merge strategy through the type constructor.
  liftGRootStrategy :: GMergingStrategy bool a -> GMergingStrategy bool (u a)

-- | Lift the root merge strategy through the unary type constructor.
grootStrategy1 :: (GMergeable bool a, GMergeable1 bool u) => GMergingStrategy bool (u a)
grootStrategy1 = liftGRootStrategy grootStrategy
{-# INLINE grootStrategy1 #-}

-- | Lifting of the 'GMergeable' class to binary type constructors.
class GMergeable2 bool (u :: Type -> Type -> Type) where
  liftGRootStrategy2 :: GMergingStrategy bool a -> GMergingStrategy bool b -> GMergingStrategy bool (u a b)

-- | Lift the root merge strategy through the binary type constructor.
grootStrategy2 :: (GMergeable bool a, GMergeable bool b, GMergeable2 bool u) => GMergingStrategy bool (u a b)
grootStrategy2 = liftGRootStrategy2 grootStrategy grootStrategy
{-# INLINE grootStrategy2 #-}

class GMergeable3 bool (u :: Type -> Type -> Type -> Type) where
  liftGRootStrategy3 :: GMergingStrategy bool a -> GMergingStrategy bool b -> GMergingStrategy bool c -> GMergingStrategy bool (u a b c)

-- | Lift the root merge strategy through the binary type constructor.
grootStrategy3 :: (GMergeable bool a, GMergeable bool b, GMergeable bool c, GMergeable3 bool u) => GMergingStrategy bool (u a b c)
grootStrategy3 = liftGRootStrategy3 grootStrategy grootStrategy grootStrategy
{-# INLINE grootStrategy3 #-}

instance (Generic1 u, GMergeable1' bool (Rep1 u)) => GMergeable1 bool (Default1 u) where
  liftGRootStrategy = unsafeCoerce (derivedLiftGMergingStrategy :: GMergingStrategy bool a -> GMergingStrategy bool (u a))
  {-# NOINLINE liftGRootStrategy #-}

class GMergeable1' bool (u :: Type -> Type) where
  liftGRootStrategy' :: GMergingStrategy bool a -> GMergingStrategy bool (u a)

instance GMergeable1' bool U1 where
  liftGRootStrategy' _ = SimpleStrategy (\_ t _ -> t)
  {-# INLINE liftGRootStrategy' #-}

instance GMergeable1' bool V1 where
  liftGRootStrategy' _ = SimpleStrategy (\_ t _ -> t)
  {-# INLINE liftGRootStrategy' #-}

instance GMergeable1' bool Par1 where
  liftGRootStrategy' m = gwrapStrategy m Par1 unPar1
  {-# INLINE liftGRootStrategy' #-}

instance GMergeable1 bool f => GMergeable1' bool (Rec1 f) where
  liftGRootStrategy' m = gwrapStrategy (liftGRootStrategy m) Rec1 unRec1
  {-# INLINE liftGRootStrategy' #-}

instance GMergeable bool c => GMergeable1' bool (K1 i c) where
  liftGRootStrategy' _ = gwrapStrategy grootStrategy K1 unK1
  {-# INLINE liftGRootStrategy' #-}

instance GMergeable1' bool a => GMergeable1' bool (M1 i c a) where
  liftGRootStrategy' m = gwrapStrategy (liftGRootStrategy' m) M1 unM1
  {-# INLINE liftGRootStrategy' #-}

instance (GMergeable1' bool a, GMergeable1' bool b) => GMergeable1' bool (a :+: b) where
  liftGRootStrategy' m =
    SortedStrategy
      ( \case
          L1 _ -> False
          R1 _ -> True
      )
      ( \idx ->
          if not idx
            then gwrapStrategy (liftGRootStrategy' m) L1 (\case (L1 v) -> v; _ -> error "impossible")
            else gwrapStrategy (liftGRootStrategy' m) R1 (\case (R1 v) -> v; _ -> error "impossible")
      )
  {-# INLINE liftGRootStrategy' #-}

instance (GMergeable1' bool a, GMergeable1' bool b) => GMergeable1' bool (a :*: b) where
  liftGRootStrategy' m = gproduct2Strategy (:*:) (\(a :*: b) -> (a, b)) (liftGRootStrategy' m) (liftGRootStrategy' m)
  {-# INLINE liftGRootStrategy' #-}

-- | Generic derivation for the 'GMergeable' class.
derivedLiftGMergingStrategy :: (Generic1 u, GMergeable1' bool (Rep1 u)) => GMergingStrategy bool a -> GMergingStrategy bool (u a)
derivedLiftGMergingStrategy m = gwrapStrategy (liftGRootStrategy' m) to1 from1
{-# INLINE derivedLiftGMergingStrategy #-}

{-
-- | Resolves the 'GMergeable' constraint through a 'GMergeable1' type constructor.
withGMergeable :: forall bool u a b. (GMergeable1 bool u, GMergeable bool a) => (GMergeable bool (u a) => b) -> b
withGMergeable v = unCConst $ withGMergeableT @bool @u @a @(CConst (GMergeable bool (u a)) b) $ CConst v
-}

-- | Auxiliary class for the generic derivation for the 'GMergeable' class.
class GMergeable' bool f where
  grootStrategy' :: GMergingStrategy bool (f a)

instance GMergeable' bool U1 where
  grootStrategy' = SimpleStrategy (\_ t _ -> t)
  {-# INLINE grootStrategy' #-}

instance GMergeable' bool V1 where
  grootStrategy' = SimpleStrategy (\_ t _ -> t)
  {-# INLINE grootStrategy' #-}

instance (GMergeable bool c) => GMergeable' bool (K1 i c) where
  grootStrategy' = gwrapStrategy grootStrategy K1 unK1
  {-# INLINE grootStrategy' #-}

instance (GMergeable' bool a) => GMergeable' bool (M1 i c a) where
  grootStrategy' = gwrapStrategy grootStrategy' M1 unM1
  {-# INLINE grootStrategy' #-}

instance (GMergeable' bool a, GMergeable' bool b) => GMergeable' bool (a :+: b) where
  grootStrategy' =
    SortedStrategy
      ( \case
          L1 _ -> False
          R1 _ -> True
      )
      ( \idx ->
          if not idx
            then gwrapStrategy grootStrategy' L1 (\case (L1 v) -> v; _ -> undefined)
            else gwrapStrategy grootStrategy' R1 (\case (R1 v) -> v; _ -> undefined)
      )
  {-# INLINE grootStrategy' #-}

gproduct2Strategy ::
  (a -> b -> r) ->
  (r -> (a, b)) ->
  GMergingStrategy bool a ->
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
  grootStrategy' = gproduct2Strategy (:*:) (\(a :*: b) -> (a, b)) grootStrategy' grootStrategy'
  {-# INLINE grootStrategy' #-}

-- instances

#define CONCRETE_ORD_MERGABLE(type) \
instance (SymBoolOp bool) => GMergeable bool type where \
  grootStrategy = \
    let sub = SimpleStrategy $ \_ t _ -> t \
     in SortedStrategy id $ const sub; \
  {-# INLINE grootStrategy #-}

#if 1
CONCRETE_ORD_MERGABLE(Bool)
CONCRETE_ORD_MERGABLE(Integer)
CONCRETE_ORD_MERGABLE(Char)
CONCRETE_ORD_MERGABLE(Int)
CONCRETE_ORD_MERGABLE(Int8)
CONCRETE_ORD_MERGABLE(Int16)
CONCRETE_ORD_MERGABLE(Int32)
CONCRETE_ORD_MERGABLE(Int64)
CONCRETE_ORD_MERGABLE(Word)
CONCRETE_ORD_MERGABLE(Word8)
CONCRETE_ORD_MERGABLE(Word16)
CONCRETE_ORD_MERGABLE(Word32)
CONCRETE_ORD_MERGABLE(Word64)
CONCRETE_ORD_MERGABLE(B.ByteString)
#endif

-- ()
deriving via (Default ()) instance (SymBoolOp bool) => GMergeable bool ()

-- Either
deriving via (Default (Either e a)) instance (SymBoolOp bool, GMergeable bool e, GMergeable bool a) => GMergeable bool (Either e a)

deriving via (Default1 (Either e)) instance (SymBoolOp bool, GMergeable bool e) => GMergeable1 bool (Either e)

instance (SymBoolOp bool) => GMergeable2 bool Either where
  liftGRootStrategy2 m1 m2 =
    SortedStrategy
      ( \case
          Left _ -> False
          Right _ -> True
      )
      ( \case
          False -> gwrapStrategy m1 Left (\case (Left v) -> v; _ -> undefined)
          True -> gwrapStrategy m2 Right (\case (Right v) -> v; _ -> undefined)
      )
  {-# INLINE liftGRootStrategy2 #-}

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
  grootStrategy = case grootStrategy :: GMergingStrategy bool a of
    SimpleStrategy m ->
      SortedStrategy length $ \_ ->
        SimpleStrategy $ \cond -> zipWith (m cond)
    NoStrategy ->
      SortedStrategy length $ const NoStrategy
    _ -> SortedStrategy length $ \_ ->
      SortedStrategy (gbuildStrategyList @bool grootStrategy) $ \(StrategyList _ strategies) ->
        let s :: [GMergingStrategy bool a] = unsafeCoerce strategies
            allSimple = all (\case SimpleStrategy _ -> True; _ -> False) s
         in if allSimple
              then SimpleStrategy $ \cond l r ->
                (\case (SimpleStrategy f, l1, r1) -> f cond l1 r1; _ -> error "impossible") <$> zip3 s l r
              else NoStrategy
  {-# INLINE grootStrategy #-}

instance (SymBoolOp bool) => GMergeable1 bool [] where
  liftGRootStrategy (ms :: GMergingStrategy bool a) = case ms of
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
  {-# INLINE liftGRootStrategy #-}

-- (,)
deriving via (Default (a, b)) instance (SymBoolOp bool, GMergeable bool a, GMergeable bool b) => GMergeable bool (a, b)

deriving via (Default1 ((,) a)) instance (SymBoolOp bool, GMergeable bool a) => GMergeable1 bool ((,) a)

instance SymBoolOp bool => GMergeable2 bool (,) where
  liftGRootStrategy2 = gproduct2Strategy (,) id
  {-# INLINE liftGRootStrategy2 #-}

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
  liftGRootStrategy2 = liftGRootStrategy3 grootStrategy
  {-# INLINE liftGRootStrategy2 #-}

instance SymBoolOp bool => GMergeable3 bool (,,) where
  liftGRootStrategy3 m1 m2 m3 =
    gproduct2Strategy
      (\a (b, c) -> (a, b, c))
      (\(a, b, c) -> (a, (b, c)))
      m1
      (liftGRootStrategy2 m2 m3)
  {-# INLINE liftGRootStrategy3 #-}

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
  grootStrategy = case grootStrategy @bool @b of
    SimpleStrategy m -> SimpleStrategy $ \cond t f v -> m cond (t v) (f v)
    _ -> NoStrategy
  {-# INLINE grootStrategy #-}

instance (SymBoolOp bool) => GMergeable1 bool ((->) a) where
  liftGRootStrategy ms = case ms of
    SimpleStrategy m -> SimpleStrategy $ \cond t f v -> m cond (t v) (f v)
    _ -> NoStrategy
  {-# INLINE liftGRootStrategy #-}

-- MaybeT
instance (SymBoolOp bool, GMergeable1 bool m, GMergeable bool a) => GMergeable bool (MaybeT m a) where
  grootStrategy = gwrapStrategy grootStrategy1 MaybeT runMaybeT
  {-# INLINE grootStrategy #-}

instance (SymBoolOp bool, GMergeable1 bool m) => GMergeable1 bool (MaybeT m) where
  liftGRootStrategy m = gwrapStrategy (liftGRootStrategy (liftGRootStrategy m)) MaybeT runMaybeT
  {-# INLINE liftGRootStrategy #-}

-- ExceptT
instance
  (SymBoolOp bool, GMergeable1 bool m, GMergeable bool e, GMergeable bool a) =>
  GMergeable bool (ExceptT e m a)
  where
  grootStrategy = gwrapStrategy grootStrategy1 ExceptT runExceptT
  {-# INLINE grootStrategy #-}

instance (SymBoolOp bool, GMergeable1 bool m, GMergeable bool e) => GMergeable1 bool (ExceptT e m) where
  liftGRootStrategy m = gwrapStrategy (liftGRootStrategy (liftGRootStrategy m)) ExceptT runExceptT
  {-# INLINE liftGRootStrategy #-}

-- state
instance
  (SymBoolOp bool, GMergeable bool s, GMergeable bool a, GMergeable1 bool m) =>
  GMergeable bool (StateLazy.StateT s m a)
  where
  grootStrategy = gwrapStrategy (liftGRootStrategy grootStrategy1) StateLazy.StateT StateLazy.runStateT
  {-# INLINE grootStrategy #-}

instance (SymBoolOp bool, GMergeable bool s, GMergeable1 bool m) => GMergeable1 bool (StateLazy.StateT s m) where
  liftGRootStrategy m =
    gwrapStrategy
      (liftGRootStrategy (liftGRootStrategy (liftGRootStrategy2 m grootStrategy)))
      StateLazy.StateT
      StateLazy.runStateT
  {-# INLINE liftGRootStrategy #-}

instance
  (SymBoolOp bool, GMergeable bool s, GMergeable bool a, GMergeable1 bool m) =>
  GMergeable bool (StateStrict.StateT s m a)
  where
  grootStrategy =
    gwrapStrategy (liftGRootStrategy grootStrategy1) StateStrict.StateT StateStrict.runStateT
  {-# INLINE grootStrategy #-}

instance (SymBoolOp bool, GMergeable bool s, GMergeable1 bool m) => GMergeable1 bool (StateStrict.StateT s m) where
  liftGRootStrategy m =
    gwrapStrategy
      (liftGRootStrategy (liftGRootStrategy (liftGRootStrategy2 m grootStrategy)))
      StateStrict.StateT
      StateStrict.runStateT
  {-# INLINE liftGRootStrategy #-}

-- writer
instance
  (SymBoolOp bool, GMergeable bool s, GMergeable bool a, GMergeable1 bool m) =>
  GMergeable bool (WriterLazy.WriterT s m a)
  where
  grootStrategy = gwrapStrategy (liftGRootStrategy grootStrategy1) WriterLazy.WriterT WriterLazy.runWriterT
  {-# INLINE grootStrategy #-}

instance (SymBoolOp bool, GMergeable bool s, GMergeable1 bool m) => GMergeable1 bool (WriterLazy.WriterT s m) where
  liftGRootStrategy m =
    gwrapStrategy
      (liftGRootStrategy (liftGRootStrategy2 m grootStrategy))
      WriterLazy.WriterT
      WriterLazy.runWriterT
  {-# INLINE liftGRootStrategy #-}

instance
  (SymBoolOp bool, GMergeable bool s, GMergeable bool a, GMergeable1 bool m) =>
  GMergeable bool (WriterStrict.WriterT s m a)
  where
  grootStrategy = gwrapStrategy (liftGRootStrategy grootStrategy1) WriterStrict.WriterT WriterStrict.runWriterT
  {-# INLINE grootStrategy #-}

instance (SymBoolOp bool, GMergeable bool s, GMergeable1 bool m) => GMergeable1 bool (WriterStrict.WriterT s m) where
  liftGRootStrategy m =
    gwrapStrategy
      (liftGRootStrategy (liftGRootStrategy2 m grootStrategy))
      WriterStrict.WriterT
      WriterStrict.runWriterT
  {-# INLINE liftGRootStrategy #-}

-- reader
instance
  (SymBoolOp bool, GMergeable bool a, GMergeable1 bool m) =>
  GMergeable bool (ReaderT s m a)
  where
  grootStrategy = gwrapStrategy (liftGRootStrategy grootStrategy1) ReaderT runReaderT
  {-# INLINE grootStrategy #-}

instance (SymBoolOp bool, GMergeable1 bool m) => GMergeable1 bool (ReaderT s m) where
  liftGRootStrategy m =
    gwrapStrategy
      (liftGRootStrategy (liftGRootStrategy m))
      ReaderT
      runReaderT
  {-# INLINE liftGRootStrategy #-}

-- Sum
instance
  (SymBoolOp bool, GMergeable1 bool l, GMergeable1 bool r, GMergeable bool x) =>
  GMergeable bool (Sum l r x)
  where
  grootStrategy =
    SortedStrategy
      ( \case
          InL _ -> False
          InR _ -> True
      )
      ( \case
          False -> gwrapStrategy grootStrategy1 InL (\case (InL v) -> v; _ -> error "impossible")
          True -> gwrapStrategy grootStrategy1 InR (\case (InR v) -> v; _ -> error "impossible")
      )
  {-# INLINE grootStrategy #-}

instance (SymBoolOp bool, GMergeable1 bool l, GMergeable1 bool r) => GMergeable1 bool (Sum l r) where
  liftGRootStrategy m =
    SortedStrategy
      ( \case
          InL _ -> False
          InR _ -> True
      )
      ( \case
          False -> gwrapStrategy (liftGRootStrategy m) InL (\case (InL v) -> v; _ -> error "impossible")
          True -> gwrapStrategy (liftGRootStrategy m) InR (\case (InR v) -> v; _ -> error "impossible")
      )
  {-# INLINE liftGRootStrategy #-}

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
  grootStrategy = gwrapStrategy grootStrategy Identity runIdentity
  {-# INLINE grootStrategy #-}

instance SymBoolOp bool => GMergeable1 bool Identity where
  liftGRootStrategy m = gwrapStrategy m Identity runIdentity
  {-# INLINE liftGRootStrategy #-}

-- IdentityT
instance (SymBoolOp bool, GMergeable1 bool m, GMergeable bool a) => GMergeable bool (IdentityT m a) where
  grootStrategy = gwrapStrategy grootStrategy1 IdentityT runIdentityT
  {-# INLINE grootStrategy #-}

instance (SymBoolOp bool, GMergeable1 bool m) => GMergeable1 bool (IdentityT m) where
  liftGRootStrategy m = gwrapStrategy (liftGRootStrategy m) IdentityT runIdentityT
  {-# INLINE liftGRootStrategy #-}

-- ContT
instance (SymBoolOp bool, GMergeable1 bool m, GMergeable bool r) => GMergeable bool (ContT r m a) where
  grootStrategy =
    gwrapStrategy
      (liftGRootStrategy grootStrategy1)
      ContT
      (\(ContT v) -> v)
  {-# INLINE grootStrategy #-}

instance (SymBoolOp bool, GMergeable1 bool m, GMergeable bool r) => GMergeable1 bool (ContT r m) where
  liftGRootStrategy _ =
    gwrapStrategy
      (liftGRootStrategy grootStrategy1)
      ContT
      (\(ContT v) -> v)
  {-# INLINE liftGRootStrategy #-}

-- RWS
instance
  (SymBoolOp bool, GMergeable bool s, GMergeable bool w, GMergeable bool a, GMergeable1 bool m) =>
  GMergeable bool (RWSLazy.RWST r w s m a)
  where
  grootStrategy = gwrapStrategy (liftGRootStrategy (liftGRootStrategy grootStrategy1)) RWSLazy.RWST (\(RWSLazy.RWST m) -> m)
  {-# INLINE grootStrategy #-}

instance
  (SymBoolOp bool, GMergeable bool s, GMergeable bool w, GMergeable1 bool m) =>
  GMergeable1 bool (RWSLazy.RWST r w s m)
  where
  liftGRootStrategy m =
    gwrapStrategy
      (liftGRootStrategy (liftGRootStrategy (liftGRootStrategy (liftGRootStrategy3 m grootStrategy grootStrategy))))
      RWSLazy.RWST
      (\(RWSLazy.RWST rws) -> rws)
  {-# INLINE liftGRootStrategy #-}

instance
  (SymBoolOp bool, GMergeable bool s, GMergeable bool w, GMergeable bool a, GMergeable1 bool m) =>
  GMergeable bool (RWSStrict.RWST r w s m a)
  where
  grootStrategy = gwrapStrategy (liftGRootStrategy (liftGRootStrategy grootStrategy1)) RWSStrict.RWST (\(RWSStrict.RWST m) -> m)
  {-# INLINE grootStrategy #-}

instance
  (SymBoolOp bool, GMergeable bool s, GMergeable bool w, GMergeable1 bool m) =>
  GMergeable1 bool (RWSStrict.RWST r w s m)
  where
  liftGRootStrategy m =
    gwrapStrategy
      (liftGRootStrategy (liftGRootStrategy (liftGRootStrategy (liftGRootStrategy3 m grootStrategy grootStrategy))))
      RWSStrict.RWST
      (\(RWSStrict.RWST rws) -> rws)
  {-# INLINE liftGRootStrategy #-}

-- Data.Monoid module
deriving via
  (Default (Monoid.Sum a))
  instance
    (GMergeable bool a) => GMergeable bool (Monoid.Sum a)

deriving via (Default1 Monoid.Sum) instance GMergeable1 bool Monoid.Sum
