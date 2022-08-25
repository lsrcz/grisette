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

module Pizza.Core.Data.Class.Mergeable
  ( MergingStrategy (..),
    Mergeable (..),
    Mergeable' (..),
    Mergeable1 (..),
    mergingStrategy1,
    Mergeable2 (..),
    mergingStrategy2,
    Mergeable3 (..),
    mergingStrategy3,
    -- withMergeable,
    derivedMergingStrategy,
    wrapStrategy,
    product2Strategy,
    DynamicSortedIdx (..),
    StrategyList (..),
    buildStrategyList,
    resolveStrategy,
    resolveStrategy',
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
import Pizza.Core.Data.Class.Bool
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

-- Resolves the indices and the terminal merge strategy for a value of some 'Mergeable' type.
resolveStrategy :: forall bool x. MergingStrategy bool x -> x -> ([DynamicSortedIdx], MergingStrategy bool x)
resolveStrategy s x = resolveStrategy' x s
{-# INLINE resolveStrategy #-}

-- Resolves the indices and the terminal merge strategy for a value given a merge strategy for its type.
resolveStrategy' :: forall bool x. x -> MergingStrategy bool x -> ([DynamicSortedIdx], MergingStrategy bool x)
resolveStrategy' x = go
  where
    go :: MergingStrategy bool x -> ([DynamicSortedIdx], MergingStrategy bool x)
    go (SortedStrategy idxFun subStrategy) = case go ss of
      (idxs, r) -> (DynamicSortedIdx idx : idxs, r)
      where
        idx = idxFun x
        ss = subStrategy idx
    go s = ([], s)
{-# INLINE resolveStrategy' #-}

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
-- Pizza will use these information to generate efficient SMT formula.
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
-- Usually the user does not have to implement 'MergingStrategy' manually,
-- and the derived 'Mergeable' type class for ADTs is sufficient.
data MergingStrategy bool a where
  -- | Simple mergeable strategy.
  --
  -- For symbolic booleans, we can implement its merge strategy as follows:
  --
  -- > SimpleStrategy ites :: MergingStrategy SymBool SymBool
  SimpleStrategy ::
    -- | Merge function.
    (bool -> a -> a -> a) ->
    MergingStrategy bool a
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
    (idx -> MergingStrategy bool a) ->
    MergingStrategy bool a
  NoStrategy :: MergingStrategy bool a

-- | Useful utility function for building merge strategies manually.
--
-- For example, to build the merge strategy for the just branch of 'Maybe a',
-- one could write
--
-- > wrapStrategy Just fromMaybe mergingStrategy :: MergingStrategy (Maybe a)
wrapStrategy ::
  -- | The merge strategy to be wrapped
  MergingStrategy bool a ->
  -- | The wrap function
  (a -> b) ->
  -- | The unwrap function, which does not have to be defined for every value
  (b -> a) ->
  MergingStrategy bool b
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

-- | Each type is associated with a root merge strategy given by 'mergingStrategy'.
-- The root merge strategy should be able to merge every value of the type.
-- Pizza will use the root merge strategy to merge the values of the type.
class Mergeable bool a where
  mergingStrategy :: MergingStrategy bool a

instance (Generic a, Mergeable' bool (Rep a)) => Mergeable bool (Default a) where
  mergingStrategy = unsafeCoerce (derivedMergingStrategy :: MergingStrategy bool a)
  {-# NOINLINE mergingStrategy #-}

-- | Generic derivation for the 'Mergeable' class.
derivedMergingStrategy :: (Generic a, Mergeable' bool (Rep a)) => MergingStrategy bool a
derivedMergingStrategy = wrapStrategy mergingStrategy' to from
{-# INLINE derivedMergingStrategy #-}

-- | Lifting of the 'Mergeable' class to unary type constructors.
class Mergeable1 bool (u :: Type -> Type) where
  -- | Lift merge strategy through the type constructor.
  liftMergingStrategy :: MergingStrategy bool a -> MergingStrategy bool (u a)

-- | Lift the root merge strategy through the unary type constructor.
mergingStrategy1 :: (Mergeable bool a, Mergeable1 bool u) => MergingStrategy bool (u a)
mergingStrategy1 = liftMergingStrategy mergingStrategy
{-# INLINE mergingStrategy1 #-}

-- | Lifting of the 'Mergeable' class to binary type constructors.
class Mergeable2 bool (u :: Type -> Type -> Type) where
  liftMergingStrategy2 :: MergingStrategy bool a -> MergingStrategy bool b -> MergingStrategy bool (u a b)

-- | Lift the root merge strategy through the binary type constructor.
mergingStrategy2 :: (Mergeable bool a, Mergeable bool b, Mergeable2 bool u) => MergingStrategy bool (u a b)
mergingStrategy2 = liftMergingStrategy2 mergingStrategy mergingStrategy
{-# INLINE mergingStrategy2 #-}

class Mergeable3 bool (u :: Type -> Type -> Type -> Type) where
  liftMergingStrategy3 :: MergingStrategy bool a -> MergingStrategy bool b -> MergingStrategy bool c -> MergingStrategy bool (u a b c)

-- | Lift the root merge strategy through the binary type constructor.
mergingStrategy3 :: (Mergeable bool a, Mergeable bool b, Mergeable bool c, Mergeable3 bool u) => MergingStrategy bool (u a b c)
mergingStrategy3 = liftMergingStrategy3 mergingStrategy mergingStrategy mergingStrategy
{-# INLINE mergingStrategy3 #-}

instance (Generic1 u, Mergeable1' bool (Rep1 u)) => Mergeable1 bool (Default1 u) where
  liftMergingStrategy = unsafeCoerce (derivedLiftMergingStrategy :: MergingStrategy bool a -> MergingStrategy bool (u a))
  {-# NOINLINE liftMergingStrategy #-}

class Mergeable1' bool (u :: Type -> Type) where
  liftMergingStrategy' :: MergingStrategy bool a -> MergingStrategy bool (u a)

instance Mergeable1' bool U1 where
  liftMergingStrategy' _ = SimpleStrategy (\_ t _ -> t)
  {-# INLINE liftMergingStrategy' #-}

instance Mergeable1' bool V1 where
  liftMergingStrategy' _ = SimpleStrategy (\_ t _ -> t)
  {-# INLINE liftMergingStrategy' #-}

instance Mergeable1' bool Par1 where
  liftMergingStrategy' m = wrapStrategy m Par1 unPar1
  {-# INLINE liftMergingStrategy' #-}

instance Mergeable1 bool f => Mergeable1' bool (Rec1 f) where
  liftMergingStrategy' m = wrapStrategy (liftMergingStrategy m) Rec1 unRec1
  {-# INLINE liftMergingStrategy' #-}

instance Mergeable bool c => Mergeable1' bool (K1 i c) where
  liftMergingStrategy' _ = wrapStrategy mergingStrategy K1 unK1
  {-# INLINE liftMergingStrategy' #-}

instance Mergeable1' bool a => Mergeable1' bool (M1 i c a) where
  liftMergingStrategy' m = wrapStrategy (liftMergingStrategy' m) M1 unM1
  {-# INLINE liftMergingStrategy' #-}

instance (Mergeable1' bool a, Mergeable1' bool b) => Mergeable1' bool (a :+: b) where
  liftMergingStrategy' m =
    SortedStrategy
      ( \case
          L1 _ -> False
          R1 _ -> True
      )
      ( \idx ->
          if not idx
            then wrapStrategy (liftMergingStrategy' m) L1 (\case (L1 v) -> v; _ -> error "impossible")
            else wrapStrategy (liftMergingStrategy' m) R1 (\case (R1 v) -> v; _ -> error "impossible")
      )
  {-# INLINE liftMergingStrategy' #-}

instance (Mergeable1' bool a, Mergeable1' bool b) => Mergeable1' bool (a :*: b) where
  liftMergingStrategy' m = product2Strategy (:*:) (\(a :*: b) -> (a, b)) (liftMergingStrategy' m) (liftMergingStrategy' m)
  {-# INLINE liftMergingStrategy' #-}

-- | Generic derivation for the 'Mergeable' class.
derivedLiftMergingStrategy :: (Generic1 u, Mergeable1' bool (Rep1 u)) => MergingStrategy bool a -> MergingStrategy bool (u a)
derivedLiftMergingStrategy m = wrapStrategy (liftMergingStrategy' m) to1 from1
{-# INLINE derivedLiftMergingStrategy #-}

{-
-- | Resolves the 'Mergeable' constraint through a 'Mergeable1' type constructor.
withMergeable :: forall bool u a b. (Mergeable1 bool u, Mergeable bool a) => (Mergeable bool (u a) => b) -> b
withMergeable v = unCConst $ withMergeableT @bool @u @a @(CConst (Mergeable bool (u a)) b) $ CConst v
-}

-- | Auxiliary class for the generic derivation for the 'Mergeable' class.
class Mergeable' bool f where
  mergingStrategy' :: MergingStrategy bool (f a)

instance Mergeable' bool U1 where
  mergingStrategy' = SimpleStrategy (\_ t _ -> t)
  {-# INLINE mergingStrategy' #-}

instance Mergeable' bool V1 where
  mergingStrategy' = SimpleStrategy (\_ t _ -> t)
  {-# INLINE mergingStrategy' #-}

instance (Mergeable bool c) => Mergeable' bool (K1 i c) where
  mergingStrategy' = wrapStrategy mergingStrategy K1 unK1
  {-# INLINE mergingStrategy' #-}

instance (Mergeable' bool a) => Mergeable' bool (M1 i c a) where
  mergingStrategy' = wrapStrategy mergingStrategy' M1 unM1
  {-# INLINE mergingStrategy' #-}

instance (Mergeable' bool a, Mergeable' bool b) => Mergeable' bool (a :+: b) where
  mergingStrategy' =
    SortedStrategy
      ( \case
          L1 _ -> False
          R1 _ -> True
      )
      ( \idx ->
          if not idx
            then wrapStrategy mergingStrategy' L1 (\case (L1 v) -> v; _ -> undefined)
            else wrapStrategy mergingStrategy' R1 (\case (R1 v) -> v; _ -> undefined)
      )
  {-# INLINE mergingStrategy' #-}

product2Strategy ::
  (a -> b -> r) ->
  (r -> (a, b)) ->
  MergingStrategy bool a ->
  MergingStrategy bool b ->
  MergingStrategy bool r
product2Strategy wrap unwrap strategy1 strategy2 =
  case (strategy1, strategy2) of
    (NoStrategy, _) -> NoStrategy
    (_, NoStrategy) -> NoStrategy
    (SimpleStrategy m1, SimpleStrategy m2) ->
      SimpleStrategy $ \cond t f -> case (unwrap t, unwrap f) of
        ((hdt, tlt), (hdf, tlf)) ->
          wrap (m1 cond hdt hdf) (m2 cond tlt tlf)
    (s1@(SimpleStrategy _), SortedStrategy idxf subf) ->
      SortedStrategy (idxf . snd . unwrap) (product2Strategy wrap unwrap s1 . subf)
    (SortedStrategy idxf subf, s2) ->
      SortedStrategy (idxf . fst . unwrap) (\idx -> product2Strategy wrap unwrap (subf idx) s2)
{-# INLINE product2Strategy #-}

instance (Mergeable' bool a, Mergeable' bool b) => Mergeable' bool (a :*: b) where
  mergingStrategy' = product2Strategy (:*:) (\(a :*: b) -> (a, b)) mergingStrategy' mergingStrategy'
  {-# INLINE mergingStrategy' #-}

-- instances

#define CONCRETE_ORD_MERGABLE(type) \
instance (SymBoolOp bool) => Mergeable bool type where \
  mergingStrategy = \
    let sub = SimpleStrategy $ \_ t _ -> t \
     in SortedStrategy id $ const sub; \
  {-# INLINE mergingStrategy #-}

CONCRETE_ORD_MERGABLE (Bool)
CONCRETE_ORD_MERGABLE (Integer)
CONCRETE_ORD_MERGABLE (Char)
CONCRETE_ORD_MERGABLE (Int)
CONCRETE_ORD_MERGABLE (Int8)
CONCRETE_ORD_MERGABLE (Int16)
CONCRETE_ORD_MERGABLE (Int32)
CONCRETE_ORD_MERGABLE (Int64)
CONCRETE_ORD_MERGABLE (Word)
CONCRETE_ORD_MERGABLE (Word8)
CONCRETE_ORD_MERGABLE (Word16)
CONCRETE_ORD_MERGABLE (Word32)
CONCRETE_ORD_MERGABLE (Word64)
CONCRETE_ORD_MERGABLE (B.ByteString)

-- ()
deriving via (Default ()) instance (SymBoolOp bool) => Mergeable bool ()

-- Either
deriving via (Default (Either e a)) instance (SymBoolOp bool, Mergeable bool e, Mergeable bool a) => Mergeable bool (Either e a)

deriving via (Default1 (Either e)) instance (SymBoolOp bool, Mergeable bool e) => Mergeable1 bool (Either e)

instance (SymBoolOp bool) => Mergeable2 bool Either where
  liftMergingStrategy2 m1 m2 =
    SortedStrategy
      ( \case
          Left _ -> False
          Right _ -> True
      )
      ( \case
          False -> wrapStrategy m1 Left (\case (Left v) -> v; _ -> undefined)
          True -> wrapStrategy m2 Right (\case (Right v) -> v; _ -> undefined)
      )
  {-# INLINE liftMergingStrategy2 #-}

-- Maybe
deriving via (Default (Maybe a)) instance (SymBoolOp bool, Mergeable bool a) => Mergeable bool (Maybe a)

deriving via (Default1 Maybe) instance (SymBoolOp bool) => Mergeable1 bool Maybe

-- | Helper type for building efficient merge strategy for list-like containers.
data StrategyList container where
  StrategyList ::
    forall bool a container.
    container [DynamicSortedIdx] ->
    container (MergingStrategy bool a) ->
    StrategyList container

-- | Helper function for building efficient merge strategy for list-like containers.
buildStrategyList ::
  forall bool a container.
  (Functor container) =>
  MergingStrategy bool a ->
  container a ->
  StrategyList container
buildStrategyList s l = StrategyList idxs strategies
  where
    r = resolveStrategy @bool s <$> l
    idxs = fst <$> r
    strategies = snd <$> r
{-# INLINE buildStrategyList #-}

instance Eq1 container => Eq (StrategyList container) where
  (StrategyList idxs1 _) == (StrategyList idxs2 _) = eq1 idxs1 idxs2
  {-# INLINE (==) #-}

instance Ord1 container => Ord (StrategyList container) where
  compare (StrategyList idxs1 _) (StrategyList idxs2 _) = compare1 idxs1 idxs2
  {-# INLINE compare #-}

instance Show1 container => Show (StrategyList container) where
  showsPrec i (StrategyList idxs1 _) = showsPrec1 i idxs1

-- List
instance (SymBoolOp bool, Mergeable bool a) => Mergeable bool [a] where
  mergingStrategy = case mergingStrategy :: MergingStrategy bool a of
    SimpleStrategy m ->
      SortedStrategy length $ \_ ->
        SimpleStrategy $ \cond -> zipWith (m cond)
    NoStrategy ->
      SortedStrategy length $ const NoStrategy
    _ -> SortedStrategy length $ \_ ->
      SortedStrategy (buildStrategyList @bool mergingStrategy) $ \(StrategyList _ strategies) ->
        let s :: [MergingStrategy bool a] = unsafeCoerce strategies
            allSimple = all (\case SimpleStrategy _ -> True; _ -> False) s
         in if allSimple
              then SimpleStrategy $ \cond l r ->
                (\case (SimpleStrategy f, l1, r1) -> f cond l1 r1; _ -> error "impossible") <$> zip3 s l r
              else NoStrategy
  {-# INLINE mergingStrategy #-}

instance (SymBoolOp bool) => Mergeable1 bool [] where
  liftMergingStrategy (ms :: MergingStrategy bool a) = case ms of
    SimpleStrategy m ->
      SortedStrategy length $ \_ ->
        SimpleStrategy $ \cond -> zipWith (m cond)
    NoStrategy ->
      SortedStrategy length $ const NoStrategy
    _ -> SortedStrategy length $ \_ ->
      SortedStrategy (buildStrategyList @bool ms) $ \(StrategyList _ strategies) ->
        let s :: [MergingStrategy bool a] = unsafeCoerce strategies
            allSimple = all (\case SimpleStrategy _ -> True; _ -> False) s
         in if allSimple
              then SimpleStrategy $ \cond l r ->
                (\case (SimpleStrategy f, l1, r1) -> f cond l1 r1; _ -> error "impossible") <$> zip3 s l r
              else NoStrategy
  {-# INLINE liftMergingStrategy #-}

-- (,)
deriving via (Default (a, b)) instance (SymBoolOp bool, Mergeable bool a, Mergeable bool b) => Mergeable bool (a, b)

deriving via (Default1 ((,) a)) instance (SymBoolOp bool, Mergeable bool a) => Mergeable1 bool ((,) a)

instance SymBoolOp bool => Mergeable2 bool (,) where
  liftMergingStrategy2 = product2Strategy (,) id
  {-# INLINE liftMergingStrategy2 #-}

-- (,,)
deriving via
  (Default (a, b, c))
  instance
    (SymBoolOp bool, Mergeable bool a, Mergeable bool b, Mergeable bool c) => Mergeable bool (a, b, c)

deriving via
  (Default1 ((,,) a b))
  instance
    (SymBoolOp bool, Mergeable bool a, Mergeable bool b) => Mergeable1 bool ((,,) a b)

instance (SymBoolOp bool, Mergeable bool a) => Mergeable2 bool ((,,) a) where
  liftMergingStrategy2 = liftMergingStrategy3 mergingStrategy
  {-# INLINE liftMergingStrategy2 #-}

instance SymBoolOp bool => Mergeable3 bool (,,) where
  liftMergingStrategy3 m1 m2 m3 =
    product2Strategy
      (\a (b, c) -> (a, b, c))
      (\(a, b, c) -> (a, (b, c)))
      m1
      (liftMergingStrategy2 m2 m3)
  {-# INLINE liftMergingStrategy3 #-}

-- (,,,)
deriving via
  (Default (a, b, c, d))
  instance
    (SymBoolOp bool, Mergeable bool a, Mergeable bool b, Mergeable bool c, Mergeable bool d) =>
    Mergeable bool (a, b, c, d)

deriving via
  (Default1 ((,,,) a b c))
  instance
    (SymBoolOp bool, Mergeable bool a, Mergeable bool b, Mergeable bool c) =>
    Mergeable1 bool ((,,,) a b c)

-- (,,,,)
deriving via
  (Default (a, b, c, d, e))
  instance
    (SymBoolOp bool, Mergeable bool a, Mergeable bool b, Mergeable bool c, Mergeable bool d, Mergeable bool e) =>
    Mergeable bool (a, b, c, d, e)

deriving via
  (Default1 ((,,,,) a b c d))
  instance
    (SymBoolOp bool, Mergeable bool a, Mergeable bool b, Mergeable bool c, Mergeable bool d) =>
    Mergeable1 bool ((,,,,) a b c d)

-- (,,,,,)
deriving via
  (Default (a, b, c, d, e, f))
  instance
    ( SymBoolOp bool,
      Mergeable bool a,
      Mergeable bool b,
      Mergeable bool c,
      Mergeable bool d,
      Mergeable bool e,
      Mergeable bool f
    ) =>
    Mergeable bool (a, b, c, d, e, f)

deriving via
  (Default1 ((,,,,,) a b c d e))
  instance
    (SymBoolOp bool, Mergeable bool a, Mergeable bool b, Mergeable bool c, Mergeable bool d, Mergeable bool e) =>
    Mergeable1 bool ((,,,,,) a b c d e)

-- (,,,,,,)
deriving via
  (Default (a, b, c, d, e, f, g))
  instance
    ( SymBoolOp bool,
      Mergeable bool a,
      Mergeable bool b,
      Mergeable bool c,
      Mergeable bool d,
      Mergeable bool e,
      Mergeable bool f,
      Mergeable bool g
    ) =>
    Mergeable bool (a, b, c, d, e, f, g)

deriving via
  (Default1 ((,,,,,,) a b c d e f))
  instance
    ( SymBoolOp bool,
      Mergeable bool a,
      Mergeable bool b,
      Mergeable bool c,
      Mergeable bool d,
      Mergeable bool e,
      Mergeable bool f
    ) =>
    Mergeable1 bool ((,,,,,,) a b c d e f)

-- (,,,,,,,)
deriving via
  (Default (a, b, c, d, e, f, g, h))
  instance
    ( SymBoolOp bool,
      Mergeable bool a,
      Mergeable bool b,
      Mergeable bool c,
      Mergeable bool d,
      Mergeable bool e,
      Mergeable bool f,
      Mergeable bool g,
      Mergeable bool h
    ) =>
    Mergeable bool (a, b, c, d, e, f, g, h)

deriving via
  (Default1 ((,,,,,,,) a b c d e f g))
  instance
    ( SymBoolOp bool,
      Mergeable bool a,
      Mergeable bool b,
      Mergeable bool c,
      Mergeable bool d,
      Mergeable bool e,
      Mergeable bool f,
      Mergeable bool g
    ) =>
    Mergeable1 bool ((,,,,,,,) a b c d e f g)

-- function
instance (SymBoolOp bool, Mergeable bool b) => Mergeable bool (a -> b) where
  mergingStrategy = case mergingStrategy @bool @b of
    SimpleStrategy m -> SimpleStrategy $ \cond t f v -> m cond (t v) (f v)
    _ -> NoStrategy
  {-# INLINE mergingStrategy #-}

instance (SymBoolOp bool) => Mergeable1 bool ((->) a) where
  liftMergingStrategy ms = case ms of
    SimpleStrategy m -> SimpleStrategy $ \cond t f v -> m cond (t v) (f v)
    _ -> NoStrategy
  {-# INLINE liftMergingStrategy #-}

-- MaybeT
instance (SymBoolOp bool, Mergeable1 bool m, Mergeable bool a) => Mergeable bool (MaybeT m a) where
  mergingStrategy = wrapStrategy mergingStrategy1 MaybeT runMaybeT
  {-# INLINE mergingStrategy #-}

instance (SymBoolOp bool, Mergeable1 bool m) => Mergeable1 bool (MaybeT m) where
  liftMergingStrategy m = wrapStrategy (liftMergingStrategy (liftMergingStrategy m)) MaybeT runMaybeT
  {-# INLINE liftMergingStrategy #-}

-- ExceptT
instance
  (SymBoolOp bool, Mergeable1 bool m, Mergeable bool e, Mergeable bool a) =>
  Mergeable bool (ExceptT e m a)
  where
  mergingStrategy = wrapStrategy mergingStrategy1 ExceptT runExceptT
  {-# INLINE mergingStrategy #-}

instance (SymBoolOp bool, Mergeable1 bool m, Mergeable bool e) => Mergeable1 bool (ExceptT e m) where
  liftMergingStrategy m = wrapStrategy (liftMergingStrategy (liftMergingStrategy m)) ExceptT runExceptT
  {-# INLINE liftMergingStrategy #-}

-- state
instance
  (SymBoolOp bool, Mergeable bool s, Mergeable bool a, Mergeable1 bool m) =>
  Mergeable bool (StateLazy.StateT s m a)
  where
  mergingStrategy = wrapStrategy (liftMergingStrategy mergingStrategy1) StateLazy.StateT StateLazy.runStateT
  {-# INLINE mergingStrategy #-}

instance (SymBoolOp bool, Mergeable bool s, Mergeable1 bool m) => Mergeable1 bool (StateLazy.StateT s m) where
  liftMergingStrategy m =
    wrapStrategy
      (liftMergingStrategy (liftMergingStrategy (liftMergingStrategy2 m mergingStrategy)))
      StateLazy.StateT
      StateLazy.runStateT
  {-# INLINE liftMergingStrategy #-}

instance
  (SymBoolOp bool, Mergeable bool s, Mergeable bool a, Mergeable1 bool m) =>
  Mergeable bool (StateStrict.StateT s m a)
  where
  mergingStrategy =
    wrapStrategy (liftMergingStrategy mergingStrategy1) StateStrict.StateT StateStrict.runStateT
  {-# INLINE mergingStrategy #-}

instance (SymBoolOp bool, Mergeable bool s, Mergeable1 bool m) => Mergeable1 bool (StateStrict.StateT s m) where
  liftMergingStrategy m =
    wrapStrategy
      (liftMergingStrategy (liftMergingStrategy (liftMergingStrategy2 m mergingStrategy)))
      StateStrict.StateT
      StateStrict.runStateT
  {-# INLINE liftMergingStrategy #-}

-- writer
instance
  (SymBoolOp bool, Mergeable bool s, Mergeable bool a, Mergeable1 bool m) =>
  Mergeable bool (WriterLazy.WriterT s m a)
  where
  mergingStrategy = wrapStrategy (liftMergingStrategy mergingStrategy1) WriterLazy.WriterT WriterLazy.runWriterT
  {-# INLINE mergingStrategy #-}

instance (SymBoolOp bool, Mergeable bool s, Mergeable1 bool m) => Mergeable1 bool (WriterLazy.WriterT s m) where
  liftMergingStrategy m =
    wrapStrategy
      (liftMergingStrategy (liftMergingStrategy2 m mergingStrategy))
      WriterLazy.WriterT
      WriterLazy.runWriterT
  {-# INLINE liftMergingStrategy #-}

instance
  (SymBoolOp bool, Mergeable bool s, Mergeable bool a, Mergeable1 bool m) =>
  Mergeable bool (WriterStrict.WriterT s m a)
  where
  mergingStrategy = wrapStrategy (liftMergingStrategy mergingStrategy1) WriterStrict.WriterT WriterStrict.runWriterT
  {-# INLINE mergingStrategy #-}

instance (SymBoolOp bool, Mergeable bool s, Mergeable1 bool m) => Mergeable1 bool (WriterStrict.WriterT s m) where
  liftMergingStrategy m =
    wrapStrategy
      (liftMergingStrategy (liftMergingStrategy2 m mergingStrategy))
      WriterStrict.WriterT
      WriterStrict.runWriterT
  {-# INLINE liftMergingStrategy #-}

-- reader
instance
  (SymBoolOp bool, Mergeable bool a, Mergeable1 bool m) =>
  Mergeable bool (ReaderT s m a)
  where
  mergingStrategy = wrapStrategy (liftMergingStrategy mergingStrategy1) ReaderT runReaderT
  {-# INLINE mergingStrategy #-}

instance (SymBoolOp bool, Mergeable1 bool m) => Mergeable1 bool (ReaderT s m) where
  liftMergingStrategy m =
    wrapStrategy
      (liftMergingStrategy (liftMergingStrategy m))
      ReaderT
      runReaderT
  {-# INLINE liftMergingStrategy #-}

-- Sum
instance
  (SymBoolOp bool, Mergeable1 bool l, Mergeable1 bool r, Mergeable bool x) =>
  Mergeable bool (Sum l r x)
  where
  mergingStrategy =
    SortedStrategy
      ( \case
          InL _ -> False
          InR _ -> True
      )
      ( \case
          False -> wrapStrategy mergingStrategy1 InL (\case (InL v) -> v; _ -> error "impossible")
          True -> wrapStrategy mergingStrategy1 InR (\case (InR v) -> v; _ -> error "impossible")
      )
  {-# INLINE mergingStrategy #-}

instance (SymBoolOp bool, Mergeable1 bool l, Mergeable1 bool r) => Mergeable1 bool (Sum l r) where
  liftMergingStrategy m =
    SortedStrategy
      ( \case
          InL _ -> False
          InR _ -> True
      )
      ( \case
          False -> wrapStrategy (liftMergingStrategy m) InL (\case (InL v) -> v; _ -> error "impossible")
          True -> wrapStrategy (liftMergingStrategy m) InR (\case (InR v) -> v; _ -> error "impossible")
      )
  {-# INLINE liftMergingStrategy #-}

-- Ordering
deriving via
  (Default Ordering)
  instance
    (SymBoolOp bool) => Mergeable bool Ordering

-- Generic
deriving via
  (Default (U1 x))
  instance
    (SymBoolOp bool) => Mergeable bool (U1 x)

deriving via
  (Default (V1 x))
  instance
    (SymBoolOp bool) => Mergeable bool (V1 x)

deriving via
  (Default (K1 i c x))
  instance
    (SymBoolOp bool, Mergeable bool c) => Mergeable bool (K1 i c x)

deriving via
  (Default (M1 i c a x))
  instance
    (SymBoolOp bool, Mergeable bool (a x)) => Mergeable bool (M1 i c a x)

deriving via
  (Default ((a :+: b) x))
  instance
    (SymBoolOp bool, Mergeable bool (a x), Mergeable bool (b x)) => Mergeable bool ((a :+: b) x)

deriving via
  (Default ((a :*: b) x))
  instance
    (SymBoolOp bool, Mergeable bool (a x), Mergeable bool (b x)) => Mergeable bool ((a :*: b) x)

-- Identity
instance (SymBoolOp bool, Mergeable bool a) => Mergeable bool (Identity a) where
  mergingStrategy = wrapStrategy mergingStrategy Identity runIdentity
  {-# INLINE mergingStrategy #-}

instance SymBoolOp bool => Mergeable1 bool Identity where
  liftMergingStrategy m = wrapStrategy m Identity runIdentity
  {-# INLINE liftMergingStrategy #-}

-- IdentityT
instance (SymBoolOp bool, Mergeable1 bool m, Mergeable bool a) => Mergeable bool (IdentityT m a) where
  mergingStrategy = wrapStrategy mergingStrategy1 IdentityT runIdentityT
  {-# INLINE mergingStrategy #-}

instance (SymBoolOp bool, Mergeable1 bool m) => Mergeable1 bool (IdentityT m) where
  liftMergingStrategy m = wrapStrategy (liftMergingStrategy m) IdentityT runIdentityT
  {-# INLINE liftMergingStrategy #-}

-- ContT
instance (SymBoolOp bool, Mergeable1 bool m, Mergeable bool r) => Mergeable bool (ContT r m a) where
  mergingStrategy =
    wrapStrategy
      (liftMergingStrategy mergingStrategy1)
      ContT
      (\(ContT v) -> v)
  {-# INLINE mergingStrategy #-}

instance (SymBoolOp bool, Mergeable1 bool m, Mergeable bool r) => Mergeable1 bool (ContT r m) where
  liftMergingStrategy _ =
    wrapStrategy
      (liftMergingStrategy mergingStrategy1)
      ContT
      (\(ContT v) -> v)
  {-# INLINE liftMergingStrategy #-}

-- RWS
instance
  (SymBoolOp bool, Mergeable bool s, Mergeable bool w, Mergeable bool a, Mergeable1 bool m) =>
  Mergeable bool (RWSLazy.RWST r w s m a)
  where
  mergingStrategy = wrapStrategy (liftMergingStrategy (liftMergingStrategy mergingStrategy1)) RWSLazy.RWST (\(RWSLazy.RWST m) -> m)
  {-# INLINE mergingStrategy #-}

instance
  (SymBoolOp bool, Mergeable bool s, Mergeable bool w, Mergeable1 bool m) =>
  Mergeable1 bool (RWSLazy.RWST r w s m)
  where
  liftMergingStrategy m =
    wrapStrategy
      (liftMergingStrategy (liftMergingStrategy (liftMergingStrategy (liftMergingStrategy3 m mergingStrategy mergingStrategy))))
      RWSLazy.RWST
      (\(RWSLazy.RWST rws) -> rws)
  {-# INLINE liftMergingStrategy #-}

instance
  (SymBoolOp bool, Mergeable bool s, Mergeable bool w, Mergeable bool a, Mergeable1 bool m) =>
  Mergeable bool (RWSStrict.RWST r w s m a)
  where
  mergingStrategy = wrapStrategy (liftMergingStrategy (liftMergingStrategy mergingStrategy1)) RWSStrict.RWST (\(RWSStrict.RWST m) -> m)
  {-# INLINE mergingStrategy #-}

instance
  (SymBoolOp bool, Mergeable bool s, Mergeable bool w, Mergeable1 bool m) =>
  Mergeable1 bool (RWSStrict.RWST r w s m)
  where
  liftMergingStrategy m =
    wrapStrategy
      (liftMergingStrategy (liftMergingStrategy (liftMergingStrategy (liftMergingStrategy3 m mergingStrategy mergingStrategy))))
      RWSStrict.RWST
      (\(RWSStrict.RWST rws) -> rws)
  {-# INLINE liftMergingStrategy #-}

-- Data.Monoid module
deriving via
  (Default (Monoid.Sum a))
  instance
    (Mergeable bool a) => Mergeable bool (Monoid.Sum a)

deriving via (Default1 Monoid.Sum) instance Mergeable1 bool Monoid.Sum
