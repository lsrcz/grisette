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
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.Class.Mergeable
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
    Mergeable' (..),
    derivedRootStrategy,

    -- * Combinators for manually building merging strategies
    wrapStrategy,
    product2Strategy,
    DynamicSortedIdx (..),
    StrategyList (..),
    buildStrategyList,
    resolveStrategy,
    resolveStrategy',
  )
where

import Control.Exception
  ( ArithException
      ( Denormal,
        DivideByZero,
        LossOfPrecision,
        Overflow,
        RatioZeroDenominator,
        Underflow
      ),
  )
import Control.Monad.Cont (ContT (ContT))
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.Identity
  ( Identity (Identity, runIdentity),
    IdentityT (IdentityT, runIdentityT),
  )
import qualified Control.Monad.RWS.Lazy as RWSLazy
import qualified Control.Monad.RWS.Strict as RWSStrict
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT))
import qualified Control.Monad.State.Lazy as StateLazy
import qualified Control.Monad.State.Strict as StateStrict
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.ByteString as B
import Data.Functor.Classes
  ( Eq1,
    Ord1,
    Show1,
    compare1,
    eq1,
    showsPrec1,
  )
import Data.Functor.Sum (Sum (InL, InR))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import qualified Data.Monoid as Monoid
import qualified Data.Text as T
import Data.Typeable
  ( Proxy (Proxy),
    Typeable,
    eqT,
    type (:~:) (Refl),
  )
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import GHC.TypeNats (KnownNat, natVal, type (<=))
import Generics.Deriving
  ( Default (Default),
    Default1 (Default1),
    Generic (Rep, from, to),
    Generic1 (Rep1, from1, to1),
    K1 (K1, unK1),
    M1 (M1, unM1),
    Par1 (Par1, unPar1),
    Rec1 (Rec1, unRec1),
    U1,
    V1,
    type (:*:) ((:*:)),
    type (:+:) (L1, R1),
  )
import Grisette.Core.Control.Exception (AssertionError, VerificationConditions)
import Grisette.Core.Data.BV
  ( BitwidthMismatch,
    IntN (IntN),
    SomeIntN (SomeIntN),
    SomeWordN (SomeWordN),
    WordN (WordN),
  )
import Grisette.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
  ( LinkedRep,
    SupportedPrim,
  )
import Grisette.IR.SymPrim.Data.SymPrim
  ( SomeSymIntN (SomeSymIntN),
    SomeSymWordN (SomeSymWordN),
    SymBool,
    SymIntN,
    SymInteger,
    SymWordN,
    type (-~>),
    type (=~>),
  )
import Grisette.Utils.Parameterized (unsafeAxiom)
import Unsafe.Coerce (unsafeCoerce)

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

-- | Resolves the indices and the terminal merge strategy for a value of some 'Mergeable' type.
resolveStrategy :: forall x. MergingStrategy x -> x -> ([DynamicSortedIdx], MergingStrategy x)
resolveStrategy s x = resolveStrategy' x s
{-# INLINE resolveStrategy #-}

-- | Resolves the indices and the terminal merge strategy for a value given a merge strategy for its type.
resolveStrategy' :: forall x. x -> MergingStrategy x -> ([DynamicSortedIdx], MergingStrategy x)
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
--      indexing with 'Data.Maybe.isJust', the 'Nothing' and 'Just' values can then
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

-- | Lifting of the 'Mergeable' class to unary type constructors.
class Mergeable1 (u :: Type -> Type) where
  -- | Lift merge strategy through the type constructor.
  liftRootStrategy :: MergingStrategy a -> MergingStrategy (u a)

-- | Lift the root merge strategy through the unary type constructor.
rootStrategy1 :: (Mergeable a, Mergeable1 u) => MergingStrategy (u a)
rootStrategy1 = liftRootStrategy rootStrategy
{-# INLINE rootStrategy1 #-}

-- | Lifting of the 'Mergeable' class to binary type constructors.
class Mergeable2 (u :: Type -> Type -> Type) where
  -- | Lift merge strategy through the type constructor.
  liftRootStrategy2 :: MergingStrategy a -> MergingStrategy b -> MergingStrategy (u a b)

-- | Lift the root merge strategy through the binary type constructor.
rootStrategy2 :: (Mergeable a, Mergeable b, Mergeable2 u) => MergingStrategy (u a b)
rootStrategy2 = liftRootStrategy2 rootStrategy rootStrategy
{-# INLINE rootStrategy2 #-}

-- | Lifting of the 'Mergeable' class to ternary type constructors.
class Mergeable3 (u :: Type -> Type -> Type -> Type) where
  -- | Lift merge strategy through the type constructor.
  liftRootStrategy3 :: MergingStrategy a -> MergingStrategy b -> MergingStrategy c -> MergingStrategy (u a b c)

-- | Lift the root merge strategy through the binary type constructor.
rootStrategy3 :: (Mergeable a, Mergeable b, Mergeable c, Mergeable3 u) => MergingStrategy (u a b c)
rootStrategy3 = liftRootStrategy3 rootStrategy rootStrategy rootStrategy
{-# INLINE rootStrategy3 #-}

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
      SortedStrategy (idxf . snd . unwrap) (product2Strategy wrap unwrap s1 . subf)
    (SortedStrategy idxf subf, s2) ->
      SortedStrategy (idxf . fst . unwrap) (\idx -> product2Strategy wrap unwrap (subf idx) s2)
{-# INLINE product2Strategy #-}

instance (Mergeable' a, Mergeable' b) => Mergeable' (a :*: b) where
  rootStrategy' = product2Strategy (:*:) (\(a :*: b) -> (a, b)) rootStrategy' rootStrategy'
  {-# INLINE rootStrategy' #-}

-- instances

#define CONCRETE_ORD_MERGEABLE(type) \
instance Mergeable type where \
  rootStrategy = \
    let sub = SimpleStrategy $ \_ t _ -> t \
     in SortedStrategy id $ const sub

#define CONCRETE_ORD_MERGEABLE_BV(type) \
instance (KnownNat n, 1 <= n) => Mergeable (type n) where \
  rootStrategy = \
    let sub = SimpleStrategy $ \_ t _ -> t \
     in SortedStrategy id $ const sub

#if 1
CONCRETE_ORD_MERGEABLE(Bool)
CONCRETE_ORD_MERGEABLE(Integer)
CONCRETE_ORD_MERGEABLE(Char)
CONCRETE_ORD_MERGEABLE(Int)
CONCRETE_ORD_MERGEABLE(Int8)
CONCRETE_ORD_MERGEABLE(Int16)
CONCRETE_ORD_MERGEABLE(Int32)
CONCRETE_ORD_MERGEABLE(Int64)
CONCRETE_ORD_MERGEABLE(Word)
CONCRETE_ORD_MERGEABLE(Word8)
CONCRETE_ORD_MERGEABLE(Word16)
CONCRETE_ORD_MERGEABLE(Word32)
CONCRETE_ORD_MERGEABLE(Word64)
CONCRETE_ORD_MERGEABLE(B.ByteString)
CONCRETE_ORD_MERGEABLE(T.Text)
CONCRETE_ORD_MERGEABLE_BV(WordN)
CONCRETE_ORD_MERGEABLE_BV(IntN)
#endif

instance Mergeable SomeIntN where
  rootStrategy =
    SortedStrategy @Natural
      (\(SomeIntN (_ :: IntN n)) -> natVal (Proxy @n))
      ( \_ ->
          SortedStrategy @Integer
            (\(SomeIntN (IntN i)) -> i)
            (const $ SimpleStrategy $ \_ l _ -> l)
      )

instance Mergeable SomeWordN where
  rootStrategy =
    SortedStrategy @Natural
      (\(SomeWordN (_ :: WordN n)) -> natVal (Proxy @n))
      ( \_ ->
          SortedStrategy @Integer
            (\(SomeWordN (WordN i)) -> i)
            (const $ SimpleStrategy $ \_ l _ -> l)
      )

-- ()
deriving via (Default ()) instance Mergeable ()

-- Either
deriving via (Default (Either e a)) instance (Mergeable e, Mergeable a) => Mergeable (Either e a)

deriving via (Default1 (Either e)) instance (Mergeable e) => Mergeable1 (Either e)

instance Mergeable2 Either where
  liftRootStrategy2 m1 m2 =
    SortedStrategy
      ( \case
          Left _ -> False
          Right _ -> True
      )
      ( \case
          False -> wrapStrategy m1 Left (\case (Left v) -> v; _ -> undefined)
          True -> wrapStrategy m2 Right (\case (Right v) -> v; _ -> undefined)
      )
  {-# INLINE liftRootStrategy2 #-}

-- Maybe
deriving via (Default (Maybe a)) instance (Mergeable a) => Mergeable (Maybe a)

deriving via (Default1 Maybe) instance Mergeable1 Maybe

-- | Helper type for building efficient merge strategy for list-like containers.
data StrategyList container where
  StrategyList ::
    forall a container.
    container [DynamicSortedIdx] ->
    container (MergingStrategy a) ->
    StrategyList container

-- | Helper function for building efficient merge strategy for list-like containers.
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

-- List
instance (Mergeable a) => Mergeable [a] where
  rootStrategy = case rootStrategy :: MergingStrategy a of
    SimpleStrategy m ->
      SortedStrategy length $ \_ ->
        SimpleStrategy $ \cond -> zipWith (m cond)
    NoStrategy ->
      SortedStrategy length $ const NoStrategy
    _ -> SortedStrategy length $ \_ ->
      SortedStrategy (buildStrategyList rootStrategy) $ \(StrategyList _ strategies) ->
        let s :: [MergingStrategy a] = unsafeCoerce strategies
            allSimple = all (\case SimpleStrategy _ -> True; _ -> False) s
         in if allSimple
              then SimpleStrategy $ \cond l r ->
                (\case (SimpleStrategy f, l1, r1) -> f cond l1 r1; _ -> error "impossible") <$> zip3 s l r
              else NoStrategy
  {-# INLINE rootStrategy #-}

instance Mergeable1 [] where
  liftRootStrategy (ms :: MergingStrategy a) = case ms of
    SimpleStrategy m ->
      SortedStrategy length $ \_ ->
        SimpleStrategy $ \cond -> zipWith (m cond)
    NoStrategy ->
      SortedStrategy length $ const NoStrategy
    _ -> SortedStrategy length $ \_ ->
      SortedStrategy (buildStrategyList ms) $ \(StrategyList _ strategies) ->
        let s :: [MergingStrategy a] = unsafeCoerce strategies
            allSimple = all (\case SimpleStrategy _ -> True; _ -> False) s
         in if allSimple
              then SimpleStrategy $ \cond l r ->
                (\case (SimpleStrategy f, l1, r1) -> f cond l1 r1; _ -> error "impossible") <$> zip3 s l r
              else NoStrategy
  {-# INLINE liftRootStrategy #-}

-- (,)
deriving via (Default (a, b)) instance (Mergeable a, Mergeable b) => Mergeable (a, b)

deriving via (Default1 ((,) a)) instance (Mergeable a) => Mergeable1 ((,) a)

instance Mergeable2 (,) where
  liftRootStrategy2 = product2Strategy (,) id
  {-# INLINE liftRootStrategy2 #-}

-- (,,)
deriving via
  (Default (a, b, c))
  instance
    (Mergeable a, Mergeable b, Mergeable c) => Mergeable (a, b, c)

deriving via
  (Default1 ((,,) a b))
  instance
    (Mergeable a, Mergeable b) => Mergeable1 ((,,) a b)

instance (Mergeable a) => Mergeable2 ((,,) a) where
  liftRootStrategy2 = liftRootStrategy3 rootStrategy
  {-# INLINE liftRootStrategy2 #-}

instance Mergeable3 (,,) where
  liftRootStrategy3 m1 m2 m3 =
    product2Strategy
      (\a (b, c) -> (a, b, c))
      (\(a, b, c) -> (a, (b, c)))
      m1
      (liftRootStrategy2 m2 m3)
  {-# INLINE liftRootStrategy3 #-}

-- (,,,)
deriving via
  (Default (a, b, c, d))
  instance
    (Mergeable a, Mergeable b, Mergeable c, Mergeable d) =>
    Mergeable (a, b, c, d)

deriving via
  (Default1 ((,,,) a b c))
  instance
    (Mergeable a, Mergeable b, Mergeable c) =>
    Mergeable1 ((,,,) a b c)

-- (,,,,)
deriving via
  (Default (a, b, c, d, e))
  instance
    (Mergeable a, Mergeable b, Mergeable c, Mergeable d, Mergeable e) =>
    Mergeable (a, b, c, d, e)

deriving via
  (Default1 ((,,,,) a b c d))
  instance
    (Mergeable a, Mergeable b, Mergeable c, Mergeable d) =>
    Mergeable1 ((,,,,) a b c d)

-- (,,,,,)
deriving via
  (Default (a, b, c, d, e, f))
  instance
    ( Mergeable a,
      Mergeable b,
      Mergeable c,
      Mergeable d,
      Mergeable e,
      Mergeable f
    ) =>
    Mergeable (a, b, c, d, e, f)

deriving via
  (Default1 ((,,,,,) a b c d e))
  instance
    (Mergeable a, Mergeable b, Mergeable c, Mergeable d, Mergeable e) =>
    Mergeable1 ((,,,,,) a b c d e)

-- (,,,,,,)
deriving via
  (Default (a, b, c, d, e, f, g))
  instance
    ( Mergeable a,
      Mergeable b,
      Mergeable c,
      Mergeable d,
      Mergeable e,
      Mergeable f,
      Mergeable g
    ) =>
    Mergeable (a, b, c, d, e, f, g)

deriving via
  (Default1 ((,,,,,,) a b c d e f))
  instance
    ( Mergeable a,
      Mergeable b,
      Mergeable c,
      Mergeable d,
      Mergeable e,
      Mergeable f
    ) =>
    Mergeable1 ((,,,,,,) a b c d e f)

-- (,,,,,,,)
deriving via
  (Default (a, b, c, d, e, f, g, h))
  instance
    ( Mergeable a,
      Mergeable b,
      Mergeable c,
      Mergeable d,
      Mergeable e,
      Mergeable f,
      Mergeable g,
      Mergeable h
    ) =>
    Mergeable (a, b, c, d, e, f, g, h)

deriving via
  (Default1 ((,,,,,,,) a b c d e f g))
  instance
    ( Mergeable a,
      Mergeable b,
      Mergeable c,
      Mergeable d,
      Mergeable e,
      Mergeable f,
      Mergeable g
    ) =>
    Mergeable1 ((,,,,,,,) a b c d e f g)

-- function
instance (Mergeable b) => Mergeable (a -> b) where
  rootStrategy = case rootStrategy @b of
    SimpleStrategy m -> SimpleStrategy $ \cond t f v -> m cond (t v) (f v)
    _ -> NoStrategy
  {-# INLINE rootStrategy #-}

instance Mergeable1 ((->) a) where
  liftRootStrategy ms = case ms of
    SimpleStrategy m -> SimpleStrategy $ \cond t f v -> m cond (t v) (f v)
    _ -> NoStrategy
  {-# INLINE liftRootStrategy #-}

-- MaybeT
instance (Mergeable1 m, Mergeable a) => Mergeable (MaybeT m a) where
  rootStrategy = wrapStrategy rootStrategy1 MaybeT runMaybeT
  {-# INLINE rootStrategy #-}

instance (Mergeable1 m) => Mergeable1 (MaybeT m) where
  liftRootStrategy m = wrapStrategy (liftRootStrategy (liftRootStrategy m)) MaybeT runMaybeT
  {-# INLINE liftRootStrategy #-}

-- ExceptT
instance
  (Mergeable1 m, Mergeable e, Mergeable a) =>
  Mergeable (ExceptT e m a)
  where
  rootStrategy = wrapStrategy rootStrategy1 ExceptT runExceptT
  {-# INLINE rootStrategy #-}

instance (Mergeable1 m, Mergeable e) => Mergeable1 (ExceptT e m) where
  liftRootStrategy m = wrapStrategy (liftRootStrategy (liftRootStrategy m)) ExceptT runExceptT
  {-# INLINE liftRootStrategy #-}

-- state
instance
  (Mergeable s, Mergeable a, Mergeable1 m) =>
  Mergeable (StateLazy.StateT s m a)
  where
  rootStrategy = wrapStrategy (liftRootStrategy rootStrategy1) StateLazy.StateT StateLazy.runStateT
  {-# INLINE rootStrategy #-}

instance (Mergeable s, Mergeable1 m) => Mergeable1 (StateLazy.StateT s m) where
  liftRootStrategy m =
    wrapStrategy
      (liftRootStrategy (liftRootStrategy (liftRootStrategy2 m rootStrategy)))
      StateLazy.StateT
      StateLazy.runStateT
  {-# INLINE liftRootStrategy #-}

instance
  (Mergeable s, Mergeable a, Mergeable1 m) =>
  Mergeable (StateStrict.StateT s m a)
  where
  rootStrategy =
    wrapStrategy (liftRootStrategy rootStrategy1) StateStrict.StateT StateStrict.runStateT
  {-# INLINE rootStrategy #-}

instance (Mergeable s, Mergeable1 m) => Mergeable1 (StateStrict.StateT s m) where
  liftRootStrategy m =
    wrapStrategy
      (liftRootStrategy (liftRootStrategy (liftRootStrategy2 m rootStrategy)))
      StateStrict.StateT
      StateStrict.runStateT
  {-# INLINE liftRootStrategy #-}

-- writer
instance
  (Mergeable s, Mergeable a, Mergeable1 m) =>
  Mergeable (WriterLazy.WriterT s m a)
  where
  rootStrategy = wrapStrategy (liftRootStrategy rootStrategy1) WriterLazy.WriterT WriterLazy.runWriterT
  {-# INLINE rootStrategy #-}

instance (Mergeable s, Mergeable1 m) => Mergeable1 (WriterLazy.WriterT s m) where
  liftRootStrategy m =
    wrapStrategy
      (liftRootStrategy (liftRootStrategy2 m rootStrategy))
      WriterLazy.WriterT
      WriterLazy.runWriterT
  {-# INLINE liftRootStrategy #-}

instance
  (Mergeable s, Mergeable a, Mergeable1 m) =>
  Mergeable (WriterStrict.WriterT s m a)
  where
  rootStrategy = wrapStrategy (liftRootStrategy rootStrategy1) WriterStrict.WriterT WriterStrict.runWriterT
  {-# INLINE rootStrategy #-}

instance (Mergeable s, Mergeable1 m) => Mergeable1 (WriterStrict.WriterT s m) where
  liftRootStrategy m =
    wrapStrategy
      (liftRootStrategy (liftRootStrategy2 m rootStrategy))
      WriterStrict.WriterT
      WriterStrict.runWriterT
  {-# INLINE liftRootStrategy #-}

-- reader
instance
  (Mergeable a, Mergeable1 m) =>
  Mergeable (ReaderT s m a)
  where
  rootStrategy = wrapStrategy (liftRootStrategy rootStrategy1) ReaderT runReaderT
  {-# INLINE rootStrategy #-}

instance (Mergeable1 m) => Mergeable1 (ReaderT s m) where
  liftRootStrategy m =
    wrapStrategy
      (liftRootStrategy (liftRootStrategy m))
      ReaderT
      runReaderT
  {-# INLINE liftRootStrategy #-}

-- Sum
instance
  (Mergeable1 l, Mergeable1 r, Mergeable x) =>
  Mergeable (Sum l r x)
  where
  rootStrategy =
    SortedStrategy
      ( \case
          InL _ -> False
          InR _ -> True
      )
      ( \case
          False -> wrapStrategy rootStrategy1 InL (\case (InL v) -> v; _ -> error "impossible")
          True -> wrapStrategy rootStrategy1 InR (\case (InR v) -> v; _ -> error "impossible")
      )
  {-# INLINE rootStrategy #-}

instance (Mergeable1 l, Mergeable1 r) => Mergeable1 (Sum l r) where
  liftRootStrategy m =
    SortedStrategy
      ( \case
          InL _ -> False
          InR _ -> True
      )
      ( \case
          False -> wrapStrategy (liftRootStrategy m) InL (\case (InL v) -> v; _ -> error "impossible")
          True -> wrapStrategy (liftRootStrategy m) InR (\case (InR v) -> v; _ -> error "impossible")
      )
  {-# INLINE liftRootStrategy #-}

-- Ordering
deriving via
  (Default Ordering)
  instance
    Mergeable Ordering

-- Generic
deriving via
  (Default (U1 x))
  instance
    Mergeable (U1 x)

deriving via
  (Default (V1 x))
  instance
    Mergeable (V1 x)

deriving via
  (Default (K1 i c x))
  instance
    (Mergeable c) => Mergeable (K1 i c x)

deriving via
  (Default (M1 i c a x))
  instance
    (Mergeable (a x)) => Mergeable (M1 i c a x)

deriving via
  (Default ((a :+: b) x))
  instance
    (Mergeable (a x), Mergeable (b x)) => Mergeable ((a :+: b) x)

deriving via
  (Default ((a :*: b) x))
  instance
    (Mergeable (a x), Mergeable (b x)) => Mergeable ((a :*: b) x)

-- Identity
instance (Mergeable a) => Mergeable (Identity a) where
  rootStrategy = wrapStrategy rootStrategy Identity runIdentity
  {-# INLINE rootStrategy #-}

instance Mergeable1 Identity where
  liftRootStrategy m = wrapStrategy m Identity runIdentity
  {-# INLINE liftRootStrategy #-}

-- IdentityT
instance (Mergeable1 m, Mergeable a) => Mergeable (IdentityT m a) where
  rootStrategy = wrapStrategy rootStrategy1 IdentityT runIdentityT
  {-# INLINE rootStrategy #-}

instance (Mergeable1 m) => Mergeable1 (IdentityT m) where
  liftRootStrategy m = wrapStrategy (liftRootStrategy m) IdentityT runIdentityT
  {-# INLINE liftRootStrategy #-}

-- ContT
instance (Mergeable1 m, Mergeable r) => Mergeable (ContT r m a) where
  rootStrategy =
    wrapStrategy
      (liftRootStrategy rootStrategy1)
      ContT
      (\(ContT v) -> v)
  {-# INLINE rootStrategy #-}

instance (Mergeable1 m, Mergeable r) => Mergeable1 (ContT r m) where
  liftRootStrategy _ =
    wrapStrategy
      (liftRootStrategy rootStrategy1)
      ContT
      (\(ContT v) -> v)
  {-# INLINE liftRootStrategy #-}

-- RWS
instance
  (Mergeable s, Mergeable w, Mergeable a, Mergeable1 m) =>
  Mergeable (RWSLazy.RWST r w s m a)
  where
  rootStrategy = wrapStrategy (liftRootStrategy (liftRootStrategy rootStrategy1)) RWSLazy.RWST (\(RWSLazy.RWST m) -> m)
  {-# INLINE rootStrategy #-}

instance
  (Mergeable s, Mergeable w, Mergeable1 m) =>
  Mergeable1 (RWSLazy.RWST r w s m)
  where
  liftRootStrategy m =
    wrapStrategy
      (liftRootStrategy (liftRootStrategy (liftRootStrategy (liftRootStrategy3 m rootStrategy rootStrategy))))
      RWSLazy.RWST
      (\(RWSLazy.RWST rws) -> rws)
  {-# INLINE liftRootStrategy #-}

instance
  (Mergeable s, Mergeable w, Mergeable a, Mergeable1 m) =>
  Mergeable (RWSStrict.RWST r w s m a)
  where
  rootStrategy = wrapStrategy (liftRootStrategy (liftRootStrategy rootStrategy1)) RWSStrict.RWST (\(RWSStrict.RWST m) -> m)
  {-# INLINE rootStrategy #-}

instance
  (Mergeable s, Mergeable w, Mergeable1 m) =>
  Mergeable1 (RWSStrict.RWST r w s m)
  where
  liftRootStrategy m =
    wrapStrategy
      (liftRootStrategy (liftRootStrategy (liftRootStrategy (liftRootStrategy3 m rootStrategy rootStrategy))))
      RWSStrict.RWST
      (\(RWSStrict.RWST rws) -> rws)
  {-# INLINE liftRootStrategy #-}

-- Data.Monoid module
deriving via
  (Default (Monoid.Sum a))
  instance
    (Mergeable a) => Mergeable (Monoid.Sum a)

deriving via (Default1 Monoid.Sum) instance Mergeable1 Monoid.Sum

#define MERGEABLE_SIMPLE(symtype) \
instance Mergeable symtype where \
  rootStrategy = SimpleStrategy symIte

#define MERGEABLE_BV(symtype) \
instance (KnownNat n, 1 <= n) => Mergeable (symtype n) where \
  rootStrategy = SimpleStrategy symIte

#define MERGEABLE_FUN(op) \
instance (SupportedPrim ca, SupportedPrim cb, LinkedRep ca sa, LinkedRep cb sb) => Mergeable (sa op sb) where \
  rootStrategy = SimpleStrategy symIte

#if 1
MERGEABLE_SIMPLE(SymBool)
MERGEABLE_SIMPLE(SymInteger)
MERGEABLE_BV(SymIntN)
MERGEABLE_BV(SymWordN)
MERGEABLE_FUN(=~>)
MERGEABLE_FUN(-~>)
#endif

instance Mergeable SomeSymIntN where
  rootStrategy =
    SortedStrategy @Natural
      (\(SomeSymIntN (_ :: SymIntN n)) -> natVal (Proxy @n))
      ( \_ ->
          SimpleStrategy
            ( \c (SomeSymIntN (l :: SymIntN l)) (SomeSymIntN (r :: SymIntN r)) ->
                case unsafeAxiom @l @r of
                  Refl -> SomeSymIntN $ symIte c l r
            )
      )

instance Mergeable SomeSymWordN where
  rootStrategy =
    SortedStrategy @Natural
      (\(SomeSymWordN (_ :: SymWordN n)) -> natVal (Proxy @n))
      ( \_ ->
          SimpleStrategy
            ( \c (SomeSymWordN (l :: SymWordN l)) (SomeSymWordN (r :: SymWordN r)) ->
                case unsafeAxiom @l @r of
                  Refl -> SomeSymWordN $ symIte c l r
            )
      )

-- Exceptions
instance Mergeable ArithException where
  rootStrategy =
    SortedStrategy
      ( \case
          Overflow -> 0 :: Int
          Underflow -> 1 :: Int
          LossOfPrecision -> 2 :: Int
          DivideByZero -> 3 :: Int
          Denormal -> 4 :: Int
          RatioZeroDenominator -> 5 :: Int
      )
      (const $ SimpleStrategy $ \_ l _ -> l)

deriving via (Default BitwidthMismatch) instance (Mergeable BitwidthMismatch)

deriving via (Default AssertionError) instance Mergeable AssertionError

deriving via (Default VerificationConditions) instance Mergeable VerificationConditions

instance (Generic a, Mergeable' (Rep a)) => Mergeable (Default a) where
  rootStrategy = unsafeCoerce (derivedRootStrategy :: MergingStrategy a)
  {-# NOINLINE rootStrategy #-}

-- | Generic derivation for the 'Mergeable' class.
--
-- Usually you can derive the merging strategy with the @DerivingVia@ and
-- @DerivingStrategies@ extension.
--
-- > data X = ... deriving (Generic) deriving Mergeable via (Default X)
derivedRootStrategy :: (Generic a, Mergeable' (Rep a)) => MergingStrategy a
derivedRootStrategy = wrapStrategy rootStrategy' to from
{-# INLINE derivedRootStrategy #-}

instance (Generic1 u, Mergeable1' (Rep1 u)) => Mergeable1 (Default1 u) where
  liftRootStrategy = unsafeCoerce (derivedLiftMergingStrategy :: MergingStrategy a -> MergingStrategy (u a))
  {-# NOINLINE liftRootStrategy #-}

class Mergeable1' (u :: Type -> Type) where
  liftRootStrategy' :: MergingStrategy a -> MergingStrategy (u a)

instance Mergeable1' U1 where
  liftRootStrategy' _ = SimpleStrategy (\_ t _ -> t)
  {-# INLINE liftRootStrategy' #-}

instance Mergeable1' V1 where
  liftRootStrategy' _ = SimpleStrategy (\_ t _ -> t)
  {-# INLINE liftRootStrategy' #-}

instance Mergeable1' Par1 where
  liftRootStrategy' m = wrapStrategy m Par1 unPar1
  {-# INLINE liftRootStrategy' #-}

instance (Mergeable1 f) => Mergeable1' (Rec1 f) where
  liftRootStrategy' m = wrapStrategy (liftRootStrategy m) Rec1 unRec1
  {-# INLINE liftRootStrategy' #-}

instance (Mergeable c) => Mergeable1' (K1 i c) where
  liftRootStrategy' _ = wrapStrategy rootStrategy K1 unK1
  {-# INLINE liftRootStrategy' #-}

instance (Mergeable1' a) => Mergeable1' (M1 i c a) where
  liftRootStrategy' m = wrapStrategy (liftRootStrategy' m) M1 unM1
  {-# INLINE liftRootStrategy' #-}

instance (Mergeable1' a, Mergeable1' b) => Mergeable1' (a :+: b) where
  liftRootStrategy' m =
    SortedStrategy
      ( \case
          L1 _ -> False
          R1 _ -> True
      )
      ( \idx ->
          if not idx
            then wrapStrategy (liftRootStrategy' m) L1 (\case (L1 v) -> v; _ -> error "impossible")
            else wrapStrategy (liftRootStrategy' m) R1 (\case (R1 v) -> v; _ -> error "impossible")
      )
  {-# INLINE liftRootStrategy' #-}

instance (Mergeable1' a, Mergeable1' b) => Mergeable1' (a :*: b) where
  liftRootStrategy' m = product2Strategy (:*:) (\(a :*: b) -> (a, b)) (liftRootStrategy' m) (liftRootStrategy' m)
  {-# INLINE liftRootStrategy' #-}

-- | Generic derivation for the 'Mergeable' class.
derivedLiftMergingStrategy :: (Generic1 u, Mergeable1' (Rep1 u)) => MergingStrategy a -> MergingStrategy (u a)
derivedLiftMergingStrategy m = wrapStrategy (liftRootStrategy' m) to1 from1
{-# INLINE derivedLiftMergingStrategy #-}

-- | Auxiliary class for the generic derivation for the 'Mergeable' class.
class Mergeable' f where
  rootStrategy' :: MergingStrategy (f a)

instance Mergeable' U1 where
  rootStrategy' = SimpleStrategy (\_ t _ -> t)
  {-# INLINE rootStrategy' #-}

instance Mergeable' V1 where
  rootStrategy' = SimpleStrategy (\_ t _ -> t)
  {-# INLINE rootStrategy' #-}

instance (Mergeable c) => Mergeable' (K1 i c) where
  rootStrategy' = wrapStrategy rootStrategy K1 unK1
  {-# INLINE rootStrategy' #-}

instance (Mergeable' a) => Mergeable' (M1 i c a) where
  rootStrategy' = wrapStrategy rootStrategy' M1 unM1
  {-# INLINE rootStrategy' #-}

instance (Mergeable' a, Mergeable' b) => Mergeable' (a :+: b) where
  rootStrategy' =
    SortedStrategy
      ( \case
          L1 _ -> False
          R1 _ -> True
      )
      ( \idx ->
          if not idx
            then wrapStrategy rootStrategy' L1 (\case (L1 v) -> v; _ -> undefined)
            else wrapStrategy rootStrategy' R1 (\case (R1 v) -> v; _ -> undefined)
      )
  {-# INLINE rootStrategy' #-}
