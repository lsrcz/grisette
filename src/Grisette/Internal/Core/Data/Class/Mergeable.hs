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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.Mergeable
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.Mergeable
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
  ( Identity (Identity),
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
  ( Typeable,
    eqT,
    type (:~:) (Refl),
  )
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeNats (KnownNat, type (+), type (<=))
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
    (:.:) (Comp1, unComp1),
    type (:*:) ((:*:)),
    type (:+:) (L1, R1),
  )
import Grisette.Internal.Core.Control.Exception (AssertionError, VerificationConditions)
import Grisette.Internal.Core.Data.Class.BitCast (BitCast (bitCast))
import Grisette.Internal.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Internal.SymPrim.BV
  ( BitwidthMismatch,
    IntN,
    WordN,
  )
import Grisette.Internal.SymPrim.FP
  ( FP,
    FPRoundingMode,
    ValidFP,
    withValidFPProofs,
  )
import Grisette.Internal.SymPrim.GeneralFun (type (-->))
import Grisette.Internal.SymPrim.Prim.Term
  ( LinkedRep,
    SupportedPrim,
  )
import Grisette.Internal.SymPrim.SymBV (SymIntN, SymWordN)
import Grisette.Internal.SymPrim.SymBool (SymBool)
import Grisette.Internal.SymPrim.SymFP (SymFP, SymFPRoundingMode)
import Grisette.Internal.SymPrim.SymGeneralFun (type (-~>))
import Grisette.Internal.SymPrim.SymInteger (SymInteger)
import Grisette.Internal.SymPrim.SymTabularFun (type (=~>))
import Grisette.Internal.SymPrim.TabularFun (type (=->))
import Grisette.Internal.TH.Derivation
  ( Strategy (ViaDefault, ViaDefault1),
    deriveFunctorArgBuiltins,
    deriveSimpleBuiltin1s,
  )
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

-- Instances
deriveFunctorArgBuiltins
  (ViaDefault ''Mergeable)
  ''Mergeable
  ''Mergeable1
  [ ''Maybe,
    ''Either,
    ''(),
    -- The following three are implemented by hand because they need to be
    -- consistent with Mergeable2 instances.
    -- ''(,),
    -- ''(,,),
    -- ''(,,,),
    ''(,,,,),
    ''(,,,,,),
    ''(,,,,,,),
    ''(,,,,,,,),
    ''(,,,,,,,,),
    ''(,,,,,,,,,),
    ''(,,,,,,,,,,),
    ''(,,,,,,,,,,,),
    ''(,,,,,,,,,,,,),
    ''(,,,,,,,,,,,,,),
    ''(,,,,,,,,,,,,,,),
    ''AssertionError,
    ''VerificationConditions,
    ''BitwidthMismatch,
    ''Identity
  ]

deriveSimpleBuiltin1s
  (ViaDefault1 ''Mergeable1)
  ''Mergeable
  ''Mergeable1
  [ ''Maybe,
    ''Either,
    -- The following three are implemented by hand because they need to be
    -- consistent with Mergeable2 instances.
    -- ''(,),
    -- ''(,,),
    -- ''(,,,),
    ''(,,,,),
    ''(,,,,,),
    ''(,,,,,,),
    ''(,,,,,,,),
    ''(,,,,,,,,),
    ''(,,,,,,,,,),
    ''(,,,,,,,,,,),
    ''(,,,,,,,,,,,),
    ''(,,,,,,,,,,,,),
    ''(,,,,,,,,,,,,,),
    ''(,,,,,,,,,,,,,,),
    ''Identity
  ]

-- List

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

instance (Mergeable a) => Mergeable [a] where
  rootStrategy = case rootStrategy :: MergingStrategy a of
    SimpleStrategy m ->
      SortedStrategy length $ \_ ->
        SimpleStrategy $ \cond -> zipWith (m cond)
    NoStrategy ->
      SortedStrategy length $ const NoStrategy
    _ -> SortedStrategy length $ \_ ->
      SortedStrategy (buildStrategyList rootStrategy) $
        \(StrategyList _ strategies) ->
          let s :: [MergingStrategy a] = unsafeCoerce strategies
              allSimple = all (\case SimpleStrategy _ -> True; _ -> False) s
           in if allSimple
                then SimpleStrategy $ \cond l r ->
                  ( \case
                      (SimpleStrategy f, l1, r1) -> f cond l1 r1
                      _ -> error "impossible"
                  )
                    <$> zip3 s l r
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
                ( \case
                    (SimpleStrategy f, l1, r1) -> f cond l1 r1
                    _ -> error "impossible"
                )
                  <$> zip3 s l r
              else NoStrategy
  {-# INLINE liftRootStrategy #-}

-- MaybeT
instance (Mergeable1 m, Mergeable a) => Mergeable (MaybeT m a) where
  rootStrategy = rootStrategy1
  {-# INLINE rootStrategy #-}

instance (Mergeable1 m) => Mergeable1 (MaybeT m) where
  liftRootStrategy m =
    wrapStrategy (liftRootStrategy (liftRootStrategy m)) MaybeT runMaybeT
  {-# INLINE liftRootStrategy #-}

-- ExceptT
instance
  (Mergeable1 m, Mergeable e, Mergeable a) =>
  Mergeable (ExceptT e m a)
  where
  rootStrategy = rootStrategy1
  {-# INLINE rootStrategy #-}

instance (Mergeable1 m, Mergeable e) => Mergeable1 (ExceptT e m) where
  liftRootStrategy m =
    wrapStrategy (liftRootStrategy (liftRootStrategy m)) ExceptT runExceptT
  {-# INLINE liftRootStrategy #-}

-- state
instance
  (Mergeable s, Mergeable a, Mergeable1 m) =>
  Mergeable (StateLazy.StateT s m a)
  where
  rootStrategy = rootStrategy1
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
  rootStrategy = rootStrategy1
  {-# INLINE rootStrategy #-}

instance
  (Mergeable s, Mergeable1 m) =>
  Mergeable1 (StateStrict.StateT s m)
  where
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
  rootStrategy = rootStrategy1
  {-# INLINE rootStrategy #-}

instance
  (Mergeable s, Mergeable1 m) =>
  Mergeable1 (WriterLazy.WriterT s m)
  where
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
  rootStrategy = rootStrategy1
  {-# INLINE rootStrategy #-}

instance
  (Mergeable s, Mergeable1 m) =>
  Mergeable1 (WriterStrict.WriterT s m)
  where
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
  rootStrategy = rootStrategy1
  {-# INLINE rootStrategy #-}

instance (Mergeable1 m) => Mergeable1 (ReaderT s m) where
  liftRootStrategy m =
    wrapStrategy
      (liftRootStrategy (liftRootStrategy m))
      ReaderT
      runReaderT
  {-# INLINE liftRootStrategy #-}

-- IdentityT
instance (Mergeable1 m, Mergeable a) => Mergeable (IdentityT m a) where
  rootStrategy = rootStrategy1
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
  rootStrategy = rootStrategy1
  {-# INLINE rootStrategy #-}

instance
  (Mergeable s, Mergeable w, Mergeable1 m) =>
  Mergeable1 (RWSLazy.RWST r w s m)
  where
  liftRootStrategy m =
    wrapStrategy
      ( liftRootStrategy . liftRootStrategy . liftRootStrategy $
          liftRootStrategy3 m rootStrategy rootStrategy
      )
      RWSLazy.RWST
      (\(RWSLazy.RWST rws) -> rws)
  {-# INLINE liftRootStrategy #-}

instance
  (Mergeable s, Mergeable w, Mergeable a, Mergeable1 m) =>
  Mergeable (RWSStrict.RWST r w s m a)
  where
  rootStrategy = rootStrategy1
  {-# INLINE rootStrategy #-}

instance
  (Mergeable s, Mergeable w, Mergeable1 m) =>
  Mergeable1 (RWSStrict.RWST r w s m)
  where
  liftRootStrategy m =
    wrapStrategy
      ( liftRootStrategy . liftRootStrategy . liftRootStrategy $
          liftRootStrategy3 m rootStrategy rootStrategy
      )
      RWSStrict.RWST
      (\(RWSStrict.RWST rws) -> rws)
  {-# INLINE liftRootStrategy #-}

-- Sum
instance
  (Mergeable1 l, Mergeable1 r, Mergeable x) =>
  Mergeable (Sum l r x)
  where
  rootStrategy = rootStrategy1
  {-# INLINE rootStrategy #-}

instance (Mergeable1 l, Mergeable1 r) => Mergeable1 (Sum l r) where
  liftRootStrategy m =
    SortedStrategy
      ( \case
          InL _ -> False
          InR _ -> True
      )
      ( \case
          False ->
            wrapStrategy
              (liftRootStrategy m)
              InL
              (\case (InL v) -> v; _ -> error "impossible")
          True ->
            wrapStrategy
              (liftRootStrategy m)
              InR
              (\case (InR v) -> v; _ -> error "impossible")
      )
  {-# INLINE liftRootStrategy #-}

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

instance Mergeable2 ((->)) where
  liftRootStrategy2 _ ms = case ms of
    SimpleStrategy m -> SimpleStrategy $ \cond t f v -> m cond (t v) (f v)
    _ -> NoStrategy
  {-# INLINE liftRootStrategy2 #-}

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

-- Data.Monoid module
deriving via
  (Default (Monoid.Sum a))
  instance
    (Mergeable a) => Mergeable (Monoid.Sum a)

deriving via (Default1 Monoid.Sum) instance Mergeable1 Monoid.Sum

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

-- (,)
instance (Mergeable a, Mergeable b) => Mergeable (a, b) where
  rootStrategy = rootStrategy1
  {-# INLINE rootStrategy #-}

instance (Mergeable a) => Mergeable1 ((,) a) where
  liftRootStrategy = liftRootStrategy2 rootStrategy
  {-# INLINE liftRootStrategy #-}

instance Mergeable2 (,) where
  liftRootStrategy2 = product2Strategy (,) id
  {-# INLINE liftRootStrategy2 #-}

-- (,,)
instance (Mergeable a, Mergeable b, Mergeable c) => Mergeable ((,,) a b c) where
  rootStrategy = rootStrategy1
  {-# INLINE rootStrategy #-}

instance (Mergeable a, Mergeable b) => Mergeable1 ((,,) a b) where
  liftRootStrategy = liftRootStrategy2 rootStrategy
  {-# INLINE liftRootStrategy #-}

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
instance
  (Mergeable a, Mergeable b, Mergeable c, Mergeable d) =>
  Mergeable ((,,,) a b c d)
  where
  rootStrategy = rootStrategy1
  {-# INLINE rootStrategy #-}

instance
  (Mergeable a, Mergeable b, Mergeable c) =>
  Mergeable1 ((,,,) a b c)
  where
  liftRootStrategy = liftRootStrategy2 rootStrategy
  {-# INLINE liftRootStrategy #-}

instance (Mergeable a, Mergeable b) => Mergeable2 ((,,,) a b) where
  liftRootStrategy2 = liftRootStrategy3 rootStrategy
  {-# INLINE liftRootStrategy2 #-}

instance (Mergeable a) => Mergeable3 ((,,,) a) where
  liftRootStrategy3 m1 m2 m3 =
    product2Strategy
      (\(a, b) (c, d) -> (a, b, c, d))
      (\(a, b, c, d) -> ((a, b), (c, d)))
      (liftRootStrategy m1)
      (liftRootStrategy2 m2 m3)
  {-# INLINE liftRootStrategy3 #-}

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
CONCRETE_ORD_MERGEABLE(Float)
CONCRETE_ORD_MERGEABLE(Double)
CONCRETE_ORD_MERGEABLE(B.ByteString)
CONCRETE_ORD_MERGEABLE(T.Text)
CONCRETE_ORD_MERGEABLE(FPRoundingMode)
CONCRETE_ORD_MERGEABLE_BV(WordN)
CONCRETE_ORD_MERGEABLE_BV(IntN)
#endif

instance (ValidFP eb sb) => Mergeable (FP eb sb) where
  rootStrategy =
    let sub = SimpleStrategy $ \_ t _ -> t
     in withValidFPProofs @eb @sb
          $ SortedStrategy
            (\fp -> (bitCast fp :: WordN (eb + sb)))
          $ const sub

#define MERGEABLE_SIMPLE(symtype) \
instance Mergeable symtype where \
  rootStrategy = SimpleStrategy symIte

#define MERGEABLE_BV(symtype) \
instance (KnownNat n, 1 <= n) => Mergeable (symtype n) where \
  rootStrategy = SimpleStrategy symIte

#define MERGEABLE_FUN(cop, op) \
instance (SupportedPrim (cop ca cb), LinkedRep ca sa, LinkedRep cb sb) => \
  Mergeable (op sa sb) where \
  rootStrategy = SimpleStrategy symIte

#if 1
MERGEABLE_SIMPLE(SymBool)
MERGEABLE_SIMPLE(SymInteger)
MERGEABLE_SIMPLE(SymFPRoundingMode)
MERGEABLE_BV(SymIntN)
MERGEABLE_BV(SymWordN)
MERGEABLE_FUN((=->), (=~>))
MERGEABLE_FUN((-->), (-~>))
#endif

instance (ValidFP eb sb) => Mergeable (SymFP eb sb) where
  rootStrategy = SimpleStrategy symIte
