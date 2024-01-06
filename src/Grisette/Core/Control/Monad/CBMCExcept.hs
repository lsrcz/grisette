{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Core.Control.Monad.CBMCExcept
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Control.Monad.CBMCExcept
  ( -- * CBMC-like error handling
    CBMCEither (..),
    CBMCExceptT (..),
    cbmcExcept,
    mapCBMCExceptT,
    withCBMCExceptT,
    OrigExcept.MonadError (..),
  )
where

#if MIN_VERSION_base(4,18,0)
import Control.Applicative
  ( Alternative (empty, (<|>)),
  )
#else
import Control.Applicative
  ( Alternative (empty, (<|>)),
    Applicative (liftA2),
  )
#endif
import Control.DeepSeq (NFData)
import Control.Monad (MonadPlus (mplus, mzero))
import qualified Control.Monad.Except as OrigExcept
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix (MonadFix (mfix))
import Control.Monad.Trans (MonadIO (liftIO), MonadTrans (lift))
import Control.Monad.Zip (MonadZip (mzipWith))
import Data.Functor.Classes
  ( Eq1 (liftEq),
    Ord1 (liftCompare),
    Read1 (liftReadList, liftReadsPrec),
    Show1 (liftShowList, liftShowsPrec),
    compare1,
    eq1,
    readsData,
    readsPrec1,
    readsUnaryWith,
    showsPrec1,
    showsUnaryWith,
  )
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Hashable (Hashable)
import GHC.Generics (Generic, Generic1)
import Grisette.Core.Control.Monad.UnionM (UnionM)
import Grisette.Core.Data.Class.EvaluateSym (EvaluateSym (evaluateSym))
import Grisette.Core.Data.Class.ExtractSymbolics
  ( ExtractSymbolics (extractSymbolics),
  )
import Grisette.Core.Data.Class.GenSym
  ( GenSym (fresh),
    GenSymSimple (simpleFresh),
    derivedNoSpecFresh,
    derivedSameShapeSimpleFresh,
  )
import Grisette.Core.Data.Class.Mergeable
  ( Mergeable (rootStrategy),
    Mergeable1 (liftRootStrategy),
    MergingStrategy (NoStrategy, SimpleStrategy, SortedStrategy),
    rootStrategy1,
    wrapStrategy,
  )
import Grisette.Core.Data.Class.SEq (SEq ((.==)))
import Grisette.Core.Data.Class.SOrd (SOrd (symCompare, (.<), (.<=), (.>), (.>=)))
import Grisette.Core.Data.Class.SimpleMergeable
  ( SimpleMergeable (mrgIte),
    SimpleMergeable1 (liftMrgIte),
    UnionLike (mergeWithStrategy, mrgIfWithStrategy, single, unionIf),
    merge,
    mrgIf,
  )
import Grisette.Core.Data.Class.Solver (UnionWithExcept (extractUnionExcept))
import Grisette.Core.Data.Class.ToCon (ToCon (toCon))
import Grisette.Core.Data.Class.ToSym (ToSym (toSym))
import Language.Haskell.TH.Syntax (Lift)
import Unsafe.Coerce (unsafeCoerce)

-- | A wrapper type for 'Either'. Uses different merging strategies.
newtype CBMCEither a b = CBMCEither {runCBMCEither :: Either a b}
  deriving newtype (Eq, Eq1, Ord, Ord1, Read, Read1, Show, Show1, Functor, Applicative, Monad, Hashable, NFData)
  deriving stock (Generic, Lift)

deriving newtype instance (SEq e, SEq a) => SEq (CBMCEither e a)

deriving newtype instance (EvaluateSym a, EvaluateSym b) => EvaluateSym (CBMCEither a b)

deriving newtype instance
  (ExtractSymbolics a, ExtractSymbolics b) =>
  ExtractSymbolics (CBMCEither a b)

instance
  ( GenSymSimple a a,
    Mergeable a,
    GenSymSimple b b,
    Mergeable b
  ) =>
  GenSym (CBMCEither a b) (CBMCEither a b)

instance
  ( GenSymSimple a a,
    GenSymSimple b b
  ) =>
  GenSymSimple (CBMCEither a b) (CBMCEither a b)
  where
  simpleFresh = derivedSameShapeSimpleFresh

instance
  (GenSym () a, Mergeable a, GenSym () b, Mergeable b) =>
  GenSym () (CBMCEither a b)
  where
  fresh = derivedNoSpecFresh

deriving newtype instance (SOrd a, SOrd b) => SOrd (CBMCEither a b)

deriving newtype instance (ToCon e1 e2, ToCon a1 a2) => ToCon (Either e1 a1) (CBMCEither e2 a2)

instance (ToCon e1 e2, ToCon a1 a2) => ToCon (CBMCEither e1 a1) (CBMCEither e2 a2) where
  toCon (CBMCEither a) = CBMCEither <$> toCon a

instance (ToCon e1 e2, ToCon a1 a2) => ToCon (CBMCEither e1 a1) (Either e2 a2) where
  toCon (CBMCEither a) = toCon a

deriving newtype instance (ToSym e1 e2, ToSym a1 a2) => ToSym (Either e1 a1) (CBMCEither e2 a2)

instance (ToSym e1 e2, ToSym a1 a2) => ToSym (CBMCEither e1 a1) (CBMCEither e2 a2) where
  toSym (CBMCEither a) = CBMCEither $ toSym a

instance (ToSym e1 e2, ToSym a1 a2) => ToSym (CBMCEither e1 a1) (Either e2 a2) where
  toSym (CBMCEither a) = toSym a

data EitherIdx idx = L idx | R deriving (Eq, Ord, Show)

instance (Mergeable e, Mergeable a) => Mergeable (CBMCEither e a) where
  rootStrategy = rootStrategy1

instance (Mergeable e) => Mergeable1 (CBMCEither e) where
  liftRootStrategy ms = case rootStrategy of
    SimpleStrategy m ->
      SortedStrategy
        ( \(CBMCEither e) -> case e of
            Left _ -> False
            Right _ -> True
        )
        ( \case
            False -> SimpleStrategy $
              \cond (CBMCEither le) (CBMCEither re) -> case (le, re) of
                (Left l, Left r) -> CBMCEither $ Left $ m cond l r
                _ -> error "impossible"
            True -> wrapStrategy ms (CBMCEither . Right) (\case (CBMCEither (Right x)) -> x; _ -> error "impossible")
        )
    NoStrategy ->
      SortedStrategy
        ( \(CBMCEither e) -> case e of
            Left _ -> False
            Right _ -> True
        )
        ( \case
            False -> NoStrategy
            True -> wrapStrategy ms (CBMCEither . Right) (\case (CBMCEither (Right x)) -> x; _ -> error "impossible")
        )
    SortedStrategy idx sub ->
      SortedStrategy
        ( \(CBMCEither e) -> case e of
            Left v -> L $ idx v
            Right _ -> R
        )
        ( \case
            L i -> wrapStrategy (sub i) (CBMCEither . Left) (\case (CBMCEither (Left x)) -> x; _ -> error "impossible")
            R -> wrapStrategy ms (CBMCEither . Right) (\case (CBMCEither (Right x)) -> x; _ -> error "impossible")
        )

cbmcEither :: forall a c b. (a -> c) -> (b -> c) -> CBMCEither a b -> c
cbmcEither l r v = either l r (unsafeCoerce v)

-- | Wrap an 'Either' value in 'CBMCExceptT'
cbmcExcept :: (Monad m) => Either e a -> CBMCExceptT e m a
cbmcExcept m = CBMCExceptT (return $ CBMCEither m)

-- | Map the error and values in a 'CBMCExceptT'
mapCBMCExceptT :: (m (Either e a) -> n (Either e' b)) -> CBMCExceptT e m a -> CBMCExceptT e' n b
mapCBMCExceptT f m = CBMCExceptT $ (unsafeCoerce . f . unsafeCoerce) (runCBMCExceptT m)

-- | Map the error in a 'CBMCExceptT'
withCBMCExceptT :: (Functor m) => (e -> e') -> CBMCExceptT e m a -> CBMCExceptT e' m a
withCBMCExceptT f = mapCBMCExceptT $ fmap $ either (Left . f) Right

-- | Similar to 'ExceptT', but with different error handling mechanism.
newtype CBMCExceptT e m a = CBMCExceptT {runCBMCExceptT :: m (CBMCEither e a)} deriving stock (Generic, Generic1)

instance (Eq e, Eq1 m) => Eq1 (CBMCExceptT e m) where
  liftEq eq (CBMCExceptT x) (CBMCExceptT y) = liftEq (liftEq eq) x y
  {-# INLINE liftEq #-}

instance (Ord e, Ord1 m) => Ord1 (CBMCExceptT e m) where
  liftCompare comp (CBMCExceptT x) (CBMCExceptT y) =
    liftCompare (liftCompare comp) x y
  {-# INLINE liftCompare #-}

instance (Read e, Read1 m) => Read1 (CBMCExceptT e m) where
  liftReadsPrec rp rl =
    readsData $
      readsUnaryWith (liftReadsPrec rp' rl') "CBMCExceptT" CBMCExceptT
    where
      rp' = liftReadsPrec rp rl
      rl' = liftReadList rp rl

instance (Show e, Show1 m) => Show1 (CBMCExceptT e m) where
  liftShowsPrec sp sl d (CBMCExceptT m) =
    showsUnaryWith (liftShowsPrec sp' sl') "CBMCExceptT" d m
    where
      sp' = liftShowsPrec sp sl
      sl' = liftShowList sp sl

instance (Eq e, Eq1 m, Eq a) => Eq (CBMCExceptT e m a) where
  (==) = eq1

instance (Ord e, Ord1 m, Ord a) => Ord (CBMCExceptT e m a) where
  compare = compare1

instance (Read e, Read1 m, Read a) => Read (CBMCExceptT e m a) where
  readsPrec = readsPrec1

instance (Show e, Show1 m, Show a) => Show (CBMCExceptT e m a) where
  showsPrec = showsPrec1

instance (Functor m) => Functor (CBMCExceptT e m) where
  fmap f = CBMCExceptT . fmap (fmap f) . runCBMCExceptT
  {-# INLINE fmap #-}

instance (Foldable f) => Foldable (CBMCExceptT e f) where
  foldMap f (CBMCExceptT a) = foldMap (cbmcEither (const mempty) f) a
  {-# INLINE foldMap #-}

instance (Traversable f) => Traversable (CBMCExceptT e f) where
  traverse f (CBMCExceptT a) =
    CBMCExceptT <$> traverse (cbmcEither (pure . CBMCEither . Left) (fmap (CBMCEither . Right) . f)) a
  {-# INLINE traverse #-}

instance (Functor m, Monad m) => Applicative (CBMCExceptT e m) where
  pure a = CBMCExceptT $ return (CBMCEither . Right $ a)
  {-# INLINE pure #-}
  CBMCExceptT f <*> CBMCExceptT v = CBMCExceptT $ do
    mf <- f
    case mf of
      CBMCEither (Left e) -> return (CBMCEither . Left $ e)
      CBMCEither (Right k) -> do
        mv <- v
        case mv of
          CBMCEither (Left e) -> return (CBMCEither . Left $ e)
          CBMCEither (Right x) -> return (CBMCEither . Right $ k x)
  {-# INLINEABLE (<*>) #-}
  m *> k = m >> k
  {-# INLINE (*>) #-}

instance (Functor m, Monad m, Monoid e) => Alternative (CBMCExceptT e m) where
  empty = CBMCExceptT $ return (CBMCEither . Left $ mempty)
  {-# INLINE empty #-}
  CBMCExceptT mx <|> CBMCExceptT my = CBMCExceptT $ do
    ex <- mx
    case ex of
      CBMCEither (Left e) -> fmap (cbmcEither (CBMCEither . Left . mappend e) (CBMCEither . Right)) my
      CBMCEither (Right x) -> return (CBMCEither . Right $ x)
  {-# INLINEABLE (<|>) #-}

instance (Monad m) => Monad (CBMCExceptT e m) where
  m >>= k = CBMCExceptT $ do
    a <- runCBMCExceptT m
    case a of
      CBMCEither (Left e) -> return (CBMCEither $ Left e)
      CBMCEither (Right x) -> runCBMCExceptT (k x)
  {-# INLINE (>>=) #-}

instance (Fail.MonadFail m) => Fail.MonadFail (CBMCExceptT e m) where
  fail = CBMCExceptT . Fail.fail
  {-# INLINE fail #-}

instance (Monad m, Monoid e) => MonadPlus (CBMCExceptT e m) where
  mzero = CBMCExceptT $ return (CBMCEither $ Left mempty)
  {-# INLINE mzero #-}
  CBMCExceptT mx `mplus` CBMCExceptT my = CBMCExceptT $ do
    ex <- mx
    case ex of
      CBMCEither (Left e) -> fmap (cbmcEither (CBMCEither . Left . mappend e) (CBMCEither . Right)) my
      CBMCEither (Right x) -> return (CBMCEither $ Right x)
  {-# INLINEABLE mplus #-}

instance (MonadFix m) => MonadFix (CBMCExceptT e m) where
  mfix f = CBMCExceptT (mfix (runCBMCExceptT . f . cbmcEither (const bomb) id))
    where
      bomb = error "mfix (CBMCExceptT): inner computation returned Left value"
  {-# INLINE mfix #-}

instance MonadTrans (CBMCExceptT e) where
  lift = CBMCExceptT . fmap (CBMCEither . Right)
  {-# INLINE lift #-}

instance (MonadIO m) => MonadIO (CBMCExceptT e m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance (MonadZip m) => MonadZip (CBMCExceptT e m) where
  mzipWith f (CBMCExceptT a) (CBMCExceptT b) = CBMCExceptT $ mzipWith (liftA2 f) a b
  {-# INLINE mzipWith #-}

instance (Contravariant m) => Contravariant (CBMCExceptT e m) where
  contramap f = CBMCExceptT . contramap (fmap f) . runCBMCExceptT
  {-# INLINE contramap #-}

throwE :: (Monad m) => e -> CBMCExceptT e m a
throwE = CBMCExceptT . return . CBMCEither . Left
{-# INLINE throwE #-}

catchE ::
  (Monad m) =>
  CBMCExceptT e m a ->
  (e -> CBMCExceptT e' m a) ->
  CBMCExceptT e' m a
m `catchE` h = CBMCExceptT $ do
  a <- runCBMCExceptT m
  case a of
    CBMCEither (Left l) -> runCBMCExceptT (h l)
    CBMCEither (Right r) -> return (CBMCEither . Right $ r)
{-# INLINE catchE #-}

instance (Monad m) => OrigExcept.MonadError e (CBMCExceptT e m) where
  throwError = throwE
  {-# INLINE throwError #-}
  catchError = catchE
  {-# INLINE catchError #-}

instance (SEq (m (CBMCEither e a))) => SEq (CBMCExceptT e m a) where
  (CBMCExceptT a) .== (CBMCExceptT b) = a .== b
  {-# INLINE (.==) #-}

instance (EvaluateSym (m (CBMCEither e a))) => EvaluateSym (CBMCExceptT e m a) where
  evaluateSym fillDefault model (CBMCExceptT v) = CBMCExceptT $ evaluateSym fillDefault model v
  {-# INLINE evaluateSym #-}

instance
  (ExtractSymbolics (m (CBMCEither e a))) =>
  ExtractSymbolics (CBMCExceptT e m a)
  where
  extractSymbolics (CBMCExceptT v) = extractSymbolics v

instance
  (Mergeable1 m, Mergeable e, Mergeable a) =>
  Mergeable (CBMCExceptT e m a)
  where
  rootStrategy = wrapStrategy rootStrategy1 CBMCExceptT runCBMCExceptT
  {-# INLINE rootStrategy #-}

instance (Mergeable1 m, Mergeable e) => Mergeable1 (CBMCExceptT e m) where
  liftRootStrategy m = wrapStrategy (liftRootStrategy (liftRootStrategy m)) CBMCExceptT runCBMCExceptT
  {-# INLINE liftRootStrategy #-}

instance
  {-# OVERLAPPABLE #-}
  ( GenSym spec (m (CBMCEither a b)),
    Mergeable1 m,
    Mergeable a,
    Mergeable b
  ) =>
  GenSym spec (CBMCExceptT a m b)
  where
  fresh v = do
    x <- fresh v
    return $ merge . fmap CBMCExceptT $ x

instance
  {-# OVERLAPPABLE #-}
  ( GenSymSimple spec (m (CBMCEither a b))
  ) =>
  GenSymSimple spec (CBMCExceptT a m b)
  where
  simpleFresh v = CBMCExceptT <$> simpleFresh v

instance
  {-# OVERLAPPING #-}
  ( GenSymSimple (m (CBMCEither e a)) (m (CBMCEither e a))
  ) =>
  GenSymSimple (CBMCExceptT e m a) (CBMCExceptT e m a)
  where
  simpleFresh (CBMCExceptT v) = CBMCExceptT <$> simpleFresh v

instance
  {-# OVERLAPPING #-}
  ( GenSymSimple (m (CBMCEither e a)) (m (CBMCEither e a)),
    Mergeable1 m,
    Mergeable e,
    Mergeable a
  ) =>
  GenSym (CBMCExceptT e m a) (CBMCExceptT e m a)

instance
  (UnionLike m, Mergeable e, Mergeable a) =>
  SimpleMergeable (CBMCExceptT e m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (UnionLike m, Mergeable e) =>
  SimpleMergeable1 (CBMCExceptT e m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (UnionLike m, Mergeable e) =>
  UnionLike (CBMCExceptT e m)
  where
  mergeWithStrategy s (CBMCExceptT v) = CBMCExceptT $ mergeWithStrategy (liftRootStrategy s) v
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy s cond (CBMCExceptT t) (CBMCExceptT f) = CBMCExceptT $ mrgIfWithStrategy (liftRootStrategy s) cond t f
  {-# INLINE mrgIfWithStrategy #-}
  single = CBMCExceptT . single . return
  {-# INLINE single #-}
  unionIf cond (CBMCExceptT l) (CBMCExceptT r) = CBMCExceptT $ unionIf cond l r
  {-# INLINE unionIf #-}

instance (SOrd (m (CBMCEither e a))) => SOrd (CBMCExceptT e m a) where
  (CBMCExceptT l) .<= (CBMCExceptT r) = l .<= r
  (CBMCExceptT l) .< (CBMCExceptT r) = l .< r
  (CBMCExceptT l) .>= (CBMCExceptT r) = l .>= r
  (CBMCExceptT l) .> (CBMCExceptT r) = l .> r
  symCompare (CBMCExceptT l) (CBMCExceptT r) = symCompare l r

instance
  (ToCon (m1 (CBMCEither e1 a)) (m2 (CBMCEither e2 b))) =>
  ToCon (CBMCExceptT e1 m1 a) (CBMCExceptT e2 m2 b)
  where
  toCon (CBMCExceptT v) = CBMCExceptT <$> toCon v

instance
  (ToCon (m1 (CBMCEither e1 a)) (Either e2 b)) =>
  ToCon (CBMCExceptT e1 m1 a) (Either e2 b)
  where
  toCon (CBMCExceptT v) = toCon v

instance
  (ToSym (m1 (CBMCEither e1 a)) (m2 (CBMCEither e2 b))) =>
  ToSym (CBMCExceptT e1 m1 a) (CBMCExceptT e2 m2 b)
  where
  toSym (CBMCExceptT v) = CBMCExceptT $ toSym v

instance
  (Monad u, UnionLike u, Mergeable e, Mergeable v) =>
  UnionWithExcept (CBMCExceptT e u v) u e v
  where
  extractUnionExcept = merge . fmap runCBMCEither . runCBMCExceptT

instance UnionWithExcept (UnionM (CBMCEither e v)) UnionM e v where
  extractUnionExcept = fmap runCBMCEither
