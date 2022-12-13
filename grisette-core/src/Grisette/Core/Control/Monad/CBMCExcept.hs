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
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Core.Control.Monad.CBMCExcept
  ( CBMCEither (..),
    CBMCExceptT (..),
    cbmcExcept,
    mapCBMCExceptT,
    withCBMCExceptT,
    OrigExcept.MonadError (..),
  )
where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import qualified Control.Monad.Except as OrigExcept
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Zip
import Data.Functor.Classes
import Data.Functor.Contravariant
import Data.Hashable
import GHC.Generics
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Evaluate
import Grisette.Core.Data.Class.ExtractSymbolics
import Grisette.Core.Data.Class.GenSym
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Data.Class.SOrd
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Core.Data.Class.Solver
import Grisette.Core.Data.Class.ToCon
import Grisette.Core.Data.Class.ToSym
import Language.Haskell.TH.Syntax (Lift)
import Unsafe.Coerce

newtype CBMCEither a b = CBMCEither {runCBMCEither :: Either a b}
  deriving newtype (Eq, Eq1, Ord, Ord1, Read, Read1, Show, Show1, Functor, Applicative, Monad, Hashable, NFData)
  deriving stock (Generic, Lift)

deriving newtype instance (SymBoolOp bool, GSEq bool e, GSEq bool a) => GSEq bool (CBMCEither e a)

deriving newtype instance (GEvaluateSym model a, GEvaluateSym model b) => GEvaluateSym model (CBMCEither a b)

deriving newtype instance
  (Monoid symbolSet, GExtractSymbolics symbolSet a, GExtractSymbolics symbolSet b) =>
  GExtractSymbolics symbolSet (CBMCEither a b)

instance
  ( SymBoolOp bool,
    GenSymSimple () bool,
    GenSymSimple a a,
    GMergeable bool a,
    GenSymSimple b b,
    GMergeable bool b
  ) =>
  GGenSym bool (CBMCEither a b) (CBMCEither a b)

instance
  ( GenSymSimple a a,
    GenSymSimple b b
  ) =>
  GenSymSimple (CBMCEither a b) (CBMCEither a b)
  where
  simpleFresh = derivedSameShapeSimpleFresh

instance
  (SymBoolOp bool, GenSymSimple () bool, GGenSym bool () a, GMergeable bool a, GGenSym bool () b, GMergeable bool b) =>
  GGenSym bool () (CBMCEither a b)
  where
  gfresh = derivedNoSpecGFresh

deriving newtype instance (SymBoolOp bool, GSOrd bool a, GSOrd bool b) => GSOrd bool (CBMCEither a b)

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

instance (SymBoolOp bool, GMergeable bool e, GMergeable bool a) => GMergeable bool (CBMCEither e a) where
  gmergingStrategy = gmergingStrategy1

instance (SymBoolOp bool, GMergeable bool e) => GMergeable1 bool (CBMCEither e) where
  liftGMergingStrategy ms = case gmergingStrategy of
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
            True -> gwrapStrategy ms (CBMCEither . Right) (\case (CBMCEither (Right x)) -> x; _ -> error "impossible")
        )
    NoStrategy ->
      SortedStrategy
        ( \(CBMCEither e) -> case e of
            Left _ -> False
            Right _ -> True
        )
        ( \case
            False -> NoStrategy
            True -> gwrapStrategy ms (CBMCEither . Right) (\case (CBMCEither (Right x)) -> x; _ -> error "impossible")
        )
    SortedStrategy idx sub ->
      SortedStrategy
        ( \(CBMCEither e) -> case e of
            Left v -> L $ idx v
            Right _ -> R
        )
        ( \case
            L i -> gwrapStrategy (sub i) (CBMCEither . Left) (\case (CBMCEither (Left x)) -> x; _ -> error "impossible")
            R -> gwrapStrategy ms (CBMCEither . Right) (\case (CBMCEither (Right x)) -> x; _ -> error "impossible")
        )

cbmcEither :: forall a c b. (a -> c) -> (b -> c) -> CBMCEither a b -> c
cbmcEither l r v = either l r (unsafeCoerce v)

cbmcExcept :: (Monad m) => Either e a -> CBMCExceptT e m a
cbmcExcept m = CBMCExceptT (return $ CBMCEither m)

mapCBMCExceptT :: (m (Either e a) -> n (Either e' b)) -> CBMCExceptT e m a -> CBMCExceptT e' n b
mapCBMCExceptT f m = CBMCExceptT $ (unsafeCoerce . f . unsafeCoerce) (runCBMCExceptT m)

withCBMCExceptT :: Functor m => (e -> e') -> CBMCExceptT e m a -> CBMCExceptT e' m a
withCBMCExceptT f = mapCBMCExceptT $ fmap $ either (Left . f) Right

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

instance Contravariant m => Contravariant (CBMCExceptT e m) where
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

instance Monad m => OrigExcept.MonadError e (CBMCExceptT e m) where
  throwError = throwE
  {-# INLINE throwError #-}
  catchError = catchE
  {-# INLINE catchError #-}

instance (SymBoolOp bool, GSEq bool (m (CBMCEither e a))) => GSEq bool (CBMCExceptT e m a) where
  (CBMCExceptT a) `gsymeq` (CBMCExceptT b) = a `gsymeq` b
  {-# INLINE gsymeq #-}

instance (GEvaluateSym model (m (CBMCEither e a))) => GEvaluateSym model (CBMCExceptT e m a) where
  gevaluateSym fillDefault model (CBMCExceptT v) = CBMCExceptT $ gevaluateSym fillDefault model v
  {-# INLINE gevaluateSym #-}

instance
  (Monoid symbolSet, GExtractSymbolics symbolSet (m (CBMCEither e a))) =>
  GExtractSymbolics symbolSet (CBMCExceptT e m a)
  where
  gextractSymbolics (CBMCExceptT v) = gextractSymbolics v

instance
  (SymBoolOp bool, GMergeable1 bool m, GMergeable bool e, GMergeable bool a) =>
  GMergeable bool (CBMCExceptT e m a)
  where
  gmergingStrategy = gwrapStrategy gmergingStrategy1 CBMCExceptT runCBMCExceptT
  {-# INLINE gmergingStrategy #-}

instance (SymBoolOp bool, GMergeable1 bool m, GMergeable bool e) => GMergeable1 bool (CBMCExceptT e m) where
  liftGMergingStrategy m = gwrapStrategy (liftGMergingStrategy (liftGMergingStrategy m)) CBMCExceptT runCBMCExceptT
  {-# INLINE liftGMergingStrategy #-}

instance
  {-# OVERLAPPABLE #-}
  ( SymBoolOp bool,
    GenSymSimple () bool,
    GGenSym bool spec (m (CBMCEither a b)),
    GMergeable1 bool m,
    GMergeable bool a,
    GMergeable bool b
  ) =>
  GGenSym bool spec (CBMCExceptT a m b)
  where
  gfresh v = do
    x <- gfresh v
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
  ( SymBoolOp bool,
    GenSymSimple () bool,
    GenSymSimple (m (CBMCEither e a)) (m (CBMCEither e a)),
    GMergeable1 bool m,
    GMergeable bool e,
    GMergeable bool a
  ) =>
  GGenSym bool (CBMCExceptT e m a) (CBMCExceptT e m a)

instance
  (SymBoolOp bool, GUnionLike bool m, GMergeable bool e, GMergeable bool a) =>
  GSimpleMergeable bool (CBMCExceptT e m a)
  where
  gmrgIte = mrgIf
  {-# INLINE gmrgIte #-}

instance
  (SymBoolOp bool, GUnionLike bool m, GMergeable bool e) =>
  GSimpleMergeable1 bool (CBMCExceptT e m)
  where
  liftGMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftGMrgIte #-}

instance
  (SymBoolOp bool, GUnionLike bool m, GMergeable bool e) =>
  GUnionLike bool (CBMCExceptT e m)
  where
  mergeWithStrategy s (CBMCExceptT v) = CBMCExceptT $ mergeWithStrategy (liftGMergingStrategy s) v
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy s cond (CBMCExceptT t) (CBMCExceptT f) = CBMCExceptT $ mrgIfWithStrategy (liftGMergingStrategy s) cond t f
  {-# INLINE mrgIfWithStrategy #-}
  single = CBMCExceptT . single . return
  {-# INLINE single #-}
  unionIf cond (CBMCExceptT l) (CBMCExceptT r) = CBMCExceptT $ unionIf cond l r
  {-# INLINE unionIf #-}

instance (SymBoolOp bool, GSOrd bool (m (CBMCEither e a))) => GSOrd bool (CBMCExceptT e m a) where
  (CBMCExceptT l) `gsymle` (CBMCExceptT r) = l `gsymle` r
  (CBMCExceptT l) `gsymlt` (CBMCExceptT r) = l `gsymlt` r
  (CBMCExceptT l) `gsymge` (CBMCExceptT r) = l `gsymge` r
  (CBMCExceptT l) `gsymgt` (CBMCExceptT r) = l `gsymgt` r
  gsymCompare (CBMCExceptT l) (CBMCExceptT r) = gsymCompare l r

instance
  ToCon (m1 (CBMCEither e1 a)) (m2 (CBMCEither e2 b)) =>
  ToCon (CBMCExceptT e1 m1 a) (CBMCExceptT e2 m2 b)
  where
  toCon (CBMCExceptT v) = CBMCExceptT <$> toCon v

instance
  ToCon (m1 (CBMCEither e1 a)) (Either e2 b) =>
  ToCon (CBMCExceptT e1 m1 a) (Either e2 b)
  where
  toCon (CBMCExceptT v) = toCon v

instance
  (ToSym (m1 (CBMCEither e1 a)) (m2 (CBMCEither e2 b))) =>
  ToSym (CBMCExceptT e1 m1 a) (CBMCExceptT e2 m2 b)
  where
  toSym (CBMCExceptT v) = CBMCExceptT $ toSym v

instance
  (Monad u, GUnionLike bool u, GMergeable bool e, GMergeable bool v) =>
  UnionWithExcept (CBMCExceptT e u v) u e v
  where
  extractUnionExcept = merge . fmap runCBMCEither . runCBMCExceptT
