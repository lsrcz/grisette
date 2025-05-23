{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Experimental.GenSymConstrained
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Experimental.GenSymConstrained
  ( -- * Symbolic value generation with errors
    GenSymConstrained (..),
    GenSymSimpleConstrained (..),
    genSymConstrained,
    genSymSimpleConstrained,
    derivedFreshConstrainedNoSpec,
    derivedSimpleFreshConstrainedNoSpec,
    derivedSimpleFreshConstrainedSameShape,

    -- * Some common GenSymConstrained specifications
    SymOrdUpperBound (..),
    SymOrdLowerBound (..),
    SymOrdBound (..),
  )
where

import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import GHC.Generics
  ( Generic (Rep, from, to),
    K1 (K1),
    M1 (M1),
    U1 (U1),
    type (:*:) ((:*:)),
    type (:+:) (L1, R1),
  )
import Grisette.Internal.Core.Control.Monad.Class.Union (MonadUnion)
import Grisette.Internal.Core.Control.Monad.Union
  ( Union,
  )
import Grisette.Internal.Core.Data.Class.GenSym
  ( GenSym (fresh),
    GenSymSimple (simpleFresh),
    ListSpec (ListSpec),
    MonadFresh,
    SimpleListSpec (SimpleListSpec),
    chooseFresh,
    chooseUnionFresh,
    runFreshT,
  )
import Grisette.Internal.Core.Data.Class.LogicalOp (LogicalOp ((.||)))
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable, Mergeable1)
import Grisette.Internal.Core.Data.Class.SimpleMergeable
  ( mrgIf,
  )
import Grisette.Internal.Core.Data.Class.SymOrd (SymOrd ((.<), (.>=)))
import Grisette.Internal.Core.Data.Class.TryMerge
  ( mrgSingle,
    tryMerge,
  )
import Grisette.Internal.Core.Data.Class.UnionView (liftUnion)
import Grisette.Internal.Core.Data.Symbol (Identifier)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.Experimental
-- >>> import Grisette.SymPrim

-- | Class of types in which symbolic values can be generated with some
-- specification.
--
-- See 'GenSym' for more details. The difference of this class is that it allows
-- constraints to be generated along with the generation of symbolic values.
class (Mergeable a) => GenSymConstrained spec a where
  -- | Generates a symbolic value with the given specification.
  --
  -- Constraint violations will throw an error in the monadic environment.
  --
  -- >>> runFreshT (freshConstrained () (SymOrdUpperBound (1 :: SymInteger) ())) "a" :: ExceptT () Union (Union SymInteger)
  -- ExceptT <If (<= 1 a@0) (Left ()) (Right {a@0})>
  freshConstrained ::
    (MonadFresh m, MonadError e m, MonadUnion m) =>
    e ->
    spec ->
    m (Union a)
  default freshConstrained ::
    (GenSymSimpleConstrained spec a) =>
    ( MonadFresh m,
      MonadError e m,
      MonadUnion m
    ) =>
    e ->
    spec ->
    m (Union a)
  freshConstrained e spec = mrgSingle <$> simpleFreshConstrained e spec

-- | Generates a symbolic value with the given specification, also place the
-- necessary constraints.
genSymConstrained :: forall spec a e. (GenSymConstrained spec a, Mergeable e) => e -> spec -> Identifier -> ExceptT e Union (Union a)
genSymConstrained e spec = tryMerge . runFreshT (freshConstrained e spec)

-- | Class of types in which symbolic values can be generated with some
-- specification.
--
-- See 'GenSymSimple' for more details. The difference of this class is that it allows
-- constraints to be generated along with the generation of symbolic values.
class (Mergeable a) => GenSymSimpleConstrained spec a where
  -- | Generates a symbolic value with the given specification.
  --
  -- Constraint violations will throw an error in the monadic environment.
  --
  -- >>> runFreshT (simpleFreshConstrained () (SymOrdUpperBound (1 :: SymInteger) ())) "a" :: ExceptT () Union SymInteger
  -- ExceptT <If (<= 1 a@0) (Left ()) (Right a@0)>
  simpleFreshConstrained ::
    (MonadFresh m, MonadError e m, MonadUnion m) =>
    e ->
    spec ->
    m a

-- | Generates a symbolic value with the given specification, also place the
-- necessary constraints.
genSymSimpleConstrained :: forall spec a e. (GenSymSimpleConstrained spec a, Mergeable e) => e -> spec -> Identifier -> ExceptT e Union a
genSymSimpleConstrained e spec = tryMerge . runFreshT (simpleFreshConstrained e spec)

instance {-# OVERLAPPABLE #-} (Mergeable a, GenSym spec a) => GenSymConstrained spec a where
  freshConstrained _ = fresh

instance {-# OVERLAPPABLE #-} (Mergeable a, GenSymSimple spec a) => GenSymSimpleConstrained spec a where
  simpleFreshConstrained _ = simpleFresh

-- | Exclusive bound, generates the values with the specification, then filters
-- out the ones that are greater than or equal to the bound
data SymOrdUpperBound a spec = SymOrdUpperBound a spec

instance {-# OVERLAPPABLE #-} (SymOrd a, Mergeable a, GenSym spec a) => GenSymConstrained (SymOrdUpperBound a spec) a where
  freshConstrained e (SymOrdUpperBound u spec) = do
    s <- fresh spec
    v <- liftUnion s
    mrgIf (v .>= u) (throwError e) (return ())
    mrgSingle $ mrgSingle v

instance {-# OVERLAPPABLE #-} (SymOrd a, Mergeable a, GenSymSimple spec a) => GenSymSimpleConstrained (SymOrdUpperBound a spec) a where
  simpleFreshConstrained e (SymOrdUpperBound u spec) = do
    s <- simpleFresh spec
    mrgIf (s .>= u) (throwError e) (return ())
    mrgSingle s

-- | Inclusive bound, generates the values with the specification, then filters
-- out the ones that are less than the bound
data SymOrdLowerBound a spec = SymOrdLowerBound a spec

instance {-# OVERLAPPABLE #-} (SymOrd a, Mergeable a, GenSym spec a) => GenSymConstrained (SymOrdLowerBound a spec) a where
  freshConstrained e (SymOrdLowerBound l spec) = do
    s <- fresh spec
    v <- liftUnion s
    mrgIf (v .< l) (throwError e) (return ())
    mrgSingle $ mrgSingle v

instance {-# OVERLAPPABLE #-} (SymOrd a, Mergeable a, GenSymSimple spec a) => GenSymSimpleConstrained (SymOrdLowerBound a spec) a where
  simpleFreshConstrained e (SymOrdLowerBound l spec) = do
    s <- simpleFresh spec
    mrgIf (s .< l) (throwError e) (return ())
    mrgSingle s

-- | Left-inclusive, right-exclusive bound, generates the values with the
-- specification, then filters out the ones that are out-of-bound
data SymOrdBound a spec = SymOrdBound a a spec

instance {-# OVERLAPPABLE #-} (SymOrd a, Mergeable a, GenSym spec a) => GenSymConstrained (SymOrdBound a spec) a where
  freshConstrained e (SymOrdBound l u spec) = do
    s <- fresh spec
    v <- liftUnion s
    mrgIf (v .< l .|| v .>= u) (throwError e) (return ())
    mrgSingle $ mrgSingle v

instance {-# OVERLAPPABLE #-} (SymOrd a, Mergeable a, GenSymSimple spec a) => GenSymSimpleConstrained (SymOrdBound a spec) a where
  simpleFreshConstrained e (SymOrdBound l u spec) = do
    s <- simpleFresh spec
    mrgIf (s .< l .|| s .>= u) (throwError e) (return ())
    mrgSingle s

instance GenSymConstrained (SymOrdBound Integer ()) Integer where
  freshConstrained _ (SymOrdBound l r _) = chooseFresh [l .. r - 1]

-- Either
instance
  ( GenSymConstrained aspec a,
    Mergeable a,
    GenSymConstrained bspec b,
    Mergeable b
  ) =>
  GenSymConstrained (Either aspec bspec) (Either a b)
  where
  freshConstrained e (Left aspec) = tryMerge $ (tryMerge . fmap Left) <$> freshConstrained e aspec
  freshConstrained e (Right bspec) = tryMerge $ (tryMerge . fmap Right) <$> freshConstrained e bspec

instance
  ( GenSymSimpleConstrained a a,
    GenSymSimpleConstrained b b
  ) =>
  GenSymSimpleConstrained (Either a b) (Either a b)
  where
  simpleFreshConstrained = derivedSimpleFreshConstrainedSameShape

instance
  (GenSymConstrained () a, Mergeable a, GenSymConstrained () b, Mergeable b) =>
  GenSymConstrained () (Either a b)
  where
  freshConstrained = derivedFreshConstrainedNoSpec

-- Maybe
instance
  (GenSymConstrained aspec a, Mergeable a) =>
  GenSymConstrained (Maybe aspec) (Maybe a)
  where
  freshConstrained _ Nothing = mrgSingle $ mrgSingle Nothing
  freshConstrained e (Just aspec) = tryMerge $ (tryMerge . fmap Just) <$> freshConstrained e aspec

instance
  (GenSymSimpleConstrained aspec a) =>
  GenSymSimpleConstrained (Maybe aspec) (Maybe a)
  where
  simpleFreshConstrained _ Nothing = mrgSingle Nothing
  simpleFreshConstrained e (Just aspec) = tryMerge $ Just <$> simpleFreshConstrained e aspec

instance (GenSymConstrained aspec a, Mergeable a) => GenSymConstrained aspec (Maybe a) where
  freshConstrained e aspec = do
    a :: Union a <- freshConstrained e aspec
    tryMerge $ chooseUnionFresh [return Nothing, Just <$> a]

-- List
instance
  (GenSymConstrained () a, Mergeable a) =>
  GenSymConstrained Integer [a]
  where
  freshConstrained e v = do
    l <- gl e v
    let xs = reverse $ scanr (:) [] l
    tryMerge $ chooseUnionFresh $ tryMerge . sequence <$> xs
    where
      gl :: (MonadFresh m, MonadError e m, MonadUnion m) => e -> Integer -> m [Union a]
      gl e1 v1
        | v1 <= 0 = mrgSingle []
        | otherwise = do
            l <- freshConstrained e1 ()
            r <- gl e1 (v1 - 1)
            mrgSingle $ l : r

instance
  (GenSymConstrained spec a, Mergeable a) =>
  GenSymConstrained (ListSpec spec) [a]
  where
  freshConstrained e (ListSpec minLen maxLen subSpec) =
    if minLen < 0 || maxLen < 0 || minLen >= maxLen
      then error $ "Bad lengths: " ++ show (minLen, maxLen)
      else do
        l <- gl e maxLen
        let xs = drop minLen $ reverse $ scanr (:) [] l
        tryMerge $ chooseUnionFresh $ tryMerge . sequence <$> xs
    where
      gl :: (MonadFresh m, MonadError e m, MonadUnion m) => e -> Int -> m [Union a]
      gl e1 currLen
        | currLen <= 0 = return []
        | otherwise = do
            l <- freshConstrained e1 subSpec
            r <- gl e1 (currLen - 1)
            return $ l : r

instance
  (GenSymConstrained a a, Mergeable a) =>
  GenSymConstrained [a] [a]
  where
  freshConstrained e l = do
    r :: [Union a] <- traverse (freshConstrained e) l
    mrgSingle $ tryMerge $ sequence r

instance
  (GenSymSimpleConstrained a a) =>
  GenSymSimpleConstrained [a] [a]
  where
  simpleFreshConstrained = derivedSimpleFreshConstrainedSameShape

instance
  (GenSymConstrained spec a, Mergeable a) =>
  GenSymConstrained (SimpleListSpec spec) [a]
  where
  freshConstrained e (SimpleListSpec len subSpec) =
    if len < 0
      then error $ "Bad lengths: " ++ show len
      else do
        tryMerge $ tryMerge . sequence <$> gl e len
    where
      gl :: (MonadFresh m, MonadError e m, MonadUnion m) => e -> Int -> m [Union a]
      gl e1 currLen
        | currLen <= 0 = mrgSingle []
        | otherwise = do
            l <- freshConstrained e1 subSpec
            r <- gl e1 (currLen - 1)
            mrgSingle $ l : r

instance
  (GenSymSimpleConstrained spec a) =>
  GenSymSimpleConstrained (SimpleListSpec spec) [a]
  where
  simpleFreshConstrained e (SimpleListSpec len subSpec) =
    if len < 0
      then error $ "Bad lengths: " ++ show len
      else do
        gl e len
    where
      gl :: (MonadFresh m, MonadError e m, MonadUnion m) => e -> Int -> m [a]
      gl e1 currLen
        | currLen <= 0 = mrgSingle []
        | otherwise = do
            l <- simpleFreshConstrained e1 subSpec
            r <- gl e1 (currLen - 1)
            mrgSingle $ l : r

-- (,)
instance
  ( GenSymConstrained aspec a,
    Mergeable a,
    GenSymConstrained bspec b,
    Mergeable b
  ) =>
  GenSymConstrained (aspec, bspec) (a, b)
  where
  freshConstrained err (aspec, bspec) = do
    a1 <- freshConstrained err aspec
    b1 <- freshConstrained err bspec
    mrgSingle $ do
      ax <- a1
      bx <- b1
      mrgSingle (ax, bx)

instance
  ( GenSymSimpleConstrained aspec a,
    GenSymSimpleConstrained bspec b
  ) =>
  GenSymSimpleConstrained (aspec, bspec) (a, b)
  where
  simpleFreshConstrained e (aspec, bspec) = do
    tryMerge $
      (,)
        <$> simpleFreshConstrained e aspec
        <*> simpleFreshConstrained e bspec

-- (,,)
instance
  ( GenSymConstrained aspec a,
    Mergeable a,
    GenSymConstrained bspec b,
    Mergeable b,
    GenSymConstrained cspec c,
    Mergeable c
  ) =>
  GenSymConstrained (aspec, bspec, cspec) (a, b, c)
  where
  freshConstrained err (aspec, bspec, cspec) = do
    a1 <- freshConstrained err aspec
    b1 <- freshConstrained err bspec
    c1 <- freshConstrained err cspec
    mrgSingle $ do
      ax <- a1
      bx <- b1
      cx <- c1
      mrgSingle (ax, bx, cx)

instance
  ( GenSymSimpleConstrained aspec a,
    GenSymSimpleConstrained bspec b,
    GenSymSimpleConstrained cspec c
  ) =>
  GenSymSimpleConstrained (aspec, bspec, cspec) (a, b, c)
  where
  simpleFreshConstrained e (aspec, bspec, cspec) = do
    tryMerge $
      (,,)
        <$> simpleFreshConstrained e aspec
        <*> simpleFreshConstrained e bspec
        <*> simpleFreshConstrained e cspec

-- (,,,)
instance
  ( GenSymConstrained aspec a,
    Mergeable a,
    GenSymConstrained bspec b,
    Mergeable b,
    GenSymConstrained cspec c,
    Mergeable c,
    GenSymConstrained dspec d,
    Mergeable d
  ) =>
  GenSymConstrained (aspec, bspec, cspec, dspec) (a, b, c, d)
  where
  freshConstrained err (aspec, bspec, cspec, dspec) = do
    a1 <- freshConstrained err aspec
    b1 <- freshConstrained err bspec
    c1 <- freshConstrained err cspec
    d1 <- freshConstrained err dspec
    mrgSingle $ do
      ax <- a1
      bx <- b1
      cx <- c1
      dx <- d1
      mrgSingle (ax, bx, cx, dx)

instance
  ( GenSymSimpleConstrained aspec a,
    GenSymSimpleConstrained bspec b,
    GenSymSimpleConstrained cspec c,
    GenSymSimpleConstrained dspec d
  ) =>
  GenSymSimpleConstrained (aspec, bspec, cspec, dspec) (a, b, c, d)
  where
  simpleFreshConstrained e (aspec, bspec, cspec, dspec) = do
    tryMerge $
      (,,,)
        <$> simpleFreshConstrained e aspec
        <*> simpleFreshConstrained e bspec
        <*> simpleFreshConstrained e cspec
        <*> simpleFreshConstrained e dspec

-- (,,,,)
instance
  ( GenSymConstrained aspec a,
    Mergeable a,
    GenSymConstrained bspec b,
    Mergeable b,
    GenSymConstrained cspec c,
    Mergeable c,
    GenSymConstrained dspec d,
    Mergeable d,
    GenSymConstrained espec e,
    Mergeable e
  ) =>
  GenSymConstrained (aspec, bspec, cspec, dspec, espec) (a, b, c, d, e)
  where
  freshConstrained err (aspec, bspec, cspec, dspec, espec) = do
    a1 <- freshConstrained err aspec
    b1 <- freshConstrained err bspec
    c1 <- freshConstrained err cspec
    d1 <- freshConstrained err dspec
    e1 <- freshConstrained err espec
    mrgSingle $ do
      ax <- a1
      bx <- b1
      cx <- c1
      dx <- d1
      ex <- e1
      mrgSingle (ax, bx, cx, dx, ex)

instance
  ( GenSymSimpleConstrained aspec a,
    GenSymSimpleConstrained bspec b,
    GenSymSimpleConstrained cspec c,
    GenSymSimpleConstrained dspec d,
    GenSymSimpleConstrained espec e
  ) =>
  GenSymSimpleConstrained (aspec, bspec, cspec, dspec, espec) (a, b, c, d, e)
  where
  simpleFreshConstrained e (aspec, bspec, cspec, dspec, espec) = do
    tryMerge $
      (,,,,)
        <$> simpleFreshConstrained e aspec
        <*> simpleFreshConstrained e bspec
        <*> simpleFreshConstrained e cspec
        <*> simpleFreshConstrained e dspec
        <*> simpleFreshConstrained e espec

-- (,,,,,)
instance
  ( GenSymConstrained aspec a,
    Mergeable a,
    GenSymConstrained bspec b,
    Mergeable b,
    GenSymConstrained cspec c,
    Mergeable c,
    GenSymConstrained dspec d,
    Mergeable d,
    GenSymConstrained espec e,
    Mergeable e,
    GenSymConstrained fspec f,
    Mergeable f
  ) =>
  GenSymConstrained (aspec, bspec, cspec, dspec, espec, fspec) (a, b, c, d, e, f)
  where
  freshConstrained err (aspec, bspec, cspec, dspec, espec, fspec) = do
    a1 <- freshConstrained err aspec
    b1 <- freshConstrained err bspec
    c1 <- freshConstrained err cspec
    d1 <- freshConstrained err dspec
    e1 <- freshConstrained err espec
    f1 <- freshConstrained err fspec
    mrgSingle $ do
      ax <- a1
      bx <- b1
      cx <- c1
      dx <- d1
      ex <- e1
      fx <- f1
      mrgSingle (ax, bx, cx, dx, ex, fx)

instance
  ( GenSymSimpleConstrained aspec a,
    GenSymSimpleConstrained bspec b,
    GenSymSimpleConstrained cspec c,
    GenSymSimpleConstrained dspec d,
    GenSymSimpleConstrained espec e,
    GenSymSimpleConstrained fspec f
  ) =>
  GenSymSimpleConstrained (aspec, bspec, cspec, dspec, espec, fspec) (a, b, c, d, e, f)
  where
  simpleFreshConstrained e (aspec, bspec, cspec, dspec, espec, fspec) = do
    tryMerge $
      (,,,,,)
        <$> simpleFreshConstrained e aspec
        <*> simpleFreshConstrained e bspec
        <*> simpleFreshConstrained e cspec
        <*> simpleFreshConstrained e dspec
        <*> simpleFreshConstrained e espec
        <*> simpleFreshConstrained e fspec

-- (,,,,,,)
instance
  ( GenSymConstrained aspec a,
    Mergeable a,
    GenSymConstrained bspec b,
    Mergeable b,
    GenSymConstrained cspec c,
    Mergeable c,
    GenSymConstrained dspec d,
    Mergeable d,
    GenSymConstrained espec e,
    Mergeable e,
    GenSymConstrained fspec f,
    Mergeable f,
    GenSymConstrained gspec g,
    Mergeable g
  ) =>
  GenSymConstrained (aspec, bspec, cspec, dspec, espec, fspec, gspec) (a, b, c, d, e, f, g)
  where
  freshConstrained err (aspec, bspec, cspec, dspec, espec, fspec, gspec) = do
    a1 <- freshConstrained err aspec
    b1 <- freshConstrained err bspec
    c1 <- freshConstrained err cspec
    d1 <- freshConstrained err dspec
    e1 <- freshConstrained err espec
    f1 <- freshConstrained err fspec
    g1 <- freshConstrained err gspec
    mrgSingle $ do
      ax <- a1
      bx <- b1
      cx <- c1
      dx <- d1
      ex <- e1
      fx <- f1
      gx <- g1
      mrgSingle (ax, bx, cx, dx, ex, fx, gx)

instance
  ( GenSymSimpleConstrained aspec a,
    GenSymSimpleConstrained bspec b,
    GenSymSimpleConstrained cspec c,
    GenSymSimpleConstrained dspec d,
    GenSymSimpleConstrained espec e,
    GenSymSimpleConstrained fspec f,
    GenSymSimpleConstrained gspec g
  ) =>
  GenSymSimpleConstrained (aspec, bspec, cspec, dspec, espec, fspec, gspec) (a, b, c, d, e, f, g)
  where
  simpleFreshConstrained e (aspec, bspec, cspec, dspec, espec, fspec, gspec) = do
    tryMerge $
      (,,,,,,)
        <$> simpleFreshConstrained e aspec
        <*> simpleFreshConstrained e bspec
        <*> simpleFreshConstrained e cspec
        <*> simpleFreshConstrained e dspec
        <*> simpleFreshConstrained e espec
        <*> simpleFreshConstrained e fspec
        <*> simpleFreshConstrained e gspec

-- (,,,,,,,)
instance
  ( GenSymConstrained aspec a,
    Mergeable a,
    GenSymConstrained bspec b,
    Mergeable b,
    GenSymConstrained cspec c,
    Mergeable c,
    GenSymConstrained dspec d,
    Mergeable d,
    GenSymConstrained espec e,
    Mergeable e,
    GenSymConstrained fspec f,
    Mergeable f,
    GenSymConstrained gspec g,
    Mergeable g,
    GenSymConstrained hspec h,
    Mergeable h
  ) =>
  GenSymConstrained (aspec, bspec, cspec, dspec, espec, fspec, gspec, hspec) (a, b, c, d, e, f, g, h)
  where
  freshConstrained err (aspec, bspec, cspec, dspec, espec, fspec, gspec, hspec) = do
    a1 <- freshConstrained err aspec
    b1 <- freshConstrained err bspec
    c1 <- freshConstrained err cspec
    d1 <- freshConstrained err dspec
    e1 <- freshConstrained err espec
    f1 <- freshConstrained err fspec
    g1 <- freshConstrained err gspec
    h1 <- freshConstrained err hspec
    mrgSingle $ do
      ax <- a1
      bx <- b1
      cx <- c1
      dx <- d1
      ex <- e1
      fx <- f1
      gx <- g1
      hx <- h1
      mrgSingle (ax, bx, cx, dx, ex, fx, gx, hx)

instance
  ( GenSymSimpleConstrained aspec a,
    GenSymSimpleConstrained bspec b,
    GenSymSimpleConstrained cspec c,
    GenSymSimpleConstrained dspec d,
    GenSymSimpleConstrained espec e,
    GenSymSimpleConstrained fspec f,
    GenSymSimpleConstrained gspec g,
    GenSymSimpleConstrained hspec h
  ) =>
  GenSymSimpleConstrained (aspec, bspec, cspec, dspec, espec, fspec, gspec, hspec) (a, b, c, d, e, f, g, h)
  where
  simpleFreshConstrained e (aspec, bspec, cspec, dspec, espec, fspec, gspec, hspec) = do
    tryMerge $
      (,,,,,,,)
        <$> simpleFreshConstrained e aspec
        <*> simpleFreshConstrained e bspec
        <*> simpleFreshConstrained e cspec
        <*> simpleFreshConstrained e dspec
        <*> simpleFreshConstrained e espec
        <*> simpleFreshConstrained e fspec
        <*> simpleFreshConstrained e gspec
        <*> simpleFreshConstrained e hspec

-- MaybeT
instance
  {-# OVERLAPPABLE #-}
  ( GenSymConstrained spec (m (Maybe a)),
    Mergeable1 m,
    Mergeable a
  ) =>
  GenSymConstrained spec (MaybeT m a)
  where
  freshConstrained e v = do
    x <- freshConstrained e v
    mrgSingle $ tryMerge . fmap MaybeT $ x

instance
  {-# OVERLAPPABLE #-}
  ( GenSymSimpleConstrained spec (m (Maybe a)),
    Mergeable1 m,
    Mergeable a
  ) =>
  GenSymSimpleConstrained spec (MaybeT m a)
  where
  simpleFreshConstrained e v = tryMerge $ MaybeT <$> simpleFreshConstrained e v

instance
  {-# OVERLAPPING #-}
  ( GenSymSimpleConstrained (m (Maybe a)) (m (Maybe a)),
    Mergeable1 m,
    Mergeable a
  ) =>
  GenSymSimpleConstrained (MaybeT m a) (MaybeT m a)
  where
  simpleFreshConstrained e (MaybeT v) = tryMerge $ MaybeT <$> simpleFreshConstrained e v

instance
  {-# OVERLAPPING #-}
  ( GenSymSimpleConstrained (m (Maybe a)) (m (Maybe a)),
    Mergeable1 m,
    Mergeable a
  ) =>
  GenSymConstrained (MaybeT m a) (MaybeT m a)

-- ExceptT
instance
  {-# OVERLAPPABLE #-}
  ( GenSymConstrained spec (m (Either a b)),
    Mergeable1 m,
    Mergeable a,
    Mergeable b
  ) =>
  GenSymConstrained spec (ExceptT a m b)
  where
  freshConstrained e v = do
    x <- freshConstrained e v
    mrgSingle $ tryMerge . fmap ExceptT $ x

instance
  {-# OVERLAPPABLE #-}
  ( GenSymSimpleConstrained spec (m (Either a b)),
    Mergeable1 m,
    Mergeable a,
    Mergeable b
  ) =>
  GenSymSimpleConstrained spec (ExceptT a m b)
  where
  simpleFreshConstrained e v = tryMerge $ ExceptT <$> simpleFreshConstrained e v

instance
  {-# OVERLAPPING #-}
  ( GenSymSimpleConstrained (m (Either e a)) (m (Either e a)),
    Mergeable1 m,
    Mergeable e,
    Mergeable a
  ) =>
  GenSymSimpleConstrained (ExceptT e m a) (ExceptT e m a)
  where
  simpleFreshConstrained e (ExceptT v) = tryMerge $ ExceptT <$> simpleFreshConstrained e v

instance
  {-# OVERLAPPING #-}
  ( GenSymSimpleConstrained (m (Either e a)) (m (Either e a)),
    Mergeable1 m,
    Mergeable e,
    Mergeable a
  ) =>
  GenSymConstrained (ExceptT e m a) (ExceptT e m a)

-- Deriving

class GenSymConstrainedNoSpec a where
  freshConstrainedNoSpec ::
    ( MonadFresh m,
      MonadError e m,
      MonadUnion m
    ) =>
    e ->
    m (Union (a c))

instance GenSymConstrainedNoSpec U1 where
  freshConstrainedNoSpec _ = return $ mrgSingle U1

instance (GenSymConstrained () c) => GenSymConstrainedNoSpec (K1 i c) where
  freshConstrainedNoSpec e = fmap K1 <$> freshConstrained e ()

instance (GenSymConstrainedNoSpec a) => GenSymConstrainedNoSpec (M1 i c a) where
  freshConstrainedNoSpec e = fmap M1 <$> freshConstrainedNoSpec e

instance
  ( GenSymConstrainedNoSpec a,
    GenSymConstrainedNoSpec b,
    forall x. Mergeable (a x),
    forall x. Mergeable (b x)
  ) =>
  GenSymConstrainedNoSpec (a :+: b)
  where
  freshConstrainedNoSpec ::
    forall m c e.
    ( MonadFresh m,
      MonadError e m,
      MonadUnion m
    ) =>
    e ->
    m (Union ((a :+: b) c))
  freshConstrainedNoSpec e = do
    cond :: bool <- simpleFresh ()
    l :: Union (a c) <- freshConstrainedNoSpec e
    r :: Union (b c) <- freshConstrainedNoSpec e
    return $ mrgIf cond (fmap L1 l) (fmap R1 r)

instance
  (GenSymConstrainedNoSpec a, GenSymConstrainedNoSpec b) =>
  GenSymConstrainedNoSpec (a :*: b)
  where
  freshConstrainedNoSpec ::
    forall m c e.
    ( MonadFresh m,
      MonadError e m,
      MonadUnion m
    ) =>
    e ->
    m (Union ((a :*: b) c))
  freshConstrainedNoSpec e = do
    l :: Union (a c) <- freshConstrainedNoSpec e
    r :: Union (b c) <- freshConstrainedNoSpec e
    return $ do
      l1 <- l
      r1 <- r
      return $ l1 :*: r1

-- | We cannot provide DerivingVia style derivation for 'GenSymConstrained', while you can
-- use this 'freshConstrained' implementation to implement 'GenSymConstrained' for your own types.
--
-- This 'freshConstrained' implementation is for the types that does not need any specification.
-- It will generate product types by generating each fields with @()@ as specification,
-- and generate all possible values for a sum type.
--
-- __Note:__ __Never__ use on recursive types.
derivedFreshConstrainedNoSpec ::
  forall a m e.
  ( Generic a,
    GenSymConstrainedNoSpec (Rep a),
    Mergeable a,
    MonadFresh m,
    MonadError e m,
    MonadUnion m
  ) =>
  e ->
  () ->
  m (Union a)
derivedFreshConstrainedNoSpec e _ = tryMerge $ (tryMerge . fmap to) <$> freshConstrainedNoSpec e

class GenSymSimpleConstrainedNoSpec a where
  simpleFreshConstrainedNoSpec ::
    ( MonadFresh m,
      MonadError e m,
      MonadUnion m
    ) =>
    e ->
    m (a c)

instance GenSymSimpleConstrainedNoSpec U1 where
  simpleFreshConstrainedNoSpec _ = return U1

instance (GenSymSimpleConstrained () c) => GenSymSimpleConstrainedNoSpec (K1 i c) where
  simpleFreshConstrainedNoSpec e = K1 <$> simpleFreshConstrained e ()

instance (GenSymSimpleConstrainedNoSpec a) => GenSymSimpleConstrainedNoSpec (M1 i c a) where
  simpleFreshConstrainedNoSpec e = M1 <$> simpleFreshConstrainedNoSpec e

instance
  (GenSymSimpleConstrainedNoSpec a, GenSymSimpleConstrainedNoSpec b) =>
  GenSymSimpleConstrainedNoSpec (a :*: b)
  where
  simpleFreshConstrainedNoSpec e = do
    l :: a c <- simpleFreshConstrainedNoSpec e
    r :: b c <- simpleFreshConstrainedNoSpec e
    return $ l :*: r

-- | We cannot provide DerivingVia style derivation for 'GenSymSimpleConstrained', while
-- you can use this 'simpleFreshConstrained' implementation to implement 'GenSymSimpleConstrained' fo
-- your own types.
--
-- This 'simpleFreshConstrained' implementation is for the types that does not need any specification.
-- It will generate product types by generating each fields with () as specification.
-- It will not work on sum types.
--
-- __Note:__ __Never__ use on recursive types.
derivedSimpleFreshConstrainedNoSpec ::
  forall a m e.
  ( Generic a,
    GenSymSimpleConstrainedNoSpec (Rep a),
    MonadFresh m,
    MonadError e m,
    MonadUnion m,
    Mergeable a
  ) =>
  e ->
  () ->
  m a
derivedSimpleFreshConstrainedNoSpec e _ = tryMerge $ (tryMerge . fmap to) $ simpleFreshConstrainedNoSpec e

class GenSymConstrainedSameShape a where
  simpleFreshConstrainedSameShape ::
    ( MonadFresh m,
      MonadError e m,
      MonadUnion m
    ) =>
    e ->
    a c ->
    m (a c)

instance GenSymConstrainedSameShape U1 where
  simpleFreshConstrainedSameShape _ _ = return U1

instance (GenSymSimpleConstrained c c) => GenSymConstrainedSameShape (K1 i c) where
  simpleFreshConstrainedSameShape e (K1 c) = K1 <$> simpleFreshConstrained e c

instance (GenSymConstrainedSameShape a) => GenSymConstrainedSameShape (M1 i c a) where
  simpleFreshConstrainedSameShape e (M1 a) = M1 <$> simpleFreshConstrainedSameShape e a

instance
  (GenSymConstrainedSameShape a, GenSymConstrainedSameShape b) =>
  GenSymConstrainedSameShape (a :+: b)
  where
  simpleFreshConstrainedSameShape e (L1 a) = L1 <$> simpleFreshConstrainedSameShape e a
  simpleFreshConstrainedSameShape e (R1 a) = R1 <$> simpleFreshConstrainedSameShape e a

instance
  (GenSymConstrainedSameShape a, GenSymConstrainedSameShape b) =>
  GenSymConstrainedSameShape (a :*: b)
  where
  simpleFreshConstrainedSameShape e (a :*: b) = do
    l :: a c <- simpleFreshConstrainedSameShape e a
    r :: b c <- simpleFreshConstrainedSameShape e b
    return $ l :*: r

-- | We cannot provide DerivingVia style derivation for 'GenSymSimpleConstrained', while
-- you can use this 'simpleFreshConstrained' implementation to implement 'GenSymSimpleConstrained' fo
-- your own types.
--
-- This 'simpleFreshConstrained' implementation is for the types that can be generated with
-- a reference value of the same type.
--
-- For sum types, it will generate the result with the same data constructor.
-- For product types, it will generate the result by generating each field with
-- the corresponding reference value.
--
-- __Note:__ __Can__ be used on recursive types.
derivedSimpleFreshConstrainedSameShape ::
  ( Generic a,
    GenSymConstrainedSameShape (Rep a),
    Mergeable a,
    MonadFresh m,
    MonadError e m,
    MonadUnion m
  ) =>
  e ->
  a ->
  m a
derivedSimpleFreshConstrainedSameShape e a = tryMerge $ (tryMerge . fmap to) $ simpleFreshConstrainedSameShape e (from a)
