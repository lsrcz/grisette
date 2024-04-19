{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.Lib.Control.Foldable
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Lib.Data.Foldable
  ( symElem,
    symMaximum,
    mrgMaximum,
    symMinimum,
    mrgMinimum,

    -- * Special biased folds
    mrgFoldrM,
    mrgFoldlM,

    -- * Folding actions

    -- ** Applicative actions
    mrgTraverse_,
    mrgFor_,
    mrgSequenceA_,
    mrgAsum,

    -- ** Monadic actions
    mrgMapM_,
    mrgForM_,
    mrgSequence_,
    mrgMsum,

    -- ** Specialized folds
    symAnd,
    symOr,
    symAny,
    symAll,
    symMaximumBy,
    mrgMaximumBy,
    symMinimumBy,
    mrgMinimumBy,

    -- ** Searches
    symNotElem,
    mrgFind,
  )
where

import Control.Monad (MonadPlus)
import Data.Foldable (Foldable (foldl'))
import Grisette.Internal.Core.Control.Monad.Union (MonadUnion)
import Grisette.Internal.Core.Control.Monad.UnionM (UnionM, liftUnionM)
import Grisette.Internal.Core.Data.Class.ITEOp (ITEOp)
import Grisette.Internal.Core.Data.Class.LogicalOp (LogicalOp (symNot, (.&&), (.||)))
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.PlainUnion (symIteMerge)
import Grisette.Internal.Core.Data.Class.SEq (SEq ((.==)))
import Grisette.Internal.Core.Data.Class.SOrd (SOrd, mrgMax, mrgMin)
import Grisette.Internal.Core.Data.Class.SimpleMergeable (mrgIf)
import Grisette.Internal.Core.Data.Class.Solvable (Solvable (con))
import Grisette.Internal.Core.Data.Class.TryMerge
  ( MonadTryMerge,
    TryMerge,
    tryMerge,
  )
import Grisette.Internal.SymPrim.SymBool (SymBool)
import Grisette.Lib.Control.Applicative (mrgAsum, mrgPure, (.*>))
import {-# SOURCE #-} Grisette.Lib.Control.Monad
  ( mrgMplus,
    mrgMzero,
    mrgReturn,
    (.>>),
  )
import Grisette.Lib.Data.Functor (mrgFmap, mrgVoid)

-- | 'Data.Foldable.elem' with symbolic equality.
symElem :: (Foldable t, SEq a) => a -> t a -> SymBool
symElem x = symAny ((.== x))
{-# INLINE symElem #-}

-- | 'Data.Foldable.maximum' with 'MergingStrategy' knowledge propagation.
mrgMaximum ::
  forall a t m.
  (Foldable t, MonadUnion m, Mergeable a, SOrd a) =>
  t a ->
  m a
mrgMaximum l = do
  r <- mrgFoldlM symMax' (Nothing :: Maybe a) l
  case r of
    Nothing -> errorWithoutStackTrace "mrgMaximum: empty structure"
    Just x -> mrgReturn x
  where
    symMax' :: Maybe a -> a -> m (Maybe a)
    symMax' mx y =
      case mx of
        Nothing -> mrgReturn $ Just y
        Just x -> mrgFmap Just $ mrgMax x y

-- | 'Data.Foldable.maximum' with result merged with 'ITEOp'.
symMaximum ::
  forall a t.
  (Foldable t, Mergeable a, SOrd a, ITEOp a) =>
  t a ->
  a
symMaximum l = symIteMerge (mrgMaximum l :: UnionM a)
{-# INLINE symMaximum #-}

-- | 'Data.Foldable.minimum' with 'MergingStrategy' knowledge propagation.
mrgMinimum ::
  forall a t m.
  (Foldable t, MonadUnion m, Mergeable a, SOrd a) =>
  t a ->
  m a
mrgMinimum l = do
  r <- mrgFoldlM symMin' (Nothing :: Maybe a) l
  case r of
    Nothing -> errorWithoutStackTrace "mrgMinimum: empty structure"
    Just x -> mrgReturn x
  where
    symMin' :: Maybe a -> a -> m (Maybe a)
    symMin' mx y =
      case mx of
        Nothing -> mrgReturn $ Just y
        Just x -> mrgFmap Just $ mrgMin x y

-- | 'Data.Foldable.minimum' with result merged with 'ITEOp'.
symMinimum ::
  forall a t.
  (Foldable t, Mergeable a, SOrd a, ITEOp a) =>
  t a ->
  a
symMinimum l = symIteMerge (mrgMinimum l :: UnionM a)
{-# INLINE symMinimum #-}

-- | 'Data.Foldable.foldrM' with 'MergingStrategy' knowledge propagation.
mrgFoldrM ::
  (MonadTryMerge m, Mergeable b, Foldable t) =>
  (a -> b -> m b) ->
  b ->
  t a ->
  m b
mrgFoldrM f z0 xs = foldl c mrgPure xs z0
  where
    c k x z = tryMerge (f x z) >>= k
{-# INLINE mrgFoldrM #-}

-- | 'Data.Foldable.foldlM' with 'MergingStrategy' knowledge propagation.
mrgFoldlM ::
  (MonadTryMerge m, Mergeable b, Foldable t) =>
  (b -> a -> m b) ->
  b ->
  t a ->
  m b
mrgFoldlM f z0 xs = foldr c mrgPure xs z0
  where
    c x k z = tryMerge (f z x) >>= k
{-# INLINE mrgFoldlM #-}

-- | 'Data.Foldable.traverse_' with 'MergingStrategy' knowledge propagation.
mrgTraverse_ ::
  (Applicative m, TryMerge m, Foldable t) => (a -> m b) -> t a -> m ()
mrgTraverse_ f = foldr c (mrgPure ())
  where
    c x k = mrgVoid (f x) .*> k
{-# INLINE mrgTraverse_ #-}

-- | 'Data.Foldable.for_' with 'MergingStrategy' knowledge propagation.
mrgFor_ ::
  (Applicative m, TryMerge m, Foldable t) => t a -> (a -> m b) -> m ()
mrgFor_ = flip mrgTraverse_
{-# INLINE mrgFor_ #-}

-- | 'Data.Foldable.sequence_' with 'MergingStrategy' knowledge propagation.
mrgSequenceA_ ::
  (Foldable t, TryMerge m, Applicative m) => t (m a) -> m ()
mrgSequenceA_ = foldr c (mrgPure ())
  where
    c m k = mrgVoid m .*> k
{-# INLINE mrgSequenceA_ #-}

-- | 'Data.Foldable.mapM_' with 'MergingStrategy' knowledge propagation.
mrgMapM_ :: (MonadTryMerge m, Foldable t) => (a -> m b) -> t a -> m ()
mrgMapM_ = mrgTraverse_
{-# INLINE mrgMapM_ #-}

-- | 'Data.Foldable.forM_' with 'MergingStrategy' knowledge propagation.
mrgForM_ :: (MonadTryMerge m, Foldable t) => t a -> (a -> m b) -> m ()
mrgForM_ = flip mrgMapM_
{-# INLINE mrgForM_ #-}

-- | 'Data.Foldable.sequence_' with 'MergingStrategy' knowledge propagation.
mrgSequence_ :: (Foldable t, MonadTryMerge m) => t (m a) -> m ()
mrgSequence_ = foldr c (mrgPure ())
  where
    c m k = mrgVoid m .>> k
{-# INLINE mrgSequence_ #-}

-- | 'Data.Foldable.msum' with 'MergingStrategy' knowledge propagation.
mrgMsum ::
  (MonadTryMerge m, Mergeable a, MonadPlus m, Foldable t) => t (m a) -> m a
mrgMsum = foldr mrgMplus mrgMzero
{-# INLINE mrgMsum #-}

-- | 'Data.Foldable.and' on symbolic boolean.
symAnd :: (Foldable t) => t SymBool -> SymBool
symAnd = foldl' (.&&) (con True)

-- | 'Data.Foldable.or' on symbolic boolean.
symOr :: (Foldable t) => t SymBool -> SymBool
symOr = foldl' (.||) (con False)

-- | 'Data.Foldable.any' on symbolic boolean.
symAny :: (Foldable t) => (a -> SymBool) -> t a -> SymBool
symAny f = foldl' (\acc v -> acc .|| f v) (con False)

-- | 'Data.Foldable.all' on symbolic boolean.
symAll :: (Foldable t) => (a -> SymBool) -> t a -> SymBool
symAll f = foldl' (\acc v -> acc .&& f v) (con True)

-- | 'Data.Foldable.maximumBy' with 'MergingStrategy' knowledge propagation.
mrgMaximumBy ::
  forall t a m.
  (Foldable t, Mergeable a, MonadUnion m) =>
  (a -> a -> UnionM Ordering) ->
  t a ->
  m a
mrgMaximumBy cmp l = do
  r <- mrgFoldlM symMax' (Nothing :: Maybe a) l
  case r of
    Nothing -> errorWithoutStackTrace "mrgMaximumBy: empty structure"
    Just x -> mrgReturn x
  where
    symMax' :: Maybe a -> a -> m (Maybe a)
    symMax' mx y =
      case mx of
        Nothing -> mrgReturn $ Just y
        Just x -> do
          cmpRes <- liftUnionM $ cmp x y
          case cmpRes of
            GT -> mrgReturn $ Just x
            _ -> mrgReturn $ Just y

-- | 'Data.Foldable.maximumBy' with result merged with 'ITEOp'.
symMaximumBy ::
  forall t a.
  (Foldable t, Mergeable a, ITEOp a) =>
  (a -> a -> UnionM Ordering) ->
  t a ->
  a
symMaximumBy cmp l = symIteMerge (mrgMaximumBy cmp l :: UnionM a)
{-# INLINE symMaximumBy #-}

-- | 'Data.Foldable.minimumBy' with 'MergingStrategy' knowledge propagation.
mrgMinimumBy ::
  forall t a m.
  (Foldable t, Mergeable a, MonadUnion m) =>
  (a -> a -> UnionM Ordering) ->
  t a ->
  m a
mrgMinimumBy cmp l = do
  r <- mrgFoldlM symMin' (Nothing :: Maybe a) l
  case r of
    Nothing -> errorWithoutStackTrace "mrgMinimumBy: empty structure"
    Just x -> mrgReturn x
  where
    symMin' :: Maybe a -> a -> m (Maybe a)
    symMin' mx y =
      case mx of
        Nothing -> mrgReturn $ Just y
        Just x -> do
          cmpRes <- liftUnionM $ cmp x y
          case cmpRes of
            GT -> mrgReturn $ Just y
            _ -> mrgReturn $ Just x

-- | 'Data.Foldable.minimumBy' with result merged with 'ITEOp'.
symMinimumBy ::
  forall t a.
  (Foldable t, Mergeable a, ITEOp a) =>
  (a -> a -> UnionM Ordering) ->
  t a ->
  a
symMinimumBy cmp l = symIteMerge (mrgMinimumBy cmp l :: UnionM a)
{-# INLINE symMinimumBy #-}

-- | 'Data.Foldable.elem' with symbolic equality.
symNotElem :: (Foldable t, SEq a) => a -> t a -> SymBool
symNotElem x = symNot . symElem x
{-# INLINE symNotElem #-}

-- | 'Data.Foldable.elem' with symbolic equality and 'MergingStrategy' knowledge
-- propagation.
mrgFind ::
  (Foldable t, MonadUnion m, Mergeable a) =>
  (a -> SymBool) ->
  t a ->
  m (Maybe a)
mrgFind f = mrgFoldlM fst (Nothing :: Maybe a)
  where
    fst acc v = do
      case acc of
        Just _ -> mrgPure acc
        Nothing -> do
          mrgIf (f v) (mrgPure $ Just v) (mrgPure Nothing)
