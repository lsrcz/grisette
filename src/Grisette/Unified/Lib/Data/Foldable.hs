{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Unified.Lib.Data.Foldable
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

    -- * Folding actions
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
import Grisette.Internal.Core.Data.Class.LogicalOp
  ( LogicalOp (symNot, (.&&), (.||)),
  )
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.ToSym (ToSym (toSym))
import Grisette.Internal.Core.Data.Class.TryMerge
  ( MonadTryMerge,
    TryMerge,
    mrgSingle,
    tryMerge,
  )
import Grisette.Unified
  ( MonadWithMode,
    UnifiedITEOp,
    UnifiedSymOrd,
    liftBaseMonad,
    mrgIf,
    symIteMerge,
  )
import Grisette.Unified.Internal.BaseMonad (BaseMonad)
import Grisette.Unified.Internal.Class.UnifiedSymEq (UnifiedSymEq, (.==))
import Grisette.Unified.Internal.Class.UnifiedSymOrd (mrgMax, mrgMin)
import Grisette.Unified.Internal.IsMode (IsMode)
import Grisette.Unified.Internal.UnifiedBool (GetBool)
import Grisette.Unified.Lib.Control.Applicative (mrgAsum, mrgPure, (.*>))
import {-# SOURCE #-} Grisette.Unified.Lib.Control.Monad
  ( mrgMplus,
    mrgMzero,
    (.>>),
  )
import Grisette.Unified.Lib.Data.Functor (mrgFmap, mrgVoid)

-- | 'Data.Foldable.elem' with symbolic equality.
symElem ::
  forall mode t a.
  (Foldable t, IsMode mode, UnifiedSymEq mode a) =>
  a ->
  t a ->
  GetBool mode
symElem x = symAny ((.== x))
{-# INLINE symElem #-}

-- | 'Data.Foldable.maximum' with unified comparison.
mrgMaximum ::
  forall mode a t m.
  (Foldable t, MonadWithMode mode m, Mergeable a, UnifiedSymOrd mode a) =>
  t a ->
  m a
mrgMaximum l = do
  r <- mrgFoldlM symMax' (Nothing :: Maybe a) l
  case r of
    Nothing -> errorWithoutStackTrace "mrgMaximum: empty structure"
    Just x -> mrgPure x
  where
    symMax' :: Maybe a -> a -> m (Maybe a)
    symMax' mx y =
      case mx of
        Nothing -> mrgPure $ Just y
        Just x -> mrgFmap Just $ mrgMax @mode x y
{-# INLINE mrgMaximum #-}

-- | 'Data.Foldable.maximum' with result merged with 'ITEOp'.
symMaximum ::
  forall mode a t.
  ( Foldable t,
    Mergeable a,
    UnifiedSymOrd mode a,
    UnifiedITEOp mode a,
    IsMode mode
  ) =>
  t a ->
  a
symMaximum l = symIteMerge (mrgMaximum @mode l :: BaseMonad mode a)
{-# INLINE symMaximum #-}

-- | 'Data.Foldable.minimum' with 'MergingStrategy' knowledge propagation.
mrgMinimum ::
  forall mode a t m.
  (Foldable t, MonadWithMode mode m, Mergeable a, UnifiedSymOrd mode a) =>
  t a ->
  m a
mrgMinimum l = do
  r <- mrgFoldlM symMin' (Nothing :: Maybe a) l
  case r of
    Nothing -> errorWithoutStackTrace "mrgMinimum: empty structure"
    Just x -> mrgPure x
  where
    symMin' :: Maybe a -> a -> m (Maybe a)
    symMin' mx y =
      case mx of
        Nothing -> mrgPure $ Just y
        Just x -> mrgFmap Just $ mrgMin @mode x y

-- | 'Data.Foldable.maximum' with result merged with 'ITEOp'.
symMinimum ::
  forall mode a t.
  ( Foldable t,
    Mergeable a,
    UnifiedSymOrd mode a,
    UnifiedITEOp mode a,
    IsMode mode
  ) =>
  t a ->
  a
symMinimum l = symIteMerge (mrgMinimum @mode l :: BaseMonad mode a)
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

-- | 'Data.Foldable.and' on unified boolean.
symAnd :: (IsMode mode, Foldable t) => t (GetBool mode) -> GetBool mode
symAnd = foldl' (.&&) (toSym True)
{-# INLINE symAnd #-}

-- | 'Data.Foldable.or' on unified boolean.
symOr :: (IsMode mode, Foldable t) => t (GetBool mode) -> GetBool mode
symOr = foldl' (.||) (toSym False)
{-# INLINE symOr #-}

-- | 'Data.Foldable.any' on unified boolean.
symAny ::
  (IsMode mode, Foldable t) => (a -> GetBool mode) -> t a -> GetBool mode
symAny f = foldl' (\acc v -> acc .|| f v) (toSym False)
{-# INLINE symAny #-}

-- | 'Data.Foldable.all' on unified boolean.
symAll ::
  (IsMode mode, Foldable t) => (a -> GetBool mode) -> t a -> GetBool mode
symAll f = foldl' (\acc v -> acc .&& f v) (toSym True)
{-# INLINE symAll #-}

-- | 'Data.Foldable.maximumBy' with 'MergingStrategy' knowledge propagation.
mrgMaximumBy ::
  forall mode t a m.
  (Foldable t, Mergeable a, MonadWithMode mode m) =>
  (a -> a -> BaseMonad mode Ordering) ->
  t a ->
  m a
mrgMaximumBy cmp l = do
  r <- mrgFoldlM symMax' (Nothing :: Maybe a) l
  case r of
    Nothing -> errorWithoutStackTrace "mrgMaximumBy: empty structure"
    Just x -> mrgSingle x
  where
    symMax' :: Maybe a -> a -> m (Maybe a)
    symMax' mx y =
      case mx of
        Nothing -> mrgSingle $ Just y
        Just x -> do
          cmpRes <- liftBaseMonad $ cmp x y
          case cmpRes of
            GT -> mrgSingle $ Just x
            _ -> mrgSingle $ Just y

-- | 'Data.Foldable.maximumBy' with result merged with 'ITEOp'.
symMaximumBy ::
  forall mode t a.
  (Foldable t, Mergeable a, UnifiedITEOp mode a, IsMode mode) =>
  (a -> a -> BaseMonad mode Ordering) ->
  t a ->
  a
symMaximumBy cmp l = symIteMerge (mrgMaximumBy cmp l :: BaseMonad mode a)
{-# INLINE symMaximumBy #-}

-- | 'Data.Foldable.minimumBy' with 'MergingStrategy' knowledge propagation.
mrgMinimumBy ::
  forall mode t a m.
  (Foldable t, Mergeable a, MonadWithMode mode m) =>
  (a -> a -> BaseMonad mode Ordering) ->
  t a ->
  m a
mrgMinimumBy cmp l = do
  r <- mrgFoldlM symMin' (Nothing :: Maybe a) l
  case r of
    Nothing -> errorWithoutStackTrace "mrgMinimumBy: empty structure"
    Just x -> mrgSingle x
  where
    symMin' :: Maybe a -> a -> m (Maybe a)
    symMin' mx y =
      case mx of
        Nothing -> mrgSingle $ Just y
        Just x -> do
          cmpRes <- liftBaseMonad $ cmp x y
          case cmpRes of
            GT -> mrgSingle $ Just y
            _ -> mrgSingle $ Just x

-- | 'Data.Foldable.minimumBy' with result merged with 'ITEOp'.
symMinimumBy ::
  forall mode t a.
  (Foldable t, Mergeable a, UnifiedITEOp mode a, IsMode mode) =>
  (a -> a -> BaseMonad mode Ordering) ->
  t a ->
  a
symMinimumBy cmp l = symIteMerge (mrgMinimumBy cmp l :: BaseMonad mode a)
{-# INLINE symMinimumBy #-}

-- | 'Data.Foldable.elem' with symbolic equality.
symNotElem ::
  (Foldable t, UnifiedSymEq mode a, IsMode mode) =>
  a ->
  t a ->
  GetBool mode
symNotElem x = symNot . symElem x
{-# INLINE symNotElem #-}

-- | 'Data.Foldable.elem' with symbolic equality and 'MergingStrategy' knowledge
-- propagation.
mrgFind ::
  (Foldable t, MonadWithMode mode m, Mergeable a) =>
  (a -> GetBool mode) ->
  t a ->
  m (Maybe a)
mrgFind f = mrgFoldlM fst (Nothing :: Maybe a)
  where
    fst acc v = do
      case acc of
        Just _ -> mrgPure acc
        Nothing -> do
          mrgIf (f v) (mrgPure $ Just v) (mrgPure Nothing)
