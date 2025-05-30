{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :   Grisette.Unified.Lib.Data.Foldable
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
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

#if MIN_VERSION_base(4,20,0)
#else
import Data.Foldable (Foldable (foldl'))
#endif

import Control.Monad (MonadPlus)
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
import Grisette.Internal.Unified.Class.UnifiedSymEq (UnifiedSymEq, (.==))
import Grisette.Internal.Unified.Class.UnifiedSymOrd (mrgMax, mrgMin)
import Grisette.Internal.Unified.UnifiedBool (GetBool)
import Grisette.Internal.Unified.UnifiedData (GetData)
import Grisette.Lib.Control.Applicative (mrgAsum, mrgPure, (.*>))
import {-# SOURCE #-} Grisette.Lib.Control.Monad
  ( mrgMplus,
    mrgMzero,
    (.>>),
  )
import Grisette.Lib.Data.Functor (mrgFmap, mrgVoid)
import Grisette.Unified
  ( EvalModeBase,
    UnifiedBranching,
    UnifiedITEOp,
    UnifiedSymOrd,
    liftUnion,
    mrgIf,
    symIteMerge,
  )

-- | 'Data.Foldable.elem' with symbolic equality.
symElem ::
  forall mode t a.
  (Foldable t, EvalModeBase mode, UnifiedSymEq mode a) =>
  a ->
  t a ->
  GetBool mode
symElem x = symAny ((.== x))
{-# INLINE symElem #-}

-- | 'Data.Foldable.maximum' with unified comparison.
mrgMaximum ::
  forall mode a t m.
  ( Foldable t,
    EvalModeBase mode,
    UnifiedBranching mode m,
    MonadTryMerge m,
    Mergeable a,
    UnifiedSymOrd mode a
  ) =>
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

-- | 'Data.Foldable.maximum' with result merged with 'Grisette.Core.ITEOp'.
symMaximum ::
  forall mode a t.
  ( Foldable t,
    Mergeable a,
    UnifiedSymOrd mode a,
    UnifiedITEOp mode a,
    EvalModeBase mode
  ) =>
  t a ->
  a
symMaximum l = symIteMerge @mode (mrgMaximum @mode l :: GetData mode a)
{-# INLINE symMaximum #-}

-- | 'Data.Foldable.minimum' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgMinimum ::
  forall mode a t m.
  ( Foldable t,
    EvalModeBase mode,
    UnifiedBranching mode m,
    MonadTryMerge m,
    Mergeable a,
    UnifiedSymOrd mode a
  ) =>
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

-- | 'Data.Foldable.maximum' with result merged with 'Grisette.Core.ITEOp'.
symMinimum ::
  forall mode a t.
  ( Foldable t,
    Mergeable a,
    UnifiedSymOrd mode a,
    UnifiedITEOp mode a,
    EvalModeBase mode
  ) =>
  t a ->
  a
symMinimum l = symIteMerge @mode (mrgMinimum @mode l :: GetData mode a)
{-# INLINE symMinimum #-}

-- | 'Data.Foldable.foldrM' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
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

-- | 'Data.Foldable.foldlM' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
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

-- | 'Data.Foldable.traverse_' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgTraverse_ ::
  (Applicative m, TryMerge m, Foldable t) => (a -> m b) -> t a -> m ()
mrgTraverse_ f = foldr c (mrgPure ())
  where
    c x k = mrgVoid (f x) .*> k
{-# INLINE mrgTraverse_ #-}

-- | 'Data.Foldable.for_' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgFor_ ::
  (Applicative m, TryMerge m, Foldable t) => t a -> (a -> m b) -> m ()
mrgFor_ = flip mrgTraverse_
{-# INLINE mrgFor_ #-}

-- | 'Data.Foldable.sequence_' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgSequenceA_ ::
  (Foldable t, TryMerge m, Applicative m) => t (m a) -> m ()
mrgSequenceA_ = foldr c (mrgPure ())
  where
    c m k = mrgVoid m .*> k
{-# INLINE mrgSequenceA_ #-}

-- | 'Data.Foldable.mapM_' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgMapM_ :: (MonadTryMerge m, Foldable t) => (a -> m b) -> t a -> m ()
mrgMapM_ = mrgTraverse_
{-# INLINE mrgMapM_ #-}

-- | 'Data.Foldable.forM_' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgForM_ :: (MonadTryMerge m, Foldable t) => t a -> (a -> m b) -> m ()
mrgForM_ = flip mrgMapM_
{-# INLINE mrgForM_ #-}

-- | 'Data.Foldable.sequence_' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgSequence_ :: (Foldable t, MonadTryMerge m) => t (m a) -> m ()
mrgSequence_ = foldr c (mrgPure ())
  where
    c m k = mrgVoid m .>> k
{-# INLINE mrgSequence_ #-}

-- | 'Data.Foldable.msum' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgMsum ::
  (MonadTryMerge m, Mergeable a, MonadPlus m, Foldable t) => t (m a) -> m a
mrgMsum = foldr mrgMplus mrgMzero
{-# INLINE mrgMsum #-}

-- | 'Data.Foldable.and' on unified boolean.
symAnd :: (EvalModeBase mode, Foldable t) => t (GetBool mode) -> GetBool mode
symAnd = foldl' (.&&) (toSym True)
{-# INLINE symAnd #-}

-- | 'Data.Foldable.or' on unified boolean.
symOr :: (EvalModeBase mode, Foldable t) => t (GetBool mode) -> GetBool mode
symOr = foldl' (.||) (toSym False)
{-# INLINE symOr #-}

-- | 'Data.Foldable.any' on unified boolean.
symAny ::
  (EvalModeBase mode, Foldable t) => (a -> GetBool mode) -> t a -> GetBool mode
symAny f = foldl' (\acc v -> acc .|| f v) (toSym False)
{-# INLINE symAny #-}

-- | 'Data.Foldable.all' on unified boolean.
symAll ::
  (EvalModeBase mode, Foldable t) => (a -> GetBool mode) -> t a -> GetBool mode
symAll f = foldl' (\acc v -> acc .&& f v) (toSym True)
{-# INLINE symAll #-}

-- | 'Data.Foldable.maximumBy' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgMaximumBy ::
  forall mode t a m.
  ( Foldable t,
    Mergeable a,
    EvalModeBase mode,
    MonadTryMerge m,
    UnifiedBranching mode m
  ) =>
  (a -> a -> GetData mode Ordering) ->
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
          cmpRes <- liftUnion @mode $ cmp x y
          case cmpRes of
            GT -> mrgSingle $ Just x
            _ -> mrgSingle $ Just y

-- | 'Data.Foldable.maximumBy' with result merged with 'Grisette.Core.ITEOp'.
symMaximumBy ::
  forall mode t a.
  (Foldable t, Mergeable a, UnifiedITEOp mode a, EvalModeBase mode) =>
  (a -> a -> GetData mode Ordering) ->
  t a ->
  a
symMaximumBy cmp l = symIteMerge @mode (mrgMaximumBy cmp l :: GetData mode a)
{-# INLINE symMaximumBy #-}

-- | 'Data.Foldable.minimumBy' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgMinimumBy ::
  forall mode t a m.
  ( Foldable t,
    Mergeable a,
    EvalModeBase mode,
    MonadTryMerge m,
    UnifiedBranching mode m
  ) =>
  (a -> a -> GetData mode Ordering) ->
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
          cmpRes <- liftUnion @mode $ cmp x y
          case cmpRes of
            GT -> mrgSingle $ Just y
            _ -> mrgSingle $ Just x

-- | 'Data.Foldable.minimumBy' with result merged with 'Grisette.Core.ITEOp'.
symMinimumBy ::
  forall mode t a.
  (Foldable t, Mergeable a, UnifiedITEOp mode a, EvalModeBase mode) =>
  (a -> a -> GetData mode Ordering) ->
  t a ->
  a
symMinimumBy cmp l = symIteMerge @mode (mrgMinimumBy cmp l :: GetData mode a)
{-# INLINE symMinimumBy #-}

-- | 'Data.Foldable.elem' with symbolic equality.
symNotElem ::
  (Foldable t, UnifiedSymEq mode a, EvalModeBase mode) =>
  a ->
  t a ->
  GetBool mode
symNotElem x = symNot . symElem x
{-# INLINE symNotElem #-}

-- | 'Data.Foldable.elem' with symbolic equality and
-- 'Grisette.Core.MergingStrategy' knowledge propagation.
mrgFind ::
  ( Foldable t,
    EvalModeBase mode,
    MonadTryMerge m,
    UnifiedBranching mode m,
    Mergeable a
  ) =>
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
