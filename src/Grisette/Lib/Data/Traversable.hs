{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.Lib.Control.Traversable
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Lib.Data.Traversable
  ( -- * The 'Traversable' class
    mrgTraverse,
    mrgSequenceA,
    mrgMapM,
    mrgSequence,

    -- * Utility functions
    mrgFor,
    mrgForM,
    mrgMapAccumM,
    mrgForAccumM,
  )
where

import Control.Monad.State (StateT (StateT, runStateT))
import Grisette.Core.Data.Class.Mergeable
  ( Mergeable (rootStrategy),
    Mergeable1,
    Mergeable2 (liftRootStrategy2),
    rootStrategy1,
  )
import Grisette.Core.Data.Class.TryMerge
  ( MonadTryMerge,
    TryMerge (tryMergeWithStrategy),
    tryMerge,
  )
import Grisette.Lib.Control.Applicative (mrgPure)

-- | 'Data.Traversable.traverse' with 'MergingStrategy' knowledge propagation.
mrgTraverse ::
  forall a b t f.
  ( Mergeable b,
    Mergeable1 t,
    TryMerge f,
    Applicative f,
    Traversable t
  ) =>
  (a -> f b) ->
  t a ->
  f (t b)
mrgTraverse f = tryMergeWithStrategy rootStrategy1 . traverse (tryMerge . f)
{-# INLINE mrgTraverse #-}

-- | 'Data.Traversable.sequenceA' with 'MergingStrategy' knowledge propagation.
mrgSequenceA ::
  forall a t f.
  ( Mergeable a,
    Mergeable1 t,
    Applicative f,
    TryMerge f,
    Traversable t
  ) =>
  t (f a) ->
  f (t a)
mrgSequenceA = mrgTraverse id
{-# INLINE mrgSequenceA #-}

-- | 'Data.Traversable.mapM' with 'MergingStrategy' knowledge propagation.
mrgMapM ::
  forall a b t f.
  ( Mergeable b,
    Mergeable1 t,
    MonadTryMerge f,
    Traversable t
  ) =>
  (a -> f b) ->
  t a ->
  f (t b)
mrgMapM = mrgTraverse
{-# INLINE mrgMapM #-}

-- | 'Data.Traversable.sequence' with 'MergingStrategy' knowledge propagation.
mrgSequence ::
  forall a t f.
  ( Mergeable a,
    Mergeable1 t,
    MonadTryMerge f,
    Traversable t
  ) =>
  t (f a) ->
  f (t a)
mrgSequence = mrgSequenceA
{-# INLINE mrgSequence #-}

-- | 'Data.Traversable.for' with 'MergingStrategy' knowledge propagation.
mrgFor ::
  ( Mergeable b,
    Mergeable1 t,
    Traversable t,
    TryMerge m,
    Applicative m
  ) =>
  t a ->
  (a -> m b) ->
  m (t b)
mrgFor = flip mrgTraverse
{-# INLINE mrgFor #-}

-- | 'Data.Traversable.forM' with 'MergingStrategy' knowledge propagation.
mrgForM ::
  ( Mergeable b,
    Mergeable1 t,
    Traversable t,
    MonadTryMerge m
  ) =>
  t a ->
  (a -> m b) ->
  m (t b)
mrgForM = flip mrgMapM
{-# INLINE mrgForM #-}

-- | 'Data.Traversable.mapAccumM' with 'MergingStrategy' knowledge propagation.
mrgMapAccumM ::
  (MonadTryMerge m, Traversable t, Mergeable s, Mergeable b, Mergeable1 t) =>
  (s -> a -> m (s, b)) ->
  s ->
  t a ->
  m (s, t b)
mrgMapAccumM f s t =
  tryMergeWithStrategy (liftRootStrategy2 rootStrategy rootStrategy1) $ do
    (tb, s) <- flip runStateT s $ do
      mrgMapM
        ( \a -> StateT $ \s -> do
            (sr, br) <- f s a
            mrgPure (br, sr)
        )
        t
    return (s, tb)
{-# INLINE mrgMapAccumM #-}

-- | 'Data.Traversable.forAccumM' and 'MergingStrategy' knowledge propagation.
mrgForAccumM ::
  (MonadTryMerge m, Traversable t, Mergeable s, Mergeable b, Mergeable1 t) =>
  s ->
  t a ->
  (s -> a -> m (s, b)) ->
  m (s, t b)
mrgForAccumM s t f = mrgMapAccumM f s t
{-# INLINE mrgForAccumM #-}
