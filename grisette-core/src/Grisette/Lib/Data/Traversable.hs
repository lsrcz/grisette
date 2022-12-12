{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.Lib.Control.Traversable
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Lib.Data.Traversable
  ( mrgTraverse,
    mrgSequenceA,
    mrgFor,
    mrgMapM,
    mrgForM,
    mrgSequence,
  )
where

import Grisette.Core.Control.Monad.Union
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Data.Class.SimpleMergeable

-- | 'Data.Traversable.traverse' with 'GMergingStrategy' knowledge propagation.
mrgTraverse ::
  forall bool a b t f.
  ( GMergeable bool b,
    GMergeable1 bool t,
    GMonadUnion bool f,
    Traversable t
  ) =>
  (a -> f b) ->
  t a ->
  f (t b)
mrgTraverse f = mergeWithStrategy gmergingStrategy1 . traverse (merge . f)
{-# INLINE mrgTraverse #-}

-- | 'Data.Traversable.sequenceA' with 'GMergingStrategy' knowledge propagation.
mrgSequenceA ::
  forall bool a t f.
  ( GMergeable bool a,
    GMergeable1 bool t,
    GMonadUnion bool f,
    Traversable t
  ) =>
  t (f a) ->
  f (t a)
mrgSequenceA = mrgTraverse id
{-# INLINE mrgSequenceA #-}

-- | 'Data.Traversable.mapM' with 'GMergingStrategy' knowledge propagation.
mrgMapM ::
  forall bool a b t f.
  ( GMergeable bool b,
    GMergeable1 bool t,
    GMonadUnion bool f,
    Traversable t
  ) =>
  (a -> f b) ->
  t a ->
  f (t b)
mrgMapM = mrgTraverse
{-# INLINE mrgMapM #-}

-- | 'Data.Traversable.sequence' with 'GMergingStrategy' knowledge propagation.
mrgSequence ::
  forall bool a t f.
  ( GMergeable bool a,
    GMergeable1 bool t,
    GMonadUnion bool f,
    Traversable t
  ) =>
  t (f a) ->
  f (t a)
mrgSequence = mrgSequenceA
{-# INLINE mrgSequence #-}

-- | 'Data.Traversable.for' with 'GMergingStrategy' knowledge propagation.
mrgFor ::
  ( GMergeable bool b,
    GMergeable1 bool t,
    Traversable t,
    GMonadUnion bool m
  ) =>
  t a ->
  (a -> m b) ->
  m (t b)
mrgFor = flip mrgTraverse
{-# INLINE mrgFor #-}

-- | 'Data.Traversable.forM' with 'GMergingStrategy' knowledge propagation.
mrgForM ::
  ( GMergeable bool b,
    GMergeable1 bool t,
    Traversable t,
    GMonadUnion bool m
  ) =>
  t a ->
  (a -> m b) ->
  m (t b)
mrgForM = flip mrgMapM
{-# INLINE mrgForM #-}
