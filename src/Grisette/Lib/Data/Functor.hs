-- |
-- Module      :   Grisette.Lib.Control.Functor
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Lib.Data.Functor
  ( mrgFmap,
    (.<$),
    (.$>),
    (.<$>),
    (.<&>),
    mrgUnzip,
    mrgVoid,
  )
where

import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.TryMerge (TryMerge)
import qualified Grisette.Unified.Lib.Data.Functor as Unified

-- | 'fmap' with 'Grisette.Core.MergingStrategy' knowledge propagation.
mrgFmap ::
  (TryMerge f, Mergeable a, Mergeable b, Functor f) =>
  (a -> b) ->
  f a ->
  f b
mrgFmap = Unified.mrgFmap
{-# INLINE mrgFmap #-}

infixl 4 .<$>

-- | '<$>' with 'Grisette.Core.MergingStrategy' knowledge propagation.
(.<$>) ::
  (TryMerge f, Mergeable a, Mergeable b, Functor f) => (a -> b) -> f a -> f b
(.<$>) = (Unified..<$>)
{-# INLINE (.<$>) #-}

infixl 4 .<$

-- | '<$' with 'Grisette.Core.MergingStrategy' knowledge propagation.
(.<$) :: (TryMerge f, Mergeable a, Mergeable b, Functor f) => b -> f a -> f b
(.<$) = (Unified..<$)
{-# INLINE (.<$) #-}

infixl 4 .$>

-- | 'Data.Functor.$>' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
(.$>) :: (TryMerge f, Mergeable a, Mergeable b, Functor f) => f a -> b -> f b
(.$>) = (Unified..$>)
{-# INLINE (.$>) #-}

infixl 1 .<&>

-- | 'Data.Functor.<&>' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
(.<&>) ::
  (TryMerge f, Mergeable a, Mergeable b, Functor f) =>
  f a ->
  (a -> b) ->
  f b
(.<&>) = (Unified..<&>)
{-# INLINE (.<&>) #-}

-- | 'unzip' with 'Grisette.Core.MergingStrategy' knowledge propagation.
mrgUnzip ::
  (TryMerge f, Mergeable a, Mergeable b, Functor f) =>
  f (a, b) ->
  (f a, f b)
mrgUnzip = Unified.mrgUnzip
{-# INLINE mrgUnzip #-}

-- | 'Data.Functor.void' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgVoid :: (TryMerge f, Functor f) => f a -> f ()
mrgVoid = Unified.mrgVoid
{-# INLINE mrgVoid #-}
