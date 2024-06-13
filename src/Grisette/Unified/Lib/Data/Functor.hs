-- |
-- Module      :   Grisette.Unified.Lib.Control.Functor
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Unified.Lib.Data.Functor
  ( mrgFmap,
    (.<$),
    (.$>),
    (.<$>),
    (.<&>),
    mrgUnzip,
    mrgVoid,
  )
where

import Control.Monad (void)
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.TryMerge (TryMerge, tryMerge)

-- | 'fmap' with 'MergingStrategy' knowledge propagation.
mrgFmap ::
  (TryMerge f, Mergeable a, Mergeable b, Functor f) =>
  (a -> b) ->
  f a ->
  f b
mrgFmap f a = tryMerge $ fmap f (tryMerge a)
{-# INLINE mrgFmap #-}

infixl 4 .<$>

-- | '<$>' with 'MergingStrategy' knowledge propagation.
(.<$>) ::
  (TryMerge f, Mergeable a, Mergeable b, Functor f) => (a -> b) -> f a -> f b
(.<$>) = mrgFmap
{-# INLINE (.<$>) #-}

infixl 4 .<$

-- | '<$' with 'MergingStrategy' knowledge propagation.
(.<$) :: (TryMerge f, Mergeable a, Mergeable b, Functor f) => b -> f a -> f b
(.<$) v f = tryMerge $ v <$ tryMerge f
{-# INLINE (.<$) #-}

infixl 4 .$>

-- | '$>' with 'MergingStrategy' knowledge propagation.
(.$>) :: (TryMerge f, Mergeable a, Mergeable b, Functor f) => f a -> b -> f b
(.$>) = flip (.<$)
{-# INLINE (.$>) #-}

infixl 1 .<&>

-- | '<&>' with 'MergingStrategy' knowledge propagation.
(.<&>) ::
  (TryMerge f, Mergeable a, Mergeable b, Functor f) =>
  f a ->
  (a -> b) ->
  f b
(.<&>) = flip mrgFmap
{-# INLINE (.<&>) #-}

-- | 'unzip' with 'MergingStrategy' knowledge propagation.
mrgUnzip ::
  (TryMerge f, Mergeable a, Mergeable b, Functor f) =>
  f (a, b) ->
  (f a, f b)
mrgUnzip ab =
  let mergedAb = tryMerge ab
   in (fst .<$> mergedAb, snd .<$> mergedAb)
{-# INLINE mrgUnzip #-}

-- | 'void' with 'MergingStrategy' knowledge propagation.
mrgVoid :: (TryMerge f, Functor f) => f a -> f ()
mrgVoid x = tryMerge $ void x
{-# INLINE mrgVoid #-}
