{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}

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
import Grisette.Internal.Core.Control.Monad.Class.Union (MonadUnion)
import Grisette.Internal.Core.Control.Monad.Union (Union)
import Grisette.Internal.Core.Data.Class.ITEOp (ITEOp)
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.SymEq (SymEq)
import Grisette.Internal.Core.Data.Class.SymOrd (SymOrd)
import Grisette.Internal.Core.Data.Class.TryMerge
  ( MonadTryMerge,
    TryMerge,
  )
import Grisette.Internal.SymPrim.SymBool (SymBool)
import Grisette.Lib.Control.Applicative (mrgAsum)
import Grisette.Unified (EvalModeTag (S))
import qualified Grisette.Unified.Lib.Data.Foldable as Unified

-- | 'Data.Foldable.elem' with symbolic equality.
symElem :: (Foldable t, SymEq a) => a -> t a -> SymBool
symElem = Unified.symElem
{-# INLINE symElem #-}

-- | 'Data.Foldable.maximum' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgMaximum ::
  forall a t m.
  (Foldable t, MonadUnion m, Mergeable a, SymOrd a) =>
  t a ->
  m a
mrgMaximum = Unified.mrgMaximum @'S
{-# INLINE mrgMaximum #-}

-- | 'Data.Foldable.maximum' with result merged with 'ITEOp'.
symMaximum ::
  forall a t.
  (Foldable t, Mergeable a, SymOrd a, ITEOp a) =>
  t a ->
  a
symMaximum = Unified.symMaximum @'S
{-# INLINE symMaximum #-}

-- | 'Data.Foldable.minimum' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgMinimum ::
  forall a t m.
  (Foldable t, MonadUnion m, Mergeable a, SymOrd a) =>
  t a ->
  m a
mrgMinimum = Unified.mrgMinimum @'S
{-# INLINE mrgMinimum #-}

-- | 'Data.Foldable.minimum' with result merged with 'ITEOp'.
symMinimum ::
  forall a t.
  (Foldable t, Mergeable a, SymOrd a, ITEOp a) =>
  t a ->
  a
symMinimum = Unified.symMinimum @'S
{-# INLINE symMinimum #-}

-- | 'Data.Foldable.foldrM' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgFoldrM ::
  (MonadTryMerge m, Mergeable b, Foldable t) =>
  (a -> b -> m b) ->
  b ->
  t a ->
  m b
mrgFoldrM = Unified.mrgFoldrM
{-# INLINE mrgFoldrM #-}

-- | 'Data.Foldable.foldlM' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgFoldlM ::
  (MonadTryMerge m, Mergeable b, Foldable t) =>
  (b -> a -> m b) ->
  b ->
  t a ->
  m b
mrgFoldlM = Unified.mrgFoldlM
{-# INLINE mrgFoldlM #-}

-- | 'Data.Foldable.traverse_' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgTraverse_ ::
  (Applicative m, TryMerge m, Foldable t) => (a -> m b) -> t a -> m ()
mrgTraverse_ = Unified.mrgTraverse_
{-# INLINE mrgTraverse_ #-}

-- | 'Data.Foldable.for_' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgFor_ ::
  (Applicative m, TryMerge m, Foldable t) => t a -> (a -> m b) -> m ()
mrgFor_ = Unified.mrgFor_
{-# INLINE mrgFor_ #-}

-- | 'Data.Foldable.sequence_' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgSequenceA_ ::
  (Foldable t, TryMerge m, Applicative m) => t (m a) -> m ()
mrgSequenceA_ = Unified.mrgSequenceA_
{-# INLINE mrgSequenceA_ #-}

-- | 'Data.Foldable.mapM_' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgMapM_ :: (MonadTryMerge m, Foldable t) => (a -> m b) -> t a -> m ()
mrgMapM_ = Unified.mrgMapM_
{-# INLINE mrgMapM_ #-}

-- | 'Data.Foldable.forM_' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgForM_ :: (MonadTryMerge m, Foldable t) => t a -> (a -> m b) -> m ()
mrgForM_ = Unified.mrgForM_
{-# INLINE mrgForM_ #-}

-- | 'Data.Foldable.sequence_' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgSequence_ :: (Foldable t, MonadTryMerge m) => t (m a) -> m ()
mrgSequence_ = Unified.mrgSequence_
{-# INLINE mrgSequence_ #-}

-- | 'Data.Foldable.msum' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgMsum ::
  (MonadTryMerge m, Mergeable a, MonadPlus m, Foldable t) => t (m a) -> m a
mrgMsum = Unified.mrgMsum
{-# INLINE mrgMsum #-}

-- | 'Data.Foldable.and' on symbolic boolean.
symAnd :: (Foldable t) => t SymBool -> SymBool
symAnd = Unified.symAnd

-- | 'Data.Foldable.or' on symbolic boolean.
symOr :: (Foldable t) => t SymBool -> SymBool
symOr = Unified.symOr

-- | 'Data.Foldable.any' on symbolic boolean.
symAny :: (Foldable t) => (a -> SymBool) -> t a -> SymBool
symAny = Unified.symAny

-- | 'Data.Foldable.all' on symbolic boolean.
symAll :: (Foldable t) => (a -> SymBool) -> t a -> SymBool
symAll = Unified.symAll

-- | 'Data.Foldable.maximumBy' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgMaximumBy ::
  forall t a m.
  (Foldable t, Mergeable a, MonadUnion m) =>
  (a -> a -> Union Ordering) ->
  t a ->
  m a
mrgMaximumBy = Unified.mrgMaximumBy
{-# INLINE mrgMaximumBy #-}

-- | 'Data.Foldable.maximumBy' with result merged with 'ITEOp'.
symMaximumBy ::
  forall t a.
  (Foldable t, Mergeable a, ITEOp a) =>
  (a -> a -> Union Ordering) ->
  t a ->
  a
symMaximumBy = Unified.symMaximumBy
{-# INLINE symMaximumBy #-}

-- | 'Data.Foldable.minimumBy' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgMinimumBy ::
  forall t a m.
  (Foldable t, Mergeable a, MonadUnion m) =>
  (a -> a -> Union Ordering) ->
  t a ->
  m a
mrgMinimumBy = Unified.mrgMinimumBy
{-# INLINE mrgMinimumBy #-}

-- | 'Data.Foldable.minimumBy' with result merged with 'ITEOp'.
symMinimumBy ::
  forall t a.
  (Foldable t, Mergeable a, ITEOp a) =>
  (a -> a -> Union Ordering) ->
  t a ->
  a
symMinimumBy = Unified.symMinimumBy
{-# INLINE symMinimumBy #-}

-- | 'Data.Foldable.elem' with symbolic equality.
symNotElem :: (Foldable t, SymEq a) => a -> t a -> SymBool
symNotElem = Unified.symNotElem
{-# INLINE symNotElem #-}

-- | 'Data.Foldable.elem' with symbolic equality and
-- 'Grisette.Core.MergingStrategy' knowledge propagation.
mrgFind ::
  (Foldable t, MonadUnion m, Mergeable a) =>
  (a -> SymBool) ->
  t a ->
  m (Maybe a)
mrgFind = Unified.mrgFind
{-# INLINE mrgFind #-}
