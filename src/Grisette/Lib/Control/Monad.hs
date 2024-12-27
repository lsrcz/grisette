{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :   Grisette.Lib.Control.Monad
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Lib.Control.Monad
  ( -- * Functor and Monad classes
    mrgFmap,
    (.<$),
    mrgReturnWithStrategy,
    mrgBindWithStrategy,
    mrgReturn,
    (.>>=),
    (.>>),
    mrgFail,
    mrgMzero,
    mrgMplus,

    -- * Functions

    -- ** Basic 'Monad' functions
    mrgMapM,
    mrgMapM_,
    mrgForM,
    mrgForM_,
    mrgSequence,
    mrgSequence_,
    (.=<<),
    (.>=>),
    (.<=<),
    mrgForever,
    mrgVoid,

    -- ** Generalisations of list functions
    mrgJoin,
    mrgMsum,
    mrgMfilter,
    symMfilter,
    mrgFilterM,
    symFilterM,
    mrgMapAndUnzipM,
    mrgZipWithM,
    mrgZipWithM_,
    mrgFoldM,
    mrgFoldM_,
    mrgReplicateM,
    symReplicateM,
    mrgReplicateM_,
    symReplicateM_,

    -- ** Conditional execution of monadic expressions
    mrgGuard,
    symGuard,
    mrgWhen,
    symWhen,
    mrgUnless,
    symUnless,

    -- ** Monadic lifting operators
    mrgLiftM,
    mrgLiftM2,
    mrgLiftM3,
    mrgLiftM4,
    mrgLiftM5,
    mrgAp,

    -- ** Strict monadic functions
    (.<$!>),
  )
where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Grisette.Internal.Core.Control.Monad.Class.Union (MonadUnion)
import Grisette.Internal.Core.Data.Class.Mergeable
  ( Mergeable,
    MergingStrategy,
  )
import Grisette.Internal.Core.Data.Class.SimpleMergeable (SymBranching)
import Grisette.Internal.Core.Data.Class.SymOrd (SymOrd)
import Grisette.Internal.Core.Data.Class.TryMerge
  ( MonadTryMerge,
    TryMerge,
  )
import Grisette.Internal.SymPrim.SymBool (SymBool)
import Grisette.Internal.Unified.EvalModeTag (EvalModeTag (S))
import Grisette.Lib.Data.Foldable
  ( mrgForM_,
    mrgMapM_,
    mrgMsum,
    mrgSequence_,
  )
import Grisette.Lib.Data.Functor (mrgFmap, mrgVoid, (.<$))
import Grisette.Lib.Data.Traversable
  ( mrgForM,
    mrgMapM,
    mrgSequence,
  )
import qualified Grisette.Unified.Lib.Control.Monad as Unified

-- | 'return' with 'MergingStrategy' knowledge propagation.
mrgReturnWithStrategy :: (MonadTryMerge u) => MergingStrategy a -> a -> u a
mrgReturnWithStrategy = Unified.mrgReturnWithStrategy
{-# INLINE mrgReturnWithStrategy #-}

-- | '>>=' with 'MergingStrategy' knowledge propagation.
mrgBindWithStrategy ::
  (MonadTryMerge u) =>
  MergingStrategy a ->
  MergingStrategy b ->
  u a ->
  (a -> u b) ->
  u b
mrgBindWithStrategy = Unified.mrgBindWithStrategy
{-# INLINE mrgBindWithStrategy #-}

-- | 'return' with 'MergingStrategy' knowledge propagation.
mrgReturn :: (MonadTryMerge u, Mergeable a) => a -> u a
mrgReturn = Unified.mrgReturn
{-# INLINE mrgReturn #-}

infixl 1 .>>=

-- | '>>=' with 'MergingStrategy' knowledge propagation.
(.>>=) ::
  (MonadTryMerge u, Mergeable a, Mergeable b) =>
  u a ->
  (a -> u b) ->
  u b
(.>>=) = (Unified..>>=)
{-# INLINE (.>>=) #-}

infixl 1 .>>

-- | '>>' with 'MergingStrategy' knowledge propagation.
--
-- This is usually more efficient than calling the original '>>' and merge the
-- results.
(.>>) :: (MonadTryMerge m, Mergeable a, Mergeable b) => m a -> m b -> m b
(.>>) = (Unified..>>)
{-# INLINE (.>>) #-}

-- | 'fail' with 'MergingStrategy' knowledge propagation.
mrgFail :: (MonadTryMerge m, Mergeable a, MonadFail m) => String -> m a
mrgFail = Unified.mrgFail
{-# INLINE mrgFail #-}

-- | 'Control.Monad.mzero' with 'MergingStrategy' knowledge propagation.
mrgMzero :: forall m a. (MonadTryMerge m, Mergeable a, MonadPlus m) => m a
mrgMzero = Unified.mrgMzero
{-# INLINE mrgMzero #-}

-- | 'Control.Monad.mplus' with 'MergingStrategy' knowledge propagation.
mrgMplus ::
  forall m a. (MonadTryMerge m, Mergeable a, MonadPlus m) => m a -> m a -> m a
mrgMplus = Unified.mrgMplus
{-# INLINE mrgMplus #-}

infixr 1 .=<<

-- | '=<<' with 'MergingStrategy' knowledge propagation.
(.=<<) ::
  (MonadTryMerge m, Mergeable a, Mergeable b) => (a -> m b) -> m a -> m b
(.=<<) = (Unified..=<<)
{-# INLINE (.=<<) #-}

infixr 1 .>=>

-- | 'Control.Monad.>=>' with 'MergingStrategy' knowledge propagation.
(.>=>) ::
  (MonadTryMerge m, Mergeable a, Mergeable b, Mergeable c) =>
  (a -> m b) ->
  (b -> m c) ->
  a ->
  m c
(.>=>) = (Unified..>=>)
{-# INLINE (.>=>) #-}

infixr 1 .<=<

-- | 'Control.Monad.<=<' with 'MergingStrategy' knowledge propagation.
(.<=<) ::
  (MonadTryMerge m, Mergeable a, Mergeable b, Mergeable c) =>
  (b -> m c) ->
  (a -> m b) ->
  a ->
  m c
(.<=<) = (Unified..<=<)
{-# INLINE (.<=<) #-}

-- | 'Control.Monad.forever' with 'MergingStrategy' knowledge propagation.
mrgForever ::
  (Applicative m, TryMerge m, Mergeable b, Mergeable a) => m a -> m b
mrgForever = Unified.mrgForever
{-# INLINE mrgForever #-}

-- | 'Control.Monad.join' with 'MergingStrategy' knowledge propagation.
mrgJoin :: (MonadTryMerge m, Mergeable a) => m (m a) -> m a
mrgJoin = Unified.mrgJoin
{-# INLINE mrgJoin #-}

-- | 'Control.Monad.mfilter' with 'MergingStrategy' knowledge propagation.
mrgMfilter ::
  (MonadTryMerge m, MonadPlus m, Mergeable a) =>
  (a -> Bool) ->
  m a ->
  m a
mrgMfilter = Unified.mrgMfilter
{-# INLINE mrgMfilter #-}

-- | 'Control.Monad.mfilter' with 'MergingStrategy' knowledge propagation and
-- symbolic conditions.
symMfilter ::
  (MonadTryMerge m, MonadPlus m, MonadUnion m, Mergeable a) =>
  (a -> SymBool) ->
  m a ->
  m a
symMfilter = Unified.symMfilter
{-# INLINE symMfilter #-}

-- | 'Control.Monad.filterM' with 'MergingStrategy' knowledge propagation.
mrgFilterM ::
  (TryMerge m, Applicative m, Mergeable a, Foldable t) =>
  (a -> m Bool) ->
  t a ->
  m [a]
mrgFilterM = Unified.mrgFilterM
{-# INLINE mrgFilterM #-}

-- | 'Control.Monad.filterM' with 'MergingStrategy' knowledge propagation and
-- symbolic conditions.
symFilterM ::
  (TryMerge m, MonadUnion m, Mergeable a, Foldable t) =>
  (a -> m SymBool) ->
  t a ->
  m [a]
symFilterM = Unified.symFilterM
{-# INLINE symFilterM #-}

-- | 'Control.Monad.mapAndUnzipM' with 'MergingStrategy' knowledge propagation.
mrgMapAndUnzipM ::
  ( Applicative m,
    TryMerge m,
    Mergeable b,
    Mergeable c
  ) =>
  (a -> m (b, c)) ->
  [a] ->
  m ([b], [c])
mrgMapAndUnzipM = Unified.mrgMapAndUnzipM
{-# INLINE mrgMapAndUnzipM #-}

-- | 'Control.Monad.zipWithM' with 'MergingStrategy' knowledge propagation.
mrgZipWithM ::
  (Applicative m, TryMerge m, Mergeable c) =>
  (a -> b -> m c) ->
  [a] ->
  [b] ->
  m [c]
mrgZipWithM = Unified.mrgZipWithM
{-# INLINE mrgZipWithM #-}

-- | 'Control.Monad.zipWithM_' with 'MergingStrategy' knowledge propagation.
mrgZipWithM_ ::
  (Applicative m, TryMerge m, Mergeable c) =>
  (a -> b -> m c) ->
  [a] ->
  [b] ->
  m ()
mrgZipWithM_ = Unified.mrgZipWithM_
{-# INLINE mrgZipWithM_ #-}

-- | 'Control.Monad.foldM' with 'MergingStrategy' knowledge propagation.
mrgFoldM ::
  (MonadTryMerge m, Mergeable b, Foldable t) =>
  (b -> a -> m b) ->
  b ->
  t a ->
  m b
mrgFoldM = Unified.mrgFoldM
{-# INLINE mrgFoldM #-}

-- | 'Control.Monad.foldM_' with 'MergingStrategy' knowledge propagation.
mrgFoldM_ ::
  (MonadTryMerge m, Foldable t, Mergeable b) =>
  (b -> a -> m b) ->
  b ->
  t a ->
  m ()
mrgFoldM_ = Unified.mrgFoldM_
{-# INLINE mrgFoldM_ #-}

-- | 'Control.Monad.replicateM' with 'MergingStrategy' knowledge propagation.
mrgReplicateM ::
  (Applicative m, TryMerge m, Mergeable a) =>
  Int ->
  m a ->
  m [a]
mrgReplicateM = Unified.mrgReplicateM
{-# INLINE mrgReplicateM #-}

-- | 'Control.Monad.replicateM' with 'MergingStrategy' knowledge propagation and
-- symbolic number of elements.
symReplicateM ::
  (MonadUnion m, TryMerge m, Mergeable a, Num int, SymOrd int) =>
  Int ->
  int ->
  m a ->
  m [a]
symReplicateM = Unified.symReplicateM @'S
{-# INLINE symReplicateM #-}

-- | 'Control.Monad.replicateM_' with 'MergingStrategy' knowledge propagation.
mrgReplicateM_ ::
  (Applicative m, TryMerge m, Mergeable a) =>
  Int ->
  m a ->
  m ()
mrgReplicateM_ = Unified.mrgReplicateM_
{-# INLINE mrgReplicateM_ #-}

-- | 'Control.Monad.replicateM_' with 'MergingStrategy' knowledge propagation
-- and symbolic number of elements.
symReplicateM_ ::
  (MonadUnion m, TryMerge m, Mergeable a, Num int, SymOrd int) =>
  Int ->
  int ->
  m a ->
  m ()
symReplicateM_ = Unified.symReplicateM_ @'S
{-# INLINE symReplicateM_ #-}

-- | 'Control.Monad.guard' with 'MergingStrategy' knowledge propagation.
mrgGuard :: (Alternative m, TryMerge m) => Bool -> m ()
mrgGuard = Unified.mrgGuard
{-# INLINE mrgGuard #-}

-- | 'Control.Monad.guard' with 'MergingStrategy' knowledge propagation and
-- symbolic conditions.
symGuard :: (SymBranching m, TryMerge m, Alternative m) => SymBool -> m ()
symGuard = Unified.symGuard
{-# INLINE symGuard #-}

-- | 'Control.Monad.when' with 'MergingStrategy' knowledge propagation.
mrgWhen :: (Applicative m, TryMerge m) => Bool -> m () -> m ()
mrgWhen = Unified.mrgWhen
{-# INLINE mrgWhen #-}

-- | 'Control.Monad.when' with 'MergingStrategy' knowledge propagation and
-- symbolic conditions.
symWhen ::
  (Applicative m, TryMerge m, SymBranching m) => SymBool -> m () -> m ()
symWhen = Unified.symWhen
{-# INLINE symWhen #-}

-- | 'Control.Monad.unless' with 'MergingStrategy' knowledge propagation.
mrgUnless :: (Applicative m, TryMerge m) => Bool -> m () -> m ()
mrgUnless = Unified.mrgUnless
{-# INLINE mrgUnless #-}

-- | 'Control.Monad.unless' with 'MergingStrategy' knowledge propagation and
-- symbolic conditions.
symUnless ::
  (Applicative m, TryMerge m, SymBranching m) => SymBool -> m () -> m ()
symUnless = Unified.symUnless
{-# INLINE symUnless #-}

-- | 'Control.Monad.liftM' with 'MergingStrategy' knowledge propagation.
mrgLiftM ::
  (MonadTryMerge m, Mergeable a, Mergeable b) => (a -> b) -> m a -> m b
mrgLiftM = Unified.mrgLiftM
{-# INLINE mrgLiftM #-}

-- | 'Control.Monad.liftM2' with 'MergingStrategy' knowledge propagation.
mrgLiftM2 ::
  (MonadTryMerge m, Mergeable a, Mergeable b, Mergeable c) =>
  (a -> b -> c) ->
  m a ->
  m b ->
  m c
mrgLiftM2 = Unified.mrgLiftM2
{-# INLINE mrgLiftM2 #-}

-- | 'Control.Monad.liftM3' with 'MergingStrategy' knowledge propagation.
mrgLiftM3 ::
  (MonadTryMerge m, Mergeable a, Mergeable b, Mergeable c, Mergeable d) =>
  (a -> b -> c -> d) ->
  m a ->
  m b ->
  m c ->
  m d
mrgLiftM3 = Unified.mrgLiftM3
{-# INLINE mrgLiftM3 #-}

-- | 'Control.Monad.liftM4' with 'MergingStrategy' knowledge propagation.
mrgLiftM4 ::
  ( MonadTryMerge m,
    Mergeable a,
    Mergeable b,
    Mergeable c,
    Mergeable d,
    Mergeable e
  ) =>
  (a -> b -> c -> d -> e) ->
  m a ->
  m b ->
  m c ->
  m d ->
  m e
mrgLiftM4 = Unified.mrgLiftM4
{-# INLINE mrgLiftM4 #-}

-- | 'Control.Monad.liftM5' with 'MergingStrategy' knowledge propagation.
mrgLiftM5 ::
  ( MonadTryMerge m,
    Mergeable a,
    Mergeable b,
    Mergeable c,
    Mergeable d,
    Mergeable e,
    Mergeable f
  ) =>
  (a -> b -> c -> d -> e -> f) ->
  m a ->
  m b ->
  m c ->
  m d ->
  m e ->
  m f
mrgLiftM5 = Unified.mrgLiftM5
{-# INLINE mrgLiftM5 #-}

-- | '<*>' with 'MergingStrategy' knowledge propagation.
mrgAp ::
  (MonadTryMerge m, Mergeable a, Mergeable b) => m (a -> b) -> m a -> m b
mrgAp = Unified.mrgAp
{-# INLINE mrgAp #-}

infixl 4 .<$!>

-- | 'Control.Monad.<$!>' with 'MergingStrategy' knowledge propagation. Merging
-- is always strict so we can directly use 'Grisette.Lib.Data.Functor..<$>'.
(.<$!>) ::
  (MonadTryMerge m, Mergeable a, Mergeable b) => (a -> b) -> m a -> m b
(.<$!>) = (Unified..<$!>)
{-# INLINE (.<$!>) #-}
