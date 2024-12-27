{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :   Grisette.Unified.Lib.Control.Monad
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Unified.Lib.Control.Monad
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
import Control.Monad (MonadPlus (mplus, mzero), join)
import Grisette.Internal.Core.Data.Class.LogicalOp (LogicalOp (symNot, (.||)))
import Grisette.Internal.Core.Data.Class.Mergeable
  ( Mergeable (rootStrategy),
    MergingStrategy,
  )
import Grisette.Internal.Core.Data.Class.SimpleMergeable (SymBranching)
import Grisette.Internal.Core.Data.Class.TryMerge
  ( MonadTryMerge,
    TryMerge (tryMergeWithStrategy),
    tryMerge,
  )
import Grisette.Internal.SymPrim.SymBool (SymBool)
import Grisette.Lib.Data.Functor (mrgFmap, mrgUnzip, mrgVoid, (.<$))
import Grisette.Lib.Data.Traversable
  ( mrgForM,
    mrgMapM,
    mrgSequence,
    mrgSequenceA,
    mrgTraverse,
  )
import Grisette.Unified
  ( EvalModeBase,
    GetBool,
    UnifiedBranching,
    UnifiedSymOrd,
    mrgIf,
    (.<=),
  )
import Grisette.Unified.Lib.Control.Applicative
  ( mrgEmpty,
    mrgLiftA2,
    mrgPure,
    (.*>),
    (.<$>),
    (.<*>),
  )
import Grisette.Unified.Lib.Data.Foldable
  ( mrgFoldlM,
    mrgForM_,
    mrgMapM_,
    mrgMsum,
    mrgSequenceA_,
    mrgSequence_,
  )

-- | 'return' with 'Grisette.Core.MergingStrategy' knowledge propagation.
mrgReturnWithStrategy :: (MonadTryMerge u) => MergingStrategy a -> a -> u a
mrgReturnWithStrategy s = tryMergeWithStrategy s . return
{-# INLINE mrgReturnWithStrategy #-}

-- | '>>=' with 'Grisette.Core.MergingStrategy' knowledge propagation.
mrgBindWithStrategy ::
  (MonadTryMerge u) =>
  MergingStrategy a ->
  MergingStrategy b ->
  u a ->
  (a -> u b) ->
  u b
mrgBindWithStrategy sa sb a f =
  tryMergeWithStrategy sb $ tryMergeWithStrategy sa a >>= f
{-# INLINE mrgBindWithStrategy #-}

-- | 'return' with 'Grisette.Core.MergingStrategy' knowledge propagation.
mrgReturn :: (MonadTryMerge u, Mergeable a) => a -> u a
mrgReturn = mrgReturnWithStrategy rootStrategy
{-# INLINE mrgReturn #-}

infixl 1 .>>=

-- | '>>=' with 'Grisette.Core.MergingStrategy' knowledge propagation.
(.>>=) ::
  (MonadTryMerge u, Mergeable a, Mergeable b) =>
  u a ->
  (a -> u b) ->
  u b
(.>>=) = mrgBindWithStrategy rootStrategy rootStrategy
{-# INLINE (.>>=) #-}

infixl 1 .>>

-- | '>>' with 'Grisette.Core.MergingStrategy' knowledge propagation.
--
-- This is usually more efficient than calling the original '>>' and merge the
-- results.
(.>>) :: (MonadTryMerge m, Mergeable a, Mergeable b) => m a -> m b -> m b
a .>> f = tryMerge $ mrgVoid a >> f
{-# INLINE (.>>) #-}

-- | 'fail' with 'Grisette.Core.MergingStrategy' knowledge propagation.
mrgFail :: (MonadTryMerge m, Mergeable a, MonadFail m) => String -> m a
mrgFail = tryMerge . fail
{-# INLINE mrgFail #-}

-- | 'Control.Monad.mzero' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgMzero :: forall m a. (MonadTryMerge m, Mergeable a, MonadPlus m) => m a
mrgMzero = tryMerge mzero
{-# INLINE mrgMzero #-}

-- | 'Control.Monad.mplus' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgMplus ::
  forall m a. (MonadTryMerge m, Mergeable a, MonadPlus m) => m a -> m a -> m a
mrgMplus a b = tryMerge $ mplus (tryMerge a) (tryMerge b)
{-# INLINE mrgMplus #-}

infixr 1 .=<<

-- | '=<<' with 'Grisette.Core.MergingStrategy' knowledge propagation.
(.=<<) ::
  (MonadTryMerge m, Mergeable a, Mergeable b) => (a -> m b) -> m a -> m b
f .=<< a = tryMerge $ f =<< tryMerge a
{-# INLINE (.=<<) #-}

infixr 1 .>=>

-- | 'Control.Monad.>=>' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
(.>=>) ::
  (MonadTryMerge m, Mergeable a, Mergeable b, Mergeable c) =>
  (a -> m b) ->
  (b -> m c) ->
  a ->
  m c
f .>=> g = \a -> tryMerge $ tryMerge (f a) >>= g
{-# INLINE (.>=>) #-}

infixr 1 .<=<

-- | 'Control.Monad.<=<' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
(.<=<) ::
  (MonadTryMerge m, Mergeable a, Mergeable b, Mergeable c) =>
  (b -> m c) ->
  (a -> m b) ->
  a ->
  m c
(.<=<) = flip (.>=>)
{-# INLINE (.<=<) #-}

-- | 'Control.Monad.forever' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgForever ::
  (Applicative m, TryMerge m, Mergeable b, Mergeable a) => m a -> m b
mrgForever a = let a' = a .*> a' in a'
{-# INLINE mrgForever #-}

-- | 'Control.Monad.join' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgJoin :: (MonadTryMerge m, Mergeable a) => m (m a) -> m a
mrgJoin a = tryMerge $ join a
{-# INLINE mrgJoin #-}

-- | 'Control.Monad.mfilter' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgMfilter ::
  (MonadTryMerge m, MonadPlus m, Mergeable a) =>
  (a -> Bool) ->
  m a ->
  m a
mrgMfilter p ma = do
  a <- tryMerge ma
  if p a then mrgReturn a else mrgMzero
{-# INLINE mrgMfilter #-}

-- | 'Control.Monad.mfilter' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation and symbolic conditions.
symMfilter ::
  forall mode m a.
  ( MonadTryMerge m,
    MonadPlus m,
    UnifiedBranching mode m,
    Mergeable a
  ) =>
  (a -> GetBool mode) ->
  m a ->
  m a
symMfilter p ma = do
  a <- tryMerge ma
  mrgIf (p a) (mrgReturn a) mrgMzero
{-# INLINE symMfilter #-}

-- | 'Control.Monad.filterM' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgFilterM ::
  (TryMerge m, Applicative m, Mergeable a, Foldable t) =>
  (a -> m Bool) ->
  t a ->
  m [a]
mrgFilterM p =
  foldr
    (\x lst -> (\flg -> if flg then (x :) else id) .<$> p x .<*> lst)
    (mrgPure [])
{-# INLINE mrgFilterM #-}

-- | 'Control.Monad.filterM' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation and symbolic conditions.
symFilterM ::
  forall mode m t a.
  ( TryMerge m,
    UnifiedBranching mode m,
    MonadTryMerge m,
    EvalModeBase mode,
    Mergeable a,
    Foldable t
  ) =>
  (a -> m (GetBool mode)) ->
  t a ->
  m [a]
symFilterM p =
  foldr
    ( \x lst -> do
        flag <- tryMerge $ p x
        mrgIf flag ((x :) <$> lst) lst
    )
    (mrgPure [])
{-# INLINE symFilterM #-}

-- | 'Control.Monad.mapAndUnzipM' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgMapAndUnzipM ::
  ( Applicative m,
    TryMerge m,
    Mergeable b,
    Mergeable c
  ) =>
  (a -> m (b, c)) ->
  [a] ->
  m ([b], [c])
mrgMapAndUnzipM f xs = mrgUnzip .<$> mrgTraverse f xs
{-# INLINE mrgMapAndUnzipM #-}

-- | 'Control.Monad.zipWithM' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgZipWithM ::
  (Applicative m, TryMerge m, Mergeable c) =>
  (a -> b -> m c) ->
  [a] ->
  [b] ->
  m [c]
mrgZipWithM f xs ys = mrgSequenceA (zipWith f xs ys)
{-# INLINE mrgZipWithM #-}

-- | 'Control.Monad.zipWithM_' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgZipWithM_ ::
  (Applicative m, TryMerge m, Mergeable c) =>
  (a -> b -> m c) ->
  [a] ->
  [b] ->
  m ()
mrgZipWithM_ f xs ys = mrgSequenceA_ (zipWith f xs ys)
{-# INLINE mrgZipWithM_ #-}

-- | 'Control.Monad.foldM' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgFoldM ::
  (MonadTryMerge m, Mergeable b, Foldable t) =>
  (b -> a -> m b) ->
  b ->
  t a ->
  m b
mrgFoldM = mrgFoldlM
{-# INLINE mrgFoldM #-}

-- | 'Control.Monad.foldM_' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgFoldM_ ::
  (MonadTryMerge m, Foldable t, Mergeable b) =>
  (b -> a -> m b) ->
  b ->
  t a ->
  m ()
mrgFoldM_ f a xs = mrgFoldlM f a xs .>> mrgPure ()
{-# INLINE mrgFoldM_ #-}

-- | 'Control.Monad.replicateM' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgReplicateM ::
  (Applicative m, TryMerge m, Mergeable a) =>
  Int ->
  m a ->
  m [a]
mrgReplicateM n = mrgSequenceA . replicate n
{-# INLINE mrgReplicateM #-}

-- | 'Control.Monad.replicateM' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation and symbolic number of elements.
symReplicateM ::
  forall mode m a int.
  ( EvalModeBase mode,
    TryMerge m,
    Applicative m,
    Mergeable a,
    Num int,
    UnifiedBranching mode m,
    UnifiedSymOrd mode Int,
    UnifiedSymOrd mode int
  ) =>
  Int ->
  int ->
  m a ->
  m [a]
symReplicateM maxCnt cnt0 f =
  loop maxCnt cnt0
  where
    loop concreteCnt cnt =
      mrgIf @mode
        (cnt .<= 0 .|| concreteCnt .<= 0)
        (mrgPure [])
        (mrgLiftA2 (:) f (loop (concreteCnt - 1) (cnt - 1)))
{-# INLINE symReplicateM #-}

-- | 'Control.Monad.replicateM_' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgReplicateM_ ::
  (Applicative m, TryMerge m, Mergeable a) =>
  Int ->
  m a ->
  m ()
mrgReplicateM_ n = mrgSequenceA_ . replicate n
{-# INLINE mrgReplicateM_ #-}

-- | 'Control.Monad.replicateM_' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation and symbolic number of elements.
symReplicateM_ ::
  forall mode m a int.
  ( EvalModeBase mode,
    TryMerge m,
    Applicative m,
    Mergeable a,
    Num int,
    UnifiedBranching mode m,
    UnifiedSymOrd mode Int,
    UnifiedSymOrd mode int
  ) =>
  Int ->
  int ->
  m a ->
  m ()
symReplicateM_ maxCnt cnt0 f =
  loop maxCnt cnt0
  where
    loop concreteCnt cnt =
      mrgIf @mode
        (cnt .<= 0 .|| concreteCnt .<= 0)
        (mrgPure ())
        (f .*> (loop (concreteCnt - 1) (cnt - 1)))
{-# INLINE symReplicateM_ #-}

-- | 'Control.Monad.guard' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgGuard :: (Alternative m, TryMerge m) => Bool -> m ()
mrgGuard True = mrgPure ()
mrgGuard False = mrgEmpty
{-# INLINE mrgGuard #-}

-- | 'Control.Monad.guard' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation and symbolic conditions.
symGuard :: (SymBranching m, TryMerge m, Alternative m) => SymBool -> m ()
symGuard b = mrgIf b (mrgPure ()) mrgEmpty
{-# INLINE symGuard #-}

-- | 'Control.Monad.when' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgWhen :: (Applicative m, TryMerge m) => Bool -> m () -> m ()
mrgWhen True a = tryMerge a
mrgWhen False _ = mrgPure ()
{-# INLINE mrgWhen #-}

-- | 'Control.Monad.when' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation and symbolic conditions.
symWhen ::
  (Applicative m, TryMerge m, SymBranching m) => SymBool -> m () -> m ()
symWhen b a = mrgIf b a (mrgPure ())
{-# INLINE symWhen #-}

-- | 'Control.Monad.unless' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgUnless :: (Applicative m, TryMerge m) => Bool -> m () -> m ()
mrgUnless b = mrgWhen (not b)
{-# INLINE mrgUnless #-}

-- | 'Control.Monad.unless' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation and symbolic conditions.
symUnless ::
  (Applicative m, TryMerge m, SymBranching m) => SymBool -> m () -> m ()
symUnless b = symWhen (symNot b)
{-# INLINE symUnless #-}

-- | 'Control.Monad.liftM' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgLiftM ::
  (MonadTryMerge m, Mergeable a, Mergeable b) => (a -> b) -> m a -> m b
mrgLiftM f a = f .<$> a
{-# INLINE mrgLiftM #-}

-- | 'Control.Monad.liftM2' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgLiftM2 ::
  (MonadTryMerge m, Mergeable a, Mergeable b, Mergeable c) =>
  (a -> b -> c) ->
  m a ->
  m b ->
  m c
mrgLiftM2 f a b = f .<$> a .<*> b
{-# INLINE mrgLiftM2 #-}

-- | 'Control.Monad.liftM3' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgLiftM3 ::
  (MonadTryMerge m, Mergeable a, Mergeable b, Mergeable c, Mergeable d) =>
  (a -> b -> c -> d) ->
  m a ->
  m b ->
  m c ->
  m d
mrgLiftM3 f a b c = f .<$> a .<*> b .<*> c
{-# INLINE mrgLiftM3 #-}

-- | 'Control.Monad.liftM4' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
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
mrgLiftM4 f a b c d = f .<$> a .<*> b .<*> c .<*> d
{-# INLINE mrgLiftM4 #-}

-- | 'Control.Monad.liftM5' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
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
mrgLiftM5 f a b c d e = f .<$> a .<*> b .<*> c .<*> d .<*> e
{-# INLINE mrgLiftM5 #-}

-- | 'Control.Monad.<*>' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation.
mrgAp ::
  (MonadTryMerge m, Mergeable a, Mergeable b) => m (a -> b) -> m a -> m b
mrgAp = (.<*>)
{-# INLINE mrgAp #-}

infixl 4 .<$!>

-- | 'Control.Monad.<$!>' with 'Grisette.Core.MergingStrategy' knowledge
-- propagation. Merging is always strict so we can directly use
-- 'Grisette.Internal.Unified.Lib.Data.Functor..<$>'.
(.<$!>) ::
  (MonadTryMerge m, Mergeable a, Mergeable b) => (a -> b) -> m a -> m b
f .<$!> a = f .<$> a
{-# INLINE (.<$!>) #-}
