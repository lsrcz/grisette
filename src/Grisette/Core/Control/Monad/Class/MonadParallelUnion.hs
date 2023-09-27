{-# LANGUAGE LambdaCase #-}

-- |
-- Module      :   Grisette.Core.Control.Monad.Class.MonadParallelUnion
-- Copyright   :   (c) Sirui Lu 2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Control.Monad.Class.MonadParallelUnion
  ( MonadParallelUnion (..),
  )
where

import Control.DeepSeq
import Control.Monad.Except
import Control.Monad.Identity
import qualified Control.Monad.RWS.Lazy as RWSLazy
import qualified Control.Monad.RWS.Strict as RWSStrict
import Control.Monad.Reader
import qualified Control.Monad.State.Lazy as StateLazy
import qualified Control.Monad.State.Strict as StateStrict
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Data.Class.SimpleMergeable

-- | Parallel union monad.
--
-- With the @QualifiedDo@ extension and the "Grisette.Qualified.ParallelUnionDo"
-- module, one can execute the paths in parallel and merge the results with:
--
-- > :set -XQualifiedDo -XOverloadedStrings
-- > import Grisette
-- > import qualified Grisette.Qualified.ParallelUnionDo as P
-- > P.do
-- >   x <- mrgIf "a" (return 1) (return 2) :: UnionM Int
-- >   return $ x + 1
-- >
-- > -- {If a 2 3}
class (UnionLike m, Monad m) => MonadParallelUnion m where
  parBindUnion :: (Mergeable b, NFData b) => m a -> (a -> m b) -> m b

instance (MonadParallelUnion m) => MonadParallelUnion (MaybeT m) where
  parBindUnion (MaybeT x) f =
    MaybeT $
      x `parBindUnion` \case
        Nothing -> return Nothing
        Just x'' -> runMaybeT $ f x''
  {-# INLINE parBindUnion #-}

instance (MonadParallelUnion m, Mergeable e, NFData e) => MonadParallelUnion (ExceptT e m) where
  parBindUnion (ExceptT x) f =
    ExceptT $
      x `parBindUnion` \case
        Left e -> return $ Left e
        Right x'' -> runExceptT $ f x''
  {-# INLINE parBindUnion #-}

instance (MonadParallelUnion m, Mergeable s, NFData s) => MonadParallelUnion (StateLazy.StateT s m) where
  parBindUnion (StateLazy.StateT x) f = StateLazy.StateT $ \s ->
    x s `parBindUnion` \case
      ~(a, s') -> StateLazy.runStateT (f a) s'
  {-# INLINE parBindUnion #-}

instance (MonadParallelUnion m, Mergeable s, NFData s) => MonadParallelUnion (StateStrict.StateT s m) where
  parBindUnion (StateStrict.StateT x) f = StateStrict.StateT $ \s ->
    x s `parBindUnion` \case
      (a, s') -> StateStrict.runStateT (f a) s'
  {-# INLINE parBindUnion #-}

instance (MonadParallelUnion m, Mergeable s, Monoid s, NFData s) => MonadParallelUnion (WriterLazy.WriterT s m) where
  parBindUnion (WriterLazy.WriterT x) f =
    WriterLazy.WriterT $
      x `parBindUnion` \case
        ~(a, w) ->
          WriterLazy.runWriterT (f a) `parBindUnion` \case
            ~(b, w') -> return (b, w <> w')
  {-# INLINE parBindUnion #-}

instance (MonadParallelUnion m, Mergeable s, Monoid s, NFData s) => MonadParallelUnion (WriterStrict.WriterT s m) where
  parBindUnion (WriterStrict.WriterT x) f =
    WriterStrict.WriterT $
      x `parBindUnion` \case
        (a, w) ->
          WriterStrict.runWriterT (f a) `parBindUnion` \case
            (b, w') -> return (b, w <> w')
  {-# INLINE parBindUnion #-}

instance (MonadParallelUnion m, Mergeable a, NFData a) => MonadParallelUnion (ReaderT a m) where
  parBindUnion (ReaderT x) f = ReaderT $ \a ->
    x a `parBindUnion` \a' -> runReaderT (f a') a
  {-# INLINE parBindUnion #-}

instance (MonadParallelUnion m) => MonadParallelUnion (IdentityT m) where
  parBindUnion (IdentityT x) f = IdentityT $ x `parBindUnion` (merge . runIdentityT . f)
  {-# INLINE parBindUnion #-}

instance
  (MonadParallelUnion m, Mergeable s, Mergeable r, Mergeable w, Monoid w, NFData r, NFData w, NFData s) =>
  MonadParallelUnion (RWSStrict.RWST r w s m)
  where
  parBindUnion m k = RWSStrict.RWST $ \r s ->
    RWSStrict.runRWST m r s `parBindUnion` \case
      (a, s', w) ->
        RWSStrict.runRWST (k a) r s' `parBindUnion` \case
          (b, s'', w') -> return (b, s'', w <> w')
  {-# INLINE parBindUnion #-}

instance
  (MonadParallelUnion m, Mergeable s, Mergeable r, Mergeable w, Monoid w, NFData r, NFData w, NFData s) =>
  MonadParallelUnion (RWSLazy.RWST r w s m)
  where
  parBindUnion m k = RWSLazy.RWST $ \r s ->
    RWSLazy.runRWST m r s `parBindUnion` \case
      ~(a, s', w) ->
        RWSLazy.runRWST (k a) r s' `parBindUnion` \case
          ~(b, s'', w') -> return (b, s'', w <> w')
  {-# INLINE parBindUnion #-}
