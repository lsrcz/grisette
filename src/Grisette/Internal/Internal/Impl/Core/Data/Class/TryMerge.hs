{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}

-- |
-- Module      :   Grisette.Internal.Internal.Impl.Core.Data.Class.TryMerge
-- Copyright   :   (c) Sirui Lu 2023-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Impl.Core.Data.Class.TryMerge () where

import Control.Monad.Cont (ContT (ContT))
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Identity
  ( IdentityT (IdentityT),
  )
import qualified Control.Monad.RWS.Lazy as RWSLazy
import qualified Control.Monad.RWS.Strict as RWSStrict
import Control.Monad.Reader (ReaderT (ReaderT))
import qualified Control.Monad.State.Lazy as StateLazy
import qualified Control.Monad.State.Strict as StateStrict
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import Data.Functor.Sum (Sum (InL, InR))
import qualified Data.Monoid as Monoid
import Grisette.Internal.Core.Data.Class.Mergeable
  ( Mergeable (rootStrategy),
    Mergeable1 (liftRootStrategy),
    Mergeable2 (liftRootStrategy2),
    Mergeable3 (liftRootStrategy3),
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.TryMerge
  ( TryMerge (tryMergeWithStrategy),
  )

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim

instance (TryMerge m) => TryMerge (MaybeT m) where
  tryMergeWithStrategy strategy (MaybeT ma) =
    MaybeT $ tryMergeWithStrategy (liftRootStrategy strategy) ma
  {-# INLINE tryMergeWithStrategy #-}

instance (Mergeable e, TryMerge m) => TryMerge (ExceptT e m) where
  tryMergeWithStrategy strategy (ExceptT ma) =
    ExceptT $ tryMergeWithStrategy (liftRootStrategy strategy) ma
  {-# INLINE tryMergeWithStrategy #-}

instance (TryMerge m) => TryMerge (ReaderT r m) where
  tryMergeWithStrategy strategy (ReaderT f) =
    ReaderT $ \v -> tryMergeWithStrategy strategy $ f v
  {-# INLINE tryMergeWithStrategy #-}

instance (Mergeable s, TryMerge m) => TryMerge (StateLazy.StateT s m) where
  tryMergeWithStrategy strategy (StateLazy.StateT f) =
    StateLazy.StateT $
      \s -> tryMergeWithStrategy (liftRootStrategy2 strategy rootStrategy) (f s)
  {-# INLINE tryMergeWithStrategy #-}

instance (Mergeable s, TryMerge m) => TryMerge (StateStrict.StateT s m) where
  tryMergeWithStrategy strategy (StateStrict.StateT f) =
    StateStrict.StateT $
      \s -> tryMergeWithStrategy (liftRootStrategy2 strategy rootStrategy) (f s)
  {-# INLINE tryMergeWithStrategy #-}

instance
  (Monoid w, Mergeable w, TryMerge m) =>
  TryMerge (WriterLazy.WriterT w m)
  where
  tryMergeWithStrategy strategy (WriterLazy.WriterT f) =
    WriterLazy.WriterT $
      tryMergeWithStrategy (liftRootStrategy2 strategy rootStrategy) f
  {-# INLINE tryMergeWithStrategy #-}

instance
  (Monoid w, Mergeable w, TryMerge m) =>
  TryMerge (WriterStrict.WriterT w m)
  where
  tryMergeWithStrategy strategy (WriterStrict.WriterT f) =
    WriterStrict.WriterT $
      tryMergeWithStrategy (liftRootStrategy2 strategy rootStrategy) f
  {-# INLINE tryMergeWithStrategy #-}

instance
  (Monoid w, Mergeable w, Mergeable s, TryMerge m) =>
  TryMerge (RWSStrict.RWST r w s m)
  where
  tryMergeWithStrategy strategy (RWSStrict.RWST f) =
    RWSStrict.RWST $
      \r s ->
        tryMergeWithStrategy
          (liftRootStrategy3 strategy rootStrategy rootStrategy)
          (f r s)
  {-# INLINE tryMergeWithStrategy #-}

instance
  (Monoid w, Mergeable w, Mergeable s, TryMerge m) =>
  TryMerge (RWSLazy.RWST r w s m)
  where
  tryMergeWithStrategy strategy (RWSLazy.RWST f) =
    RWSLazy.RWST $
      \r s ->
        tryMergeWithStrategy
          (liftRootStrategy3 strategy rootStrategy rootStrategy)
          (f r s)
  {-# INLINE tryMergeWithStrategy #-}

instance (TryMerge m) => TryMerge (IdentityT m) where
  tryMergeWithStrategy strategy (IdentityT ma) =
    IdentityT $ tryMergeWithStrategy strategy ma
  {-# INLINE tryMergeWithStrategy #-}

instance (TryMerge m, Mergeable r) => TryMerge (ContT r m) where
  tryMergeWithStrategy _ (ContT ma) =
    ContT $ \c -> tryMergeWithStrategy rootStrategy (ma c)
  {-# INLINE tryMergeWithStrategy #-}

#define TRYMERGE_ID(T) \
  instance TryMerge (T) where { \
    tryMergeWithStrategy _ = id; {-# INLINE tryMergeWithStrategy #-} \
  }

#if 1
TRYMERGE_ID(Either a)
TRYMERGE_ID(Maybe)
TRYMERGE_ID([])
TRYMERGE_ID((,) a)
TRYMERGE_ID((,,) a b)
TRYMERGE_ID((,,,) a b c)
TRYMERGE_ID((,,,,) a b c d)
TRYMERGE_ID((,,,,,) a b c d e)
TRYMERGE_ID((,,,,,,) a b c d e f)
TRYMERGE_ID((,,,,,,,) a b c d e f g)
TRYMERGE_ID((,,,,,,,,) a b c d e f g h)
#endif

instance (TryMerge f, TryMerge g) => TryMerge (Sum f g) where
  tryMergeWithStrategy strategy (InL fa) =
    InL $ tryMergeWithStrategy strategy fa
  tryMergeWithStrategy strategy (InR fa) =
    InR $ tryMergeWithStrategy strategy fa

instance TryMerge Monoid.Sum where
  tryMergeWithStrategy _ = id
  {-# INLINE tryMergeWithStrategy #-}
