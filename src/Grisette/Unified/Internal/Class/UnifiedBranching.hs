{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- |
-- Module      :   Grisette.Unified.Internal.Class.UnifiedBranching
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Unified.Internal.Class.UnifiedBranching
  ( UnifiedBranching (..),
  )
where

import Control.Monad.Cont (ContT)
import Control.Monad.Except (ExceptT)
import Control.Monad.Identity (Identity (runIdentity), IdentityT)
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Trans.RWS.Lazy as RWSLazy
import qualified Control.Monad.Trans.RWS.Strict as RWSStrict
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.State.Lazy as StateLazy
import qualified Control.Monad.Trans.State.Strict as StateStrict
import qualified Control.Monad.Trans.Writer.Lazy as WriterLazy
import qualified Control.Monad.Trans.Writer.Strict as WriterStrict
import Data.Kind (Constraint)
import Data.Type.Bool (If)
import Data.Typeable (Typeable, eqT, type (:~:) (Refl))
import Grisette.Internal.Core.Control.Monad.UnionM (liftUnionM)
import Grisette.Internal.Core.Data.Class.GenSym (FreshT)
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.SimpleMergeable (UnionMergeable1)
import qualified Grisette.Internal.Core.Data.Class.SimpleMergeable as Grisette
import Grisette.Internal.Core.Data.Class.TryMerge
  ( TryMerge,
    mrgSingle,
  )
import Grisette.Unified.Internal.EvaluationMode
  ( BaseMonad,
    EvaluationMode (Con, Sym),
    IsConMode,
  )
import Grisette.Unified.Internal.UnifiedBool (UnifiedBool (GetBool))

-- | A class that provides a unified branching operation for unified types.
--
-- On all monads with 'TryMerge' instance, the 'mrgIf' function could use
-- concrete Boolean variable for branching.
--
-- On all monads with 'UnionMergeable1' instance, the 'mrgIf' function could use
-- symbolic Boolean variable for branching.
--
-- Note that you may sometimes need to write visible type application for the
-- mode parameter when the mode for the boolean variable isn't clear.
--
-- > mrgIf @mode (a .== b) ...
--
-- or
--
-- > mrgIf (a .== b :: GetBool mode) ...
class
  (Typeable mode, TryMerge m) =>
  UnifiedBranching (mode :: EvaluationMode) m
  where
  mrgIf :: (Mergeable a) => GetBool mode -> m a -> m a -> m a
  mrgIf c t e = case (eqT @mode @'Con, eqT @mode @'Sym) of
    (Just Refl, _) -> withBaseBranching @mode @m $ if c then t else e
    (_, Just Refl) -> withBaseBranching @mode @m $ Grisette.mrgIf c t e
    _ -> error "impossible"

  liftBaseMonad ::
    ( Applicative m,
      UnifiedBranching mode m,
      Mergeable a,
      TryMerge (BaseMonad mode),
      Functor (BaseMonad mode)
    ) =>
    BaseMonad mode a ->
    m a
  liftBaseMonad b = case (eqT @mode @'Con, eqT @mode @'Sym) of
    (Just Refl, _) -> withBaseBranching @mode @m $ mrgSingle . runIdentity $ b
    (_, Just Refl) -> withBaseBranching @mode @m $ liftUnionM b
    _ -> error "impossible"

  withBaseBranching ::
    ((If (IsConMode mode) (() :: Constraint) (UnionMergeable1 m)) => r) -> r

withBaseBranchingTrans ::
  forall mode m t m0 r.
  ( m ~ t m0,
    UnifiedBranching mode m0,
    (UnionMergeable1 m0) => UnionMergeable1 m
  ) =>
  ((If (IsConMode mode) (() :: Constraint) (UnionMergeable1 m)) => r) ->
  r
withBaseBranchingTrans r =
  case (eqT @mode @'Con, eqT @mode @'Sym) of
    (Just Refl, _) -> withBaseBranching @mode @m0 r
    (_, Just Refl) -> withBaseBranching @mode @m0 r
    _ -> error "impossible"

instance
  {-# INCOHERENT #-}
  ( Typeable mode,
    TryMerge m,
    If (IsConMode mode) (() :: Constraint) (UnionMergeable1 m)
  ) =>
  UnifiedBranching mode m
  where
  withBaseBranching r = r

instance
  {-# INCOHERENT #-}
  (Typeable mode, UnifiedBranching mode m, Applicative m) =>
  UnifiedBranching mode (FreshT m)
  where
  withBaseBranching = withBaseBranchingTrans @mode @(FreshT m)

instance
  {-# INCOHERENT #-}
  (Typeable mode, UnifiedBranching mode m, Applicative m) =>
  UnifiedBranching mode (MaybeT m)
  where
  withBaseBranching = withBaseBranchingTrans @mode @(MaybeT m)

instance
  {-# INCOHERENT #-}
  (Typeable mode, UnifiedBranching mode m, Applicative m) =>
  UnifiedBranching mode (IdentityT m)
  where
  withBaseBranching = withBaseBranchingTrans @mode @(IdentityT m)

instance
  {-# INCOHERENT #-}
  (Typeable mode, UnifiedBranching mode m, Applicative m) =>
  UnifiedBranching mode (ReaderT r m)
  where
  withBaseBranching = withBaseBranchingTrans @mode @(ReaderT r m)

instance
  {-# INCOHERENT #-}
  ( Typeable mode,
    UnifiedBranching mode m,
    Applicative m,
    Mergeable s
  ) =>
  UnifiedBranching mode (StateLazy.StateT s m)
  where
  withBaseBranching = withBaseBranchingTrans @mode @(StateLazy.StateT s m)

instance
  {-# INCOHERENT #-}
  ( Typeable mode,
    UnifiedBranching mode m,
    Applicative m,
    Monoid w,
    Mergeable w
  ) =>
  UnifiedBranching mode (StateStrict.StateT w m)
  where
  withBaseBranching = withBaseBranchingTrans @mode @(StateStrict.StateT w m)

instance
  {-# INCOHERENT #-}
  ( Typeable mode,
    UnifiedBranching mode m,
    Applicative m,
    Monoid w,
    Mergeable w
  ) =>
  UnifiedBranching mode (WriterLazy.WriterT w m)
  where
  withBaseBranching = withBaseBranchingTrans @mode @(WriterLazy.WriterT w m)

instance
  {-# INCOHERENT #-}
  ( Typeable mode,
    UnifiedBranching mode m,
    Applicative m,
    Monoid w,
    Mergeable w
  ) =>
  UnifiedBranching mode (WriterStrict.WriterT w m)
  where
  withBaseBranching = withBaseBranchingTrans @mode @(WriterStrict.WriterT w m)

instance
  {-# INCOHERENT #-}
  (Typeable mode, Mergeable e, UnifiedBranching mode m, Applicative m) =>
  UnifiedBranching mode (ExceptT e m)
  where
  withBaseBranching = withBaseBranchingTrans @mode @(ExceptT e m)

instance
  {-# INCOHERENT #-}
  (Typeable mode, Mergeable e, UnifiedBranching mode m, Monad m) =>
  UnifiedBranching mode (ContT e m)
  where
  withBaseBranching = withBaseBranchingTrans @mode @(ContT e m)

instance
  {-# INCOHERENT #-}
  ( Typeable mode,
    UnifiedBranching mode m,
    Applicative m,
    Mergeable s,
    Monoid w,
    Mergeable w
  ) =>
  UnifiedBranching mode (RWSLazy.RWST r w s m)
  where
  withBaseBranching = withBaseBranchingTrans @mode @(RWSLazy.RWST r w s m)

instance
  {-# INCOHERENT #-}
  ( Typeable mode,
    UnifiedBranching mode m,
    Applicative m,
    Mergeable s,
    Monoid w,
    Mergeable w
  ) =>
  UnifiedBranching mode (RWSStrict.RWST r w s m)
  where
  withBaseBranching = withBaseBranchingTrans @mode @(RWSStrict.RWST r w s m)
