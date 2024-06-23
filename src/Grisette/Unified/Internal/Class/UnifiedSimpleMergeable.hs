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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Grisette.Unified.Internal.Class.UnifiedSimpleMergeable
  ( mrgIf,
    liftBaseMonad,
    mrgIte,
    UnifiedBranching (..),
    UnifiedSimpleMergeable (..),
    UnifiedSimpleMergeable1 (..),
    UnifiedSimpleMergeable2 (..),
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
import Data.Typeable (Typeable)
import Grisette.Internal.Core.Control.Exception (AssertionError)
import Grisette.Internal.Core.Control.Monad.UnionM (liftUnionM)
import Grisette.Internal.Core.Data.Class.GenSym (FreshT)
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.SimpleMergeable
  ( SimpleMergeable,
    SimpleMergeable1,
    SimpleMergeable2,
    SymBranching,
  )
import qualified Grisette.Internal.Core.Data.Class.SimpleMergeable
import qualified Grisette.Internal.Core.Data.Class.SimpleMergeable as Grisette
import Grisette.Internal.Core.Data.Class.TryMerge
  ( TryMerge,
    mrgSingle,
  )
import Grisette.Internal.TH.DeriveUnifiedInterface
  ( deriveFunctorArgUnifiedInterfaces,
    deriveUnifiedInterface1s,
  )
import Grisette.Unified.Internal.BaseMonad (BaseMonad)
import Grisette.Unified.Internal.EvaluationMode
  ( EvaluationMode,
    IsConMode,
  )
import Grisette.Unified.Internal.UnifiedBool (UnifiedBool (GetBool))
import Grisette.Unified.Internal.Util (withMode)

mrgIf ::
  forall mode a m.
  (Typeable mode, Mergeable a, UnifiedBranching mode m) =>
  GetBool mode ->
  m a ->
  m a ->
  m a
mrgIf c t e =
  withMode @mode
    (withBaseBranching @mode @m $ if c then t else e)
    (withBaseBranching @mode @m $ Grisette.mrgIf c t e)
{-# INLINE mrgIf #-}

liftBaseMonad ::
  forall mode a m.
  ( Applicative m,
    UnifiedBranching mode m,
    Mergeable a
  ) =>
  BaseMonad mode a ->
  m a
liftBaseMonad b =
  withMode @mode
    (withBaseBranching @mode @m $ mrgSingle . runIdentity $ b)
    (withBaseBranching @mode @m $ liftUnionM b)
{-# INLINE liftBaseMonad #-}

mrgIte ::
  forall mode a.
  (Typeable mode, UnifiedSimpleMergeable mode a) =>
  GetBool mode ->
  a ->
  a ->
  a
mrgIte c t e =
  withMode @mode
    (withBaseSimpleMergeable @mode @a $ if c then t else e)
    ( withBaseSimpleMergeable @mode @a $
        Grisette.Internal.Core.Data.Class.SimpleMergeable.mrgIte c t e
    )

-- | A class that provides a unified simple merging.
--
-- We use this type class to help resolve the constraints for `SimpleMergeable`.
class UnifiedSimpleMergeable mode a where
  withBaseSimpleMergeable ::
    ((If (IsConMode mode) (() :: Constraint) (SimpleMergeable a)) => r) -> r

-- | A class that provides lifting of unified simple merging.
--
-- We use this type class to help resolve the constraints for
-- `SimpleMergeable1`.
class UnifiedSimpleMergeable1 mode f where
  withBaseSimpleMergeable1 ::
    ((If (IsConMode mode) (() :: Constraint) (SimpleMergeable1 f)) => r) -> r

-- | A class that provides lifting of unified simple merging.
--
-- We use this type class to help resolve the constraints for
-- `SimpleMergeable2`.
class UnifiedSimpleMergeable2 mode f where
  withBaseSimpleMergeable2 ::
    ((If (IsConMode mode) (() :: Constraint) (SimpleMergeable2 f)) => r) -> r

-- | A class that provides a unified branching operation for unified types.
--
-- On monads with 'TryMerge' instance, the 'mrgIf' function could use concrete
-- Boolean variable for branching.
--
-- On monads with 'SymBranching' instance, the 'mrgIf' function could use
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
  withBaseBranching ::
    ((If (IsConMode mode) (() :: Constraint) (SymBranching m)) => r) -> r

withBaseBranchingTrans ::
  forall mode m t m0 r.
  ( m ~ t m0,
    UnifiedBranching mode m0,
    (SymBranching m0) => SymBranching m
  ) =>
  ((If (IsConMode mode) (() :: Constraint) (SymBranching m)) => r) ->
  r
withBaseBranchingTrans r =
  withMode @mode
    (withBaseBranching @mode @m0 r)
    (withBaseBranching @mode @m0 r)
{-# INLINE withBaseBranchingTrans #-}

instance
  {-# INCOHERENT #-}
  ( Typeable mode,
    TryMerge m,
    If (IsConMode mode) (() :: Constraint) (SymBranching m)
  ) =>
  UnifiedBranching mode m
  where
  withBaseBranching r = r
  {-# INLINE withBaseBranching #-}

instance
  {-# INCOHERENT #-}
  ( Typeable mode,
    If (IsConMode mode) (() :: Constraint) (SimpleMergeable m)
  ) =>
  UnifiedSimpleMergeable mode m
  where
  withBaseSimpleMergeable r = r
  {-# INLINE withBaseSimpleMergeable #-}

instance
  {-# INCOHERENT #-}
  ( Typeable mode,
    If (IsConMode mode) (() :: Constraint) (SimpleMergeable1 m)
  ) =>
  UnifiedSimpleMergeable1 mode m
  where
  withBaseSimpleMergeable1 r = r
  {-# INLINE withBaseSimpleMergeable1 #-}

instance
  {-# INCOHERENT #-}
  ( Typeable mode,
    If (IsConMode mode) (() :: Constraint) (SimpleMergeable2 m)
  ) =>
  UnifiedSimpleMergeable2 mode m
  where
  withBaseSimpleMergeable2 r = r
  {-# INLINE withBaseSimpleMergeable2 #-}

deriveFunctorArgUnifiedInterfaces
  ''UnifiedSimpleMergeable
  'withBaseSimpleMergeable
  ''UnifiedSimpleMergeable1
  'withBaseSimpleMergeable1
  [ ''(),
    ''(,),
    ''(,,),
    ''(,,,),
    ''(,,,,),
    ''(,,,,,),
    ''(,,,,,,),
    ''(,,,,,,,),
    ''(,,,,,,,,),
    ''(,,,,,,,,,),
    ''(,,,,,,,,,,),
    ''(,,,,,,,,,,,),
    ''(,,,,,,,,,,,,),
    ''(,,,,,,,,,,,,,),
    ''(,,,,,,,,,,,,,,),
    ''AssertionError,
    ''Identity
  ]

deriveUnifiedInterface1s
  ''UnifiedSimpleMergeable
  'withBaseSimpleMergeable
  ''UnifiedSimpleMergeable1
  'withBaseSimpleMergeable1
  [ ''(,),
    ''(,,),
    ''(,,,),
    ''(,,,,),
    ''(,,,,,),
    ''(,,,,,,),
    ''(,,,,,,,),
    ''(,,,,,,,,),
    ''(,,,,,,,,,),
    ''(,,,,,,,,,,),
    ''(,,,,,,,,,,,),
    ''(,,,,,,,,,,,,),
    ''(,,,,,,,,,,,,,),
    ''(,,,,,,,,,,,,,,),
    ''Identity
  ]

instance (Typeable mode) => UnifiedSimpleMergeable2 mode (,) where
  withBaseSimpleMergeable2 r = withMode @mode r r
  {-# INLINE withBaseSimpleMergeable2 #-}

instance
  (Typeable mode, UnifiedSimpleMergeable mode a) =>
  UnifiedSimpleMergeable2 mode ((,,) a)
  where
  withBaseSimpleMergeable2 r =
    withMode @mode
      (withBaseSimpleMergeable @mode @a r)
      (withBaseSimpleMergeable @mode @a r)
  {-# INLINE withBaseSimpleMergeable2 #-}

instance
  ( Typeable mode,
    UnifiedSimpleMergeable mode a,
    UnifiedSimpleMergeable mode b
  ) =>
  UnifiedSimpleMergeable2 mode ((,,,) a b)
  where
  withBaseSimpleMergeable2 r =
    withMode @mode
      (withBaseSimpleMergeable @mode @a $ withBaseSimpleMergeable @mode @b r)
      (withBaseSimpleMergeable @mode @a $ withBaseSimpleMergeable @mode @b r)
  {-# INLINE withBaseSimpleMergeable2 #-}

instance
  (Typeable mode, UnifiedSimpleMergeable mode b) =>
  UnifiedSimpleMergeable mode (a -> b)
  where
  withBaseSimpleMergeable r =
    withMode @mode
      (withBaseSimpleMergeable @mode @b r)
      (withBaseSimpleMergeable @mode @b r)
  {-# INLINE withBaseSimpleMergeable #-}

instance (Typeable mode) => UnifiedSimpleMergeable1 mode ((->) a) where
  withBaseSimpleMergeable1 r = withMode @mode r r
  {-# INLINE withBaseSimpleMergeable1 #-}

instance (Typeable mode) => UnifiedSimpleMergeable2 mode (->) where
  withBaseSimpleMergeable2 r = withMode @mode r r
  {-# INLINE withBaseSimpleMergeable2 #-}

instance
  (Typeable mode, UnifiedBranching mode m, Mergeable a) =>
  UnifiedSimpleMergeable mode (FreshT m a)
  where
  withBaseSimpleMergeable r =
    withMode @mode
      (withBaseBranching @mode @(FreshT m) r)
      (withBaseBranching @mode @(FreshT m) r)
  {-# INLINE withBaseSimpleMergeable #-}

instance
  (Typeable mode, UnifiedBranching mode m) =>
  UnifiedSimpleMergeable1 mode (FreshT m)
  where
  withBaseSimpleMergeable1 r =
    withMode @mode
      (withBaseBranching @mode @(FreshT m) r)
      (withBaseBranching @mode @(FreshT m) r)
  {-# INLINE withBaseSimpleMergeable1 #-}

instance
  (Typeable mode, UnifiedBranching mode m) =>
  UnifiedBranching mode (FreshT m)
  where
  withBaseBranching = withBaseBranchingTrans @mode @(FreshT m)
  {-# INLINE withBaseBranching #-}

instance
  (Typeable mode, UnifiedBranching mode m, Mergeable a) =>
  UnifiedSimpleMergeable mode (MaybeT m a)
  where
  withBaseSimpleMergeable r =
    withMode @mode
      (withBaseBranching @mode @(MaybeT m) r)
      (withBaseBranching @mode @(MaybeT m) r)
  {-# INLINE withBaseSimpleMergeable #-}

instance
  (Typeable mode, UnifiedBranching mode m) =>
  UnifiedSimpleMergeable1 mode (MaybeT m)
  where
  withBaseSimpleMergeable1 r =
    withMode @mode
      (withBaseBranching @mode @(MaybeT m) r)
      (withBaseBranching @mode @(MaybeT m) r)
  {-# INLINE withBaseSimpleMergeable1 #-}

instance
  (Typeable mode, UnifiedBranching mode m) =>
  UnifiedBranching mode (MaybeT m)
  where
  withBaseBranching = withBaseBranchingTrans @mode @(MaybeT m)
  {-# INLINE withBaseBranching #-}

instance
  (Typeable mode, UnifiedBranching mode m, Mergeable a) =>
  UnifiedSimpleMergeable mode (IdentityT m a)
  where
  withBaseSimpleMergeable r =
    withMode @mode
      (withBaseBranching @mode @(IdentityT m) r)
      (withBaseBranching @mode @(IdentityT m) r)
  {-# INLINE withBaseSimpleMergeable #-}

instance
  (Typeable mode, UnifiedBranching mode m) =>
  UnifiedSimpleMergeable1 mode (IdentityT m)
  where
  withBaseSimpleMergeable1 r =
    withMode @mode
      (withBaseBranching @mode @(IdentityT m) r)
      (withBaseBranching @mode @(IdentityT m) r)
  {-# INLINE withBaseSimpleMergeable1 #-}

instance
  (Typeable mode, UnifiedBranching mode m) =>
  UnifiedBranching mode (IdentityT m)
  where
  withBaseBranching = withBaseBranchingTrans @mode @(IdentityT m)
  {-# INLINE withBaseBranching #-}

instance
  (Typeable mode, UnifiedBranching mode m, Mergeable a) =>
  UnifiedSimpleMergeable mode (ReaderT r m a)
  where
  withBaseSimpleMergeable r =
    withMode @mode
      (withBaseBranching @mode @(ReaderT r m) r)
      (withBaseBranching @mode @(ReaderT r m) r)
  {-# INLINE withBaseSimpleMergeable #-}

instance
  (Typeable mode, UnifiedBranching mode m) =>
  UnifiedSimpleMergeable1 mode (ReaderT r m)
  where
  withBaseSimpleMergeable1 r =
    withMode @mode
      (withBaseBranching @mode @(ReaderT r m) r)
      (withBaseBranching @mode @(ReaderT r m) r)
  {-# INLINE withBaseSimpleMergeable1 #-}

instance
  (Typeable mode, UnifiedBranching mode m) =>
  UnifiedBranching mode (ReaderT r m)
  where
  withBaseBranching = withBaseBranchingTrans @mode @(ReaderT r m)
  {-# INLINE withBaseBranching #-}

instance
  (Typeable mode, UnifiedBranching mode m, Mergeable s, Mergeable a) =>
  UnifiedSimpleMergeable mode (StateLazy.StateT s m a)
  where
  withBaseSimpleMergeable r =
    withMode @mode
      (withBaseBranching @mode @(StateLazy.StateT s m) r)
      (withBaseBranching @mode @(StateLazy.StateT s m) r)
  {-# INLINE withBaseSimpleMergeable #-}

instance
  (Typeable mode, UnifiedBranching mode m, Mergeable s) =>
  UnifiedSimpleMergeable1 mode (StateLazy.StateT s m)
  where
  withBaseSimpleMergeable1 r =
    withMode @mode
      (withBaseBranching @mode @(StateLazy.StateT s m) r)
      (withBaseBranching @mode @(StateLazy.StateT s m) r)
  {-# INLINE withBaseSimpleMergeable1 #-}

instance
  ( Typeable mode,
    UnifiedBranching mode m,
    Mergeable s
  ) =>
  UnifiedBranching mode (StateLazy.StateT s m)
  where
  withBaseBranching = withBaseBranchingTrans @mode @(StateLazy.StateT s m)
  {-# INLINE withBaseBranching #-}

instance
  (Typeable mode, UnifiedBranching mode m, Mergeable s, Mergeable a) =>
  UnifiedSimpleMergeable mode (StateStrict.StateT s m a)
  where
  withBaseSimpleMergeable r =
    withMode @mode
      (withBaseBranching @mode @(StateStrict.StateT s m) r)
      (withBaseBranching @mode @(StateStrict.StateT s m) r)
  {-# INLINE withBaseSimpleMergeable #-}

instance
  (Typeable mode, UnifiedBranching mode m, Mergeable s) =>
  UnifiedSimpleMergeable1 mode (StateStrict.StateT s m)
  where
  withBaseSimpleMergeable1 r =
    withMode @mode
      (withBaseBranching @mode @(StateStrict.StateT s m) r)
      (withBaseBranching @mode @(StateStrict.StateT s m) r)
  {-# INLINE withBaseSimpleMergeable1 #-}

instance
  ( Typeable mode,
    UnifiedBranching mode m,
    Mergeable s
  ) =>
  UnifiedBranching mode (StateStrict.StateT s m)
  where
  withBaseBranching = withBaseBranchingTrans @mode @(StateStrict.StateT s m)
  {-# INLINE withBaseBranching #-}

instance
  ( Typeable mode,
    UnifiedBranching mode m,
    Mergeable w,
    Mergeable a,
    Monoid w
  ) =>
  UnifiedSimpleMergeable mode (WriterLazy.WriterT w m a)
  where
  withBaseSimpleMergeable r =
    withMode @mode
      (withBaseBranching @mode @(WriterLazy.WriterT w m) r)
      (withBaseBranching @mode @(WriterLazy.WriterT w m) r)
  {-# INLINE withBaseSimpleMergeable #-}

instance
  (Typeable mode, UnifiedBranching mode m, Mergeable w, Monoid w) =>
  UnifiedSimpleMergeable1 mode (WriterLazy.WriterT w m)
  where
  withBaseSimpleMergeable1 r =
    withMode @mode
      (withBaseBranching @mode @(WriterLazy.WriterT w m) r)
      (withBaseBranching @mode @(WriterLazy.WriterT w m) r)
  {-# INLINE withBaseSimpleMergeable1 #-}

instance
  ( Typeable mode,
    UnifiedBranching mode m,
    Monoid w,
    Mergeable w
  ) =>
  UnifiedBranching mode (WriterLazy.WriterT w m)
  where
  withBaseBranching = withBaseBranchingTrans @mode @(WriterLazy.WriterT w m)
  {-# INLINE withBaseBranching #-}

instance
  ( Typeable mode,
    UnifiedBranching mode m,
    Mergeable w,
    Mergeable a,
    Monoid w
  ) =>
  UnifiedSimpleMergeable mode (WriterStrict.WriterT w m a)
  where
  withBaseSimpleMergeable r =
    withMode @mode
      (withBaseBranching @mode @(WriterStrict.WriterT w m) r)
      (withBaseBranching @mode @(WriterStrict.WriterT w m) r)
  {-# INLINE withBaseSimpleMergeable #-}

instance
  (Typeable mode, UnifiedBranching mode m, Mergeable w, Monoid w) =>
  UnifiedSimpleMergeable1 mode (WriterStrict.WriterT w m)
  where
  withBaseSimpleMergeable1 r =
    withMode @mode
      (withBaseBranching @mode @(WriterStrict.WriterT w m) r)
      (withBaseBranching @mode @(WriterStrict.WriterT w m) r)
  {-# INLINE withBaseSimpleMergeable1 #-}

instance
  ( Typeable mode,
    UnifiedBranching mode m,
    Monoid w,
    Mergeable w
  ) =>
  UnifiedBranching mode (WriterStrict.WriterT w m)
  where
  withBaseBranching = withBaseBranchingTrans @mode @(WriterStrict.WriterT w m)
  {-# INLINE withBaseBranching #-}

instance
  ( Typeable mode,
    UnifiedBranching mode m,
    Mergeable e,
    Mergeable a
  ) =>
  UnifiedSimpleMergeable mode (ExceptT e m a)
  where
  withBaseSimpleMergeable r =
    withMode @mode
      (withBaseBranching @mode @(ExceptT e m) r)
      (withBaseBranching @mode @(ExceptT e m) r)
  {-# INLINE withBaseSimpleMergeable #-}

instance
  (Typeable mode, UnifiedBranching mode m, Mergeable e) =>
  UnifiedSimpleMergeable1 mode (ExceptT e m)
  where
  withBaseSimpleMergeable1 r =
    withMode @mode
      (withBaseBranching @mode @(ExceptT e m) r)
      (withBaseBranching @mode @(ExceptT e m) r)
  {-# INLINE withBaseSimpleMergeable1 #-}

instance
  (Typeable mode, Mergeable e, UnifiedBranching mode m) =>
  UnifiedBranching mode (ExceptT e m)
  where
  withBaseBranching = withBaseBranchingTrans @mode @(ExceptT e m)
  {-# INLINE withBaseBranching #-}

instance
  ( Typeable mode,
    UnifiedBranching mode m,
    Mergeable r,
    Mergeable a
  ) =>
  UnifiedSimpleMergeable mode (ContT r m a)
  where
  withBaseSimpleMergeable r =
    withMode @mode
      (withBaseBranching @mode @(ContT r m) r)
      (withBaseBranching @mode @(ContT r m) r)
  {-# INLINE withBaseSimpleMergeable #-}

instance
  (Typeable mode, UnifiedBranching mode m, Mergeable r) =>
  UnifiedSimpleMergeable1 mode (ContT r m)
  where
  withBaseSimpleMergeable1 r =
    withMode @mode
      (withBaseBranching @mode @(ContT r m) r)
      (withBaseBranching @mode @(ContT r m) r)
  {-# INLINE withBaseSimpleMergeable1 #-}

instance
  (Typeable mode, Mergeable r, UnifiedBranching mode m) =>
  UnifiedBranching mode (ContT r m)
  where
  withBaseBranching = withBaseBranchingTrans @mode @(ContT r m)
  {-# INLINE withBaseBranching #-}

instance
  ( Typeable mode,
    UnifiedBranching mode m,
    Mergeable w,
    Monoid w,
    Mergeable s,
    Mergeable a
  ) =>
  UnifiedSimpleMergeable mode (RWSLazy.RWST r w s m a)
  where
  withBaseSimpleMergeable r =
    withMode @mode
      (withBaseBranching @mode @(RWSLazy.RWST r w s m) r)
      (withBaseBranching @mode @(RWSLazy.RWST r w s m) r)
  {-# INLINE withBaseSimpleMergeable #-}

instance
  ( Typeable mode,
    UnifiedBranching mode m,
    Mergeable w,
    Monoid w,
    Mergeable s
  ) =>
  UnifiedSimpleMergeable1 mode (RWSLazy.RWST r w s m)
  where
  withBaseSimpleMergeable1 r =
    withMode @mode
      (withBaseBranching @mode @(RWSLazy.RWST r w s m) r)
      (withBaseBranching @mode @(RWSLazy.RWST r w s m) r)
  {-# INLINE withBaseSimpleMergeable1 #-}

instance
  ( Typeable mode,
    UnifiedBranching mode m,
    Mergeable s,
    Monoid w,
    Mergeable w
  ) =>
  UnifiedBranching mode (RWSLazy.RWST r w s m)
  where
  withBaseBranching = withBaseBranchingTrans @mode @(RWSLazy.RWST r w s m)
  {-# INLINE withBaseBranching #-}

instance
  ( Typeable mode,
    UnifiedBranching mode m,
    Mergeable w,
    Monoid w,
    Mergeable s,
    Mergeable a
  ) =>
  UnifiedSimpleMergeable mode (RWSStrict.RWST r w s m a)
  where
  withBaseSimpleMergeable r =
    withMode @mode
      (withBaseBranching @mode @(RWSStrict.RWST r w s m) r)
      (withBaseBranching @mode @(RWSStrict.RWST r w s m) r)
  {-# INLINE withBaseSimpleMergeable #-}

instance
  ( Typeable mode,
    UnifiedBranching mode m,
    Mergeable w,
    Monoid w,
    Mergeable s
  ) =>
  UnifiedSimpleMergeable1 mode (RWSStrict.RWST r w s m)
  where
  withBaseSimpleMergeable1 r =
    withMode @mode
      (withBaseBranching @mode @(RWSStrict.RWST r w s m) r)
      (withBaseBranching @mode @(RWSStrict.RWST r w s m) r)
  {-# INLINE withBaseSimpleMergeable1 #-}

instance
  ( Typeable mode,
    UnifiedBranching mode m,
    Mergeable s,
    Monoid w,
    Mergeable w
  ) =>
  UnifiedBranching mode (RWSStrict.RWST r w s m)
  where
  withBaseBranching = withBaseBranchingTrans @mode @(RWSStrict.RWST r w s m)
  {-# INLINE withBaseBranching #-}
