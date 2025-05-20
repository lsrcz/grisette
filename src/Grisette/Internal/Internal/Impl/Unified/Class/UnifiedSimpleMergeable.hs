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

-- |
-- Module      :   Grisette.Internal.Internal.Impl.Unified.Class.UnifiedSimpleMergeable
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Impl.Unified.Class.UnifiedSimpleMergeable
  ( mrgIf,
    liftBaseMonad,
    mrgIte,
    mrgIte1,
    liftMrgIte,
    mrgIte2,
    liftMrgIte2,
    simpleMerge,
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
import Grisette.Internal.Core.Control.Exception (AssertionError)
import Grisette.Internal.Core.Control.Monad.Union (liftUnion)
import Grisette.Internal.Core.Data.Class.GenSym (FreshT)
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable, Mergeable1)
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
import qualified Grisette.Internal.Core.Data.Class.UnionView as Grisette
import Grisette.Internal.Internal.Decl.Unified.Class.UnifiedSimpleMergeable
  ( UnifiedBranching (withBaseBranching),
    UnifiedSimpleMergeable (withBaseSimpleMergeable),
    UnifiedSimpleMergeable1 (withBaseSimpleMergeable1),
    UnifiedSimpleMergeable2 (withBaseSimpleMergeable2),
  )
import Grisette.Internal.TH.Derivation.Derive (derive)
import Grisette.Internal.Unified.BaseMonad (BaseMonad)
import Grisette.Internal.Unified.EvalModeTag (IsConMode)
import Grisette.Internal.Unified.UnifiedBool (UnifiedBool (GetBool))
import Grisette.Internal.Unified.Util (DecideEvalMode, withMode)

-- | Unified `Grisette.mrgIf`.
--
-- This function isn't able to infer the mode of the boolean variable, so you
-- need to provide the mode explicitly. For example:
--
-- > mrgIf @mode (a .== b) ...
-- > mrgIf (a .== b :: SymBool) ...
-- > mrgIf (a .== b :: GetBool mode) ...
mrgIf ::
  forall mode a m.
  (DecideEvalMode mode, Mergeable a, UnifiedBranching mode m) =>
  GetBool mode ->
  m a ->
  m a ->
  m a
mrgIf c t e =
  withMode @mode
    (if c then t else e)
    (withBaseBranching @mode @m $ Grisette.mrgIf c t e)
{-# INLINE mrgIf #-}

-- | Unified lifting of a base monad.
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
    (withBaseBranching @mode @m $ liftUnion b)
{-# INLINE liftBaseMonad #-}

-- | Unified merge of simply mergeable values in the base monad.
simpleMerge ::
  forall mode a.
  (DecideEvalMode mode, UnifiedSimpleMergeable mode a) =>
  BaseMonad mode a ->
  a
simpleMerge =
  withMode @mode
    runIdentity
    (withBaseSimpleMergeable @mode @a Grisette.simpleMerge)

-- | Unified `Grisette.mrgIte`.
mrgIte ::
  forall mode a.
  (DecideEvalMode mode, UnifiedSimpleMergeable mode a) =>
  GetBool mode ->
  a ->
  a ->
  a
mrgIte c t e =
  withMode @mode
    (if c then t else e)
    ( withBaseSimpleMergeable @mode @a $
        Grisette.Internal.Core.Data.Class.SimpleMergeable.mrgIte c t e
    )

-- | Unified `Grisette.mrgIte1`.
mrgIte1 ::
  forall mode f a.
  ( DecideEvalMode mode,
    UnifiedSimpleMergeable1 mode f,
    UnifiedSimpleMergeable mode a
  ) =>
  GetBool mode ->
  f a ->
  f a ->
  f a
mrgIte1 c t e =
  withMode @mode
    (if c then t else e)
    ( withBaseSimpleMergeable @mode @a $
        withBaseSimpleMergeable1 @mode @f $
          Grisette.Internal.Core.Data.Class.SimpleMergeable.mrgIte1 c t e
    )

-- | Unified `Grisette.liftMrgIte`.
liftMrgIte ::
  forall mode f a.
  ( DecideEvalMode mode,
    UnifiedSimpleMergeable1 mode f
  ) =>
  (GetBool mode -> a -> a -> a) ->
  GetBool mode ->
  f a ->
  f a ->
  f a
liftMrgIte f c t e =
  withMode @mode
    (if c then t else e)
    ( withBaseSimpleMergeable1 @mode @f $
        Grisette.Internal.Core.Data.Class.SimpleMergeable.liftMrgIte f c t e
    )

-- | Unified `Grisette.mrgIte2`.
mrgIte2 ::
  forall mode f a b.
  ( DecideEvalMode mode,
    UnifiedSimpleMergeable2 mode f,
    UnifiedSimpleMergeable mode a,
    UnifiedSimpleMergeable mode b
  ) =>
  GetBool mode ->
  f a b ->
  f a b ->
  f a b
mrgIte2 c t e =
  withMode @mode
    (if c then t else e)
    ( withBaseSimpleMergeable @mode @a $
        withBaseSimpleMergeable @mode @b $
          withBaseSimpleMergeable2 @mode @f $
            Grisette.Internal.Core.Data.Class.SimpleMergeable.mrgIte2 c t e
    )

-- | Unified `Grisette.liftMrgIte2`.
liftMrgIte2 ::
  forall mode f a b.
  ( DecideEvalMode mode,
    UnifiedSimpleMergeable2 mode f
  ) =>
  (GetBool mode -> a -> a -> a) ->
  (GetBool mode -> b -> b -> b) ->
  GetBool mode ->
  f a b ->
  f a b ->
  f a b
liftMrgIte2 f g c t e =
  withMode @mode
    (if c then t else e)
    ( withBaseSimpleMergeable2 @mode @f $
        Grisette.Internal.Core.Data.Class.SimpleMergeable.liftMrgIte2 f g c t e
    )

instance
  {-# INCOHERENT #-}
  ( DecideEvalMode mode,
    TryMerge m,
    If (IsConMode mode) ((TryMerge m) :: Constraint) (SymBranching m)
  ) =>
  UnifiedBranching mode m
  where
  withBaseBranching r = r
  {-# INLINE withBaseBranching #-}

instance
  {-# INCOHERENT #-}
  ( DecideEvalMode mode,
    Mergeable m,
    If (IsConMode mode) (() :: Constraint) (SimpleMergeable m)
  ) =>
  UnifiedSimpleMergeable mode m
  where
  withBaseSimpleMergeable r = r
  {-# INLINE withBaseSimpleMergeable #-}

instance
  {-# INCOHERENT #-}
  ( DecideEvalMode mode,
    If (IsConMode mode) (() :: Constraint) (SimpleMergeable1 m)
  ) =>
  UnifiedSimpleMergeable1 mode m
  where
  withBaseSimpleMergeable1 r = r
  {-# INLINE withBaseSimpleMergeable1 #-}

instance
  {-# INCOHERENT #-}
  ( DecideEvalMode mode,
    If (IsConMode mode) (() :: Constraint) (SimpleMergeable2 m)
  ) =>
  UnifiedSimpleMergeable2 mode m
  where
  withBaseSimpleMergeable2 r = r
  {-# INLINE withBaseSimpleMergeable2 #-}

derive
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
    ''(,,,,,,,,,,,,,,)
  ]
  [ ''UnifiedSimpleMergeable,
    ''UnifiedSimpleMergeable1,
    ''UnifiedSimpleMergeable2
  ]

derive
  [''Identity]
  [''UnifiedSimpleMergeable, ''UnifiedSimpleMergeable1]

derive
  [''(), ''AssertionError]
  [''UnifiedSimpleMergeable]

instance
  (DecideEvalMode mode, UnifiedSimpleMergeable mode b) =>
  UnifiedSimpleMergeable mode (a -> b)
  where
  withBaseSimpleMergeable r =
    withMode @mode
      (withBaseSimpleMergeable @mode @b r)
      (withBaseSimpleMergeable @mode @b r)
  {-# INLINE withBaseSimpleMergeable #-}

instance (DecideEvalMode mode) => UnifiedSimpleMergeable1 mode ((->) a) where
  withBaseSimpleMergeable1 r = withMode @mode r r
  {-# INLINE withBaseSimpleMergeable1 #-}

instance (DecideEvalMode mode) => UnifiedSimpleMergeable2 mode (->) where
  withBaseSimpleMergeable2 r = withMode @mode r r
  {-# INLINE withBaseSimpleMergeable2 #-}

instance
  (DecideEvalMode mode, UnifiedBranching mode m, Mergeable a, Mergeable1 m) =>
  UnifiedSimpleMergeable mode (FreshT m a)
  where
  withBaseSimpleMergeable r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseSimpleMergeable #-}

instance
  (DecideEvalMode mode, UnifiedBranching mode m) =>
  UnifiedSimpleMergeable1 mode (FreshT m)
  where
  withBaseSimpleMergeable1 r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseSimpleMergeable1 #-}

instance
  (DecideEvalMode mode, UnifiedBranching mode m) =>
  UnifiedBranching mode (FreshT m)
  where
  withBaseBranching r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseBranching #-}

instance
  (DecideEvalMode mode, UnifiedBranching mode m, Mergeable a, Mergeable1 m) =>
  UnifiedSimpleMergeable mode (MaybeT m a)
  where
  withBaseSimpleMergeable r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseSimpleMergeable #-}

instance
  (DecideEvalMode mode, UnifiedBranching mode m) =>
  UnifiedSimpleMergeable1 mode (MaybeT m)
  where
  withBaseSimpleMergeable1 r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseSimpleMergeable1 #-}

instance
  (DecideEvalMode mode, UnifiedBranching mode m) =>
  UnifiedBranching mode (MaybeT m)
  where
  withBaseBranching r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseBranching #-}

instance
  (DecideEvalMode mode, UnifiedBranching mode m, Mergeable a, Mergeable1 m) =>
  UnifiedSimpleMergeable mode (IdentityT m a)
  where
  withBaseSimpleMergeable r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseSimpleMergeable #-}

instance
  (DecideEvalMode mode, UnifiedBranching mode m) =>
  UnifiedSimpleMergeable1 mode (IdentityT m)
  where
  withBaseSimpleMergeable1 r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseSimpleMergeable1 #-}

instance
  (DecideEvalMode mode, UnifiedBranching mode m) =>
  UnifiedBranching mode (IdentityT m)
  where
  withBaseBranching r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseBranching #-}

instance
  (DecideEvalMode mode, UnifiedBranching mode m, Mergeable a, Mergeable1 m) =>
  UnifiedSimpleMergeable mode (ReaderT r m a)
  where
  withBaseSimpleMergeable r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseSimpleMergeable #-}

instance
  (DecideEvalMode mode, UnifiedBranching mode m) =>
  UnifiedSimpleMergeable1 mode (ReaderT r m)
  where
  withBaseSimpleMergeable1 r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseSimpleMergeable1 #-}

instance
  (DecideEvalMode mode, UnifiedBranching mode m) =>
  UnifiedBranching mode (ReaderT r m)
  where
  withBaseBranching r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseBranching #-}

instance
  ( DecideEvalMode mode,
    UnifiedBranching mode m,
    Mergeable s,
    Mergeable a,
    Mergeable1 m
  ) =>
  UnifiedSimpleMergeable mode (StateLazy.StateT s m a)
  where
  withBaseSimpleMergeable r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseSimpleMergeable #-}

instance
  (DecideEvalMode mode, UnifiedBranching mode m, Mergeable s) =>
  UnifiedSimpleMergeable1 mode (StateLazy.StateT s m)
  where
  withBaseSimpleMergeable1 r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseSimpleMergeable1 #-}

instance
  ( DecideEvalMode mode,
    UnifiedBranching mode m,
    Mergeable s
  ) =>
  UnifiedBranching mode (StateLazy.StateT s m)
  where
  withBaseBranching r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseBranching #-}

instance
  ( DecideEvalMode mode,
    UnifiedBranching mode m,
    Mergeable s,
    Mergeable a,
    Mergeable1 m
  ) =>
  UnifiedSimpleMergeable mode (StateStrict.StateT s m a)
  where
  withBaseSimpleMergeable r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseSimpleMergeable #-}

instance
  (DecideEvalMode mode, UnifiedBranching mode m, Mergeable s) =>
  UnifiedSimpleMergeable1 mode (StateStrict.StateT s m)
  where
  withBaseSimpleMergeable1 r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseSimpleMergeable1 #-}

instance
  ( DecideEvalMode mode,
    UnifiedBranching mode m,
    Mergeable s
  ) =>
  UnifiedBranching mode (StateStrict.StateT s m)
  where
  withBaseBranching r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseBranching #-}

instance
  ( DecideEvalMode mode,
    UnifiedBranching mode m,
    Mergeable w,
    Mergeable a,
    Monoid w,
    Mergeable1 m
  ) =>
  UnifiedSimpleMergeable mode (WriterLazy.WriterT w m a)
  where
  withBaseSimpleMergeable r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseSimpleMergeable #-}

instance
  ( DecideEvalMode mode,
    UnifiedBranching mode m,
    Mergeable w,
    Monoid w,
    Mergeable1 m
  ) =>
  UnifiedSimpleMergeable1 mode (WriterLazy.WriterT w m)
  where
  withBaseSimpleMergeable1 r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseSimpleMergeable1 #-}

instance
  ( DecideEvalMode mode,
    UnifiedBranching mode m,
    Monoid w,
    Mergeable w
  ) =>
  UnifiedBranching mode (WriterLazy.WriterT w m)
  where
  withBaseBranching r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseBranching #-}

instance
  ( DecideEvalMode mode,
    UnifiedBranching mode m,
    Mergeable w,
    Mergeable a,
    Monoid w,
    Mergeable1 m
  ) =>
  UnifiedSimpleMergeable mode (WriterStrict.WriterT w m a)
  where
  withBaseSimpleMergeable r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseSimpleMergeable #-}

instance
  ( DecideEvalMode mode,
    UnifiedBranching mode m,
    Mergeable w,
    Monoid w,
    Mergeable1 m
  ) =>
  UnifiedSimpleMergeable1 mode (WriterStrict.WriterT w m)
  where
  withBaseSimpleMergeable1 r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseSimpleMergeable1 #-}

instance
  ( DecideEvalMode mode,
    UnifiedBranching mode m,
    Monoid w,
    Mergeable w,
    Mergeable1 m
  ) =>
  UnifiedBranching mode (WriterStrict.WriterT w m)
  where
  withBaseBranching r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseBranching #-}

instance
  ( DecideEvalMode mode,
    UnifiedBranching mode m,
    Mergeable e,
    Mergeable a,
    Mergeable1 m
  ) =>
  UnifiedSimpleMergeable mode (ExceptT e m a)
  where
  withBaseSimpleMergeable r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseSimpleMergeable #-}

instance
  (DecideEvalMode mode, UnifiedBranching mode m, Mergeable e, Mergeable1 m) =>
  UnifiedSimpleMergeable1 mode (ExceptT e m)
  where
  withBaseSimpleMergeable1 r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseSimpleMergeable1 #-}

instance
  (DecideEvalMode mode, Mergeable e, UnifiedBranching mode m) =>
  UnifiedBranching mode (ExceptT e m)
  where
  withBaseBranching r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseBranching #-}

instance
  ( DecideEvalMode mode,
    UnifiedBranching mode m,
    Mergeable r,
    Mergeable a,
    Mergeable1 m
  ) =>
  UnifiedSimpleMergeable mode (ContT r m a)
  where
  withBaseSimpleMergeable r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseSimpleMergeable #-}

instance
  (DecideEvalMode mode, UnifiedBranching mode m, Mergeable r, Mergeable1 m) =>
  UnifiedSimpleMergeable1 mode (ContT r m)
  where
  withBaseSimpleMergeable1 r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseSimpleMergeable1 #-}

instance
  (DecideEvalMode mode, Mergeable r, UnifiedBranching mode m, Mergeable1 m) =>
  UnifiedBranching mode (ContT r m)
  where
  withBaseBranching r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseBranching #-}

instance
  ( DecideEvalMode mode,
    UnifiedBranching mode m,
    Mergeable w,
    Monoid w,
    Mergeable s,
    Mergeable a,
    Mergeable1 m
  ) =>
  UnifiedSimpleMergeable mode (RWSLazy.RWST r w s m a)
  where
  withBaseSimpleMergeable r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseSimpleMergeable #-}

instance
  ( DecideEvalMode mode,
    UnifiedBranching mode m,
    Mergeable w,
    Monoid w,
    Mergeable s,
    Mergeable1 m
  ) =>
  UnifiedSimpleMergeable1 mode (RWSLazy.RWST r w s m)
  where
  withBaseSimpleMergeable1 r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseSimpleMergeable1 #-}

instance
  ( DecideEvalMode mode,
    UnifiedBranching mode m,
    Mergeable s,
    Monoid w,
    Mergeable w
  ) =>
  UnifiedBranching mode (RWSLazy.RWST r w s m)
  where
  withBaseBranching r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseBranching #-}

instance
  ( DecideEvalMode mode,
    UnifiedBranching mode m,
    Mergeable w,
    Monoid w,
    Mergeable s,
    Mergeable a,
    Mergeable1 m
  ) =>
  UnifiedSimpleMergeable mode (RWSStrict.RWST r w s m a)
  where
  withBaseSimpleMergeable r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseSimpleMergeable #-}

instance
  ( DecideEvalMode mode,
    UnifiedBranching mode m,
    Mergeable w,
    Monoid w,
    Mergeable s
  ) =>
  UnifiedSimpleMergeable1 mode (RWSStrict.RWST r w s m)
  where
  withBaseSimpleMergeable1 r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseSimpleMergeable1 #-}

instance
  ( DecideEvalMode mode,
    UnifiedBranching mode m,
    Mergeable s,
    Monoid w,
    Mergeable w
  ) =>
  UnifiedBranching mode (RWSStrict.RWST r w s m)
  where
  withBaseBranching r =
    withMode @mode
      (withBaseBranching @mode @m r)
      (withBaseBranching @mode @m r)
  {-# INLINE withBaseBranching #-}
