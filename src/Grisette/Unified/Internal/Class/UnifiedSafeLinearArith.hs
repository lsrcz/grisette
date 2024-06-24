{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Grisette.Unified.Internal.Class.UnifiedSafeLinearArith
  ( safeAdd,
    safeNeg,
    safeSub,
    UnifiedSafeLinearArith (..),
  )
where

import Control.Monad.Error.Class (MonadError)
import Data.Typeable (Typeable)
import GHC.TypeLits (KnownNat, type (<=))
import Grisette.Internal.Core.Data.Class.SafeLinearArith
  ( ArithException,
    SafeLinearArith,
  )
import qualified Grisette.Internal.Core.Data.Class.SafeLinearArith
import Grisette.Internal.SymPrim.BV (BitwidthMismatch, IntN, WordN)
import Grisette.Internal.SymPrim.SomeBV
  ( SomeIntN,
    SomeSymIntN,
    SomeSymWordN,
    SomeWordN,
  )
import Grisette.Internal.SymPrim.SymBV (SymIntN, SymWordN)
import Grisette.Internal.SymPrim.SymInteger (SymInteger)
import Grisette.Unified.Internal.Class.UnifiedSimpleMergeable
  ( UnifiedBranching (withBaseBranching),
  )
import Grisette.Unified.Internal.EvaluationMode
  ( EvaluationMode (Sym),
  )
import Grisette.Unified.Internal.Util (withMode)

safeAdd ::
  forall mode e a m.
  ( MonadError e m,
    UnifiedSafeLinearArith mode e a m
  ) =>
  a ->
  a ->
  m a
safeAdd a b =
  withBaseSafeLinearArith @mode @e @a @m $
    Grisette.Internal.Core.Data.Class.SafeLinearArith.safeAdd a b
{-# INLINE safeAdd #-}

safeNeg ::
  forall mode e a m.
  ( MonadError e m,
    UnifiedSafeLinearArith mode e a m
  ) =>
  a ->
  m a
safeNeg a =
  withBaseSafeLinearArith @mode @e @a @m $
    Grisette.Internal.Core.Data.Class.SafeLinearArith.safeNeg a
{-# INLINE safeNeg #-}

safeSub ::
  forall mode e a m.
  ( MonadError e m,
    UnifiedSafeLinearArith mode e a m
  ) =>
  a ->
  a ->
  m a
safeSub a b =
  withBaseSafeLinearArith @mode @e @a @m $
    Grisette.Internal.Core.Data.Class.SafeLinearArith.safeSub a b
{-# INLINE safeSub #-}

class UnifiedSafeLinearArith (mode :: EvaluationMode) e a m where
  withBaseSafeLinearArith :: ((SafeLinearArith e a m) => r) -> r

instance
  {-# INCOHERENT #-}
  (UnifiedBranching mode m, SafeLinearArith e a m) =>
  UnifiedSafeLinearArith mode e a m
  where
  withBaseSafeLinearArith r = r

instance
  (Typeable mode, MonadError ArithException m, UnifiedBranching mode m) =>
  UnifiedSafeLinearArith mode ArithException Integer m
  where
  withBaseSafeLinearArith r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  (MonadError ArithException m, UnifiedBranching 'Sym m) =>
  UnifiedSafeLinearArith 'Sym ArithException SymInteger m
  where
  withBaseSafeLinearArith r = withBaseBranching @'Sym @m r

instance
  (MonadError ArithException m, UnifiedBranching mode m, KnownNat n, 1 <= n) =>
  UnifiedSafeLinearArith mode ArithException (IntN n) m
  where
  withBaseSafeLinearArith r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  (MonadError ArithException m, UnifiedBranching 'Sym m, KnownNat n, 1 <= n) =>
  UnifiedSafeLinearArith 'Sym ArithException (SymIntN n) m
  where
  withBaseSafeLinearArith r = withBaseBranching @'Sym @m r

instance
  (MonadError ArithException m, UnifiedBranching mode m, KnownNat n, 1 <= n) =>
  UnifiedSafeLinearArith mode ArithException (WordN n) m
  where
  withBaseSafeLinearArith r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  (MonadError ArithException m, UnifiedBranching 'Sym m, KnownNat n, 1 <= n) =>
  UnifiedSafeLinearArith 'Sym ArithException (SymWordN n) m
  where
  withBaseSafeLinearArith r = withBaseBranching @'Sym @m r

instance
  ( MonadError (Either BitwidthMismatch ArithException) m,
    UnifiedBranching mode m
  ) =>
  UnifiedSafeLinearArith
    mode
    (Either BitwidthMismatch ArithException)
    SomeIntN
    m
  where
  withBaseSafeLinearArith r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  ( MonadError (Either BitwidthMismatch ArithException) m,
    UnifiedBranching 'Sym m
  ) =>
  UnifiedSafeLinearArith
    'Sym
    (Either BitwidthMismatch ArithException)
    SomeSymIntN
    m
  where
  withBaseSafeLinearArith r = withBaseBranching @'Sym @m r

instance
  ( MonadError (Either BitwidthMismatch ArithException) m,
    UnifiedBranching mode m
  ) =>
  UnifiedSafeLinearArith
    mode
    (Either BitwidthMismatch ArithException)
    SomeWordN
    m
  where
  withBaseSafeLinearArith r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  ( MonadError (Either BitwidthMismatch ArithException) m,
    UnifiedBranching 'Sym m
  ) =>
  UnifiedSafeLinearArith
    'Sym
    (Either BitwidthMismatch ArithException)
    SomeSymWordN
    m
  where
  withBaseSafeLinearArith r = withBaseBranching @'Sym @m r
