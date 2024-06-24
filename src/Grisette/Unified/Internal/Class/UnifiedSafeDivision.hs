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

module Grisette.Unified.Internal.Class.UnifiedSafeDivision
  ( safeDiv,
    safeMod,
    safeDivMod,
    safeQuot,
    safeRem,
    safeQuotRem,
    UnifiedSafeDivision (..),
  )
where

import Control.Monad.Error.Class (MonadError)
import GHC.TypeLits (KnownNat, type (<=))
import Grisette.Internal.Core.Data.Class.SafeDivision
  ( ArithException,
    SafeDivision,
  )
import qualified Grisette.Internal.Core.Data.Class.SafeDivision
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

safeDiv :: forall mode e a m. (UnifiedSafeDivision mode e a m) => a -> a -> m a
safeDiv a b =
  withBaseSafeDivision @mode @e @a @m $
    Grisette.Internal.Core.Data.Class.SafeDivision.safeDiv a b
{-# INLINE safeDiv #-}

safeMod :: forall mode e a m. (UnifiedSafeDivision mode e a m) => a -> a -> m a
safeMod a b =
  withBaseSafeDivision @mode @e @a @m $
    Grisette.Internal.Core.Data.Class.SafeDivision.safeMod a b
{-# INLINE safeMod #-}

safeDivMod ::
  forall mode e a m. (UnifiedSafeDivision mode e a m) => a -> a -> m (a, a)
safeDivMod a b =
  withBaseSafeDivision @mode @e @a @m $
    Grisette.Internal.Core.Data.Class.SafeDivision.safeDivMod a b
{-# INLINE safeDivMod #-}

safeQuot :: forall mode e a m. (UnifiedSafeDivision mode e a m) => a -> a -> m a
safeQuot a b =
  withBaseSafeDivision @mode @e @a @m $
    Grisette.Internal.Core.Data.Class.SafeDivision.safeQuot a b
{-# INLINE safeQuot #-}

safeRem :: forall mode e a m. (UnifiedSafeDivision mode e a m) => a -> a -> m a
safeRem a b =
  withBaseSafeDivision @mode @e @a @m $
    Grisette.Internal.Core.Data.Class.SafeDivision.safeRem a b
{-# INLINE safeRem #-}

safeQuotRem ::
  forall mode e a m. (UnifiedSafeDivision mode e a m) => a -> a -> m (a, a)
safeQuotRem a b =
  withBaseSafeDivision @mode @e @a @m $
    Grisette.Internal.Core.Data.Class.SafeDivision.safeQuotRem a b
{-# INLINE safeQuotRem #-}

class UnifiedSafeDivision (mode :: EvaluationMode) e a m where
  withBaseSafeDivision :: ((SafeDivision e a m) => r) -> r

instance
  {-# INCOHERENT #-}
  (UnifiedBranching mode m, SafeDivision e a m) =>
  UnifiedSafeDivision mode e a m
  where
  withBaseSafeDivision r = r

instance
  (MonadError ArithException m, UnifiedBranching mode m) =>
  UnifiedSafeDivision mode ArithException Integer m
  where
  withBaseSafeDivision r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  (MonadError ArithException m, UnifiedBranching 'Sym m) =>
  UnifiedSafeDivision 'Sym ArithException SymInteger m
  where
  withBaseSafeDivision r = withBaseBranching @'Sym @m r

instance
  (MonadError ArithException m, UnifiedBranching mode m, KnownNat n, 1 <= n) =>
  UnifiedSafeDivision mode ArithException (IntN n) m
  where
  withBaseSafeDivision r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  (MonadError ArithException m, UnifiedBranching 'Sym m, KnownNat n, 1 <= n) =>
  UnifiedSafeDivision 'Sym ArithException (SymIntN n) m
  where
  withBaseSafeDivision r = withBaseBranching @'Sym @m r

instance
  (MonadError ArithException m, UnifiedBranching mode m, KnownNat n, 1 <= n) =>
  UnifiedSafeDivision mode ArithException (WordN n) m
  where
  withBaseSafeDivision r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  (MonadError ArithException m, UnifiedBranching 'Sym m, KnownNat n, 1 <= n) =>
  UnifiedSafeDivision 'Sym ArithException (SymWordN n) m
  where
  withBaseSafeDivision r = withBaseBranching @'Sym @m r

instance
  ( MonadError (Either BitwidthMismatch ArithException) m,
    UnifiedBranching mode m
  ) =>
  UnifiedSafeDivision
    mode
    (Either BitwidthMismatch ArithException)
    SomeIntN
    m
  where
  withBaseSafeDivision r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  ( MonadError (Either BitwidthMismatch ArithException) m,
    UnifiedBranching 'Sym m
  ) =>
  UnifiedSafeDivision
    'Sym
    (Either BitwidthMismatch ArithException)
    SomeSymIntN
    m
  where
  withBaseSafeDivision r = withBaseBranching @'Sym @m r

instance
  ( MonadError (Either BitwidthMismatch ArithException) m,
    UnifiedBranching mode m
  ) =>
  UnifiedSafeDivision
    mode
    (Either BitwidthMismatch ArithException)
    SomeWordN
    m
  where
  withBaseSafeDivision r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  ( MonadError (Either BitwidthMismatch ArithException) m,
    UnifiedBranching 'Sym m
  ) =>
  UnifiedSafeDivision
    'Sym
    (Either BitwidthMismatch ArithException)
    SomeSymWordN
    m
  where
  withBaseSafeDivision r = withBaseBranching @'Sym @m r
