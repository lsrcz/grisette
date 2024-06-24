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

module Grisette.Unified.Internal.Class.UnifiedSafeSymShift
  ( safeSymShiftL,
    safeSymShiftR,
    safeSymStrictShiftL,
    safeSymStrictShiftR,
    UnifiedSafeSymShift (..),
  )
where

import Control.Monad.Error.Class (MonadError)
import GHC.TypeLits (KnownNat, type (<=))
import Grisette.Internal.Core.Data.Class.SafeDivision
  ( ArithException,
  )
import Grisette.Internal.Core.Data.Class.SafeSymShift (SafeSymShift)
import qualified Grisette.Internal.Core.Data.Class.SafeSymShift
import Grisette.Internal.SymPrim.BV (BitwidthMismatch, IntN, WordN)
import Grisette.Internal.SymPrim.SomeBV (SomeIntN, SomeSymIntN, SomeSymWordN, SomeWordN)
import Grisette.Internal.SymPrim.SymBV (SymIntN, SymWordN)
import Grisette.Unified.Internal.Class.UnifiedSimpleMergeable
  ( UnifiedBranching (withBaseBranching),
  )
import Grisette.Unified.Internal.EvaluationMode
  ( EvaluationMode (Sym),
  )
import Grisette.Unified.Internal.Util (withMode)

safeSymShiftL ::
  forall mode e a m. (UnifiedSafeSymShift mode e a m) => a -> a -> m a
safeSymShiftL a b =
  withBaseSafeSymShift @mode @e @a @m $
    Grisette.Internal.Core.Data.Class.SafeSymShift.safeSymShiftL a b
{-# INLINE safeSymShiftL #-}

safeSymShiftR ::
  forall mode e a m. (UnifiedSafeSymShift mode e a m) => a -> a -> m a
safeSymShiftR a b =
  withBaseSafeSymShift @mode @e @a @m $
    Grisette.Internal.Core.Data.Class.SafeSymShift.safeSymShiftR a b
{-# INLINE safeSymShiftR #-}

safeSymStrictShiftL ::
  forall mode e a m. (UnifiedSafeSymShift mode e a m) => a -> a -> m a
safeSymStrictShiftL a b =
  withBaseSafeSymShift @mode @e @a @m $
    Grisette.Internal.Core.Data.Class.SafeSymShift.safeSymStrictShiftL a b
{-# INLINE safeSymStrictShiftL #-}

safeSymStrictShiftR ::
  forall mode e a m. (UnifiedSafeSymShift mode e a m) => a -> a -> m a
safeSymStrictShiftR a b =
  withBaseSafeSymShift @mode @e @a @m $
    Grisette.Internal.Core.Data.Class.SafeSymShift.safeSymStrictShiftR a b
{-# INLINE safeSymStrictShiftR #-}

class UnifiedSafeSymShift (mode :: EvaluationMode) e a m where
  withBaseSafeSymShift :: ((SafeSymShift e a m) => r) -> r

instance
  {-# INCOHERENT #-}
  (UnifiedBranching mode m, SafeSymShift e a m) =>
  UnifiedSafeSymShift mode e a m
  where
  withBaseSafeSymShift r = r

instance
  (MonadError ArithException m, UnifiedBranching mode m, KnownNat n, 1 <= n) =>
  UnifiedSafeSymShift mode ArithException (IntN n) m
  where
  withBaseSafeSymShift r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  (MonadError ArithException m, UnifiedBranching 'Sym m, KnownNat n, 1 <= n) =>
  UnifiedSafeSymShift 'Sym ArithException (SymIntN n) m
  where
  withBaseSafeSymShift r = withBaseBranching @'Sym @m r

instance
  (MonadError ArithException m, UnifiedBranching mode m, KnownNat n, 1 <= n) =>
  UnifiedSafeSymShift mode ArithException (WordN n) m
  where
  withBaseSafeSymShift r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  (MonadError ArithException m, UnifiedBranching 'Sym m, KnownNat n, 1 <= n) =>
  UnifiedSafeSymShift 'Sym ArithException (SymWordN n) m
  where
  withBaseSafeSymShift r = withBaseBranching @'Sym @m r

instance
  ( MonadError (Either BitwidthMismatch ArithException) m,
    UnifiedBranching mode m
  ) =>
  UnifiedSafeSymShift
    mode
    (Either BitwidthMismatch ArithException)
    SomeIntN
    m
  where
  withBaseSafeSymShift r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  ( MonadError (Either BitwidthMismatch ArithException) m,
    UnifiedBranching 'Sym m
  ) =>
  UnifiedSafeSymShift
    'Sym
    (Either BitwidthMismatch ArithException)
    SomeSymIntN
    m
  where
  withBaseSafeSymShift r = withBaseBranching @'Sym @m r

instance
  ( MonadError (Either BitwidthMismatch ArithException) m,
    UnifiedBranching mode m
  ) =>
  UnifiedSafeSymShift
    mode
    (Either BitwidthMismatch ArithException)
    SomeWordN
    m
  where
  withBaseSafeSymShift r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  ( MonadError (Either BitwidthMismatch ArithException) m,
    UnifiedBranching 'Sym m
  ) =>
  UnifiedSafeSymShift
    'Sym
    (Either BitwidthMismatch ArithException)
    SomeSymWordN
    m
  where
  withBaseSafeSymShift r = withBaseBranching @'Sym @m r
