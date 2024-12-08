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

-- |
-- Module      :   Grisette.Unified.Internal.Class.UnifiedSafeSymShift
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Unified.Internal.Class.UnifiedSafeSymShift
  ( safeSymShiftL,
    safeSymShiftR,
    safeSymStrictShiftL,
    safeSymStrictShiftR,
    UnifiedSafeSymShift (..),
  )
where

import Control.Exception (ArithException)
import Control.Monad.Error.Class (MonadError)
import GHC.TypeLits (KnownNat, type (<=))
import Grisette.Internal.Core.Data.Class.SafeSymShift (SafeSymShift)
import qualified Grisette.Internal.Core.Data.Class.SafeSymShift
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.SomeBV
  ( SomeBVException,
    SomeIntN,
    SomeSymIntN,
    SomeSymWordN,
    SomeWordN,
  )
import Grisette.Internal.SymPrim.SymBV (SymIntN, SymWordN)
import Grisette.Unified.Internal.Class.UnifiedSimpleMergeable
  ( UnifiedBranching (withBaseBranching),
  )
import Grisette.Unified.Internal.EvalModeTag
  ( EvalModeTag (S),
  )
import Grisette.Unified.Internal.Util (withMode)

-- | Unified `Grisette.Internal.Core.Data.Class.SafeSymShift.safeSymShiftL`
-- operation.
--
-- This function isn't able to infer the mode, so you need to provide the mode
-- explicitly. For example:
--
-- > safeSymShiftL @mode a b
safeSymShiftL ::
  forall mode e a m.
  ( MonadError e m,
    UnifiedSafeSymShift mode e a m
  ) =>
  a ->
  a ->
  m a
safeSymShiftL a b =
  withBaseSafeSymShift @mode @e @a @m $
    Grisette.Internal.Core.Data.Class.SafeSymShift.safeSymShiftL a b
{-# INLINE safeSymShiftL #-}

-- | Unified `Grisette.Internal.Core.Data.Class.SafeSymShift.safeSymShiftR`
-- operation.
--
-- This function isn't able to infer the mode, so you need to provide the mode
-- explicitly. For example:
--
-- > safeSymShiftR @mode a b
safeSymShiftR ::
  forall mode e a m.
  ( MonadError e m,
    UnifiedSafeSymShift mode e a m
  ) =>
  a ->
  a ->
  m a
safeSymShiftR a b =
  withBaseSafeSymShift @mode @e @a @m $
    Grisette.Internal.Core.Data.Class.SafeSymShift.safeSymShiftR a b
{-# INLINE safeSymShiftR #-}

-- | Unified
-- `Grisette.Internal.Core.Data.Class.SafeSymShift.safeSymStrictShiftL`
-- operation.
--
-- This function isn't able to infer the mode, so you need to provide the mode
-- explicitly. For example:
--
-- > safeSymStrictShiftL @mode a b
safeSymStrictShiftL ::
  forall mode e a m.
  ( MonadError e m,
    UnifiedSafeSymShift mode e a m
  ) =>
  a ->
  a ->
  m a
safeSymStrictShiftL a b =
  withBaseSafeSymShift @mode @e @a @m $
    Grisette.Internal.Core.Data.Class.SafeSymShift.safeSymStrictShiftL a b
{-# INLINE safeSymStrictShiftL #-}

-- | Unified
-- `Grisette.Internal.Core.Data.Class.SafeSymShift.safeSymStrictShiftR`
-- operation.
--
-- This function isn't able to infer the mode, so you need to provide the mode
-- explicitly. For example:
--
-- > safeSymStrictShiftR @mode a b
safeSymStrictShiftR ::
  forall mode e a m.
  ( MonadError e m,
    UnifiedSafeSymShift mode e a m
  ) =>
  a ->
  a ->
  m a
safeSymStrictShiftR a b =
  withBaseSafeSymShift @mode @e @a @m $
    Grisette.Internal.Core.Data.Class.SafeSymShift.safeSymStrictShiftR a b
{-# INLINE safeSymStrictShiftR #-}

-- | A class that provides unified safe symbolic rotation operations.
--
-- We use this type class to help resolve the constraints for `SafeSymShift`.
class UnifiedSafeSymShift (mode :: EvalModeTag) e a m where
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
  (MonadError ArithException m, UnifiedBranching 'S m, KnownNat n, 1 <= n) =>
  UnifiedSafeSymShift 'S ArithException (SymIntN n) m
  where
  withBaseSafeSymShift r = withBaseBranching @'S @m r

instance
  (MonadError ArithException m, UnifiedBranching mode m, KnownNat n, 1 <= n) =>
  UnifiedSafeSymShift mode ArithException (WordN n) m
  where
  withBaseSafeSymShift r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  (MonadError ArithException m, UnifiedBranching 'S m, KnownNat n, 1 <= n) =>
  UnifiedSafeSymShift 'S ArithException (SymWordN n) m
  where
  withBaseSafeSymShift r = withBaseBranching @'S @m r

instance
  ( MonadError (Either SomeBVException ArithException) m,
    UnifiedBranching mode m
  ) =>
  UnifiedSafeSymShift
    mode
    (Either SomeBVException ArithException)
    SomeIntN
    m
  where
  withBaseSafeSymShift r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  ( MonadError (Either SomeBVException ArithException) m,
    UnifiedBranching 'S m
  ) =>
  UnifiedSafeSymShift
    'S
    (Either SomeBVException ArithException)
    SomeSymIntN
    m
  where
  withBaseSafeSymShift r = withBaseBranching @'S @m r

instance
  ( MonadError (Either SomeBVException ArithException) m,
    UnifiedBranching mode m
  ) =>
  UnifiedSafeSymShift
    mode
    (Either SomeBVException ArithException)
    SomeWordN
    m
  where
  withBaseSafeSymShift r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  ( MonadError (Either SomeBVException ArithException) m,
    UnifiedBranching 'S m
  ) =>
  UnifiedSafeSymShift
    'S
    (Either SomeBVException ArithException)
    SomeSymWordN
    m
  where
  withBaseSafeSymShift r = withBaseBranching @'S @m r
