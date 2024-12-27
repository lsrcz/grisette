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
-- Module      :   Grisette.Internal.Unified.Class.UnifiedSafeDiv
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Unified.Class.UnifiedSafeDiv
  ( safeDiv,
    safeMod,
    safeDivMod,
    safeQuot,
    safeRem,
    safeQuotRem,
    UnifiedSafeDiv (..),
  )
where

import Control.Monad.Error.Class (MonadError)
import GHC.TypeLits (KnownNat, type (<=))
import Grisette.Internal.Core.Data.Class.SafeDiv
  ( ArithException,
    SafeDiv,
  )
import qualified Grisette.Internal.Core.Data.Class.SafeDiv
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.SomeBV
  ( SomeBVException,
    SomeIntN,
    SomeSymIntN,
    SomeSymWordN,
    SomeWordN,
  )
import Grisette.Internal.SymPrim.SymBV (SymIntN, SymWordN)
import Grisette.Internal.SymPrim.SymInteger (SymInteger)
import Grisette.Internal.Unified.Class.UnifiedSimpleMergeable
  ( UnifiedBranching (withBaseBranching),
  )
import Grisette.Internal.Unified.EvalModeTag
  ( EvalModeTag (S),
  )
import Grisette.Internal.Unified.Util (withMode)

-- | Unified `Grisette.Internal.Core.Data.Class.SafeDiv.safeDiv` operation.
--
-- This function isn't able to infer the mode, so you need to provide the mode
-- explicitly. For example:
--
-- > safeDiv @mode a b
safeDiv ::
  forall mode e a m.
  (MonadError e m, UnifiedSafeDiv mode e a m) =>
  a ->
  a ->
  m a
safeDiv a b =
  withBaseSafeDiv @mode @e @a @m $
    Grisette.Internal.Core.Data.Class.SafeDiv.safeDiv a b
{-# INLINE safeDiv #-}

-- | Unified `Grisette.Internal.Core.Data.Class.SafeDiv.safeMod` operation.
--
-- This function isn't able to infer the mode, so you need to provide the mode
-- explicitly. For example:
--
-- > safeMod @mode a b
safeMod ::
  forall mode e a m.
  (MonadError e m, UnifiedSafeDiv mode e a m) =>
  a ->
  a ->
  m a
safeMod a b =
  withBaseSafeDiv @mode @e @a @m $
    Grisette.Internal.Core.Data.Class.SafeDiv.safeMod a b
{-# INLINE safeMod #-}

-- | Unified `Grisette.Internal.Core.Data.Class.SafeDiv.safeDivMod`
-- operation.
--
-- This function isn't able to infer the mode, so you need to provide the mode
-- explicitly. For example:
--
-- > safeDivMod @mode a b
safeDivMod ::
  forall mode e a m.
  (MonadError e m, UnifiedSafeDiv mode e a m) =>
  a ->
  a ->
  m (a, a)
safeDivMod a b =
  withBaseSafeDiv @mode @e @a @m $
    Grisette.Internal.Core.Data.Class.SafeDiv.safeDivMod a b
{-# INLINE safeDivMod #-}

-- | Unified `Grisette.Internal.Core.Data.Class.SafeDiv.safeQuot`
-- operation.
--
-- This function isn't able to infer the mode, so you need to provide the mode
-- explicitly. For example:
--
-- > safeQuot @mode a b
safeQuot ::
  forall mode e a m.
  (MonadError e m, UnifiedSafeDiv mode e a m) =>
  a ->
  a ->
  m a
safeQuot a b =
  withBaseSafeDiv @mode @e @a @m $
    Grisette.Internal.Core.Data.Class.SafeDiv.safeQuot a b
{-# INLINE safeQuot #-}

-- | Unified `Grisette.Internal.Core.Data.Class.SafeDiv.safeRem` operation.
--
-- This function isn't able to infer the mode, so you need to provide the mode
-- explicitly. For example:
--
-- > safeRem @mode a b
safeRem ::
  forall mode e a m.
  (MonadError e m, UnifiedSafeDiv mode e a m) =>
  a ->
  a ->
  m a
safeRem a b =
  withBaseSafeDiv @mode @e @a @m $
    Grisette.Internal.Core.Data.Class.SafeDiv.safeRem a b
{-# INLINE safeRem #-}

-- | Unified `Grisette.Internal.Core.Data.Class.SafeDiv.safeQuotRem`
-- operation.
--
-- This function isn't able to infer the mode, so you need to provide the mode
-- explicitly. For example:
--
-- > safeQuotRem @mode a b
safeQuotRem ::
  forall mode e a m.
  (MonadError e m, UnifiedSafeDiv mode e a m) =>
  a ->
  a ->
  m (a, a)
safeQuotRem a b =
  withBaseSafeDiv @mode @e @a @m $
    Grisette.Internal.Core.Data.Class.SafeDiv.safeQuotRem a b
{-# INLINE safeQuotRem #-}

-- | A class that provides unified division operations.
--
-- We use this type class to help resolve the constraints for `SafeDiv`.
class UnifiedSafeDiv (mode :: EvalModeTag) e a m where
  withBaseSafeDiv :: ((SafeDiv e a m) => r) -> r

instance
  {-# INCOHERENT #-}
  (UnifiedBranching mode m, SafeDiv e a m) =>
  UnifiedSafeDiv mode e a m
  where
  withBaseSafeDiv r = r

instance
  (MonadError ArithException m, UnifiedBranching mode m) =>
  UnifiedSafeDiv mode ArithException Integer m
  where
  withBaseSafeDiv r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  (MonadError ArithException m, UnifiedBranching 'S m) =>
  UnifiedSafeDiv 'S ArithException SymInteger m
  where
  withBaseSafeDiv r = withBaseBranching @'S @m r

instance
  (MonadError ArithException m, UnifiedBranching mode m, KnownNat n, 1 <= n) =>
  UnifiedSafeDiv mode ArithException (IntN n) m
  where
  withBaseSafeDiv r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  (MonadError ArithException m, UnifiedBranching 'S m, KnownNat n, 1 <= n) =>
  UnifiedSafeDiv 'S ArithException (SymIntN n) m
  where
  withBaseSafeDiv r = withBaseBranching @'S @m r

instance
  (MonadError ArithException m, UnifiedBranching mode m, KnownNat n, 1 <= n) =>
  UnifiedSafeDiv mode ArithException (WordN n) m
  where
  withBaseSafeDiv r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  (MonadError ArithException m, UnifiedBranching 'S m, KnownNat n, 1 <= n) =>
  UnifiedSafeDiv 'S ArithException (SymWordN n) m
  where
  withBaseSafeDiv r = withBaseBranching @'S @m r

instance
  ( MonadError (Either SomeBVException ArithException) m,
    UnifiedBranching mode m
  ) =>
  UnifiedSafeDiv
    mode
    (Either SomeBVException ArithException)
    SomeIntN
    m
  where
  withBaseSafeDiv r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  ( MonadError (Either SomeBVException ArithException) m,
    UnifiedBranching 'S m
  ) =>
  UnifiedSafeDiv
    'S
    (Either SomeBVException ArithException)
    SomeSymIntN
    m
  where
  withBaseSafeDiv r = withBaseBranching @'S @m r

instance
  ( MonadError (Either SomeBVException ArithException) m,
    UnifiedBranching mode m
  ) =>
  UnifiedSafeDiv
    mode
    (Either SomeBVException ArithException)
    SomeWordN
    m
  where
  withBaseSafeDiv r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  ( MonadError (Either SomeBVException ArithException) m,
    UnifiedBranching 'S m
  ) =>
  UnifiedSafeDiv
    'S
    (Either SomeBVException ArithException)
    SomeSymWordN
    m
  where
  withBaseSafeDiv r = withBaseBranching @'S @m r
