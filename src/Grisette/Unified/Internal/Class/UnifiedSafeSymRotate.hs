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
-- Module      :   Grisette.Unified.Internal.Class.UnifiedSafeSymRotate
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Unified.Internal.Class.UnifiedSafeSymRotate
  ( safeSymRotateL,
    safeSymRotateR,
    UnifiedSafeSymRotate (..),
  )
where

import Control.Exception (ArithException)
import Control.Monad.Error.Class (MonadError)
import GHC.TypeLits (KnownNat, type (<=))
import Grisette.Internal.Core.Data.Class.SafeSymRotate (SafeSymRotate)
import qualified Grisette.Internal.Core.Data.Class.SafeSymRotate
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

-- | Unified `Grisette.Internal.Core.Data.Class.SafeSymRotate.safeSymRotateL`
-- operation.
--
-- This function isn't able to infer the mode, so you need to provide the mode
-- explicitly. For example:
--
-- > safeSymRotateL @mode a b
safeSymRotateL ::
  forall mode e a m.
  ( MonadError e m,
    UnifiedSafeSymRotate mode e a m
  ) =>
  a ->
  a ->
  m a
safeSymRotateL a b =
  withBaseSafeSymRotate @mode @e @a @m $
    Grisette.Internal.Core.Data.Class.SafeSymRotate.safeSymRotateL a b
{-# INLINE safeSymRotateL #-}

-- | Unified `Grisette.Internal.Core.Data.Class.SafeSymRotate.safeSymRotateR`
-- operation.
--
-- This function isn't able to infer the mode, so you need to provide the mode
-- explicitly. For example:
--
-- > safeSymRotateR @mode a b
safeSymRotateR ::
  forall mode e a m.
  ( MonadError e m,
    UnifiedSafeSymRotate mode e a m
  ) =>
  a ->
  a ->
  m a
safeSymRotateR a b =
  withBaseSafeSymRotate @mode @e @a @m $
    Grisette.Internal.Core.Data.Class.SafeSymRotate.safeSymRotateR a b
{-# INLINE safeSymRotateR #-}

-- | A class that provides unified safe symbolic rotation operations.
--
-- We use this type class to help resolve the constraints for `SafeSymRotate`.
class UnifiedSafeSymRotate (mode :: EvalModeTag) e a m where
  withBaseSafeSymRotate :: ((SafeSymRotate e a m) => r) -> r

instance
  {-# INCOHERENT #-}
  (UnifiedBranching mode m, SafeSymRotate e a m) =>
  UnifiedSafeSymRotate mode e a m
  where
  withBaseSafeSymRotate r = r

instance
  (MonadError ArithException m, UnifiedBranching mode m, KnownNat n, 1 <= n) =>
  UnifiedSafeSymRotate mode ArithException (IntN n) m
  where
  withBaseSafeSymRotate r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  (MonadError ArithException m, UnifiedBranching 'S m, KnownNat n, 1 <= n) =>
  UnifiedSafeSymRotate 'S ArithException (SymIntN n) m
  where
  withBaseSafeSymRotate r = withBaseBranching @'S @m r

instance
  (MonadError ArithException m, UnifiedBranching mode m, KnownNat n, 1 <= n) =>
  UnifiedSafeSymRotate mode ArithException (WordN n) m
  where
  withBaseSafeSymRotate r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  (MonadError ArithException m, UnifiedBranching 'S m, KnownNat n, 1 <= n) =>
  UnifiedSafeSymRotate 'S ArithException (SymWordN n) m
  where
  withBaseSafeSymRotate r = withBaseBranching @'S @m r

instance
  ( MonadError (Either SomeBVException ArithException) m,
    UnifiedBranching mode m
  ) =>
  UnifiedSafeSymRotate
    mode
    (Either SomeBVException ArithException)
    SomeIntN
    m
  where
  withBaseSafeSymRotate r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  ( MonadError (Either SomeBVException ArithException) m,
    UnifiedBranching 'S m
  ) =>
  UnifiedSafeSymRotate
    'S
    (Either SomeBVException ArithException)
    SomeSymIntN
    m
  where
  withBaseSafeSymRotate r = withBaseBranching @'S @m r

instance
  ( MonadError (Either SomeBVException ArithException) m,
    UnifiedBranching mode m
  ) =>
  UnifiedSafeSymRotate
    mode
    (Either SomeBVException ArithException)
    SomeWordN
    m
  where
  withBaseSafeSymRotate r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  ( MonadError (Either SomeBVException ArithException) m,
    UnifiedBranching 'S m
  ) =>
  UnifiedSafeSymRotate
    'S
    (Either SomeBVException ArithException)
    SomeSymWordN
    m
  where
  withBaseSafeSymRotate r = withBaseBranching @'S @m r
