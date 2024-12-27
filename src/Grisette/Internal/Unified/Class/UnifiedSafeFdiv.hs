{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

-- |
-- Module      :   Grisette.Internal.Unified.Class.UnifiedSafeFdiv
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Unified.Class.UnifiedSafeFdiv
  ( safeFdiv,
    UnifiedSafeFdiv (..),
  )
where

import Control.Exception (ArithException)
import Control.Monad.Error.Class (MonadError)
import Grisette.Internal.Core.Data.Class.SafeFdiv (SafeFdiv)
import qualified Grisette.Internal.Core.Data.Class.SafeFdiv
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.SymAlgReal (SymAlgReal)
import Grisette.Internal.Unified.Class.UnifiedSimpleMergeable
  ( UnifiedBranching (withBaseBranching),
  )
import Grisette.Internal.Unified.EvalModeTag (EvalModeTag (S))
import Grisette.Internal.Unified.Util (withMode)

-- | Unified `Grisette.Internal.Core.Data.Class.SafeFdiv.safeFdiv` operation.
--
-- This function isn't able to infer the mode, so you need to provide the mode
-- explicitly. For example:
--
-- > safeFdiv @mode a b
safeFdiv ::
  forall mode e a m.
  (MonadError e m, UnifiedSafeFdiv mode e a m) =>
  a ->
  a ->
  m a
safeFdiv a b =
  withBaseUnifiedSafeFdiv @mode @e @a @m $
    Grisette.Internal.Core.Data.Class.SafeFdiv.safeFdiv a b
{-# INLINE safeFdiv #-}

-- | A class that provides unified floating division operations.
--
-- We use this type class to help resolve the constraints for `SafeFdiv`.
class UnifiedSafeFdiv (mode :: EvalModeTag) e a m where
  withBaseUnifiedSafeFdiv :: ((SafeFdiv e a m) => r) -> r

instance
  {-# INCOHERENT #-}
  (UnifiedBranching mode m, SafeFdiv e a m) =>
  UnifiedSafeFdiv mode e a m
  where
  withBaseUnifiedSafeFdiv r = r

instance
  (MonadError ArithException m, UnifiedBranching mode m) =>
  UnifiedSafeFdiv mode ArithException AlgReal m
  where
  withBaseUnifiedSafeFdiv r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  (MonadError ArithException m, UnifiedBranching 'S m) =>
  UnifiedSafeFdiv 'S ArithException SymAlgReal m
  where
  withBaseUnifiedSafeFdiv r = withBaseBranching @'S @m r
