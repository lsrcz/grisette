{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Unified.Internal.MonadWithMode
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Unified.Internal.MonadWithMode (MonadWithMode) where

import Grisette.Internal.Core.Data.Class.TryMerge (TryMerge)
import Grisette.Unified.Internal.Class.UnifiedSimpleMergeable (UnifiedBranching)
import Grisette.Unified.Internal.EvalMode (EvalMode)

-- | A constraint that specifies that the mode is valid, and provide all the
-- corresponding constraints for the operations for the types.
--
-- This also provide the branching constraints for the monad, and the safe
-- operations: for example, 'Grisette.Unified.SafeUnifiedInteger' provides
-- 'Grisette.safeDiv' for the integer type with in @ExceptT ArithException m@.
--
-- For users with GHC prior to 9.2.1, see notes in 'EvalMode'.
type MonadWithMode mode m =
  ( EvalMode mode,
    Monad m,
    TryMerge m,
    UnifiedBranching mode m
  )
