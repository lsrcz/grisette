{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Internal.Impl.Unified.UnifiedFP
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Impl.Unified.UnifiedFP () where

import Control.Monad.Error.Class (MonadError)
import Grisette.Internal.Internal.Decl.Unified.UnifiedFP
  ( AllUnifiedFP,
    GetFP,
    GetFPRoundingMode,
    SafeUnifiedFP,
    SafeUnifiedFPImpl,
    UnifiedFP,
    UnifiedFPImpl,
  )
import Grisette.Internal.SymPrim.FP
  ( FP,
    FPRoundingMode,
    NotRepresentableFPError,
    ValidFP,
  )
import Grisette.Internal.SymPrim.SymFP (SymFP, SymFPRoundingMode)
import Grisette.Internal.Unified.Class.UnifiedSafeFromFP (UnifiedSafeFromFP)
import Grisette.Internal.Unified.Class.UnifiedSimpleMergeable (UnifiedBranching)
import Grisette.Internal.Unified.EvalModeTag (EvalModeTag (C, S))
import Grisette.Internal.Unified.UnifiedInteger (GetInteger)

instance
  (ValidFP eb sb) =>
  UnifiedFPImpl 'C FP eb sb (FP eb sb) FPRoundingMode
  where
  type GetFP 'C = FP
  type GetFPRoundingMode 'C = FPRoundingMode

instance
  (ValidFP eb sb) =>
  UnifiedFPImpl 'S SymFP eb sb (SymFP eb sb) SymFPRoundingMode
  where
  type GetFP 'S = SymFP
  type GetFPRoundingMode 'S = SymFPRoundingMode

instance
  ( UnifiedFPImpl
      mode
      (GetFP mode)
      eb
      sb
      (GetFP mode eb sb)
      (GetFPRoundingMode mode)
  ) =>
  UnifiedFP mode eb sb

instance
  (UnifiedFPImpl mode fpn eb sb fp rd) =>
  SafeUnifiedFPImpl mode fpn eb sb fp rd m

instance
  ( SafeUnifiedFPImpl
      mode
      (GetFP mode)
      eb
      sb
      (GetFP mode eb sb)
      (GetFPRoundingMode mode)
      m,
    UnifiedSafeFromFP
      mode
      NotRepresentableFPError
      (GetInteger mode)
      (GetFP mode eb sb)
      (GetFPRoundingMode mode)
      m
  ) =>
  SafeUnifiedFP mode eb sb m

instance
  ( forall eb sb. (ValidFP eb sb) => UnifiedFP mode eb sb,
    forall eb sb m.
    ( ValidFP eb sb,
      UnifiedBranching mode m,
      MonadError NotRepresentableFPError m
    ) =>
    SafeUnifiedFP mode eb sb m
  ) =>
  AllUnifiedFP mode
