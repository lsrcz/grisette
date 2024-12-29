{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Internal.Impl.Unified.FPFPConversion
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Impl.Unified.FPFPConversion () where

import Grisette.Internal.Internal.Decl.Unified.FPFPConversion
  ( AllUnifiedFPFPConversion,
    UnifiedFPFPConversion,
    UnifiedFPFPConversionImpl,
  )
import Grisette.Internal.Internal.Decl.Unified.UnifiedFP
  ( UnifiedFPImpl (GetFP, GetFPRoundingMode),
  )
import Grisette.Internal.Internal.Impl.Unified.UnifiedFP ()
import Grisette.Internal.SymPrim.FP (FP, FPRoundingMode, ValidFP)
import Grisette.Internal.SymPrim.SymFP (SymFP, SymFPRoundingMode)
import Grisette.Internal.Unified.EvalModeTag (EvalModeTag (C, S))

instance
  (ValidFP eb0 sb0, ValidFP eb1 sb1) =>
  UnifiedFPFPConversionImpl
    'C
    FP
    eb0
    sb0
    eb1
    sb1
    (FP eb0 sb0)
    (FP eb1 sb1)
    FPRoundingMode

instance
  (ValidFP eb0 sb0, ValidFP eb1 sb1) =>
  UnifiedFPFPConversionImpl
    'S
    SymFP
    eb0
    sb0
    eb1
    sb1
    (SymFP eb0 sb0)
    (SymFP eb1 sb1)
    SymFPRoundingMode

instance
  ( UnifiedFPFPConversionImpl
      (mode :: EvalModeTag)
      (GetFP mode)
      eb0
      sb0
      eb1
      sb1
      (GetFP mode eb0 sb0)
      (GetFP mode eb1 sb1)
      (GetFPRoundingMode mode)
  ) =>
  UnifiedFPFPConversion mode eb0 sb0 eb1 sb1

instance
  ( forall eb0 sb0 eb1 sb1.
    (ValidFP eb0 sb0, ValidFP eb1 sb1) =>
    UnifiedFPFPConversion
      mode
      eb0
      sb0
      eb1
      sb1
  ) =>
  AllUnifiedFPFPConversion mode
