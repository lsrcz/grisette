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
-- Module      :   Grisette.Internal.Internal.Decl.Unified.FPFPConversion
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Decl.Unified.FPFPConversion
  ( UnifiedFPFPConversionImpl,
    UnifiedFPFPConversion,
    AllUnifiedFPFPConversion,
  )
where

import Grisette.Internal.Core.Data.Class.IEEEFP (IEEEFPConvertible)
import Grisette.Internal.Internal.Decl.Unified.UnifiedFP
  ( UnifiedFPImpl (GetFP, GetFPRoundingMode),
  )
import Grisette.Internal.SymPrim.FP (ValidFP)
import Grisette.Internal.Unified.EvalModeTag (EvalModeTag)

-- | Implementation for 'UnifiedFPFPConversion'.
class
  ( UnifiedFPImpl mode fpn eb0 sb0 fp0 fprd,
    UnifiedFPImpl mode fpn eb1 sb1 fp1 fprd,
    IEEEFPConvertible fp0 fp1 fprd
  ) =>
  UnifiedFPFPConversionImpl
    (mode :: EvalModeTag)
    fpn
    eb0
    sb0
    eb1
    sb1
    fp0
    fp1
    fprd

-- | Unified constraints for conversion from floating point numbers to floating
-- point numbers.
class
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

-- | Evaluation mode with unified conversion from floating-points to
-- floating-points.
class
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
