{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}

module Grisette.Unified.Internal.FPFPConversion
  ( UnifiedFPFPConversion,
    AllUnifiedFPFPConversion,
  )
where

import Grisette.Internal.Core.Data.Class.IEEEFP (IEEEFPConvertible)
import Grisette.Internal.SymPrim.FP (FP, FPRoundingMode, ValidFP)
import Grisette.Internal.SymPrim.SymFP (SymFP, SymFPRoundingMode)
import Grisette.Unified.Internal.EvalModeTag (EvalModeTag (Con, Sym))
import Grisette.Unified.Internal.UnifiedFP (UnifiedFPImpl (GetFP, GetFPRoundingMode))

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

instance
  (ValidFP eb0 sb0, ValidFP eb1 sb1) =>
  UnifiedFPFPConversionImpl
    'Con
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
    'Sym
    SymFP
    eb0
    sb0
    eb1
    sb1
    (SymFP eb0 sb0)
    (SymFP eb1 sb1)
    SymFPRoundingMode

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
