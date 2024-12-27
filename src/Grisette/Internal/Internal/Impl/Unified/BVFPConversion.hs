{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Internal.Impl.Unified.BVFPConversion
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Impl.Unified.BVFPConversion
  ( UnifiedBVFPConversion,
    SafeUnifiedBVFPConversion,
    AllUnifiedBVFPConversion,
  )
where

import Control.Monad.Error.Class (MonadError)
import GHC.TypeLits (KnownNat, type (+), type (<=))
import Grisette.Internal.Internal.Decl.Unified.BVFPConversion
  ( AllUnifiedBVFPConversion,
    SafeUnifiedBVFPConversion,
    SafeUnifiedBVFPConversionImpl,
    UnifiedBVFPConversion,
    UnifiedBVFPConversionImpl,
  )
import Grisette.Internal.Internal.Decl.Unified.UnifiedBV
  ( UnifiedBVImpl (GetIntN, GetWordN),
  )
import Grisette.Internal.Internal.Decl.Unified.UnifiedFP
  ( UnifiedFPImpl (GetFP, GetFPRoundingMode),
  )
import Grisette.Internal.Internal.Impl.Unified.UnifiedBV ()
import Grisette.Internal.Internal.Impl.Unified.UnifiedFP ()
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP
  ( FP,
    FPRoundingMode,
    NotRepresentableFPError,
    ValidFP,
  )
import Grisette.Internal.SymPrim.SymBV (SymIntN, SymWordN)
import Grisette.Internal.SymPrim.SymFP (SymFP, SymFPRoundingMode)
import Grisette.Internal.Unified.Class.UnifiedSafeBitCast (UnifiedSafeBitCast)
import Grisette.Internal.Unified.Class.UnifiedSafeFromFP (UnifiedSafeFromFP)
import Grisette.Internal.Unified.Class.UnifiedSimpleMergeable (UnifiedBranching)
import Grisette.Internal.Unified.EvalModeTag (EvalModeTag (C, S))

instance
  (ValidFP eb sb, KnownNat n, 1 <= n, n ~ (eb + sb)) =>
  UnifiedBVFPConversionImpl
    'C
    WordN
    IntN
    FP
    n
    eb
    sb
    (WordN n)
    (IntN n)
    (FP eb sb)
    FPRoundingMode

instance
  (ValidFP eb sb, KnownNat n, 1 <= n, n ~ (eb + sb)) =>
  UnifiedBVFPConversionImpl
    'S
    SymWordN
    SymIntN
    SymFP
    n
    eb
    sb
    (SymWordN n)
    (SymIntN n)
    (SymFP eb sb)
    SymFPRoundingMode

instance
  ( UnifiedBVFPConversionImpl mode wordn intn fpn n eb sb word int fp fprd,
    UnifiedSafeBitCast mode NotRepresentableFPError fp int m,
    UnifiedSafeBitCast mode NotRepresentableFPError fp word m,
    UnifiedSafeFromFP mode NotRepresentableFPError word fp fprd m
  ) =>
  SafeUnifiedBVFPConversionImpl mode wordn intn fpn n eb sb word int fp fprd m

instance
  ( SafeUnifiedBVFPConversionImpl
      mode
      (GetWordN mode)
      (GetIntN mode)
      (GetFP mode)
      n
      eb
      sb
      (GetWordN mode n)
      (GetIntN mode n)
      (GetFP mode eb sb)
      (GetFPRoundingMode mode)
      m
  ) =>
  SafeUnifiedBVFPConversion mode n eb sb m

instance
  ( UnifiedBVFPConversionImpl
      (mode :: EvalModeTag)
      (GetWordN mode)
      (GetIntN mode)
      (GetFP mode)
      n
      eb
      sb
      (GetWordN mode n)
      (GetIntN mode n)
      (GetFP mode eb sb)
      (GetFPRoundingMode mode)
  ) =>
  UnifiedBVFPConversion mode n eb sb

instance
  ( forall n eb sb.
    (ValidFP eb sb, KnownNat n, 1 <= n, n ~ (eb + sb)) =>
    UnifiedBVFPConversion mode n eb sb,
    forall n eb sb m.
    ( UnifiedBranching mode m,
      ValidFP eb sb,
      KnownNat n,
      1 <= n,
      n ~ (eb + sb),
      MonadError NotRepresentableFPError m
    ) =>
    SafeUnifiedBVFPConversion mode n eb sb m
  ) =>
  AllUnifiedBVFPConversion mode
