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
-- Module      :   Grisette.Unified.Internal.BVFPConversion
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Unified.Internal.BVFPConversion
  ( UnifiedBVFPConversion,
    SafeUnifiedBVFPConversion,
    AllUnifiedBVFPConversion,
  )
where

import Control.Monad.Error.Class (MonadError)
import GHC.TypeLits (KnownNat, type (+), type (<=))
import Grisette.Internal.Core.Data.Class.BitCast
  ( BitCast,
    BitCastCanonical,
    BitCastOr,
  )
import Grisette.Internal.Core.Data.Class.IEEEFP
  ( IEEEFPConvertible,
  )
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP
  ( FP,
    FPRoundingMode,
    NotRepresentableFPError,
    ValidFP,
  )
import Grisette.Internal.SymPrim.SymBV (SymIntN, SymWordN)
import Grisette.Internal.SymPrim.SymFP (SymFP, SymFPRoundingMode)
import Grisette.Unified.Internal.Class.UnifiedFromIntegral (UnifiedFromIntegral)
import Grisette.Unified.Internal.Class.UnifiedSafeBitCast (UnifiedSafeBitCast)
import Grisette.Unified.Internal.Class.UnifiedSafeFromFP (UnifiedSafeFromFP)
import Grisette.Unified.Internal.Class.UnifiedSimpleMergeable (UnifiedBranching)
import Grisette.Unified.Internal.EvalModeTag (EvalModeTag (C, S))
import Grisette.Unified.Internal.UnifiedBV (UnifiedBVImpl (GetIntN, GetWordN))
import Grisette.Unified.Internal.UnifiedFP
  ( UnifiedFPImpl (GetFP, GetFPRoundingMode),
  )

class
  ( UnifiedBVImpl mode wordn intn n word int,
    UnifiedFPImpl mode fpn eb sb fp fprd,
    BitCast word fp,
    BitCast int fp,
    BitCastOr fp word,
    BitCastOr fp int,
    BitCastCanonical fp word,
    BitCastCanonical fp int,
    UnifiedFromIntegral mode word fp,
    UnifiedFromIntegral mode int fp,
    IEEEFPConvertible int fp fprd,
    IEEEFPConvertible word fp fprd
  ) =>
  UnifiedBVFPConversionImpl
    (mode :: EvalModeTag)
    wordn
    intn
    fpn
    n
    eb
    sb
    word
    int
    fp
    fprd

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

class
  ( UnifiedBVFPConversionImpl mode wordn intn fpn n eb sb word int fp fprd,
    UnifiedSafeBitCast mode NotRepresentableFPError fp int m,
    UnifiedSafeBitCast mode NotRepresentableFPError fp word m,
    UnifiedSafeFromFP mode NotRepresentableFPError word fp fprd m
  ) =>
  SafeUnifiedBVFPConversionImpl mode wordn intn fpn n eb sb word int fp fprd m

instance
  ( UnifiedBVFPConversionImpl mode wordn intn fpn n eb sb word int fp fprd,
    UnifiedSafeBitCast mode NotRepresentableFPError fp int m,
    UnifiedSafeBitCast mode NotRepresentableFPError fp word m,
    UnifiedSafeFromFP mode NotRepresentableFPError word fp fprd m
  ) =>
  SafeUnifiedBVFPConversionImpl mode wordn intn fpn n eb sb word int fp fprd m

-- | Unified constraints for safe conversion from bit-vectors to floating point
-- numbers.
class
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

-- | Unified constraints for conversion from bit-vectors to floating point
-- numbers.
class
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

-- | Evaluation mode with unified conversion from bit-vectors to
-- floating-points.
class
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
