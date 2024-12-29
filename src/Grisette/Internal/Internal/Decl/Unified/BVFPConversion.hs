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
-- Module      :   Grisette.Internal.Internal.Decl.Unified.BVFPConversion
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Decl.Unified.BVFPConversion
  ( UnifiedBVFPConversion,
    UnifiedBVFPConversionImpl,
    SafeUnifiedBVFPConversion,
    SafeUnifiedBVFPConversionImpl,
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
import Grisette.Internal.Internal.Decl.Unified.UnifiedBV
  ( UnifiedBVImpl (GetIntN, GetWordN),
  )
import Grisette.Internal.Internal.Decl.Unified.UnifiedFP
  ( UnifiedFPImpl (GetFP, GetFPRoundingMode),
  )
import Grisette.Internal.SymPrim.FP
  ( NotRepresentableFPError,
    ValidFP,
  )
import Grisette.Internal.Unified.Class.UnifiedFromIntegral (UnifiedFromIntegral)
import Grisette.Internal.Unified.Class.UnifiedSafeBitCast (UnifiedSafeBitCast)
import Grisette.Internal.Unified.Class.UnifiedSafeFromFP (UnifiedSafeFromFP)
import Grisette.Internal.Unified.Class.UnifiedSimpleMergeable (UnifiedBranching)
import Grisette.Internal.Unified.EvalModeTag (EvalModeTag)

-- | Implementation for 'UnifiedBVFPConversion'.
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

-- | Implementation for 'SafeUnifiedBVFPConversion'.
class
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
