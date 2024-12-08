{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Unified.Internal.BVBVConversion
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Unified.Internal.BVBVConversion
  ( UnifiedBVBVConversion,
    AllUnifiedBVBVConversion,
  )
where

import GHC.TypeNats (KnownNat, Nat, type (<=))
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.SymBV (SymIntN, SymWordN)
import Grisette.Unified.Internal.Class.UnifiedFromIntegral (UnifiedFromIntegral)
import Grisette.Unified.Internal.EvalModeTag (EvalModeTag (C, S))
import Grisette.Unified.Internal.UnifiedBV (UnifiedBVImpl (GetIntN, GetWordN))

class
  ( bv0 ~ bvn0 n0,
    bv1 ~ bvn1 n1,
    UnifiedFromIntegral mode bv0 bv1
  ) =>
  UnifiedBVBVConversionImpl
    (mode :: EvalModeTag)
    bvn0
    bvn1
    (n0 :: Nat)
    (n1 :: Nat)
    bv0
    bv1
    | bvn0 n0 -> bv0,
      bvn1 n1 -> bv1,
      bv0 -> bvn0 n0,
      bv1 -> bvn1 n1

#define QUOTE() '

#define CONINSTANCE(ty0, ty1) \
instance \
  (KnownNat n0, 1 <= n0, KnownNat n1, 1 <= n1) => \
  UnifiedBVBVConversionImpl QUOTE()C ty0 ty1 n0 n1 (ty0 n0) (ty1 n1)

#define SYMINSTANCE(ty0, ty1) \
instance \
  (KnownNat n0, 1 <= n0, KnownNat n1, 1 <= n1) => \
  UnifiedBVBVConversionImpl QUOTE()S ty0 ty1 n0 n1 (ty0 n0) (ty1 n1)

#if 1
CONINSTANCE(WordN, WordN)
CONINSTANCE(WordN, IntN)
CONINSTANCE(IntN, WordN)
CONINSTANCE(IntN, IntN)
SYMINSTANCE(SymWordN, SymWordN)
SYMINSTANCE(SymWordN, SymIntN)
SYMINSTANCE(SymIntN, SymWordN)
SYMINSTANCE(SymIntN, SymIntN)
#endif

-- | Unified constraints for conversion between bit-vectors.
class
  ( UnifiedBVBVConversionImpl
      mode
      (GetWordN mode)
      (GetWordN mode)
      n0
      n1
      (GetWordN mode n0)
      (GetWordN mode n1),
    UnifiedBVBVConversionImpl
      mode
      (GetWordN mode)
      (GetIntN mode)
      n0
      n1
      (GetWordN mode n0)
      (GetIntN mode n1),
    UnifiedBVBVConversionImpl
      mode
      (GetIntN mode)
      (GetWordN mode)
      n0
      n1
      (GetIntN mode n0)
      (GetWordN mode n1),
    UnifiedBVBVConversionImpl
      mode
      (GetIntN mode)
      (GetIntN mode)
      n0
      n1
      (GetIntN mode n0)
      (GetIntN mode n1)
  ) =>
  UnifiedBVBVConversion (mode :: EvalModeTag) n0 n1

instance
  ( UnifiedBVBVConversionImpl
      mode
      (GetWordN mode)
      (GetWordN mode)
      n0
      n1
      (GetWordN mode n0)
      (GetWordN mode n1),
    UnifiedBVBVConversionImpl
      mode
      (GetWordN mode)
      (GetIntN mode)
      n0
      n1
      (GetWordN mode n0)
      (GetIntN mode n1),
    UnifiedBVBVConversionImpl
      mode
      (GetIntN mode)
      (GetWordN mode)
      n0
      n1
      (GetIntN mode n0)
      (GetWordN mode n1),
    UnifiedBVBVConversionImpl
      mode
      (GetIntN mode)
      (GetIntN mode)
      n0
      n1
      (GetIntN mode n0)
      (GetIntN mode n1)
  ) =>
  UnifiedBVBVConversion (mode :: EvalModeTag) n0 n1

-- | Evaluation mode with unified conversion from bit-vectors to bit-vectors.
class
  ( forall n0 n1.
    (KnownNat n0, KnownNat n1, 1 <= n0, 1 <= n1) =>
    UnifiedBVBVConversion mode n0 n1
  ) =>
  AllUnifiedBVBVConversion mode

instance
  ( forall n0 n1.
    (KnownNat n0, KnownNat n1, 1 <= n0, 1 <= n1) =>
    UnifiedBVBVConversion mode n0 n1
  ) =>
  AllUnifiedBVBVConversion mode
