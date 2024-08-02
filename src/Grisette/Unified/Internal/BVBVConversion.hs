{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Unified.Internal.BVBVConversion
  ( UnifiedBVBVConversion,
    AllUnifiedBVBVConversion,
  )
where

import GHC.TypeNats (KnownNat, Nat, type (<=))
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.SymBV (SymIntN, SymWordN)
import Grisette.Unified.Internal.Class.UnifiedFromIntegral (UnifiedFromIntegral)
import Grisette.Unified.Internal.EvalModeTag (EvalModeTag (Con, Sym))
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

#define INSTANCE(md, ty0, ty1) \
instance \
  (KnownNat n0, 1 <= n0, KnownNat n1, 1 <= n1) => \
  UnifiedBVBVConversionImpl 'md ty0 ty1 n0 n1 (ty0 n0) (ty1 n1)

INSTANCE (Con, WordN, WordN)
INSTANCE (Con, WordN, IntN)
INSTANCE (Con, IntN, WordN)
INSTANCE (Con, IntN, IntN)
INSTANCE (Sym, SymWordN, SymWordN)
INSTANCE (Sym, SymWordN, SymIntN)
INSTANCE (Sym, SymIntN, SymWordN)
INSTANCE (Sym, SymIntN, SymIntN)

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
