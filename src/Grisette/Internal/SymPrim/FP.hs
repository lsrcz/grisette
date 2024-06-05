{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- HLINT ignore "Unused LANGUAGE pragma" -}

module Grisette.Internal.SymPrim.FP
  ( ValidFP,
    FP (..),
    FP16,
    FP32,
    FP64,
    withValidFPProofs,
    fpAsWordN,
    wordNAsFP,
    fp32AsFloat,
    fp64AsDouble,
    floatAsFP32,
    doubleAsFP64,
  )
where

import Control.DeepSeq (NFData (rnf))
import Data.Bits (Bits (shiftL, shiftR))
import Data.Hashable (Hashable (hashWithSalt))
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (Proxy))
import Data.SBV
  ( BVIsNonZero,
    FloatingPoint,
    SWord,
    SWord32,
    SWord64,
    SymVal (literal, unliteral),
    ValidFloat,
    nan,
    sDoubleAsSWord64,
    sFloatAsSWord32,
    sFloatingPointAsSWord,
    sWord32AsSFloat,
    sWord64AsSDouble,
    sWordAsSFloatingPoint,
  )
import Data.Type.Equality (type (:~:) (Refl))
import GHC.TypeLits (KnownNat, Nat, natVal, type (+), type (<=))
import Grisette.Internal.Core.Data.Class.BitVector (SizedBV (sizedBVConcat))
import Grisette.Internal.SymPrim.BV (WordN)
import Grisette.Internal.Utils.Parameterized
  ( KnownProof (KnownProof),
    knownAdd,
    unsafeAxiom,
    unsafeLeqProof,
    withKnownProof,
    withLeqProof,
  )
import Language.Haskell.TH.Syntax (Lift (liftTyped))

bvIsNonZeroFromGEq1 ::
  forall w r proxy.
  (1 <= w) =>
  proxy w ->
  ((BVIsNonZero w) => r) ->
  r
bvIsNonZeroFromGEq1 _ r1 = case unsafeAxiom :: w :~: 1 of
  Refl -> r1

-- | A type-level proof that the given bit-widths are valid for a floating-point
-- number.
type ValidFP (eb :: Nat) (sb :: Nat) = ValidFloat eb sb

-- | IEEE 754 floating-point number with @eb@ exponent bits and @sb@ significand
-- bits.
newtype FP (eb :: Nat) (sb :: Nat) = FP {unFP :: FloatingPoint eb sb}
  deriving newtype (Eq, Show, Ord)

-- | IEEE 754 half-precision floating-point number.
type FP16 = FP 5 11

-- | IEEE 754 single-precision floating-point number.
type FP32 = FP 8 24

-- | IEEE 754 double-precision floating-point number.
type FP64 = FP 11 53

-- | Some type-level witnesses that could be derived from 'ValidFP'.
withValidFPProofs ::
  forall eb sb r.
  (ValidFP eb sb) =>
  ( ( KnownNat (eb + sb),
      BVIsNonZero (eb + sb),
      1 <= eb + sb,
      1 <= eb,
      1 <= sb
    ) =>
    r
  ) ->
  r
withValidFPProofs r =
  withKnownProof (knownAdd (KnownProof @eb) (KnownProof @sb)) $
    withLeqProof (unsafeLeqProof @1 @(eb + sb)) $
      withLeqProof (unsafeLeqProof @1 @eb) $
        withLeqProof (unsafeLeqProof @1 @sb) $
          bvIsNonZeroFromGEq1 (Proxy @(eb + sb)) r

-- | Bitcast a floating-point number to a bit-vector.
fpAsWordN ::
  forall eb sb.
  (ValidFP eb sb) =>
  FP eb sb ->
  WordN (eb + sb)
fpAsWordN (FP f)
  | isNaN f =
      withValidFPProofs @eb @sb $
        sizedBVConcat (shiftR (-1) 1 :: WordN eb) highsb
  | otherwise = wordn
  where
    sb = fromIntegral $ natVal (Proxy @sb) :: Int
    highsb = withValidFPProofs @eb @sb $ shiftL 3 (sb - 2) :: WordN sb
    wordn :: WordN (eb + sb)
    wordn =
      withValidFPProofs @eb @sb $
        fromIntegral $
          fromJust $
            unliteral $
              sFloatingPointAsSWord $
                literal f

-- | Bitcast an 'FP32' to 'Float'.
fp32AsFloat :: FP32 -> Float
fp32AsFloat v = fromJust $ unliteral $ sWord32AsSFloat sword
  where
    sword :: SWord32
    sword = fromIntegral $ fpAsWordN v

-- | Bitcast an 'FP64' to 'Double'.
fp64AsDouble :: FP64 -> Double
fp64AsDouble v = fromJust $ unliteral $ sWord64AsSDouble sword
  where
    sword :: SWord64
    sword = fromIntegral $ fpAsWordN v

-- | Bitcast a bit-vector to a floating-point number.
wordNAsFP ::
  forall eb sb.
  (ValidFP eb sb) =>
  WordN (eb + sb) ->
  FP eb sb
wordNAsFP v = FP fp
  where
    sword :: SWord (eb + sb)
    sword = withValidFPProofs @eb @sb fromIntegral v
    fp :: FloatingPoint eb sb
    fp =
      withValidFPProofs @eb @sb $
        fromJust $
          unliteral $
            sWordAsSFloatingPoint sword

-- | Bitcast a 'Float' to 'FP32'.
floatAsFP32 :: Float -> FP32
floatAsFP32 f
  | isNaN f = FP nan
  | otherwise =
      wordNAsFP . fromIntegral . fromJust . unliteral $
        sFloatAsSWord32 (literal f)

-- | Bitcast a 'Double' to 'FP64'.
doubleAsFP64 :: Double -> FP64
doubleAsFP64 f
  | isNaN f = FP nan
  | otherwise =
      wordNAsFP . fromIntegral . fromJust . unliteral $
        sDoubleAsSWord64 (literal f)

instance NFData (FP eb sb) where
  rnf (FP x) = x `seq` ()

instance (ValidFP eb sb) => Lift (FP eb sb) where
  liftTyped fp = [||wordNAsFP wordnValue||]
    where
      wordnValue = fpAsWordN fp

instance (ValidFP eb sb) => Hashable (FP eb sb) where
  hashWithSalt salt x = hashWithSalt salt (fpAsWordN x)

deriving newtype instance (ValidFloat eb sb) => Num (FP eb sb)

deriving newtype instance (ValidFloat eb sb) => Fractional (FP eb sb)

deriving newtype instance (ValidFloat eb sb) => Floating (FP eb sb)

deriving newtype instance (ValidFloat eb sb) => Real (FP eb sb)

deriving newtype instance (ValidFloat eb sb) => RealFrac (FP eb sb)

deriving newtype instance (ValidFloat eb sb) => RealFloat (FP eb sb)
