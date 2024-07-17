{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- HLINT ignore "Unused LANGUAGE pragma" -}

-- |
-- Module      :   Grisette.Internal.SymPrim.FP
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.FP
  ( ValidFP,
    FP (..),
    FP16,
    FP32,
    FP64,
    withValidFPProofs,
    FPRoundingMode (..),
    allFPRoundingMode,
    BitCastNaNError (..),
  )
where

import Control.DeepSeq (NFData (rnf))
import Control.Exception (Exception)
import Data.Bits (Bits (complement, shiftL, shiftR, xor, (.&.)))
import Data.Hashable (Hashable (hashWithSalt))
import Data.Int (Int16, Int32, Int64)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (Proxy))
import Data.SBV
  ( BVIsNonZero,
    FloatingPoint,
    SWord,
    SymVal (literal, unliteral),
    ValidFloat,
    Word16,
    Word32,
    Word64,
    infinity,
    nan,
    sFloatingPointAsSWord,
    sWordAsSFloatingPoint,
  )
import Data.Type.Equality (type (:~:) (Refl))
import GHC.Exception (Exception (displayException))
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, Nat, natVal, type (+), type (<=))
import Grisette.Internal.Core.Data.Class.BitCast
  ( BitCast (bitCast),
    BitCastCanonical (bitCastCanonicalValue),
    BitCastOr (bitCastOr),
    bitCastOrCanonical,
  )
import Grisette.Internal.Core.Data.Class.BitVector (SizedBV (sizedBVConcat))
import Grisette.Internal.SymPrim.BV (IntN, WordN, WordN16, WordN32, WordN64)
import Grisette.Internal.Utils.Parameterized
  ( KnownProof (KnownProof),
    knownAdd,
    unsafeAxiom,
    unsafeLeqProof,
    withKnownProof,
    withLeqProof,
  )
import Language.Haskell.TH.Syntax (Lift (liftTyped))
import Test.QuickCheck (frequency, oneof)
import qualified Test.QuickCheck as QC

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
-- >>> import Grisette.Backend
-- >>> import Data.Proxy

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
--
-- >>> 1.0 + 2.0 :: FP 11 53
-- 3.0
--
-- More operations are available. Please refer to "Grisette.Core#g:symops" for
-- more information.
newtype FP (eb :: Nat) (sb :: Nat) = FP {unFP :: FloatingPoint eb sb}
  deriving newtype (Eq, Show)

-- Workaround for https://github.com/GaloisInc/libBF-hs/pull/32, which affects
-- the correctness of the Ord instance for 'Data.SBV.FloatingPoint'.
instance (ValidFP eb sb) => Ord (FP eb sb) where
  FP x < FP y | isNaN x || isNaN y = False
  FP x < FP y = x < y
  FP x <= FP y | isNaN x || isNaN y = False
  FP x <= FP y = x <= y
  FP x > FP y | isNaN x || isNaN y = False
  FP x > FP y = x > y
  FP x >= FP y | isNaN x || isNaN y = False
  FP x >= FP y = x >= y

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

instance (ValidFP eb sb, r ~ (eb + sb)) => BitCast (WordN r) (FP eb sb) where
  bitCast v = FP fp
    where
      sword :: SWord r
      sword = withValidFPProofs @eb @sb fromIntegral v
      fp :: FloatingPoint eb sb
      fp =
        withValidFPProofs @eb @sb $
          fromJust $
            unliteral $
              sWordAsSFloatingPoint sword

instance (ValidFP eb sb, r ~ (eb + sb)) => BitCast (IntN r) (FP eb sb) where
  bitCast x = withValidFPProofs @eb @sb $ bitCast (bitCast x :: WordN (eb + sb))

#define BITCAST_VIA_INTERMEDIATE(from, to, intermediate) \
  instance BitCast (from) (to) where \
    bitCast x = bitCast (bitCast x :: intermediate)

#if 1
BITCAST_VIA_INTERMEDIATE(Double, FP64, WordN 64)
BITCAST_VIA_INTERMEDIATE(Int64, FP64, WordN 64)
BITCAST_VIA_INTERMEDIATE(Word64, FP64, WordN 64)

BITCAST_VIA_INTERMEDIATE(Float, FP32, WordN 32)
BITCAST_VIA_INTERMEDIATE(Int32, FP32, WordN 32)
BITCAST_VIA_INTERMEDIATE(Word32, FP32, WordN 32)

BITCAST_VIA_INTERMEDIATE(Word16, FP16, WordN 16)
BITCAST_VIA_INTERMEDIATE(Int16, FP16, WordN 16)
#endif
instance NFData (FP eb sb) where
  rnf (FP x) = x `seq` ()

instance (ValidFP eb sb) => Lift (FP eb sb) where
  liftTyped fp = [||bitCast wordnValue||]
    where
      wordnValue = bitCastOrCanonical fp :: WordN (eb + sb)

instance (ValidFP eb sb) => Hashable (FP eb sb) where
  hashWithSalt salt x =
    hashWithSalt salt (bitCastOrCanonical x :: WordN (eb + sb))

deriving newtype instance (ValidFloat eb sb) => Num (FP eb sb)

deriving newtype instance (ValidFloat eb sb) => Fractional (FP eb sb)

deriving newtype instance (ValidFloat eb sb) => Floating (FP eb sb)

deriving newtype instance (ValidFloat eb sb) => Real (FP eb sb)

deriving newtype instance (ValidFloat eb sb) => RealFrac (FP eb sb)

deriving newtype instance (ValidFloat eb sb) => RealFloat (FP eb sb)

instance (ValidFloat eb sb) => QC.Arbitrary (FP eb sb) where
  arbitrary = do
    frequency
      [ ( 3,
          withValidFPProofs @eb @sb $
            bitCast
              <$> (QC.arbitrary :: QC.Gen (WordN (eb + sb)))
        ),
        ( 5,
          -- mostly normalized numbers
          withValidFPProofs @eb @sb $ do
            e <- egen
            s <- sgen
            msb <- msbGen
            smsb <- smsbGen
            return $ bitCast (sizedBVConcat e s `xor` msb `xor` smsb)
        ),
        ( 4,
          -- mostly denormalized numbers
          withValidFPProofs @eb @sb $ do
            s <- sgen
            msb <- msbGen
            smsb <- smsbGen
            return $
              bitCast (sizedBVConcat 0 s `xor` msb .&. complement smsb)
        ),
        (1, oneof $ return <$> [nan, 0, -0, infinity, -infinity])
      ]
    where
      eb = fromIntegral $ natVal (Proxy @eb) :: Int
      sb = fromIntegral $ natVal (Proxy @sb) :: Int
      egen = withValidFPProofs @eb @sb $ QC.arbitrary :: QC.Gen (WordN eb)
      sgen = withValidFPProofs @eb @sb $ QC.arbitrary :: QC.Gen (WordN sb)
      msbGen =
        withValidFPProofs @eb @sb $
          oneof [return 0, return $ 1 `shiftL` (eb + sb - 1)] ::
          QC.Gen (WordN (eb + sb))
      smsbGen =
        withValidFPProofs @eb @sb $
          oneof [return 0, return $ 1 `shiftL` (sb - 1)] ::
          QC.Gen (WordN (eb + sb))

-- | Rounding mode for floating-point operations.
data FPRoundingMode
  = -- | Round to nearest, ties to even.
    RNE
  | -- | Round to nearest, ties to away from zero.
    RNA
  | -- | Round towards positive infinity.
    RTP
  | -- | Round towards negative infinity.
    RTN
  | -- | Round towards zero.
    RTZ
  deriving (Eq, Ord, Generic, Lift)
  deriving anyclass (Hashable, NFData)

instance Show FPRoundingMode where
  show RNE = "rne"
  show RNA = "rna"
  show RTP = "rtp"
  show RTN = "rtn"
  show RTZ = "rtz"

-- | All IEEE 754 rounding modes.
allFPRoundingMode :: [FPRoundingMode]
allFPRoundingMode = [RNE, RNA, RTP, RTN, RTZ]

instance QC.Arbitrary FPRoundingMode where
  arbitrary = QC.elements [RNE, RNA, RTP, RTN, RTZ]

instance
  (ValidFP eb sb, n ~ eb + sb) =>
  BitCastCanonical (FP eb sb) (WordN n)
  where
  bitCastCanonicalValue _ =
    withValidFPProofs @eb @sb $
      sizedBVConcat (shiftR (-1) 1 :: WordN eb) highsb
    where
      sb = fromIntegral $ natVal (Proxy @sb) :: Int
      highsb = withValidFPProofs @eb @sb $ shiftL 3 (sb - 2) :: WordN sb

instance
  (ValidFP eb sb, n ~ eb + sb) =>
  BitCastCanonical (FP eb sb) (IntN n)
  where
  bitCastCanonicalValue n =
    withValidFPProofs @eb @sb $
      bitCast (bitCastCanonicalValue n :: WordN n)

#define BIT_CAST_CANONICAL_VIA_INTERMEDIATE(from, to, intermediate) \
  instance BitCastCanonical (from) (to) where \
    bitCastCanonicalValue x = bitCast (bitCastCanonicalValue x :: intermediate)

#if 1
BIT_CAST_CANONICAL_VIA_INTERMEDIATE(FP64, Word64, WordN64)
BIT_CAST_CANONICAL_VIA_INTERMEDIATE(FP64, Int64, WordN64)
BIT_CAST_CANONICAL_VIA_INTERMEDIATE(FP64, Double, WordN64)
BIT_CAST_CANONICAL_VIA_INTERMEDIATE(FP32, Word32, WordN32)
BIT_CAST_CANONICAL_VIA_INTERMEDIATE(FP32, Int32, WordN32)
BIT_CAST_CANONICAL_VIA_INTERMEDIATE(FP32, Float, WordN32)
BIT_CAST_CANONICAL_VIA_INTERMEDIATE(FP16, Word16, WordN16)
BIT_CAST_CANONICAL_VIA_INTERMEDIATE(FP16, Int16, WordN16)
#endif

instance (ValidFP eb sb, r ~ (eb + sb)) => BitCastOr (FP eb sb) (WordN r) where
  bitCastOr d (FP f)
    | isNaN f = d
    | otherwise = wordn
    where
      wordn :: WordN (eb + sb)
      wordn =
        withValidFPProofs @eb @sb $
          fromIntegral $
            fromJust $
              unliteral $
                sFloatingPointAsSWord $
                  literal f

instance
  (ValidFP eb sb, n ~ eb + sb) =>
  BitCastOr (FP eb sb) (IntN n)
  where
  bitCastOr d n =
    withValidFPProofs @eb @sb $
      bitCast (bitCastOr (bitCast d) n :: WordN n)

#define BIT_CAST_OR_VIA_INTERMEDIATE(from, to, intermediate) \
  instance BitCastOr (from) (to) where \
    bitCastOr d x = bitCast (bitCastOr (bitCast d) x :: intermediate)

#if 1
BIT_CAST_OR_VIA_INTERMEDIATE(FP64, Word64, WordN64)
BIT_CAST_OR_VIA_INTERMEDIATE(FP64, Int64, WordN64)
BIT_CAST_OR_VIA_INTERMEDIATE(FP64, Double, WordN64)
BIT_CAST_OR_VIA_INTERMEDIATE(FP32, Word32, WordN32)
BIT_CAST_OR_VIA_INTERMEDIATE(FP32, Int32, WordN32)
BIT_CAST_OR_VIA_INTERMEDIATE(FP32, Float, WordN32)
BIT_CAST_OR_VIA_INTERMEDIATE(FP16, Word16, WordN16)
BIT_CAST_OR_VIA_INTERMEDIATE(FP16, Int16, WordN16)
#endif

-- | An error thrown when bitcasting 'FP' NaN to other types.
data BitCastNaNError = BitCastNaNError
  deriving (Show, Eq, Ord, Generic)

instance Exception BitCastNaNError where
  displayException BitCastNaNError =
    "Bitcasting NaN value cannot be done with SMT-LIB2"
