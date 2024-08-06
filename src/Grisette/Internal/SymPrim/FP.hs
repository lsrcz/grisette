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
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
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
    NotRepresentableFPError (..),
    ConvertibleBound (..),
    nextFP,
    prevFP,
  )
where

import Control.DeepSeq (NFData (rnf))
import Control.Exception (Exception, throw)
import Data.Bits (Bits (complement, shiftL, shiftR, xor, (.&.)))
import Data.Hashable (Hashable (hashWithSalt))
import Data.Int (Int16, Int32, Int64)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (Proxy))
import Data.Ratio (numerator)
import Data.SBV
  ( BVIsNonZero,
    FloatingPoint,
    SWord,
    SymVal (literal, unliteral),
    ValidFloat,
    Word16,
    Word32,
    Word64,
    denominator,
    infinity,
    nan,
    sFloatingPointAsSWord,
    sWordAsSFloatingPoint,
  )
import Data.SBV.Float (fpEncodeFloat)
import qualified Data.SBV.Float as SBVF
import qualified Data.SBV.Internals as SBVI
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
import Grisette.Internal.Core.Data.Class.IEEEFP
  ( IEEEFPConstants
      ( fpMaxNormalized,
        fpMaxSubnormal,
        fpMinNormalized,
        fpMinSubnormal,
        fpNaN,
        fpNegativeInfinite,
        fpNegativeZero,
        fpPositiveInfinite,
        fpPositiveZero
      ),
    IEEEFPConvertible (fromFPOr, toFP),
    IEEEFPOp
      ( fpAbs,
        fpMaximum,
        fpMaximumNumber,
        fpMinimum,
        fpMinimumNumber,
        fpNeg,
        fpRem
      ),
    IEEEFPRoundingMode (rna, rne, rtn, rtp, rtz),
    IEEEFPRoundingOp (fpAdd, fpDiv, fpFMA, fpMul, fpRoundToIntegral, fpSqrt, fpSub),
    IEEEFPToAlgReal,
    fpIsInfinite,
    fpIsNaN,
    fpIsNegativeInfinite,
    fpIsNegativeZero,
    fpIsPositiveInfinite,
    fpIsPositiveZero,
    fpIsZero,
  )
import Grisette.Internal.SymPrim.AlgReal
  ( AlgReal (AlgExactRational),
    UnsupportedAlgRealOperation (UnsupportedAlgRealOperation, msg, op),
  )
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
import LibBF
  ( BFOpts,
    BigFloat,
    RoundMode,
    Status,
    allowSubnormal,
    bfAdd,
    bfDiv,
    bfFMA,
    bfFromInteger,
    bfIsInf,
    bfIsNaN,
    bfIsNeg,
    bfIsPos,
    bfIsZero,
    bfMul,
    bfNaN,
    bfNegInf,
    bfNegZero,
    bfPosInf,
    bfPosZero,
    bfRoundFloat,
    bfRoundInt,
    bfSqrt,
    bfSub,
    expBits,
    precBits,
    rnd,
    pattern NearAway,
    pattern NearEven,
    pattern ToNegInf,
    pattern ToPosInf,
    pattern ToZero,
  )
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
  (ValidFP eb sb, n ~ (eb + sb)) =>
  BitCastCanonical (FP eb sb) (WordN n)
  where
  bitCastCanonicalValue _ =
    withValidFPProofs @eb @sb $
      sizedBVConcat (shiftR (-1) 1 :: WordN eb) highsb
    where
      sb = fromIntegral $ natVal (Proxy @sb) :: Int
      highsb = withValidFPProofs @eb @sb $ shiftL 3 (sb - 2) :: WordN sb

instance
  (ValidFP eb sb, n ~ (eb + sb)) =>
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
  (ValidFP eb sb, n ~ (eb + sb)) =>
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

-- | An error thrown when bitcasting or converting t'FP' NaN to other types.
data NotRepresentableFPError
  = NaNError
  | FPUnderflowError
  | FPOverflowError
  deriving (Show, Eq, Ord, Generic)

instance Exception NotRepresentableFPError where
  displayException NaNError =
    "Converting NaN value cannot be done precisely with SMT-LIB2"
  displayException FPUnderflowError =
    "Converting FP values that cannot be represented by non-FP types due to "
      <> "underflowing"
  displayException FPOverflowError =
    "Converting FP values that cannot be represented by non-FP types due to "
      <> "overflowing"

instance (ValidFP eb sb) => IEEEFPConstants (FP eb sb) where
  fpPositiveInfinite = FP infinity
  {-# INLINE fpPositiveInfinite #-}
  fpNegativeInfinite = FP $ -infinity
  {-# INLINE fpNegativeInfinite #-}
  fpNaN = FP nan
  {-# INLINE fpNaN #-}
  fpNegativeZero = FP $ -0
  {-# INLINE fpNegativeZero #-}
  fpPositiveZero = FP 0
  {-# INLINE fpPositiveZero #-}

  fpMinNormalized =
    withValidFPProofs @eb @sb $
      bitCast $
        (1 :: WordN (eb + sb)) `shiftL` fromIntegral (natVal (Proxy @sb) - 1)
  {-# INLINE fpMinNormalized #-}

  fpMaxNormalized =
    withValidFPProofs @eb @sb $
      bitCast $
        complement
          ( (1 :: WordN (eb + sb))
              `shiftL` fromIntegral (natVal (Proxy @sb) - 1)
          )
          `shiftL` 1
          `shiftR` 1
  {-# INLINE fpMaxNormalized #-}

  fpMinSubnormal = withValidFPProofs @eb @sb $ bitCast (1 :: WordN (eb + sb))
  {-# INLINE fpMinSubnormal #-}

  fpMaxSubnormal =
    withValidFPProofs @eb @sb $
      bitCast
        ( (1 :: WordN (eb + sb))
            `shiftL` fromIntegral (natVal (Proxy @sb) - 1)
            - 1
        )
  {-# INLINE fpMaxSubnormal #-}

cmpHandleNegZero :: (ValidFP eb sb) => FP eb sb -> FP eb sb -> Bool
cmpHandleNegZero x y =
  if fpIsZero x && fpIsZero y then fpIsNegativeZero x else x < y

instance (ValidFP eb sb) => IEEEFPOp (FP eb sb) where
  fpAbs = abs
  {-# INLINE fpAbs #-}
  fpNeg = negate
  {-# INLINE fpNeg #-}
  fpRem = SBVI.fpRemH
  {-# INLINE fpRem #-}
  fpMinimum a b
    | fpIsNaN a || fpIsNaN b = fpNaN
    | cmpHandleNegZero a b = a
    | otherwise = b
  {-# INLINE fpMinimum #-}
  fpMinimumNumber a b
    | fpIsNaN a = b
    | fpIsNaN b = a
    | cmpHandleNegZero a b = a
    | otherwise = b
  {-# INLINE fpMinimumNumber #-}
  fpMaximum a b
    | fpIsNaN a || fpIsNaN b = fpNaN
    | cmpHandleNegZero a b = b
    | otherwise = a
  {-# INLINE fpMaximum #-}
  fpMaximumNumber a b
    | fpIsNaN a = b
    | fpIsNaN b = a
    | cmpHandleNegZero a b = b
    | otherwise = a
  {-# INLINE fpMaximumNumber #-}

instance IEEEFPRoundingMode FPRoundingMode where
  rne = RNE
  {-# INLINE rne #-}
  rna = RNA
  {-# INLINE rna #-}
  rtp = RTP
  {-# INLINE rtp #-}
  rtn = RTN
  {-# INLINE rtn #-}
  rtz = RTZ
  {-# INLINE rtz #-}

libBFRoundingMode :: FPRoundingMode -> RoundMode
libBFRoundingMode RNE = NearEven
libBFRoundingMode RNA = NearAway
libBFRoundingMode RTP = ToPosInf
libBFRoundingMode RTN = ToNegInf
libBFRoundingMode RTZ = ToZero

libBFOpts ::
  forall eb sb. (ValidFP eb sb) => FPRoundingMode -> FP eb sb -> BFOpts
libBFOpts mode _ = rnd rd <> precBits sb <> expBits eb <> allowSubnormal
  where
    eb = fromIntegral $ natVal (Proxy @eb) :: Int
    sb = fromIntegral $ natVal (Proxy @sb) :: Word
    rd = libBFRoundingMode mode

toLibBF :: forall eb sb. (ValidFP eb sb) => FP eb sb -> BigFloat
toLibBF f
  | fpIsNegativeZero f = bfNegZero
  | fpIsPositiveZero f = bfPosZero
  | fpIsPositiveInfinite f = bfPosInf
  | fpIsNegativeInfinite f = bfNegInf
  | fpIsNaN f = bfNaN
  | otherwise =
      SBVF.fpValue $
        uncurry (fpEncodeFloat eb sb) $
          decodeFloat f
  where
    eb = fromIntegral $ natVal (Proxy @eb) :: Int
    sb = fromIntegral $ natVal (Proxy @sb) :: Int

fromLibBF :: forall eb sb. (ValidFP eb sb) => BigFloat -> FP eb sb
fromLibBF f
  | bfIsNeg f && bfIsZero f = fpNegativeZero
  | bfIsPos f && bfIsZero f = fpPositiveZero
  | bfIsNeg f && bfIsInf f = fpNegativeInfinite
  | bfIsPos f && bfIsInf f = fpPositiveInfinite
  | bfIsNaN f = fpNaN
  | otherwise = uncurry encodeFloat $ decodeFloat fp
  where
    fp = SBVF.FP eb sb f
    eb = fromIntegral $ natVal (Proxy @eb) :: Int
    sb = fromIntegral $ natVal (Proxy @sb) :: Int

liftLibBF1 ::
  (ValidFP eb sb) =>
  (BFOpts -> BigFloat -> (BigFloat, Status)) ->
  FPRoundingMode ->
  FP eb sb ->
  FP eb sb
liftLibBF1 f rd x = fromLibBF $ fst $ f opts xbf
  where
    opts = libBFOpts rd x
    xbf = toLibBF x

liftLibBF2 ::
  (ValidFP eb sb) =>
  (BFOpts -> BigFloat -> BigFloat -> (BigFloat, Status)) ->
  FPRoundingMode ->
  FP eb sb ->
  FP eb sb ->
  FP eb sb
liftLibBF2 f rd l r = fromLibBF $ fst $ f opts lbf rbf
  where
    opts = libBFOpts rd l
    lbf = toLibBF l
    rbf = toLibBF r

liftLibBF3 ::
  (ValidFP eb sb) =>
  (BFOpts -> BigFloat -> BigFloat -> BigFloat -> (BigFloat, Status)) ->
  FPRoundingMode ->
  FP eb sb ->
  FP eb sb ->
  FP eb sb ->
  FP eb sb
liftLibBF3 f rd x y z = fromLibBF $ fst $ f opts xbf ybf zbf
  where
    opts = libBFOpts rd x
    xbf = toLibBF x
    ybf = toLibBF y
    zbf = toLibBF z

instance (ValidFP eb sb) => IEEEFPRoundingOp (FP eb sb) FPRoundingMode where
  fpAdd = liftLibBF2 bfAdd
  {-# INLINE fpAdd #-}
  fpSub = liftLibBF2 bfSub
  {-# INLINE fpSub #-}
  fpMul = liftLibBF2 bfMul
  {-# INLINE fpMul #-}
  fpDiv = liftLibBF2 bfDiv
  {-# INLINE fpDiv #-}
  fpFMA = liftLibBF3 bfFMA
  {-# INLINE fpFMA #-}
  fpSqrt = liftLibBF1 bfSqrt
  {-# INLINE fpSqrt #-}
  fpRoundToIntegral rd x =
    fromLibBF $ fst $ bfRoundInt (libBFRoundingMode rd) $ toLibBF x
  {-# INLINE fpRoundToIntegral #-}

instance
  (ValidFP eb sb) =>
  IEEEFPConvertible AlgReal (FP eb sb) FPRoundingMode
  where
  fromFPOr d _ fp
    | fpIsInfinite fp = d
    | fpIsNaN fp = d
    | otherwise =
        let (m, n) = decodeFloat fp
         in fromRational (toRational m * (2 ^^ n))
  toFP mode (AlgExactRational v) = fromLibBF $ fst $ bfDiv opts n d
    where
      opts = libBFOpts mode (undefined :: FP eb sb)
      n = bfFromInteger $ numerator v
      d = bfFromInteger $ denominator v
  toFP _ r =
    throw
      UnsupportedAlgRealOperation {op = "toFP", msg = show r}

instance
  (ValidFP eb sb) =>
  IEEEFPToAlgReal AlgReal (FP eb sb) FPRoundingMode

roundRationalToInteger :: FPRoundingMode -> Rational -> Integer
roundRationalToInteger mode r
  | d == 1 = n
  | d == 2 = case mode of
      RNE -> if even ndivd then ndivd else ndivd + 1
      RNA -> if n > 0 then ndivd + 1 else ndivd
      RTP -> ndivd + 1
      RTN -> ndivd
      RTZ -> if n > 0 then ndivd else ndivd + 1
  | otherwise = case mode of
      RNE -> if nmodd > d `div` 2 then ndivd + 1 else ndivd
      RNA -> if nmodd > d `div` 2 then ndivd + 1 else ndivd
      RTP -> ndivd + 1
      RTN -> ndivd
      RTZ -> if n > 0 then ndivd else ndivd + 1
  where
    n = numerator r
    d = denominator r
    ndivd = n `div` d
    nmodd = n `mod` d

instance
  (ValidFP eb sb) =>
  IEEEFPConvertible Integer (FP eb sb) FPRoundingMode
  where
  fromFPOr d mode fp
    | fpIsInfinite fp = d
    | fpIsNaN fp = d
    | otherwise =
        let r = fromFPOr (fromIntegral d) mode fp
         in case r of
              AlgExactRational v -> roundRationalToInteger mode v
              _ -> error "Should not happen"
  toFP mode r = toFP mode (fromIntegral r :: AlgReal)

instance
  (ValidFP eb sb, KnownNat n, 1 <= n) =>
  IEEEFPConvertible (WordN n) (FP eb sb) FPRoundingMode
  where
  fromFPOr d mode fp
    | fpIsInfinite fp = d
    | fpIsNaN fp = d
    | otherwise =
        let p = fromFPOr (fromIntegral d :: Integer) mode fp
         in if p < (fromIntegral (minBound :: WordN n))
              || p > (fromIntegral (maxBound :: WordN n))
              then d
              else fromIntegral p
  toFP mode r = toFP mode (fromIntegral r :: AlgReal)

instance
  (ValidFP eb sb, KnownNat n, 1 <= n) =>
  IEEEFPConvertible (IntN n) (FP eb sb) FPRoundingMode
  where
  fromFPOr d mode fp
    | fpIsInfinite fp = d
    | fpIsNaN fp = d
    | otherwise =
        let p = fromFPOr (fromIntegral d :: Integer) mode fp
         in if p < (fromIntegral (minBound :: IntN n))
              || p > (fromIntegral (maxBound :: IntN n))
              then d
              else fromIntegral p
  toFP mode r = toFP mode (fromIntegral r :: AlgReal)

instance
  (ValidFP eb sb, ValidFP eb' sb') =>
  IEEEFPConvertible (FP eb' sb') (FP eb sb) FPRoundingMode
  where
  fromFPOr _ = toFP
  toFP mode fp
    | fpIsNegativeInfinite fp = fpNegativeInfinite
    | fpIsPositiveInfinite fp = fpPositiveInfinite
    | fpIsNaN fp = fpNaN
    | fpIsNegativeZero fp = fpNegativeZero
    | fpIsPositiveZero fp = fpPositiveZero
    | otherwise =
        let bffp = toLibBF fp
            opts = libBFOpts mode (undefined :: FP eb sb)
         in fromLibBF $ fst $ bfRoundFloat opts bffp

-- | Next representable floating-point number.
--
-- Note:
--
-- > nextFP(+inf) = +inf
-- > nextFP(-inf) = -maxNormalized
-- > nextFP(NaN) = NaN
--
-- The function do not distinguish between -0 and +0.
nextFP :: forall eb sb. (ValidFP eb sb) => FP eb sb -> FP eb sb
nextFP x
  | fpIsNaN x = fpNaN
  | fpIsNegativeInfinite x = -fpMaxNormalized
  | x == -fpMinNormalized = -fpMaxSubnormal
  | x == -fpMinSubnormal = 0
  | x == 0 = fpMinSubnormal
  | x == fpMaxSubnormal = fpMinNormalized
  | x == fpMaxNormalized = fpPositiveInfinite
  | fpIsPositiveInfinite x = fpPositiveInfinite
  | x > 0 =
      withValidFPProofs @eb @sb $
        bitCast ((bitCastOrCanonical x :: WordN (eb + sb)) + 1)
  | otherwise =
      withValidFPProofs @eb @sb $
        bitCast ((bitCastOrCanonical x :: WordN (eb + sb)) - 1)

-- | Previous representable floating-point number.
--
-- Note:
--
-- > prevFP(+inf) = +maxNormalized
-- > prevFP(-inf) = -inf
-- > prevFP(NaN) = NaN
--
-- The function do not distinguish between -0 and +0.
prevFP :: forall eb sb. (ValidFP eb sb) => FP eb sb -> FP eb sb
prevFP x
  | fpIsNaN x = fpNaN
  | fpIsPositiveInfinite x = fpMaxNormalized
  | x == fpMinNormalized = fpMaxSubnormal
  | x == fpMinSubnormal = 0
  | x == 0 = -fpMinSubnormal
  | x == -fpMaxSubnormal = -fpMinNormalized
  | x == -fpMaxNormalized = fpNegativeInfinite
  | fpIsNegativeInfinite x = fpNegativeInfinite
  | x > 0 =
      withValidFPProofs @eb @sb $
        bitCast ((bitCastOrCanonical x :: WordN (eb + sb)) - 1)
  | otherwise =
      withValidFPProofs @eb @sb $
        bitCast ((bitCastOrCanonical x :: WordN (eb + sb)) + 1)

-- | Bounds for converting bit vectors to floating-point numbers. Out-of-range
-- FP values cannot be converted to a representable bit-vector.
class ConvertibleBound bv where
  convertibleLowerBound ::
    forall eb sb n.
    (ValidFP eb sb, KnownNat n, 1 <= n) =>
    bv n ->
    FPRoundingMode ->
    FP eb sb
  convertibleUpperBound ::
    forall eb sb n.
    (ValidFP eb sb, KnownNat n, 1 <= n) =>
    bv n ->
    FPRoundingMode ->
    FP eb sb

instance ConvertibleBound WordN where
  convertibleLowerBound _ RTP = nextFP $ -1
  convertibleLowerBound _ RTZ = nextFP $ -1
  convertibleLowerBound _ RTN = 0
  convertibleLowerBound _ RNA = nextFP $ -0.5
  convertibleLowerBound _ RNE = -0.5
  convertibleUpperBound ::
    forall eb sb n.
    (ValidFP eb sb, KnownNat n, 1 <= n) =>
    WordN n ->
    FPRoundingMode ->
    FP eb sb
  convertibleUpperBound _ mode
    | ebn < n = fpMaxNormalized
    | ebn == n && sb <= n = fpMaxNormalized
    | ebn >= n && sb > n = case mode of
        RTP -> toFP rne (maxBound :: WordN n)
        RTZ -> prevFP $ toFP rne (maxBound :: WordN n) + 1
        RTN -> prevFP $ toFP rne (maxBound :: WordN n) + 1
        RNA -> prevFP $ toFP rne (maxBound :: WordN n) + 0.5
        RNE -> prevFP $ toFP rne (maxBound :: WordN n) + 0.5
    | ebn > n && sb == n = toFP rne (maxBound :: WordN n)
    -- ebn > n && sb < n
    | otherwise =
        prevFP $ toFP rne (maxBound `div` 2 + 1 :: WordN n) * 2
    where
      n = natVal (Proxy @n)
      eb = natVal (Proxy @eb)
      ebn = 2 ^ (eb - 1)
      sb = natVal (Proxy @sb)

instance ConvertibleBound IntN where
  convertibleLowerBound ::
    forall eb sb n.
    (ValidFP eb sb, KnownNat n, 1 <= n) =>
    IntN n ->
    FPRoundingMode ->
    FP eb sb
  convertibleLowerBound _ mode
    | ebn <= n - 1 = -fpMaxNormalized
    | ebn > n - 1 && sb <= n - 1 = toFP rne (minBound :: IntN n)
    -- ebn > n - 1 && sb > n - 1
    | otherwise = case mode of
        RTP -> nextFP $ toFP rne (minBound :: IntN n) - 1
        RTZ -> nextFP $ toFP rne (minBound :: IntN n) - 1
        RTN -> toFP rne (minBound :: IntN n)
        RNA ->
          if sb == n
            then toFP rne (minBound :: IntN n) - 0.5
            else nextFP $ toFP rne (minBound :: IntN n) - 0.5
        RNE -> toFP rne (minBound :: IntN n) - 0.5
    where
      n = natVal (Proxy @n)
      eb = natVal (Proxy @eb)
      ebn = 2 ^ (eb - 1)
      sb = natVal (Proxy @sb)
  convertibleUpperBound ::
    forall eb sb n.
    (ValidFP eb sb, KnownNat n, 1 <= n) =>
    IntN n ->
    FPRoundingMode ->
    FP eb sb
  convertibleUpperBound _ mode
    | ebn < n - 1 = fpMaxNormalized
    | ebn == n - 1 && sb <= n - 1 = fpMaxNormalized
    | ebn >= n - 1 && sb > n - 1 = case mode of
        RTP -> toFP rne (maxBound :: IntN n)
        RTZ -> prevFP $ toFP rne (maxBound :: IntN n) + 1
        RTN -> prevFP $ toFP rne (maxBound :: IntN n) + 1
        RNA -> prevFP $ toFP rne (maxBound :: IntN n) + 0.5
        RNE -> prevFP $ toFP rne (maxBound :: IntN n) + 0.5
    | ebn > n - 1 && sb == n - 1 = toFP rne (maxBound :: IntN n)
    -- ebn > n - 1 && sb < n - 1
    | otherwise =
        prevFP $ toFP rne (maxBound `div` 2 + 1 :: IntN n) * 2
    where
      n = natVal (Proxy @n)
      eb = natVal (Proxy @eb)
      ebn = 2 ^ (eb - 1)
      sb = natVal (Proxy @sb)
