{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Grisette.SymPrim.FPTests (fpTests) where

import Data.Foldable (traverse_)
import Data.Kind (Type)
import Data.Ratio ((%))
import Data.SBV (SMTResult (Satisfiable, Unsatisfiable))
import qualified Data.SBV as SBV
import Data.Serialize (decode, encode)
import Data.Word (Word32, Word64)
import GHC.TypeLits (KnownNat, Nat, type (<=))
import Grisette
  ( AlgReal (AlgExactRational),
    FP,
    FPRoundingMode (RTP),
    IEEEFPConstants
      ( fpMaxNormalized,
        fpMaxSubnormal,
        fpMinNormalized,
        fpMinSubnormal
      ),
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
    IEEEFPRoundingOp
      ( fpAdd,
        fpDiv,
        fpFMA,
        fpMul,
        fpRoundToIntegral,
        fpSqrt,
        fpSub
      ),
    KeyEq (keyEq),
    WordN,
    bitCastOrCanonical,
  )
import Grisette.Internal.Core.Data.Class.BitCast (BitCast (bitCast))
import Grisette.Internal.Core.Data.Class.IEEEFP
  ( IEEEFPConstants
      ( fpNaN,
        fpNegativeInfinite,
        fpNegativeZero,
        fpPositiveInfinite,
        fpPositiveZero
      ),
    IEEEFPConvertible (fromFPOr, toFP),
    fpIsNaN,
    fpIsNegativeInfinite,
    fpIsNegativeZero,
    fpIsPositiveInfinite,
    fpIsPositiveZero,
  )
import Grisette.Internal.Core.Data.Class.SafeFromFP (SafeFromFP (safeFromFP))
import Grisette.Internal.Core.Data.Class.SymIEEEFP
  ( SymIEEEFPTraits
      ( symFpIsInfinite,
        symFpIsNaN,
        symFpIsNegative,
        symFpIsNegativeInfinite,
        symFpIsNegativeZero,
        symFpIsNormal,
        symFpIsPoint,
        symFpIsPositive,
        symFpIsPositiveInfinite,
        symFpIsPositiveZero,
        symFpIsSubnormal,
        symFpIsZero
      ),
  )
import Grisette.Internal.SymPrim.BV (IntN)
import Grisette.Internal.SymPrim.FP
  ( ConvertibleBound (convertibleLowerBound, convertibleUpperBound),
    FP32,
    NotRepresentableFPError,
    ValidFP,
    nextFP,
    prevFP,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalIEEEFPConvertibleTerm
  ( genericFPCast,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( SupportedPrim (conSBVTerm),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (assertBool, (@?=))
import Test.QuickCheck (Arbitrary, ioProperty)
import Type.Reflection (Typeable, typeRep)

sameFP :: forall a b. (RealFloat a, RealFloat b) => a -> b -> Bool
sameFP x y
  | isNaN x && isNaN y = True
  | isInfinite x && isInfinite y =
      (x < 0 && y < 0) || (x > 0 && y > 0)
  -- \| GHC's floating point support doesn't conform to IEEE754.
  | otherwise = (uncurry encodeFloat (decodeFloat x) :: b) == y

fp32ConversionTest :: (Word32 -> IO ()) -> [Test]
fp32ConversionTest testFun =
  [ testProperty "property" $ ioProperty . testFun,
    testCase "NaN" $ testFun 0x7f800100,
    testCase "+inf" $ testFun 0x7f800000,
    testCase "-inf" $ testFun 0xFf800000,
    testCase "0" $ testFun 0,
    testCase "-0" $ testFun 0x80000000
  ]

fp64ConversionTest :: (Word64 -> IO ()) -> [Test]
fp64ConversionTest testFun =
  [ testProperty "property" $ ioProperty . testFun,
    testCase "NaN" $ testFun 0x7ff8000010000000,
    testCase "+inf" $ testFun 0x7FF0000000000000,
    testCase "-inf" $ testFun 0xFFF0000000000000,
    testCase "0" $ testFun 0,
    testCase "-0" $ testFun 0x8000000000000000
  ]

unaryOpComplianceWithFloat ::
  String ->
  (FP32 -> a) ->
  (Float -> b) ->
  (a -> b -> Bool) ->
  Test
unaryOpComplianceWithFloat name fpOp floatOp cmp =
  testProperty name $ \x ->
    let x' = bitCastOrCanonical x
        actual = fpOp x
        expected = floatOp x'
     in cmp actual expected

binOpComplianceWithFloat ::
  String ->
  (FP32 -> FP32 -> a) ->
  (Float -> Float -> b) ->
  (a -> b -> Bool) ->
  Test
binOpComplianceWithFloat name fpOp floatOp cmp =
  testProperty name $ \x y ->
    let x' = bitCastOrCanonical x
        y' = bitCastOrCanonical y
        actual = fpOp x y
        expected = floatOp x' y'
     in cmp actual expected

fpTests :: Test
fpTests =
  testGroup
    "FP"
    [ testGroup
        "bitcast"
        [ testGroup "WordN -> FP" $
            fp32ConversionTest $ \(x :: Word32) -> do
              let fp = bitCast x :: FP32
              let float = bitCast x :: Float
              assertBool "Must be the same FP" $ sameFP fp float,
          testGroup "FP -> WordN" $ do
            fp32ConversionTest $ \(x :: Word32) -> do
              let fp = bitCast x :: FP32
              let regulated =
                    if isNaN fp
                      then 0x7fc00000
                      else bitCastOrCanonical fp :: WordN 32
              let actual = bitCastOrCanonical (bitCast regulated :: FP32)
              actual @?= regulated
        ],
      testGroup
        "Eq"
        [ binOpComplianceWithFloat "==" (==) (==) (==),
          binOpComplianceWithFloat "/=" (/=) (/=) (==)
        ],
      testGroup
        "Ord"
        [ binOpComplianceWithFloat "<" (<) (<) (==),
          binOpComplianceWithFloat "<=" (<=) (<=) (==),
          binOpComplianceWithFloat ">" (>) (>) (==),
          binOpComplianceWithFloat ">=" (>=) (>=) (==)
        ],
      testGroup
        "Num"
        [ binOpComplianceWithFloat "+" (+) (+) sameFP,
          binOpComplianceWithFloat "-" (-) (-) sameFP,
          binOpComplianceWithFloat "*" (*) (*) sameFP,
          unaryOpComplianceWithFloat "negate" negate negate sameFP,
          unaryOpComplianceWithFloat "abs" abs abs sameFP,
          unaryOpComplianceWithFloat "signum" signum signum sameFP,
          testProperty "fromInteger" $ \x ->
            let fp = fromInteger x :: FP32
                float = fromInteger x :: Float
             in sameFP fp float
        ],
      testCase "Lift" $ do
        let x = bitCast (0x12345678 :: WordN 32) :: FP32
        $$([||x||]) @?= x,
      testGroup
        "Fractional"
        [ binOpComplianceWithFloat "/" (/) (/) sameFP,
          unaryOpComplianceWithFloat "recip" recip recip sameFP,
          testProperty "fromRational" $ \x ->
            let fp = fromRational x :: FP32
                float = fromRational x :: Float
             in sameFP fp float
        ],
      testGroup
        "Floating"
        [ -- Only the following operations are supported in SBV
          unaryOpComplianceWithFloat "sqrt" sqrt sqrt sameFP,
          binOpComplianceWithFloat "(**)" (**) (**) sameFP
        ],
      -- Real instantce is not compliant with Float.
      -- testGroup
      --   "Real"
      --   [ unaryOpComplianceWithFloat "toRational" toRational toRational (==)
      --   ]
      -- RealFrac instance is not compliant with Float.
      -- testGroup
      --   "RealFrac"
      --   [ unaryOpComplianceWithFloat
      --       "truncate"
      --       truncate
      --       truncate
      --       ((==) @Integer),
      --     unaryOpComplianceWithFloat "round" round round ((==) @Integer),
      --     unaryOpComplianceWithFloat "ceiling" ceiling ceiling ((==) @Integer),
      --     unaryOpComplianceWithFloat "floor" floor floor ((==) @Integer)
      --   ]
      testGroup
        "RealFloat"
        [ unaryOpComplianceWithFloat "floatRadix" floatRadix floatRadix (==),
          unaryOpComplianceWithFloat "floatDigits" floatDigits floatDigits (==),
          unaryOpComplianceWithFloat "floatRange" floatRange floatRange (==),
          -- decodeFloat is not compliant with Float
          -- unaryOpComplianceWithFloat "decodeFloat" decodeFloat decodeFloat (==)
          -- encodeFloat isn't tested
          -- exponent is not compliant with Float
          -- unaryOpComplianceWithFloat "exponent" exponent exponent (==)
          -- significand is not compliant with Float
          -- unaryOpComplianceWithFloat "significand" significand significand sameFP
          testProperty "scaleFloat" $ \i (x :: FP32) ->
            let x' = bitCastOrCanonical x :: Float
                actual = scaleFloat i x
                expected = scaleFloat i x'
             in sameFP actual expected,
          testProperty "isNaN" $ \(x :: FP32) ->
            let x' = bitCastOrCanonical x :: Float
             in isNaN x == isNaN x',
          unaryOpComplianceWithFloat "isInfinite" isInfinite isInfinite (==),
          unaryOpComplianceWithFloat
            "isDenormalized"
            isDenormalized
            isDenormalized
            (==),
          unaryOpComplianceWithFloat
            "isNegativeZero"
            isNegativeZero
            isNegativeZero
            (==),
          unaryOpComplianceWithFloat "isIEEE" isIEEE isIEEE (==) -- ,
          -- atan2 is not supported
          -- binOpComplianceWithFloat "atan2" atan2 atan2 sameFP
        ],
      testGroup
        "SymIEEEFPTraits"
        [ unaryOpComplianceWithFloat "symFpIsNaN" symFpIsNaN symFpIsNaN keyEq,
          unaryOpComplianceWithFloat
            "symFpIsPositive"
            symFpIsPositive
            symFpIsPositive
            keyEq,
          unaryOpComplianceWithFloat
            "symFpIsNegative"
            symFpIsNegative
            symFpIsNegative
            keyEq,
          unaryOpComplianceWithFloat
            "symFpIsPositiveInfinite"
            symFpIsPositiveInfinite
            symFpIsPositiveInfinite
            keyEq,
          unaryOpComplianceWithFloat
            "symFpIsNegativeInfinite"
            symFpIsNegativeInfinite
            symFpIsNegativeInfinite
            keyEq,
          unaryOpComplianceWithFloat
            "symFpIsInfinite"
            symFpIsInfinite
            symFpIsInfinite
            keyEq,
          unaryOpComplianceWithFloat
            "symFpIsPositiveZero"
            symFpIsPositiveZero
            symFpIsPositiveZero
            keyEq,
          unaryOpComplianceWithFloat
            "symFpIsNegativeZero"
            symFpIsNegativeZero
            symFpIsNegativeZero
            keyEq,
          unaryOpComplianceWithFloat "symFpIsZero" symFpIsZero symFpIsZero keyEq,
          unaryOpComplianceWithFloat
            "symFpIsNormal"
            symFpIsNormal
            symFpIsNormal
            keyEq,
          unaryOpComplianceWithFloat
            "symFpIsSubnormal"
            symFpIsSubnormal
            symFpIsSubnormal
            keyEq,
          unaryOpComplianceWithFloat
            "symFpIsPoint"
            symFpIsPoint
            symFpIsPoint
            keyEq
        ],
      testGroup
        "IEEEFPConstants"
        [ testCase "fpPositiveInfinite" $
            fpIsPositiveInfinite (fpPositiveInfinite :: FP32) @?= True,
          testCase "fpNegativeInfinite" $
            fpIsNegativeInfinite (fpNegativeInfinite :: FP32) @?= True,
          testCase "fpNaN" $
            fpIsNaN (fpNaN :: FP32) @?= True,
          testCase "fpPositiveZero" $
            fpIsPositiveZero (fpPositiveZero :: FP32) @?= True,
          testCase "fpNegativeZero" $
            fpIsNegativeZero (fpNegativeZero :: FP32) @?= True,
          testCase "fpMinNormalized" $
            fpMinNormalized @?= (1.5625e-2 :: FP 4 4),
          testCase "fpMaxNormalized" $
            fpMaxNormalized @?= (2.4e2 :: FP 4 4),
          testCase "fpMinSubnormal" $
            fpMinSubnormal @?= (1.953125e-3 :: FP 4 4),
          testCase "fpMaxSubnormal" $
            fpMaxSubnormal @?= (1.3671875e-2 :: FP 4 4)
        ],
      testGroup
        "IEEEFPOp"
        [ testCase "fpAbs" $ do
            SameFPObj (fpAbs (0 :: FP32)) @?= 0
            SameFPObj (fpAbs (fpNegativeZero :: FP32)) @?= 0
            SameFPObj (fpAbs (fpPositiveInfinite :: FP32))
              @?= fpPositiveInfinite
            SameFPObj (fpAbs (fpNegativeInfinite :: FP32))
              @?= fpPositiveInfinite
            SameFPObj (fpAbs (1 :: FP32)) @?= 1
            SameFPObj (fpAbs (-1 :: FP32)) @?= 1
            SameFPObj (fpAbs (fpNaN :: FP32)) @?= fpNaN,
          testCase "fpNeg" $ do
            SameFPObj (fpNeg (0 :: FP32)) @?= -0
            SameFPObj (fpNeg (fpNegativeZero :: FP32)) @?= 0
            SameFPObj (fpNeg (fpPositiveInfinite :: FP32))
              @?= fpNegativeInfinite
            SameFPObj (fpNeg (fpNegativeInfinite :: FP32))
              @?= fpPositiveInfinite
            SameFPObj (fpNeg (1 :: FP32)) @?= -1
            SameFPObj (fpNeg (-1 :: FP32)) @?= 1
            SameFPObj (fpNeg (fpNaN :: FP32)) @?= fpNaN,
          testGroup
            "fpRem"
            [ testCase "inf or nan / x" $ do
                let lhs =
                      [fpPositiveInfinite :: FP32, fpNegativeInfinite, fpNaN]
                let rhs =
                      [ 0,
                        -0,
                        1,
                        -1,
                        fpPositiveInfinite,
                        fpNegativeInfinite,
                        fpNaN
                      ]
                traverse_ (\(l, r) -> SameFPObj (fpRem l r) @?= fpNaN) $
                  zip lhs rhs,
              testCase "0 / neither 0 nor nan" $ do
                let lhs = [fpPositiveZero :: FP32, fpNegativeZero]
                let rhs =
                      [1, -1, fpPositiveInfinite, fpNegativeInfinite]
                traverse_ (\(l, r) -> SameFPObj (fpRem l r) @?= SameFPObj l) $
                  [(l, r) | l <- lhs, r <- rhs],
              testCase "0 / 0 or nan" $ do
                let lhs = [fpPositiveZero :: FP32, fpNegativeZero]
                let rhs = [fpPositiveZero, fpNegativeZero, fpNaN]
                traverse_ (\(l, r) -> SameFPObj (fpRem l r) @?= fpNaN) $
                  [(l, r) | l <- lhs, r <- rhs],
              testCase "normal" $ do
                SameFPObj (fpRem (5 :: FP32) 4) @?= 1
                SameFPObj (fpRem (6 :: FP32) 4) @?= -2
                SameFPObj (fpRem (7 :: FP32) 4) @?= -1
                SameFPObj (fpRem (8 :: FP32) 4) @?= 0
                SameFPObj (fpRem (9 :: FP32) 4) @?= 1
                SameFPObj (fpRem (10 :: FP32) 4) @?= 2
            ],
          testCase "fpMinimum" $ do
            SameFPObj (fpMinimum (0 :: FP32) 0) @?= 0
            SameFPObj (fpMinimum (0 :: FP32) (-0)) @?= -0
            SameFPObj (fpMinimum (-0 :: FP32) 0) @?= -0
            SameFPObj (fpMinimum (-0 :: FP32) (-0)) @?= -0
            SameFPObj (fpMinimum (fpNaN :: FP32) fpNaN) @?= fpNaN
            SameFPObj (fpMinimum (fpNaN :: FP32) 1) @?= fpNaN
            SameFPObj (fpMinimum (1 :: FP32) fpNaN) @?= fpNaN
            SameFPObj (fpMinimum (fpNaN :: FP32) fpPositiveInfinite) @?= fpNaN
            SameFPObj (fpMinimum (fpNegativeInfinite :: FP32) fpNaN) @?= fpNaN
            SameFPObj (fpMinimum (1 :: FP32) 2) @?= 1,
          testCase "fpMinimumNumber" $ do
            SameFPObj (fpMinimumNumber (0 :: FP32) 0) @?= 0
            SameFPObj (fpMinimumNumber (0 :: FP32) (-0)) @?= -0
            SameFPObj (fpMinimumNumber (-0 :: FP32) 0) @?= -0
            SameFPObj (fpMinimumNumber (-0 :: FP32) (-0)) @?= -0
            SameFPObj (fpMinimumNumber (fpNaN :: FP32) fpNaN) @?= fpNaN
            SameFPObj (fpMinimumNumber (fpNaN :: FP32) 1) @?= 1
            SameFPObj (fpMinimumNumber (1 :: FP32) fpNaN) @?= 1
            SameFPObj (fpMinimumNumber (fpNaN :: FP32) fpPositiveInfinite)
              @?= fpPositiveInfinite
            SameFPObj (fpMinimumNumber (fpNegativeInfinite :: FP32) fpNaN)
              @?= fpNegativeInfinite
            SameFPObj (fpMinimumNumber (1 :: FP32) 2) @?= 1,
          testCase "fpMaximum" $ do
            SameFPObj (fpMaximum (0 :: FP32) 0) @?= 0
            SameFPObj (fpMaximum (0 :: FP32) (-0)) @?= 0
            SameFPObj (fpMaximum (-0 :: FP32) 0) @?= 0
            SameFPObj (fpMaximum (-0 :: FP32) (-0)) @?= -0
            SameFPObj (fpMaximum (fpNaN :: FP32) fpNaN) @?= fpNaN
            SameFPObj (fpMaximum (fpNaN :: FP32) 1) @?= fpNaN
            SameFPObj (fpMaximum (1 :: FP32) fpNaN) @?= fpNaN
            SameFPObj (fpMaximum (fpNaN :: FP32) fpPositiveInfinite) @?= fpNaN
            SameFPObj (fpMaximum (fpNegativeInfinite :: FP32) fpNaN) @?= fpNaN
            SameFPObj (fpMaximum (1 :: FP32) 2) @?= 2,
          testCase "fpMaximumNumber" $ do
            SameFPObj (fpMaximumNumber (0 :: FP32) 0) @?= 0
            SameFPObj (fpMaximumNumber (0 :: FP32) (-0)) @?= 0
            SameFPObj (fpMaximumNumber (-0 :: FP32) 0) @?= 0
            SameFPObj (fpMaximumNumber (-0 :: FP32) (-0)) @?= -0
            SameFPObj (fpMaximumNumber (fpNaN :: FP32) fpNaN) @?= fpNaN
            SameFPObj (fpMaximumNumber (fpNaN :: FP32) 1) @?= 1
            SameFPObj (fpMaximumNumber (1 :: FP32) fpNaN) @?= 1
            SameFPObj (fpMaximumNumber (fpNaN :: FP32) fpPositiveInfinite)
              @?= fpPositiveInfinite
            SameFPObj (fpMaximumNumber (fpNegativeInfinite :: FP32) fpNaN)
              @?= fpNegativeInfinite
            SameFPObj (fpMaximumNumber (1 :: FP32) 2) @?= 2
        ],
      testGroup
        "IEEEFPRoundingOp"
        [ testCase "unop nan" $ do
            let op = [fpRoundToIntegral, fpSqrt]
            let roundingMode = [rne, rna, rtz, rtn, rtp]
            traverse_ (\(o, rd) -> SameFPObj (o rd fpNaN) @?= fpNaN) $
              [(op, rd) | op <- op, rd <- roundingMode],
          testCase "binop nan" $ do
            let op = [fpAdd, fpSub, fpMul, fpDiv]
            let roundingMode = [rne, rna, rtz, rtn, rtp]
            let operands =
                  [(fpNaN :: FP32, 1 :: FP32), (1, fpNaN), (fpNaN, fpNaN)]
            traverse_ (\(o, r, (a, b)) -> SameFPObj (o r a b) @?= fpNaN) $
              [(o, r, (a, b)) | o <- op, r <- roundingMode, (a, b) <- operands],
          testCase "ternop nan" $ do
            let op = [fpFMA]
            let roundingMode = [rne, rna, rtz, rtn, rtp]
            let operand = [fpNaN :: FP32, 1]
            let operands =
                  [ (a, b, c)
                  | a <- operand,
                    b <- operand,
                    c <- operand,
                    fpIsNaN a || fpIsNaN b || fpIsNaN c
                  ]
            traverse_ (\(o, r, (a, b, c)) -> SameFPObj (o r a b c) @?= fpNaN) $
              [ (o, r, (a, b, c))
              | o <- op,
                r <- roundingMode,
                (a, b, c) <- operands
              ],
          testCase "fpAdd" $ do
            let v = 60 :: FP 4 4
            fpAdd rne 2 v @?= 64
            fpAdd rna 2 v @?= 64
            fpAdd rne (-2) v @?= 56
            fpAdd rna (-2) v @?= 60
            fpAdd rtz 2 v @?= 60
            fpAdd rtn 2 v @?= 60
            fpAdd rtp 2 v @?= 64
            fpAdd rne (-2) (-v) @?= -64
            fpAdd rna (-2) (-v) @?= -64
            fpAdd rtz (-2) (-v) @?= -60
            fpAdd rtn (-2) (-v) @?= -64
            fpAdd rtp (-2) (-v) @?= -60
            fpAdd rne 1 v @?= 60
            fpAdd rna 1 v @?= 60
            fpAdd rtz 1 v @?= 60
            fpAdd rtn 1 v @?= 60
            fpAdd rtp 1 v @?= 64
            fpAdd rne (-1) (-v) @?= -60
            fpAdd rna (-1) (-v) @?= -60
            fpAdd rtz (-1) (-v) @?= -60
            fpAdd rtn (-1) (-v) @?= -64
            fpAdd rtp (-1) (-v) @?= -60
            fpAdd rne 3 v @?= 64
            fpAdd rna 3 v @?= 64
            fpAdd rtz 3 v @?= 60
            fpAdd rtn 3 v @?= 60
            fpAdd rtp 3 v @?= 64
            fpAdd rne (-3) (-v) @?= -64
            fpAdd rna (-3) (-v) @?= -64
            fpAdd rtz (-3) (-v) @?= -60
            fpAdd rtn (-3) (-v) @?= -64
            fpAdd rtp (-3) (-v) @?= -60
        ],
      testGroup "ConvertibleBound" $ do
        let test ::
              forall n eb sb.
              ( ValidFP eb sb,
                KnownNat n,
                1 <= n,
                SBV.BVIsNonZero n
              ) =>
              Test
            test =
              testGroup
                ( "n="
                    <> show (typeRep @n)
                    <> ",eb="
                    <> show (typeRep @eb)
                    <> ",sb="
                    <> show (typeRep @sb)
                )
                $ do
                  let tcase ::
                        forall (bv :: Nat -> Type) (sbvbv :: Nat -> Type).
                        ( ConvertibleBound bv,
                          Num (bv n),
                          SBV.SymVal (sbvbv n),
                          Num (SBV.SBV (sbvbv n)),
                          Typeable bv
                        ) =>
                        Test
                      tcase = testCase (show $ typeRep @bv) $ do
                        let lb =
                              convertibleLowerBound (0 :: bv n) RTP :: FP eb sb
                        let rb =
                              convertibleUpperBound (0 :: bv n) RTP :: FP eb sb
                        let lbad = prevFP lb
                        let rbad = nextFP rb
                        let sbvlb = conSBVTerm lb
                        let sbvrb = conSBVTerm rb
                        let sbvlbad = conSBVTerm lbad
                        let sbvrbad = conSBVTerm rbad
                        let sbvlbbv =
                              genericFPCast SBV.sRTP sbvlb :: SBV.SBV (sbvbv n)
                        let sbvrbbv =
                              genericFPCast SBV.sRTP sbvrb :: SBV.SBV (sbvbv n)
                        let sbvlbadbv =
                              genericFPCast
                                SBV.sRTP
                                sbvlbad ::
                                SBV.SBV (sbvbv n)
                        let sbvrbadbv =
                              genericFPCast
                                SBV.sRTP
                                sbvrbad ::
                                SBV.SBV (sbvbv n)
                        SBV.SatResult (Unsatisfiable {}) <-
                          SBV.sat (sbvlbbv SBV..== 1)
                        SBV.SatResult (Unsatisfiable {}) <-
                          SBV.sat (sbvrbbv SBV..== 1)
                        SBV.SatResult (Satisfiable {}) <-
                          SBV.sat (sbvlbadbv SBV..== 1)
                        SBV.SatResult (Satisfiable {}) <-
                          SBV.sat (sbvrbadbv SBV..== 1)
                        return ()
                  [tcase @IntN @SBV.IntN, tcase @WordN @SBV.WordN]
        [ test @12 @4 @16,
          test @12 @4 @14,
          test @12 @4 @13,
          test @12 @4 @12,
          test @12 @4 @11,
          test @12 @4 @10,
          test @12 @4 @9,
          test @12 @4 @2,
          test @10 @4 @16,
          test @10 @4 @12,
          test @10 @4 @11,
          test @10 @4 @10,
          test @10 @4 @9,
          test @10 @4 @8,
          test @10 @4 @7,
          test @10 @4 @2,
          test @9 @4 @16,
          test @9 @4 @11,
          test @9 @4 @10,
          test @9 @4 @9,
          test @9 @4 @8,
          test @9 @4 @7,
          test @9 @4 @6,
          test @9 @4 @2,
          test @8 @4 @16,
          test @8 @4 @10,
          test @8 @4 @9,
          test @8 @4 @8,
          test @8 @4 @7,
          test @8 @4 @6,
          test @8 @4 @5,
          test @8 @4 @2,
          test @7 @4 @16,
          test @7 @4 @9,
          test @7 @4 @8,
          test @7 @4 @7,
          test @7 @4 @6,
          test @7 @4 @5,
          test @7 @4 @3,
          test @7 @4 @2,
          test @6 @4 @16,
          test @6 @4 @8,
          test @6 @4 @7,
          test @6 @4 @6,
          test @6 @4 @5,
          test @6 @4 @3,
          test @6 @4 @2,
          test @5 @4 @16,
          test @5 @4 @7,
          test @5 @4 @6,
          test @5 @4 @5,
          test @5 @4 @3,
          test @5 @4 @2
          ],
      testGroup "IEEEFPConvertible" $ do
        let safeFromFPComplianceTest ::
              forall v.
              ( Show v,
                Eq v,
                Arbitrary v,
                SafeFromFP
                  NotRepresentableFPError
                  v
                  (FP 4 4)
                  FPRoundingMode
                  (Either NotRepresentableFPError)
              ) =>
              Test
            safeFromFPComplianceTest = testProperty "safeFromFP" $
              \(d :: v) (md :: FPRoundingMode) (v :: FP 4 4) -> do
                let s = safeFromFP md v :: Either NotRepresentableFPError v
                case s of
                  Left _ -> fromFPOr d md v == d
                  Right r -> fromFPOr d md v == r
        [ testGroup
            "AlgReal"
            [ testCase "fromFPOr" $ do
                fromFPOr (1 :: AlgReal) rne (fpPositiveZero :: FP 4 4) @?= 0
                fromFPOr (1 :: AlgReal) rne (fpNegativeZero :: FP 4 4) @?= 0
                fromFPOr (1 :: AlgReal) rne (fpPositiveInfinite :: FP 4 4) @?= 1
                fromFPOr (1 :: AlgReal) rne (fpNegativeInfinite :: FP 4 4) @?= 1
                fromFPOr (1 :: AlgReal) rne (fpNaN :: FP 4 4) @?= 1
                fromFPOr (1 :: AlgReal) rne (3.75 :: FP 4 4)
                  @?= AlgExactRational (15 % 4),
              safeFromFPComplianceTest @AlgReal,
              testCase "toFP" $ do
                toFP rne (AlgExactRational (15 % 4)) @?= (3.75 :: FP 4 4)
                toFP rne (AlgExactRational (15 % 8)) @?= (1.875 :: FP 4 4)
                toFP rne (AlgExactRational (17 % 8)) @?= (2 :: FP 4 4)
                toFP rna (AlgExactRational (17 % 8)) @?= (2.25 :: FP 4 4)
                toFP rtz (AlgExactRational (17 % 8)) @?= (2 :: FP 4 4)
                toFP rtp (AlgExactRational (17 % 8)) @?= (2.25 :: FP 4 4)
                toFP rtn (AlgExactRational (17 % 8)) @?= (2 :: FP 4 4)

                toFP rne (AlgExactRational (-(17 % 8))) @?= (-2 :: FP 4 4)
                toFP rna (AlgExactRational (-(17 % 8))) @?= (-2.25 :: FP 4 4)
                toFP rtz (AlgExactRational (-(17 % 8))) @?= (-2 :: FP 4 4)
                toFP rtp (AlgExactRational (-(17 % 8))) @?= (-2 :: FP 4 4)
                toFP rtn (AlgExactRational (-(17 % 8))) @?= (-2.25 :: FP 4 4)
            ],
          testGroup "FP" $ do
            let tcase name func = testCase name $ do
                  mapM_
                    ( \rm -> do
                        assertBool "+0" $
                          fpIsPositiveZero
                            (func rm (fpPositiveZero :: FP 6 6) :: FP 4 4)
                        assertBool "-0" $
                          fpIsNegativeZero (func rm (fpNegativeZero :: FP 6 6))
                        assertBool "+oo" $
                          fpIsPositiveInfinite
                            (func rm (fpPositiveInfinite :: FP 6 6))
                        assertBool "-oo" $
                          fpIsNegativeInfinite
                            (func rm (fpNegativeInfinite :: FP 6 6))
                        assertBool "nan" $
                          fpIsNaN (func rm (fpNaN :: FP 6 6))
                    )
                    [rna, rne, rtz, rtn, rtp]
                  let posfps = (/ 16) . fromIntegral <$> [48 .. 56] :: [FP 6 6]
                  let fps = (negate <$> posfps) <> posfps
                  let rnaExpected =
                        [3, 3, 3.25, 3.25, 3.25, 3.25, 3.5, 3.5, 3.5]
                  func rna <$> fps @?= (negate <$> rnaExpected) <> rnaExpected
                  let rneExpected =
                        [3, 3, 3, 3.25, 3.25, 3.25, 3.5, 3.5, 3.5]
                  func rne <$> fps @?= (negate <$> rneExpected) <> rneExpected
                  let rtzExpected =
                        [3, 3, 3, 3, 3.25, 3.25, 3.25, 3.25, 3.5]
                  func rtz <$> fps @?= (negate <$> rtzExpected) <> rtzExpected
                  let rtnExpected =
                        [3, 3, 3, 3, 3.25, 3.25, 3.25, 3.25, 3.5]
                  let rtpExpected =
                        [3, 3.25, 3.25, 3.25, 3.25, 3.5, 3.5, 3.5, 3.5]
                  func rtn <$> fps @?= (negate <$> rtpExpected) <> rtnExpected
                  func rtp <$> fps @?= (negate <$> rtnExpected) <> rtpExpected
            [ tcase "fromFPOr" (fromFPOr 2),
              tcase "toFP" toFP
              ],
          testGroup "Integrals" $ do
            let fps = (/ 4) . fromIntegral <$> [-7 .. 7] :: [FP 4 4]
            let rneFromExpected :: (Num a) => [a]
                rneFromExpected =
                  [-2, -2, -1, -1, -1, 0, 0, 0, 0, 0, 1, 1, 1, 2, 2]
                rnaFromExpected :: (Num a) => [a]
                rnaFromExpected =
                  [-2, -2, -1, -1, -1, -1, 0, 0, 0, 1, 1, 1, 1, 2, 2]
                rtpFromExpected :: (Num a) => [a]
                rtpFromExpected =
                  [-1, -1, -1, -1, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2]
                rtnFromExpected :: (Num a) => [a]
                rtnFromExpected =
                  [-2, -2, -2, -1, -1, -1, -1, 0, 0, 0, 0, 1, 1, 1, 1]
                rtzFromExpected :: (Num a) => [a]
                rtzFromExpected =
                  [-1, -1, -1, -1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1]
            let posints :: (Num a, Enum a) => [a]
                posints = [30 .. 40]
                ints :: (Num a, Enum a) => [a]
                ints = (negate <$> posints) <> posints
            let rnePosToFPExpected =
                  [30 :: FP 4 4, 32, 32, 32, 32, 36, 36, 36, 40, 40, 40]
            let rnaPosToFPExpected =
                  [30 :: FP 4 4, 32, 32, 32, 36, 36, 36, 36, 40, 40, 40]
            let rtzPosToFPExpected =
                  [30 :: FP 4 4, 30, 32, 32, 32, 32, 36, 36, 36, 36, 40]
            let rtnPosToFPExpected =
                  [30 :: FP 4 4, 30, 32, 32, 32, 32, 36, 36, 36, 36, 40]
            let rtpPosToFPExpected =
                  [30 :: FP 4 4, 32, 32, 36, 36, 36, 36, 40, 40, 40, 40]
            let rneToFPExpected =
                  fmap negate rnePosToFPExpected <> rnePosToFPExpected
            let rnaToFPExpected =
                  fmap negate rnaPosToFPExpected <> rnaPosToFPExpected
            let rtzToFPExpected =
                  fmap negate rtzPosToFPExpected <> rtzPosToFPExpected
            let rtnToFPExpected =
                  fmap negate rtpPosToFPExpected <> rtnPosToFPExpected
            let rtpToFPExpected =
                  fmap negate rtnPosToFPExpected <> rtpPosToFPExpected

            let boundTest0 ::
                  forall bv n eb sb.
                  ( ValidFP eb sb,
                    KnownNat n,
                    1 <= n,
                    ConvertibleBound bv,
                    IEEEFPConvertible (bv n) (FP eb sb) FPRoundingMode,
                    Eq (bv n),
                    Show (bv n)
                  ) =>
                  String ->
                  bv n ->
                  [bv n] ->
                  [bv n] ->
                  Test
                boundTest0 name d lbvs rbvs = testGroup name $ do
                  (rm, lbv, rbv) <- zip3 [rne, rna, rtz, rtn, rtp] lbvs rbvs
                  return $ testCase (show rm) $ do
                    let (lb :: FP eb sb) = convertibleLowerBound d rm
                    let (rb :: FP eb sb) = convertibleUpperBound d rm
                    let lbad = prevFP lb
                    let rbad = nextFP rb
                    fromFPOr d rm lbad @?= d
                    fromFPOr d rm rbad @?= d
                    fromFPOr d rm lb @?= lbv
                    fromFPOr d rm rb @?= rbv
            let boundTest ::
                  forall bv n eb sb.
                  ( ValidFP eb sb,
                    KnownNat n,
                    1 <= n,
                    ConvertibleBound bv,
                    IEEEFPConvertible (bv n) (FP eb sb) FPRoundingMode,
                    Eq (bv n),
                    Show (bv n)
                  ) =>
                  String ->
                  bv n ->
                  bv n ->
                  bv n ->
                  Test
                boundTest name d lbv rbv =
                  boundTest0 @bv @n @eb @sb
                    name
                    d
                    (replicate 5 lbv)
                    (replicate 5 rbv)

            [ testGroup
                "Integer"
                [ testCase "fromFPOr" $ do
                    fromFPOr (1 :: Integer) rne (fpPositiveZero :: FP 4 4) @?= 0
                    fromFPOr (1 :: Integer) rne (fpNegativeZero :: FP 4 4) @?= 0
                    fromFPOr (1 :: Integer) rne (fpPositiveInfinite :: FP 4 4)
                      @?= 1
                    fromFPOr (1 :: Integer) rne (fpNegativeInfinite :: FP 4 4)
                      @?= 1
                    fromFPOr (1 :: Integer) rne (fpNaN :: FP 4 4) @?= 1
                    fromFPOr (100 :: Integer) rne <$> fps @?= rneFromExpected
                    fromFPOr (100 :: Integer) rna <$> fps @?= rnaFromExpected
                    fromFPOr (100 :: Integer) rtp <$> fps @?= rtpFromExpected
                    fromFPOr (100 :: Integer) rtn <$> fps @?= rtnFromExpected
                    fromFPOr (100 :: Integer) rtz <$> fps @?= rtzFromExpected,
                  safeFromFPComplianceTest @Integer,
                  testCase "toFP" $ do
                    toFP rne (15 :: Integer) @?= (15 :: FP 4 4)
                    toFP rne <$> (ints :: [Integer]) @?= rneToFPExpected
                    toFP rna <$> (ints :: [Integer]) @?= rnaToFPExpected
                    toFP rtz <$> (ints :: [Integer]) @?= rtzToFPExpected
                    toFP rtn <$> (ints :: [Integer]) @?= rtnToFPExpected
                    toFP rtp <$> (ints :: [Integer]) @?= rtpToFPExpected
                ],
              testGroup
                "IntN"
                [ testCase "fromFPOr" $ do
                    fromFPOr (1 :: IntN 32) rne (fpPositiveZero :: FP 4 4) @?= 0
                    fromFPOr (1 :: IntN 32) rne (fpNegativeZero :: FP 4 4) @?= 0
                    fromFPOr (1 :: IntN 32) rne (fpPositiveInfinite :: FP 4 4)
                      @?= 1
                    fromFPOr (1 :: IntN 32) rne (fpNegativeInfinite :: FP 4 4)
                      @?= 1
                    fromFPOr (1 :: IntN 32) rne (fpNaN :: FP 4 4) @?= 1
                    fromFPOr (100 :: IntN 32) rne <$> fps @?= rneFromExpected
                    fromFPOr (100 :: IntN 32) rna <$> fps @?= rnaFromExpected
                    fromFPOr (100 :: IntN 32) rtp <$> fps @?= rtpFromExpected
                    fromFPOr (100 :: IntN 32) rtn <$> fps @?= rtnFromExpected
                    fromFPOr (100 :: IntN 32) rtz <$> fps @?= rtzFromExpected
                    fromFPOr (0 :: IntN 3) rne (-3.5 :: FP 4 4) @?= -4
                    fromFPOr (0 :: IntN 3) rne (3.5 :: FP 4 4) @?= 0
                    fromFPOr (0 :: IntN 3) rna (-3.5 :: FP 4 4) @?= -4
                    fromFPOr (0 :: IntN 3) rna (3.5 :: FP 4 4) @?= 0
                    fromFPOr (0 :: IntN 3) rne (-4.5 :: FP 4 4) @?= -4
                    fromFPOr (0 :: IntN 3) rne (4.5 :: FP 4 4) @?= 0
                    fromFPOr (0 :: IntN 3) rna (-4.5 :: FP 4 4) @?= 0
                    fromFPOr (0 :: IntN 3) rna (4.5 :: FP 4 4) @?= 0,
                  safeFromFPComplianceTest @(IntN 3),
                  testGroup "ConvertibleBound" $ do
                    [ boundTest0 @IntN @12 @4 @16
                        "ebn<n-1&&sb>n-1,12/4/16"
                        100
                        [-256, -256, -255, -256, -255]
                        [256, 256, 255, 255, 256],
                      boundTest0 @IntN @12 @4 @12
                        "ebn<n-1&&sb>n-1,12/4/12"
                        100
                        [-256, -256, -255, -256, -255]
                        [256, 256, 255, 255, 256],
                      boundTest0 @IntN @12 @4 @11
                        "ebn<n-1&&sb==n-1,12/4/11"
                        100
                        [-256, -256, -255, -256, -255]
                        [256, 256, 255, 255, 256],
                      boundTest0 @IntN @12 @4 @10
                        "ebn<n-1&&sb<n-1,12/4/10"
                        100
                        [-256, -256, -255, -256, -255]
                        [256, 256, 255, 255, 256],
                      boundTest0 @IntN @12 @4 @9
                        "ebn<n-1&&sb<n-1,12/4/9"
                        100
                        [-256, -256, -255, -256, -255]
                        [256, 256, 255, 255, 256],
                      boundTest @IntN @12 @4 @2
                        "ebn<n-1&&sb<n-1,12/4/2"
                        100
                        (-192)
                        192,
                      boundTest0 @IntN @10 @4 @16
                        "ebn<n-1&&sb>n-1,10/4/16"
                        100
                        [-256, -256, -255, -256, -255]
                        [256, 256, 255, 255, 256],
                      boundTest0 @IntN @10 @4 @11
                        "ebn<n-1&&sb>n-1,10/4/10"
                        100
                        [-256, -256, -255, -256, -255]
                        [256, 256, 255, 255, 256],
                      boundTest0 @IntN @10 @4 @10
                        "ebn<n-1&&sb==n-1,10/4/9"
                        100
                        [-256, -256, -255, -256, -255]
                        [256, 256, 255, 255, 256],
                      boundTest @IntN @10 @4 @8
                        "ebn<n-1&&sb<n-1,10/4/8"
                        100
                        (-255)
                        255,
                      boundTest @IntN @10 @4 @7
                        "ebn<n-1&&sb<n-1,10/4/7"
                        100
                        (-254)
                        254,
                      boundTest @IntN @10 @4 @2
                        "ebn<n-1&&sb<n-1,10/4/2"
                        100
                        (-192)
                        192,
                      boundTest0 @IntN @9 @4 @16
                        "ebn==n-1&&sb>n-1,9/4/16"
                        100
                        [-256, -256, -255, -256, -255]
                        [255, 255, 255, 255, 255],
                      boundTest0 @IntN @9 @4 @9
                        "ebn==n-1&&sb>n-1,9/4/9"
                        100
                        [-256, -256, -255, -256, -255]
                        [255, 255, 255, 255, 255],
                      boundTest @IntN @9 @4 @8
                        "ebn==n-1&&sb==n-1,9/4/8"
                        100
                        (-255)
                        255,
                      boundTest @IntN @9 @4 @7
                        "ebn==n-1&&sb<n-1,9/4/7"
                        100
                        (-254)
                        254,
                      boundTest @IntN @9 @4 @2
                        "ebn==n-1&&sb<n-1,9/4/2"
                        100
                        (-192)
                        192,
                      boundTest @IntN @8 @4 @16
                        "ebn>n-1&&sb>n-1,8/4/16"
                        100
                        (-128)
                        127,
                      boundTest @IntN @8 @4 @8
                        "ebn>n-1&&sb>n-1,8/4/8"
                        100
                        (-128)
                        127,
                      boundTest @IntN @8 @4 @7
                        "ebn>n-1&&sb==n-1,8/4/7"
                        100
                        (-128)
                        127,
                      boundTest @IntN @8 @4 @6
                        "ebn>n-1&&sb<n-1,8/4/6"
                        100
                        (-128)
                        126,
                      boundTest @IntN @8 @4 @5
                        "ebn>n-1&&sb<n-1,8/4/5"
                        100
                        (-128)
                        124,
                      boundTest @IntN @8 @4 @2
                        "ebn>n-1&&sb<n-1,8/4/2"
                        100
                        (-128)
                        96,
                      boundTest @IntN @7 @4 @16
                        "ebn>n-1&&sb>n-1,7/4/16"
                        100
                        (-64)
                        63,
                      boundTest @IntN @7 @4 @7
                        "ebn>n-1&&sb>n-1,7/4/7"
                        100
                        (-64)
                        63,
                      boundTest @IntN @7 @4 @6
                        "ebn>n-1&&sb==n-1,7/4/6"
                        100
                        (-64)
                        63,
                      boundTest @IntN @7 @4 @5
                        "ebn>n-1&&sb<n-1,7/4/5"
                        100
                        (-64)
                        62,
                      boundTest @IntN @7 @4 @4
                        "ebn>n-1&&sb<n-1,7/4/4"
                        100
                        (-64)
                        60,
                      boundTest @IntN @7 @4 @2
                        "ebn>n-1&&sb<n-1,7/4/2"
                        100
                        (-64)
                        48,
                      boundTest @IntN @5 @4 @16
                        "ebn>n-1&&sb>n-1,5/4/16"
                        100
                        (-16)
                        15,
                      boundTest @IntN @5 @4 @5
                        "ebn>n-1&&sb>n-1,5/4/5"
                        100
                        (-16)
                        15,
                      boundTest @IntN @5 @4 @4
                        "ebn>n-1&&sb==n-1,5/4/4"
                        100
                        (-16)
                        15,
                      boundTest @IntN @5 @4 @3
                        "ebn>n-1&&sb<n-1,5/4/3"
                        100
                        (-16)
                        14,
                      boundTest @IntN @5 @4 @2
                        "ebn>n-1&&sb<n-1,5/4/2"
                        100
                        (-16)
                        12
                      ],
                  testCase "toFP" $ do
                    toFP rne (15 :: IntN 32) @?= (15 :: FP 4 4)
                    toFP rne <$> (ints :: [IntN 32]) @?= rneToFPExpected
                    toFP rna <$> (ints :: [IntN 32]) @?= rnaToFPExpected
                    toFP rtz <$> (ints :: [IntN 32]) @?= rtzToFPExpected
                    toFP rtn <$> (ints :: [IntN 32]) @?= rtnToFPExpected
                    toFP rtp <$> (ints :: [IntN 32]) @?= rtpToFPExpected
                ],
              testGroup
                "WordN"
                [ testCase "fromFPOr" $ do
                    fromFPOr (1 :: WordN 32) rne (fpPositiveZero :: FP 4 4)
                      @?= 0
                    fromFPOr (1 :: WordN 32) rne (fpNegativeZero :: FP 4 4)
                      @?= 0
                    fromFPOr (1 :: WordN 32) rne (fpPositiveInfinite :: FP 4 4)
                      @?= 1
                    fromFPOr (1 :: WordN 32) rne (fpNegativeInfinite :: FP 4 4)
                      @?= 1
                    fromFPOr (1 :: WordN 32) rne (fpNaN :: FP 4 4) @?= 1
                    let m = fmap (min 100)
                    fromFPOr (100 :: WordN 32) rne <$> fps @?= m rneFromExpected
                    fromFPOr (100 :: WordN 32) rna <$> fps @?= m rnaFromExpected
                    fromFPOr (100 :: WordN 32) rtp <$> fps @?= m rtpFromExpected
                    fromFPOr (100 :: WordN 32) rtn <$> fps @?= m rtnFromExpected
                    fromFPOr (100 :: WordN 32) rtz
                      <$> fps
                        @?= m rtzFromExpected
                    fromFPOr (4 :: WordN 3) rne (-0.5 :: FP 4 4) @?= 0
                    fromFPOr (4 :: WordN 3) rne (0.5 :: FP 4 4) @?= 0
                    fromFPOr (4 :: WordN 3) rna (-0.5 :: FP 4 4) @?= 4
                    fromFPOr (4 :: WordN 3) rna (0.5 :: FP 4 4) @?= 1
                    fromFPOr (4 :: WordN 3) rne (6.5 :: FP 4 4) @?= 6
                    fromFPOr (4 :: WordN 3) rne (7.5 :: FP 4 4) @?= 4
                    fromFPOr (4 :: WordN 3) rna (6.5 :: FP 4 4) @?= 7
                    fromFPOr (4 :: WordN 3) rna (7.5 :: FP 4 4) @?= 4,
                  safeFromFPComplianceTest @(WordN 3),
                  testGroup "ConvertibleBound" $ do
                    [ boundTest0 @WordN @12 @4 @16
                        "ebn<n&&sb>n,12/4/16"
                        100
                        [0, 0, 0, 0, 0]
                        [256, 256, 255, 255, 256],
                      boundTest0 @WordN @12 @4 @13
                        "ebn<n&&sb>n,12/4/13"
                        100
                        [0, 0, 0, 0, 0]
                        [256, 256, 255, 255, 256],
                      boundTest0 @WordN @12 @4 @12
                        "ebn<n&&sb==n,12/4/12"
                        100
                        [0, 0, 0, 0, 0]
                        [256, 256, 255, 255, 256],
                      boundTest0 @WordN @12 @4 @11
                        "ebn<n&&sb<n,12/4/11"
                        100
                        [0, 0, 0, 0, 0]
                        [256, 256, 255, 255, 256],
                      boundTest @WordN @12 @4 @2 "ebn<n&&sb<n,12/4/2" 100 0 192,
                      boundTest0 @WordN @9 @4 @16
                        "ebn<n&&sb>n,9/4/16"
                        100
                        [0, 0, 0, 0, 0]
                        [256, 256, 255, 255, 256],
                      boundTest0 @WordN @9 @4 @10
                        "ebn<n&&sb>n,9/4/10"
                        100
                        [0, 0, 0, 0, 0]
                        [256, 256, 255, 255, 256],
                      boundTest0 @WordN @9 @4 @9
                        "ebn<n&&sb==n,9/4/9"
                        100
                        [0, 0, 0, 0, 0]
                        [256, 256, 255, 255, 256],
                      boundTest @WordN @9 @4 @8 "ebn<n&&sb<n,9/4/8" 100 0 255,
                      boundTest @WordN @9 @4 @2 "ebn<n&&sb<n,9/4/2" 100 0 192,
                      boundTest @WordN @8 @4 @16
                        "ebn==n&&sb>n,8/4/16"
                        100
                        0
                        255,
                      boundTest @WordN @8 @4 @9 "ebn==n&&sb>n,8/4/16" 100 0 255,
                      boundTest @WordN @8 @4 @9 "ebn==n&&sb>n,8/4/9" 100 0 255,
                      boundTest @WordN @8 @4 @8 "ebn==n&&sb==n,8/4/8" 100 0 255,
                      boundTest @WordN @8 @4 @7 "ebn==n&&sb<n,8/4/7" 100 0 254,
                      boundTest @WordN @8 @4 @2 "ebn==n&&sb<n,8/4/2" 100 0 192,
                      boundTest @WordN @7 @4 @16 "ebn>n&&sb>n,7/4/16" 100 0 127,
                      boundTest @WordN @7 @4 @8 "ebn>n&&sb>n,7/4/8" 100 0 127,
                      boundTest @WordN @7 @4 @7 "ebn>n&&sb==n,7/4/7" 100 0 127,
                      boundTest @WordN @7 @4 @6 "ebn>n&&sb<n,7/4/6" 100 0 126,
                      boundTest @WordN @7 @4 @2 "ebn>n&&sb<n,7/4/2" 100 0 96,
                      boundTest @WordN @5 @4 @16 "ebn>n&&sb>n,7/4/16" 100 0 31,
                      boundTest @WordN @5 @4 @6 "ebn>n&&sb>n,5/4/6" 100 0 31,
                      boundTest @WordN @5 @4 @5 "ebn>n&&sb==n,5/4/5" 100 0 31,
                      boundTest @WordN @5 @4 @4 "ebn>n&&sb<n,5/4/4" 100 0 30,
                      boundTest @WordN @5 @4 @2 "ebn>n&&sb<n,5/4/2" 100 0 24
                      ],
                  testCase "toFP" $ do
                    toFP rne (15 :: WordN 32) @?= (15 :: FP 4 4)
                    toFP rne <$> (posints :: [WordN 32]) @?= rnePosToFPExpected
                    toFP rna <$> (posints :: [WordN 32]) @?= rnaPosToFPExpected
                    toFP rtz <$> (posints :: [WordN 32]) @?= rtzPosToFPExpected
                    toFP rtn <$> (posints :: [WordN 32]) @?= rtnPosToFPExpected
                    toFP rtp <$> (posints :: [WordN 32]) @?= rtpPosToFPExpected
                ]
              ]
          ],
      testProperty "Serialize" $ \(x :: FP 8 24) ->
        ioProperty $
          if isNaN x
            then case (decode . encode) x of
              Right (v :: FP 8 24) -> assertBool "Should be NaN" $ fpIsNaN v
              Left err -> fail err
            else Right x @?= (decode . encode) x
    ]

newtype SameFPObj = SameFPObj FP32 deriving newtype (Show, Num, IEEEFPConstants)

instance Eq SameFPObj where
  SameFPObj a == SameFPObj b
    | a == 0 && b == 0 =
        fpIsPositiveZero a == fpIsPositiveZero b
  SameFPObj a == SameFPObj b | fpIsNaN a && fpIsNaN b = True
  SameFPObj a == SameFPObj b = a == b
