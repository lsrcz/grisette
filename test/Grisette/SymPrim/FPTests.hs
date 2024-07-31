{-# LANGUAGE DataKinds #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Grisette.SymPrim.FPTests (fpTests) where

import Data.Foldable (traverse_)
import Data.Word (Word32, Word64)
import Grisette
  ( FP,
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
    WordN,
    bitCastOrCanonical, IEEEFPConstants (fpMinNormalized, fpMaxNormalized, fpMinSubnormal, fpMaxSubnormal),
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
    fpIsNaN,
    fpIsNegativeInfinite,
    fpIsNegativeZero,
    fpIsPositiveInfinite,
    fpIsPositiveZero,
  )
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
import Grisette.Internal.SymPrim.FP (FP32)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (assertBool, (@?=))
import Test.QuickCheck (ioProperty)

sameFP :: forall a b. (RealFloat a, RealFloat b) => a -> b -> Bool
sameFP x y
  | isNaN x && isNaN y = True
  | isInfinite x && isInfinite y =
      (x < 0 && y < 0) || (x > 0 && y > 0)
  -- | GHC's floating point support doesn't conform to IEEE754.
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
        [ unaryOpComplianceWithFloat "symFpIsNaN" symFpIsNaN symFpIsNaN (==),
          unaryOpComplianceWithFloat
            "symFpIsPositive"
            symFpIsPositive
            symFpIsPositive
            (==),
          unaryOpComplianceWithFloat
            "symFpIsNegative"
            symFpIsNegative
            symFpIsNegative
            (==),
          unaryOpComplianceWithFloat
            "symFpIsPositiveInfinite"
            symFpIsPositiveInfinite
            symFpIsPositiveInfinite
            (==),
          unaryOpComplianceWithFloat
            "symFpIsNegativeInfinite"
            symFpIsNegativeInfinite
            symFpIsNegativeInfinite
            (==),
          unaryOpComplianceWithFloat
            "symFpIsInfinite"
            symFpIsInfinite
            symFpIsInfinite
            (==),
          unaryOpComplianceWithFloat
            "symFpIsPositiveZero"
            symFpIsPositiveZero
            symFpIsPositiveZero
            (==),
          unaryOpComplianceWithFloat
            "symFpIsNegativeZero"
            symFpIsNegativeZero
            symFpIsNegativeZero
            (==),
          unaryOpComplianceWithFloat "symFpIsZero" symFpIsZero symFpIsZero (==),
          unaryOpComplianceWithFloat
            "symFpIsNormal"
            symFpIsNormal
            symFpIsNormal
            (==),
          unaryOpComplianceWithFloat
            "symFpIsSubnormal"
            symFpIsSubnormal
            symFpIsSubnormal
            (==),
          unaryOpComplianceWithFloat
            "symFpIsPoint"
            symFpIsPoint
            symFpIsPoint
            (==)
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
        ]
    ]

newtype SameFPObj = SameFPObj FP32 deriving newtype (Show, Num, IEEEFPConstants)

instance Eq SameFPObj where
  SameFPObj a == SameFPObj b
    | a == 0 && b == 0 =
        fpIsPositiveZero a == fpIsPositiveZero b
  SameFPObj a == SameFPObj b | fpIsNaN a && fpIsNaN b = True
  SameFPObj a == SameFPObj b = a == b
