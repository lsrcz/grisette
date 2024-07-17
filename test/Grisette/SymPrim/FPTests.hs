{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module Grisette.SymPrim.FPTests (fpTests) where

import Data.Word (Word32, Word64)
import Grisette (WordN)
import Grisette.Internal.Core.Data.Class.BitCast (BitCast (bitCast))
import Grisette.Internal.Core.Data.Class.IEEEFP
  ( IEEEConstants
      ( fpNaN,
        fpNegativeInfinite,
        fpNegativeZero,
        fpPositiveInfinite,
        fpPositiveZero
      ),
    SymIEEEFPTraits
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
    fpIsNaN,
    fpIsNegativeInfinite,
    fpIsNegativeZero,
    fpIsPositiveInfinite,
    fpIsPositiveZero,
  )
import Grisette.Internal.Core.Data.Class.SafeBitCast (bitCastOrCanonical)
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
  | otherwise =
      (uncurry encodeFloat (decodeFloat x) :: b) == y

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
        "IEEEConstants"
        [ testCase "fpPositiveInfinite" $
            fpIsPositiveInfinite (fpPositiveInfinite :: FP32) @?= True,
          testCase "fpNegativeInfinite" $
            fpIsNegativeInfinite (fpNegativeInfinite :: FP32) @?= True,
          testCase "fpNaN" $
            fpIsNaN (fpNaN :: FP32) @?= True,
          testCase "fpPositiveZero" $
            fpIsPositiveZero (fpPositiveZero :: FP32) @?= True,
          testCase "fpNegativeZero" $
            fpIsNegativeZero (fpNegativeZero :: FP32) @?= True
        ]
    ]
