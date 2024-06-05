{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.SymPrim.FPTests (fpTests) where

import Data.Array.ST (MArray, STUArray, newArray, readArray)
import Data.Array.Unsafe (castSTUArray)
import Data.Word (Word32, Word64)
import GHC.ST (ST, runST)
import Grisette.Internal.SymPrim.FP
  ( FP32,
    FP64,
    doubleAsFP64,
    floatAsFP32,
    fp32AsFloat,
    fp64AsDouble,
    fpAsWordN,
    wordNAsFP,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (assertBool, (@?=))
import Test.QuickCheck (ioProperty)

wordToFloat :: Word32 -> Float
wordToFloat x = runST (cast x)

floatToWord :: Float -> Word32
floatToWord x = runST (cast x)

wordToDouble :: Word64 -> Double
wordToDouble x = runST (cast x)

{-# INLINE cast #-}
cast ::
  ( MArray (STUArray s) a (ST s),
    MArray (STUArray s) b (ST s)
  ) =>
  a ->
  ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0

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

fpTests :: Test
fpTests =
  testGroup
    "FP"
    [ testGroup
        "bitcast"
        [ testGroup "wordNAsFP" $
            fp32ConversionTest $ \(x :: Word32) -> do
              let fp = wordNAsFP $ fromIntegral x :: FP32
              let float = wordToFloat x
              assertBool "Must be the same FP" $ sameFP fp float,
          testGroup "fpAsWordN" $ do
            fp32ConversionTest $ \(x :: Word32) -> do
              let fp = wordToFloat x
              let expected = if isNaN fp then 0x7fc00000 else floatToWord fp
              let actual =
                    fpAsWordN (wordNAsFP $ fromIntegral expected :: FP32)
              fromIntegral actual @?= expected,
          testGroup "fp32AsFloat" $ do
            fp32ConversionTest $ \(x :: Word32) -> do
              let expected = wordToFloat x
              let actual = fp32AsFloat (wordNAsFP $ fromIntegral x)
              assertBool "Must be the same FP" $ sameFP actual expected,
          testGroup "floatAsFp32" $ do
            fp32ConversionTest $ \(x :: Word32) -> do
              let expected = floatAsFP32 $ wordToFloat x
              let actual = wordNAsFP (fromIntegral x) :: FP32
              assertBool "Must be the same FP" $ sameFP actual expected,
          testGroup "fp64AsDouble" $ do
            fp64ConversionTest $ \(x :: Word64) -> do
              let expected = wordToDouble x
              let actual = fp64AsDouble (wordNAsFP $ fromIntegral x)
              assertBool "Must be the same FP" $ sameFP actual expected,
          testGroup "doubleAsFp64" $ do
            fp64ConversionTest $ \(x :: Word64) -> do
              let expected = doubleAsFP64 $ wordToDouble x
              let actual = wordNAsFP (fromIntegral x) :: FP64
              assertBool "Must be the same FP" $ sameFP actual expected
        ]
    ]
