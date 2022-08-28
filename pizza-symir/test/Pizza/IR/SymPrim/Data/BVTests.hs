{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pizza.IR.SymPrim.Data.BVTests where

import Control.Monad
import Data.Bits
import Data.Int
import Data.Proxy
import Data.Word
import Pizza.Core.Data.Class.BitVector
import Pizza.IR.SymPrim.Data.BV
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding ((.&.))

wordUnaryConform :: HasCallStack => (WordN 8 -> WordN 8) -> (Word8 -> Word8) -> Word8 -> Assertion
wordUnaryConform f g x = unWordN (f (fromIntegral x)) @=? toInteger (g x)

wordUnaryNonNegIntConform :: HasCallStack => (Int -> WordN 8) -> (Int -> Word8) -> Int -> Assertion
wordUnaryNonNegIntConform f g y = when (y >= 0) $ unWordN (f y) @=? toInteger (g y)

wordBinIntConform :: HasCallStack => (WordN 8 -> Int -> WordN 8) -> (Word8 -> Int -> Word8) -> Word8 -> Int -> Assertion
wordBinIntConform f g x y = unWordN (f (fromIntegral x) y) @=? toInteger (g x y)

wordBinNonNegIntConform :: HasCallStack => (WordN 8 -> Int -> WordN 8) -> (Word8 -> Int -> Word8) -> Word8 -> Int -> Assertion
wordBinNonNegIntConform f g x y = when (y >= 0) $ unWordN (f (fromIntegral x) y) @=? toInteger (g x y)

wordBinConform :: HasCallStack => (WordN 8 -> WordN 8 -> WordN 8) -> (Word8 -> Word8 -> Word8) -> Word8 -> Word8 -> Assertion
wordBinConform f g x y = unWordN (f (fromIntegral x) (fromIntegral y)) @=? toInteger (g x y)

intN8eqint8 :: IntN 8 -> Int8 -> Assertion
intN8eqint8 (IntN v) i
  | v < 0 = assertFailure "Bad IntN"
  | v <= 127 = v @=? fromIntegral i
  | v == 128 = i @=? -128
  | otherwise = 256 - v @=? fromIntegral (-i)

intUnaryConform :: (IntN 8 -> IntN 8) -> (Int8 -> Int8) -> Int8 -> Assertion
intUnaryConform f g x = intN8eqint8 (f (fromIntegral x)) (g x)

intUnaryNonNegIntConform :: (Int -> IntN 8) -> (Int -> Int8) -> Int -> Assertion
intUnaryNonNegIntConform f g y = when (y >= 0) $ intN8eqint8 (f y) (g y)

intBinIntConform :: (IntN 8 -> Int -> IntN 8) -> (Int8 -> Int -> Int8) -> Int8 -> Int -> Assertion
intBinIntConform f g x y = intN8eqint8 (f (fromIntegral x) y) (g x y)

intBinNonNegIntConform :: (IntN 8 -> Int -> IntN 8) -> (Int8 -> Int -> Int8) -> Int8 -> Int -> Assertion
intBinNonNegIntConform f g x y = when (y >= 0) $ intN8eqint8 (f (fromIntegral x) y) (g x y)

intBinConform :: (IntN 8 -> IntN 8 -> IntN 8) -> (Int8 -> Int8 -> Int8) -> Int8 -> Int8 -> Assertion
intBinConform f g x y = intN8eqint8 (f (fromIntegral x) (fromIntegral y)) (g x y)

bvTests :: TestTree
bvTests =
  testGroup
    "BVTests"
    [ testGroup
        "WordN 8 conform to Word8 for Bits instances"
        [ testProperty "(.&.)" $ \x y -> ioProperty $ wordBinConform (.&.) (.&.) x y,
          testProperty "(.|.)" $ \x y -> ioProperty $ wordBinConform (.|.) (.|.) x y,
          testProperty "xor" $ \x y -> ioProperty $ wordBinConform xor xor x y,
          testProperty "complement" $ ioProperty . wordUnaryConform complement complement,
          testProperty "shift" $ \x y -> ioProperty $ wordBinIntConform shift shift x y,
          testProperty "rotate" $ \x y -> ioProperty $ wordBinIntConform rotate rotate x y,
          testCase "zeroBits" $ (zeroBits :: WordN 8) @=? 0,
          testProperty "bit" $ ioProperty . wordUnaryNonNegIntConform bit bit,
          testProperty "setBit" $ \x y -> ioProperty $ wordBinNonNegIntConform setBit setBit x y,
          testProperty "clearBit" $ \x y -> ioProperty $ wordBinNonNegIntConform clearBit clearBit x y,
          testProperty "complementBit" $ \x y -> ioProperty $ wordBinNonNegIntConform complementBit complementBit x y,
          testProperty "testBit" $ \(x :: Word8) i -> i < 0 || testBit x i == testBit (fromIntegral x :: WordN 8) i,
          testCase "bitSizeMaybe" $ bitSizeMaybe (0 :: WordN 8) @=? Just 8,
          testCase "isSigned" $ isSigned (0 :: WordN 8) @=? False,
          testProperty "shiftL" $ \x y -> ioProperty $ wordBinNonNegIntConform shiftL shiftL x y,
          testProperty "shiftR" $ \x y -> ioProperty $ wordBinNonNegIntConform shiftR shiftR x y,
          testProperty "rotateL" $ \x y -> ioProperty $ wordBinNonNegIntConform rotateL rotateL x y,
          testProperty "rotateR" $ \x y -> ioProperty $ wordBinNonNegIntConform rotateR rotateR x y,
          testProperty "popCount" $ ioProperty . \(x :: Word8) -> popCount x @=? popCount (fromIntegral x :: WordN 8)
        ],
      testGroup
        "WordN 8 conform to Word8 for Num instances"
        [ testProperty "(+)" $ \x y -> ioProperty $ wordBinConform (+) (+) x y,
          testProperty "(*)" $ \x y -> ioProperty $ wordBinConform (*) (*) x y,
          testProperty "(-)" $ \x y -> ioProperty $ wordBinConform (-) (-) x y,
          testProperty "negate" $ ioProperty . wordUnaryConform negate negate,
          testProperty "abs" $ ioProperty . wordUnaryConform abs abs,
          testProperty "signum" $ ioProperty . wordUnaryConform signum signum,
          testProperty "fromInteger" $
            ioProperty . \(x :: Integer) ->
              unWordN (fromInteger x :: WordN 8) @=? toInteger (fromInteger x :: Word8)
        ],
      testGroup
        "WordN 8 conform to Word8 for Ord instances"
        [ testProperty "(<=)" $ \(x :: Word8) y -> ioProperty $ x <= y @=? (fromIntegral x :: WordN 8) <= (fromIntegral y :: WordN 8)
        ],
      testGroup
        "IntN 8 conform to Int8 for Bits instances"
        [ testProperty "(.&.)" $ \x y -> ioProperty $ intBinConform (.&.) (.&.) x y,
          testProperty "(.|.)" $ \x y -> ioProperty $ intBinConform (.|.) (.|.) x y,
          testProperty "xor" $ \x y -> ioProperty $ intBinConform xor xor x y,
          testProperty "complement" $ ioProperty . intUnaryConform complement complement,
          testProperty "shift" $ \x y -> ioProperty $ intBinIntConform shift shift x y,
          testProperty "rotate" $ \x y -> ioProperty $ intBinIntConform rotate rotate x y,
          testCase "zeroBits" $ (zeroBits :: IntN 8) @=? 0,
          testProperty "bit" $ ioProperty . intUnaryNonNegIntConform bit bit,
          testProperty "setBit" $ \x y -> ioProperty $ intBinNonNegIntConform setBit setBit x y,
          testProperty "clearBit" $ \x y -> ioProperty $ intBinNonNegIntConform clearBit clearBit x y,
          testProperty "complementBit" $ \x y -> ioProperty $ intBinNonNegIntConform complementBit complementBit x y,
          testProperty "testBit" $ \(x :: Int8) i -> i < 0 || testBit x i == testBit (fromIntegral x :: IntN 8) i,
          testCase "bitSizeMaybe" $ bitSizeMaybe (0 :: IntN 8) @=? Just 8,
          testCase "isSigned" $ isSigned (0 :: IntN 8) @=? True,
          testProperty "shiftL" $ \x y -> ioProperty $ intBinNonNegIntConform shiftL shiftL x y,
          testProperty "shiftR" $ \x y -> ioProperty $ intBinNonNegIntConform shiftR shiftR x y,
          testProperty "rotateL" $ \x y -> ioProperty $ intBinNonNegIntConform rotateL rotateL x y,
          testProperty "rotateR" $ \x y -> ioProperty $ intBinNonNegIntConform rotateR rotateR x y,
          testProperty "popCount" $ ioProperty . \(x :: Int8) -> popCount x @=? popCount (fromIntegral x :: IntN 8)
        ],
      testGroup
        "IntN 8 conform to Int8 for Num instances"
        [ testProperty "(+)" $ \x y -> ioProperty $ intBinConform (+) (+) x y,
          testProperty "(*)" $ \x y -> ioProperty $ intBinConform (*) (*) x y,
          testProperty "(-)" $ \x y -> ioProperty $ intBinConform (-) (-) x y,
          testProperty "negate" $ ioProperty . wordUnaryConform negate negate,
          testProperty "abs" $ ioProperty . wordUnaryConform abs abs,
          testProperty "signum" $ ioProperty . wordUnaryConform signum signum,
          testProperty "fromInteger" $
            ioProperty . \(x :: Integer) ->
              intN8eqint8 (fromInteger x :: IntN 8) (fromInteger x :: Int8)
        ],
      testGroup
        "IntN 8 conform to IntN for Ord instances"
        [ testProperty "(<=)" $ \(x :: Int8) y -> ioProperty $ (fromIntegral x :: IntN 8) <= (fromIntegral y :: IntN 8) @=? x <= y
        ],
      testGroup
        "WordN bvops"
        [ testProperty "bvconcat" $ \(x :: Integer) (y :: Integer) ->
            ioProperty $
              bvconcat (fromInteger x :: WordN 5) (fromInteger y :: WordN 7) @=? fromInteger (x * 128 + y `mod` 128),
          testProperty "bvzeroExtend" $ ioProperty . \(x :: Integer) -> bvzeroExtend (Proxy :: Proxy 12) (fromInteger x :: WordN 7) @=? fromInteger (x `mod` 128),
          testCase "bvsignExtend" $ do
            bvsignExtend (Proxy :: Proxy 12) (0 :: WordN 8) @=? 0
            bvsignExtend (Proxy :: Proxy 12) (1 :: WordN 8) @=? 1
            bvsignExtend (Proxy :: Proxy 12) (127 :: WordN 8) @=? 127
            bvsignExtend (Proxy :: Proxy 12) (128 :: WordN 8) @=? 3968
            bvsignExtend (Proxy :: Proxy 12) (255 :: WordN 8) @=? 4095,
          testProperty "bvextend is bvzeroExtend" $
            ioProperty . \(x :: Integer) ->
              bvextend (Proxy :: Proxy 12) (fromInteger x :: WordN 8) @=? bvzeroExtend (Proxy :: Proxy 12) (fromInteger x :: WordN 8),
          testCase "bvselect" $ do
            bvselect (Proxy :: Proxy 3) (Proxy :: Proxy 3) (0b11100 :: WordN 8) @=? 0b11
            bvselect (Proxy :: Proxy 3) (Proxy :: Proxy 3) (0b111000 :: WordN 8) @=? 0b111
            bvselect (Proxy :: Proxy 3) (Proxy :: Proxy 3) (0b101000 :: WordN 8) @=? 0b101
            bvselect (Proxy :: Proxy 3) (Proxy :: Proxy 3) (0b1010000 :: WordN 8) @=? 0b10
        ],
      testGroup
        "IntN bvops"
        [ testProperty "bvconcat" $ \(x :: Integer) (y :: Integer) ->
            ioProperty $
              bvconcat (fromInteger x :: IntN 5) (fromInteger y :: IntN 7) @=? fromInteger (x * 128 + y `mod` 128),
          testProperty "bvzeroExtend" $ ioProperty . \(x :: Integer) -> bvzeroExtend (Proxy :: Proxy 12) (fromInteger x :: IntN 7) @=? fromInteger (x `mod` 128),
          testCase "bvsignExtend" $ do
            bvsignExtend (Proxy :: Proxy 12) (0 :: WordN 8) @=? 0
            bvsignExtend (Proxy :: Proxy 12) (1 :: WordN 8) @=? 1
            bvsignExtend (Proxy :: Proxy 12) (127 :: WordN 8) @=? 127
            bvsignExtend (Proxy :: Proxy 12) (128 :: WordN 8) @=? 3968
            bvsignExtend (Proxy :: Proxy 12) (255 :: WordN 8) @=? 4095,
          testProperty "bvextend is bvsignExtend" $
            ioProperty . \(x :: Integer) ->
              bvextend (Proxy :: Proxy 12) (fromInteger x :: IntN 8) @=? bvsignExtend (Proxy :: Proxy 12) (fromInteger x :: IntN 8),
          testCase "bvselect" $ do
            bvselect (Proxy :: Proxy 3) (Proxy :: Proxy 3) (0b11100 :: IntN 8) @=? 0b11
            bvselect (Proxy :: Proxy 3) (Proxy :: Proxy 3) (0b111000 :: IntN 8) @=? 0b111
            bvselect (Proxy :: Proxy 3) (Proxy :: Proxy 3) (0b101000 :: IntN 8) @=? 0b101
            bvselect (Proxy :: Proxy 3) (Proxy :: Proxy 3) (0b1010000 :: IntN 8) @=? 0b10
        ]
    ]
