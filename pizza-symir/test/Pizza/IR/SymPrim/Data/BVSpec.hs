{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pizza.IR.SymPrim.Data.BVSpec where

import Control.Monad
import Data.Bits
import Data.Int
import Data.Proxy
import Data.Word
import Pizza.Core.Data.Class.BitVector
import Pizza.IR.SymPrim.Data.BV
import Test.Hspec
import Test.Hspec.QuickCheck

wordUnaryConform :: HasCallStack => (WordN 8 -> WordN 8) -> (Word8 -> Word8) -> Word8 -> Expectation
wordUnaryConform f g x = unWordN (f (fromIntegral x)) `shouldBe` toInteger (g x)

wordUnaryNonNegIntConform :: HasCallStack => (Int -> WordN 8) -> (Int -> Word8) -> Int -> Expectation
wordUnaryNonNegIntConform f g y = when (y >= 0) $ unWordN (f y) `shouldBe` toInteger (g y)

wordBinIntConform :: HasCallStack => (WordN 8 -> Int -> WordN 8) -> (Word8 -> Int -> Word8) -> Word8 -> Int -> Expectation
wordBinIntConform f g x y = unWordN (f (fromIntegral x) y) `shouldBe` toInteger (g x y)

wordBinNonNegIntConform :: HasCallStack => (WordN 8 -> Int -> WordN 8) -> (Word8 -> Int -> Word8) -> Word8 -> Int -> Expectation
wordBinNonNegIntConform f g x y = when (y >= 0) $ unWordN (f (fromIntegral x) y) `shouldBe` toInteger (g x y)

wordBinConform :: HasCallStack => (WordN 8 -> WordN 8 -> WordN 8) -> (Word8 -> Word8 -> Word8) -> Word8 -> Word8 -> Expectation
wordBinConform f g x y = unWordN (f (fromIntegral x) (fromIntegral y)) `shouldBe` toInteger (g x y)

intN8eqint8 :: IntN 8 -> Int8 -> Expectation
intN8eqint8 (IntN v) i
  | v < 0 = expectationFailure "Bad IntN"
  | v <= 127 = v `shouldBe` fromIntegral i
  | v == 128 = i `shouldBe` -128
  | otherwise = 256 - v `shouldBe` fromIntegral (-i)

intUnaryConform :: (IntN 8 -> IntN 8) -> (Int8 -> Int8) -> Int8 -> Expectation
intUnaryConform f g x = intN8eqint8 (f (fromIntegral x)) (g x)

intUnaryNonNegIntConform :: (Int -> IntN 8) -> (Int -> Int8) -> Int -> Expectation
intUnaryNonNegIntConform f g y = when (y >= 0) $ intN8eqint8 (f y) (g y)

intBinIntConform :: (IntN 8 -> Int -> IntN 8) -> (Int8 -> Int -> Int8) -> Int8 -> Int -> Expectation
intBinIntConform f g x y = intN8eqint8 (f (fromIntegral x) y) (g x y)

intBinNonNegIntConform :: (IntN 8 -> Int -> IntN 8) -> (Int8 -> Int -> Int8) -> Int8 -> Int -> Expectation
intBinNonNegIntConform f g x y = when (y >= 0) $ intN8eqint8 (f (fromIntegral x) y) (g x y)

intBinConform :: (IntN 8 -> IntN 8 -> IntN 8) -> (Int8 -> Int8 -> Int8) -> Int8 -> Int8 -> Expectation
intBinConform f g x y = intN8eqint8 (f (fromIntegral x) (fromIntegral y)) (g x y)

spec :: Spec
spec = do
  describe "WordN 8 conform to Word8 for Bits instances" $ do
    prop "(.&.) conformance" $ wordBinConform (.&.) (.&.)
    prop "(.|.) conformance" $ wordBinConform (.|.) (.|.)
    prop "xor conformance" $ wordBinConform xor xor
    prop "complement conformance" $ wordUnaryConform complement complement
    prop "shift conformance" $ wordBinIntConform shift shift
    prop "rotate conformance" $ wordBinIntConform rotate rotate
    it "zeroBits conformance" $ (zeroBits :: WordN 8) `shouldBe` 0
    prop "bit conformance" $ wordUnaryNonNegIntConform bit bit
    prop "setBit conformance" $ wordBinNonNegIntConform setBit setBit
    prop "clearBit conformance" $ wordBinNonNegIntConform clearBit clearBit
    prop "complementBit conformance" $ wordBinNonNegIntConform complementBit complementBit
    prop "testBit conformance" $ \(x :: Word8) i -> i < 0 || testBit x i == testBit (fromIntegral x :: WordN 8) i
    it "bitSizeMaybe conformance" $ bitSizeMaybe (0 :: WordN 8) `shouldBe` Just 8
    it "isSigned conformance" $ isSigned (0 :: WordN 8) `shouldBe` False
    prop "shiftL conformance" $ wordBinNonNegIntConform shiftL shiftL
    prop "shiftR conformance" $ wordBinNonNegIntConform shiftR shiftR
    prop "rotateL conformance" $ wordBinNonNegIntConform rotateL rotateL
    prop "rotateR conformance" $ wordBinNonNegIntConform rotateR rotateR
    prop "popCount conformance" $ \(x :: Word8) -> popCount x == popCount (fromIntegral x :: WordN 8)

  describe "WordN 8 conform to Word8 for Num instances" $ do
    prop "(+) conformance" $ wordBinConform (+) (+)
    prop "(*) conformance" $ wordBinConform (*) (*)
    prop "(-) conformance" $ wordBinConform (-) (-)
    prop "negate conformance" $ wordUnaryConform negate negate
    prop "abs conformance" $ wordUnaryConform abs abs
    prop "signum conformance" $ wordUnaryConform signum signum
    prop "fromInteger conformance" $ \(x :: Integer) ->
      unWordN (fromInteger x :: WordN 8) == toInteger (fromInteger x :: Word8)

  describe "WordN 8 conform to Word8 for Ord instances" $ do
    prop "(<=) conformance" $ \(x :: Word8) y -> x <= y `shouldBe` (fromIntegral x :: WordN 8) <= (fromIntegral y :: WordN 8)

  describe "IntN 8 conform to Int8 for Bits instances" $ do
    prop "(.&.) conformance" $ intBinConform (.&.) (.&.)
    prop "(.|.) conformance" $ intBinConform (.|.) (.|.)
    prop "xor conformance" $ intBinConform xor xor
    prop "complement conformance" $ intUnaryConform complement complement
    prop "shift conformance" $ intBinIntConform shift shift
    prop "rotate conformance" $ intBinIntConform rotate rotate
    it "zeroBits conformance" $ (zeroBits :: IntN 8) `shouldBe` 0
    prop "bit conformance" $ intUnaryNonNegIntConform bit bit
    prop "setBit conformance" $ intBinNonNegIntConform setBit setBit
    prop "clearBit conformance" $ intBinNonNegIntConform clearBit clearBit
    prop "complementBit conformance" $ intBinNonNegIntConform complementBit complementBit
    prop "testBit conformance" $ \(x :: Int8) i -> i < 0 || testBit x i == testBit (fromIntegral x :: IntN 8) i
    it "bitSizeMaybe conformance" $ bitSizeMaybe (0 :: IntN 8) `shouldBe` Just 8
    it "isSigned conformance" $ isSigned (0 :: IntN 8) `shouldBe` True
    prop "shiftL conformance" $ intBinNonNegIntConform shiftL shiftL
    prop "shiftR conformance" $ intBinNonNegIntConform shiftR shiftR
    prop "rotateL conformance" $ intBinNonNegIntConform rotateL rotateL
    prop "rotateR conformance" $ intBinNonNegIntConform rotateR rotateR
    prop "popCount conformance" $ \(x :: Int8) -> popCount x == popCount (fromIntegral x :: IntN 8)

  describe "IntN 8 conform to Int8 for Num instances" $ do
    prop "(+) conformance" $ intBinConform (+) (+)
    prop "(*) conformance" $ intBinConform (*) (*)
    prop "(-) conformance" $ intBinConform (-) (-)
    prop "negate conformance" $ wordUnaryConform negate negate
    prop "abs conformance" $ wordUnaryConform abs abs
    prop "signum conformance" $ wordUnaryConform signum signum
    prop "fromInteger conformance" $ \(x :: Integer) ->
      intN8eqint8 (fromInteger x :: IntN 8) (fromInteger x :: Int8)

  describe "IntN 8 conform to IntN for Ord instances" $ do
    prop "(<=) conformance" $ \(x :: Int8) y -> (fromIntegral x :: IntN 8) <= (fromIntegral y :: IntN 8) `shouldBe` x <= y

  describe "WordN bvops" $ do
    prop "WordN bvconcat" $ \(x :: Integer) (y :: Integer) ->
      bvconcat (fromInteger x :: WordN 5) (fromInteger y :: WordN 7) `shouldBe` fromInteger (x * 128 + y `mod` 128)
    prop "WordN bvzeroExtend" $ \(x :: Integer) -> bvzeroExtend (Proxy :: Proxy 12) (fromInteger x :: WordN 7) `shouldBe` fromInteger (x `mod` 128)
    it "WordN bvsignExtend" $ do
      bvsignExtend (Proxy :: Proxy 12) (0 :: WordN 8) `shouldBe` 0
      bvsignExtend (Proxy :: Proxy 12) (1 :: WordN 8) `shouldBe` 1
      bvsignExtend (Proxy :: Proxy 12) (127 :: WordN 8) `shouldBe` 127
      bvsignExtend (Proxy :: Proxy 12) (128 :: WordN 8) `shouldBe` 3968
      bvsignExtend (Proxy :: Proxy 12) (255 :: WordN 8) `shouldBe` 4095
    prop "WordN bvextend is bvzeroExtend" $ \(x :: Integer) ->
      bvextend (Proxy :: Proxy 12) (fromInteger x :: WordN 8) `shouldBe` bvzeroExtend (Proxy :: Proxy 12) (fromInteger x :: WordN 8)
    it "WordN bvselect" $ do
      bvselect (Proxy :: Proxy 3) (Proxy :: Proxy 3) (0b11100 :: WordN 8) `shouldBe` 0b11
      bvselect (Proxy :: Proxy 3) (Proxy :: Proxy 3) (0b111000 :: WordN 8) `shouldBe` 0b111
      bvselect (Proxy :: Proxy 3) (Proxy :: Proxy 3) (0b101000 :: WordN 8) `shouldBe` 0b101
      bvselect (Proxy :: Proxy 3) (Proxy :: Proxy 3) (0b1010000 :: WordN 8) `shouldBe` 0b10
  describe "IntN bvops" $ do
    prop "IntN bvconcat" $ \(x :: Integer) (y :: Integer) ->
      bvconcat (fromInteger x :: IntN 5) (fromInteger y :: IntN 7) `shouldBe` fromInteger (x * 128 + y `mod` 128)
    prop "IntN bvzeroExtend" $ \(x :: Integer) -> bvzeroExtend (Proxy :: Proxy 12) (fromInteger x :: IntN 7) `shouldBe` fromInteger (x `mod` 128)
    it "IntN bvsignExtend" $ do
      bvsignExtend (Proxy :: Proxy 12) (0 :: WordN 8) `shouldBe` 0
      bvsignExtend (Proxy :: Proxy 12) (1 :: WordN 8) `shouldBe` 1
      bvsignExtend (Proxy :: Proxy 12) (127 :: WordN 8) `shouldBe` 127
      bvsignExtend (Proxy :: Proxy 12) (128 :: WordN 8) `shouldBe` 3968
      bvsignExtend (Proxy :: Proxy 12) (255 :: WordN 8) `shouldBe` 4095
    prop "IntN bvextend is bvsignExtend" $ \(x :: Integer) ->
      bvextend (Proxy :: Proxy 12) (fromInteger x :: IntN 8) `shouldBe` bvsignExtend (Proxy :: Proxy 12) (fromInteger x :: IntN 8)
    it "IntN bvselect" $ do
      bvselect (Proxy :: Proxy 3) (Proxy :: Proxy 3) (0b11100 :: IntN 8) `shouldBe` 0b11
      bvselect (Proxy :: Proxy 3) (Proxy :: Proxy 3) (0b111000 :: IntN 8) `shouldBe` 0b111
      bvselect (Proxy :: Proxy 3) (Proxy :: Proxy 3) (0b101000 :: IntN 8) `shouldBe` 0b101
      bvselect (Proxy :: Proxy 3) (Proxy :: Proxy 3) (0b1010000 :: IntN 8) `shouldBe` 0b10
