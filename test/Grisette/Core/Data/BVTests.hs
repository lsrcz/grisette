{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Core.Data.BVTests (bvTests) where

import Control.DeepSeq (NFData (rnf), deepseq, force)
import Control.Exception
  ( ArithException,
    SomeException,
    catch,
    evaluate,
  )
import Control.Monad (when)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Bits
  ( Bits
      ( bit,
        bitSizeMaybe,
        clearBit,
        complement,
        complementBit,
        isSigned,
        popCount,
        rotate,
        rotateL,
        rotateR,
        setBit,
        shift,
        shiftL,
        shiftR,
        testBit,
        xor,
        zeroBits,
        (.&.),
        (.|.)
      ),
    FiniteBits (countLeadingZeros, countTrailingZeros, finiteBitSize),
  )
import Data.Int (Int8)
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (Typeable, typeRep)
import Data.Word (Word8)
import GHC.Stack (HasCallStack)
import Grisette.Core.Data.BV
  ( IntN (IntN),
    WordN (unWordN),
  )
import Grisette.Core.Data.Class.BitVector
  ( BV (bv),
    SizedBV
      ( sizedBVConcat,
        sizedBVExt,
        sizedBVSelect,
        sizedBVSext,
        sizedBVZext
      ),
  )
import Grisette.Core.Data.SomeBV
  ( SomeIntN,
    SomeWordN,
    pattern SomeIntN,
    pattern SomeWordN,
  )
import Test.Framework (Test, TestName, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (Assertion, assertFailure, (@=?), (@?=))
import Test.QuickCheck (Arbitrary, Property, ioProperty)

unaryConform :: forall a b c d. (Show c, Eq c, HasCallStack) => (a -> b) -> (d -> c) -> (a -> c) -> (b -> d) -> a -> Property
unaryConform a2b d2c f g x = ioProperty $ f x @=? d2c (g (a2b x))

binaryConform ::
  forall a b c d e f.
  (Show e, Eq e, HasCallStack) =>
  (a -> b) ->
  (c -> d) ->
  (f -> e) ->
  (a -> c -> e) ->
  (b -> d -> f) ->
  a ->
  c ->
  Property
binaryConform a2b c2d f2e f g x y = ioProperty $ f x y @=? f2e (g (a2b x) (c2d y))

wordUnaryConform :: (HasCallStack) => (WordN 8 -> WordN 8) -> (Word8 -> Word8) -> Word8 -> Assertion
wordUnaryConform f g x = unWordN (f (fromIntegral x)) @=? toInteger (g x)

wordUnaryNonNegIntConform :: (HasCallStack) => (Int -> WordN 8) -> (Int -> Word8) -> Int -> Assertion
wordUnaryNonNegIntConform f g y = when (y >= 0) $ unWordN (f y) @=? toInteger (g y)

wordBinIntConform :: (HasCallStack) => (WordN 8 -> Int -> WordN 8) -> (Word8 -> Int -> Word8) -> Word8 -> Int -> Assertion
wordBinIntConform f g x y = unWordN (f (fromIntegral x) y) @=? toInteger (g x y)

wordBinNonNegIntConform :: (HasCallStack) => (WordN 8 -> Int -> WordN 8) -> (Word8 -> Int -> Word8) -> Word8 -> Int -> Assertion
wordBinNonNegIntConform f g x y = when (y >= 0) $ unWordN (f (fromIntegral x) y) @=? toInteger (g x y)

wordBinConform :: (HasCallStack) => (WordN 8 -> WordN 8 -> WordN 8) -> (Word8 -> Word8 -> Word8) -> Word8 -> Word8 -> Assertion
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

finiteBitsConformTest ::
  forall ref typ.
  (Arbitrary ref, Typeable ref, Typeable typ, Show ref, Show typ, Eq ref, Eq typ, FiniteBits ref, FiniteBits typ, Integral ref, Integral typ) =>
  Proxy ref ->
  Proxy typ ->
  Int ->
  Test
finiteBitsConformTest pref ptyp numBits =
  testGroup
    (show (typeRep ptyp) ++ " conform to " ++ show (typeRep pref) ++ " for FiniteBits instances")
    [ testCase "finiteBitSize" $ finiteBitSize (0 :: typ) @=? numBits,
      testProperty "countLeadingZeros" $ unaryConform @ref @typ fromIntegral id countLeadingZeros countLeadingZeros,
      testProperty "countTrailingZeros" $ unaryConform @ref @typ fromIntegral id countTrailingZeros countTrailingZeros
    ]

boundedConformTest ::
  forall ref typ.
  (Typeable ref, Typeable typ, Bounded typ, Bounded ref, Integral ref, Num typ, Eq typ, Show typ) =>
  Proxy ref ->
  Proxy typ ->
  Test
boundedConformTest pref ptyp =
  testGroup
    (show (typeRep ptyp) ++ " conform to " ++ show (typeRep pref) ++ " for Bounded instances")
    [ testCase "minBound" $ (minBound :: typ) @=? fromIntegral (minBound :: ref),
      testCase "maxBound" $ (maxBound :: typ) @=? fromIntegral (maxBound :: ref)
    ]

shouldThrow :: (NFData a) => String -> a -> IO ()
shouldThrow name x = do
  errored <- catch (evaluate $ x `deepseq` True) (\(_ :: SomeException) -> return False)
  when errored $ assertFailure $ name ++ " should throw an exception"

succPredLikeTest ::
  forall a b.
  (Arbitrary a, Eq a, Eq b, Show a, Show b, NFData b) =>
  TestName ->
  String ->
  (a -> b) ->
  (a -> a) ->
  (b -> b) ->
  a ->
  b ->
  Test
succPredLikeTest name boundName a2b fa fb bounda boundb =
  testGroup
    name
    [ testProperty (name ++ " non " ++ boundName) $
        ioProperty . \(x :: a) ->
          if x == bounda then return () else a2b (fa x) @=? fb (a2b x),
      testCase (name ++ " " ++ boundName) $ shouldThrow (name ++ " " ++ boundName) $ fb boundb
    ]

enumConformTest ::
  forall ref typ.
  ( Arbitrary ref,
    Typeable ref,
    Typeable typ,
    Eq ref,
    Eq typ,
    Show ref,
    Show typ,
    NFData typ,
    Integral ref,
    Integral typ,
    Bounded ref,
    Bounded typ
  ) =>
  Proxy ref ->
  Proxy typ ->
  Test
enumConformTest pref ptyp =
  testGroup
    (show (typeRep ptyp) ++ " conform to " ++ show (typeRep pref) ++ " for Enum instances")
    [ succPredLikeTest @ref @typ "succ" "maxBound" fromIntegral succ succ maxBound maxBound,
      succPredLikeTest @ref @typ "pred" "minBound" fromIntegral pred pred minBound minBound,
      testGroup
        "toEnum"
        [ testProperty "toEnum in bounds" $
            ioProperty . \(x :: ref) ->
              toInteger (toEnum (fromIntegral x) :: ref) @=? toInteger (toEnum (fromIntegral x) :: typ),
          testCase "toEnum (fromIntegral minBound - 1)" $
            shouldThrow "toEnum (fromIntegral minBound - 1)" (toEnum (fromIntegral (minBound :: typ) - 1) :: typ),
          testCase "toEnum (fromIntegral maxBound + 1)" $
            shouldThrow "toEnum (fromIntegral maxBound + 1)" (toEnum (fromIntegral (maxBound :: typ) + 1) :: typ)
        ],
      testProperty "fromEnum" $ unaryConform @ref @typ fromIntegral id fromEnum fromEnum,
      testProperty "enumFrom" $ unaryConform @ref @typ fromIntegral (fromIntegral <$>) enumFrom enumFrom,
      testProperty "enumFromThen" $ \(x :: ref) y ->
        ioProperty $ do
          if x == y
            then return ()
            else do
              (fromIntegral <$> enumFromThen x y) @=? enumFromThen (fromIntegral x :: typ) (fromIntegral y),
      testProperty "enumFromTo" $ binaryConform @ref @typ fromIntegral fromIntegral (fromIntegral <$>) enumFromTo enumFromTo,
      testProperty "enumFromThenTo" $ \(x :: ref) y z ->
        ioProperty $
          if x == y
            then return ()
            else (fromIntegral <$> enumFromThenTo x y z) @=? enumFromThenTo (fromIntegral x :: typ) (fromIntegral y) (fromIntegral z)
    ]

newtype AEWrapper = AEWrapper ArithException deriving (Eq)

instance Show AEWrapper where
  show (AEWrapper x) = show x

instance NFData AEWrapper where
  rnf (AEWrapper x) = x `seq` ()

sameDiv :: (NFData a, NFData b, Eq b, Show b) => a -> a -> (a -> b) -> (a -> a -> a) -> (b -> b -> b) -> IO ()
sameDiv x y a2b fa fb = do
  xa <- evaluate (force $ Right $ fa x y) `catch` \(e :: ArithException) -> return $ Left $ AEWrapper e
  xb <- evaluate (force $ Right $ fb (a2b x) (a2b y)) `catch` \(e :: ArithException) -> return $ Left $ AEWrapper e
  xb @=? a2b <$> xa

sameDivMod :: (NFData a, NFData b, Eq b, Show b) => a -> a -> (a -> b) -> (a -> a -> (a, a)) -> (b -> b -> (b, b)) -> IO ()
sameDivMod x y a2b fa fb = do
  xa <- evaluate (force $ Right $ fa x y) `catch` \(e :: ArithException) -> return $ Left $ AEWrapper e
  xb <- evaluate (force $ Right $ fb (a2b x) (a2b y)) `catch` \(e :: ArithException) -> return $ Left $ AEWrapper e
  xb @=? bimap a2b a2b <$> xa

divLikeTest ::
  forall a b.
  (Arbitrary a, Eq a, Eq b, Num a, Show a, Bounded a, Bits a, Eq b, Show b, Num b, NFData b, Bounded b, Bits b, NFData a) =>
  TestName ->
  (a -> b) ->
  (a -> a -> a) ->
  (b -> b -> b) ->
  Test
divLikeTest name a2b fa fb =
  testGroup
    name
    [ testCase "divided by zero" $ do
        sameDiv 1 0 a2b fa fb
        sameDiv 0 0 a2b fa fb
        sameDiv (-1) 0 a2b fa fb
        sameDiv minBound 0 a2b fa fb
        sameDiv maxBound 0 a2b fa fb,
      testCase "min divided by -1" $ do
        sameDiv minBound (-1) a2b fa fb,
      testProperty "prop" $ \(x :: a) y -> ioProperty $ sameDiv x y a2b fa fb
    ]

divModLikeTest ::
  forall a b.
  (Arbitrary a, Eq a, Eq b, Num a, NFData a, Show a, Bounded a, Bits a, Eq b, Show b, Num b, NFData b, Bounded b, Bits b) =>
  TestName ->
  (a -> b) ->
  (a -> a -> (a, a)) ->
  (b -> b -> (b, b)) ->
  Test
divModLikeTest name a2b fa fb =
  testGroup
    name
    [ testCase "divided by zero" $ do
        sameDivMod 1 0 a2b fa fb
        sameDivMod 0 0 a2b fa fb
        sameDivMod (-1) 0 a2b fa fb
        sameDivMod minBound 0 a2b fa fb
        sameDivMod maxBound 0 a2b fa fb,
      testCase "min divided by -1" $ do
        sameDivMod minBound (-1) a2b fa fb,
      testProperty "prop" $ \(x :: a) y -> ioProperty $ sameDivMod x y a2b fa fb
    ]

realConformTest ::
  forall proxy ref typ.
  (Typeable ref, Typeable typ, Integral ref, Num typ, Arbitrary ref, Real typ, Show ref) =>
  proxy ref ->
  proxy typ ->
  Test
realConformTest pref ptyp =
  testGroup
    (show (typeRep ptyp) ++ " conform to " ++ show (typeRep pref) ++ " for Real instances")
    [ testProperty "toRational" $ unaryConform @ref @typ fromIntegral id toRational toRational
    ]

integralConformTest ::
  forall ref typ.
  ( Arbitrary ref,
    Typeable ref,
    Typeable typ,
    Eq ref,
    Eq typ,
    Show ref,
    Show typ,
    Num ref,
    Num typ,
    Integral ref,
    Integral typ,
    Bits ref,
    Bits typ,
    Bounded ref,
    Bounded typ,
    NFData typ,
    NFData ref
  ) =>
  Proxy ref ->
  Proxy typ ->
  Test
integralConformTest pref ptyp =
  testGroup
    (show (typeRep ptyp) ++ " conform to " ++ show (typeRep pref) ++ " for Integral instances")
    [ divLikeTest @ref @typ "quot" fromIntegral quot quot,
      divLikeTest @ref @typ "rem" fromIntegral rem rem,
      divModLikeTest @ref @typ "quotRem" fromIntegral quotRem quotRem,
      divLikeTest @ref @typ "div" fromIntegral div div,
      divLikeTest @ref @typ "mod" fromIntegral mod mod,
      divModLikeTest @ref @typ "divMod" fromIntegral divMod divMod,
      testProperty "toInteger" $ unaryConform @ref @typ fromIntegral id toInteger toInteger
    ]

sizedBVTests :: Test
sizedBVTests =
  testGroup
    "sizedBV"
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
      finiteBitsConformTest (Proxy @Word8) (Proxy @(WordN 8)) 8,
      boundedConformTest (Proxy @Word8) (Proxy @(WordN 8)),
      enumConformTest (Proxy @Word8) (Proxy @(WordN 8)),
      realConformTest (Proxy @Word8) (Proxy @(WordN 8)),
      integralConformTest (Proxy @Word8) (Proxy @(WordN 8)),
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
      finiteBitsConformTest (Proxy @Int8) (Proxy @(IntN 8)) 8,
      boundedConformTest (Proxy @Int8) (Proxy @(IntN 8)),
      enumConformTest (Proxy @Int8) (Proxy @(IntN 8)),
      realConformTest (Proxy @Int8) (Proxy @(IntN 8)),
      integralConformTest (Proxy @Int8) (Proxy @(IntN 8)),
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
        [ testProperty "sizedBVConcat" $ \(x :: Integer) (y :: Integer) ->
            ioProperty $
              sizedBVConcat (fromInteger x :: WordN 5) (fromInteger y :: WordN 7) @=? fromInteger (x * 128 + y `mod` 128),
          testProperty "sizedBVZext" $ ioProperty . \(x :: Integer) -> sizedBVZext (Proxy :: Proxy 12) (fromInteger x :: WordN 7) @=? fromInteger (x `mod` 128),
          testCase "sizedBVSext" $ do
            sizedBVSext (Proxy :: Proxy 12) (0 :: WordN 8) @=? 0
            sizedBVSext (Proxy :: Proxy 12) (1 :: WordN 8) @=? 1
            sizedBVSext (Proxy :: Proxy 12) (127 :: WordN 8) @=? 127
            sizedBVSext (Proxy :: Proxy 12) (128 :: WordN 8) @=? 3968
            sizedBVSext (Proxy :: Proxy 12) (255 :: WordN 8) @=? 4095,
          testProperty "sizedBVExt is sizedBVZext" $
            ioProperty . \(x :: Integer) ->
              sizedBVExt (Proxy :: Proxy 12) (fromInteger x :: WordN 8) @=? sizedBVZext (Proxy :: Proxy 12) (fromInteger x :: WordN 8),
          testCase "sizedBVSelect" $ do
            sizedBVSelect (Proxy :: Proxy 3) (Proxy :: Proxy 3) (0b11100 :: WordN 8) @=? 0b11
            sizedBVSelect (Proxy :: Proxy 3) (Proxy :: Proxy 3) (0b111000 :: WordN 8) @=? 0b111
            sizedBVSelect (Proxy :: Proxy 3) (Proxy :: Proxy 3) (0b101000 :: WordN 8) @=? 0b101
            sizedBVSelect (Proxy :: Proxy 3) (Proxy :: Proxy 3) (0b1010000 :: WordN 8) @=? 0b10
        ],
      testGroup
        "IntN bvops"
        [ testProperty "sizedBVConcat" $ \(x :: Integer) (y :: Integer) ->
            ioProperty $
              sizedBVConcat (fromInteger x :: IntN 5) (fromInteger y :: IntN 7) @=? fromInteger (x * 128 + y `mod` 128),
          testProperty "sizedBVZext" $ ioProperty . \(x :: Integer) -> sizedBVZext (Proxy :: Proxy 12) (fromInteger x :: IntN 7) @=? fromInteger (x `mod` 128),
          testCase "sizedBVSext" $ do
            sizedBVSext (Proxy :: Proxy 12) (0 :: WordN 8) @=? 0
            sizedBVSext (Proxy :: Proxy 12) (1 :: WordN 8) @=? 1
            sizedBVSext (Proxy :: Proxy 12) (127 :: WordN 8) @=? 127
            sizedBVSext (Proxy :: Proxy 12) (128 :: WordN 8) @=? 3968
            sizedBVSext (Proxy :: Proxy 12) (255 :: WordN 8) @=? 4095,
          testProperty "sizedBVExt is sizedBVSext" $
            ioProperty . \(x :: Integer) ->
              sizedBVExt (Proxy :: Proxy 12) (fromInteger x :: IntN 8) @=? sizedBVSext (Proxy :: Proxy 12) (fromInteger x :: IntN 8),
          testCase "sizedBVSelect" $ do
            sizedBVSelect (Proxy :: Proxy 3) (Proxy :: Proxy 3) (0b11100 :: IntN 8) @=? 0b11
            sizedBVSelect (Proxy :: Proxy 3) (Proxy :: Proxy 3) (0b111000 :: IntN 8) @=? 0b111
            sizedBVSelect (Proxy :: Proxy 3) (Proxy :: Proxy 3) (0b101000 :: IntN 8) @=? 0b101
            sizedBVSelect (Proxy :: Proxy 3) (Proxy :: Proxy 3) (0b1010000 :: IntN 8) @=? 0b10
        ],
      testGroup
        "read"
        [ testProperty "read . show" $ \(x :: IntN 8) -> read (show x) == x,
          testProperty "read . show" $ \(x :: WordN 8) -> read (show x) == x,
          testProperty "read . show" $ \(x :: IntN 9) -> read (show x) == x,
          testProperty "read . show" $ \(x :: WordN 9) -> read (show x) == x
        ],
      testGroup
        "Regression"
        [ testCase "division of min bound and minus one for signed bit vector should throw" $ do
            shouldThrow "divMod" $ divMod (minBound :: IntN 8) (-1 :: IntN 8)
            shouldThrow "div" $ div (minBound :: IntN 8) (-1 :: IntN 8)
            shouldThrow "quotRem" $ quotRem (minBound :: IntN 8) (-1 :: IntN 8)
            shouldThrow "quot" $ quot (minBound :: IntN 8) (-1 :: IntN 8),
          testCase "toInteger for IntN 1" $ do
            toInteger (0 :: IntN 1) @=? 0
            toInteger (1 :: IntN 1) @=? (-1),
          testProperty "WordN shiftL by large amount" $ \(x :: WordN 128) ->
            ioProperty $ shiftL x maxBound @=? 0,
          testProperty "IntN shiftL by large amount" $ \(x :: IntN 128) ->
            ioProperty $ shiftL x maxBound @=? 0
        ]
    ]

someWordNTests :: Test
someWordNTests =
  testGroup
    "SomeWordN"
    [ testGroup
        "BV"
        [ testGroup
            "bv"
            [ testCase "bv 12 21" $
                (bv 12 21 :: SomeWordN) @?= SomeWordN (0x015 :: WordN 12)
            ]
        ]
    ]

someIntNTests :: Test
someIntNTests =
  testGroup
    "SomeIntN"
    [ testGroup
        "BV"
        [ testGroup
            "bv"
            [ testCase "bv 12 21" $
                (bv 12 21 :: SomeIntN) @?= SomeIntN (0x015 :: IntN 12)
            ]
        ]
    ]

bvTests :: Test
bvTests = testGroup "BV" [sizedBVTests, someWordNTests, someIntNTests]
