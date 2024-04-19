{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Core.Data.Class.SafeSymRotateTests
  ( safeSymRotateTests,
  )
where

import Control.Exception (ArithException (Overflow))
import Control.Monad.Except (ExceptT)
import Data.Bits (Bits (rotateL, rotateR), FiniteBits (finiteBitSize))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Typeable (Proxy (Proxy), Typeable)
import Data.Word (Word16, Word32, Word64, Word8)
import Grisette
  ( IntN,
    Mergeable,
    SafeSymRotate (safeSymRotateL, safeSymRotateR),
    Solvable (con),
    SymIntN,
    SymWordN,
    UnionM,
    WordN,
  )
import Grisette.Internal.SymPrim.Prim.Term (LinkedRep)
import Grisette.Lib.Control.Monad (mrgReturn)
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@?=))
import Test.QuickCheck (Arbitrary, ioProperty)
import Test.QuickCheck.Gen (chooseInt)
import Test.QuickCheck.Property (forAll)

type EM = ExceptT ArithException UnionM

overflowError :: (Mergeable a) => EM a
overflowError = mrgThrowError Overflow

concreteTypeSafeSymRotateTests ::
  forall proxy a.
  ( Arbitrary a,
    Show a,
    Num a,
    Eq a,
    SafeSymRotate ArithException a EM,
    FiniteBits a,
    Bounded a,
    Typeable a,
    Integral a,
    Mergeable a
  ) =>
  proxy a ->
  [Test]
concreteTypeSafeSymRotateTests _ =
  [ testProperty "In bound" $ \(x :: a) -> do
      let b = fromIntegral (maxBound :: a) :: Integer
      let bs = 2 * fromIntegral (finiteBitSize x) :: Integer
      let maxRotateAmount = fromIntegral (min b bs)
      forAll (chooseInt (0, maxRotateAmount)) $
        \(s :: Int) ->
          ioProperty $ do
            let rotateAmount = fromIntegral s
            let rotateLExpected = mrgReturn (rotateL x s) :: EM a
            let rotateRExpected = mrgReturn (rotateR x s) :: EM a
            safeSymRotateL x rotateAmount @?= rotateLExpected
            safeSymRotateR x rotateAmount @?= rotateRExpected
  ]

concreteSignedAtLeastThreeBitsTypeSafeSymRotateTests ::
  forall proxy a.
  ( Arbitrary a,
    Show a,
    Num a,
    Eq a,
    SafeSymRotate ArithException a EM,
    FiniteBits a,
    Bounded a,
    Typeable a,
    Integral a,
    Mergeable a
  ) =>
  proxy a ->
  [Test]
concreteSignedAtLeastThreeBitsTypeSafeSymRotateTests p =
  testCase
    "Min bound"
    ( do
        let x = -1 :: a
        let rotateAmount = minBound :: a
        safeSymRotateL x rotateAmount @?= overflowError
        safeSymRotateR x rotateAmount @?= overflowError
    )
    : concreteTypeSafeSymRotateTests p

concreteUnsignedSymTypeSafeSymRotateTests ::
  forall proxy c s.
  ( Arbitrary c,
    Show s,
    Num s,
    Eq s,
    SafeSymRotate ArithException s EM,
    FiniteBits c,
    FiniteBits s,
    Bounded c,
    Typeable s,
    Integral c,
    LinkedRep c s,
    Solvable c s,
    Mergeable s
  ) =>
  proxy s ->
  [Test]
concreteUnsignedSymTypeSafeSymRotateTests _ =
  [ testProperty "In bound" $ \(x :: c) -> do
      let b = fromIntegral (maxBound :: c) :: Integer
      let bs = 2 * fromIntegral (finiteBitSize x) :: Integer
      let maxRotateAmount = fromIntegral (min b bs)
      forAll (chooseInt (0, maxRotateAmount)) $
        \(s :: Int) ->
          ioProperty $ do
            let rotateAmount = fromIntegral s
            let rotateLExpected = mrgReturn (con (rotateL x s)) :: EM s
            let rotateRExpected = mrgReturn (con (rotateR x s)) :: EM s
            safeSymRotateL (con x) rotateAmount @?= rotateLExpected
            safeSymRotateR (con x) rotateAmount @?= rotateRExpected
  ]

concreteSignedAtLeastThreeBitsSymTypeSafeSymRotateTests ::
  forall proxy c s.
  ( Arbitrary c,
    Show s,
    Num s,
    Eq s,
    SafeSymRotate ArithException s EM,
    FiniteBits c,
    FiniteBits s,
    Bounded c,
    Typeable s,
    Integral c,
    LinkedRep c s,
    Solvable c s,
    Mergeable s
  ) =>
  proxy s ->
  [Test]
concreteSignedAtLeastThreeBitsSymTypeSafeSymRotateTests p =
  testCase
    "Min bound"
    ( do
        let x = con (-1 :: c)
        let rotateAmount = con (minBound :: c)
        safeSymRotateL x rotateAmount @?= (overflowError :: EM s)
        safeSymRotateR x rotateAmount @?= overflowError
    )
    : concreteUnsignedSymTypeSafeSymRotateTests p

safeSymRotateTests :: Test
safeSymRotateTests =
  testGroup
    "SafeSymRotate"
    [ testGroup "Word8" $ concreteTypeSafeSymRotateTests (Proxy @Word8),
      testGroup "Word16" $ concreteTypeSafeSymRotateTests (Proxy @Word16),
      testGroup "Word32" $ concreteTypeSafeSymRotateTests (Proxy @Word32),
      testGroup "Word64" $ concreteTypeSafeSymRotateTests (Proxy @Word64),
      testGroup "Word" $ concreteTypeSafeSymRotateTests (Proxy @Word),
      testGroup "WordN 1" $ concreteTypeSafeSymRotateTests (Proxy @(WordN 1)),
      testGroup "WordN 2" $ concreteTypeSafeSymRotateTests (Proxy @(WordN 2)),
      testGroup "WordN 3" $ concreteTypeSafeSymRotateTests (Proxy @(WordN 3)),
      testGroup "WordN 63" $ concreteTypeSafeSymRotateTests (Proxy @(WordN 63)),
      testGroup "WordN 64" $ concreteTypeSafeSymRotateTests (Proxy @(WordN 64)),
      testGroup "WordN 65" $ concreteTypeSafeSymRotateTests (Proxy @(WordN 65)),
      testGroup "WordN 128" $
        concreteTypeSafeSymRotateTests (Proxy @(WordN 128)),
      testGroup "SymWordN 1" $
        concreteUnsignedSymTypeSafeSymRotateTests (Proxy @(SymWordN 1)),
      testGroup "SymWordN 2" $
        concreteUnsignedSymTypeSafeSymRotateTests (Proxy @(SymWordN 2)),
      testGroup "SymWordN 3" $
        concreteUnsignedSymTypeSafeSymRotateTests (Proxy @(SymWordN 3)),
      testGroup "SymWordN 63" $
        concreteUnsignedSymTypeSafeSymRotateTests (Proxy @(SymWordN 63)),
      testGroup "SymWordN 64" $
        concreteUnsignedSymTypeSafeSymRotateTests (Proxy @(SymWordN 64)),
      testGroup "SymWordN 65" $
        concreteUnsignedSymTypeSafeSymRotateTests (Proxy @(SymWordN 65)),
      testGroup "SymWordN 128" $
        concreteUnsignedSymTypeSafeSymRotateTests (Proxy @(SymWordN 128)),
      testGroup "Int8" $
        concreteSignedAtLeastThreeBitsTypeSafeSymRotateTests (Proxy @Int8),
      testGroup "Int16" $
        concreteSignedAtLeastThreeBitsTypeSafeSymRotateTests (Proxy @Int16),
      testGroup "Int32" $
        concreteSignedAtLeastThreeBitsTypeSafeSymRotateTests (Proxy @Int32),
      testGroup "Int64" $
        concreteSignedAtLeastThreeBitsTypeSafeSymRotateTests (Proxy @Int64),
      testGroup "Int" $
        concreteSignedAtLeastThreeBitsTypeSafeSymRotateTests (Proxy @Int),
      testGroup
        "IntN 1"
        [ testGroup
            "SafeSymRotate"
            [ testGroup
                "rotate left"
                [ testCase "By 0" $ do
                    safeSymRotateL (-1) 0 @?= (mrgReturn $ -1 :: EM (IntN 1))
                    safeSymRotateR (-1) 0 @?= (mrgReturn $ -1 :: EM (IntN 1)),
                  testCase "By -1" $ do
                    safeSymRotateL (-1) (-1 :: IntN 1) @?= overflowError
                    safeSymRotateR (-1) (-1 :: IntN 1) @?= overflowError
                ]
            ]
        ],
      testGroup
        "IntN 2"
        [ testGroup
            "SafeSymRotate"
            [ testGroup
                "rotate left"
                [ testCase "By 0" $ do
                    safeSymRotateL (-2) 0 @?= (mrgReturn $ -2 :: EM (IntN 2))
                    safeSymRotateR (-2) 0 @?= (mrgReturn $ -2 :: EM (IntN 2)),
                  testCase "By 1" $ do
                    safeSymRotateL (-2) 1 @?= (mrgReturn 1 :: EM (IntN 2))
                    safeSymRotateR (-2) 1 @?= (mrgReturn 1 :: EM (IntN 2)),
                  testCase "By -1" $ do
                    safeSymRotateL (-1) (-1 :: IntN 2) @?= overflowError
                    safeSymRotateR (-1) (-1 :: IntN 2) @?= overflowError,
                  testCase "By -2" $ do
                    safeSymRotateL (-1) (-2 :: IntN 2) @?= overflowError
                    safeSymRotateR (-1) (-2 :: IntN 2) @?= overflowError
                ]
            ]
        ],
      testGroup "IntN 3" $
        concreteSignedAtLeastThreeBitsTypeSafeSymRotateTests (Proxy @(IntN 3)),
      testGroup "IntN 63" $
        concreteSignedAtLeastThreeBitsTypeSafeSymRotateTests (Proxy @(IntN 63)),
      testGroup "IntN 64" $
        concreteSignedAtLeastThreeBitsTypeSafeSymRotateTests (Proxy @(IntN 64)),
      testGroup "IntN 65" $
        concreteSignedAtLeastThreeBitsTypeSafeSymRotateTests (Proxy @(IntN 65)),
      testGroup "IntN 128" $
        concreteSignedAtLeastThreeBitsTypeSafeSymRotateTests
          (Proxy @(IntN 128)),
      testGroup "SymIntN 3" $
        concreteSignedAtLeastThreeBitsSymTypeSafeSymRotateTests
          (Proxy @(SymIntN 3)),
      testGroup "SymIntN 63" $
        concreteSignedAtLeastThreeBitsSymTypeSafeSymRotateTests
          (Proxy @(SymIntN 63)),
      testGroup "SymIntN 64" $
        concreteSignedAtLeastThreeBitsSymTypeSafeSymRotateTests
          (Proxy @(SymIntN 64)),
      testGroup "SymIntN 65" $
        concreteSignedAtLeastThreeBitsSymTypeSafeSymRotateTests
          (Proxy @(SymIntN 65)),
      testGroup "SymIntN 128" $
        concreteSignedAtLeastThreeBitsSymTypeSafeSymRotateTests
          (Proxy @(SymIntN 128)),
      testGroup
        "SymIntN 1"
        [ testGroup
            "SafeSymRotate"
            [ testGroup
                "rotate left"
                [ testCase "By 0" $ do
                    safeSymRotateL (-1) 0 @?= (mrgReturn $ -1 :: EM (SymIntN 1))
                    safeSymRotateR (-1) 0
                      @?= (mrgReturn $ -1 :: EM (SymIntN 1)),
                  testCase "By -1" $ do
                    safeSymRotateL (-1) (-1 :: SymIntN 1) @?= overflowError
                    safeSymRotateR (-1) (-1 :: SymIntN 1) @?= overflowError
                ]
            ]
        ],
      testGroup
        "SymIntN 2"
        [ testGroup
            "SafeSymRotate"
            [ testGroup
                "rotate left"
                [ testCase "By 0" $ do
                    safeSymRotateL (-2) 0 @?= (mrgReturn $ -2 :: EM (SymIntN 2))
                    safeSymRotateR (-2) 0
                      @?= (mrgReturn $ -2 :: EM (SymIntN 2)),
                  testCase "By 1" $ do
                    safeSymRotateL (-2) 1 @?= (mrgReturn 1 :: EM (IntN 2))
                    safeSymRotateR (-2) 1 @?= (mrgReturn 1 :: EM (IntN 2)),
                  testCase "By -1" $ do
                    safeSymRotateL (-1) (-1 :: SymIntN 2) @?= overflowError
                    safeSymRotateR (-1) (-1 :: SymIntN 2) @?= overflowError,
                  testCase "By -2" $ do
                    safeSymRotateL (-1) (-2 :: SymIntN 2) @?= overflowError
                    safeSymRotateR (-1) (-2 :: SymIntN 2) @?= overflowError
                ]
            ]
        ]
    ]
