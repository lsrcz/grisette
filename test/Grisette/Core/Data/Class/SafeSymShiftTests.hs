{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Core.Data.Class.SafeSymShiftTests (safeSymShiftTests) where

import Control.Exception (ArithException (Overflow))
import Control.Monad.Except (ExceptT)
import Data.Bits (Bits (shiftL, shiftR), FiniteBits (finiteBitSize))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Typeable (Proxy (Proxy), Typeable)
import Data.Word (Word16, Word32, Word64, Word8)
import Grisette.Core.Control.Monad.UnionM (UnionM)
import Grisette.Core.Data.BV (IntN, WordN)
import Grisette.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Core.Data.Class.SafeSymShift
  ( SafeSymShift
      ( safeSymShiftL,
        safeSymShiftR,
        safeSymStrictShiftL,
        safeSymStrictShiftR
      ),
  )
import Grisette.Core.Data.Class.Solvable (Solvable (con))
import Grisette.SymPrim.Prim.Term (LinkedRep)
import Grisette.SymPrim.SymBV (SymIntN, SymWordN)
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

concreteTypeSafeSymShiftTests ::
  forall proxy a.
  ( Arbitrary a,
    Show a,
    Num a,
    Eq a,
    SafeSymShift ArithException a EM,
    FiniteBits a,
    Bounded a,
    Typeable a,
    Integral a,
    Mergeable a
  ) =>
  proxy a ->
  [Test]
concreteTypeSafeSymShiftTests _ =
  [ testProperty "In bound" $ \(x :: a) ->
      forAll (chooseInt (0, finiteBitSize x - 1)) $
        \(s :: Int) ->
          ioProperty $ do
            let shiftAmount = fromIntegral s
            let shiftLExpected = mrgReturn (shiftL x s) :: EM a
            let shiftRExpected = mrgReturn (shiftR x s) :: EM a
            safeSymShiftL x shiftAmount @?= shiftLExpected
            safeSymStrictShiftL x shiftAmount @?= shiftLExpected
            safeSymShiftR x shiftAmount @?= shiftRExpected
            safeSymStrictShiftR x shiftAmount @?= shiftRExpected,
    testCase "Bit size" $ do
      let x = maxBound :: a
      let shiftAmount = fromIntegral $ finiteBitSize x
      safeSymShiftL x shiftAmount @?= (mrgReturn 0 :: EM a)
      safeSymStrictShiftL x shiftAmount @?= overflowError
      safeSymShiftR x shiftAmount @?= (mrgReturn 0 :: EM a)
      safeSymStrictShiftR x shiftAmount @?= overflowError,
    testCase "Max bound" $ do
      let x = maxBound :: a
      let shiftAmount = maxBound :: a
      safeSymShiftL x shiftAmount @?= (mrgReturn 0 :: EM a)
      safeSymStrictShiftL x shiftAmount @?= overflowError
      safeSymShiftR x shiftAmount @?= (mrgReturn 0 :: EM a)
      safeSymStrictShiftR x shiftAmount @?= overflowError
  ]

concreteSignedAtLeastThreeBitsTypeSafeSymShiftTests ::
  forall proxy a.
  ( Arbitrary a,
    Show a,
    Num a,
    Eq a,
    SafeSymShift ArithException a EM,
    FiniteBits a,
    Bounded a,
    Typeable a,
    Integral a,
    Mergeable a
  ) =>
  proxy a ->
  [Test]
concreteSignedAtLeastThreeBitsTypeSafeSymShiftTests p =
  testCase
    "Min bound"
    ( do
        let x = -1 :: a
        let shiftAmount = minBound :: a
        safeSymShiftL x shiftAmount @?= overflowError
        safeSymStrictShiftL x shiftAmount @?= overflowError
        safeSymShiftR x shiftAmount @?= overflowError
        safeSymStrictShiftR x shiftAmount @?= overflowError
    )
    : (concreteTypeSafeSymShiftTests p)

concreteUnsignedSymTypeSafeSymShiftTests ::
  forall proxy c s.
  ( Arbitrary c,
    Show s,
    Num s,
    Eq s,
    SafeSymShift ArithException s EM,
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
concreteUnsignedSymTypeSafeSymShiftTests _ =
  [ testProperty "In bound" $ \(x :: c) ->
      forAll (chooseInt (0, finiteBitSize x - 1)) $
        \(s :: Int) ->
          ioProperty $ do
            let shiftAmount = fromIntegral s
            let shiftLExpected = mrgReturn (con (shiftL x s)) :: EM s
            let shiftRExpected = mrgReturn (con (shiftR x s)) :: EM s
            safeSymShiftL (con x) shiftAmount @?= shiftLExpected
            safeSymStrictShiftL (con x) shiftAmount @?= shiftLExpected
            safeSymShiftR (con x) shiftAmount @?= shiftRExpected
            safeSymStrictShiftR (con x) shiftAmount @?= shiftRExpected,
    testCase "Bit size" $ do
      let x = con (maxBound :: c)
      let shiftAmount = fromIntegral $ finiteBitSize x
      safeSymShiftL x shiftAmount @?= (mrgReturn 0 :: EM s)
      safeSymStrictShiftL x shiftAmount @?= overflowError
      safeSymShiftR x shiftAmount @?= (mrgReturn 0 :: EM s)
      safeSymStrictShiftR x shiftAmount @?= overflowError,
    testCase "Max bound" $ do
      let x = con (maxBound :: c)
      let shiftAmount = con (maxBound :: c)
      safeSymShiftL x shiftAmount @?= (mrgReturn 0 :: EM s)
      safeSymStrictShiftL x shiftAmount @?= overflowError
      safeSymShiftR x shiftAmount @?= (mrgReturn 0 :: EM s)
      safeSymStrictShiftR x shiftAmount @?= overflowError
  ]

concreteSignedAtLeastThreeBitsSymTypeSafeSymShiftTests ::
  forall proxy c s.
  ( Arbitrary c,
    Show s,
    Num s,
    Eq s,
    SafeSymShift ArithException s EM,
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
concreteSignedAtLeastThreeBitsSymTypeSafeSymShiftTests p =
  testCase
    "Min bound"
    ( do
        let x = con (-1 :: c)
        let shiftAmount = con (minBound :: c)
        safeSymShiftL x shiftAmount @?= (overflowError :: EM s)
        safeSymStrictShiftL x shiftAmount @?= overflowError
        safeSymShiftR x shiftAmount @?= overflowError
        safeSymStrictShiftR x shiftAmount @?= overflowError
    )
    : concreteUnsignedSymTypeSafeSymShiftTests p

safeSymShiftTests :: Test
safeSymShiftTests =
  testGroup
    "SafeSymShift"
    [ testGroup "Word8" $ concreteTypeSafeSymShiftTests (Proxy @Word8),
      testGroup "Word16" $ concreteTypeSafeSymShiftTests (Proxy @Word16),
      testGroup "Word32" $ concreteTypeSafeSymShiftTests (Proxy @Word32),
      testGroup "Word64" $ concreteTypeSafeSymShiftTests (Proxy @Word64),
      testGroup "Word" $ concreteTypeSafeSymShiftTests (Proxy @Word),
      testGroup "WordN 1" $ concreteTypeSafeSymShiftTests (Proxy @(WordN 1)),
      testGroup "WordN 2" $ concreteTypeSafeSymShiftTests (Proxy @(WordN 2)),
      testGroup "WordN 3" $ concreteTypeSafeSymShiftTests (Proxy @(WordN 3)),
      testGroup "WordN 63" $ concreteTypeSafeSymShiftTests (Proxy @(WordN 63)),
      testGroup "WordN 64" $ concreteTypeSafeSymShiftTests (Proxy @(WordN 64)),
      testGroup "WordN 65" $ concreteTypeSafeSymShiftTests (Proxy @(WordN 65)),
      testGroup "WordN 128" $
        concreteTypeSafeSymShiftTests (Proxy @(WordN 128)),
      testGroup "SymWordN 1" $
        concreteUnsignedSymTypeSafeSymShiftTests (Proxy @(SymWordN 1)),
      testGroup "SymWordN 2" $
        concreteUnsignedSymTypeSafeSymShiftTests (Proxy @(SymWordN 2)),
      testGroup "SymWordN 3" $
        concreteUnsignedSymTypeSafeSymShiftTests (Proxy @(SymWordN 3)),
      testGroup "SymWordN 63" $
        concreteUnsignedSymTypeSafeSymShiftTests (Proxy @(SymWordN 63)),
      testGroup "SymWordN 64" $
        concreteUnsignedSymTypeSafeSymShiftTests (Proxy @(SymWordN 64)),
      testGroup "SymWordN 65" $
        concreteUnsignedSymTypeSafeSymShiftTests (Proxy @(SymWordN 65)),
      testGroup "SymWordN 128" $
        concreteUnsignedSymTypeSafeSymShiftTests (Proxy @(SymWordN 128)),
      testGroup "Int8" $
        concreteSignedAtLeastThreeBitsTypeSafeSymShiftTests (Proxy @Int8),
      testGroup "Int16" $
        concreteSignedAtLeastThreeBitsTypeSafeSymShiftTests (Proxy @Int16),
      testGroup "Int32" $
        concreteSignedAtLeastThreeBitsTypeSafeSymShiftTests (Proxy @Int32),
      testGroup "Int64" $
        concreteSignedAtLeastThreeBitsTypeSafeSymShiftTests (Proxy @Int64),
      testGroup "Int" $
        concreteSignedAtLeastThreeBitsTypeSafeSymShiftTests (Proxy @Int),
      testGroup
        "IntN 1"
        [ testGroup
            "SafeSymShift"
            [ testGroup
                "shift left"
                [ testCase "By 0" $ do
                    safeSymShiftL (-1) 0 @?= (mrgReturn $ -1 :: EM (IntN 1))
                    safeSymStrictShiftL (-1) 0
                      @?= (mrgReturn $ -1 :: EM (IntN 1))
                    safeSymShiftR (-1) 0 @?= (mrgReturn $ -1 :: EM (IntN 1))
                    safeSymStrictShiftR (-1) 0
                      @?= (mrgReturn $ -1 :: EM (IntN 1)),
                  testCase "By -1" $ do
                    safeSymShiftL (-1) (-1 :: IntN 1) @?= overflowError
                    safeSymStrictShiftL (-1) (-1 :: IntN 1) @?= overflowError
                    safeSymShiftR (-1) (-1 :: IntN 1) @?= overflowError
                    safeSymStrictShiftR (-1) (-1 :: IntN 1) @?= overflowError
                ]
            ]
        ],
      testGroup
        "IntN 2"
        [ testGroup
            "SafeSymShift"
            [ testGroup
                "shift left"
                [ testCase "By 0" $ do
                    safeSymShiftL (-1) 0 @?= (mrgReturn $ -1 :: EM (IntN 2))
                    safeSymStrictShiftL (-1) 0
                      @?= (mrgReturn $ -1 :: EM (IntN 2))
                    safeSymShiftR (-1) 0 @?= (mrgReturn $ -1 :: EM (IntN 2))
                    safeSymStrictShiftR (-1) 0
                      @?= (mrgReturn $ -1 :: EM (IntN 2)),
                  testCase "By 1" $ do
                    safeSymShiftL (-1) 1 @?= (mrgReturn $ -2 :: EM (IntN 2))
                    safeSymStrictShiftL (-1) 1
                      @?= (mrgReturn $ -2 :: EM (IntN 2))
                    safeSymShiftR (-1) 1 @?= (mrgReturn $ -1 :: EM (IntN 2))
                    safeSymStrictShiftR (-1) 1
                      @?= (mrgReturn $ -1 :: EM (IntN 2))
                    safeSymShiftR 1 1 @?= (mrgReturn 0 :: EM (IntN 2))
                    safeSymStrictShiftR 1 1 @?= (mrgReturn 0 :: EM (IntN 2)),
                  testCase "By -1" $ do
                    safeSymShiftL (-1) (-1 :: IntN 2) @?= overflowError
                    safeSymStrictShiftL (-1) (-1 :: IntN 2) @?= overflowError
                    safeSymShiftR (-1) (-1 :: IntN 2) @?= overflowError
                    safeSymStrictShiftR (-1) (-1 :: IntN 2) @?= overflowError,
                  testCase "By -2" $ do
                    safeSymShiftL (-1) (-2 :: IntN 2) @?= overflowError
                    safeSymStrictShiftL (-1) (-2 :: IntN 2) @?= overflowError
                    safeSymShiftR (-1) (-2 :: IntN 2) @?= overflowError
                    safeSymStrictShiftR (-1) (-2 :: IntN 2) @?= overflowError
                ]
            ]
        ],
      testGroup "IntN 3" $
        concreteSignedAtLeastThreeBitsTypeSafeSymShiftTests (Proxy @(IntN 3)),
      testGroup "IntN 63" $
        concreteSignedAtLeastThreeBitsTypeSafeSymShiftTests (Proxy @(IntN 63)),
      testGroup "IntN 64" $
        concreteSignedAtLeastThreeBitsTypeSafeSymShiftTests (Proxy @(IntN 64)),
      testGroup "IntN 65" $
        concreteSignedAtLeastThreeBitsTypeSafeSymShiftTests (Proxy @(IntN 65)),
      testGroup "IntN 128" $
        concreteSignedAtLeastThreeBitsTypeSafeSymShiftTests (Proxy @(IntN 128)),
      testGroup "SymIntN 3" $
        concreteSignedAtLeastThreeBitsSymTypeSafeSymShiftTests
          (Proxy @(SymIntN 3)),
      testGroup "SymIntN 63" $
        concreteSignedAtLeastThreeBitsSymTypeSafeSymShiftTests
          (Proxy @(SymIntN 63)),
      testGroup "SymIntN 64" $
        concreteSignedAtLeastThreeBitsSymTypeSafeSymShiftTests
          (Proxy @(SymIntN 64)),
      testGroup "SymIntN 65" $
        concreteSignedAtLeastThreeBitsSymTypeSafeSymShiftTests
          (Proxy @(SymIntN 65)),
      testGroup "SymIntN 128" $
        concreteSignedAtLeastThreeBitsSymTypeSafeSymShiftTests
          (Proxy @(SymIntN 128)),
      testGroup
        "SymIntN 1"
        [ testGroup
            "SafeSymShift"
            [ testGroup
                "shift left"
                [ testCase "By 0" $ do
                    safeSymShiftL (-1) 0 @?= (mrgReturn $ -1 :: EM (SymIntN 1))
                    safeSymStrictShiftL (-1) 0
                      @?= (mrgReturn $ -1 :: EM (SymIntN 1))
                    safeSymShiftR (-1) 0 @?= (mrgReturn $ -1 :: EM (SymIntN 1))
                    safeSymStrictShiftR (-1) 0
                      @?= (mrgReturn $ -1 :: EM (SymIntN 1)),
                  testCase "By -1" $ do
                    safeSymShiftL (-1) (-1 :: SymIntN 1) @?= overflowError
                    safeSymStrictShiftL (-1) (-1 :: SymIntN 1) @?= overflowError
                    safeSymShiftR (-1) (-1 :: SymIntN 1) @?= overflowError
                    safeSymStrictShiftR (-1) (-1 :: SymIntN 1) @?= overflowError
                ]
            ]
        ],
      testGroup
        "SymIntN 2"
        [ testGroup
            "SafeSymShift"
            [ testGroup
                "shift left"
                [ testCase "By 0" $ do
                    safeSymShiftL (-1) 0 @?= (mrgReturn $ -1 :: EM (SymIntN 2))
                    safeSymStrictShiftL (-1) 0
                      @?= (mrgReturn $ -1 :: EM (SymIntN 2))
                    safeSymShiftR (-1) 0 @?= (mrgReturn $ -1 :: EM (SymIntN 2))
                    safeSymStrictShiftR (-1) 0
                      @?= (mrgReturn $ -1 :: EM (SymIntN 2)),
                  testCase "By 1" $ do
                    safeSymShiftL (-1) 1 @?= (mrgReturn $ -2 :: EM (SymIntN 2))
                    safeSymStrictShiftL (-1) 1
                      @?= (mrgReturn $ -2 :: EM (SymIntN 2))
                    safeSymShiftR (-1) 1 @?= (mrgReturn $ -1 :: EM (SymIntN 2))
                    safeSymStrictShiftR (-1) 1
                      @?= (mrgReturn $ -1 :: EM (SymIntN 2))
                    safeSymShiftR 1 1 @?= (mrgReturn 0 :: EM (SymIntN 2))
                    safeSymStrictShiftR 1 1 @?= (mrgReturn 0 :: EM (SymIntN 2)),
                  testCase "By -1" $ do
                    safeSymShiftL (-1) (-1 :: SymIntN 2) @?= overflowError
                    safeSymStrictShiftL (-1) (-1 :: SymIntN 2) @?= overflowError
                    safeSymShiftR (-1) (-1 :: SymIntN 2) @?= overflowError
                    safeSymStrictShiftR (-1) (-1 :: SymIntN 2)
                      @?= overflowError,
                  testCase "By -2" $ do
                    safeSymShiftL (-1) (-2 :: SymIntN 2) @?= overflowError
                    safeSymStrictShiftL (-1) (-2 :: SymIntN 2) @?= overflowError
                    safeSymShiftR (-1) (-2 :: SymIntN 2) @?= overflowError
                    safeSymStrictShiftR (-1) (-2 :: SymIntN 2) @?= overflowError
                ]
            ]
        ]
    ]
