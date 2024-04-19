{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Core.Data.Class.SymRotateTests (symRotateTests) where

import Data.Bits (Bits (rotate), FiniteBits (finiteBitSize))
import Data.Data (Proxy (Proxy), Typeable, typeRep)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import Grisette
  ( IntN,
    LinkedRep,
    Solvable (con),
    SymIntN,
    SymRotate (symRotate, symRotateNegated),
    SymWordN,
    WordN,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (Assertion, (@?=))
import Test.QuickCheck (Arbitrary, chooseInt, forAll, ioProperty)

concreteRotateIsCorrect ::
  (SymRotate a, Show a, Integral a, FiniteBits a) =>
  a ->
  a ->
  Assertion
concreteRotateIsCorrect a s =
  symRotate a s
    @?= rotate
      a
      ( fromIntegral $
          (fromIntegral s :: Integer) `mod` fromIntegral (finiteBitSize a)
      )

concreteRotateNegatedIsCorrect ::
  (SymRotate a, Show a, Integral a, FiniteBits a) =>
  a ->
  a ->
  Assertion
concreteRotateNegatedIsCorrect a s =
  symRotateNegated a s
    @?= rotate
      a
      ( fromIntegral $
          (-fromIntegral s :: Integer) `mod` fromIntegral (finiteBitSize a)
      )

concreteUnsignedTypeSymRotateTests ::
  forall proxy a.
  ( Arbitrary a,
    Show a,
    Num a,
    Eq a,
    SymRotate a,
    FiniteBits a,
    Bounded a,
    Typeable a,
    Integral a
  ) =>
  proxy a ->
  Test
concreteUnsignedTypeSymRotateTests p =
  testGroup
    (show $ typeRep p)
    [ testGroup
        "SymRotate"
        [ testProperty "symRotate" $ \(x :: a) ->
            forAll (chooseInt (0, finiteBitSize x)) $
              \(s :: Int) ->
                ioProperty $ concreteRotateIsCorrect x (fromIntegral s),
          testProperty "symRotateNegated" $ \(x :: a) ->
            forAll (chooseInt (0, finiteBitSize x)) $
              \(s :: Int) ->
                ioProperty $ concreteRotateNegatedIsCorrect x (fromIntegral s),
          testProperty "symRotate max" $ \(x :: a) ->
            ioProperty $ concreteRotateIsCorrect x maxBound,
          testProperty "symRotateNegated max" $ \(x :: a) ->
            ioProperty $ concreteRotateNegatedIsCorrect x maxBound
        ]
    ]

concreteSignedTypeSymRotateTests ::
  forall proxy a.
  ( Arbitrary a,
    Show a,
    Num a,
    Eq a,
    SymRotate a,
    FiniteBits a,
    Bounded a,
    Typeable a,
    Integral a
  ) =>
  proxy a ->
  Test
concreteSignedTypeSymRotateTests p =
  testGroup
    (show $ typeRep p)
    [ testGroup
        "SymRotate"
        [ testProperty "symRotate" $ \(x :: a) ->
            forAll (chooseInt (-finiteBitSize x, finiteBitSize x)) $
              \(s :: Int) ->
                ioProperty $ concreteRotateIsCorrect x (fromIntegral s),
          testProperty "symRotateNegated" $ \(x :: a) ->
            forAll (chooseInt (-finiteBitSize x, finiteBitSize x)) $
              \(s :: Int) ->
                ioProperty $ concreteRotateNegatedIsCorrect x (fromIntegral s),
          testProperty "symRotate max" $ \(x :: a) ->
            ioProperty $ concreteRotateIsCorrect x maxBound,
          testProperty "symRotateNegated max" $ \(x :: a) ->
            ioProperty $ concreteRotateNegatedIsCorrect x maxBound,
          testProperty "symRotate min" $ \(x :: a) ->
            ioProperty $ concreteRotateIsCorrect x minBound,
          testProperty "symRotateNegated min" $ \(x :: a) ->
            ioProperty $ concreteRotateNegatedIsCorrect x minBound
        ]
    ]

symbolicTypeSymRotateTests ::
  forall proxy c s.
  ( Arbitrary c,
    Show s,
    Num s,
    Eq s,
    SymRotate c,
    SymRotate s,
    FiniteBits c,
    FiniteBits s,
    Bounded c,
    Typeable s,
    Integral c,
    LinkedRep c s,
    Solvable c s
  ) =>
  proxy s ->
  Test
symbolicTypeSymRotateTests p =
  testGroup
    (show $ typeRep p)
    [ testGroup
        "SymRotate"
        [ testProperty "concrete/concrete symRotate" $ \(x :: c) ->
            forAll (chooseInt (-finiteBitSize x, finiteBitSize x)) $
              \(s :: Int) ->
                ioProperty $
                  symRotate (con x :: s) (fromIntegral s)
                    @?= con (symRotate x (fromIntegral s)),
          testProperty "symRotate max" $ \(x :: c) ->
            ioProperty $
              symRotate (con x :: s) (con maxBound)
                @?= con (symRotate x maxBound),
          testProperty "symRotate min" $ \(x :: c) ->
            ioProperty $ do
              symRotate (con x :: s) (con minBound)
                @?= con (symRotate x minBound)
        ]
    ]

symRotateTests :: Test
symRotateTests =
  testGroup
    "SymRotate"
    [ concreteUnsignedTypeSymRotateTests (Proxy :: Proxy Word8),
      concreteUnsignedTypeSymRotateTests (Proxy :: Proxy Word16),
      concreteUnsignedTypeSymRotateTests (Proxy :: Proxy Word32),
      concreteUnsignedTypeSymRotateTests (Proxy :: Proxy Word64),
      concreteUnsignedTypeSymRotateTests (Proxy :: Proxy Word),
      concreteUnsignedTypeSymRotateTests (Proxy :: Proxy (WordN 1)),
      concreteUnsignedTypeSymRotateTests (Proxy :: Proxy (WordN 2)),
      concreteUnsignedTypeSymRotateTests (Proxy :: Proxy (WordN 3)),
      concreteUnsignedTypeSymRotateTests (Proxy :: Proxy (WordN 63)),
      concreteUnsignedTypeSymRotateTests (Proxy :: Proxy (WordN 64)),
      concreteUnsignedTypeSymRotateTests (Proxy :: Proxy (WordN 65)),
      concreteUnsignedTypeSymRotateTests (Proxy :: Proxy (WordN 128)),
      concreteSignedTypeSymRotateTests (Proxy :: Proxy Int8),
      concreteSignedTypeSymRotateTests (Proxy :: Proxy Int16),
      concreteSignedTypeSymRotateTests (Proxy :: Proxy Int32),
      concreteSignedTypeSymRotateTests (Proxy :: Proxy Int64),
      concreteSignedTypeSymRotateTests (Proxy :: Proxy Int),
      concreteSignedTypeSymRotateTests (Proxy :: Proxy (IntN 1)),
      concreteSignedTypeSymRotateTests (Proxy :: Proxy (IntN 2)),
      concreteSignedTypeSymRotateTests (Proxy :: Proxy (IntN 3)),
      concreteSignedTypeSymRotateTests (Proxy :: Proxy (IntN 63)),
      concreteSignedTypeSymRotateTests (Proxy :: Proxy (IntN 64)),
      concreteSignedTypeSymRotateTests (Proxy :: Proxy (IntN 65)),
      concreteSignedTypeSymRotateTests (Proxy :: Proxy (IntN 128)),
      symbolicTypeSymRotateTests (Proxy :: Proxy (SymWordN 1)),
      symbolicTypeSymRotateTests (Proxy :: Proxy (SymWordN 2)),
      symbolicTypeSymRotateTests (Proxy :: Proxy (SymWordN 3)),
      symbolicTypeSymRotateTests (Proxy :: Proxy (SymWordN 63)),
      symbolicTypeSymRotateTests (Proxy :: Proxy (SymWordN 64)),
      symbolicTypeSymRotateTests (Proxy :: Proxy (SymWordN 65)),
      symbolicTypeSymRotateTests (Proxy :: Proxy (SymWordN 128)),
      symbolicTypeSymRotateTests (Proxy :: Proxy (SymIntN 1)),
      symbolicTypeSymRotateTests (Proxy :: Proxy (SymIntN 2)),
      symbolicTypeSymRotateTests (Proxy :: Proxy (SymIntN 3)),
      symbolicTypeSymRotateTests (Proxy :: Proxy (SymIntN 63)),
      symbolicTypeSymRotateTests (Proxy :: Proxy (SymIntN 64)),
      symbolicTypeSymRotateTests (Proxy :: Proxy (SymIntN 65)),
      symbolicTypeSymRotateTests (Proxy :: Proxy (SymIntN 128))
    ]
