{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Core.Data.Class.SymShiftTests (symShiftTests) where

import Data.Bits (Bits (shift), FiniteBits (finiteBitSize))
import Data.Data (Proxy (Proxy), Typeable, typeRep)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import Grisette
  ( IntN,
    Solvable (con),
    SymIntN,
    SymShift (symShift, symShiftNegated),
    SymWordN,
    WordN,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@?=))
import Test.QuickCheck (Arbitrary, chooseInt, forAll, ioProperty)

concreteUnsignedTypeSymShiftTests ::
  forall proxy a.
  ( Arbitrary a,
    Show a,
    Num a,
    SymShift a,
    FiniteBits a,
    Bounded a,
    Typeable a
  ) =>
  proxy a ->
  Test
concreteUnsignedTypeSymShiftTests p =
  testGroup
    (show $ typeRep p)
    [ testGroup
        "SymShift"
        [ testProperty "symShift" $ \(x :: a) ->
            forAll (chooseInt (0, finiteBitSize x)) $
              \(s :: Int) ->
                ioProperty $
                  symShift x (fromIntegral s) @?= shift x s,
          testProperty "symShiftNegated" $ \(x :: a) ->
            forAll (chooseInt (0, finiteBitSize x)) $
              \(s :: Int) ->
                ioProperty $
                  symShiftNegated x (fromIntegral s) @?= shift x (-s),
          testProperty "symShift max" $ \(x :: a) ->
            ioProperty $ symShift x maxBound @?= 0,
          testProperty "symShiftNegated max" $ \(x :: a) ->
            ioProperty $ symShiftNegated x maxBound @?= 0
        ]
    ]

concreteSignedTypeSymShiftTests ::
  forall proxy a.
  ( Arbitrary a,
    Show a,
    SymShift a,
    FiniteBits a,
    Bounded a,
    Typeable a,
    Integral a
  ) =>
  proxy a ->
  Test
concreteSignedTypeSymShiftTests p =
  testGroup
    (show $ typeRep p)
    [ testGroup
        "SymShift"
        [ testProperty "symShift" $ \(x :: a) ->
            forAll (chooseInt (-finiteBitSize x, finiteBitSize x)) $
              \(s :: Int) -> ioProperty $ do
                symShift x (fromIntegral s)
                  @?= shift x (fromIntegral (fromIntegral s :: a)),
          testProperty "symShiftNegated" $ \(x :: a) ->
            forAll (chooseInt (-finiteBitSize x, finiteBitSize x)) $
              \(s :: Int) -> ioProperty $ do
                symShiftNegated x (fromIntegral s)
                  @?= shift x (-fromIntegral (fromIntegral s :: a)),
          testProperty "symShift max" $ \(x :: a) ->
            ioProperty $ do
              case finiteBitSize x of
                1 -> symShift x maxBound @?= x
                2 -> symShift x maxBound @?= shift x 1
                _ -> symShift x maxBound @?= 0,
          testProperty "symShiftNegated max" $ \(x :: a) ->
            ioProperty $ do
              case finiteBitSize x of
                1 -> symShiftNegated x maxBound @?= x
                2 -> symShiftNegated x maxBound @?= shift x (-1)
                _ -> symShiftNegated x maxBound @?= if x >= 0 then 0 else -1,
          testProperty "symShift min" $ \(x :: a) ->
            ioProperty $ do
              case finiteBitSize x of
                1 -> symShift x minBound @?= shift x (-1)
                2 -> symShift x minBound @?= shift x (-2)
                _ -> symShift x minBound @?= if x >= 0 then 0 else -1,
          testProperty "symShiftNegated min" $ \(x :: a) ->
            ioProperty $ symShiftNegated x minBound @?= 0
        ]
    ]

symbolicTypeSymShiftTests ::
  forall proxy c s.
  ( Arbitrary c,
    Show s,
    Num s,
    SymShift c,
    SymShift s,
    FiniteBits c,
    Bounded c,
    Typeable s,
    Integral c,
    Solvable c s,
    Show c
  ) =>
  proxy s ->
  Test
symbolicTypeSymShiftTests p =
  testGroup
    (show $ typeRep p)
    [ testGroup
        "SymShift"
        [ testProperty "concrete/concrete symShift" $ \(x :: c) ->
            forAll (chooseInt (-finiteBitSize x, finiteBitSize x)) $
              \(s :: Int) ->
                ioProperty $
                  symShift (con x :: s) (fromIntegral s)
                    @?= con (symShift x (fromIntegral s)),
          testProperty "concrete/concrete symShiftNegated" $ \(x :: c) ->
            forAll (chooseInt (-finiteBitSize x, finiteBitSize x)) $
              \(s :: Int) ->
                ioProperty $
                  symShiftNegated (con x :: s) (fromIntegral s)
                    @?= con (symShiftNegated x (fromIntegral s)),
          testProperty "symShift max" $ \(x :: c) ->
            ioProperty $
              symShift (con x :: s) (con maxBound)
                @?= con (symShift x maxBound),
          testProperty "symShiftNegated max" $ \(x :: c) ->
            ioProperty $
              symShiftNegated (con x :: s) (con maxBound)
                @?= con (symShiftNegated x maxBound),
          testProperty "symShift min" $ \(x :: c) ->
            ioProperty $ do
              symShift (con x :: s) (con minBound)
                @?= con (symShift x minBound),
          testProperty "symShiftNegated min" $ \(x :: c) ->
            ioProperty $ do
              symShiftNegated (con x :: s) (con minBound)
                @?= con (symShiftNegated x minBound)
        ]
    ]

symShiftTests :: Test
symShiftTests =
  testGroup
    "SymShift"
    [ concreteUnsignedTypeSymShiftTests (Proxy :: Proxy Word8),
      concreteUnsignedTypeSymShiftTests (Proxy :: Proxy Word16),
      concreteUnsignedTypeSymShiftTests (Proxy :: Proxy Word32),
      concreteUnsignedTypeSymShiftTests (Proxy :: Proxy Word64),
      concreteUnsignedTypeSymShiftTests (Proxy :: Proxy Word),
      concreteUnsignedTypeSymShiftTests (Proxy :: Proxy (WordN 1)),
      concreteUnsignedTypeSymShiftTests (Proxy :: Proxy (WordN 2)),
      concreteUnsignedTypeSymShiftTests (Proxy :: Proxy (WordN 3)),
      concreteUnsignedTypeSymShiftTests (Proxy :: Proxy (WordN 63)),
      concreteUnsignedTypeSymShiftTests (Proxy :: Proxy (WordN 64)),
      concreteUnsignedTypeSymShiftTests (Proxy :: Proxy (WordN 65)),
      concreteUnsignedTypeSymShiftTests (Proxy :: Proxy (WordN 128)),
      concreteSignedTypeSymShiftTests (Proxy :: Proxy Int8),
      concreteSignedTypeSymShiftTests (Proxy :: Proxy Int16),
      concreteSignedTypeSymShiftTests (Proxy :: Proxy Int32),
      concreteSignedTypeSymShiftTests (Proxy :: Proxy Int64),
      concreteSignedTypeSymShiftTests (Proxy :: Proxy Int),
      concreteSignedTypeSymShiftTests (Proxy :: Proxy (IntN 1)),
      concreteSignedTypeSymShiftTests (Proxy :: Proxy (IntN 2)),
      concreteSignedTypeSymShiftTests (Proxy :: Proxy (IntN 3)),
      concreteSignedTypeSymShiftTests (Proxy :: Proxy (IntN 63)),
      concreteSignedTypeSymShiftTests (Proxy :: Proxy (IntN 64)),
      concreteSignedTypeSymShiftTests (Proxy :: Proxy (IntN 65)),
      concreteSignedTypeSymShiftTests (Proxy :: Proxy (IntN 128)),
      symbolicTypeSymShiftTests (Proxy :: Proxy (SymWordN 1)),
      symbolicTypeSymShiftTests (Proxy :: Proxy (SymWordN 2)),
      symbolicTypeSymShiftTests (Proxy :: Proxy (SymWordN 3)),
      symbolicTypeSymShiftTests (Proxy :: Proxy (SymWordN 63)),
      symbolicTypeSymShiftTests (Proxy :: Proxy (SymWordN 64)),
      symbolicTypeSymShiftTests (Proxy :: Proxy (SymWordN 65)),
      symbolicTypeSymShiftTests (Proxy :: Proxy (SymWordN 128)),
      symbolicTypeSymShiftTests (Proxy :: Proxy (SymIntN 1)),
      symbolicTypeSymShiftTests (Proxy :: Proxy (SymIntN 2)),
      symbolicTypeSymShiftTests (Proxy :: Proxy (SymIntN 3)),
      symbolicTypeSymShiftTests (Proxy :: Proxy (SymIntN 63)),
      symbolicTypeSymShiftTests (Proxy :: Proxy (SymIntN 64)),
      symbolicTypeSymShiftTests (Proxy :: Proxy (SymIntN 65)),
      symbolicTypeSymShiftTests (Proxy :: Proxy (SymIntN 128))
    ]
