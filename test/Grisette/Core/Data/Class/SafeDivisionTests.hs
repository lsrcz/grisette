{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Core.Data.Class.SafeDivisionTests (safeDivisionTests) where

import Control.DeepSeq (NFData, force)
import Control.Exception (ArithException, catch)
import Data.Data (Typeable, typeRep)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Proxy (Proxy (Proxy))
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.IO (evaluate)
import Grisette
  ( IntN,
    SafeDivision (safeDiv, safeDivMod, safeMod, safeQuot, safeQuotRem, safeRem),
    WordN,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@?=))
import Test.QuickCheck (Arbitrary, ioProperty)

matches ::
  (NFData a, Eq a, Show a) =>
  (t -> t -> Either ArithException a) ->
  (t -> t -> a) ->
  t ->
  t ->
  IO ()
matches f fref x y = do
  rref <-
    (Right <$> evaluate (force (fref x y)))
      `catch` \(e :: ArithException) -> return $ Left e
  f x y @?= rref

generalOpTest ::
  (NFData r, Arbitrary t, Show t, Eq r, Show r, Eq r, Num t) =>
  String ->
  (t -> t -> Either ArithException r) ->
  (t -> t -> r) ->
  Test
generalOpTest name f fref =
  testGroup
    name
    [ testProperty "random" $ \x y -> ioProperty $ matches f fref x y,
      testCase "divided by zero" $ matches f fref 1 0
    ]

opBoundedSignedTest ::
  (NFData a, Arbitrary t, Show t, Eq a, Show a, Eq a, Num t, Bounded t) =>
  String ->
  (t -> t -> Either ArithException a) ->
  (t -> t -> a) ->
  Test
opBoundedSignedTest name f fref =
  testGroup
    name
    [ testProperty "random" $ \x y -> ioProperty $ matches f fref x y,
      testCase "divided by zero" $ matches f fref 1 0,
      testCase "minBound/-1" $ matches f fref minBound (-1)
    ]

type OpTestFunc t =
  forall r.
  (Eq r, Show r, Eq r, NFData r) =>
  String ->
  (t -> t -> Either ArithException r) ->
  (t -> t -> r) ->
  Test

testType ::
  forall t.
  ( NFData t,
    Show t,
    SafeDivision ArithException t (Either ArithException),
    Integral t,
    Typeable t
  ) =>
  OpTestFunc t ->
  OpTestFunc t ->
  Proxy t ->
  Test
testType divQuotTest modRemTest p =
  testGroup
    (show $ typeRep p)
    [ divQuotTest "div" safeDiv (div @t),
      modRemTest "mod" safeMod (mod @t),
      divQuotTest "divMod" safeDivMod (divMod @t),
      divQuotTest "quot" safeQuot (quot @t),
      modRemTest "rem" safeRem (rem @t),
      modRemTest "quotRem" safeQuotRem (quotRem @t)
    ]

safeDivisionTests :: Test
safeDivisionTests =
  testGroup
    "SafeDivision"
    [ testType generalOpTest generalOpTest (Proxy :: Proxy Integer),
      testType opBoundedSignedTest generalOpTest (Proxy :: Proxy Int8),
      testType opBoundedSignedTest generalOpTest (Proxy :: Proxy Int16),
      testType opBoundedSignedTest generalOpTest (Proxy :: Proxy Int32),
      testType opBoundedSignedTest generalOpTest (Proxy :: Proxy Int64),
      testType opBoundedSignedTest generalOpTest (Proxy :: Proxy Int),
      testType opBoundedSignedTest generalOpTest (Proxy :: Proxy (IntN 8)),
      testType generalOpTest generalOpTest (Proxy :: Proxy Word),
      testType generalOpTest generalOpTest (Proxy :: Proxy Word8),
      testType generalOpTest generalOpTest (Proxy :: Proxy Word16),
      testType generalOpTest generalOpTest (Proxy :: Proxy Word32),
      testType generalOpTest generalOpTest (Proxy :: Proxy Word64),
      testType generalOpTest generalOpTest (Proxy :: Proxy (WordN 8))
    ]
