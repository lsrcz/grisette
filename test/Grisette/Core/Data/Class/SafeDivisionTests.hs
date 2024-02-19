{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Core.Data.Class.SafeDivisionTests (safeDivisionTests) where

import Control.DeepSeq (NFData, force)
import Control.Exception (ArithException, catch)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Data (Typeable, typeRep)
import Data.Proxy (Proxy (Proxy))
import GHC.IO (evaluate)
import GHC.Int (Int16, Int32, Int64, Int8)
import GHC.Word (Word16, Word32, Word64, Word8)
import Grisette
  ( BV (bv),
    IntN,
    Mergeable,
    SafeDivision (safeDiv, safeDivMod, safeMod, safeQuot, safeQuotRem, safeRem),
    UnionM,
    WordN,
    mrgPure,
  )
import Grisette.Core.Control.Monad.UnionM (isMerged)
import Grisette.Core.Data.BV (BitwidthMismatch (BitwidthMismatch))
import Grisette.Core.Data.SomeBV
  ( SomeIntN,
    SomeWordN,
    pattern SomeIntN,
    pattern SomeWordN,
  )
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (assertBool, (@?=))
import Test.QuickCheck (Arbitrary, ioProperty)

matches ::
  (NFData r, Eq r', Show r', Mergeable r', Mergeable e, Eq e, Show e) =>
  (t -> t') ->
  (r -> r') ->
  (ArithException -> e) ->
  (t' -> t' -> ExceptT e UnionM r') ->
  (t -> t -> r) ->
  t ->
  t ->
  IO ()
matches wrapInput wrapOutput wrapError f fref x y = do
  rref <-
    (mrgPure . wrapOutput <$> evaluate (force (fref x y)))
      `catch` \(e :: ArithException) -> return $ mrgThrowError $ wrapError e
  let r = f (wrapInput x) (wrapInput y)
  assertBool "Is merged" $ isMerged $ runExceptT r
  r @?= rref

generalOpTestBase ::
  ( NFData r,
    Arbitrary t,
    Show t,
    Eq r',
    Show r',
    Eq r,
    Num t,
    Mergeable r',
    Mergeable e,
    Show e,
    Eq e
  ) =>
  (t -> t') ->
  (r -> r') ->
  (ArithException -> e) ->
  String ->
  (t' -> t' -> ExceptT e UnionM r') ->
  (t -> t -> r) ->
  Test
generalOpTestBase wrapInput wrapOutput wrapError name f fref =
  testGroup
    name
    [ testProperty "random" $ \x y ->
        ioProperty $ matches wrapInput wrapOutput wrapError f fref x y,
      testCase "divided by zero" $
        matches wrapInput wrapOutput wrapError f fref 1 0
    ]

generalOpTest ::
  (NFData r, Arbitrary t, Show t, Eq r, Show r, Eq r, Num t, Mergeable r) =>
  String ->
  (t -> t -> ExceptT ArithException UnionM r) ->
  (t -> t -> r) ->
  Test
generalOpTest = generalOpTestBase id id id

opBoundedTestBase ::
  ( NFData r,
    Arbitrary t,
    Show t,
    Show r',
    Eq r',
    Num t,
    Bounded t,
    Mergeable r',
    Mergeable e,
    Show e,
    Eq e
  ) =>
  (t -> t') ->
  (r -> r') ->
  (ArithException -> e) ->
  String ->
  (t' -> t' -> ExceptT e UnionM r') ->
  (t -> t -> r) ->
  Test
opBoundedTestBase wrapInput wrapOutput wrapError name f fref =
  testGroup
    name
    [ testProperty "random" $ \x y ->
        ioProperty $ matches wrapInput wrapOutput wrapError f fref x y,
      testCase "divided by zero" $
        matches wrapInput wrapOutput wrapError f fref 1 0,
      testCase "minBound/-1" $
        matches wrapInput wrapOutput wrapError f fref minBound (-1)
    ]

opBoundedTest ::
  ( NFData r,
    Arbitrary t,
    Show t,
    Show r,
    Eq r,
    Num t,
    Bounded t,
    Mergeable r
  ) =>
  String ->
  (t -> t -> ExceptT ArithException UnionM r) ->
  (t -> t -> r) ->
  Test
opBoundedTest = opBoundedTestBase id id id

type OpTestFunc t =
  forall r.
  (Eq r, Show r, Eq r, NFData r, Mergeable r) =>
  String ->
  (t -> t -> ExceptT ArithException UnionM r) ->
  (t -> t -> r) ->
  Test

testType ::
  forall t.
  ( NFData t,
    Show t,
    SafeDivision ArithException t (ExceptT ArithException UnionM),
    Mergeable t,
    Integral t,
    Typeable t
  ) =>
  OpTestFunc t ->
  Proxy t ->
  Test
testType testFunc p =
  testGroup
    (show $ typeRep p)
    [ testFunc "div" safeDiv (div @t),
      testFunc "mod" safeMod (mod @t),
      testFunc "divMod" safeDivMod (divMod @t),
      testFunc "quot" safeQuot (quot @t),
      testFunc "rem" safeRem (rem @t),
      testFunc "quotRem" safeQuotRem (quotRem @t)
    ]

-- type SomeOpTestFunc t t' =
--   forall r r'.
--   (Eq r, Show r, Eq r, NFData r, Mergeable r) =>
--   String ->
--   (t' -> t' -> ExceptT ArithException UnionM r') ->
--   (t -> t -> r) ->
--   Test
--
-- testSomeType ::
--   forall t t'.
--   ( NFData t,
--     Show t,
--     SafeDivision ArithException t' (ExceptT ArithException UnionM),
--     Mergeable t,
--     Integral t,
--     Typeable t'
--   ) =>
--   SomeDivTestFunc t t' ->
--   SomeDivTestFunc t t' ->
--   Proxy t ->
--   Proxy t' ->
--   Test
-- testSomeType divQuotTest modRemTest _ p =
--   testGroup
--     (show $ typeRep p)
--     [ divQuotTest "div" safeDiv (div @t),
--       modRemTest "mod" safeMod (mod @t),
--       divQuotTest "divMod" safeDivMod (divMod @t),
--       divQuotTest "quot" safeQuot (quot @t),
--       modRemTest "rem" safeRem (rem @t),
--       modRemTest "quotRem" safeQuotRem (quotRem @t)
--     ]

safeDivisionTests :: Test
safeDivisionTests =
  testGroup
    "SafeDivision"
    [ testType generalOpTest (Proxy :: Proxy Integer),
      testType opBoundedTest (Proxy :: Proxy Int8),
      testType opBoundedTest (Proxy :: Proxy Int16),
      testType opBoundedTest (Proxy :: Proxy Int32),
      testType opBoundedTest (Proxy :: Proxy Int64),
      testType opBoundedTest (Proxy :: Proxy Int),
      testType opBoundedTest (Proxy :: Proxy (IntN 8)),
      testType opBoundedTest (Proxy :: Proxy Word),
      testType opBoundedTest (Proxy :: Proxy Word8),
      testType opBoundedTest (Proxy :: Proxy Word16),
      testType opBoundedTest (Proxy :: Proxy Word32),
      testType opBoundedTest (Proxy :: Proxy Word64),
      testType opBoundedTest (Proxy :: Proxy (WordN 8)),
      testGroup "SomeWordN" $ do
        let singleOutputTest =
              opBoundedTestBase
                SomeWordN
                SomeWordN
                (\e -> Right e :: Either BitwidthMismatch ArithException)
        let doubleOutputTest =
              opBoundedTestBase
                SomeWordN
                (bimap SomeWordN SomeWordN)
                (\e -> Right e :: Either BitwidthMismatch ArithException)
        [ singleOutputTest "div" safeDiv (div @(WordN 8)),
          singleOutputTest "mod" safeMod (mod @(WordN 8)),
          doubleOutputTest "divMod" safeDivMod (divMod @(WordN 8)),
          singleOutputTest "quot" safeQuot (quot @(WordN 8)),
          singleOutputTest "rem" safeRem (rem @(WordN 8)),
          doubleOutputTest "quotRem" safeQuotRem (quotRem @(WordN 8)),
          testCase "Bitwidth mismatch" $ do
            let actual =
                  safeDiv (bv 10 2) (bv 11 3) ::
                    ExceptT
                      (Either BitwidthMismatch ArithException)
                      UnionM
                      SomeWordN
            let expected = mrgThrowError $ Left BitwidthMismatch
            actual @?= expected
          ],
      testGroup "SomeIntN" $ do
        let singleOutputTest =
              opBoundedTestBase
                SomeIntN
                SomeIntN
                (\e -> Right e :: Either BitwidthMismatch ArithException)
        let doubleOutputTest =
              opBoundedTestBase
                SomeIntN
                (bimap SomeIntN SomeIntN)
                (\e -> Right e :: Either BitwidthMismatch ArithException)
        [ singleOutputTest "div" safeDiv (div @(IntN 8)),
          singleOutputTest "mod" safeMod (mod @(IntN 8)),
          doubleOutputTest "divMod" safeDivMod (divMod @(IntN 8)),
          singleOutputTest "quot" safeQuot (quot @(IntN 8)),
          singleOutputTest "rem" safeRem (rem @(IntN 8)),
          doubleOutputTest "quotRem" safeQuotRem (quotRem @(IntN 8)),
          testCase "Bitwidth mismatch" $ do
            let actual =
                  safeDiv (bv 10 2) (bv 11 3) ::
                    ExceptT
                      (Either BitwidthMismatch ArithException)
                      UnionM
                      SomeIntN
            let expected = mrgThrowError $ Left BitwidthMismatch
            actual @?= expected
          ]
    ]
