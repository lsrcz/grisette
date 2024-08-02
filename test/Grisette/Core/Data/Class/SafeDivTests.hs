{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Core.Data.Class.SafeDivTests (safeDivTests) where

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
    BitwidthMismatch (BitwidthMismatch),
    IntN,
    Mergeable,
    SafeDiv (safeDiv, safeDivMod, safeMod, safeQuot, safeQuotRem, safeRem),
    SomeIntN,
    SomeWordN,
    Union,
    WordN,
    mrgPure,
    pattern SomeIntN,
    pattern SomeWordN,
  )
import Grisette.Internal.Core.Control.Monad.Union (isMerged)
import Grisette.Internal.Core.Data.Class.SafeDiv
  ( DivOr (divModOr, divOr, modOr, quotOr, quotRemOr, remOr),
  )
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (assertBool, (@?=))
import Test.QuickCheck (Arbitrary, ioProperty)

divOrMatches ::
  (NFData r, Eq r', Show r') =>
  (t -> t') ->
  (r -> r') ->
  (r' -> t' -> t' -> r') ->
  (t -> t -> r) ->
  r ->
  t ->
  t ->
  IO ()
divOrMatches wrapInput wrapOutput f fref d x y = do
  rref <-
    (wrapOutput <$> evaluate (force (fref x y)))
      `catch` \(_ :: ArithException) -> return $ wrapOutput d
  let r = f (wrapOutput d) (wrapInput x) (wrapInput y)
  r @?= rref

generalOpDivOrTestBase ::
  ( NFData r,
    Arbitrary t,
    Show t,
    Arbitrary r,
    Show r,
    Eq r',
    Show r',
    Num t
  ) =>
  (t -> t') ->
  (r -> r') ->
  String ->
  (r' -> t' -> t' -> r') ->
  (t -> t -> r) ->
  Test
generalOpDivOrTestBase wrapInput wrapOutput name f fref =
  testGroup
    name
    [ testProperty "random" $ \d x y ->
        ioProperty $ divOrMatches wrapInput wrapOutput f fref d x y,
      testProperty "divided by zero" $ \d ->
        ioProperty $ divOrMatches wrapInput wrapOutput f fref d 1 0
    ]

generalOpDivOrTest ::
  ( NFData r,
    Arbitrary t,
    Show t,
    Arbitrary r,
    Show r,
    Eq r,
    Num t
  ) =>
  String ->
  (r -> t -> t -> r) ->
  (t -> t -> r) ->
  Test
generalOpDivOrTest = generalOpDivOrTestBase id id

opBoundedDivOrTestBase ::
  ( NFData r,
    Arbitrary t,
    Show t,
    Arbitrary r,
    Show r,
    Eq r',
    Show r',
    Bounded t,
    Num t
  ) =>
  (t -> t') ->
  (r -> r') ->
  String ->
  (r' -> t' -> t' -> r') ->
  (t -> t -> r) ->
  Test
opBoundedDivOrTestBase wrapInput wrapOutput name f fref =
  testGroup
    name
    [ testProperty "random" $ \d x y ->
        ioProperty $ divOrMatches wrapInput wrapOutput f fref d x y,
      testProperty "divided by zero" $ \d ->
        ioProperty $ divOrMatches wrapInput wrapOutput f fref d 1 0,
      testProperty "minBound/-1" $ \d ->
        ioProperty $ divOrMatches wrapInput wrapOutput f fref d minBound (-1)
    ]

opBoundedDivOrTest ::
  ( NFData r,
    Arbitrary t,
    Show t,
    Arbitrary r,
    Show r,
    Eq r,
    Bounded t,
    Num t
  ) =>
  String ->
  (r -> t -> t -> r) ->
  (t -> t -> r) ->
  Test
opBoundedDivOrTest = opBoundedDivOrTestBase id id

safeDivMatches ::
  (NFData r, Eq r', Show r', Mergeable r', Mergeable e, Eq e, Show e) =>
  (t -> t') ->
  (r -> r') ->
  (ArithException -> e) ->
  (t' -> t' -> ExceptT e Union r') ->
  (t -> t -> r) ->
  t ->
  t ->
  IO ()
safeDivMatches wrapInput wrapOutput wrapError f fref x y = do
  rref <-
    (mrgPure . wrapOutput <$> evaluate (force (fref x y)))
      `catch` \(e :: ArithException) -> return $ mrgThrowError $ wrapError e
  let r = f (wrapInput x) (wrapInput y)
  assertBool "Is merged" $ isMerged $ runExceptT r
  r @?= rref

generalOpSafeDivTestBase ::
  ( NFData r,
    Arbitrary t,
    Show t,
    Eq r',
    Show r',
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
  (t' -> t' -> ExceptT e Union r') ->
  (t -> t -> r) ->
  Test
generalOpSafeDivTestBase wrapInput wrapOutput wrapError name f fref =
  testGroup
    name
    [ testProperty "random" $ \x y ->
        ioProperty $ safeDivMatches wrapInput wrapOutput wrapError f fref x y,
      testCase "divided by zero" $
        safeDivMatches wrapInput wrapOutput wrapError f fref 1 0
    ]

generalOpSafeDivTest ::
  (NFData r, Arbitrary t, Show t, Show r, Eq r, Num t, Mergeable r) =>
  String ->
  (t -> t -> ExceptT ArithException Union r) ->
  (t -> t -> r) ->
  Test
generalOpSafeDivTest = generalOpSafeDivTestBase id id id

opBoundedSafeDivTestBase ::
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
  (t' -> t' -> ExceptT e Union r') ->
  (t -> t -> r) ->
  Test
opBoundedSafeDivTestBase wrapInput wrapOutput wrapError name f fref =
  testGroup
    name
    [ testProperty "random" $ \x y ->
        ioProperty $ safeDivMatches wrapInput wrapOutput wrapError f fref x y,
      testCase "divided by zero" $
        safeDivMatches wrapInput wrapOutput wrapError f fref 1 0,
      testCase "minBound/-1" $
        safeDivMatches wrapInput wrapOutput wrapError f fref minBound (-1)
    ]

opBoundedSafeDivTest ::
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
  (t -> t -> ExceptT ArithException Union r) ->
  (t -> t -> r) ->
  Test
opBoundedSafeDivTest = opBoundedSafeDivTestBase id id id

type OpSafeDivTestFunc t =
  forall r.
  (Eq r, Show r, Eq r, NFData r, Mergeable r) =>
  String ->
  (t -> t -> ExceptT ArithException Union r) ->
  (t -> t -> r) ->
  Test

type OpDivOrTestFunc t =
  forall r.
  (Eq r, Show r, Eq r, NFData r, Mergeable r, Arbitrary r) =>
  String ->
  (r -> t -> t -> r) ->
  (t -> t -> r) ->
  Test

testType ::
  forall t.
  ( NFData t,
    Show t,
    SafeDiv ArithException t (ExceptT ArithException Union),
    Integral t,
    Typeable t,
    Arbitrary t
  ) =>
  OpSafeDivTestFunc t ->
  OpDivOrTestFunc t ->
  Proxy t ->
  Test
testType safeDivTestFunc divOrTestFunc p =
  testGroup
    (show $ typeRep p)
    [ divOrTestFunc "divOr" divOr (div @t),
      divOrTestFunc "modOr" modOr (mod @t),
      divOrTestFunc "divModOr" divModOr (divMod @t),
      divOrTestFunc "quotOr" quotOr (quot @t),
      divOrTestFunc "remOr" remOr (rem @t),
      divOrTestFunc "quotRemOr" quotRemOr (quotRem @t),
      safeDivTestFunc "safeDiv" safeDiv (div @t),
      safeDivTestFunc "safeMod" safeMod (mod @t),
      safeDivTestFunc "safeDivMod" safeDivMod (divMod @t),
      safeDivTestFunc "safeQuot" safeQuot (quot @t),
      safeDivTestFunc "safeRem" safeRem (rem @t),
      safeDivTestFunc "safeQuotRem" safeQuotRem (quotRem @t)
    ]

safeDivTests :: Test
safeDivTests =
  testGroup
    "SafeDiv"
    [ testType generalOpSafeDivTest generalOpDivOrTest (Proxy :: Proxy Integer),
      testType opBoundedSafeDivTest opBoundedDivOrTest (Proxy :: Proxy Int8),
      testType opBoundedSafeDivTest opBoundedDivOrTest (Proxy :: Proxy Int16),
      testType opBoundedSafeDivTest opBoundedDivOrTest (Proxy :: Proxy Int32),
      testType opBoundedSafeDivTest opBoundedDivOrTest (Proxy :: Proxy Int64),
      testType opBoundedSafeDivTest opBoundedDivOrTest (Proxy :: Proxy Int),
      testType
        opBoundedSafeDivTest
        opBoundedDivOrTest
        (Proxy :: Proxy (IntN 8)),
      testType opBoundedSafeDivTest opBoundedDivOrTest (Proxy :: Proxy Word),
      testType opBoundedSafeDivTest opBoundedDivOrTest (Proxy :: Proxy Word8),
      testType opBoundedSafeDivTest opBoundedDivOrTest (Proxy :: Proxy Word16),
      testType opBoundedSafeDivTest opBoundedDivOrTest (Proxy :: Proxy Word32),
      testType opBoundedSafeDivTest opBoundedDivOrTest (Proxy :: Proxy Word64),
      testType
        opBoundedSafeDivTest
        opBoundedDivOrTest
        (Proxy :: Proxy (WordN 8)),
      testGroup "SomeWordN" $ do
        let singleOutputDivOrTest =
              opBoundedDivOrTestBase
                SomeWordN
                SomeWordN
        let doubleOutputDivOrTest =
              opBoundedDivOrTestBase
                SomeWordN
                (bimap SomeWordN SomeWordN)
        let singleOutputSafeDivTest =
              opBoundedSafeDivTestBase
                SomeWordN
                SomeWordN
                (\e -> Right e :: Either BitwidthMismatch ArithException)
        let doubleOutputSafeDivTest =
              opBoundedSafeDivTestBase
                SomeWordN
                (bimap SomeWordN SomeWordN)
                (\e -> Right e :: Either BitwidthMismatch ArithException)
        [ singleOutputDivOrTest "divOr" divOr (div @(WordN 8)),
          singleOutputDivOrTest "modOr" modOr (mod @(WordN 8)),
          doubleOutputDivOrTest "divModOr" divModOr (divMod @(WordN 8)),
          singleOutputDivOrTest "quotOr" quotOr (quot @(WordN 8)),
          singleOutputDivOrTest "remOr" remOr (rem @(WordN 8)),
          doubleOutputDivOrTest "quotRemOr" quotRemOr (quotRem @(WordN 8)),
          singleOutputSafeDivTest "safeDiv" safeDiv (div @(WordN 8)),
          singleOutputSafeDivTest "safeMod" safeMod (mod @(WordN 8)),
          doubleOutputSafeDivTest "safeDivMod" safeDivMod (divMod @(WordN 8)),
          singleOutputSafeDivTest "safeQuot" safeQuot (quot @(WordN 8)),
          singleOutputSafeDivTest "safeRem" safeRem (rem @(WordN 8)),
          doubleOutputSafeDivTest
            "safeQuotRem"
            safeQuotRem
            (quotRem @(WordN 8)),
          testCase "Bitwidth mismatch" $ do
            let actual =
                  safeDiv (bv 10 2) (bv 11 3) ::
                    ExceptT
                      (Either BitwidthMismatch ArithException)
                      Union
                      SomeWordN
            let expected = mrgThrowError $ Left BitwidthMismatch
            actual @?= expected
          ],
      testGroup "SomeIntN" $ do
        let singleOutputDivOrTest =
              opBoundedDivOrTestBase
                SomeIntN
                SomeIntN
        let doubleOutputDivOrTest =
              opBoundedDivOrTestBase
                SomeIntN
                (bimap SomeIntN SomeIntN)
        let singleOutputSafeDivTest =
              opBoundedSafeDivTestBase
                SomeIntN
                SomeIntN
                (\e -> Right e :: Either BitwidthMismatch ArithException)
        let doubleOutputSafeDivTest =
              opBoundedSafeDivTestBase
                SomeIntN
                (bimap SomeIntN SomeIntN)
                (\e -> Right e :: Either BitwidthMismatch ArithException)
        [ singleOutputDivOrTest "divOr" divOr (div @(IntN 8)),
          singleOutputDivOrTest "modOr" modOr (mod @(IntN 8)),
          doubleOutputDivOrTest "divModOr" divModOr (divMod @(IntN 8)),
          singleOutputDivOrTest "quotOr" quotOr (quot @(IntN 8)),
          singleOutputDivOrTest "remOr" remOr (rem @(IntN 8)),
          doubleOutputDivOrTest "quotRemOr" quotRemOr (quotRem @(IntN 8)),
          singleOutputSafeDivTest "div" safeDiv (div @(IntN 8)),
          singleOutputSafeDivTest "mod" safeMod (mod @(IntN 8)),
          doubleOutputSafeDivTest "divMod" safeDivMod (divMod @(IntN 8)),
          singleOutputSafeDivTest "quot" safeQuot (quot @(IntN 8)),
          singleOutputSafeDivTest "rem" safeRem (rem @(IntN 8)),
          doubleOutputSafeDivTest "quotRem" safeQuotRem (quotRem @(IntN 8)),
          testCase "Bitwidth mismatch" $ do
            let actual =
                  safeDiv (bv 10 2) (bv 11 3) ::
                    ExceptT
                      (Either BitwidthMismatch ArithException)
                      Union
                      SomeIntN
            let expected = mrgThrowError $ Left BitwidthMismatch
            actual @?= expected
          ]
    ]
