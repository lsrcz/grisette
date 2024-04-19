{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Core.Data.Class.SafeLinearArithTests
  ( safeLinearArithTests,
  )
where

import Control.Exception (ArithException (Overflow, Underflow))
import Control.Monad.Except (ExceptT, MonadError)
import Data.Data (Proxy (Proxy), Typeable, typeRep)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import Grisette
  ( BV (bv),
    BitwidthMismatch (BitwidthMismatch),
    IntN,
    Mergeable,
    SafeLinearArith (safeAdd, safeNeg, safeSub),
    SomeIntN,
    SomeWordN,
    TryMerge,
    UnionM,
    WordN,
    mrgSingle,
    pattern SomeIntN,
    pattern SomeWordN,
  )
import Grisette.Lib.Control.Monad.Except (mrgModifyError, mrgThrowError)
import Grisette.Lib.Data.Functor (mrgFmap)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@?=))
import Test.QuickCheck (Arbitrary, ioProperty)

binSafeOp ::
  forall a m.
  ( Integral a,
    Bounded a,
    MonadError ArithException m,
    TryMerge m,
    Mergeable a
  ) =>
  (Integer -> Integer -> Integer) ->
  a ->
  a ->
  m a
binSafeOp op l r
  | result > fromIntegral (maxBound :: a) =
      mrgThrowError Overflow
  | result < fromIntegral (minBound :: a) =
      mrgThrowError Underflow
  | otherwise = mrgSingle $ fromIntegral result
  where
    result = op (fromIntegral l) (fromIntegral r)

unarySafeOp ::
  forall a m.
  ( Integral a,
    Bounded a,
    MonadError ArithException m,
    TryMerge m,
    Mergeable a
  ) =>
  (Integer -> Integer) ->
  a ->
  m a
unarySafeOp op l
  | result > fromIntegral (maxBound :: a) =
      mrgThrowError Overflow
  | result < fromIntegral (minBound :: a) =
      mrgThrowError Underflow
  | otherwise = mrgSingle $ fromIntegral result
  where
    result = op (fromIntegral l)

safeLinearArithTest ::
  forall a b e.
  ( SafeLinearArith e b (ExceptT e UnionM),
    Integral a,
    Bounded a,
    Arbitrary a,
    Show a,
    Show b,
    Eq b,
    Eq e,
    Show e,
    Typeable a,
    Mergeable a,
    Mergeable e
  ) =>
  (a -> b) ->
  (ArithException -> e) ->
  Test
safeLinearArithTest wrap transformError =
  testGroup
    (show $ typeRep (Proxy @a))
    [ testProperty "safeAdd" $ \(l :: a) (r :: a) -> ioProperty $ do
        let actual = safeAdd (wrap l) (wrap r)
        let expected = mrgModifyError transformError $ binSafeOp (+) l r
        actual @?= (mrgFmap wrap expected :: ExceptT e UnionM b),
      testProperty "safeSub" $ \(l :: a) (r :: a) -> ioProperty $ do
        let actual = safeSub (wrap l) (wrap r)
        let expected = mrgModifyError transformError $ binSafeOp (-) l r
        actual @?= (mrgFmap wrap expected :: ExceptT e UnionM b),
      testProperty "safeNeg" $ \(l :: a) -> ioProperty $ do
        let actual = safeNeg (wrap l) :: ExceptT e UnionM b
        let expected = mrgModifyError transformError $ unarySafeOp negate l
        actual @?= mrgFmap wrap expected
    ]

safeLinearArithTestSimple ::
  forall a.
  ( SafeLinearArith ArithException a (ExceptT ArithException UnionM),
    Integral a,
    Bounded a,
    Arbitrary a,
    Show a,
    Show a,
    Typeable a
  ) =>
  Test
safeLinearArithTestSimple = safeLinearArithTest @a @a id id

safeLinearArithTests :: Test
safeLinearArithTests =
  testGroup
    "SafeLinearArith"
    [ safeLinearArithTestSimple @Int,
      safeLinearArithTestSimple @Int8,
      safeLinearArithTestSimple @Int16,
      safeLinearArithTestSimple @Int32,
      safeLinearArithTestSimple @Int64,
      safeLinearArithTestSimple @(IntN 1),
      safeLinearArithTestSimple @(IntN 2),
      safeLinearArithTestSimple @(IntN 3),
      safeLinearArithTestSimple @(IntN 128),
      safeLinearArithTest @(IntN 2)
        @SomeIntN
        @(Either BitwidthMismatch ArithException)
        SomeIntN
        Right,
      safeLinearArithTest @(IntN 128)
        @SomeIntN
        @(Either BitwidthMismatch ArithException)
        SomeIntN
        Right,
      testCase "SomeIntN different bit width" $ do
        let l = bv 2 1 :: SomeIntN
        let r = bv 3 1 :: SomeIntN
        let actual =
              safeAdd l r ::
                ExceptT (Either BitwidthMismatch ArithException) UnionM SomeIntN
        let expected = mrgThrowError $ Left BitwidthMismatch
        actual @?= expected,
      safeLinearArithTestSimple @Word,
      safeLinearArithTestSimple @Word8,
      safeLinearArithTestSimple @Word16,
      safeLinearArithTestSimple @Word32,
      safeLinearArithTestSimple @Word64,
      safeLinearArithTestSimple @(WordN 1),
      safeLinearArithTestSimple @(WordN 2),
      safeLinearArithTestSimple @(WordN 3),
      safeLinearArithTestSimple @(WordN 128),
      safeLinearArithTest @(WordN 2)
        @SomeWordN
        @(Either BitwidthMismatch ArithException)
        SomeWordN
        Right,
      safeLinearArithTest @(WordN 128)
        @SomeWordN
        @(Either BitwidthMismatch ArithException)
        SomeWordN
        Right,
      testCase "SomeWordN different bit width" $ do
        let l = bv 2 1 :: SomeWordN
        let r = bv 3 1 :: SomeWordN
        let actual =
              safeAdd l r ::
                ExceptT
                  (Either BitwidthMismatch ArithException)
                  UnionM
                  SomeWordN
        let expected = mrgThrowError $ Left BitwidthMismatch
        actual @?= expected
    ]
