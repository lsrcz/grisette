{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.IR.SymPrim.Data.Prim.IntegralTests (integralTests) where

import Control.DeepSeq (NFData (rnf), force)
import Control.Exception (ArithException, catch, evaluate)
import Data.Proxy (Proxy (Proxy))
import Grisette.Core.Data.BV (IntN (IntN), WordN (WordN))
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Integral
  ( pevalDivBoundedIntegralTerm,
    pevalDivIntegralTerm,
    pevalModIntegralTerm,
    pevalQuotBoundedIntegralTerm,
    pevalQuotIntegralTerm,
    pevalRemIntegralTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.Term
  ( SupportedPrim,
    Term,
    conTerm,
    divBoundedIntegralTerm,
    divIntegralTerm,
    modIntegralTerm,
    quotBoundedIntegralTerm,
    quotIntegralTerm,
    remIntegralTerm,
    ssymTerm,
  )
import Test.Framework (Test, TestName, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@=?))
import Test.QuickCheck (Arbitrary, ioProperty)

newtype AEWrapper = AEWrapper ArithException deriving (Eq)

instance Show AEWrapper where
  show (AEWrapper x) = show x

instance NFData AEWrapper where
  rnf (AEWrapper x) = x `seq` ()

sameDivPeval ::
  forall t.
  (Num t, Eq t, SupportedPrim t, Integral t) =>
  t ->
  t ->
  (Term t -> Term t -> Term t) ->
  (t -> t -> t) ->
  (Term t -> Term t -> Term t) ->
  IO ()
sameDivPeval i j pf cf consf = do
  cx <- evaluate (force $ Right $ cf i j) `catch` \(_ :: ArithException) -> return $ Left AEWrapper
  case cx of
    Left _ -> pf (conTerm i) (conTerm j) @=? consf (conTerm i) (conTerm j)
    Right t -> pf (conTerm i) (conTerm j) @=? conTerm t

divisionPevalBoundedTests ::
  forall p t.
  (Num t, Eq t, Bounded t, SupportedPrim t, Integral t) =>
  p t ->
  TestName ->
  (Term t -> Term t -> Term t) ->
  (t -> t -> t) ->
  (Term t -> Term t -> Term t) ->
  Test
divisionPevalBoundedTests _ name pf cf consf =
  testGroup
    name
    [ testCase "On concrete min divide by -1" $
        sameDivPeval minBound (-1) pf cf consf
    ]

divisionPevalTests ::
  forall p t0 t.
  (Num t, Eq t, Arbitrary t0, Show t0, SupportedPrim t, Integral t) =>
  p t ->
  TestName ->
  (t0 -> t) ->
  (Term t -> Term t -> Term t) ->
  (t -> t -> t) ->
  (Term t -> Term t -> Term t) ->
  Test
divisionPevalTests _ name transform pf cf consf =
  testGroup
    name
    [ testProperty "On concrete prop" $
        ioProperty . \(i0 :: t0, j0 :: t0) -> do
          let i = transform i0
          let j = transform j0
          sameDivPeval i j pf cf consf,
      testProperty "On concrete divide by 0" $
        ioProperty . \(i0 :: t0) -> do
          let i = transform i0
          sameDivPeval i 0 pf cf consf,
      testCase "divide by 1" $ do
        pf (ssymTerm "a" :: Term t) (conTerm 1) @=? ssymTerm "a",
      testCase "On symbolic" $ do
        pf (ssymTerm "a" :: Term t) (ssymTerm "b")
          @=? consf (ssymTerm "a" :: Term t) (ssymTerm "b" :: Term t)
    ]

divisionPevalBoundedTestGroup ::
  TestName ->
  (forall t. (SupportedPrim t, Bounded t, Integral t) => Term t -> Term t -> Term t) ->
  (forall t. (Bounded t, Integral t) => t -> t -> t) ->
  (forall t. (SupportedPrim t, Bounded t, Integral t) => Term t -> Term t -> Term t) ->
  Test
divisionPevalBoundedTestGroup name pf cf consf =
  testGroup
    name
    [ divisionPevalTests (Proxy @(IntN 4)) "IntN" IntN pf cf consf,
      divisionPevalBoundedTests (Proxy @(IntN 4)) "IntN Bounded" pf cf consf
    ]

divisionPevalUnboundedTestGroup ::
  TestName ->
  (forall t. (SupportedPrim t, Integral t) => Term t -> Term t -> Term t) ->
  (forall t. (Integral t) => t -> t -> t) ->
  (forall t. (SupportedPrim t, Integral t) => Term t -> Term t -> Term t) ->
  Test
divisionPevalUnboundedTestGroup name pf cf consf =
  testGroup
    name
    [ divisionPevalTests (Proxy @Integer) "Integer" id pf cf consf,
      divisionPevalTests (Proxy @(WordN 4)) "WordN" WordN pf cf consf,
      divisionPevalBoundedTests (Proxy @(WordN 4)) "WordN Bounded" pf cf consf
    ]

moduloPevalTests ::
  forall p t0 t.
  (Num t, Eq t, Arbitrary t0, Show t0, SupportedPrim t, Integral t) =>
  p t ->
  TestName ->
  (t0 -> t) ->
  (Term t -> Term t -> Term t) ->
  (t -> t -> t) ->
  (Term t -> Term t -> Term t) ->
  Test
moduloPevalTests _ name transform pf cf consf =
  testGroup
    name
    [ testProperty "On concrete" $
        ioProperty . \(i0 :: t0, j0 :: t0) -> do
          let i = transform i0
          let j = transform j0
          sameDivPeval i j pf cf consf,
      testProperty "On concrete divide by 0" $
        ioProperty . \(i0 :: t0) -> do
          let i = transform i0
          sameDivPeval i 0 pf cf consf,
      testCase "mod by 1" $ do
        pf (ssymTerm "a" :: Term t) (conTerm 1) @=? conTerm 0,
      testCase "mod by -1" $ do
        pf (ssymTerm "a" :: Term t) (conTerm $ -1) @=? conTerm 0,
      testCase "On symbolic" $ do
        pf (ssymTerm "a" :: Term t) (ssymTerm "b")
          @=? consf (ssymTerm "a" :: Term t) (ssymTerm "b" :: Term t)
    ]

moduloPevalTestGroup ::
  TestName ->
  (forall t. (SupportedPrim t, Integral t) => Term t -> Term t -> Term t) ->
  (forall t. (Integral t) => t -> t -> t) ->
  (forall t. (SupportedPrim t, Integral t) => Term t -> Term t -> Term t) ->
  Test
moduloPevalTestGroup name pf cf consf =
  testGroup
    name
    [ moduloPevalTests (Proxy @Integer) "Integer" id pf cf consf,
      moduloPevalTests (Proxy @(IntN 4)) "IntN" IntN pf cf consf,
      moduloPevalTests (Proxy @(WordN 4)) "WordN" WordN pf cf consf
    ]

integralTests :: Test
integralTests =
  testGroup
    "Integral"
    [ divisionPevalUnboundedTestGroup "Div unbounded" pevalDivIntegralTerm div divIntegralTerm,
      divisionPevalUnboundedTestGroup "Quot unbounded" pevalQuotIntegralTerm quot quotIntegralTerm,
      divisionPevalBoundedTestGroup "Div bounded" pevalDivBoundedIntegralTerm div divBoundedIntegralTerm,
      divisionPevalBoundedTestGroup "Quot bounded" pevalQuotBoundedIntegralTerm quot quotBoundedIntegralTerm,
      moduloPevalTestGroup "Mod" pevalModIntegralTerm mod modIntegralTerm,
      moduloPevalTestGroup "Rem" pevalRemIntegralTerm rem remIntegralTerm
    ]
