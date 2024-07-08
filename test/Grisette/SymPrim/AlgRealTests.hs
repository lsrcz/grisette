{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Grisette.SymPrim.AlgRealTests (algRealTests) where

import Control.DeepSeq (NFData, force)
import Control.Exception (ArithException, evaluate, try)
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (assertBool, (@?=))
import Test.QuickCheck (ioProperty)

unaryOpComplianceWithRational ::
  (NFData a, NFData b) =>
  String ->
  (AlgReal -> a) ->
  (Rational -> b) ->
  (a -> b -> Bool) ->
  Test
unaryOpComplianceWithRational name algRealOp rationalOp cmp =
  testProperty name $ \x -> ioProperty $ do
    actual <- try $ evaluate $ force $ algRealOp (fromRational x)
    expected <- try $ evaluate $ force $ rationalOp x
    case (actual, expected) of
      (Left (ea :: ArithException), Left ee) -> ea @?= ee
      (Right a, Right e) -> assertBool "Not compliant" $ cmp a e
      _ -> assertBool "Not compliant" False

binOpComplianceWithRational ::
  (NFData a, NFData b) =>
  String ->
  (AlgReal -> AlgReal -> a) ->
  (Rational -> Rational -> b) ->
  (a -> b -> Bool) ->
  Test
binOpComplianceWithRational name algRealOp rationalOp cmp =
  testProperty name $ \x y -> ioProperty $ do
    actual <-
      try $ evaluate $ force $ algRealOp (fromRational x) (fromRational y)
    expected <- try $ evaluate $ force $ rationalOp x y
    case (actual, expected) of
      (Left (ea :: ArithException), Left ee) -> ea @?= ee
      (Right a, Right e) -> assertBool "Not compliant" $ cmp a e
      _ -> assertBool "Not compliant" False

eqAlgRealRational :: AlgReal -> Rational -> Bool
eqAlgRealRational l r = l == fromRational r

algRealTests :: Test
algRealTests =
  testGroup
    "AlgReal"
    [ testGroup
        "Eq"
        [ binOpComplianceWithRational "==" (==) (==) (==),
          binOpComplianceWithRational "/=" (/=) (/=) (==)
        ],
      testGroup
        "Ord"
        [ binOpComplianceWithRational "<" (<) (<) (==),
          binOpComplianceWithRational "<=" (<=) (<=) (==),
          binOpComplianceWithRational ">" (>) (>) (==),
          binOpComplianceWithRational ">=" (>=) (>=) (==)
        ],
      testGroup
        "Num"
        [ binOpComplianceWithRational "+" (+) (+) eqAlgRealRational,
          binOpComplianceWithRational "*" (*) (*) eqAlgRealRational,
          binOpComplianceWithRational "-" (-) (-) eqAlgRealRational,
          unaryOpComplianceWithRational
            "negate"
            negate
            negate
            eqAlgRealRational,
          unaryOpComplianceWithRational "abs" abs abs eqAlgRealRational,
          unaryOpComplianceWithRational
            "signum"
            signum
            signum
            eqAlgRealRational,
          testProperty "fromInteger" $ \x ->
            let actual = fromInteger x
                expected = fromRational $ toRational x
             in eqAlgRealRational actual expected
        ],
      testCase "Lift" $ do
        let x = 1 :: AlgReal
        $$([||x||]) @?= x,
      testGroup
        "Fractional"
        [ binOpComplianceWithRational "/" (/) (/) eqAlgRealRational,
          unaryOpComplianceWithRational
            "recip"
            recip
            recip
            eqAlgRealRational,
          testProperty "fromRational" $ \x ->
            let actual = fromRational x
             in eqAlgRealRational actual x
        ]
    ]
