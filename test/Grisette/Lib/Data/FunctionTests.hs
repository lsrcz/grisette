{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Data.FunctionTests (functionTests) where

import Grisette (SymInteger)
import Grisette.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Core.Data.Class.SimpleMergeable (mrgIf)
import Grisette.Lib.Data.Either (mrgLeft, mrgRight)
import Grisette.Lib.Data.Function (mrgOn, (.$), (.&))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit.Base ((@?=))

functionTests :: Test
functionTests =
  testGroup
    "Function"
    [ testCase ".$" $ do
        let actual =
              (either (+ 1) (\x -> x - 2))
                .$ (mrgIf "cond" (mrgLeft "a") (mrgRight "b"))
        let expected = symIte "cond" ("a" + 1) ("b" - 2) :: SymInteger
        actual @?= expected,
      testCase ".&" $ do
        let actual =
              (mrgIf "cond" (mrgLeft "a") (mrgRight "b"))
                .& (either (+ 1) (\x -> x - 2))
        let expected = symIte "cond" ("a" + 1) ("b" - 2) :: SymInteger
        actual @?= expected,
      testCase "mrgOn" $ do
        let f = (+)
        let u (Left x) = x
            u (Right x) = x
        let actual =
              mrgOn
                f
                u
                (mrgIf "cond1" (mrgLeft "a1") (mrgRight "b1"))
                (mrgIf "cond2" (mrgLeft "a2") (mrgRight "b2"))
        let expected =
              symIte "cond1" "a1" "b1" + symIte "cond2" "a2" "b2" ::
                SymInteger
        actual @?= expected
    ]
