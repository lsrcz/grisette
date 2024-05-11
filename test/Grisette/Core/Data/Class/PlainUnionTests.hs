{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Core.Data.Class.PlainUnionTests (plainUnionTests) where

import Grisette
  ( ITEOp (symIte),
    LogicalOp ((.&&)),
    Solvable (con),
    SymBool,
    UnionM,
    UnionMergeable1 (mrgIfPropagatedStrategy),
    mrgIf,
    mrgSingle,
    onUnion,
    simpleMerge,
    (.#),
    pattern If,
    pattern Single,
  )
import Grisette.Internal.Core.Data.Class.PlainUnion
  ( PlainUnion (overestimateUnionValues),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

plainUnionTests :: Test
plainUnionTests =
  testGroup
    "PlainUnion"
    [ testCase "simpleMerge" $ do
        simpleMerge
          ( mrgIfPropagatedStrategy "a" (return "b") (return "c") ::
              UnionM SymBool
          )
          @?= symIte "a" "b" "c",
      testCase "(.#)" $ do
        let symAll = foldl (.&&) (con True)
        symAll
          .# ( mrgIfPropagatedStrategy
                 "cond"
                 (return ["a"])
                 (return ["b", "c"]) ::
                 UnionM [SymBool]
             )
          @?= symIte "cond" "a" ("b" .&& "c"),
      testCase "onUnion" $ do
        let symAll = foldl (.&&) (con True)
        let symAllU = onUnion symAll
        symAllU
          ( mrgIfPropagatedStrategy "cond" (return ["a"]) (return ["b", "c"]) ::
              UnionM [SymBool]
          )
          @?= symIte "cond" "a" ("b" .&& "c"),
      testGroup
        "Single and If pattern"
        [ testCase "Unmerged" $
            case mrgIfPropagatedStrategy "a" (return "b") (return "c") ::
                   UnionM SymBool of
              Single _ -> fail "Expected If"
              If c l r -> do
                c @?= "a"
                l @?= return "b"
                r @?= return "c",
          testCase "Merged" $
            case mrgIf "a" (return "b") (return "c") :: UnionM SymBool of
              If {} -> fail "Expected Single"
              Single v -> v @?= symIte "a" "b" "c",
          testCase "Construct single" $
            (Single "a" :: UnionM SymBool) @?= mrgSingle "a",
          testCase "Construct If" $ do
            let actual = If "a" (return "b") (return "c") :: UnionM SymBool
            let expected = mrgIf "a" (return "b") (return "c")
            actual @?= expected
        ],
      testCase "overestimateUnionValues" $ do
        overestimateUnionValues (return 1 :: UnionM Int) @?= [1]
        overestimateUnionValues (mrgIf "a" (return 1) (return 2) :: UnionM Int)
          @?= [1, 2 :: Int]
        overestimateUnionValues
          (mrgIf "a" (return 1) (mrgIf "x" (return 3) (return 2)) :: UnionM Int)
          @?= [1, 2, 3 :: Int]
    ]
