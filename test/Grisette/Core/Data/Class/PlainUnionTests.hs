{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Core.Data.Class.PlainUnionTests (plainUnionTests) where

import Grisette.Core.Control.Monad.UnionM (UnionM)
import Grisette.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Core.Data.Class.LogicalOp (LogicalOp ((.&&)))
import Grisette.Core.Data.Class.PlainUnion
  ( onUnion,
    simpleMerge,
    (.#),
    pattern If,
    pattern Single,
  )
import Grisette.Core.Data.Class.SimpleMergeable
  ( UnionMergeable1 (mrgIfPropagatedStrategy),
    mrgIf,
  )
import Grisette.Core.Data.Class.Solvable (Solvable (con))
import Grisette.Core.Data.Class.TryMerge (mrgSingle)
import Grisette.SymPrim.SymBool (SymBool)
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
                r @?= return "c"
              _ -> fail "Should not happen",
          testCase "Merged" $
            case mrgIf "a" (return "b") (return "c") :: UnionM SymBool of
              If {} -> fail "Expected Single"
              Single v -> v @?= symIte "a" "b" "c"
              _ -> fail "Should not happen",
          testCase "Construct single" $
            (Single "a" :: UnionM SymBool) @?= mrgSingle "a",
          testCase "Construct If" $ do
            let actual = If "a" (return "b") (return "c") :: UnionM SymBool
            let expected = mrgIf "a" (return "b") (return "c")
            actual @?= expected
        ]
    ]
