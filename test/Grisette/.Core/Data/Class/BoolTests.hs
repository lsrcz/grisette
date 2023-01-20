module Grisette.Core.Data.Class.BoolTests where

import Grisette.Core.Data.Class.Bool
import Test.Tasty
import Test.Tasty.HUnit

data CustomAndBool
  = CASBool String
  | CAAnd CustomAndBool CustomAndBool
  | CANot CustomAndBool
  deriving (Show, Eq)

instance LogicalOp CustomAndBool where
  nots (CANot x) = x
  nots x = CANot x
  (&&~) = CAAnd

data CustomOrBool
  = COSBool String
  | COOr CustomOrBool CustomOrBool
  | CONot CustomOrBool
  deriving (Show, Eq)

instance LogicalOp CustomOrBool where
  nots (CONot x) = x
  nots x = CONot x
  (||~) = COOr

boolTests :: TestTree
boolTests =
  testGroup
    "BoolTests"
    [ testGroup
        "LogicalOp"
        [ testGroup
            "Use and"
            [ testCase "nots" $
                nots (CASBool "a") @=? CANot (CASBool "a"),
              testCase "&&~" $
                CASBool "a" &&~ CASBool "b" @=? CAAnd (CASBool "a") (CASBool "b"),
              testCase "||~" $
                CASBool "a" ||~ CASBool "b" @=? CANot (CAAnd (CANot $ CASBool "a") (CANot $ CASBool "b"))
            ],
          testGroup
            "Use or"
            [ testCase "nots" $
                nots (COSBool "a") @=? CONot (COSBool "a"),
              testCase "&&~" $
                COSBool "a" &&~ COSBool "b" @=? CONot (COOr (CONot $ COSBool "a") (CONot $ COSBool "b")),
              testCase "||~" $
                COSBool "a" ||~ COSBool "b" @=? COOr (COSBool "a") (COSBool "b"),
              testCase "xors" $
                COSBool "a"
                  `xors` COSBool "b"
                  @=? COOr (CONot (COOr (CONot (COSBool "a")) (COSBool "b"))) (CONot (COOr (COSBool "a") (CONot (COSBool "b")))),
              testCase "implies" $
                COSBool "a" `implies` COSBool "b" @=? COOr (CONot (COSBool "a")) (COSBool "b")
            ]
        ]
    ]
