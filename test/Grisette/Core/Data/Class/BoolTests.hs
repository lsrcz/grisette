module Grisette.Core.Data.Class.BoolTests (boolTests) where

import Grisette
  ( LogicalOp (false, symImplies, symNot, symXor, true, (.&&), (.||)),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

data CustomAndBool
  = CACBool Bool
  | CASBool String
  | CAAnd CustomAndBool CustomAndBool
  | CANot CustomAndBool
  deriving (Show, Eq)

instance LogicalOp CustomAndBool where
  true = CACBool True
  symNot (CACBool b) = CACBool $ not b
  symNot (CANot x) = x
  symNot x = CANot x
  (.&&) = CAAnd

data CustomOrBool
  = COCBool Bool
  | COSBool String
  | COOr CustomOrBool CustomOrBool
  | CONot CustomOrBool
  deriving (Show, Eq)

instance LogicalOp CustomOrBool where
  false = COCBool False
  symNot (COCBool b) = COCBool $ not b
  symNot (CONot x) = x
  symNot x = CONot x
  (.||) = COOr

boolTests :: Test
boolTests =
  testGroup
    "Bool"
    [ testGroup
        "LogicalOp"
        [ testGroup
            "Use and"
            [ testCase "symNot" $
                symNot (CASBool "a") @?= CANot (CASBool "a"),
              testCase ".&&" $
                CASBool "a"
                  .&& CASBool "b"
                  @?= CAAnd (CASBool "a") (CASBool "b"),
              testCase ".||" $
                CASBool "a"
                  .|| CASBool "b"
                  @?= CANot (CAAnd (CANot $ CASBool "a") (CANot $ CASBool "b"))
            ],
          testGroup
            "Use or"
            [ testCase "symNot" $
                symNot (COSBool "a") @?= CONot (COSBool "a"),
              testCase ".&&" $
                COSBool "a"
                  .&& COSBool "b"
                  @?= CONot (COOr (CONot $ COSBool "a") (CONot $ COSBool "b")),
              testCase ".||" $
                COSBool "a"
                  .|| COSBool "b"
                  @?= COOr (COSBool "a") (COSBool "b"),
              testCase "symXor" $
                COSBool "a"
                  `symXor` COSBool "b"
                  @?= COOr
                    (CONot (COOr (CONot (COSBool "a")) (COSBool "b")))
                    (CONot (COOr (COSBool "a") (CONot (COSBool "b")))),
              testCase "symImplies" $
                COSBool "a"
                  `symImplies` COSBool "b"
                  @?= COOr
                    (CONot (COSBool "a"))
                    (COSBool "b")
            ]
        ]
    ]
