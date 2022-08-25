module Pizza.Core.Data.Class.BoolSpec where

import Pizza.Core.Data.Class.Bool
import Test.Hspec

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

spec :: Spec
spec = do
  describe "Default implementation for boolean ops" $ do
    describe "Default implementation in LogicalOp" $ do
      it "Default implementation in LogicalOp should be correct" $ do
        nots (CASBool "a") `shouldBe` CANot (CASBool "a")
        CASBool "a" &&~ CASBool "b" `shouldBe` CAAnd (CASBool "a") (CASBool "b")
        CASBool "a" ||~ CASBool "b" `shouldBe` CANot (CAAnd (CANot $ CASBool "a") (CANot $ CASBool "b"))
        nots (COSBool "a") `shouldBe` CONot (COSBool "a")
        COSBool "a" &&~ COSBool "b" `shouldBe` CONot (COOr (CONot $ COSBool "a") (CONot $ COSBool "b"))
        COSBool "a" ||~ COSBool "b" `shouldBe` COOr (COSBool "a") (COSBool "b")
        COSBool "a"
          `xors` COSBool "b"
          `shouldBe` COOr (CONot (COOr (CONot (COSBool "a")) (COSBool "b"))) (CONot (COOr (COSBool "a") (CONot (COSBool "b"))))
        COSBool "a" `implies` COSBool "b" `shouldBe` COOr (CONot (COSBool "a")) (COSBool "b")
