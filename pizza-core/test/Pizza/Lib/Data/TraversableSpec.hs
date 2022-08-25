module Pizza.Lib.Data.TraversableSpec where

import Control.Monad.Except
import Pizza.Core.Control.Monad.UnionMBase
import Pizza.Core.Data.Class.SimpleMergeable
import Pizza.Lib.Data.Traversable
import Pizza.TestUtils.SBool
import Test.Hspec

spec :: Spec
spec = do
  describe "mrgTraverse" $
    it "mrgTraverse should work" $ do
      runExceptT
        ( mrgTraverse
            (\(c, d, x, y, z) -> ExceptT $ unionIf c (return $ Left x) (unionIf d (return $ Right y) (return $ Right z)))
            [(SSBool "a", SSBool "c", 3, 4, 5), (SSBool "b", SSBool "d", 2, 3, 6)] ::
            ExceptT Integer (UnionMBase SBool) [Integer]
        )
        `shouldBe` runExceptT
          ( do
              a <- mrgIf (SSBool "a") (throwError 3) (mrgIf (SSBool "c") (return 4) (return 5))
              b <- mrgIf (SSBool "b") (throwError 2) (mrgIf (SSBool "d") (return 3) (return 6))
              mrgSingle [a, b]
          )
  describe "mrgSequenceA" $
    it "mrgSequenceA should work" $ do
      runExceptT
        ( mrgSequenceA
            [ ExceptT $ unionIf (SSBool "a") (return $ Left 3) (unionIf (SSBool "c") (return $ Right 4) (return $ Right 5)),
              ExceptT $ unionIf (SSBool "b") (return $ Left 2) (unionIf (SSBool "d") (return $ Right 3) (return $ Right 6))
            ] ::
            ExceptT Integer (UnionMBase SBool) [Integer]
        )
        `shouldBe` runExceptT
          ( do
              a <- mrgIf (SSBool "a") (throwError 3) (mrgIf (SSBool "c") (return 4) (return 5))
              b <- mrgIf (SSBool "b") (throwError 2) (mrgIf (SSBool "d") (return 3) (return 6))
              mrgSingle [a, b]
          )
  describe "mrgMapM" $
    it "mrgMapM should work" $ do
      runExceptT
        ( mrgMapM
            (\(c, d, x, y, z) -> ExceptT $ unionIf c (return $ Left x) (unionIf d (return $ Right y) (return $ Right z)))
            [(SSBool "a", SSBool "c", 3, 4, 5), (SSBool "b", SSBool "d", 2, 3, 6)] ::
            ExceptT Integer (UnionMBase SBool) [Integer]
        )
        `shouldBe` runExceptT
          ( do
              a <- mrgIf (SSBool "a") (throwError 3) (mrgIf (SSBool "c") (return 4) (return 5))
              b <- mrgIf (SSBool "b") (throwError 2) (mrgIf (SSBool "d") (return 3) (return 6))
              mrgSingle [a, b]
          )
  describe "mrgSequence" $
    it "mrgSequence should work" $ do
      runExceptT
        ( mrgSequence
            [ ExceptT $ unionIf (SSBool "a") (return $ Left 3) (unionIf (SSBool "c") (return $ Right 4) (return $ Right 5)),
              ExceptT $ unionIf (SSBool "b") (return $ Left 2) (unionIf (SSBool "d") (return $ Right 3) (return $ Right 6))
            ] ::
            ExceptT Integer (UnionMBase SBool) [Integer]
        )
        `shouldBe` runExceptT
          ( do
              a <- mrgIf (SSBool "a") (throwError 3) (mrgIf (SSBool "c") (return 4) (return 5))
              b <- mrgIf (SSBool "b") (throwError 2) (mrgIf (SSBool "d") (return 3) (return 6))
              mrgSingle [a, b]
          )
  describe "mrgFor" $
    it "mrgFor should work" $ do
      runExceptT
        ( mrgFor
            [(SSBool "a", SSBool "c", 3, 4, 5), (SSBool "b", SSBool "d", 2, 3, 6)]
            (\(c, d, x, y, z) -> ExceptT $ unionIf c (return $ Left x) (unionIf d (return $ Right y) (return $ Right z))) ::
            ExceptT Integer (UnionMBase SBool) [Integer]
        )
        `shouldBe` runExceptT
          ( do
              a <- mrgIf (SSBool "a") (throwError 3) (mrgIf (SSBool "c") (return 4) (return 5))
              b <- mrgIf (SSBool "b") (throwError 2) (mrgIf (SSBool "d") (return 3) (return 6))
              mrgSingle [a, b]
          )
  describe "mrgForM" $
    it "mrgForM should work" $ do
      runExceptT
        ( mrgForM
            [(SSBool "a", SSBool "c", 3, 4, 5), (SSBool "b", SSBool "d", 2, 3, 6)]
            (\(c, d, x, y, z) -> ExceptT $ unionIf c (return $ Left x) (unionIf d (return $ Right y) (return $ Right z))) ::
            ExceptT Integer (UnionMBase SBool) [Integer]
        )
        `shouldBe` runExceptT
          ( do
              a <- mrgIf (SSBool "a") (throwError 3) (mrgIf (SSBool "c") (return 4) (return 5))
              b <- mrgIf (SSBool "b") (throwError 2) (mrgIf (SSBool "d") (return 3) (return 6))
              mrgSingle [a, b]
          )
