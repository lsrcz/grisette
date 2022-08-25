module Pizza.Lib.Data.FoldableSpec where

import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Pizza.Core.Control.Monad.UnionMBase
import Pizza.Core.Data.Class.SimpleMergeable
import Pizza.Lib.Control.Monad
import Pizza.Lib.Data.Foldable
import Pizza.TestUtils.SBool
import Test.Hspec

spec :: Spec
spec = do
  describe "mrgFoldlM" $ do
    it "mrgFoldlM should work" $ do
      ( mrgFoldlM
          (\acc (c, v) -> unionIf c (single $ acc + v) (single $ acc * v))
          10
          [(SSBool "a", 2), (SSBool "b", 3)] ::
          UnionMBase SBool Integer
        )
        `shouldBe` mrgIf
          (SSBool "a")
          (mrgIf (SSBool "b") (mrgReturn 15) (mrgReturn 36))
          (mrgIf (SSBool "b") (mrgReturn 23) (mrgReturn 60))
  describe "mrgFoldrM" $ do
    it "mrgFoldrM should work" $ do
      ( mrgFoldrM
          (\(c, v) acc -> unionIf c (single $ acc + v) (single $ acc * v))
          10
          [(SSBool "a", 2), (SSBool "b", 3)] ::
          UnionMBase SBool Integer
        )
        `shouldBe` mrgIf
          (SSBool "b")
          (mrgIf (SSBool "a") (mrgReturn 15) (mrgReturn 26))
          (mrgIf (SSBool "a") (mrgReturn 32) (mrgReturn 60))
  describe "mrgTraverse_" $ do
    it "mrgTraverse_ should work" $ do
      runExceptT
        ( mrgTraverse_
            (\(c, x) -> ExceptT $ unionIf c (return $ Left x) (return $ Right c))
            [(SSBool "a", 3), (SSBool "b", 2)] ::
            ExceptT Integer (UnionMBase SBool) ()
        )
        `shouldBe` runExceptT
          ( do
              _ <- mrgIf (SSBool "a") (throwError 3) (return ())
              _ <- mrgIf (SSBool "b") (throwError 2) (return ())
              mrgReturn ()
          )
  describe "mrgFor_" $ do
    it "mrgFor_ should work" $ do
      runExceptT
        ( mrgFor_
            [(SSBool "a", 3), (SSBool "b", 2)]
            (\(c, x) -> ExceptT $ unionIf c (return $ Left x) (return $ Right c)) ::
            ExceptT Integer (UnionMBase SBool) ()
        )
        `shouldBe` runExceptT
          ( do
              _ <- mrgIf (SSBool "a") (throwError 3) (return ())
              _ <- mrgIf (SSBool "b") (throwError 2) (return ())
              mrgReturn ()
          )
  describe "mrgMapM_" $ do
    it "mrgMapM_ should work" $ do
      runExceptT
        ( mrgMapM_
            (\(c, x) -> ExceptT $ unionIf c (return $ Left x) (return $ Right c))
            [(SSBool "a", 3), (SSBool "b", 2)] ::
            ExceptT Integer (UnionMBase SBool) ()
        )
        `shouldBe` runExceptT
          ( do
              _ <- mrgIf (SSBool "a") (throwError 3) (return ())
              _ <- mrgIf (SSBool "b") (throwError 2) (return ())
              mrgReturn ()
          )
  describe "mrgForM_" $ do
    it "mrgForM_ should work" $ do
      runExceptT
        ( mrgForM_
            [(SSBool "a", 3), (SSBool "b", 2)]
            (\(c, x) -> ExceptT $ unionIf c (return $ Left x) (return $ Right c)) ::
            ExceptT Integer (UnionMBase SBool) ()
        )
        `shouldBe` runExceptT
          ( do
              _ <- mrgIf (SSBool "a") (throwError 3) (return ())
              _ <- mrgIf (SSBool "b") (throwError 2) (return ())
              mrgReturn ()
          )
  describe "mrgSequence_" $ do
    it "mrgSequence_ should work" $ do
      runExceptT
        ( mrgSequence_
            [mrgIf (SSBool "a") (throwError 3) (return ()), mrgIf (SSBool "b") (throwError 2) (return ())] ::
            ExceptT Integer (UnionMBase SBool) ()
        )
        `shouldBe` runExceptT
          ( do
              _ <- mrgIf (SSBool "a") (throwError 3) (return ())
              _ <- mrgIf (SSBool "b") (throwError 2) (return ())
              mrgReturn ()
          )
  describe "mrgMsum" $ do
    it "mrgMsum should work" $ do
      (mrgMsum [mrgMzero, mrgMzero] :: MaybeT (UnionMBase SBool) Integer) `shouldBe` mrgMzero
      (mrgMsum [mrgReturn 1, mrgMzero] :: MaybeT (UnionMBase SBool) Integer) `shouldBe` mrgReturn 1
      (mrgMsum [mrgMzero, mrgReturn 1] :: MaybeT (UnionMBase SBool) Integer) `shouldBe` mrgReturn 1
      (mrgMsum [mrgReturn 2, mrgReturn 1] :: MaybeT (UnionMBase SBool) Integer) `shouldBe` mrgReturn 2
