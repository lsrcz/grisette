{-# LANGUAGE ScopedTypeVariables #-}

module Pizza.Lib.Control.MonadSpec where

import Control.Monad.Trans.Maybe
import Pizza.Core.Control.Monad.UnionMBase
import Pizza.Core.Data.Class.SimpleMergeable
import Pizza.Lib.Control.Monad
import Pizza.TestUtils.SBool
import Test.Hspec

spec :: Spec
spec = do
  describe "mrgFoldM" $ do
    it "mrgFoldM should work" $ do
      ( mrgFoldM
          (\acc (c, v) -> unionIf c (single $ acc + v) (single $ acc * v))
          10
          [(SSBool "a", 2), (SSBool "b", 3)] ::
          UnionMBase SBool Integer
        )
        `shouldBe` mrgIf
          (SSBool "a")
          (mrgIf (SSBool "b") (mrgReturn 15) (mrgReturn 36))
          (mrgIf (SSBool "b") (mrgReturn 23) (mrgReturn 60))
  describe "mrgMzero" $ do
    it "mrgMzero should work" $ do
      (mrgMzero :: MaybeT (UnionMBase SBool) Integer) `shouldBe` MaybeT (mrgReturn Nothing)
  describe "mrgMplus" $ do
    it "mrgMplus should work" $ do
      (mrgMzero `mrgMplus` mrgMzero :: MaybeT (UnionMBase SBool) Integer) `shouldBe` MaybeT (mrgReturn Nothing)
      (mrgReturn 1 `mrgMplus` mrgMzero :: MaybeT (UnionMBase SBool) Integer)
        `shouldBe` mrgReturn 1
      (mrgMzero `mrgMplus` mrgReturn 1 :: MaybeT (UnionMBase SBool) Integer)
        `shouldBe` mrgReturn 1
      (mrgReturn 2 `mrgMplus` mrgReturn 1 :: MaybeT (UnionMBase SBool) Integer)
        `shouldBe` mrgReturn 2
  describe "mrgFmap" $ do
    it "mrgFmap should work" $ do
      mrgFmap (\x -> x * x) (mrgIf (SSBool "a") (mrgReturn $ -1) (mrgReturn 1) :: UnionMBase SBool Integer)
        `shouldBe` mrgReturn 1
  describe ">>~" $ do
    it ">>~ should work" $ do
      (unionIf (SSBool "a") (single $ -1) (single 1) :: UnionMBase SBool Integer)
        >>~ unionIf (SSBool "a") (single $ -1) (single 1)
        `shouldBe` (mrgIf (SSBool "a") (mrgReturn $ -1) (mrgReturn 1) :: UnionMBase SBool Integer)
