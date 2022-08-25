{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Pizza.Core.Data.UnionBaseSpec where

import GHC.Generics
import Pizza.Core.Data.Class.Bool
import Pizza.Core.Data.Class.Mergeable
import Pizza.Core.Data.UnionBase
import Pizza.TestUtils.SBool
import Test.Hspec

data TripleSum a b c = TS1 a | TS2 b | TS3 c deriving (Show, Eq, Generic)

instance (SymBoolOp bool, Mergeable bool a, Mergeable bool b, Mergeable bool c) => Mergeable bool (TripleSum a b c) where
  mergingStrategy =
    SortedStrategy
      (\case TS1 _ -> (0 :: Int); TS2 _ -> (1 :: Int); TS3 _ -> (2 :: Int))
      ( \case
          0 -> wrapStrategy mergingStrategy TS1 (\(TS1 x) -> x)
          1 -> wrapStrategy mergingStrategy TS2 (\(TS2 x) -> x)
          2 -> wrapStrategy mergingStrategy TS3 (\(TS3 x) -> x)
          _ -> error "Bad"
      )

spec :: Spec
spec = do
  describe "ifWithLeftMost" $ do
    it "ifWithLeftMost should maintain left most info" $ do
      ifWithLeftMost False (SSBool "a") (Single (1 :: Integer)) (Single 2)
        `shouldBe` If 1 False (SSBool "a") (Single 1) (Single 2)
      ifWithLeftMost
        True
        (SSBool "a")
        (If 1 True (SSBool "b") (Single (1 :: Integer)) (Single 2))
        (If 3 True (SSBool "c") (Single 3) (Single 4))
        `shouldBe` If
          1
          True
          (SSBool "a")
          (If 1 True (SSBool "b") (Single (1 :: Integer)) (Single 2))
          (If 3 True (SSBool "c") (Single 3) (Single 4))
  describe "ifWithStrategy" $ do
    it "ifWithStrategy with concrete condition" $ do
      ifWithStrategy mergingStrategy (CBool True) (Single (1 :: Integer)) (Single 2) `shouldBe` Single 1
      ifWithStrategy mergingStrategy (CBool False) (Single (1 :: Integer)) (Single 2) `shouldBe` Single 2
    it "ifWithStrategy with condition equal to sub conditions" $ do
      let a = ifWithStrategy mergingStrategy (SSBool "a") (Single (1 :: Integer)) (Single 2)
      ifWithStrategy mergingStrategy (SSBool "a") a (Single 3)
        `shouldBe` If 1 True (SSBool "a") (Single 1) (Single 3)
      ifWithStrategy mergingStrategy (SSBool "a") (Single 0) a
        `shouldBe` If 0 True (SSBool "a") (Single 0) (Single 2)
    it "ifWithStrategy with simple mergeables" $ do
      ifWithStrategy mergingStrategy (SSBool "a") (Single (SSBool "b")) (Single (SSBool "c"))
        `shouldBe` Single (ITE (SSBool "a") (SSBool "b") (SSBool "c"))
    describe "ifWithStrategy with ordered mergeables" $ do
      describe "ifWithStrategy on Single/Single" $ do
        it "ifWithStrategy on Single/Single with idxt < idxf" $ do
          ifWithStrategy mergingStrategy (SSBool "a") (Single (1 :: Integer)) (Single 2)
            `shouldBe` If 1 True (SSBool "a") (Single 1) (Single 2)
          ifWithStrategy mergingStrategy (SSBool "a") (Single Nothing) (Single (Just (2 :: Integer)))
            `shouldBe` If Nothing True (SSBool "a") (Single Nothing) (Single (Just 2))
        describe "ifWithStrategy on Single/Single with idxt == idxf" $ do
          it "ifWithStrategy on Single/Single with idxt == idxf as terminal" $ do
            ifWithStrategy mergingStrategy (SSBool "a") (Single (1 :: Integer)) (Single 1)
              `shouldBe` Single 1
            ifWithStrategy mergingStrategy (SSBool "a") (Single (Just (SSBool "b"))) (Single (Just (SSBool "c")))
              `shouldBe` Single (Just (ITE (SSBool "a") (SSBool "b") (SSBool "c")))
          it "ifWithStrategy on Single/Single with idxt == idxf but not terminal" $ do
            ifWithStrategy mergingStrategy (SSBool "a") (Single (Just (1 :: Integer))) (Single (Just (2 :: Integer)))
              `shouldBe` If (Just 1) True (SSBool "a") (Single $ Just 1) (Single (Just 2))
            ifWithStrategy
              mergingStrategy
              (SSBool "a")
              (Single $ Just $ Just $ SSBool "b")
              (Single $ Just $ Just $ SSBool "c")
              `shouldBe` Single (Just (Just (ITE (SSBool "a") (SSBool "b") (SSBool "c"))))
        it "ifWithStrategy on Single/Single with idxt > idxf" $ do
          ifWithStrategy mergingStrategy (SSBool "a") (Single (2 :: Integer)) (Single 1)
            `shouldBe` If 1 True (Not $ SSBool "a") (Single 1) (Single 2)
          ifWithStrategy mergingStrategy (SSBool "a") (Single (Just (2 :: Integer))) (Single Nothing)
            `shouldBe` If Nothing True (Not $ SSBool "a") (Single Nothing) (Single (Just 2))
      describe "ifWithStrategy on Single/If" $ do
        describe "ifWithStrategy on Single/If degenerate to Single/Single when idxft == idxff" $ do
          it "ifWithStrategy on Single/If for degenerated case with idxt < idxf" $ do
            let x =
                  ifWithStrategy
                    mergingStrategy
                    (SSBool "a")
                    (Single (Just (1 :: Integer)))
                    (Single (Just (2 :: Integer)))
            ifWithStrategy mergingStrategy (SSBool "b") (Single Nothing) x
              `shouldBe` If
                Nothing
                True
                (SSBool "b")
                (Single Nothing)
                (If (Just 1) True (SSBool "a") (Single $ Just 1) (Single (Just 2)))
          it "ifWithStrategy on Single/If for degenerated case with idxt == idxf" $ do
            let x =
                  ifWithStrategy
                    mergingStrategy
                    (SSBool "a")
                    (Single (Just (1 :: Integer)))
                    (Single (Just (3 :: Integer)))
            ifWithStrategy mergingStrategy (SSBool "b") (Single $ Just 0) x
              `shouldBe` If
                (Just 0)
                True
                (SSBool "b")
                (Single $ Just 0)
                (If (Just 1) True (SSBool "a") (Single $ Just 1) (Single (Just 3)))
            ifWithStrategy mergingStrategy (SSBool "b") (Single $ Just 1) x
              `shouldBe` If (Just 1) True (Or (SSBool "b") (SSBool "a")) (Single $ Just 1) (Single (Just 3))
            ifWithStrategy mergingStrategy (SSBool "b") (Single $ Just 2) x
              `shouldBe` If
                (Just 1)
                True
                (And (Not (SSBool "b")) (SSBool "a"))
                (Single $ Just 1)
                (If (Just 2) True (SSBool "b") (Single $ Just 2) (Single $ Just 3))
            ifWithStrategy mergingStrategy (SSBool "b") (Single $ Just 3) x
              `shouldBe` If (Just 1) True (And (Not (SSBool "b")) (SSBool "a")) (Single $ Just 1) (Single (Just 3))
            ifWithStrategy mergingStrategy (SSBool "b") (Single $ Just 4) x
              `shouldBe` If
                (Just 1)
                True
                (And (Not (SSBool "b")) (SSBool "a"))
                (Single $ Just 1)
                (If (Just 3) True (Not $ SSBool "b") (Single $ Just 3) (Single $ Just 4))
          it "ifWithStrategy on Single/If for degenerated case with idxt > idxf" $ do
            let x =
                  ifWithStrategy
                    mergingStrategy
                    (SSBool "a")
                    (Single (Left (1 :: Integer)))
                    (Single (Left (2 :: Integer)))
            ifWithStrategy mergingStrategy (SSBool "b") (Single $ Right (1 :: Integer)) x
              `shouldBe` If
                (Left 1)
                True
                (Not $ SSBool "b")
                (If (Left 1) True (SSBool "a") (Single $ Left 1) (Single (Left 2)))
                (Single $ Right 1)
        it "ifWithStrategy on Single/If for idxt < idxft" $ do
          let x =
                ifWithStrategy
                  mergingStrategy
                  (SSBool "a")
                  (Single (1 :: Integer))
                  (Single (3 :: Integer))
          ifWithStrategy mergingStrategy (SSBool "b") (Single 0) x
            `shouldBe` If 0 True (SSBool "b") (Single 0) (If 1 True (SSBool "a") (Single 1) (Single 3))
        it "ifWithStrategy on Single/If for idxt == idxft" $ do
          let x =
                ifWithStrategy
                  mergingStrategy
                  (SSBool "a")
                  (Single $ Left (1 :: Integer))
                  (Single $ Right (3 :: Integer))
          ifWithStrategy mergingStrategy (SSBool "b") (Single $ Left 0) x
            `shouldBe` If
              (Left 0)
              True
              (Or (SSBool "b") (SSBool "a"))
              (If (Left 0) True (SSBool "b") (Single $ Left 0) (Single $ Left 1))
              (Single $ Right 3)
        it "ifWithStrategy on Single/If for idxt > idxft" $ do
          let x =
                ifWithStrategy
                  mergingStrategy
                  (SSBool "a")
                  (Single $ Left (1 :: Integer))
                  (Single $ Right (3 :: Integer))
          ifWithStrategy mergingStrategy (SSBool "b") (Single $ Right 0) x
            `shouldBe` If
              (Left 1)
              True
              (And (Not (SSBool "b")) (SSBool "a"))
              (Single $ Left 1)
              (If (Right 0) True (SSBool "b") (Single $ Right 0) (Single $ Right 3))
      describe "ifWithStrategy on If/Single" $ do
        describe "ifWithStrategy on If/Single degenerate to Single/Single when idxtt == idxtf" $ do
          it "ifWithStrategy on Single/If for degenerated case with idxt < idxf" $ do
            let x =
                  ifWithStrategy
                    mergingStrategy
                    (SSBool "a")
                    (Single (Left (1 :: Integer)))
                    (Single (Left (2 :: Integer)))
            ifWithStrategy mergingStrategy (SSBool "b") x (Single $ Right (2 :: Integer))
              `shouldBe` If
                (Left 1)
                True
                (SSBool "b")
                (If (Left 1) True (SSBool "a") (Single $ Left 1) (Single (Left 2)))
                (Single $ Right 2)
          it "ifWithStrategy on Single/If for degenerated case with idxt == idxf" $ do
            let x =
                  ifWithStrategy
                    mergingStrategy
                    (SSBool "a")
                    (Single (Just (1 :: Integer)))
                    (Single (Just (3 :: Integer)))
            ifWithStrategy mergingStrategy (SSBool "b") x (Single $ Just 0)
              `shouldBe` If
                (Just 0)
                True
                (Not (SSBool "b"))
                (Single $ Just 0)
                (If (Just 1) True (SSBool "a") (Single $ Just 1) (Single (Just 3)))
            ifWithStrategy mergingStrategy (SSBool "b") x (Single $ Just 1)
              `shouldBe` If (Just 1) True (Or (Not $ SSBool "b") (SSBool "a")) (Single $ Just 1) (Single (Just 3))
            ifWithStrategy mergingStrategy (SSBool "b") x (Single $ Just 2)
              `shouldBe` If
                (Just 1)
                True
                (And (SSBool "b") (SSBool "a"))
                (Single $ Just 1)
                (If (Just 2) True (Not $ SSBool "b") (Single $ Just 2) (Single $ Just 3))
            ifWithStrategy mergingStrategy (SSBool "b") x (Single $ Just 3)
              `shouldBe` If (Just 1) True (And (SSBool "b") (SSBool "a")) (Single $ Just 1) (Single (Just 3))
            ifWithStrategy mergingStrategy (SSBool "b") x (Single $ Just 4)
              `shouldBe` If
                (Just 1)
                True
                (And (SSBool "b") (SSBool "a"))
                (Single $ Just 1)
                (If (Just 3) True (SSBool "b") (Single $ Just 3) (Single $ Just 4))
          it "ifWithStrategy on Single/If for degenerated case with idxt > idxf" $ do
            let x =
                  ifWithStrategy
                    mergingStrategy
                    (SSBool "a")
                    (Single (Right (1 :: Integer)))
                    (Single (Right (2 :: Integer)))
            ifWithStrategy mergingStrategy (SSBool "b") x (Single $ Left (1 :: Integer))
              `shouldBe` If
                (Left 1)
                True
                (Not $ SSBool "b")
                (Single $ Left 1)
                (If (Right 1) True (SSBool "a") (Single $ Right 1) (Single (Right 2)))
        it "ifWithStrategy on Single/If for idxtt < idxf" $ do
          let x =
                ifWithStrategy
                  mergingStrategy
                  (SSBool "a")
                  (Single $ Left (1 :: Integer))
                  (Single $ Right (3 :: Integer))
          ifWithStrategy mergingStrategy (SSBool "b") x (Single $ Right 0)
            `shouldBe` If
              (Left 1)
              True
              (And (SSBool "b") (SSBool "a"))
              (Single $ Left 1)
              (If (Right 0) True (Not $ SSBool "b") (Single $ Right 0) (Single $ Right 3))
        it "ifWithStrategy on Single/If for idxtt == idxf" $ do
          let x =
                ifWithStrategy
                  mergingStrategy
                  (SSBool "a")
                  (Single $ Left (1 :: Integer))
                  (Single $ Right (3 :: Integer))
          ifWithStrategy mergingStrategy (SSBool "b") x (Single $ Left 0)
            `shouldBe` If
              (Left 0)
              True
              (Or (Not $ SSBool "b") (SSBool "a"))
              (If (Left 0) True (Not $ SSBool "b") (Single $ Left 0) (Single $ Left 1))
              (Single $ Right 3)
        it "ifWithStrategy on Single/If for idxtt > idxf" $ do
          let x =
                ifWithStrategy
                  mergingStrategy
                  (SSBool "a")
                  (Single (1 :: Integer))
                  (Single (3 :: Integer))
          ifWithStrategy mergingStrategy (SSBool "b") x (Single 0)
            `shouldBe` If 0 True (Not $ SSBool "b") (Single 0) (If 1 True (SSBool "a") (Single 1) (Single 3))
      describe "ifWithStrategy on If/If" $ do
        it "ifWithStrategy on If/If degenerate to Single/If when idxtt == idxtf" $ do
          let x = ifWithStrategy mergingStrategy (SSBool "a") (Single $ Left (1 :: Integer)) (Single $ Left (2 :: Integer))
          let y = ifWithStrategy mergingStrategy (SSBool "b") (Single $ Left (1 :: Integer)) (Single $ Right (2 :: Integer))
          ifWithStrategy mergingStrategy (SSBool "c") x y
            `shouldBe` If
              (Left 1)
              True
              (Or (SSBool "c") (SSBool "b"))
              (If (Left 1) True (Or (Not (SSBool "c")) (SSBool "a")) (Single $ Left 1) (Single $ Left 2))
              (Single $ Right 2)
        it "ifWithStrategy on If/If degenerate to Single/If when idxff == idxft" $ do
          let x = ifWithStrategy mergingStrategy (SSBool "a") (Single $ Left (1 :: Integer)) (Single $ Left (2 :: Integer))
          let y = ifWithStrategy mergingStrategy (SSBool "b") (Single $ Left (1 :: Integer)) (Single $ Right (2 :: Integer))
          ifWithStrategy mergingStrategy (SSBool "c") y x
            `shouldBe` If
              (Left 1)
              True
              (Or (Not $ SSBool "c") (SSBool "b"))
              (If (Left 1) True (Or (SSBool "c") (SSBool "a")) (Single $ Left 1) (Single $ Left 2))
              (Single $ Right 2)
        it "ifWithStrategy on If/If non-degenerated case when idxtt < idxft" $ do
          let x = ifWithStrategy mergingStrategy (SSBool "a") (Single $ TS1 (1 :: Integer)) (Single $ TS2 (2 :: Integer))
          let y = ifWithStrategy mergingStrategy (SSBool "b") (Single $ TS2 (1 :: Integer)) (Single $ TS3 (2 :: Integer))
          ifWithStrategy mergingStrategy (SSBool "c") x y
            `shouldBe` If
              (TS1 1)
              True
              (And (SSBool "c") (SSBool "a"))
              (Single $ TS1 1)
              ( If
                  (TS2 1)
                  True
                  (Or (SSBool "c") (SSBool "b"))
                  (If (TS2 1) True (Not $ SSBool "c") (Single $ TS2 1) (Single $ TS2 2))
                  (Single $ TS3 2)
              )
        it "ifWithStrategy on If/If non-degenerated case when idxtt == idxft" $ do
          let x = ifWithStrategy mergingStrategy (SSBool "a") (Single $ TS1 (1 :: Integer)) (Single $ TS2 (2 :: Integer))
          let y = ifWithStrategy mergingStrategy (SSBool "b") (Single $ TS1 (2 :: Integer)) (Single $ TS3 (2 :: Integer))
          ifWithStrategy mergingStrategy (SSBool "c") x y
            `shouldBe` If
              (TS1 1)
              True
              (ITE (SSBool "c") (SSBool "a") (SSBool "b"))
              (If (TS1 1) True (SSBool "c") (Single $ TS1 1) (Single $ TS1 2))
              (If (TS2 2) True (SSBool "c") (Single $ TS2 2) (Single $ TS3 2))
        it "ifWithStrategy on If/If non-degenerated case when idxtt > idxft" $ do
          let x = ifWithStrategy mergingStrategy (SSBool "a") (Single $ TS2 (1 :: Integer)) (Single $ TS3 (2 :: Integer))
          let y = ifWithStrategy mergingStrategy (SSBool "b") (Single $ TS1 (1 :: Integer)) (Single $ TS2 (2 :: Integer))
          ifWithStrategy mergingStrategy (SSBool "c") x y
            `shouldBe` If
              (TS1 1)
              True
              (And (Not $ SSBool "c") (SSBool "b"))
              (Single $ TS1 1)
              ( If
                  (TS2 1)
                  True
                  (Or (Not $ SSBool "c") (SSBool "a"))
                  (If (TS2 1) True (SSBool "c") (Single $ TS2 1) (Single $ TS2 2))
                  (Single $ TS3 2)
              )
    it "ifWithStrategy should tolerate non-merged Ifs" $ do
      let x = If (Right 2) False (SSBool "a") (Single $ Right (2 :: Integer)) (Single $ Left (2 :: Integer))
      let y = If (Right 3) False (SSBool "b") (Single $ Right 3) (Single $ Left 1)
      ifWithStrategy mergingStrategy (SSBool "c") x y
        `shouldBe` If
          (Left 1)
          True
          (ITE (SSBool "c") (Not $ SSBool "a") (Not $ SSBool "b"))
          (If (Left 1) True (Not $ SSBool "c") (Single $ Left 1) (Single $ Left 2))
          (If (Right 2) True (SSBool "c") (Single $ Right 2) (Single $ Right 3))
  describe "fullReconstruct" $ do
    it "fullReconstruct should work" $ do
      let x = If (Right 2) False (SSBool "a") (Single $ Right (2 :: Integer)) (Single $ Left (2 :: Integer))
      let y = If (Right 3) False (SSBool "b") (Single $ Right 3) (Single $ Left 1)
      let z = If (Right 2) False (SSBool "c") x y
      fullReconstruct mergingStrategy z
        `shouldBe` If
          (Left 1)
          True
          (ITE (SSBool "c") (Not $ SSBool "a") (Not $ SSBool "b"))
          (If (Left 1) True (Not $ SSBool "c") (Single $ Left 1) (Single $ Left 2))
          (If (Right 2) True (SSBool "c") (Single $ Right 2) (Single $ Right 3))
