{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Pizza.Core.Data.Class.GenSymSpec where

import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Pizza.Core.Control.Monad.UnionMBase
import Pizza.Core.Data.Class.GenSym
import Pizza.Core.Data.Class.SimpleMergeable
import Pizza.TestUtils.SBool
import Test.Hspec

spec :: Spec
spec = do
  describe "GenSym for common types" $ do
    describe "GenSym for SBool" $ do
      it "GenSym for SBool with ()" $ do
        (genSym @SBool () "a" :: UnionMBase SBool SBool)
          `shouldBe` mrgSingle (ISBool "a" 0)
        (genSymSimple () "a" :: SBool)
          `shouldBe` ISBool "a" 0
      it "GenSym for SBool with SBool" $ do
        (genSym @SBool (CBool True) "a" :: UnionMBase SBool SBool)
          `shouldBe` mrgSingle (ISBool "a" 0)
        (genSymSimple (CBool True) "a" :: SBool)
          `shouldBe` ISBool "a" 0
    describe "GenSym for Bool" $ do
      it "GenSym for Bool with ()" $ do
        (genSym @SBool () "a" :: UnionMBase SBool Bool)
          `shouldBe` mrgIf (ISBool "a" 0) (mrgSingle False) (mrgSingle True)
      it "GenSym for Bool with Bool" $ do
        (genSym @SBool True "a" :: UnionMBase SBool Bool)
          `shouldBe` mrgSingle True
        (genSym @SBool False "a" :: UnionMBase SBool Bool)
          `shouldBe` mrgSingle False
        (genSymSimple True "a" :: Bool)
          `shouldBe` True
        (genSymSimple False "a" :: Bool)
          `shouldBe` False
    describe "GenSym for Integer" $ do
      it "GenSym for Integer with Integer" $ do
        (genSym @SBool (1 :: Integer) "a" :: UnionMBase SBool Integer)
          `shouldBe` mrgSingle 1
        (genSymSimple (1 :: Integer) "a" :: Integer)
          `shouldBe` 1
      it "GenSym for Integer with upper bound" $ do
        (genSym @SBool (EnumGenUpperBound (3 :: Integer)) "a" :: UnionMBase SBool Integer)
          `shouldBe` mrgIf (ISBool "a" 0) (mrgSingle 0) (mrgIf (ISBool "a" 1) (mrgSingle 1) (mrgSingle 2))
      it "GenSym for Integer with bound" $ do
        (genSym @SBool (EnumGenBound (-1 :: Integer) 2) "a" :: UnionMBase SBool Integer)
          `shouldBe` mrgIf (ISBool "a" 0) (mrgSingle (-1)) (mrgIf (ISBool "a" 1) (mrgSingle 0) (mrgSingle 1))
    describe "GenSym for Char" $ do
      it "GenSym for Char with Char" $ do
        (genSym @SBool 'x' "a" :: UnionMBase SBool Char)
          `shouldBe` mrgSingle 'x'
        (genSymSimple 'x' "a" :: Char) `shouldBe` 'x'
      it "GenSym for Integer with upper bound" $ do
        (genSym @SBool (EnumGenUpperBound @Char (toEnum 3)) "a" :: UnionMBase SBool Char)
          `shouldBe` mrgIf (ISBool "a" 0) (mrgSingle $ toEnum 0) (mrgIf (ISBool "a" 1) (mrgSingle $ toEnum 1) (mrgSingle $ toEnum 2))
      it "GenSym for Integer with bound" $ do
        (genSym @SBool (EnumGenBound 'a' 'd') "a" :: UnionMBase SBool Char)
          `shouldBe` mrgIf (ISBool "a" 0) (mrgSingle 'a') (mrgIf (ISBool "a" 1) (mrgSingle 'b') (mrgSingle 'c'))
    describe "GenSym for Maybe" $ do
      it "GenSym for Maybe with Maybe" $ do
        (genSym (Just (SSBool "a")) "a" :: UnionMBase SBool (Maybe SBool)) `shouldBe` mrgSingle (Just (ISBool "a" 0))
        (genSym (Nothing :: Maybe SBool) "a" :: UnionMBase SBool (Maybe SBool)) `shouldBe` mrgSingle Nothing
        (genSymSimple (Just (SSBool "a")) "a" :: Maybe SBool) `shouldBe` Just (ISBool "a" 0)
        (genSymSimple (Nothing :: Maybe SBool) "a" :: Maybe SBool) `shouldBe` Nothing
      it "GenSym for Maybe with ()" $ do
        (genSym () "a" :: UnionMBase SBool (Maybe SBool))
          `shouldBe` mrgIf (ISBool "a" 0) (mrgSingle Nothing) (mrgSingle (Just (ISBool "a" 1)))
    describe "GenSym for Either" $ do
      it "GenSym for Either with Either" $ do
        (genSym (Left (SSBool "a") :: Either SBool SBool) "a" :: UnionMBase SBool (Either SBool SBool))
          `shouldBe` mrgSingle (Left (ISBool "a" 0))
        (genSym (Right (SSBool "a") :: Either SBool SBool) "a" :: UnionMBase SBool (Either SBool SBool))
          `shouldBe` mrgSingle (Right (ISBool "a" 0))
        (genSymSimple (Left (SSBool "a") :: Either SBool SBool) "a" :: Either SBool SBool)
          `shouldBe` Left (ISBool "a" 0)
        (genSymSimple (Right (SSBool "a") :: Either SBool SBool) "a" :: Either SBool SBool)
          `shouldBe` Right (ISBool "a" 0)
      it "GenSym for Either with ()" $ do
        (genSym () "a" :: UnionMBase SBool (Either SBool SBool))
          `shouldBe` mrgIf (ISBool "a" 0) (mrgSingle $ Left $ ISBool "a" 1) (mrgSingle $ Right $ ISBool "a" 2)
    describe "GenSym for List" $ do
      it "GenSym for List with max length" $ do
        (genSym (0 :: Integer) "a" :: UnionMBase SBool [SBool]) `shouldBe` mrgSingle []
        (genSym (3 :: Integer) "a" :: UnionMBase SBool [SBool])
          `shouldBe` mrgIf
            (ISBool "a" 3)
            (mrgSingle [])
            ( mrgIf
                (ISBool "a" 4)
                (mrgSingle [ISBool "a" 2])
                ( mrgIf
                    (ISBool "a" 5)
                    (mrgSingle [ISBool "a" 1, ISBool "a" 2])
                    (mrgSingle [ISBool "a" 0, ISBool "a" 1, ISBool "a" 2])
                )
            )
      it "GenSym for List with min & max length" $ do
        (genSym (ListSpec 1 3 ()) "a" :: UnionMBase SBool [SBool])
          `shouldBe` mrgIf
            (ISBool "a" 3)
            (mrgSingle [ISBool "a" 2])
            ( mrgIf
                (ISBool "a" 4)
                (mrgSingle [ISBool "a" 1, ISBool "a" 2])
                (mrgSingle [ISBool "a" 0, ISBool "a" 1, ISBool "a" 2])
            )
        (genSym (ListSpec 1 2 (ListSpec 1 2 ())) "a" :: UnionMBase SBool [UnionMBase SBool [SBool]])
          `shouldBe` mrgIf
            (ISBool "a" 6)
            ( mrgSingle
                [ mrgIf
                    (ISBool "a" 5)
                    (mrgSingle [ISBool "a" 4])
                    (mrgSingle [ISBool "a" 3, ISBool "a" 4])
                ]
            )
            ( mrgSingle
                [ mrgIf
                    (ISBool "a" 2)
                    (mrgSingle [ISBool "a" 1])
                    (mrgSingle [ISBool "a" 0, ISBool "a" 1]),
                  mrgIf
                    (ISBool "a" 5)
                    (mrgSingle [ISBool "a" 4])
                    (mrgSingle [ISBool "a" 3, ISBool "a" 4])
                ]
            )
      it "GenSym for List with exact length" $ do
        (genSym (SimpleListSpec 2 ()) "a" :: UnionMBase SBool [SBool])
          `shouldBe` mrgSingle [ISBool "a" 0, ISBool "a" 1]
        (genSymSimple (SimpleListSpec 2 ()) "a" :: [SBool])
          `shouldBe` [ISBool "a" 0, ISBool "a" 1]
        (genSym (SimpleListSpec 2 (SimpleListSpec 2 ())) "a" :: UnionMBase SBool [[SBool]])
          `shouldBe` mrgSingle [[ISBool "a" 0, ISBool "a" 1], [ISBool "a" 2, ISBool "a" 3]]
        (genSymSimple (SimpleListSpec 2 (SimpleListSpec 2 ())) "a" :: [[SBool]])
          `shouldBe` [[ISBool "a" 0, ISBool "a" 1], [ISBool "a" 2, ISBool "a" 3]]
      it "GenSym for List with same shape" $ do
        (genSym [[CBool True], [SSBool "a", SSBool "b"]] "a" :: UnionMBase SBool [[SBool]])
          `shouldBe` mrgSingle [[ISBool "a" 0], [ISBool "a" 1, ISBool "a" 2]]
        (genSymSimple [[CBool True], [SSBool "a", SSBool "b"]] "a" :: [[SBool]])
          `shouldBe` [[ISBool "a" 0], [ISBool "a" 1, ISBool "a" 2]]
    describe "GenSym for ()" $ do
      it "GenSym for () with ()" $ do
        (genSym () "a" :: UnionMBase SBool ()) `shouldBe` mrgSingle ()
        (genSymSimple () "a" :: ()) `shouldBe` ()
    describe "GenSym for (,)" $ do
      it "GenSym for (,) with some spec" $ do
        (genSym (EnumGenUpperBound @Integer 2, EnumGenUpperBound @Integer 2) "a" :: UnionMBase SBool (Integer, Integer))
          `shouldBe` do
            x1 <- mrgIf (ISBool "a" 0) (mrgSingle 0) (mrgSingle 1)
            x2 <- mrgIf (ISBool "a" 1) (mrgSingle 0) (mrgSingle 1)
            mrgSingle (x1, x2)
        (genSymSimple ((), [[SSBool "b"], [SSBool "b", SSBool "c"]]) "a" :: (SBool, [[SBool]]))
          `shouldBe` (ISBool "a" 0, [[ISBool "a" 1], [ISBool "a" 2, ISBool "a" 3]])
      it "GenSym for (,) with no spec" $ do
        (genSym () "a" :: UnionMBase SBool (SBool, SBool)) `shouldBe` mrgSingle (ISBool "a" 0, ISBool "a" 1)
        (genSymSimple () "a" :: (SBool, SBool)) `shouldBe` (ISBool "a" 0, ISBool "a" 1)
    describe "GenSym for (,,)" $ do
      it "GenSym for (,,) with some spec" $ do
        ( genSym
            ( EnumGenUpperBound @Integer 2,
              EnumGenUpperBound @Integer 2,
              EnumGenUpperBound @Integer 2
            )
            "a" ::
            UnionMBase SBool (Integer, Integer, Integer)
          )
          `shouldBe` do
            x1 <- mrgIf (ISBool "a" 0) (mrgSingle 0) (mrgSingle 1)
            x2 <- mrgIf (ISBool "a" 1) (mrgSingle 0) (mrgSingle 1)
            x3 <- mrgIf (ISBool "a" 2) (mrgSingle 0) (mrgSingle 1)
            mrgSingle (x1, x2, x3)
        (genSymSimple ((), [[SSBool "b"], [SSBool "b", SSBool "c"]], ()) "a" :: (SBool, [[SBool]], SBool))
          `shouldBe` (ISBool "a" 0, [[ISBool "a" 1], [ISBool "a" 2, ISBool "a" 3]], ISBool "a" 4)
      it "GenSym for (,,) with no spec" $ do
        (genSym () "a" :: UnionMBase SBool (SBool, SBool, SBool))
          `shouldBe` mrgSingle (ISBool "a" 0, ISBool "a" 1, ISBool "a" 2)
        (genSymSimple () "a" :: (SBool, SBool, SBool))
          `shouldBe` (ISBool "a" 0, ISBool "a" 1, ISBool "a" 2)
    describe "GenSym for (,,,)" $ do
      it "GenSym for (,,,) with some spec" $ do
        ( genSym
            ( EnumGenUpperBound @Integer 2,
              EnumGenUpperBound @Integer 2,
              EnumGenUpperBound @Integer 2,
              EnumGenUpperBound @Integer 2
            )
            "a" ::
            UnionMBase SBool (Integer, Integer, Integer, Integer)
          )
          `shouldBe` do
            x1 <- mrgIf (ISBool "a" 0) (mrgSingle 0) (mrgSingle 1)
            x2 <- mrgIf (ISBool "a" 1) (mrgSingle 0) (mrgSingle 1)
            x3 <- mrgIf (ISBool "a" 2) (mrgSingle 0) (mrgSingle 1)
            x4 <- mrgIf (ISBool "a" 3) (mrgSingle 0) (mrgSingle 1)
            mrgSingle (x1, x2, x3, x4)
        ( genSymSimple
            ((), [[SSBool "b"], [SSBool "b", SSBool "c"]], (), ())
            "a" ::
            (SBool, [[SBool]], SBool, SBool)
          )
          `shouldBe` ( ISBool "a" 0,
                       [[ISBool "a" 1], [ISBool "a" 2, ISBool "a" 3]],
                       ISBool "a" 4,
                       ISBool "a" 5
                     )
      it "GenSym for (,,,) with no spec" $ do
        (genSym () "a" :: UnionMBase SBool (SBool, SBool, SBool, SBool))
          `shouldBe` mrgSingle (ISBool "a" 0, ISBool "a" 1, ISBool "a" 2, ISBool "a" 3)
        (genSymSimple () "a" :: (SBool, SBool, SBool, SBool))
          `shouldBe` (ISBool "a" 0, ISBool "a" 1, ISBool "a" 2, ISBool "a" 3)
    describe "GenSym for (,,,,)" $ do
      it "GenSym for (,,,,) with some spec" $ do
        ( genSym
            ( EnumGenUpperBound @Integer 2,
              EnumGenUpperBound @Integer 2,
              EnumGenUpperBound @Integer 2,
              EnumGenUpperBound @Integer 2,
              EnumGenUpperBound @Integer 2
            )
            "a" ::
            UnionMBase SBool (Integer, Integer, Integer, Integer, Integer)
          )
          `shouldBe` do
            x1 <- mrgIf (ISBool "a" 0) (mrgSingle 0) (mrgSingle 1)
            x2 <- mrgIf (ISBool "a" 1) (mrgSingle 0) (mrgSingle 1)
            x3 <- mrgIf (ISBool "a" 2) (mrgSingle 0) (mrgSingle 1)
            x4 <- mrgIf (ISBool "a" 3) (mrgSingle 0) (mrgSingle 1)
            x5 <- mrgIf (ISBool "a" 4) (mrgSingle 0) (mrgSingle 1)
            mrgSingle (x1, x2, x3, x4, x5)
        ( genSymSimple
            ((), [[SSBool "b"], [SSBool "b", SSBool "c"]], (), (), ())
            "a" ::
            (SBool, [[SBool]], SBool, SBool, SBool)
          )
          `shouldBe` ( ISBool "a" 0,
                       [[ISBool "a" 1], [ISBool "a" 2, ISBool "a" 3]],
                       ISBool "a" 4,
                       ISBool "a" 5,
                       ISBool "a" 6
                     )
      it "GenSym for (,,,,) with no spec" $ do
        (genSym () "a" :: UnionMBase SBool (SBool, SBool, SBool, SBool, SBool))
          `shouldBe` mrgSingle
            (ISBool "a" 0, ISBool "a" 1, ISBool "a" 2, ISBool "a" 3, ISBool "a" 4)
        (genSymSimple () "a" :: (SBool, SBool, SBool, SBool, SBool))
          `shouldBe` ( ISBool "a" 0,
                       ISBool "a" 1,
                       ISBool "a" 2,
                       ISBool "a" 3,
                       ISBool "a" 4
                     )
    describe "GenSym for (,,,,,)" $ do
      it "GenSym for (,,,,,) with some spec" $ do
        ( genSym
            ( EnumGenUpperBound @Integer 2,
              EnumGenUpperBound @Integer 2,
              EnumGenUpperBound @Integer 2,
              EnumGenUpperBound @Integer 2,
              EnumGenUpperBound @Integer 2,
              EnumGenUpperBound @Integer 2
            )
            "a" ::
            UnionMBase SBool (Integer, Integer, Integer, Integer, Integer, Integer)
          )
          `shouldBe` do
            x1 <- mrgIf (ISBool "a" 0) (mrgSingle 0) (mrgSingle 1)
            x2 <- mrgIf (ISBool "a" 1) (mrgSingle 0) (mrgSingle 1)
            x3 <- mrgIf (ISBool "a" 2) (mrgSingle 0) (mrgSingle 1)
            x4 <- mrgIf (ISBool "a" 3) (mrgSingle 0) (mrgSingle 1)
            x5 <- mrgIf (ISBool "a" 4) (mrgSingle 0) (mrgSingle 1)
            x6 <- mrgIf (ISBool "a" 5) (mrgSingle 0) (mrgSingle 1)
            mrgSingle (x1, x2, x3, x4, x5, x6)
        ( genSymSimple
            ((), [[SSBool "b"], [SSBool "b", SSBool "c"]], (), (), (), ())
            "a" ::
            (SBool, [[SBool]], SBool, SBool, SBool, SBool)
          )
          `shouldBe` ( ISBool "a" 0,
                       [[ISBool "a" 1], [ISBool "a" 2, ISBool "a" 3]],
                       ISBool "a" 4,
                       ISBool "a" 5,
                       ISBool "a" 6,
                       ISBool "a" 7
                     )
      it "GenSym for (,,,,,) with no spec" $ do
        (genSym () "a" :: UnionMBase SBool (SBool, SBool, SBool, SBool, SBool, SBool))
          `shouldBe` mrgSingle
            ( ISBool "a" 0,
              ISBool "a" 1,
              ISBool "a" 2,
              ISBool "a" 3,
              ISBool "a" 4,
              ISBool "a" 5
            )
        (genSymSimple () "a" :: (SBool, SBool, SBool, SBool, SBool, SBool))
          `shouldBe` ( ISBool "a" 0,
                       ISBool "a" 1,
                       ISBool "a" 2,
                       ISBool "a" 3,
                       ISBool "a" 4,
                       ISBool "a" 5
                     )
    describe "GenSym for (,,,,,,)" $ do
      it "GenSym for (,,,,,,) with some spec" $ do
        ( genSym
            ( EnumGenUpperBound @Integer 2,
              EnumGenUpperBound @Integer 2,
              EnumGenUpperBound @Integer 2,
              EnumGenUpperBound @Integer 2,
              EnumGenUpperBound @Integer 2,
              EnumGenUpperBound @Integer 2,
              EnumGenUpperBound @Integer 2
            )
            "a" ::
            UnionMBase SBool (Integer, Integer, Integer, Integer, Integer, Integer, Integer)
          )
          `shouldBe` do
            x1 <- mrgIf (ISBool "a" 0) (mrgSingle 0) (mrgSingle 1)
            x2 <- mrgIf (ISBool "a" 1) (mrgSingle 0) (mrgSingle 1)
            x3 <- mrgIf (ISBool "a" 2) (mrgSingle 0) (mrgSingle 1)
            x4 <- mrgIf (ISBool "a" 3) (mrgSingle 0) (mrgSingle 1)
            x5 <- mrgIf (ISBool "a" 4) (mrgSingle 0) (mrgSingle 1)
            x6 <- mrgIf (ISBool "a" 5) (mrgSingle 0) (mrgSingle 1)
            x7 <- mrgIf (ISBool "a" 6) (mrgSingle 0) (mrgSingle 1)
            mrgSingle (x1, x2, x3, x4, x5, x6, x7)
        ( genSymSimple
            ((), [[SSBool "b"], [SSBool "b", SSBool "c"]], (), (), (), (), ())
            "a" ::
            (SBool, [[SBool]], SBool, SBool, SBool, SBool, SBool)
          )
          `shouldBe` ( ISBool "a" 0,
                       [[ISBool "a" 1], [ISBool "a" 2, ISBool "a" 3]],
                       ISBool "a" 4,
                       ISBool "a" 5,
                       ISBool "a" 6,
                       ISBool "a" 7,
                       ISBool "a" 8
                     )
      it "GenSym for (,,,,,,) with no spec" $ do
        (genSym () "a" :: UnionMBase SBool (SBool, SBool, SBool, SBool, SBool, SBool, SBool))
          `shouldBe` mrgSingle
            ( ISBool "a" 0,
              ISBool "a" 1,
              ISBool "a" 2,
              ISBool "a" 3,
              ISBool "a" 4,
              ISBool "a" 5,
              ISBool "a" 6
            )
        (genSymSimple () "a" :: (SBool, SBool, SBool, SBool, SBool, SBool, SBool))
          `shouldBe` ( ISBool "a" 0,
                       ISBool "a" 1,
                       ISBool "a" 2,
                       ISBool "a" 3,
                       ISBool "a" 4,
                       ISBool "a" 5,
                       ISBool "a" 6
                     )
    describe "GenSym for (,,,,,,,)" $ do
      it "GenSym for (,,,,,,,) with some spec" $ do
        ( genSym
            ( EnumGenUpperBound @Integer 2,
              EnumGenUpperBound @Integer 2,
              EnumGenUpperBound @Integer 2,
              EnumGenUpperBound @Integer 2,
              EnumGenUpperBound @Integer 2,
              EnumGenUpperBound @Integer 2,
              EnumGenUpperBound @Integer 2,
              EnumGenUpperBound @Integer 2
            )
            "a" ::
            UnionMBase SBool (Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer)
          )
          `shouldBe` do
            x1 <- mrgIf (ISBool "a" 0) (mrgSingle 0) (mrgSingle 1)
            x2 <- mrgIf (ISBool "a" 1) (mrgSingle 0) (mrgSingle 1)
            x3 <- mrgIf (ISBool "a" 2) (mrgSingle 0) (mrgSingle 1)
            x4 <- mrgIf (ISBool "a" 3) (mrgSingle 0) (mrgSingle 1)
            x5 <- mrgIf (ISBool "a" 4) (mrgSingle 0) (mrgSingle 1)
            x6 <- mrgIf (ISBool "a" 5) (mrgSingle 0) (mrgSingle 1)
            x7 <- mrgIf (ISBool "a" 6) (mrgSingle 0) (mrgSingle 1)
            x8 <- mrgIf (ISBool "a" 7) (mrgSingle 0) (mrgSingle 1)
            mrgSingle (x1, x2, x3, x4, x5, x6, x7, x8)
        ( genSymSimple
            ((), [[SSBool "b"], [SSBool "b", SSBool "c"]], (), (), (), (), (), ())
            "a" ::
            (SBool, [[SBool]], SBool, SBool, SBool, SBool, SBool, SBool)
          )
          `shouldBe` ( ISBool "a" 0,
                       [[ISBool "a" 1], [ISBool "a" 2, ISBool "a" 3]],
                       ISBool "a" 4,
                       ISBool "a" 5,
                       ISBool "a" 6,
                       ISBool "a" 7,
                       ISBool "a" 8,
                       ISBool "a" 9
                     )
      it "GenSym for (,,,,,,,) with no spec" $ do
        (genSym () "a" :: UnionMBase SBool (SBool, SBool, SBool, SBool, SBool, SBool, SBool, SBool))
          `shouldBe` mrgSingle
            ( ISBool "a" 0,
              ISBool "a" 1,
              ISBool "a" 2,
              ISBool "a" 3,
              ISBool "a" 4,
              ISBool "a" 5,
              ISBool "a" 6,
              ISBool "a" 7
            )
        (genSymSimple () "a" :: (SBool, SBool, SBool, SBool, SBool, SBool, SBool, SBool))
          `shouldBe` ( ISBool "a" 0,
                       ISBool "a" 1,
                       ISBool "a" 2,
                       ISBool "a" 3,
                       ISBool "a" 4,
                       ISBool "a" 5,
                       ISBool "a" 6,
                       ISBool "a" 7
                     )
    describe "GenSym for MaybeT" $ do
      it "GenSym for MaybeT with same shape" $ do
        (genSym (MaybeT Nothing :: MaybeT Maybe SBool) "a" :: UnionMBase SBool (MaybeT Maybe SBool))
          `shouldBe` mrgSingle (MaybeT Nothing)
        (genSymSimple (MaybeT Nothing :: MaybeT Maybe SBool) "a" :: MaybeT Maybe SBool)
          `shouldBe` MaybeT Nothing
        (genSym (MaybeT (Just Nothing) :: MaybeT Maybe SBool) "a" :: UnionMBase SBool (MaybeT Maybe SBool))
          `shouldBe` mrgSingle (MaybeT (Just Nothing))
        (genSymSimple (MaybeT (Just (Just $ SSBool "a")) :: MaybeT Maybe SBool) "a" :: MaybeT Maybe SBool)
          `shouldBe` MaybeT (Just (Just $ ISBool "a" 0))
        (genSym (MaybeT (Just (Just $ SSBool "a")) :: MaybeT Maybe SBool) "a" :: UnionMBase SBool (MaybeT Maybe SBool))
          `shouldBe` mrgSingle (MaybeT (Just (Just $ ISBool "a" 0)))
        (genSymSimple (MaybeT (Just (Just $ SSBool "a")) :: MaybeT Maybe SBool) "a" :: MaybeT Maybe SBool)
          `shouldBe` MaybeT (Just (Just $ ISBool "a" 0))
      it "GenSym for MaybeT with general spec" $ do
        (genSym () "a" :: UnionMBase SBool (MaybeT Maybe SBool))
          `shouldBe` mrgIf
            (ISBool "a" 0)
            (mrgSingle $ MaybeT Nothing)
            ( mrgIf
                (ISBool "a" 1)
                (mrgSingle $ MaybeT $ Just Nothing)
                (mrgSingle $ MaybeT $ Just $ Just $ ISBool "a" 2)
            )
        (genSymSimple (Nothing :: Maybe (Maybe SBool)) "a" :: MaybeT Maybe SBool)
          `shouldBe` MaybeT Nothing
        (genSymSimple (Just Nothing :: Maybe (Maybe SBool)) "a" :: MaybeT Maybe SBool)
          `shouldBe` MaybeT (Just Nothing)
        (genSymSimple (Just $ Just $ SSBool "a" :: Maybe (Maybe SBool)) "a" :: MaybeT Maybe SBool)
          `shouldBe` MaybeT (Just (Just $ ISBool "a" 0))
        (genSymSimple (Just $ Just $ SSBool "a" :: Maybe (Maybe SBool)) "a" :: MaybeT Maybe SBool)
          `shouldBe` MaybeT (Just (Just $ ISBool "a" 0))

    describe "GenSym for ExceptT" $ do
      it "GenSym for ExceptT with same shape" $ do
        (genSym (ExceptT Nothing :: ExceptT SBool Maybe SBool) "a" :: UnionMBase SBool (ExceptT SBool Maybe SBool))
          `shouldBe` mrgSingle (ExceptT Nothing)
        (genSymSimple (ExceptT Nothing :: ExceptT SBool Maybe SBool) "a" :: ExceptT SBool Maybe SBool)
          `shouldBe` ExceptT Nothing
        ( genSym (ExceptT $ Just $ Left $ SSBool "a" :: ExceptT SBool Maybe SBool) "a" ::
            UnionMBase SBool (ExceptT SBool Maybe SBool)
          )
          `shouldBe` mrgSingle (ExceptT $ Just $ Left $ ISBool "a" 0)
        ( genSymSimple
            (ExceptT $ Just $ Left $ SSBool "a" :: ExceptT SBool Maybe SBool)
            "a" ::
            ExceptT SBool Maybe SBool
          )
          `shouldBe` ExceptT (Just $ Left $ ISBool "a" 0)
        ( genSym (ExceptT $ Just $ Right $ SSBool "a" :: ExceptT SBool Maybe SBool) "a" ::
            UnionMBase SBool (ExceptT SBool Maybe SBool)
          )
          `shouldBe` mrgSingle (ExceptT $ Just $ Right $ ISBool "a" 0)
        ( genSymSimple
            (ExceptT $ Just $ Right $ SSBool "a" :: ExceptT SBool Maybe SBool)
            "a" ::
            ExceptT SBool Maybe SBool
          )
          `shouldBe` ExceptT (Just $ Right $ ISBool "a" 0)
    it "GenSym for ExceptT with general spec" $ do
      (genSym () "a" :: UnionMBase SBool (ExceptT SBool Maybe SBool))
        `shouldBe` mrgIf
          (ISBool "a" 0)
          (mrgSingle $ ExceptT Nothing)
          ( mrgIf
              (ISBool "a" 1)
              (mrgSingle $ ExceptT $ Just $ Left $ ISBool "a" 2)
              (mrgSingle $ ExceptT $ Just $ Right $ ISBool "a" 3)
          )
      (genSymSimple (Nothing :: Maybe (Either SBool SBool)) "a" :: ExceptT SBool Maybe SBool)
        `shouldBe` ExceptT Nothing
      (genSymSimple (Just $ Left $ SSBool "a" :: Maybe (Either SBool SBool)) "a" :: ExceptT SBool Maybe SBool)
        `shouldBe` ExceptT (Just (Left $ ISBool "a" 0))
      (genSymSimple (Just $ Right $ SSBool "a" :: Maybe (Either SBool SBool)) "a" :: ExceptT SBool Maybe SBool)
        `shouldBe` ExceptT (Just (Right $ ISBool "a" 0))
