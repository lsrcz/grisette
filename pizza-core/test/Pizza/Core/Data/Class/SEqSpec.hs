{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Pizza.Core.Data.Class.SEqSpec where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import Data.Bifunctor
import qualified Data.ByteString as B
import Data.Foldable
import Data.Functor.Sum
import Data.Int
import Data.Word
import Generics.Deriving
import Pizza.Core.Data.Class.Bool
import Pizza.TestUtils.SBool
import Pizza.TestUtils.SEq
import Test.Hspec
import Test.Hspec.QuickCheck

data A = A1 | A2 SBool | A3 SBool SBool
  deriving (Generic, Show, Eq)
  deriving (SEq SBool) via (Default A)

spec :: Spec
spec = do
  describe "SEq for common types" $ do
    describe "SEq for SBool" $ do
      it "SEq for concrete SBool should work" $ do
        let bools :: [Bool] = [True, False]
        traverse_ (\(i, j) -> CBool i ==~ CBool j `shouldBe` CBool (i == j)) [(x, y) | x <- bools, y <- bools]
      it "SEq for symbolic SBool should work with partial evaluation" $ do
        CBool True ==~ SSBool "a" `shouldBe` SSBool "a"
        CBool False ==~ SSBool "a" `shouldBe` Not (SSBool "a")
        SSBool "a" ==~ CBool True `shouldBe` SSBool "a"
        SSBool "a" ==~ CBool False `shouldBe` Not (SSBool "a")
      it "SEq for purely symbolic SBool should work" $ do
        SSBool "a" ==~ SSBool "b" `shouldBe` Equal (SSBool "a") (SSBool "b")
        SSBool "a" ==~ SSBool "a" `shouldBe` CBool True
    describe "SEq for Bool" $ do
      prop "SEq for Bool should work" (concreteSEqOkProp @Bool)
    describe "SEq for Integer" $ do
      prop "SEq for Integer should work" (concreteSEqOkProp @Integer)
    describe "SEq for Char" $ do
      prop "SEq for Char should work" (concreteSEqOkProp @Char)
    describe "SEq for Int" $ do
      prop "SEq for Int should work" (concreteSEqOkProp @Int)
    describe "SEq for Int8" $ do
      prop "SEq for Int8 should work" (concreteSEqOkProp @Int8)
    describe "SEq for Int16" $ do
      prop "SEq for Int16 should work" (concreteSEqOkProp @Int16)
    describe "SEq for Int32" $ do
      prop "SEq for Int32 should work" (concreteSEqOkProp @Int32)
    describe "SEq for Int64" $ do
      prop "SEq for Int64 should work" (concreteSEqOkProp @Int64)
    describe "SEq for Word" $ do
      prop "SEq for Word should work" (concreteSEqOkProp @Word)
    describe "SEq for Word8" $ do
      prop "SEq for Word8 should work" (concreteSEqOkProp @Word8)
    describe "SEq for Word16" $ do
      prop "SEq for Word16 should work" (concreteSEqOkProp @Word16)
    describe "SEq for Word32" $ do
      prop "SEq for Word32 should work" (concreteSEqOkProp @Word32)
    describe "SEq for Word64" $ do
      prop "SEq for Word64 should work" (concreteSEqOkProp @Word64)
    describe "SEq for List" $ do
      prop "SEq for concrete List should work" (concreteSEqOkProp @[Integer])
      it "SEq for general List should work" $ do
        ([] :: [Bool]) ==~ [] `shouldBe` CBool True
        [SSBool "a"] ==~ [SSBool "b"] `shouldBe` Equal (SSBool "a") (SSBool "b")
        [SSBool "a", SSBool "b"] ==~ [SSBool "c", SSBool "d"]
          `shouldBe` And (SSBool "a" ==~ SSBool "c") (SSBool "b" ==~ SSBool "d")
        [SSBool "a"] ==~ [] `shouldBe` CBool False
        [SSBool "a"] ==~ [SSBool "c", SSBool "d"] `shouldBe` CBool False
    describe "SEq for Maybe" $ do
      prop "SEq for concrete Maybe should work" (concreteSEqOkProp @(Maybe Integer))
      it "SEq for general Maybe should work" $ do
        (Nothing :: Maybe SBool) ==~ Nothing `shouldBe` CBool True
        Just (SSBool "a") ==~ Nothing `shouldBe` CBool False
        Nothing ==~ Just (SSBool "a") `shouldBe` CBool False
        Just (SSBool "a") ==~ Just (SSBool "b") `shouldBe` Equal (SSBool "a") (SSBool "b")
    describe "SEq for Either" $ do
      prop "SEq for concrete Either should work" (concreteSEqOkProp @(Either Integer Integer))
      it "SEq for general Either should work" $ do
        (Left (SSBool "a") :: Either SBool SBool) ==~ Left (SSBool "b")
          `shouldBe` Equal (SSBool "a") (SSBool "b")
        (Right (SSBool "a") :: Either SBool SBool) ==~ Left (SSBool "b") `shouldBe` CBool False
        (Left (SSBool "a") :: Either SBool SBool) ==~ Right (SSBool "b") `shouldBe` CBool False
        (Right (SSBool "a") :: Either SBool SBool) ==~ Right (SSBool "b")
          `shouldBe` Equal (SSBool "a") (SSBool "b")
    describe "SEq for MaybeT" $ do
      prop "SEq for concrete MaybeT should work" (concreteSEqOkProp @(MaybeT Maybe Integer) . bimap MaybeT MaybeT)
      it "SEq for general MaybeT should work" $ do
        (MaybeT Nothing :: MaybeT Maybe SBool) ==~ MaybeT Nothing `shouldBe` CBool True
        (MaybeT Nothing :: MaybeT Maybe SBool) ==~ MaybeT (Just Nothing) `shouldBe` CBool False
        (MaybeT Nothing :: MaybeT Maybe SBool) ==~ MaybeT (Just (Just (SSBool "a"))) `shouldBe` CBool False
        MaybeT (Just Nothing) ==~ (MaybeT Nothing :: MaybeT Maybe SBool) `shouldBe` CBool False
        MaybeT (Just (Just (SSBool "a"))) ==~ (MaybeT Nothing :: MaybeT Maybe SBool) `shouldBe` CBool False
        MaybeT (Just Nothing) ==~ (MaybeT (Just Nothing) :: MaybeT Maybe SBool) `shouldBe` CBool True
        MaybeT (Just (Just (SSBool "a"))) ==~ (MaybeT (Just Nothing) :: MaybeT Maybe SBool) `shouldBe` CBool False
        MaybeT (Just Nothing) ==~ (MaybeT (Just (Just (SSBool "b"))) :: MaybeT Maybe SBool) `shouldBe` CBool False
        MaybeT (Just (Just (SSBool "a"))) ==~ (MaybeT (Just (Just (SSBool "b"))) :: MaybeT Maybe SBool)
          `shouldBe` Equal (SSBool "a") (SSBool "b")
    describe "SEq for ExceptT" $ do
      prop "SEq for concrete ExceptT should work" (concreteSEqOkProp @(ExceptT Integer Maybe Integer) . bimap ExceptT ExceptT)
      it "SEq for general ExceptT should work" $ do
        (ExceptT Nothing :: ExceptT SBool Maybe SBool) ==~ ExceptT Nothing `shouldBe` CBool True
        (ExceptT Nothing :: ExceptT SBool Maybe SBool) ==~ ExceptT (Just (Left (SSBool "a"))) `shouldBe` CBool False
        (ExceptT Nothing :: ExceptT SBool Maybe SBool) ==~ ExceptT (Just (Right (SSBool "a"))) `shouldBe` CBool False
        ExceptT (Just (Left (SSBool "a"))) ==~ (ExceptT Nothing :: ExceptT SBool Maybe SBool) `shouldBe` CBool False
        ExceptT (Just (Right (SSBool "a"))) ==~ (ExceptT Nothing :: ExceptT SBool Maybe SBool) `shouldBe` CBool False
        ExceptT (Just (Left (SSBool "a"))) ==~ (ExceptT (Just (Left (SSBool "b"))) :: ExceptT SBool Maybe SBool)
          `shouldBe` Equal (SSBool "a") (SSBool "b")
        ExceptT (Just (Right (SSBool "a"))) ==~ (ExceptT (Just (Left (SSBool "b"))) :: ExceptT SBool Maybe SBool)
          `shouldBe` CBool False
        ExceptT (Just (Left (SSBool "a"))) ==~ (ExceptT (Just (Right (SSBool "b"))) :: ExceptT SBool Maybe SBool)
          `shouldBe` CBool False
        ExceptT (Just (Right (SSBool "a"))) ==~ (ExceptT (Just (Right (SSBool "b"))) :: ExceptT SBool Maybe SBool)
          `shouldBe` Equal (SSBool "a") (SSBool "b")
    describe "SEq for ()" $ do
      prop "SEq for () should work" (concreteSEqOkProp @())
    describe "SEq for (,)" $ do
      prop "SEq for concrete (,) should work" (concreteSEqOkProp @(Integer, Integer))
      it "SEq for general (,) should work" $ do
        (SSBool "a", SSBool "c") ==~ (SSBool "b", SSBool "d")
          `shouldBe` And (Equal (SSBool "a") (SSBool "b")) (Equal (SSBool "c") (SSBool "d"))
    describe "SEq for (,,)" $ do
      prop "SEq for concrete (,,) should work" (concreteSEqOkProp @(Integer, Integer, Integer))
      it "SEq for general (,,) should work" $ do
        (SSBool "a", SSBool "c", SSBool "e") ==~ (SSBool "b", SSBool "d", SSBool "f")
          `shouldBe` And
            (Equal (SSBool "a") (SSBool "b"))
            (And (Equal (SSBool "c") (SSBool "d")) (Equal (SSBool "e") (SSBool "f")))
    describe "SEq for (,,,)" $ do
      prop "SEq for concrete (,,,) should work" (concreteSEqOkProp @(Integer, Integer, Integer, Integer))
      it "SEq for general (,,,) should work" $ do
        (SSBool "a", SSBool "c", SSBool "e", SSBool "g") ==~ (SSBool "b", SSBool "d", SSBool "f", SSBool "h")
          `shouldBe` And
            ( And
                (Equal (SSBool "a") (SSBool "b"))
                (Equal (SSBool "c") (SSBool "d"))
            )
            ( And
                (Equal (SSBool "e") (SSBool "f"))
                (Equal (SSBool "g") (SSBool "h"))
            )
    describe "SEq for (,,,,)" $ do
      prop "SEq for concrete (,,,,) should work" (concreteSEqOkProp @(Integer, Integer, Integer, Integer, Integer))
      it "SEq for general (,,,,) should work" $ do
        (SSBool "a", SSBool "c", SSBool "e", SSBool "g", SSBool "i")
          ==~ (SSBool "b", SSBool "d", SSBool "f", SSBool "h", SSBool "j")
          `shouldBe` And
            ( And
                (Equal (SSBool "a") (SSBool "b"))
                (Equal (SSBool "c") (SSBool "d"))
            )
            ( And
                (Equal (SSBool "e") (SSBool "f"))
                ( And
                    (Equal (SSBool "g") (SSBool "h"))
                    (Equal (SSBool "i") (SSBool "j"))
                )
            )
    describe "SEq for (,,,,,)" $ do
      prop "SEq for concrete (,,,,,) should work" (concreteSEqOkProp @(Integer, Integer, Integer, Integer, Integer, Integer))
      it "SEq for general (,,,,,) should work" $ do
        (SSBool "a", SSBool "c", SSBool "e", SSBool "g", SSBool "i", SSBool "k")
          ==~ (SSBool "b", SSBool "d", SSBool "f", SSBool "h", SSBool "j", SSBool "l")
          `shouldBe` And
            ( And
                (Equal (SSBool "a") (SSBool "b"))
                ( And
                    (Equal (SSBool "c") (SSBool "d"))
                    (Equal (SSBool "e") (SSBool "f"))
                )
            )
            ( And
                (Equal (SSBool "g") (SSBool "h"))
                ( And
                    (Equal (SSBool "i") (SSBool "j"))
                    (Equal (SSBool "k") (SSBool "l"))
                )
            )
    describe "SEq for (,,,,,,)" $ do
      prop "SEq for concrete (,,,,,,) should work" (concreteSEqOkProp @(Integer, Integer, Integer, Integer, Integer, Integer, Integer))
      it "SEq for general (,,,,,) should work" $ do
        (SSBool "a", SSBool "c", SSBool "e", SSBool "g", SSBool "i", SSBool "k", SSBool "m")
          ==~ (SSBool "b", SSBool "d", SSBool "f", SSBool "h", SSBool "j", SSBool "l", SSBool "n")
          `shouldBe` And
            ( And
                (Equal (SSBool "a") (SSBool "b"))
                ( And
                    (Equal (SSBool "c") (SSBool "d"))
                    (Equal (SSBool "e") (SSBool "f"))
                )
            )
            ( And
                ( And
                    (Equal (SSBool "g") (SSBool "h"))
                    (Equal (SSBool "i") (SSBool "j"))
                )
                ( And
                    (Equal (SSBool "k") (SSBool "l"))
                    (Equal (SSBool "m") (SSBool "n"))
                )
            )
    describe "SEq for (,,,,,,,)" $ do
      prop
        "SEq for concrete (,,,,,,,) should work"
        (concreteSEqOkProp @(Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer))
      it "SEq for general (,,,,,,) should work" $ do
        (SSBool "a", SSBool "c", SSBool "e", SSBool "g", SSBool "i", SSBool "k", SSBool "m", SSBool "o")
          ==~ (SSBool "b", SSBool "d", SSBool "f", SSBool "h", SSBool "j", SSBool "l", SSBool "n", SSBool "p")
          `shouldBe` And
            ( And
                ( And
                    (Equal (SSBool "a") (SSBool "b"))
                    (Equal (SSBool "c") (SSBool "d"))
                )
                ( And
                    (Equal (SSBool "e") (SSBool "f"))
                    (Equal (SSBool "g") (SSBool "h"))
                )
            )
            ( And
                ( And
                    (Equal (SSBool "i") (SSBool "j"))
                    (Equal (SSBool "k") (SSBool "l"))
                )
                ( And
                    (Equal (SSBool "m") (SSBool "n"))
                    (Equal (SSBool "o") (SSBool "p"))
                )
            )
    describe "SEq for ByteString" $ do
      it "SEq for ByteString should work" $ do
        let bytestrings :: [B.ByteString] = ["", "a", "ab"]
        traverse_ (\(i, j) -> i ==~ j `shouldBe` CBool (i == j)) [(x, y) | x <- bytestrings, y <- bytestrings]
    describe "SEq for Sum" $ do
      prop
        "SEq for concrete Sum should work"
        ( \v ->
            let eitherToSum :: Either (Maybe Integer) (Maybe Integer) -> Sum Maybe Maybe Integer
                eitherToSum (Left x) = InL x
                eitherToSum (Right x) = InR x
             in concreteSEqOkProp (bimap eitherToSum eitherToSum v)
        )
      it "SEq for general Sum should work" $ do
        (InL $ Just $ SSBool "a" :: Sum Maybe Maybe SBool) ==~ InL (Just $ SSBool "b")
          `shouldBe` Equal (SSBool "a") (SSBool "b")
        (InL $ Just $ SSBool "a" :: Sum Maybe Maybe SBool) ==~ InR (Just $ SSBool "b") `shouldBe` CBool False
        (InR $ Just $ SSBool "a" :: Sum Maybe Maybe SBool) ==~ InR (Just $ SSBool "b")
          `shouldBe` Equal (SSBool "a") (SSBool "b")
        (InR $ Just $ SSBool "a" :: Sum Maybe Maybe SBool) ==~ InL (Just $ SSBool "b") `shouldBe` CBool False
    describe "SEq for Writer" $ do
      prop
        "SEq for concrete Lazy WriterT should work"
        ( \(v1 :: Either Integer (Integer, Integer), v2 :: Either Integer (Integer, Integer)) ->
            concreteSEqOkProp (WriterLazy.WriterT v1, WriterLazy.WriterT v2)
        )
      prop
        "SEq for concrete Strict WriterT should work"
        ( \(v1 :: Either Integer (Integer, Integer), v2 :: Either Integer (Integer, Integer)) ->
            concreteSEqOkProp (WriterStrict.WriterT v1, WriterStrict.WriterT v2)
        )
      it "SEq for general Lazy WriterT should work" $ do
        (WriterLazy.WriterT (Left $ SSBool "a") :: WriterLazy.WriterT SBool (Either SBool) SBool)
          ==~ WriterLazy.WriterT (Left $ SSBool "b")
          `shouldBe` Equal (SSBool "a") (SSBool "b")
        (WriterLazy.WriterT (Left $ SSBool "a") :: WriterLazy.WriterT SBool (Either SBool) SBool)
          ==~ WriterLazy.WriterT (Right (SSBool "b", SSBool "c"))
          `shouldBe` CBool False
        (WriterLazy.WriterT (Right (SSBool "b", SSBool "c")) :: WriterLazy.WriterT SBool (Either SBool) SBool)
          ==~ WriterLazy.WriterT (Left $ SSBool "a")
          `shouldBe` CBool False
        (WriterLazy.WriterT (Right (SSBool "a", SSBool "b")) :: WriterLazy.WriterT SBool (Either SBool) SBool)
          ==~ WriterLazy.WriterT (Right (SSBool "c", SSBool "d"))
          `shouldBe` And (Equal (SSBool "a") (SSBool "c")) (Equal (SSBool "b") (SSBool "d"))
      it "SEq for general Strict WriterT should work" $ do
        (WriterStrict.WriterT (Left $ SSBool "a") :: WriterStrict.WriterT SBool (Either SBool) SBool)
          ==~ WriterStrict.WriterT (Left $ SSBool "b")
          `shouldBe` Equal (SSBool "a") (SSBool "b")
        (WriterStrict.WriterT (Left $ SSBool "a") :: WriterStrict.WriterT SBool (Either SBool) SBool)
          ==~ WriterStrict.WriterT (Right (SSBool "b", SSBool "c"))
          `shouldBe` CBool False
        (WriterStrict.WriterT (Right (SSBool "b", SSBool "c")) :: WriterStrict.WriterT SBool (Either SBool) SBool)
          ==~ WriterStrict.WriterT (Left $ SSBool "a")
          `shouldBe` CBool False
        (WriterStrict.WriterT (Right (SSBool "a", SSBool "b")) :: WriterStrict.WriterT SBool (Either SBool) SBool)
          ==~ WriterStrict.WriterT (Right (SSBool "c", SSBool "d"))
          `shouldBe` And (Equal (SSBool "a") (SSBool "c")) (Equal (SSBool "b") (SSBool "d"))
    describe "SEq for Identity" $ do
      prop
        "SEq for concrete Identity should work"
        (\(v1 :: Integer, v2) -> concreteSEqOkProp (Identity v1, Identity v2))
      it "SEq for general IdentityT should work" $ do
        (Identity $ SSBool "a" :: Identity SBool) ==~ Identity (SSBool "b")
          `shouldBe` Equal (SSBool "a") (SSBool "b")
    describe "SEq for IdentityT" $ do
      prop
        "SEq for concrete IdentityT should work"
        (\(v1 :: Either Integer Integer, v2) -> concreteSEqOkProp (IdentityT v1, IdentityT v2))
      it "SEq for general IdentityT should work" $ do
        (IdentityT $ Left $ SSBool "a" :: IdentityT (Either SBool) SBool) ==~ IdentityT (Left $ SSBool "b")
          `shouldBe` Equal (SSBool "a") (SSBool "b")
        (IdentityT $ Left $ SSBool "a" :: IdentityT (Either SBool) SBool) ==~ IdentityT (Right $ SSBool "b")
          `shouldBe` CBool False
        (IdentityT $ Right $ SSBool "a" :: IdentityT (Either SBool) SBool) ==~ IdentityT (Left $ SSBool "b")
          `shouldBe` CBool False
        (IdentityT $ Right $ SSBool "a" :: IdentityT (Either SBool) SBool) ==~ IdentityT (Right $ SSBool "b")
          `shouldBe` Equal (SSBool "a") (SSBool "b")
  describe "deriving SEq for ADT" $ do
    it "derived SEq for simple ADT" $ do
      A1 ==~ A1 `shouldBe` CBool True
      A1 ==~ A2 (SSBool "a") `shouldBe` CBool False
      A1 ==~ A3 (SSBool "a") (SSBool "b") `shouldBe` CBool False
      A2 (SSBool "a") ==~ A1 `shouldBe` CBool False
      A2 (SSBool "a") ==~ A2 (SSBool "b") `shouldBe` Equal (SSBool "a") (SSBool "b")
      A2 (SSBool "a") ==~ A3 (SSBool "b") (SSBool "c") `shouldBe` CBool False
      A3 (SSBool "a") (SSBool "b") ==~ A1 `shouldBe` CBool False
      A3 (SSBool "a") (SSBool "b") ==~ A2 (SSBool "c") `shouldBe` CBool False
      A3 (SSBool "a") (SSBool "b") ==~ A3 (SSBool "c") (SSBool "d")
        `shouldBe` And (Equal (SSBool "a") (SSBool "c")) (Equal (SSBool "b") (SSBool "d"))
