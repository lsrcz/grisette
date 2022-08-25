{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Pizza.Core.Data.Class.ToConSpec where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.ByteString.Char8 as C
import Data.Foldable
import Data.Functor.Sum
import Data.Int
import Data.Word
import Pizza.Core.Data.Class.ToCon
import Pizza.TestUtils.SBool
import Pizza.TestUtils.ToCon
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "ToCon for common types" $ do
    describe "ToCon for SBool" $ do
      it "ToCon for concrete SBool should work" $ do
        let bools :: [Bool] = [True, False]
        traverse_ (\v -> toCon (CBool v) `shouldBe` Just v) bools
      it "ToCon for symbolic SBool to symbolic SBool should work" $ do
        let sbools :: [SBool] =
              [ CBool True,
                SSBool "a",
                ISBool "a" 1,
                And (SSBool "a") (SSBool "b"),
                Or (SSBool "a") (SSBool "b"),
                Not (SSBool "a"),
                Equal (SSBool "a") (SSBool "b"),
                ITE (SSBool "a") (SSBool "b") (SSBool "c")
              ]
        traverse_ (\v -> toCon v `shouldBe` Just v) sbools
      it "ToCon for symbolic SBool should work" $ do
        let sbools :: [SBool] =
              [ SSBool "a",
                ISBool "a" 1,
                And (SSBool "a") (SSBool "b"),
                Or (SSBool "a") (SSBool "b"),
                Not (SSBool "a"),
                Equal (SSBool "a") (SSBool "b"),
                ITE (SSBool "a") (SSBool "b") (SSBool "c")
              ]
        traverse_ (\v -> toCon v `shouldBe` (Nothing :: Maybe Bool)) sbools
    describe "ToCon for Bool" $ do
      prop "ToCon for Bool should always be identical to Just" $ toConForConcreteOkProp @Bool
    describe "ToCon for Integer" $ do
      prop "ToCon for Integer should always be identical to Just" $ toConForConcreteOkProp @Integer
    describe "ToCon for Char" $ do
      prop "ToCon for Char should always be identical to Just" $ toConForConcreteOkProp @Char

    describe "ToCon for Int" $ do
      prop "ToCon for Int should always be identical to Just" $ toConForConcreteOkProp @Int
    describe "ToCon for Int8" $ do
      prop "ToCon for Int8 should always be identical to Just" $ toConForConcreteOkProp @Int8
    describe "ToCon for Int16" $ do
      prop "ToCon for Int16 should always be identical to Just" $ toConForConcreteOkProp @Int16
    describe "ToCon for Int32" $ do
      prop "ToCon for Int32 should always be identical to Just" $ toConForConcreteOkProp @Int32
    describe "ToCon for Int64" $ do
      prop "ToCon for Int64 should always be identical to Just" $ toConForConcreteOkProp @Int64

    describe "ToCon for Word" $ do
      prop "ToCon for Word should always be identical to Just" $ toConForConcreteOkProp @Word
    describe "ToCon for Word8" $ do
      prop "ToCon for Word8 should always be identical to Just" $ toConForConcreteOkProp @Word8
    describe "ToCon for Word16" $ do
      prop "ToCon for Word16 should always be identical to Just" $ toConForConcreteOkProp @Word16
    describe "ToCon for Word32" $ do
      prop "ToCon for Word32 should always be identical to Just" $ toConForConcreteOkProp @Word32
    describe "ToCon for Word64" $ do
      prop "ToCon for Word64 should always be identical to Just" $ toConForConcreteOkProp @Word64

    describe "ToCon for ()" $ do
      prop "ToCon for () should always be identical to Just" $ toConForConcreteOkProp @()
    describe "ToCon for ByteString" $ do
      prop "ToCon for ByteString should always be identical to Just" $
        \(v :: String) -> toConForConcreteOkProp (C.pack v)
    describe "ToCon for List" $ do
      prop "ToCon for concrete List should always be identical to Just" $ toConForConcreteOkProp @[Integer]
      it "ToCon for general List should work" $ do
        toCon ([] :: [SBool]) `shouldBe` (Just [] :: Maybe [Bool])
        toCon ([CBool True] :: [SBool]) `shouldBe` (Just [True] :: Maybe [Bool])
        toCon ([SSBool "a"] :: [SBool]) `shouldBe` (Nothing :: Maybe [Bool])
        toCon ([CBool True, CBool False] :: [SBool]) `shouldBe` (Just [True, False] :: Maybe [Bool])
        toCon ([CBool True, SSBool "a"] :: [SBool]) `shouldBe` (Nothing :: Maybe [Bool])
    describe "ToCon for Maybe" $ do
      prop "ToCon for concrete Maybe should always be identical to Just" $ toConForConcreteOkProp @(Maybe Integer)
      it "ToCon for general Maybe should work" $ do
        toCon (Nothing :: Maybe SBool) `shouldBe` (Just Nothing :: Maybe (Maybe Bool))
        toCon (Just (CBool True) :: Maybe SBool) `shouldBe` (Just (Just True) :: Maybe (Maybe Bool))
        toCon (Just (SSBool "a") :: Maybe SBool) `shouldBe` (Nothing :: Maybe (Maybe Bool))
    describe "ToCon for Either" $ do
      prop "ToCon for concrete Either should always be identical to Just" $ toConForConcreteOkProp @(Either Integer Integer)
      it "ToCon for general Either should work" $ do
        toCon (Left (CBool True) :: Either SBool SBool) `shouldBe` (Just (Left True) :: Maybe (Either Bool Bool))
        toCon (Right (CBool True) :: Either SBool SBool) `shouldBe` (Just (Right True) :: Maybe (Either Bool Bool))
        toCon (Left (SSBool "a") :: Either SBool SBool) `shouldBe` (Nothing :: Maybe (Either Bool Bool))
        toCon (Right (SSBool "a") :: Either SBool SBool) `shouldBe` (Nothing :: Maybe (Either Bool Bool))
    describe "ToCon for MaybeT" $ do
      prop "ToCon for concrete MaybeT should always be identical to Just" $
        \(v :: Maybe (Maybe Integer)) -> toConForConcreteOkProp (MaybeT v)
      it "ToCon for general MaybeT should work" $ do
        toCon (MaybeT Nothing :: MaybeT Maybe SBool) `shouldBe` (Just $ MaybeT Nothing :: Maybe (MaybeT Maybe Bool))
        toCon (MaybeT $ Just Nothing :: MaybeT Maybe SBool)
          `shouldBe` (Just $ MaybeT $ Just Nothing :: Maybe (MaybeT Maybe Bool))
        toCon (MaybeT $ Just $ Just $ CBool True :: MaybeT Maybe SBool)
          `shouldBe` (Just $ MaybeT $ Just $ Just True :: Maybe (MaybeT Maybe Bool))
        toCon (MaybeT $ Just $ Just $ SSBool "a" :: MaybeT Maybe SBool)
          `shouldBe` (Nothing :: Maybe (MaybeT Maybe Bool))
    describe "ToCon for ExceptT" $ do
      prop "ToCon for concrete ExceptT should always be identical to Just" $
        \(v :: Maybe (Either Integer Integer)) -> toConForConcreteOkProp (ExceptT v)
      it "ToCon for general ExceptT should work" $ do
        toCon (ExceptT Nothing :: ExceptT SBool Maybe SBool)
          `shouldBe` (Just $ ExceptT Nothing :: Maybe (ExceptT Bool Maybe Bool))
        toCon (ExceptT $ Just $ Left $ CBool True :: ExceptT SBool Maybe SBool)
          `shouldBe` (Just $ ExceptT $ Just $ Left True :: Maybe (ExceptT Bool Maybe Bool))
        toCon (ExceptT $ Just $ Left $ SSBool "a" :: ExceptT SBool Maybe SBool)
          `shouldBe` (Nothing :: Maybe (ExceptT Bool Maybe Bool))
        toCon (ExceptT $ Just $ Right $ CBool True :: ExceptT SBool Maybe SBool)
          `shouldBe` (Just $ ExceptT $ Just $ Right True :: Maybe (ExceptT Bool Maybe Bool))
        toCon (ExceptT $ Just $ Right $ SSBool "a" :: ExceptT SBool Maybe SBool)
          `shouldBe` (Nothing :: Maybe (ExceptT Bool Maybe Bool))
    describe "ToCon for (,)" $ do
      prop "ToCon for concrete (,) should always be identical to Just" $
        toConForConcreteOkProp @(Integer, Integer)
      it "ToCon for generic (,) should work" $ do
        toCon (CBool True, CBool False) `shouldBe` Just (True, False)
        toCon (CBool True, SSBool "a") `shouldBe` (Nothing :: Maybe (Bool, Bool))
    describe "ToCon for (,,)" $ do
      prop "ToCon for concrete (,,) should always be identical to Just" $
        toConForConcreteOkProp @(Integer, Integer, Integer)
      it "ToCon for generic (,,) should work" $ do
        toCon (CBool False, CBool True, CBool False) `shouldBe` Just (False, True, False)
        toCon (CBool False, CBool True, SSBool "a") `shouldBe` (Nothing :: Maybe (Bool, Bool, Bool))
    describe "ToCon for (,,,)" $ do
      prop "ToCon for concrete (,,,) should always be identical to Just" $
        toConForConcreteOkProp @(Integer, Integer, Integer, Integer)
      it "ToCon for generic (,,,) should work" $ do
        toCon (CBool True, CBool False, CBool True, CBool False) `shouldBe` Just (True, False, True, False)
        toCon (CBool True, CBool False, CBool True, SSBool "a") `shouldBe` (Nothing :: Maybe (Bool, Bool, Bool, Bool))
    describe "ToCon for (,,,,)" $ do
      prop "ToCon for concrete (,,,,) should always be identical to Just" $
        toConForConcreteOkProp @(Integer, Integer, Integer, Integer, Integer)
      it "ToCon for generic (,,,,) should work" $ do
        toCon (CBool False, CBool True, CBool False, CBool True, CBool False)
          `shouldBe` Just (False, True, False, True, False)
        toCon (CBool False, CBool True, CBool False, CBool True, SSBool "a")
          `shouldBe` (Nothing :: Maybe (Bool, Bool, Bool, Bool, Bool))
    describe "ToCon for (,,,,,)" $ do
      prop "ToCon for concrete (,,,,,) should always be identical to Just" $
        toConForConcreteOkProp @(Integer, Integer, Integer, Integer, Integer, Integer)
      it "ToCon for generic (,,,,,) should work" $ do
        toCon (CBool True, CBool False, CBool True, CBool False, CBool True, CBool False)
          `shouldBe` Just (True, False, True, False, True, False)
        toCon (CBool True, CBool False, CBool True, CBool False, CBool True, SSBool "a")
          `shouldBe` (Nothing :: Maybe (Bool, Bool, Bool, Bool, Bool, Bool))
    describe "ToCon for (,,,,,,)" $ do
      prop "ToCon for concrete (,,,,,,) should always be identical to Just" $
        toConForConcreteOkProp @(Integer, Integer, Integer, Integer, Integer, Integer, Integer)
      it "ToCon for generic (,,,,,,) should work" $ do
        toCon (CBool False, CBool True, CBool False, CBool True, CBool False, CBool True, CBool False)
          `shouldBe` Just (False, True, False, True, False, True, False)
        toCon (CBool False, CBool True, CBool False, CBool True, CBool False, CBool True, SSBool "a")
          `shouldBe` (Nothing :: Maybe (Bool, Bool, Bool, Bool, Bool, Bool, Bool))
    describe "ToCon for (,,,,,,,)" $ do
      prop "ToCon for concrete (,,,,,,,) should always be identical to Just" $
        toConForConcreteOkProp @(Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer)
      it "ToCon for generic (,,,,,,,) should work" $ do
        toCon (CBool True, CBool False, CBool True, CBool False, CBool True, CBool False, CBool True, CBool False)
          `shouldBe` Just (True, False, True, False, True, False, True, False)
        toCon (CBool True, CBool False, CBool True, CBool False, CBool True, CBool False, CBool True, SSBool "a")
          `shouldBe` (Nothing :: Maybe (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool))
    describe "ToCon for Sum" $ do
      prop "ToCon for concrete Sum should always be identical to Just" $
        \(v :: Either (Maybe Integer) (Maybe Integer)) -> toConForConcreteOkProp $ case v of
          Left x -> InL x
          Right x -> InR x
      it "ToCon for generic Sum should work" $ do
        toCon (InL (Just (CBool True)) :: Sum Maybe (Either SBool) SBool)
          `shouldBe` (Just (InL (Just True)) :: Maybe (Sum Maybe (Either Bool) Bool))
        toCon (InL (Just (SSBool "a")) :: Sum Maybe (Either SBool) SBool)
          `shouldBe` (Nothing :: Maybe (Sum Maybe (Either Bool) Bool))
        toCon (InR (Left (CBool True)) :: Sum Maybe (Either SBool) SBool)
          `shouldBe` (Just (InR (Left True)) :: Maybe (Sum Maybe (Either Bool) Bool))
        toCon (InR (Right (CBool True)) :: Sum Maybe (Either SBool) SBool)
          `shouldBe` (Just (InR (Right True)) :: Maybe (Sum Maybe (Either Bool) Bool))
        toCon (InR (Left (SSBool "a")) :: Sum Maybe (Either SBool) SBool)
          `shouldBe` (Nothing :: Maybe (Sum Maybe (Either Bool) Bool))
        toCon (InR (Right (SSBool "a")) :: Sum Maybe (Either SBool) SBool)
          `shouldBe` (Nothing :: Maybe (Sum Maybe (Either Bool) Bool))
    describe "ToCon for WriterT" $ do
      prop "ToCon for concrete Lazy WriterT should always be identical to Just" $
        \(v :: Either Integer (Integer, Integer)) -> toConForConcreteOkProp $ WriterLazy.WriterT v
      prop "ToCon for concrete Strict WriterT should always be identical to Just" $
        \(v :: Either Integer (Integer, Integer)) -> toConForConcreteOkProp $ WriterStrict.WriterT v
      it "ToCon for generic Lazy WriterT should work" $ do
        toCon (WriterLazy.WriterT $ Left $ CBool True :: WriterLazy.WriterT SBool (Either SBool) SBool)
          `shouldBe` Just (WriterLazy.WriterT $ Left True :: WriterLazy.WriterT Bool (Either Bool) Bool)
        toCon (WriterLazy.WriterT $ Left $ SSBool "a" :: WriterLazy.WriterT SBool (Either SBool) SBool)
          `shouldBe` (Nothing :: Maybe (WriterLazy.WriterT Bool (Either Bool) Bool))
        toCon (WriterLazy.WriterT $ Right (CBool True, CBool True) :: WriterLazy.WriterT SBool (Either SBool) SBool)
          `shouldBe` Just (WriterLazy.WriterT $ Right (True, True) :: WriterLazy.WriterT Bool (Either Bool) Bool)
        toCon (WriterLazy.WriterT $ Right (SSBool "a", CBool True) :: WriterLazy.WriterT SBool (Either SBool) SBool)
          `shouldBe` (Nothing :: Maybe (WriterLazy.WriterT Bool (Either Bool) Bool))
        toCon (WriterLazy.WriterT $ Right (CBool True, SSBool "a") :: WriterLazy.WriterT SBool (Either SBool) SBool)
          `shouldBe` (Nothing :: Maybe (WriterLazy.WriterT Bool (Either Bool) Bool))
        toCon (WriterLazy.WriterT $ Right (SSBool "a", SSBool "b") :: WriterLazy.WriterT SBool (Either SBool) SBool)
          `shouldBe` (Nothing :: Maybe (WriterLazy.WriterT Bool (Either Bool) Bool))
      it "ToCon for generic Strict WriterT should work" $ do
        toCon (WriterStrict.WriterT $ Left $ CBool True :: WriterStrict.WriterT SBool (Either SBool) SBool)
          `shouldBe` Just (WriterStrict.WriterT $ Left True :: WriterStrict.WriterT Bool (Either Bool) Bool)
        toCon (WriterStrict.WriterT $ Left $ SSBool "a" :: WriterStrict.WriterT SBool (Either SBool) SBool)
          `shouldBe` (Nothing :: Maybe (WriterStrict.WriterT Bool (Either Bool) Bool))
        toCon (WriterStrict.WriterT $ Right (CBool True, CBool True) :: WriterStrict.WriterT SBool (Either SBool) SBool)
          `shouldBe` Just (WriterStrict.WriterT $ Right (True, True) :: WriterStrict.WriterT Bool (Either Bool) Bool)
        toCon (WriterStrict.WriterT $ Right (SSBool "a", CBool True) :: WriterStrict.WriterT SBool (Either SBool) SBool)
          `shouldBe` (Nothing :: Maybe (WriterStrict.WriterT Bool (Either Bool) Bool))
        toCon (WriterStrict.WriterT $ Right (CBool True, SSBool "a") :: WriterStrict.WriterT SBool (Either SBool) SBool)
          `shouldBe` (Nothing :: Maybe (WriterStrict.WriterT Bool (Either Bool) Bool))
        toCon (WriterStrict.WriterT $ Right (SSBool "a", SSBool "b") :: WriterStrict.WriterT SBool (Either SBool) SBool)
          `shouldBe` (Nothing :: Maybe (WriterStrict.WriterT Bool (Either Bool) Bool))
    describe "ToCon for Identity" $ do
      prop "ToCon for concrete Identity should always be identical to Just" $
        \(v :: Integer) -> toConForConcreteOkProp $ Identity v
      it "ToCon for general Identity should work" $ do
        toCon (Identity $ CBool True) `shouldBe` Just (Identity True)
        toCon (Identity $ SSBool "a") `shouldBe` (Nothing :: Maybe (Identity Bool))
    describe "ToCon for IdentityT" $ do
      prop "ToCon for concrete IdentityT should always be identical to Just" $
        \(v :: Either Integer Integer) -> toConForConcreteOkProp $ IdentityT v
      it "ToCon for general IdentityT should work" $ do
        toCon (IdentityT $ Left $ CBool True :: IdentityT (Either SBool) SBool)
          `shouldBe` Just (IdentityT $ Left True :: IdentityT (Either Bool) Bool)
        toCon (IdentityT $ Left $ SSBool "a" :: IdentityT (Either SBool) SBool)
          `shouldBe` (Nothing :: Maybe (IdentityT (Either Bool) Bool))
        toCon (IdentityT $ Right $ CBool True :: IdentityT (Either SBool) SBool)
          `shouldBe` Just (IdentityT $ Right True :: IdentityT (Either Bool) Bool)
        toCon (IdentityT $ Right $ SSBool "a" :: IdentityT (Either SBool) SBool)
          `shouldBe` (Nothing :: Maybe (IdentityT (Either Bool) Bool))
