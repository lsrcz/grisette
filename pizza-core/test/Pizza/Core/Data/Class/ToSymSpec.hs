{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Pizza.Core.Data.Class.ToSymSpec where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import qualified Control.Monad.State.Lazy as StateLazy
import qualified Control.Monad.State.Strict as StateStrict
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.ByteString.Char8 as C
import Data.Foldable
import Data.Functor.Sum
import Data.Int
import Data.Word
import Pizza.Core.Data.Class.ToSym
import Pizza.TestUtils.SBool
import Pizza.TestUtils.ToSym
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "ToSym for common types" $ do
    describe "ToSym for SBool" $ do
      it "ToSym for SBool from Bool should work" $ do
        let bools = [True, False]
        traverse_ (\v -> toSym v `shouldBe` CBool v) bools
      it "ToSym for SBool from SBool should work" $ do
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
        traverse_ (\v -> toSym v `shouldBe` v) sbools
    describe "ToSym for Bool" $ do
      prop "ToSym for Bool to Bool should be id" $ toSymForConcreteOkProp @Bool
    describe "ToSym for Integer" $ do
      prop "ToSym for Integer to Integer should be id" $ toSymForConcreteOkProp @Integer
    describe "ToSym for Char" $ do
      prop "ToSym for Char to Char should be id" $ toSymForConcreteOkProp @Char

    describe "ToSym for Int" $ do
      prop "ToSym for Int to Int should be id" $ toSymForConcreteOkProp @Int
    describe "ToSym for Int8" $ do
      prop "ToSym for Int8 to Int8 should be id" $ toSymForConcreteOkProp @Int8
    describe "ToSym for Int16" $ do
      prop "ToSym for Int16 to Int16 should be id" $ toSymForConcreteOkProp @Int16
    describe "ToSym for Int32" $ do
      prop "ToSym for Int32 to Int32 should be id" $ toSymForConcreteOkProp @Int32
    describe "ToSym for Int64" $ do
      prop "ToSym for Int64 to Int64 should be id" $ toSymForConcreteOkProp @Int64
    describe "ToSym for Word" $ do
      prop "ToSym for Word to Word should be id" $ toSymForConcreteOkProp @Word
    describe "ToSym for Word8" $ do
      prop "ToSym for Word8 to Word8 should be id" $ toSymForConcreteOkProp @Word8
    describe "ToSym for Word16" $ do
      prop "ToSym for Word16 to Word16 should be id" $ toSymForConcreteOkProp @Word16
    describe "ToSym for Word32" $ do
      prop "ToSym for Word32 to Word32 should be id" $ toSymForConcreteOkProp @Word32
    describe "ToSym for Word64" $ do
      prop "ToSym for Word64 to Word64 should be id" $ toSymForConcreteOkProp @Word64

    describe "ToSym for ()" $ do
      prop "ToSym for () to () should be id" $ toSymForConcreteOkProp @()
    describe "ToSym for ByteString" $ do
      prop "ToSym for ByteString to ByteString should be id" $
        \(v :: String) -> toSymForConcreteOkProp (C.pack v)
    describe "ToSym for List" $ do
      prop "ToSym for concrete List to concrete List should be id" $
        toSymForConcreteOkProp @[Integer]
      it "ToSym for general List should work" $ do
        toSym ([] :: [Bool]) `shouldBe` ([] :: [SBool])
        toSym ([True, False] :: [Bool]) `shouldBe` ([CBool True, CBool False] :: [SBool])
    describe "ToSym for Maybe" $ do
      prop "ToSym for concrete Maybe to concrete Maybe should be id" $
        toSymForConcreteOkProp @(Maybe Integer)
      it "ToSym for general Maybe should work" $ do
        toSym (Nothing :: Maybe Bool) `shouldBe` (Nothing :: Maybe SBool)
        toSym (Just True :: Maybe Bool) `shouldBe` (Just $ CBool True :: Maybe SBool)
    describe "ToSym for Either" $ do
      prop "ToSym for concrete Either to concrete Either should be id" $
        toSymForConcreteOkProp @(Either Integer Integer)
      it "ToSym for general Either should work" $ do
        toSym (Left True :: Either Bool Bool) `shouldBe` (Left $ CBool True :: Either SBool SBool)
        toSym (Right True :: Either Bool Bool) `shouldBe` (Right $ CBool True :: Either SBool SBool)
    describe "ToSym for MaybeT" $ do
      prop "ToSym for concrete MaybeT to concrete MaybeT should be id" $
        \(v :: Maybe (Maybe Integer)) -> toSymForConcreteOkProp $ MaybeT v
      it "ToSym for general MaybeT should work" $ do
        toSym (MaybeT Nothing :: MaybeT Maybe Bool) `shouldBe` (MaybeT Nothing :: MaybeT Maybe SBool)
        toSym (MaybeT $ Just Nothing :: MaybeT Maybe Bool) `shouldBe` (MaybeT $ Just Nothing :: MaybeT Maybe SBool)
        toSym (MaybeT $ Just $ Just True :: MaybeT Maybe Bool)
          `shouldBe` (MaybeT $ Just $ Just $ CBool True :: MaybeT Maybe SBool)
    describe "ToSym for ExceptT" $ do
      prop "ToSym for concrete ExceptT to concrete ExceptT should be id" $
        \(v :: Maybe (Either Integer Integer)) -> toSymForConcreteOkProp $ ExceptT v
      it "ToSym for general ExceptT should work" $ do
        toSym (ExceptT Nothing :: ExceptT Bool Maybe Bool) `shouldBe` (ExceptT Nothing :: ExceptT SBool Maybe SBool)
        toSym (ExceptT $ Just $ Left True :: ExceptT Bool Maybe Bool)
          `shouldBe` (ExceptT $ Just $ Left $ CBool True :: ExceptT SBool Maybe SBool)
        toSym (ExceptT $ Just $ Right False :: ExceptT Bool Maybe Bool)
          `shouldBe` (ExceptT $ Just $ Right $ CBool False :: ExceptT SBool Maybe SBool)
    describe "ToSym for (,)" $ do
      prop "ToSym for concrete (,) to concrete (,) should be id" $
        toSymForConcreteOkProp @(Integer, Integer)
      it "ToSym for general (,) should work" $
        toSym (True, False) `shouldBe` (CBool True, CBool False)
    describe "ToSym for (,,)" $ do
      prop "ToSym for concrete (,,) to concrete (,,) should be id" $
        toSymForConcreteOkProp @(Integer, Integer, Integer)
      it "ToSym for general (,,) should work" $
        toSym (True, False, True) `shouldBe` (CBool True, CBool False, CBool True)
    describe "ToSym for (,,,)" $ do
      prop "ToSym for concrete (,,,) to concrete (,,,) should be id" $
        toSymForConcreteOkProp @(Integer, Integer, Integer, Integer)
      it "ToSym for general (,,,) should work" $
        toSym (True, False, True, False) `shouldBe` (CBool True, CBool False, CBool True, CBool False)
    describe "ToSym for (,,,,)" $ do
      prop "ToSym for concrete (,,,,) to concrete (,,,,) should be id" $
        toSymForConcreteOkProp @(Integer, Integer, Integer, Integer, Integer)
      it "ToSym for general (,,,,) should work" $
        toSym (True, False, True, False, True) `shouldBe` (CBool True, CBool False, CBool True, CBool False, CBool True)
    describe "ToSym for (,,,,,)" $ do
      prop "ToSym for concrete (,,,,,) to concrete (,,,,,) should be id" $
        toSymForConcreteOkProp @(Integer, Integer, Integer, Integer, Integer, Integer)
      it "ToSym for general (,,,,,) should work" $
        toSym (True, False, True, False, True, False)
          `shouldBe` (CBool True, CBool False, CBool True, CBool False, CBool True, CBool False)
    describe "ToSym for (,,,,,,)" $ do
      prop "ToSym for concrete (,,,,,,) to concrete (,,,,,,) should be id" $
        toSymForConcreteOkProp @(Integer, Integer, Integer, Integer, Integer, Integer, Integer)
      it "ToSym for general (,,,,,,) should work" $
        toSym (True, False, True, False, True, False, True)
          `shouldBe` (CBool True, CBool False, CBool True, CBool False, CBool True, CBool False, CBool True)
    describe "ToSym for (,,,,,,,)" $ do
      prop "ToSym for concrete (,,,,,,,) to concrete (,,,,,,,) should be id" $
        toSymForConcreteOkProp @(Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer)
      it "ToSym for general (,,,,,,,) should work" $
        toSym (True, False, True, False, True, False, True, False)
          `shouldBe` (CBool True, CBool False, CBool True, CBool False, CBool True, CBool False, CBool True, CBool False)
    describe "ToSym for Sum" $ do
      prop "ToSym for concrete Sum to concrete Sum should be id" $ \(x :: Either (Maybe Integer) (Either Integer Integer)) ->
        case x of
          Left v -> toSymForConcreteOkProp @(Sum Maybe (Either Integer) Integer) (InL v)
          Right v -> toSymForConcreteOkProp @(Sum Maybe (Either Integer) Integer) (InR v)
      it "ToSym for generic Sum should work" $ do
        toSym (InL $ Just True :: Sum Maybe (Either Bool) Bool)
          `shouldBe` (InL $ Just $ CBool True :: Sum Maybe (Either SBool) SBool)
        toSym (InR $ Left True :: Sum Maybe (Either Bool) Bool)
          `shouldBe` (InR $ Left $ CBool True :: Sum Maybe (Either SBool) SBool)
        toSym (InR $ Right False :: Sum Maybe (Either Bool) Bool)
          `shouldBe` (InR $ Right $ CBool False :: Sum Maybe (Either SBool) SBool)
    describe "ToSym for functions" $ do
      prop "ToSym for general function should work" $ \(f :: [(Either Bool Bool, Either Bool Bool)], x :: Either Bool Bool) ->
        let func [] _ = Left False
            func ((fk, fv) : _) xv | fk == xv = fv
            func (_ : fs) xv = func fs xv
         in (toSym (func f x) :: Either SBool SBool) == toSym (func f) x
    describe "ToSym for Lazy StateT" $ do
      prop "ToSym for general Lazy StateT should work" $ \(f :: [(Bool, Either Bool (Bool, Bool))], x :: Bool) ->
        let func [] _ = Left False
            func ((fk, fv) : _) xv | fk == xv = fv
            func (_ : fs) xv = func fs xv
            st :: StateLazy.StateT Bool (Either Bool) Bool = StateLazy.StateT (func f)
         in (StateLazy.runStateT (toSym st) x :: Either SBool (SBool, Bool)) == toSym (func f) x
    describe "ToSym for Strict StateT" $ do
      prop "ToSym for general Strict StateT should work" $ \(f :: [(Bool, Either Bool (Bool, Bool))], x :: Bool) ->
        let func [] _ = Left False
            func ((fk, fv) : _) xv | fk == xv = fv
            func (_ : fs) xv = func fs xv
            st :: StateStrict.StateT Bool (Either Bool) Bool = StateStrict.StateT (func f)
         in (StateStrict.runStateT (toSym st) x :: Either SBool (SBool, Bool)) == toSym (func f) x
    describe "ToSym for Lazy WriterT" $ do
      prop "ToSym for general Lazy WriterT should work" $ \(f :: Either Bool (Bool, Integer)) ->
        let w :: WriterLazy.WriterT Integer (Either Bool) Bool = WriterLazy.WriterT f
         in (WriterLazy.runWriterT (toSym w) :: Either SBool (SBool, Integer)) == toSym f
      prop "ToSym for general Strict WriterT should work" $ \(f :: Either Bool (Bool, Integer)) ->
        let w :: WriterStrict.WriterT Integer (Either Bool) Bool = WriterStrict.WriterT f
         in (WriterStrict.runWriterT (toSym w) :: Either SBool (SBool, Integer)) == toSym f
    describe "ToSym for ReaderT" $ do
      prop "ToSym for general ReaderT should work" $ \(f :: [(Bool, Either Bool Bool)], x :: Bool) ->
        let func [] _ = Left False
            func ((fk, fv) : _) xv | fk == xv = fv
            func (_ : fs) xv = func fs xv
            st :: ReaderT Bool (Either Bool) Bool = ReaderT (func f)
         in (runReaderT (toSym st) x :: Either SBool SBool) == toSym (func f) x
    describe "ToSym for Identity" $ do
      prop "ToSym for concrete Identity to concrete Identity should be id" $
        toSymForConcreteOkProp @(Identity Integer)
      it "ToSym for general Identity should work" $ do
        toSym (Identity True :: Identity Bool)
          `shouldBe` (Identity $ CBool True :: Identity SBool)
    describe "ToSym for IdentityT" $ do
      prop "ToSym for concrete IdentityT to concrete IdentityT should be id" $ \x ->
        toSymForConcreteOkProp @(IdentityT Maybe Integer) (IdentityT x)
      it "ToSym for general IdentityT should work" $ do
        toSym (IdentityT (Just True) :: IdentityT Maybe Bool)
          `shouldBe` (IdentityT $ Just $ CBool True :: IdentityT Maybe SBool)
        toSym (IdentityT Nothing :: IdentityT Maybe Bool)
          `shouldBe` (IdentityT Nothing :: IdentityT Maybe SBool)
