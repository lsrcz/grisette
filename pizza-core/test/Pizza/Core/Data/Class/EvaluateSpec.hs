{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Pizza.Core.Data.Class.EvaluateSpec where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.ByteString as B
import Data.Functor.Sum
import qualified Data.HashMap.Strict as M
import Data.Int
import Data.Word
import Pizza.Core.Data.Class.Evaluate
import Pizza.TestUtils.Evaluate
import Pizza.TestUtils.SBool
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "Evaluate for common types" $ do
    describe "Evaluate for SBool" $ do
      it "Evaluate for SBool with empty model / no fill default" $ do
        let model = M.empty :: M.HashMap Symbol Bool
        evaluateSym False model (CBool True) `shouldBe` CBool True
        evaluateSym False model (SSBool "a") `shouldBe` SSBool "a"
        evaluateSym False model (ISBool "a" 1) `shouldBe` ISBool "a" 1
        evaluateSym False model (Or (SSBool "a") (SSBool "b"))
          `shouldBe` Or (SSBool "a") (SSBool "b")
        evaluateSym False model (And (SSBool "a") (SSBool "b"))
          `shouldBe` And (SSBool "a") (SSBool "b")
        evaluateSym False model (Equal (SSBool "a") (SSBool "b"))
          `shouldBe` Equal (SSBool "a") (SSBool "b")
        evaluateSym False model (Not (SSBool "a"))
          `shouldBe` Not (SSBool "a")
        evaluateSym False model (ITE (SSBool "a") (SSBool "b") (SSBool "c"))
          `shouldBe` ITE (SSBool "a") (SSBool "b") (SSBool "c")
      it "Evaluate for SBool with empty model / with fill default" $ do
        let model = M.empty :: M.HashMap Symbol Bool
        evaluateSym True model (CBool True) `shouldBe` CBool True
        evaluateSym True model (SSBool "a") `shouldBe` CBool False
        evaluateSym True model (ISBool "a" 1) `shouldBe` CBool False
        evaluateSym True model (Or (SSBool "a") (SSBool "b")) `shouldBe` CBool False
        evaluateSym True model (And (SSBool "a") (SSBool "b")) `shouldBe` CBool False
        evaluateSym True model (Equal (SSBool "a") (SSBool "b")) `shouldBe` CBool True
        evaluateSym True model (Not (SSBool "a")) `shouldBe` CBool True
        evaluateSym True model (ITE (SSBool "a") (SSBool "b") (SSBool "c")) `shouldBe` CBool False
      it "Evaluate for SBool with some model" $ do
        let model =
              M.fromList
                [ (SSymbol "a", True),
                  (ISymbol "a" 1, False),
                  (SSymbol "b", False),
                  (SSymbol "c", True)
                ] ::
                M.HashMap Symbol Bool
        evaluateSym True model (CBool True) `shouldBe` CBool True
        evaluateSym True model (SSBool "a") `shouldBe` CBool True
        evaluateSym True model (ISBool "a" 1) `shouldBe` CBool False
        evaluateSym True model (Or (SSBool "a") (SSBool "b")) `shouldBe` CBool True
        evaluateSym True model (And (SSBool "a") (SSBool "b")) `shouldBe` CBool False
        evaluateSym True model (Equal (SSBool "a") (SSBool "b")) `shouldBe` CBool False
        evaluateSym True model (Not (SSBool "a")) `shouldBe` CBool False
        evaluateSym True model (ITE (SSBool "a") (SSBool "b") (SSBool "c")) `shouldBe` CBool False
    describe "Evaluate for Bool" $ do
      prop "Evaluate for Bool should work" (concreteEvaluateSymOkProp @Bool)
    describe "Evaluate for Integer" $ do
      prop "Evaluate for Integer should work" (concreteEvaluateSymOkProp @Integer)
    describe "Evaluate for Char" $ do
      prop "Evaluate for Char should work" (concreteEvaluateSymOkProp @Char)

    describe "Evaluate for Int" $ do
      prop "Evaluate for Int should work" (concreteEvaluateSymOkProp @Int)
    describe "Evaluate for Int8" $ do
      prop "Evaluate for Int8 should work" (concreteEvaluateSymOkProp @Int8)
    describe "Evaluate for Int16" $ do
      prop "Evaluate for Int16 should work" (concreteEvaluateSymOkProp @Int16)
    describe "Evaluate for Int32" $ do
      prop "Evaluate for Int32 should work" (concreteEvaluateSymOkProp @Int32)
    describe "Evaluate for Int64" $ do
      prop "Evaluate for Int64 should work" (concreteEvaluateSymOkProp @Int64)
    describe "Evaluate for Word" $ do
      prop "Evaluate for Word should work" (concreteEvaluateSymOkProp @Word)
    describe "Evaluate for Word8" $ do
      prop "Evaluate for Word8 should work" (concreteEvaluateSymOkProp @Word8)
    describe "Evaluate for Word16" $ do
      prop "Evaluate for Word16 should work" (concreteEvaluateSymOkProp @Word16)
    describe "Evaluate for Word32" $ do
      prop "Evaluate for Word32 should work" (concreteEvaluateSymOkProp @Word32)
    describe "Evaluate for Word64" $ do
      prop "Evaluate for Word64 should work" (concreteEvaluateSymOkProp @Word64)

    describe "Evaluate for List" $ do
      prop "Evaluate for concrete List should work" (concreteEvaluateSymOkProp @[Integer])
      it "Evaluate for List should work" $ do
        let model =
              M.fromList
                [ (SSymbol "a", True),
                  (SSymbol "b", False)
                ] ::
                M.HashMap Symbol Bool
        evaluateSym True model ([] :: [SBool]) `shouldBe` []
        evaluateSym False model ([] :: [SBool]) `shouldBe` []
        evaluateSym False model [SSBool "a", SSBool "b", SSBool "c"] `shouldBe` [CBool True, CBool False, SSBool "c"]
        evaluateSym True model [SSBool "a", SSBool "b", SSBool "c"] `shouldBe` [CBool True, CBool False, CBool False]
    describe "Evaluate for Maybe" $ do
      prop "Evaluate for concrete Maybe should work" (concreteEvaluateSymOkProp @(Maybe Integer))
      it "Evaluate for Maybe should work" $ do
        let model = M.fromList [(SSymbol "a", True)] :: M.HashMap Symbol Bool
        evaluateSym False model (Nothing :: Maybe SBool) `shouldBe` Nothing
        evaluateSym True model (Nothing :: Maybe SBool) `shouldBe` Nothing
        evaluateSym False model (Just (SSBool "a")) `shouldBe` Just (CBool True)
        evaluateSym True model (Just (SSBool "a")) `shouldBe` Just (CBool True)
        evaluateSym False model (Just (SSBool "b")) `shouldBe` Just (SSBool "b")
        evaluateSym True model (Just (SSBool "b")) `shouldBe` Just (CBool False)
    describe "Evaluate for Either" $ do
      prop "Evaluate for concrete Either should work" (concreteEvaluateSymOkProp @(Either Integer Integer))
      it "Evaluate for Either should work" $ do
        let model = M.fromList [(SSymbol "a", True)] :: M.HashMap Symbol Bool
        evaluateSym False model (Left (SSBool "a") :: Either SBool SBool) `shouldBe` Left (CBool True)
        evaluateSym True model (Left (SSBool "a") :: Either SBool SBool) `shouldBe` Left (CBool True)
        evaluateSym False model (Left (SSBool "b") :: Either SBool SBool) `shouldBe` Left (SSBool "b")
        evaluateSym True model (Left (SSBool "b") :: Either SBool SBool) `shouldBe` Left (CBool False)
        evaluateSym False model (Right (SSBool "a") :: Either SBool SBool) `shouldBe` Right (CBool True)
        evaluateSym True model (Right (SSBool "a") :: Either SBool SBool) `shouldBe` Right (CBool True)
        evaluateSym False model (Right (SSBool "b") :: Either SBool SBool) `shouldBe` Right (SSBool "b")
        evaluateSym True model (Right (SSBool "b") :: Either SBool SBool) `shouldBe` Right (CBool False)
    describe "Evaluate for MaybeT" $ do
      prop "Evaluate for concrete MaybeT should work" (concreteEvaluateSymOkProp @(MaybeT Maybe Integer) . MaybeT)
      it "Evaluate for MaybeT should work" $ do
        let model = M.fromList [(SSymbol "a", True)] :: M.HashMap Symbol Bool
        evaluateSym False model (MaybeT Nothing :: MaybeT Maybe SBool) `shouldBe` MaybeT Nothing
        evaluateSym True model (MaybeT Nothing :: MaybeT Maybe SBool) `shouldBe` MaybeT Nothing
        evaluateSym False model (MaybeT $ Just Nothing :: MaybeT Maybe SBool) `shouldBe` MaybeT (Just Nothing)
        evaluateSym True model (MaybeT $ Just Nothing :: MaybeT Maybe SBool) `shouldBe` MaybeT (Just Nothing)
        evaluateSym False model (MaybeT $ Just $ Just $ SSBool "a") `shouldBe` MaybeT (Just (Just (CBool True)))
        evaluateSym True model (MaybeT $ Just $ Just $ SSBool "a") `shouldBe` MaybeT (Just (Just (CBool True)))
        evaluateSym False model (MaybeT $ Just $ Just $ SSBool "b") `shouldBe` MaybeT (Just (Just (SSBool "b")))
        evaluateSym True model (MaybeT $ Just $ Just $ SSBool "b") `shouldBe` MaybeT (Just (Just (CBool False)))
    describe "Evaluate for ExceptT" $ do
      prop "Evaluate for concrete ExceptT should work" (concreteEvaluateSymOkProp @(ExceptT Integer Maybe Integer) . ExceptT)
      it "Evaluate for MaybeT should work" $ do
        let model = M.fromList [(SSymbol "a", True)] :: M.HashMap Symbol Bool
        evaluateSym False model (ExceptT Nothing :: ExceptT SBool Maybe SBool) `shouldBe` ExceptT Nothing
        evaluateSym True model (ExceptT Nothing :: ExceptT SBool Maybe SBool) `shouldBe` ExceptT Nothing

        evaluateSym False model (ExceptT $ Just $ Left $ SSBool "a" :: ExceptT SBool Maybe SBool)
          `shouldBe` ExceptT (Just $ Left $ CBool True)
        evaluateSym True model (ExceptT $ Just $ Left $ SSBool "a" :: ExceptT SBool Maybe SBool)
          `shouldBe` ExceptT (Just $ Left $ CBool True)
        evaluateSym False model (ExceptT $ Just $ Left $ SSBool "b" :: ExceptT SBool Maybe SBool)
          `shouldBe` ExceptT (Just $ Left $ SSBool "b")
        evaluateSym True model (ExceptT $ Just $ Left $ SSBool "b" :: ExceptT SBool Maybe SBool)
          `shouldBe` ExceptT (Just $ Left $ CBool False)

        evaluateSym False model (ExceptT $ Just $ Right $ SSBool "a" :: ExceptT SBool Maybe SBool)
          `shouldBe` ExceptT (Just $ Right $ CBool True)
        evaluateSym True model (ExceptT $ Just $ Right $ SSBool "a" :: ExceptT SBool Maybe SBool)
          `shouldBe` ExceptT (Just $ Right $ CBool True)
        evaluateSym False model (ExceptT $ Just $ Right $ SSBool "b" :: ExceptT SBool Maybe SBool)
          `shouldBe` ExceptT (Just $ Right $ SSBool "b")
        evaluateSym True model (ExceptT $ Just $ Right $ SSBool "b" :: ExceptT SBool Maybe SBool)
          `shouldBe` ExceptT (Just $ Right $ CBool False)
    describe "Evaluate for ()" $ do
      prop "Evaluate for () should work" (concreteEvaluateSymOkProp @())
    describe "Evaluate for (,)" $ do
      prop "Evaluate for concrete (,) should work" (concreteEvaluateSymOkProp @(Integer, Integer))
      it "Evaluate for (,) should work" $ do
        let model = M.fromList [(SSymbol "a", True)] :: M.HashMap Symbol Bool
        evaluateSym False model (SSBool "a", SSBool "b") `shouldBe` (CBool True, SSBool "b")
        evaluateSym True model (SSBool "a", SSBool "b") `shouldBe` (CBool True, CBool False)
    describe "Evaluate for (,,)" $ do
      prop "Evaluate for concrete (,,) should work" (concreteEvaluateSymOkProp @(Integer, Integer, Integer))
      it "Evaluate for (,,) should work" $ do
        let model = M.fromList [(SSymbol "a", True)] :: M.HashMap Symbol Bool
        evaluateSym False model (SSBool "a", SSBool "b", SSBool "c") `shouldBe` (CBool True, SSBool "b", SSBool "c")
        evaluateSym True model (SSBool "a", SSBool "b", SSBool "c") `shouldBe` (CBool True, CBool False, CBool False)
    describe "Evaluate for (,,,)" $ do
      prop "Evaluate for concrete (,,,) should work" (concreteEvaluateSymOkProp @(Integer, Integer, Integer, Integer))
      it "Evaluate for (,,,) should work" $ do
        let model = M.fromList [(SSymbol "a", True)] :: M.HashMap Symbol Bool
        evaluateSym False model (SSBool "a", SSBool "b", SSBool "c", SSBool "d")
          `shouldBe` (CBool True, SSBool "b", SSBool "c", SSBool "d")
        evaluateSym True model (SSBool "a", SSBool "b", SSBool "c", SSBool "d")
          `shouldBe` (CBool True, CBool False, CBool False, CBool False)
    describe "Evaluate for (,,,,)" $ do
      prop
        "Evaluate for concrete (,,,,) should work"
        (concreteEvaluateSymOkProp @(Integer, Integer, Integer, Integer, Integer))
      it "Evaluate for (,,,,) should work" $ do
        let model = M.fromList [(SSymbol "a", True)] :: M.HashMap Symbol Bool
        evaluateSym False model (SSBool "a", SSBool "b", SSBool "c", SSBool "d", SSBool "e")
          `shouldBe` (CBool True, SSBool "b", SSBool "c", SSBool "d", SSBool "e")
        evaluateSym True model (SSBool "a", SSBool "b", SSBool "c", SSBool "d", SSBool "e")
          `shouldBe` (CBool True, CBool False, CBool False, CBool False, CBool False)
    describe "Evaluate for (,,,,,)" $ do
      prop
        "Evaluate for concrete (,,,,,) should work"
        (concreteEvaluateSymOkProp @(Integer, Integer, Integer, Integer, Integer, Integer))
      it "Evaluate for (,,,,,) should work" $ do
        let model = M.fromList [(SSymbol "a", True)] :: M.HashMap Symbol Bool
        evaluateSym False model (SSBool "a", SSBool "b", SSBool "c", SSBool "d", SSBool "e", SSBool "f")
          `shouldBe` (CBool True, SSBool "b", SSBool "c", SSBool "d", SSBool "e", SSBool "f")
        evaluateSym True model (SSBool "a", SSBool "b", SSBool "c", SSBool "d", SSBool "e", SSBool "f")
          `shouldBe` (CBool True, CBool False, CBool False, CBool False, CBool False, CBool False)
    describe "Evaluate for (,,,,,,)" $ do
      prop
        "Evaluate for concrete (,,,,,,) should work"
        (concreteEvaluateSymOkProp @(Integer, Integer, Integer, Integer, Integer, Integer, Integer))
      it "Evaluate for (,,,,,,) should work" $ do
        let model = M.fromList [(SSymbol "a", True)] :: M.HashMap Symbol Bool
        evaluateSym
          False
          model
          (SSBool "a", SSBool "b", SSBool "c", SSBool "d", SSBool "e", SSBool "f", SSBool "g")
          `shouldBe` (CBool True, SSBool "b", SSBool "c", SSBool "d", SSBool "e", SSBool "f", SSBool "g")
        evaluateSym
          True
          model
          (SSBool "a", SSBool "b", SSBool "c", SSBool "d", SSBool "e", SSBool "f", SSBool "h")
          `shouldBe` (CBool True, CBool False, CBool False, CBool False, CBool False, CBool False, CBool False)
    describe "Evaluate for (,,,,,,,)" $ do
      prop
        "Evaluate for concrete (,,,,,,,) should work"
        (concreteEvaluateSymOkProp @(Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer))
      it "Evaluate for (,,,,,,,) should work" $ do
        let model = M.fromList [(SSymbol "a", True)] :: M.HashMap Symbol Bool
        evaluateSym
          False
          model
          (SSBool "a", SSBool "b", SSBool "c", SSBool "d", SSBool "e", SSBool "f", SSBool "g", SSBool "h")
          `shouldBe` (CBool True, SSBool "b", SSBool "c", SSBool "d", SSBool "e", SSBool "f", SSBool "g", SSBool "h")
        evaluateSym
          True
          model
          (SSBool "a", SSBool "b", SSBool "c", SSBool "d", SSBool "e", SSBool "f", SSBool "h", SSBool "h")
          `shouldBe` (CBool True, CBool False, CBool False, CBool False, CBool False, CBool False, CBool False, CBool False)
    describe "Evaluate for ByteString" $
      it "Evaluate for ByteString should work" $ do
        let model = M.fromList [(SSymbol "a", True)] :: M.HashMap Symbol Bool
        evaluateSym False model ("" :: B.ByteString) `shouldBe` ""
        evaluateSym True model ("" :: B.ByteString) `shouldBe` ""
        evaluateSym False model ("a" :: B.ByteString) `shouldBe` "a"
        evaluateSym True model ("a" :: B.ByteString) `shouldBe` "a"
    describe "Evaluate for Sum" $ do
      prop
        "Evaluate for concrete Sum should work"
        ( \(x :: Either (Maybe Integer) (Maybe Integer)) -> case x of
            Left val -> concreteEvaluateSymOkProp @(Sum Maybe Maybe Integer) $ InL val
            Right val -> concreteEvaluateSymOkProp @(Sum Maybe Maybe Integer) $ InR val
        )
      it "Evaluate for Sum should work" $ do
        let model = M.fromList [(SSymbol "a", True)] :: M.HashMap Symbol Bool
        evaluateSym False model (InL Nothing :: Sum Maybe Maybe SBool) `shouldBe` InL Nothing
        evaluateSym True model (InL Nothing :: Sum Maybe Maybe SBool) `shouldBe` InL Nothing
        evaluateSym False model (InR Nothing :: Sum Maybe Maybe SBool) `shouldBe` InR Nothing
        evaluateSym True model (InR Nothing :: Sum Maybe Maybe SBool) `shouldBe` InR Nothing

        evaluateSym False model (InL (Just $ SSBool "a") :: Sum Maybe Maybe SBool) `shouldBe` InL (Just $ CBool True)
        evaluateSym True model (InL (Just $ SSBool "a") :: Sum Maybe Maybe SBool) `shouldBe` InL (Just $ CBool True)
        evaluateSym False model (InL (Just $ SSBool "b") :: Sum Maybe Maybe SBool) `shouldBe` InL (Just $ SSBool "b")
        evaluateSym True model (InL (Just $ SSBool "b") :: Sum Maybe Maybe SBool) `shouldBe` InL (Just $ CBool False)

        evaluateSym False model (InR (Just $ SSBool "a") :: Sum Maybe Maybe SBool) `shouldBe` InR (Just $ CBool True)
        evaluateSym True model (InR (Just $ SSBool "a") :: Sum Maybe Maybe SBool) `shouldBe` InR (Just $ CBool True)
        evaluateSym False model (InR (Just $ SSBool "b") :: Sum Maybe Maybe SBool) `shouldBe` InR (Just $ SSBool "b")
        evaluateSym True model (InR (Just $ SSBool "b") :: Sum Maybe Maybe SBool) `shouldBe` InR (Just $ CBool False)
    describe "Evaluate for WriterT" $ do
      prop
        "Evaluate for concrete Lazy WriterT should work"
        (\(x :: Either Integer (Integer, Integer)) -> concreteEvaluateSymOkProp (WriterLazy.WriterT x))
      it "Evaluate for general Lazy WriteT should work" $ do
        let model = M.fromList [(SSymbol "a", True)] :: M.HashMap Symbol Bool
        evaluateSym False model (WriterLazy.WriterT $ Left $ SSBool "a" :: WriterLazy.WriterT SBool (Either SBool) SBool)
          `shouldBe` WriterLazy.WriterT (Left $ CBool True)
        evaluateSym True model (WriterLazy.WriterT $ Left $ SSBool "a" :: WriterLazy.WriterT SBool (Either SBool) SBool)
          `shouldBe` WriterLazy.WriterT (Left $ CBool True)
        evaluateSym False model (WriterLazy.WriterT $ Left $ SSBool "b" :: WriterLazy.WriterT SBool (Either SBool) SBool)
          `shouldBe` WriterLazy.WriterT (Left $ SSBool "b")
        evaluateSym True model (WriterLazy.WriterT $ Left $ SSBool "b" :: WriterLazy.WriterT SBool (Either SBool) SBool)
          `shouldBe` WriterLazy.WriterT (Left $ CBool False)

        evaluateSym False model (WriterLazy.WriterT $ Right (SSBool "a", SSBool "b") :: WriterLazy.WriterT SBool (Either SBool) SBool)
          `shouldBe` WriterLazy.WriterT (Right (CBool True, SSBool "b"))
        evaluateSym True model (WriterLazy.WriterT $ Right (SSBool "a", SSBool "b") :: WriterLazy.WriterT SBool (Either SBool) SBool)
          `shouldBe` WriterLazy.WriterT (Right (CBool True, CBool False))
      prop
        "Evaluate for concrete Strict WriterT should work"
        (\(x :: Either Integer (Integer, Integer)) -> concreteEvaluateSymOkProp (WriterStrict.WriterT x))
      it "Evaluate for general Strict WriteT should work" $ do
        let model = M.fromList [(SSymbol "a", True)] :: M.HashMap Symbol Bool
        evaluateSym False model (WriterStrict.WriterT $ Left $ SSBool "a" :: WriterStrict.WriterT SBool (Either SBool) SBool)
          `shouldBe` WriterStrict.WriterT (Left $ CBool True)
        evaluateSym True model (WriterStrict.WriterT $ Left $ SSBool "a" :: WriterStrict.WriterT SBool (Either SBool) SBool)
          `shouldBe` WriterStrict.WriterT (Left $ CBool True)
        evaluateSym False model (WriterStrict.WriterT $ Left $ SSBool "b" :: WriterStrict.WriterT SBool (Either SBool) SBool)
          `shouldBe` WriterStrict.WriterT (Left $ SSBool "b")
        evaluateSym True model (WriterStrict.WriterT $ Left $ SSBool "b" :: WriterStrict.WriterT SBool (Either SBool) SBool)
          `shouldBe` WriterStrict.WriterT (Left $ CBool False)

        evaluateSym False model (WriterStrict.WriterT $ Right (SSBool "a", SSBool "b") :: WriterStrict.WriterT SBool (Either SBool) SBool)
          `shouldBe` WriterStrict.WriterT (Right (CBool True, SSBool "b"))
        evaluateSym True model (WriterStrict.WriterT $ Right (SSBool "a", SSBool "b") :: WriterStrict.WriterT SBool (Either SBool) SBool)
          `shouldBe` WriterStrict.WriterT (Right (CBool True, CBool False))
    describe "Evaluate for Identity" $ do
      prop
        "Evaluate for concrete Identity should work"
        (\(x :: Integer) -> concreteEvaluateSymOkProp $ Identity x)
      it "Evaluate for general Identity should work" $ do
        let model = M.fromList [(SSymbol "a", True)] :: M.HashMap Symbol Bool
        evaluateSym False model (Identity $ SSBool "a") `shouldBe` Identity (CBool True)
        evaluateSym True model (Identity $ SSBool "a") `shouldBe` Identity (CBool True)
        evaluateSym False model (Identity $ SSBool "b") `shouldBe` Identity (SSBool "b")
        evaluateSym True model (Identity $ SSBool "b") `shouldBe` Identity (CBool False)
    describe "Evaluate for IdentityT" $ do
      prop
        "Evaluate for concrete IdentityT should work"
        (\(x :: Either Integer Integer) -> concreteEvaluateSymOkProp $ IdentityT x)
      it "Evaluate for general IdentityT should work" $ do
        let model = M.fromList [(SSymbol "a", True)] :: M.HashMap Symbol Bool
        evaluateSym False model (IdentityT $ Left $ SSBool "a" :: IdentityT (Either SBool) SBool)
          `shouldBe` IdentityT (Left $ CBool True)
        evaluateSym True model (IdentityT $ Left $ SSBool "a" :: IdentityT (Either SBool) SBool)
          `shouldBe` IdentityT (Left $ CBool True)
        evaluateSym False model (IdentityT $ Left $ SSBool "b" :: IdentityT (Either SBool) SBool)
          `shouldBe` IdentityT (Left $ SSBool "b")
        evaluateSym True model (IdentityT $ Left $ SSBool "b" :: IdentityT (Either SBool) SBool)
          `shouldBe` IdentityT (Left $ CBool False)
        evaluateSym False model (IdentityT $ Right $ SSBool "a" :: IdentityT (Either SBool) SBool)
          `shouldBe` IdentityT (Right $ CBool True)
        evaluateSym True model (IdentityT $ Right $ SSBool "a" :: IdentityT (Either SBool) SBool)
          `shouldBe` IdentityT (Right $ CBool True)
        evaluateSym False model (IdentityT $ Right $ SSBool "b" :: IdentityT (Either SBool) SBool)
          `shouldBe` IdentityT (Right $ SSBool "b")
        evaluateSym True model (IdentityT $ Right $ SSBool "b" :: IdentityT (Either SBool) SBool)
          `shouldBe` IdentityT (Right $ CBool False)
