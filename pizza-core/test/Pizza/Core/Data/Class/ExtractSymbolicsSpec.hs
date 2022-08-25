{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Pizza.Core.Data.Class.ExtractSymbolicsSpec where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.ByteString as B
import qualified Data.HashSet as S
import Data.Int
import Data.Word
import GHC.Generics
import Generics.Deriving
import Pizza.Core.Data.Class.ExtractSymbolics
import Pizza.TestUtils.ExtractSymbolics
import Pizza.TestUtils.SBool
import Test.Hspec
import Test.Hspec.QuickCheck

data A = A1 | A2 SBool | A3 SBool SBool
  deriving (Generic, Show, Eq)
  deriving (ExtractSymbolics (S.HashSet Symbol)) via (Default A)

spec :: Spec
spec = do
  describe "ExtractSymbolics for common types" $ do
    it "ExtractSymbolics for SBool" $ do
      extractSymbolics (CBool True) `shouldBe` (S.empty :: S.HashSet Symbol)
      extractSymbolics (SSBool "a") `shouldBe` S.singleton (SSymbol "a")
      extractSymbolics (ISBool "a" 1) `shouldBe` S.singleton (ISymbol "a" 1)
      extractSymbolics (And (SSBool "a") (ISBool "b" 1))
        `shouldBe` S.fromList [SSymbol "a", ISymbol "b" 1]
      extractSymbolics (Or (SSBool "a") (ISBool "b" 1))
        `shouldBe` S.fromList [SSymbol "a", ISymbol "b" 1]
      extractSymbolics (Equal (SSBool "a") (ISBool "b" 1))
        `shouldBe` S.fromList [SSymbol "a", ISymbol "b" 1]
      extractSymbolics (ITE (SSBool "a") (ISBool "b" 1) (SSBool "c"))
        `shouldBe` S.fromList [SSymbol "a", ISymbol "b" 1, SSymbol "c"]
      extractSymbolics (Not $ ISBool "a" 1) `shouldBe` S.singleton (ISymbol "a" 1)
    prop "ExtractSymbolics for Bool" (concreteExtractSymbolicsOkProp @Bool)
    prop "ExtractSymbolics for Integer" (concreteExtractSymbolicsOkProp @Integer)
    prop "ExtractSymbolics for Char" (concreteExtractSymbolicsOkProp @Char)
    prop "ExtractSymbolics for Int" (concreteExtractSymbolicsOkProp @Int)
    prop "ExtractSymbolics for Int8" (concreteExtractSymbolicsOkProp @Int8)
    prop "ExtractSymbolics for Int16" (concreteExtractSymbolicsOkProp @Int16)
    prop "ExtractSymbolics for Int32" (concreteExtractSymbolicsOkProp @Int32)
    prop "ExtractSymbolics for Int64" (concreteExtractSymbolicsOkProp @Int64)
    prop "ExtractSymbolics for Word" (concreteExtractSymbolicsOkProp @Word)
    prop "ExtractSymbolics for Word8" (concreteExtractSymbolicsOkProp @Word8)
    prop "ExtractSymbolics for Word16" (concreteExtractSymbolicsOkProp @Word16)
    prop "ExtractSymbolics for Word32" (concreteExtractSymbolicsOkProp @Word32)
    prop "ExtractSymbolics for Word64" (concreteExtractSymbolicsOkProp @Word64)
    it "ExtractSymbolics for List" $ do
      extractSymbolics ([] :: [SBool]) `shouldBe` (S.empty :: S.HashSet Symbol)
      extractSymbolics [SSBool "a"] `shouldBe` S.singleton (SSymbol "a")
      extractSymbolics [SSBool "a", SSBool "b"] `shouldBe` S.fromList [SSymbol "a", SSymbol "b"]
    it "ExtractSymbolics for Maybe" $ do
      extractSymbolics (Nothing :: Maybe SBool) `shouldBe` (S.empty :: S.HashSet Symbol)
      extractSymbolics (Just (SSBool "a")) `shouldBe` S.singleton (SSymbol "a")
    it "ExtractSymbolics for Either" $ do
      extractSymbolics (Left (SSBool "a") :: Either SBool SBool) `shouldBe` S.singleton (SSymbol "a")
      extractSymbolics (Right (SSBool "a") :: Either SBool SBool) `shouldBe` S.singleton (SSymbol "a")
    it "ExtractSymbolics for MaybeT" $ do
      extractSymbolics (MaybeT Nothing :: MaybeT Maybe SBool) `shouldBe` (S.empty :: S.HashSet Symbol)
      extractSymbolics (MaybeT (Just Nothing) :: MaybeT Maybe SBool) `shouldBe` (S.empty :: S.HashSet Symbol)
      extractSymbolics (MaybeT (Just (Just (SSBool "a")))) `shouldBe` S.singleton (SSymbol "a")
    it "ExtractSymbolics for ExceptT" $ do
      extractSymbolics (ExceptT Nothing :: ExceptT SBool Maybe SBool) `shouldBe` (S.empty :: S.HashSet Symbol)
      extractSymbolics (ExceptT (Just (Left (SSBool "a"))) :: ExceptT SBool Maybe SBool)
        `shouldBe` S.singleton (SSymbol "a")
      extractSymbolics (ExceptT (Just (Right (SSBool "a"))) :: ExceptT SBool Maybe SBool)
        `shouldBe` S.singleton (SSymbol "a")
    it "ExtractSymbolics for Lazy WriterT" $ do
      extractSymbolics (WriterLazy.WriterT Nothing :: WriterLazy.WriterT SBool Maybe SBool) `shouldBe` (S.empty :: S.HashSet Symbol)
      extractSymbolics (WriterLazy.WriterT (Just (SSBool "a", SSBool "b")) :: WriterLazy.WriterT SBool Maybe SBool)
        `shouldBe` S.fromList [SSymbol "a", SSymbol "b"]
    it "ExtractSymbolics for Strict WriterT" $ do
      extractSymbolics (WriterStrict.WriterT Nothing :: WriterStrict.WriterT SBool Maybe SBool) `shouldBe` (S.empty :: S.HashSet Symbol)
      extractSymbolics (WriterStrict.WriterT (Just (SSBool "a", SSBool "b")) :: WriterStrict.WriterT SBool Maybe SBool)
        `shouldBe` S.fromList [SSymbol "a", SSymbol "b"]
    prop "ExtractSymbolics for ()" (concreteExtractSymbolicsOkProp @())
    it "ExtractSymbolics for (,)" $ do
      extractSymbolics (SSBool "a", SSBool "b") `shouldBe` S.fromList [SSymbol "a", SSymbol "b"]
    it "ExtractSymbolics for ByteString" $ do
      extractSymbolics ("" :: B.ByteString) `shouldBe` (S.empty :: S.HashSet Symbol)
      extractSymbolics ("a" :: B.ByteString) `shouldBe` (S.empty :: S.HashSet Symbol)
    it "ExtractSymbolic for Identity" $ do
      extractSymbolics (Identity $ SSBool "a") `shouldBe` S.singleton (SSymbol "a")
    it "ExtractSymbolic for IdentityT" $ do
      extractSymbolics (IdentityT $ Left $ SSBool "a" :: IdentityT (Either SBool) SBool) `shouldBe` S.singleton (SSymbol "a")
      extractSymbolics (IdentityT $ Right $ SSBool "a" :: IdentityT (Either SBool) SBool) `shouldBe` S.singleton (SSymbol "a")
  describe "deriving ExtractSymbolics for ADT" $ do
    it "derived ExtractSymbolics for simple ADT" $ do
      extractSymbolics A1 `shouldBe` (S.empty :: S.HashSet Symbol)
      extractSymbolics (A2 (SSBool "a")) `shouldBe` S.singleton (SSymbol "a")
      extractSymbolics (A3 (SSBool "a") (SSBool "b")) `shouldBe` S.fromList [SSymbol "a", SSymbol "b"]
