{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Pizza.Core.Data.Class.ExtractSymbolicsTests where

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
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

data A = A1 | A2 SBool | A3 SBool SBool
  deriving (Generic, Show, Eq)
  deriving (ExtractSymbolics (S.HashSet Symbol)) via (Default A)

extractSymbolicsTests :: TestTree
extractSymbolicsTests =
  testGroup
    "ExtractSymbolicsTests"
    [ testGroup
        "Common types"
        [ testGroup
            "SBool"
            [ testCase "CBool" $
                extractSymbolics (CBool True) @=? (S.empty :: S.HashSet Symbol),
              testCase "SSBool" $
                extractSymbolics (SSBool "a") @=? S.singleton (SSymbol "a"),
              testCase "ISBool" $
                extractSymbolics (ISBool "a" 1) @=? S.singleton (ISymbol "a" 1),
              testCase "And" $
                extractSymbolics (And (SSBool "a") (ISBool "b" 1))
                  @=? S.fromList [SSymbol "a", ISymbol "b" 1],
              testCase "Or" $
                extractSymbolics (Or (SSBool "a") (ISBool "b" 1))
                  @=? S.fromList [SSymbol "a", ISymbol "b" 1],
              testCase "Equal" $
                extractSymbolics (Equal (SSBool "a") (ISBool "b" 1))
                  @=? S.fromList [SSymbol "a", ISymbol "b" 1],
              testCase "ITE" $
                extractSymbolics (ITE (SSBool "a") (ISBool "b" 1) (SSBool "c"))
                  @=? S.fromList [SSymbol "a", ISymbol "b" 1, SSymbol "c"],
              testCase "Not" $
                extractSymbolics (Not $ ISBool "a" 1) @=? S.singleton (ISymbol "a" 1)
            ],
          testProperty "Bool" (ioProperty . concreteExtractSymbolicsOkProp @Bool),
          testProperty "Integer" (ioProperty . concreteExtractSymbolicsOkProp @Integer),
          testProperty "Char" (ioProperty . concreteExtractSymbolicsOkProp @Char),
          testProperty "Int" (ioProperty . concreteExtractSymbolicsOkProp @Int),
          testProperty "Int8" (ioProperty . concreteExtractSymbolicsOkProp @Int8),
          testProperty "Int16" (ioProperty . concreteExtractSymbolicsOkProp @Int16),
          testProperty "Int32" (ioProperty . concreteExtractSymbolicsOkProp @Int32),
          testProperty "Int64" (ioProperty . concreteExtractSymbolicsOkProp @Int64),
          testProperty "Word" (ioProperty . concreteExtractSymbolicsOkProp @Word),
          testProperty "Word8" (ioProperty . concreteExtractSymbolicsOkProp @Word8),
          testProperty "Word16" (ioProperty . concreteExtractSymbolicsOkProp @Word16),
          testProperty "Word32" (ioProperty . concreteExtractSymbolicsOkProp @Word32),
          testProperty "Word64" (ioProperty . concreteExtractSymbolicsOkProp @Word64),
          testGroup
            "[SBool]"
            [ testCase "[]" $
                extractSymbolics ([] :: [SBool]) @=? (S.empty :: S.HashSet Symbol),
              testCase "[v]" $
                extractSymbolics [SSBool "a"] @=? S.singleton (SSymbol "a"),
              testCase "[v1, v2]" $
                extractSymbolics [SSBool "a", SSBool "b"] @=? S.fromList [SSymbol "a", SSymbol "b"]
            ],
          testGroup
            "Maybe SBool"
            [ testCase "Nothing" $
                extractSymbolics (Nothing :: Maybe SBool) @=? (S.empty :: S.HashSet Symbol),
              testCase "Just v" $
                extractSymbolics (Just (SSBool "a")) @=? S.singleton (SSymbol "a")
            ],
          testGroup
            "Either SBool SBool"
            [ testCase "Left v" $
                extractSymbolics (Left (SSBool "a") :: Either SBool SBool) @=? S.singleton (SSymbol "a"),
              testCase "Right v" $
                extractSymbolics (Right (SSBool "a") :: Either SBool SBool) @=? S.singleton (SSymbol "a")
            ],
          testGroup
            "MaybeT Maybe SBool"
            [ testCase "MaybeT Nothing" $
                extractSymbolics (MaybeT Nothing :: MaybeT Maybe SBool) @=? (S.empty :: S.HashSet Symbol),
              testCase "MaybeT (Just Nothing)" $
                extractSymbolics (MaybeT (Just Nothing) :: MaybeT Maybe SBool) @=? (S.empty :: S.HashSet Symbol),
              testCase "MaybeT (Just (Just v))" $
                extractSymbolics (MaybeT (Just (Just (SSBool "a")))) @=? S.singleton (SSymbol "a")
            ],
          testGroup
            "ExceptT SBool Maybe SBool"
            [ testCase "ExceptT Nothing" $
                extractSymbolics (ExceptT Nothing :: ExceptT SBool Maybe SBool) @=? (S.empty :: S.HashSet Symbol),
              testCase "ExceptT (Just (Left v))" $
                extractSymbolics (ExceptT (Just (Left (SSBool "a"))) :: ExceptT SBool Maybe SBool)
                  @=? S.singleton (SSymbol "a"),
              testCase "ExceptT (Just (Right v))" $
                extractSymbolics (ExceptT (Just (Right (SSBool "a"))) :: ExceptT SBool Maybe SBool)
                  @=? S.singleton (SSymbol "a")
            ],
          testGroup
            "WriterT SBool Maybe SBool"
            [ testGroup
                "Lazy"
                [ testCase "WriterT Nothing" $
                    extractSymbolics (WriterLazy.WriterT Nothing :: WriterLazy.WriterT SBool Maybe SBool) @=? (S.empty :: S.HashSet Symbol),
                  testCase "WriterT (Just (v1, v2))" $
                    extractSymbolics (WriterLazy.WriterT (Just (SSBool "a", SSBool "b")) :: WriterLazy.WriterT SBool Maybe SBool)
                      @=? S.fromList [SSymbol "a", SSymbol "b"]
                ],
              testGroup
                "Strict"
                [ testCase "WriterT Nothing" $
                    extractSymbolics (WriterStrict.WriterT Nothing :: WriterStrict.WriterT SBool Maybe SBool) @=? (S.empty :: S.HashSet Symbol),
                  testCase "WriterT (Just (v1, v2))" $
                    extractSymbolics (WriterStrict.WriterT (Just (SSBool "a", SSBool "b")) :: WriterStrict.WriterT SBool Maybe SBool)
                      @=? S.fromList [SSymbol "a", SSymbol "b"]
                ]
            ],
          testProperty "()" (ioProperty . concreteExtractSymbolicsOkProp @()),
          testCase "(,)" $
            extractSymbolics (SSBool "a", SSBool "b") @=? S.fromList [SSymbol "a", SSymbol "b"],
          testCase "(,,)" $
            extractSymbolics (SSBool "a", SSBool "b", SSBool "c")
              @=? S.fromList [SSymbol "a", SSymbol "b", SSymbol "c"],
          testGroup
            "ByteString"
            [ testCase "\"\"" $
                extractSymbolics ("" :: B.ByteString) @=? (S.empty :: S.HashSet Symbol),
              testCase "\"a\"" $
                extractSymbolics ("a" :: B.ByteString) @=? (S.empty :: S.HashSet Symbol)
            ],
          testCase "Identity SBool" $
            extractSymbolics (Identity $ SSBool "a") @=? S.singleton (SSymbol "a"),
          testGroup
            "IdentityT (Either SBool) SBool"
            [ testCase "Identity (Left v)" $
                extractSymbolics (IdentityT $ Left $ SSBool "a" :: IdentityT (Either SBool) SBool) @=? S.singleton (SSymbol "a"),
              testCase "Identity (Right v)" $
                extractSymbolics (IdentityT $ Right $ SSBool "a" :: IdentityT (Either SBool) SBool) @=? S.singleton (SSymbol "a")
            ]
        ],
      testGroup
        "deriving ExtractSymbolics for ADT"
        [ testGroup
            "Simple ADT"
            [ testCase "A1" $
                extractSymbolics A1 @=? (S.empty :: S.HashSet Symbol),
              testCase "A2" $
                extractSymbolics (A2 (SSBool "a")) @=? S.singleton (SSymbol "a"),
              testCase "A3" $
                extractSymbolics (A3 (SSBool "a") (SSBool "b")) @=? S.fromList [SSymbol "a", SSymbol "b"]
            ]
        ]
    ]
