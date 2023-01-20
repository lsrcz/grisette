{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Core.Data.Class.ExtractSymbolicsTests where

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
import Grisette.Core.Data.Class.ExtractSymbolics
import Grisette.TestUtils.ExtractSymbolics
import Grisette.TestUtils.SBool
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

data A = A1 | A2 SBool | A3 SBool SBool
  deriving (Generic, Show, Eq)
  deriving (GExtractSymbolics (S.HashSet Symbol)) via (Default A)

gextractSymbolicsTests :: TestTree
gextractSymbolicsTests =
  testGroup
    "GExtractSymbolicsTests"
    [ testGroup
        "Common types"
        [ testGroup
            "SBool"
            [ testCase "CBool" $
                gextractSymbolics (CBool True) @=? (S.empty :: S.HashSet Symbol),
              testCase "SSBool" $
                gextractSymbolics (SSBool "a") @=? S.singleton (SSymbol "a"),
              testCase "ISBool" $
                gextractSymbolics (ISBool "a" 1) @=? S.singleton (ISymbol "a" 1),
              testCase "And" $
                gextractSymbolics (And (SSBool "a") (ISBool "b" 1))
                  @=? S.fromList [SSymbol "a", ISymbol "b" 1],
              testCase "Or" $
                gextractSymbolics (Or (SSBool "a") (ISBool "b" 1))
                  @=? S.fromList [SSymbol "a", ISymbol "b" 1],
              testCase "Equal" $
                gextractSymbolics (Equal (SSBool "a") (ISBool "b" 1))
                  @=? S.fromList [SSymbol "a", ISymbol "b" 1],
              testCase "ITE" $
                gextractSymbolics (ITE (SSBool "a") (ISBool "b" 1) (SSBool "c"))
                  @=? S.fromList [SSymbol "a", ISymbol "b" 1, SSymbol "c"],
              testCase "Not" $
                gextractSymbolics (Not $ ISBool "a" 1) @=? S.singleton (ISymbol "a" 1)
            ],
          testProperty "Bool" (ioProperty . concreteGExtractSymbolicsOkProp @Bool),
          testProperty "Integer" (ioProperty . concreteGExtractSymbolicsOkProp @Integer),
          testProperty "Char" (ioProperty . concreteGExtractSymbolicsOkProp @Char),
          testProperty "Int" (ioProperty . concreteGExtractSymbolicsOkProp @Int),
          testProperty "Int8" (ioProperty . concreteGExtractSymbolicsOkProp @Int8),
          testProperty "Int16" (ioProperty . concreteGExtractSymbolicsOkProp @Int16),
          testProperty "Int32" (ioProperty . concreteGExtractSymbolicsOkProp @Int32),
          testProperty "Int64" (ioProperty . concreteGExtractSymbolicsOkProp @Int64),
          testProperty "Word" (ioProperty . concreteGExtractSymbolicsOkProp @Word),
          testProperty "Word8" (ioProperty . concreteGExtractSymbolicsOkProp @Word8),
          testProperty "Word16" (ioProperty . concreteGExtractSymbolicsOkProp @Word16),
          testProperty "Word32" (ioProperty . concreteGExtractSymbolicsOkProp @Word32),
          testProperty "Word64" (ioProperty . concreteGExtractSymbolicsOkProp @Word64),
          testGroup
            "[SBool]"
            [ testCase "[]" $
                gextractSymbolics ([] :: [SBool]) @=? (S.empty :: S.HashSet Symbol),
              testCase "[v]" $
                gextractSymbolics [SSBool "a"] @=? S.singleton (SSymbol "a"),
              testCase "[v1, v2]" $
                gextractSymbolics [SSBool "a", SSBool "b"] @=? S.fromList [SSymbol "a", SSymbol "b"]
            ],
          testGroup
            "Maybe SBool"
            [ testCase "Nothing" $
                gextractSymbolics (Nothing :: Maybe SBool) @=? (S.empty :: S.HashSet Symbol),
              testCase "Just v" $
                gextractSymbolics (Just (SSBool "a")) @=? S.singleton (SSymbol "a")
            ],
          testGroup
            "Either SBool SBool"
            [ testCase "Left v" $
                gextractSymbolics (Left (SSBool "a") :: Either SBool SBool) @=? S.singleton (SSymbol "a"),
              testCase "Right v" $
                gextractSymbolics (Right (SSBool "a") :: Either SBool SBool) @=? S.singleton (SSymbol "a")
            ],
          testGroup
            "MaybeT Maybe SBool"
            [ testCase "MaybeT Nothing" $
                gextractSymbolics (MaybeT Nothing :: MaybeT Maybe SBool) @=? (S.empty :: S.HashSet Symbol),
              testCase "MaybeT (Just Nothing)" $
                gextractSymbolics (MaybeT (Just Nothing) :: MaybeT Maybe SBool) @=? (S.empty :: S.HashSet Symbol),
              testCase "MaybeT (Just (Just v))" $
                gextractSymbolics (MaybeT (Just (Just (SSBool "a")))) @=? S.singleton (SSymbol "a")
            ],
          testGroup
            "ExceptT SBool Maybe SBool"
            [ testCase "ExceptT Nothing" $
                gextractSymbolics (ExceptT Nothing :: ExceptT SBool Maybe SBool) @=? (S.empty :: S.HashSet Symbol),
              testCase "ExceptT (Just (Left v))" $
                gextractSymbolics (ExceptT (Just (Left (SSBool "a"))) :: ExceptT SBool Maybe SBool)
                  @=? S.singleton (SSymbol "a"),
              testCase "ExceptT (Just (Right v))" $
                gextractSymbolics (ExceptT (Just (Right (SSBool "a"))) :: ExceptT SBool Maybe SBool)
                  @=? S.singleton (SSymbol "a")
            ],
          testGroup
            "WriterT SBool Maybe SBool"
            [ testGroup
                "Lazy"
                [ testCase "WriterT Nothing" $
                    gextractSymbolics (WriterLazy.WriterT Nothing :: WriterLazy.WriterT SBool Maybe SBool) @=? (S.empty :: S.HashSet Symbol),
                  testCase "WriterT (Just (v1, v2))" $
                    gextractSymbolics (WriterLazy.WriterT (Just (SSBool "a", SSBool "b")) :: WriterLazy.WriterT SBool Maybe SBool)
                      @=? S.fromList [SSymbol "a", SSymbol "b"]
                ],
              testGroup
                "Strict"
                [ testCase "WriterT Nothing" $
                    gextractSymbolics (WriterStrict.WriterT Nothing :: WriterStrict.WriterT SBool Maybe SBool) @=? (S.empty :: S.HashSet Symbol),
                  testCase "WriterT (Just (v1, v2))" $
                    gextractSymbolics (WriterStrict.WriterT (Just (SSBool "a", SSBool "b")) :: WriterStrict.WriterT SBool Maybe SBool)
                      @=? S.fromList [SSymbol "a", SSymbol "b"]
                ]
            ],
          testProperty "()" (ioProperty . concreteGExtractSymbolicsOkProp @()),
          testCase "(,)" $
            gextractSymbolics (SSBool "a", SSBool "b") @=? S.fromList [SSymbol "a", SSymbol "b"],
          testCase "(,,)" $
            gextractSymbolics (SSBool "a", SSBool "b", SSBool "c")
              @=? S.fromList [SSymbol "a", SSymbol "b", SSymbol "c"],
          testGroup
            "ByteString"
            [ testCase "\"\"" $
                gextractSymbolics ("" :: B.ByteString) @=? (S.empty :: S.HashSet Symbol),
              testCase "\"a\"" $
                gextractSymbolics ("a" :: B.ByteString) @=? (S.empty :: S.HashSet Symbol)
            ],
          testCase "Identity SBool" $
            gextractSymbolics (Identity $ SSBool "a") @=? S.singleton (SSymbol "a"),
          testGroup
            "IdentityT (Either SBool) SBool"
            [ testCase "Identity (Left v)" $
                gextractSymbolics (IdentityT $ Left $ SSBool "a" :: IdentityT (Either SBool) SBool) @=? S.singleton (SSymbol "a"),
              testCase "Identity (Right v)" $
                gextractSymbolics (IdentityT $ Right $ SSBool "a" :: IdentityT (Either SBool) SBool) @=? S.singleton (SSymbol "a")
            ]
        ],
      testGroup
        "deriving GExtractSymbolics for ADT"
        [ testGroup
            "Simple ADT"
            [ testCase "A1" $
                gextractSymbolics A1 @=? (S.empty :: S.HashSet Symbol),
              testCase "A2" $
                gextractSymbolics (A2 (SSBool "a")) @=? S.singleton (SSymbol "a"),
              testCase "A3" $
                gextractSymbolics (A3 (SSBool "a") (SSBool "b")) @=? S.fromList [SSymbol "a", SSymbol "b"]
            ]
        ]
    ]
