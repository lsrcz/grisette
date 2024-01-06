{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Core.Data.Class.ExtractSymbolicsTests
  ( extractSymbolicsTests,
  )
where

import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Identity
  ( Identity (Identity),
    IdentityT (IdentityT),
  )
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.ByteString as B
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Generics.Deriving (Default (Default))
import Grisette.Core.Data.Class.ExtractSymbolics
  ( ExtractSymbolics (extractSymbolics),
  )
import Grisette.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Core.Data.Class.LogicalOp (LogicalOp (symNot, (.&&), (.||)))
import Grisette.Core.Data.Class.ModelOps
  ( SymbolSetOps (emptySet),
    SymbolSetRep (buildSymbolSet),
  )
import Grisette.Core.Data.Class.SEq (SEq ((.==)))
import Grisette.Core.Data.Class.TestValues
  ( isymBool,
    isymbolBool,
    ssymBool,
    ssymbolBool,
    symTrue,
  )
import Grisette.IR.SymPrim.Data.SymPrim (SymBool)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (Assertion, (@?=))
import Test.QuickCheck (ioProperty)

data A = A1 | A2 SymBool | A3 SymBool SymBool
  deriving (Generic, Show, Eq)
  deriving (ExtractSymbolics) via (Default A)

concreteExtractSymbolicsOkProp ::
  (HasCallStack, ExtractSymbolics a) => (a, a) -> Assertion
concreteExtractSymbolicsOkProp x = extractSymbolics x @?= emptySet

extractSymbolicsTests :: Test
extractSymbolicsTests =
  testGroup
    "ExtractSymbolics"
    [ testGroup
        "Common types"
        [ testGroup
            "SymBool"
            [ testCase "con" $
                extractSymbolics symTrue @?= emptySet,
              testCase "ssym" $
                extractSymbolics (ssymBool "a")
                  @?= buildSymbolSet (ssymbolBool "a"),
              testCase "isym" $
                extractSymbolics (isymBool "a" 1)
                  @?= buildSymbolSet (isymbolBool "a" 1),
              testCase "And" $
                extractSymbolics (ssymBool "a" .&& isymBool "b" 1)
                  @?= buildSymbolSet (ssymbolBool "a", isymbolBool "b" 1),
              testCase "Or" $
                extractSymbolics (ssymBool "a" .|| isymBool "b" 1)
                  @?= buildSymbolSet (ssymbolBool "a", isymbolBool "b" 1),
              testCase "Equal" $
                extractSymbolics (ssymBool "a" .== isymBool "b" 1)
                  @?= buildSymbolSet (ssymbolBool "a", isymbolBool "b" 1),
              testCase "ITE" $
                extractSymbolics
                  (symIte (ssymBool "a") (isymBool "b" 1) (ssymBool "c"))
                  @?= buildSymbolSet
                    ( ssymbolBool "a",
                      isymbolBool "b" 1,
                      ssymbolBool "c"
                    ),
              testCase "Not" $
                extractSymbolics (symNot $ isymBool "a" 1)
                  @?= buildSymbolSet (isymbolBool "a" 1)
            ],
          testProperty "Bool" $
            ioProperty . concreteExtractSymbolicsOkProp @Bool,
          testProperty "Integer" $
            ioProperty . concreteExtractSymbolicsOkProp @Integer,
          testProperty "Char" $
            ioProperty . concreteExtractSymbolicsOkProp @Char,
          testProperty "Int" $ ioProperty . concreteExtractSymbolicsOkProp @Int,
          testProperty "Int8" $
            ioProperty . concreteExtractSymbolicsOkProp @Int8,
          testProperty "Int16" $
            ioProperty . concreteExtractSymbolicsOkProp @Int16,
          testProperty "Int32" $
            ioProperty . concreteExtractSymbolicsOkProp @Int32,
          testProperty "Int64" $
            ioProperty . concreteExtractSymbolicsOkProp @Int64,
          testProperty "Word" $
            ioProperty . concreteExtractSymbolicsOkProp @Word,
          testProperty "Word8" $
            ioProperty . concreteExtractSymbolicsOkProp @Word8,
          testProperty "Word16" $
            ioProperty . concreteExtractSymbolicsOkProp @Word16,
          testProperty "Word32" $
            ioProperty . concreteExtractSymbolicsOkProp @Word32,
          testProperty "Word64" $
            ioProperty . concreteExtractSymbolicsOkProp @Word64,
          testGroup
            "[SymBool]"
            [ testCase "[]" $
                extractSymbolics ([] :: [SymBool]) @?= emptySet,
              testCase "[v]" $
                extractSymbolics [ssymBool "a"]
                  @?= buildSymbolSet (ssymbolBool "a"),
              testCase "[v1, v2]" $
                extractSymbolics [ssymBool "a", ssymBool "b"]
                  @?= buildSymbolSet (ssymbolBool "a", ssymbolBool "b")
            ],
          testGroup
            "Maybe SymBool"
            [ testCase "Nothing" $
                extractSymbolics (Nothing :: Maybe SymBool) @?= emptySet,
              testCase "Just v" $
                extractSymbolics (Just (ssymBool "a"))
                  @?= buildSymbolSet (ssymbolBool "a")
            ],
          testGroup
            "Either SymBool SymBool"
            [ testCase "Left v" $
                extractSymbolics
                  (Left (ssymBool "a") :: Either SymBool SymBool)
                  @?= buildSymbolSet (ssymbolBool "a"),
              testCase "Right v" $
                extractSymbolics
                  (Right (ssymBool "a") :: Either SymBool SymBool)
                  @?= buildSymbolSet (ssymbolBool "a")
            ],
          testGroup
            "MaybeT Maybe SymBool"
            [ testCase "MaybeT Nothing" $
                extractSymbolics (MaybeT Nothing :: MaybeT Maybe SymBool)
                  @?= emptySet,
              testCase "MaybeT (Just Nothing)" $
                extractSymbolics (MaybeT (Just Nothing) :: MaybeT Maybe SymBool)
                  @?= emptySet,
              testCase "MaybeT (Just (Just v))" $
                extractSymbolics (MaybeT (Just (Just (ssymBool "a"))))
                  @?= buildSymbolSet (ssymbolBool "a")
            ],
          testGroup
            "ExceptT SymBool Maybe SymBool"
            [ testCase "ExceptT Nothing" $
                extractSymbolics
                  (ExceptT Nothing :: ExceptT SymBool Maybe SymBool)
                  @?= emptySet,
              testCase "ExceptT (Just (Left v))" $
                extractSymbolics
                  ( ExceptT (Just (Left (ssymBool "a"))) ::
                      ExceptT SymBool Maybe SymBool
                  )
                  @?= buildSymbolSet (ssymbolBool "a"),
              testCase "ExceptT (Just (Right v))" $
                extractSymbolics
                  ( ExceptT (Just (Right (ssymBool "a"))) ::
                      ExceptT SymBool Maybe SymBool
                  )
                  @?= buildSymbolSet (ssymbolBool "a")
            ],
          testGroup
            "WriterT SymBool Maybe SymBool"
            [ testGroup
                "Lazy"
                [ testCase "WriterT Nothing" $
                    extractSymbolics
                      ( WriterLazy.WriterT Nothing ::
                          WriterLazy.WriterT SymBool Maybe SymBool
                      )
                      @?= emptySet,
                  testCase "WriterT (Just (v1, v2))" $
                    extractSymbolics
                      ( WriterLazy.WriterT
                          (Just (ssymBool "a", ssymBool "b")) ::
                          WriterLazy.WriterT SymBool Maybe SymBool
                      )
                      @?= buildSymbolSet (ssymbolBool "a", ssymbolBool "b")
                ],
              testGroup
                "Strict"
                [ testCase "WriterT Nothing" $
                    extractSymbolics
                      ( WriterStrict.WriterT Nothing ::
                          WriterStrict.WriterT SymBool Maybe SymBool
                      )
                      @?= emptySet,
                  testCase "WriterT (Just (v1, v2))" $
                    extractSymbolics
                      ( WriterStrict.WriterT
                          (Just (ssymBool "a", ssymBool "b")) ::
                          WriterStrict.WriterT SymBool Maybe SymBool
                      )
                      @?= buildSymbolSet (ssymbolBool "a", ssymbolBool "b")
                ]
            ],
          testProperty "()" (ioProperty . concreteExtractSymbolicsOkProp @()),
          testCase "(,)" $
            extractSymbolics (ssymBool "a", ssymBool "b")
              @?= buildSymbolSet (ssymbolBool "a", ssymbolBool "b"),
          testCase "(,,)" $
            extractSymbolics (ssymBool "a", ssymBool "b", ssymBool "c")
              @?= buildSymbolSet
                (ssymbolBool "a", ssymbolBool "b", ssymbolBool "c"),
          testGroup
            "ByteString"
            [ testCase "\"\"" $
                extractSymbolics ("" :: B.ByteString) @?= emptySet,
              testCase "\"a\"" $
                extractSymbolics ("a" :: B.ByteString) @?= emptySet
            ],
          testCase "Identity SymBool" $
            extractSymbolics (Identity (ssymBool "a"))
              @?= buildSymbolSet (ssymbolBool "a"),
          testGroup
            "IdentityT (Either SymBool) SymBool"
            [ testCase "Identity (Left v)" $
                extractSymbolics
                  ( IdentityT $ Left (ssymBool "a") ::
                      IdentityT (Either SymBool) SymBool
                  )
                  @?= buildSymbolSet (ssymbolBool "a"),
              testCase "Identity (Right v)" $
                extractSymbolics
                  ( IdentityT $ Right (ssymBool "a") ::
                      IdentityT (Either SymBool) SymBool
                  )
                  @?= buildSymbolSet (ssymbolBool "a")
            ]
        ],
      testGroup
        "deriving ExtractSymbolics for ADT"
        [ testGroup
            "Simple ADT"
            [ testCase "A1" $
                extractSymbolics A1 @?= emptySet,
              testCase "A2" $
                extractSymbolics (A2 (ssymBool "a"))
                  @?= buildSymbolSet (ssymbolBool "a"),
              testCase "A3" $
                extractSymbolics (A3 (ssymBool "a") (ssymBool "b"))
                  @?= buildSymbolSet (ssymbolBool "a", ssymbolBool "b")
            ]
        ]
    ]
