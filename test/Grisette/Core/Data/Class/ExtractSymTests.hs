{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Core.Data.Class.ExtractSymTests
  ( extractSymTests,
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
import Grisette
  ( ExtractSym (extractSym),
    ITEOp (symIte),
    LogicalOp (symNot, (.&&), (.||)),
    Solvable (ssym),
    SymBool,
    SymEq ((.==)),
    SymbolSetOps (emptySet),
    SymbolSetRep (buildSymbolSet),
  )
import Grisette.Core.Data.Class.TestValues
  ( isymBool,
    isymbolBool,
    ssymBool,
    ssymbolBool,
    symTrue,
  )
import Grisette.Internal.SymPrim.SymInteger (SymInteger)
import Test.Framework (Test, TestOptions' (topt_timeout), plusTestOptions, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (Assertion, (@?=))
import Test.QuickCheck (ioProperty)

data A = A1 | A2 SymBool | A3 SymBool SymBool
  deriving (Generic, Show, Eq)
  deriving (ExtractSym) via (Default A)

concreteExtractSymOkProp ::
  (HasCallStack, ExtractSym a) => (a, a) -> Assertion
concreteExtractSymOkProp x = extractSym x @?= emptySet

extractSymTests :: Test
extractSymTests =
  testGroup
    "ExtractSym"
    [ plusTestOptions (mempty {topt_timeout = Just (Just 1000000)}) $
        testCase "proper memo" $ do
          let pair = ("a" :: SymInteger, "b" :: SymInteger)
          let iter (x, y) = (y, x + y)
          let r = iterate iter pair !! 100
          extractSym r @?= extractSym pair,
      testGroup
        "Common types"
        [ testGroup
            "SymBool"
            [ testCase "con" $
                extractSym symTrue @?= emptySet,
              testCase "ssym" $
                extractSym (ssymBool "a")
                  @?= buildSymbolSet (ssymbolBool "a"),
              testCase "isym" $
                extractSym (isymBool "a" 1)
                  @?= buildSymbolSet (isymbolBool "a" 1),
              testCase "And" $
                extractSym (ssymBool "a" .&& isymBool "b" 1)
                  @?= buildSymbolSet (ssymbolBool "a", isymbolBool "b" 1),
              testCase "Or" $
                extractSym (ssymBool "a" .|| isymBool "b" 1)
                  @?= buildSymbolSet (ssymbolBool "a", isymbolBool "b" 1),
              testCase "Equal" $
                extractSym (ssymBool "a" .== isymBool "b" 1)
                  @?= buildSymbolSet (ssymbolBool "a", isymbolBool "b" 1),
              testCase "ITE" $
                extractSym
                  (symIte (ssym "a") (isymBool "b" 1) (ssymBool "c"))
                  @?= buildSymbolSet
                    ( ssymbolBool "a",
                      isymbolBool "b" 1,
                      ssymbolBool "c"
                    ),
              testCase "Not" $
                extractSym (symNot $ isymBool "a" 1)
                  @?= buildSymbolSet (isymbolBool "a" 1)
            ],
          testProperty "Bool" $
            ioProperty . concreteExtractSymOkProp @Bool,
          testProperty "Integer" $
            ioProperty . concreteExtractSymOkProp @Integer,
          testProperty "Char" $
            ioProperty . concreteExtractSymOkProp @Char,
          testProperty "Int" $ ioProperty . concreteExtractSymOkProp @Int,
          testProperty "Int8" $
            ioProperty . concreteExtractSymOkProp @Int8,
          testProperty "Int16" $
            ioProperty . concreteExtractSymOkProp @Int16,
          testProperty "Int32" $
            ioProperty . concreteExtractSymOkProp @Int32,
          testProperty "Int64" $
            ioProperty . concreteExtractSymOkProp @Int64,
          testProperty "Word" $
            ioProperty . concreteExtractSymOkProp @Word,
          testProperty "Word8" $
            ioProperty . concreteExtractSymOkProp @Word8,
          testProperty "Word16" $
            ioProperty . concreteExtractSymOkProp @Word16,
          testProperty "Word32" $
            ioProperty . concreteExtractSymOkProp @Word32,
          testProperty "Word64" $
            ioProperty . concreteExtractSymOkProp @Word64,
          testGroup
            "[SymBool]"
            [ testCase "[]" $
                extractSym ([] :: [SymBool]) @?= emptySet,
              testCase "[v]" $
                extractSym [ssymBool "a"]
                  @?= buildSymbolSet (ssymbolBool "a"),
              testCase "[v1, v2]" $
                extractSym [ssymBool "a", ssymBool "b"]
                  @?= buildSymbolSet (ssymbolBool "a", ssymbolBool "b")
            ],
          testGroup
            "Maybe SymBool"
            [ testCase "Nothing" $
                extractSym (Nothing :: Maybe SymBool) @?= emptySet,
              testCase "Just v" $
                extractSym (Just (ssymBool "a"))
                  @?= buildSymbolSet (ssymbolBool "a")
            ],
          testGroup
            "Either SymBool SymBool"
            [ testCase "Left v" $
                extractSym
                  (Left (ssym "a") :: Either SymBool SymBool)
                  @?= buildSymbolSet (ssymbolBool "a"),
              testCase "Right v" $
                extractSym
                  (Right (ssym "a") :: Either SymBool SymBool)
                  @?= buildSymbolSet (ssymbolBool "a")
            ],
          testGroup
            "MaybeT Maybe SymBool"
            [ testCase "MaybeT Nothing" $
                extractSym (MaybeT Nothing :: MaybeT Maybe SymBool)
                  @?= emptySet,
              testCase "MaybeT (Just Nothing)" $
                extractSym (MaybeT (Just Nothing) :: MaybeT Maybe SymBool)
                  @?= emptySet,
              testCase "MaybeT (Just (Just v))" $
                extractSym (MaybeT (Just (Just (ssymBool "a"))))
                  @?= buildSymbolSet (ssymbolBool "a")
            ],
          testGroup
            "ExceptT SymBool Maybe SymBool"
            [ testCase "ExceptT Nothing" $
                extractSym
                  (ExceptT Nothing :: ExceptT SymBool Maybe SymBool)
                  @?= emptySet,
              testCase "ExceptT (Just (Left v))" $
                extractSym
                  ( ExceptT (Just (Left (ssym "a"))) ::
                      ExceptT SymBool Maybe SymBool
                  )
                  @?= buildSymbolSet (ssymbolBool "a"),
              testCase "ExceptT (Just (Right v))" $
                extractSym
                  ( ExceptT (Just (Right (ssym "a"))) ::
                      ExceptT SymBool Maybe SymBool
                  )
                  @?= buildSymbolSet (ssymbolBool "a")
            ],
          testGroup
            "WriterT SymBool Maybe SymBool"
            [ testGroup
                "Lazy"
                [ testCase "WriterT Nothing" $
                    extractSym
                      ( WriterLazy.WriterT Nothing ::
                          WriterLazy.WriterT SymBool Maybe SymBool
                      )
                      @?= emptySet,
                  testCase "WriterT (Just (v1, v2))" $
                    extractSym
                      ( WriterLazy.WriterT
                          (Just (ssym "a", ssym "b")) ::
                          WriterLazy.WriterT SymBool Maybe SymBool
                      )
                      @?= buildSymbolSet (ssymbolBool "a", ssymbolBool "b")
                ],
              testGroup
                "Strict"
                [ testCase "WriterT Nothing" $
                    extractSym
                      ( WriterStrict.WriterT Nothing ::
                          WriterStrict.WriterT SymBool Maybe SymBool
                      )
                      @?= emptySet,
                  testCase "WriterT (Just (v1, v2))" $
                    extractSym
                      ( WriterStrict.WriterT
                          (Just (ssym "a", ssym "b")) ::
                          WriterStrict.WriterT SymBool Maybe SymBool
                      )
                      @?= buildSymbolSet (ssymbolBool "a", ssymbolBool "b")
                ]
            ],
          testProperty "()" (ioProperty . concreteExtractSymOkProp @()),
          testCase "(,)" $
            extractSym (ssymBool "a", ssymBool "b")
              @?= buildSymbolSet (ssymbolBool "a", ssymbolBool "b"),
          testCase "(,,)" $
            extractSym (ssymBool "a", ssymBool "b", ssymBool "c")
              @?= buildSymbolSet
                (ssymbolBool "a", ssymbolBool "b", ssymbolBool "c"),
          testGroup
            "ByteString"
            [ testCase "\"\"" $
                extractSym ("" :: B.ByteString) @?= emptySet,
              testCase "\"a\"" $
                extractSym ("a" :: B.ByteString) @?= emptySet
            ],
          testCase "Identity SymBool" $
            extractSym (Identity (ssymBool "a"))
              @?= buildSymbolSet (ssymbolBool "a"),
          testGroup
            "IdentityT (Either SymBool) SymBool"
            [ testCase "Identity (Left v)" $
                extractSym
                  ( IdentityT $ Left (ssym "a") ::
                      IdentityT (Either SymBool) SymBool
                  )
                  @?= buildSymbolSet (ssymbolBool "a"),
              testCase "Identity (Right v)" $
                extractSym
                  ( IdentityT $ Right (ssym "a") ::
                      IdentityT (Either SymBool) SymBool
                  )
                  @?= buildSymbolSet (ssymbolBool "a")
            ]
        ],
      testGroup
        "deriving ExtractSym for ADT"
        [ testGroup
            "Simple ADT"
            [ testCase "A1" $
                extractSym A1 @?= emptySet,
              testCase "A2" $
                extractSym (A2 (ssym "a"))
                  @?= buildSymbolSet (ssymbolBool "a"),
              testCase "A3" $
                extractSym (A3 (ssym "a") (ssym "b"))
                  @?= buildSymbolSet (ssymbolBool "a", ssymbolBool "b")
            ]
        ]
    ]
