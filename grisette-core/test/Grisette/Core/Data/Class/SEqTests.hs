{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Core.Data.Class.SEqTests where

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
import Grisette.Core.Data.Class.Bool
import Grisette.TestUtils.SBool
import Grisette.TestUtils.SEq
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

data A = A1 | A2 SBool | A3 SBool SBool
  deriving (Generic, Show, Eq)
  deriving (GSEq SBool) via (Default A)

seqTests :: TestTree
seqTests =
  testGroup
    "SEqTests"
    [ testGroup
        "SEq for common types"
        [ testGroup
            "SBool"
            [ testCase "CBool" $ do
                let bools :: [Bool] = [True, False]
                traverse_ (\(i, j) -> CBool i `gsymeq` CBool j @=? CBool (i == j)) [(x, y) | x <- bools, y <- bools],
              testCase "CBool True vs SBool" $ do
                CBool True `gsymeq` SSBool "a" @=? SSBool "a",
              testCase "CBool False vs SBool" $ do
                CBool False `gsymeq` SSBool "a" @=? Not (SSBool "a"),
              testCase "SBool vs CBool True" $ do
                SSBool "a" `gsymeq` CBool True @=? SSBool "a",
              testCase "SBool vs CBool False" $ do
                SSBool "a" `gsymeq` CBool False @=? Not (SSBool "a"),
              testCase "SBool vs same SBool" $ do
                SSBool "a" `gsymeq` SSBool "a" @=? CBool True,
              testCase "SBool vs different SBool" $ do
                SSBool "a" `gsymeq` SSBool "b" @=? Equal (SSBool "a") (SSBool "b")
            ],
          testProperty "Bool" (ioProperty . concreteSEqOkProp @Bool),
          testProperty "Integer" (ioProperty . concreteSEqOkProp @Integer),
          testProperty "Char" (ioProperty . concreteSEqOkProp @Char),
          testProperty "Int" (ioProperty . concreteSEqOkProp @Int),
          testProperty "Int8" (ioProperty . concreteSEqOkProp @Int8),
          testProperty "Int16" (ioProperty . concreteSEqOkProp @Int16),
          testProperty "Int32" (ioProperty . concreteSEqOkProp @Int32),
          testProperty "Int64" (ioProperty . concreteSEqOkProp @Int64),
          testProperty "Word" (ioProperty . concreteSEqOkProp @Word),
          testProperty "Word8" (ioProperty . concreteSEqOkProp @Word8),
          testProperty "Word16" (ioProperty . concreteSEqOkProp @Word16),
          testProperty "Word32" (ioProperty . concreteSEqOkProp @Word32),
          testProperty "Word64" (ioProperty . concreteSEqOkProp @Word64),
          testGroup
            "List"
            [ testProperty "[Integer]" (ioProperty . concreteSEqOkProp @[Integer]),
              testGroup
                "[SBool]"
                [ testCase "Same length 1" $
                    [SSBool "a"] `gsymeq` [SSBool "b"] @=? Equal (SSBool "a") (SSBool "b"),
                  testCase "Same length 2" $
                    [SSBool "a", SSBool "b"]
                      `gsymeq` [SSBool "c", SSBool "d"]
                      @=? And (SSBool "a" `gsymeq` SSBool "c") (SSBool "b" `gsymeq` SSBool "d"),
                  testCase "length 1 vs length 0" $
                    [SSBool "a"] `gsymeq` [] @=? CBool False,
                  testCase "length 1 vs length 2" $
                    [SSBool "a"] `gsymeq` [SSBool "c", SSBool "d"] @=? CBool False
                ]
            ],
          testGroup
            "Maybe"
            [ testProperty "Maybe Integer" (ioProperty . concreteSEqOkProp @(Maybe Integer)),
              testGroup
                "Maybe SBool"
                [ testCase "Nothing vs Nothing" $
                    (Nothing :: Maybe SBool) `gsymeq` Nothing @=? CBool True,
                  testCase "Just vs Nothing" $
                    Just (SSBool "a") `gsymeq` Nothing @=? CBool False,
                  testCase "Nothing vs Just" $
                    Nothing `gsymeq` Just (SSBool "a") @=? CBool False,
                  testCase "Just vs Just" $
                    Just (SSBool "a") `gsymeq` Just (SSBool "b") @=? Equal (SSBool "a") (SSBool "b")
                ]
            ],
          testGroup
            "Either"
            [ testProperty "Either" (ioProperty . concreteSEqOkProp @(Either Integer Integer)),
              testGroup
                "Either SBool SBool"
                [ testCase "Left vs Left" $
                    (Left (SSBool "a") :: Either SBool SBool)
                      `gsymeq` Left (SSBool "b")
                      @=? Equal (SSBool "a") (SSBool "b"),
                  testCase "Right vs Left" $
                    (Right (SSBool "a") :: Either SBool SBool) `gsymeq` Left (SSBool "b") @=? CBool False,
                  testCase "Left vs Right" $
                    (Left (SSBool "a") :: Either SBool SBool) `gsymeq` Right (SSBool "b") @=? CBool False,
                  testCase "Right vs Right" $
                    (Right (SSBool "a") :: Either SBool SBool)
                      `gsymeq` Right (SSBool "b")
                      @=? Equal (SSBool "a") (SSBool "b")
                ]
            ],
          testGroup
            "MaybeT"
            [ testProperty "MaybeT" (ioProperty . concreteSEqOkProp @(MaybeT Maybe Integer) . bimap MaybeT MaybeT),
              testGroup
                "MaybeT Maybe SBool"
                [ testCase "MaybeT Nothing vs MaybeT Nothing" $
                    (MaybeT Nothing :: MaybeT Maybe SBool) `gsymeq` MaybeT Nothing @=? CBool True,
                  testCase "MaybeT Nothing vs MaybeT (Just Nothing)" $
                    (MaybeT Nothing :: MaybeT Maybe SBool) `gsymeq` MaybeT (Just Nothing) @=? CBool False,
                  testCase "MaybeT Nothing vs MaybeT (Just (Just v))" $
                    (MaybeT Nothing :: MaybeT Maybe SBool) `gsymeq` MaybeT (Just (Just (SSBool "a"))) @=? CBool False,
                  testCase "MaybeT (Just Nothing) vs MaybeT Nothing" $
                    MaybeT (Just Nothing) `gsymeq` (MaybeT Nothing :: MaybeT Maybe SBool) @=? CBool False,
                  testCase "MaybeT (Just (Just v)) vs MaybeT Nothing" $
                    MaybeT (Just (Just (SSBool "a"))) `gsymeq` (MaybeT Nothing :: MaybeT Maybe SBool) @=? CBool False,
                  testCase "MaybeT (Just Nothing) vs MaybeT (Just Nothing)" $
                    MaybeT (Just Nothing) `gsymeq` (MaybeT (Just Nothing) :: MaybeT Maybe SBool) @=? CBool True,
                  testCase "MaybeT (Just (Just v)) vs MaybeT (Just Nothing)" $
                    MaybeT (Just (Just (SSBool "a"))) `gsymeq` (MaybeT (Just Nothing) :: MaybeT Maybe SBool) @=? CBool False,
                  testCase "MaybeT (Just Nothing) vs MaybeT (Just (Just v))" $
                    MaybeT (Just Nothing) `gsymeq` (MaybeT (Just (Just (SSBool "b"))) :: MaybeT Maybe SBool) @=? CBool False,
                  testCase "MaybeT (Just (Just v)) vs MaybeT (Just (Just v))" $
                    MaybeT (Just (Just (SSBool "a")))
                      `gsymeq` (MaybeT (Just (Just (SSBool "b"))) :: MaybeT Maybe SBool)
                      @=? Equal (SSBool "a") (SSBool "b")
                ]
            ],
          testGroup
            "ExceptT"
            [ testProperty "ExceptT Integer Maybe Itneger" (ioProperty . concreteSEqOkProp @(ExceptT Integer Maybe Integer) . bimap ExceptT ExceptT),
              testGroup
                "ExceptT SBool Maybe SBool"
                [ testCase "ExceptT Nothing vs ExceptT Nothing" $
                    (ExceptT Nothing :: ExceptT SBool Maybe SBool) `gsymeq` ExceptT Nothing @=? CBool True,
                  testCase "ExceptT Nothing vs ExceptT (Just (Left v))" $
                    (ExceptT Nothing :: ExceptT SBool Maybe SBool) `gsymeq` ExceptT (Just (Left (SSBool "a"))) @=? CBool False,
                  testCase "ExceptT Nothing vs ExceptT (Just (Right v))" $
                    (ExceptT Nothing :: ExceptT SBool Maybe SBool) `gsymeq` ExceptT (Just (Right (SSBool "a"))) @=? CBool False,
                  testCase "ExceptT (Just (Left v)) vs ExceptT Nothing" $
                    ExceptT (Just (Left (SSBool "a"))) `gsymeq` (ExceptT Nothing :: ExceptT SBool Maybe SBool) @=? CBool False,
                  testCase "ExceptT (Just (Right v)) vs ExceptT Nothing" $
                    ExceptT (Just (Right (SSBool "a"))) `gsymeq` (ExceptT Nothing :: ExceptT SBool Maybe SBool) @=? CBool False,
                  testCase "ExceptT (Just (Left v)) vs ExceptT (Just (Left v))" $
                    ExceptT (Just (Left (SSBool "a")))
                      `gsymeq` (ExceptT (Just (Left (SSBool "b"))) :: ExceptT SBool Maybe SBool)
                      @=? Equal (SSBool "a") (SSBool "b"),
                  testCase "ExceptT (Just (Right v)) vs ExceptT (Just (Left v))" $
                    ExceptT (Just (Right (SSBool "a")))
                      `gsymeq` (ExceptT (Just (Left (SSBool "b"))) :: ExceptT SBool Maybe SBool)
                      @=? CBool False,
                  testCase "ExceptT (Just (Left v)) vs ExceptT (Just (Right v))" $
                    ExceptT (Just (Left (SSBool "a")))
                      `gsymeq` (ExceptT (Just (Right (SSBool "b"))) :: ExceptT SBool Maybe SBool)
                      @=? CBool False,
                  testCase "ExceptT (Just (Right v)) vs ExceptT (Just (Right v))" $
                    ExceptT (Just (Right (SSBool "a")))
                      `gsymeq` (ExceptT (Just (Right (SSBool "b"))) :: ExceptT SBool Maybe SBool)
                      @=? Equal (SSBool "a") (SSBool "b")
                ]
            ],
          testProperty "()" (ioProperty . concreteSEqOkProp @()),
          testGroup
            "(,)"
            [ testProperty "(Integer, Integer)" (ioProperty . concreteSEqOkProp @(Integer, Integer)),
              testCase "(SBool, SBool)" $ do
                (SSBool "a", SSBool "c")
                  `gsymeq` (SSBool "b", SSBool "d")
                  @=? And (Equal (SSBool "a") (SSBool "b")) (Equal (SSBool "c") (SSBool "d"))
            ],
          testGroup
            "(,,)"
            [ testProperty "(Integer, Integer, Integer)" (ioProperty . concreteSEqOkProp @(Integer, Integer, Integer)),
              testCase "(SBool, SBool, SBool)" $
                (SSBool "a", SSBool "c", SSBool "e")
                  `gsymeq` (SSBool "b", SSBool "d", SSBool "f")
                  @=? And
                    (Equal (SSBool "a") (SSBool "b"))
                    (And (Equal (SSBool "c") (SSBool "d")) (Equal (SSBool "e") (SSBool "f")))
            ],
          testGroup
            "(,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer)"
                (ioProperty . concreteSEqOkProp @(Integer, Integer, Integer, Integer)),
              testCase "(SBool, SBool, SBool, SBool)" $ do
                (SSBool "a", SSBool "c", SSBool "e", SSBool "g")
                  `gsymeq` (SSBool "b", SSBool "d", SSBool "f", SSBool "h")
                  @=? And
                    ( And
                        (Equal (SSBool "a") (SSBool "b"))
                        (Equal (SSBool "c") (SSBool "d"))
                    )
                    ( And
                        (Equal (SSBool "e") (SSBool "f"))
                        (Equal (SSBool "g") (SSBool "h"))
                    )
            ],
          testGroup
            "(,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer)"
                (ioProperty . concreteSEqOkProp @(Integer, Integer, Integer, Integer, Integer)),
              testCase "(SBool, SBool, SBool, SBool, SBool)" $ do
                (SSBool "a", SSBool "c", SSBool "e", SSBool "g", SSBool "i")
                  `gsymeq` (SSBool "b", SSBool "d", SSBool "f", SSBool "h", SSBool "j")
                  @=? And
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
            ],
          testGroup
            "(,,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer, Integer)"
                (ioProperty . concreteSEqOkProp @(Integer, Integer, Integer, Integer, Integer, Integer)),
              testCase "(SBool, SBool, SBool, SBool, SBool, SBool)" $
                (SSBool "a", SSBool "c", SSBool "e", SSBool "g", SSBool "i", SSBool "k")
                  `gsymeq` (SSBool "b", SSBool "d", SSBool "f", SSBool "h", SSBool "j", SSBool "l")
                  @=? And
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
            ],
          testGroup
            "(,,,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer, Integer, Integer)"
                (ioProperty . concreteSEqOkProp @(Integer, Integer, Integer, Integer, Integer, Integer, Integer)),
              testCase "(SBool, SBool, SBool, SBool, SBool, SBool, SBool)" $ do
                (SSBool "a", SSBool "c", SSBool "e", SSBool "g", SSBool "i", SSBool "k", SSBool "m")
                  `gsymeq` (SSBool "b", SSBool "d", SSBool "f", SSBool "h", SSBool "j", SSBool "l", SSBool "n")
                  @=? And
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
            ],
          testGroup
            "(,,,,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer)"
                (ioProperty . concreteSEqOkProp @(Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer)),
              testCase "(SBool, SBool, SBool, SBool, SBool, SBool, SBool, SBool)" $
                (SSBool "a", SSBool "c", SSBool "e", SSBool "g", SSBool "i", SSBool "k", SSBool "m", SSBool "o")
                  `gsymeq` (SSBool "b", SSBool "d", SSBool "f", SSBool "h", SSBool "j", SSBool "l", SSBool "n", SSBool "p")
                  @=? And
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
            ],
          testCase "ByteString" $ do
            let bytestrings :: [B.ByteString] = ["", "a", "ab"]
            traverse_ (\(i, j) -> i `gsymeq` j @=? CBool (i == j)) [(x, y) | x <- bytestrings, y <- bytestrings],
          testGroup
            "Sum"
            [ testProperty "Sum Maybe Maybe Integer" $
                ioProperty
                  . ( \v ->
                        let eitherToSum :: Either (Maybe Integer) (Maybe Integer) -> Sum Maybe Maybe Integer
                            eitherToSum (Left x) = InL x
                            eitherToSum (Right x) = InR x
                         in concreteSEqOkProp (bimap eitherToSum eitherToSum v)
                    ),
              testGroup
                "Sum Maybe Maybe SBool"
                [ testCase "InL (Just v) vs InL (Just v)" $
                    (InL $ Just $ SSBool "a" :: Sum Maybe Maybe SBool)
                      `gsymeq` InL (Just $ SSBool "b")
                      @=? Equal (SSBool "a") (SSBool "b"),
                  testCase "InL (Just v) vs InR (Just v)" $
                    (InL $ Just $ SSBool "a" :: Sum Maybe Maybe SBool) `gsymeq` InR (Just $ SSBool "b") @=? CBool False,
                  testCase "InR (Just v) vs InR (Just v)" $
                    (InR $ Just $ SSBool "a" :: Sum Maybe Maybe SBool)
                      `gsymeq` InR (Just $ SSBool "b")
                      @=? Equal (SSBool "a") (SSBool "b"),
                  testCase "InR (Just v) vs InL (Just v)" $
                    (InR $ Just $ SSBool "a" :: Sum Maybe Maybe SBool) `gsymeq` InL (Just $ SSBool "b") @=? CBool False
                ]
            ],
          testGroup
            "Writer"
            [ testGroup
                "Lazy"
                [ testProperty
                    "WriterT Integer (Either Integer) Integer"
                    ( ioProperty . \(v1 :: Either Integer (Integer, Integer), v2 :: Either Integer (Integer, Integer)) ->
                        concreteSEqOkProp (WriterLazy.WriterT v1, WriterLazy.WriterT v2)
                    ),
                  testGroup
                    "WriterT SBool (Either SBool) SBool"
                    [ testCase "WriterT (Left v) vs WriterT (Left v)" $
                        (WriterLazy.WriterT (Left $ SSBool "a") :: WriterLazy.WriterT SBool (Either SBool) SBool)
                          `gsymeq` WriterLazy.WriterT (Left $ SSBool "b")
                          @=? Equal (SSBool "a") (SSBool "b"),
                      testCase "WriterT (Left v) vs WriterT (Right v)" $
                        (WriterLazy.WriterT (Left $ SSBool "a") :: WriterLazy.WriterT SBool (Either SBool) SBool)
                          `gsymeq` WriterLazy.WriterT (Right (SSBool "b", SSBool "c"))
                          @=? CBool False,
                      testCase "WriterT (Right v) vs WriterT (Left v)" $
                        (WriterLazy.WriterT (Right (SSBool "b", SSBool "c")) :: WriterLazy.WriterT SBool (Either SBool) SBool)
                          `gsymeq` WriterLazy.WriterT (Left $ SSBool "a")
                          @=? CBool False,
                      testCase "WriterT (Right v) vs WriterT (Right v)" $
                        (WriterLazy.WriterT (Right (SSBool "a", SSBool "b")) :: WriterLazy.WriterT SBool (Either SBool) SBool)
                          `gsymeq` WriterLazy.WriterT (Right (SSBool "c", SSBool "d"))
                          @=? And (Equal (SSBool "a") (SSBool "c")) (Equal (SSBool "b") (SSBool "d"))
                    ]
                ],
              testGroup
                "Strict"
                [ testProperty
                    "WriterT Integer (Either Integer) Integer"
                    ( ioProperty . \(v1 :: Either Integer (Integer, Integer), v2 :: Either Integer (Integer, Integer)) ->
                        concreteSEqOkProp (WriterStrict.WriterT v1, WriterStrict.WriterT v2)
                    ),
                  testGroup
                    "WriterT SBool (Either SBool) SBool"
                    [ testCase "WriterT (Left v) vs WriterT (Left v)" $
                        (WriterStrict.WriterT (Left $ SSBool "a") :: WriterStrict.WriterT SBool (Either SBool) SBool)
                          `gsymeq` WriterStrict.WriterT (Left $ SSBool "b")
                          @=? Equal (SSBool "a") (SSBool "b"),
                      testCase "WriterT (Left v) vs WriterT (Right v)" $
                        (WriterStrict.WriterT (Left $ SSBool "a") :: WriterStrict.WriterT SBool (Either SBool) SBool)
                          `gsymeq` WriterStrict.WriterT (Right (SSBool "b", SSBool "c"))
                          @=? CBool False,
                      testCase "WriterT (Right v) vs WriterT (Left v)" $
                        (WriterStrict.WriterT (Right (SSBool "b", SSBool "c")) :: WriterStrict.WriterT SBool (Either SBool) SBool)
                          `gsymeq` WriterStrict.WriterT (Left $ SSBool "a")
                          @=? CBool False,
                      testCase "WriterT (Right v) vs WriterT (Right v)" $
                        (WriterStrict.WriterT (Right (SSBool "a", SSBool "b")) :: WriterStrict.WriterT SBool (Either SBool) SBool)
                          `gsymeq` WriterStrict.WriterT (Right (SSBool "c", SSBool "d"))
                          @=? And (Equal (SSBool "a") (SSBool "c")) (Equal (SSBool "b") (SSBool "d"))
                    ]
                ]
            ],
          testGroup
            "Identity"
            [ testProperty
                "Identity Integer"
                (ioProperty . \(v1 :: Integer, v2) -> concreteSEqOkProp (Identity v1, Identity v2)),
              testCase "Identity SBool" $ do
                (Identity $ SSBool "a" :: Identity SBool)
                  `gsymeq` Identity (SSBool "b")
                  @=? Equal (SSBool "a") (SSBool "b")
            ],
          testGroup
            "IdentityT"
            [ testProperty
                "IdentityT (Either Integer) Integer"
                (ioProperty . \(v1 :: Either Integer Integer, v2) -> concreteSEqOkProp (IdentityT v1, IdentityT v2)),
              testGroup
                "IdentityT (Either SBool) SBool"
                [ testCase "IdentityT (Left v) vs IdentityT (Left v)" $
                    (IdentityT $ Left $ SSBool "a" :: IdentityT (Either SBool) SBool)
                      `gsymeq` IdentityT (Left $ SSBool "b")
                      @=? Equal (SSBool "a") (SSBool "b"),
                  testCase "IdentityT (Left v) vs IdentityT (Right v)" $
                    (IdentityT $ Left $ SSBool "a" :: IdentityT (Either SBool) SBool)
                      `gsymeq` IdentityT (Right $ SSBool "b")
                      @=? CBool False,
                  testCase "IdentityT (Right v) vs IdentityT (Left v)" $
                    (IdentityT $ Right $ SSBool "a" :: IdentityT (Either SBool) SBool)
                      `gsymeq` IdentityT (Left $ SSBool "b")
                      @=? CBool False,
                  testCase "IdentityT (Right v) vs IdentityT (Right v)" $
                    (IdentityT $ Right $ SSBool "a" :: IdentityT (Either SBool) SBool)
                      `gsymeq` IdentityT (Right $ SSBool "b")
                      @=? Equal (SSBool "a") (SSBool "b")
                ]
            ]
        ],
      testGroup
        "deriving SEq for ADT"
        [ testGroup
            "Simple ADT"
            [ testCase "A1 vs A1" $
                A1 `gsymeq` A1 @=? CBool True,
              testCase "A1 vs A2" $
                A1 `gsymeq` A2 (SSBool "a") @=? CBool False,
              testCase "A1 vs A3" $
                A1 `gsymeq` A3 (SSBool "a") (SSBool "b") @=? CBool False,
              testCase "A2 vs A1" $
                A2 (SSBool "a") `gsymeq` A1 @=? CBool False,
              testCase "A2 vs A2" $
                A2 (SSBool "a") `gsymeq` A2 (SSBool "b") @=? Equal (SSBool "a") (SSBool "b"),
              testCase "A2 vs A3" $
                A2 (SSBool "a") `gsymeq` A3 (SSBool "b") (SSBool "c") @=? CBool False,
              testCase "A3 vs A1" $
                A3 (SSBool "a") (SSBool "b") `gsymeq` A1 @=? CBool False,
              testCase "A3 vs A2" $
                A3 (SSBool "a") (SSBool "b") `gsymeq` A2 (SSBool "c") @=? CBool False,
              testCase "A3 vs A3" $
                A3 (SSBool "a") (SSBool "b")
                  `gsymeq` A3 (SSBool "c") (SSBool "d")
                  @=? And (Equal (SSBool "a") (SSBool "c")) (Equal (SSBool "b") (SSBool "d"))
            ]
        ]
    ]
