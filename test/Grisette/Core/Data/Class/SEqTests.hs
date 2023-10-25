{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Core.Data.Class.SEqTests (seqTests) where

import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Identity
  ( Identity (Identity),
    IdentityT (IdentityT),
  )
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.ByteString as B
import Data.Foldable (traverse_)
import Data.Functor.Sum (Sum (InL, InR))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Stack (HasCallStack)
import Generics.Deriving (Default (Default), Generic)
import Grisette.Core.Data.Class.Bool
  ( LogicalOp (nots, (&&~)),
    SEq ((/=~), (==~)),
  )
import Grisette.Core.Data.Class.Solvable (Solvable (con))
import Grisette.Core.Data.Class.TestValues (conBool, ssymBool)
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bool (pevalEqvTerm)
import Grisette.IR.SymPrim.Data.SymPrim (SymBool (SymBool))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (Assertion, (@=?))
import Test.QuickCheck (ioProperty)

data A = A1 | A2 SymBool | A3 SymBool SymBool
  deriving (Generic, Show, Eq)
  deriving (SEq) via (Default A)

concreteSEqOkProp :: (HasCallStack, SEq a, Eq a) => (a, a) -> Assertion
concreteSEqOkProp (i, j) = do
  i ==~ j @=? con (i == j)
  i /=~ j @=? con (i /= j)

seqTests :: Test
seqTests =
  testGroup
    "SEq"
    [ testGroup
        "SEq for common types"
        [ testGroup
            "SymBool"
            [ testCase "conBool" $ do
                let bools :: [Bool] = [True, False]
                traverse_
                  ( \(i, j) ->
                      conBool i
                        ==~ conBool j
                        @=? conBool (i == j)
                  )
                  [(x, y) | x <- bools, y <- bools],
              testCase "conBool True vs SymBool" $ do
                conBool True ==~ ssymBool "a" @=? ssymBool "a",
              testCase "conBool False vs SymBool" $ do
                conBool False ==~ ssymBool "a" @=? nots (ssymBool "a"),
              testCase "SymBool vs conBool True" $ do
                ssymBool "a" ==~ conBool True @=? ssymBool "a",
              testCase "SymBool vs conBool False" $ do
                ssymBool "a" ==~ conBool False @=? nots (ssymBool "a"),
              testCase "SymBool vs same SymBool" $ do
                ssymBool "a" ==~ ssymBool "a" @=? conBool True,
              testCase "SymBool vs different SymBool" $ do
                let SymBool terma = ssymBool "a"
                    SymBool termb = ssymBool "b"
                ssymBool "a"
                  ==~ ssymBool "b"
                  @=? SymBool (pevalEqvTerm terma termb)
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
            [ testProperty "[Integer]" $
                ioProperty . concreteSEqOkProp @[Integer],
              testGroup
                "[SymBool]"
                [ testCase "Same length 1" $
                    [ssymBool "a"]
                      ==~ [ssymBool "b"]
                      @=? ssymBool "a"
                        ==~ ssymBool "b",
                  testCase "Same length 2" $
                    [ssymBool "a", ssymBool "b"]
                      ==~ [ssymBool "c", ssymBool "d"]
                      @=? (ssymBool "a" ==~ ssymBool "c")
                        &&~ (ssymBool "b" ==~ ssymBool "d"),
                  testCase "length 1 vs length 0" $
                    [ssymBool "a"] ==~ [] @=? conBool False,
                  testCase "length 1 vs length 2" $
                    [ssymBool "a"]
                      ==~ [ssymBool "c", ssymBool "d"]
                      @=? conBool False
                ]
            ],
          testGroup
            "Maybe"
            [ testProperty "Maybe Integer" $
                ioProperty . concreteSEqOkProp @(Maybe Integer),
              testGroup
                "Maybe SymBool"
                [ testCase "Nothing vs Nothing" $
                    (Nothing :: Maybe SymBool) ==~ Nothing @=? conBool True,
                  testCase "Just vs Nothing" $
                    Just (ssymBool "a") ==~ Nothing @=? conBool False,
                  testCase "Nothing vs Just" $
                    Nothing ==~ Just (ssymBool "a") @=? conBool False,
                  testCase "Just vs Just" $
                    Just (ssymBool "a")
                      ==~ Just (ssymBool "b")
                      @=? ssymBool "a"
                        ==~ ssymBool "b"
                ]
            ],
          testGroup
            "Either"
            [ testProperty "Either" $
                ioProperty . concreteSEqOkProp @(Either Integer Integer),
              testGroup
                "Either SymBool SymBool"
                [ testCase "Left vs Left" $
                    (Left (ssymBool "a") :: Either SymBool SymBool)
                      ==~ Left (ssymBool "b")
                      @=? ssymBool "a"
                        ==~ ssymBool "b",
                  testCase "Right vs Left" $
                    (Right (ssymBool "a") :: Either SymBool SymBool)
                      ==~ Left (ssymBool "b")
                      @=? conBool False,
                  testCase "Left vs Right" $
                    (Left (ssymBool "a") :: Either SymBool SymBool)
                      ==~ Right (ssymBool "b")
                      @=? conBool False,
                  testCase "Right vs Right" $
                    (Right (ssymBool "a") :: Either SymBool SymBool)
                      ==~ Right (ssymBool "b")
                      @=? ssymBool "a"
                        ==~ ssymBool "b"
                ]
            ],
          testGroup
            "MaybeT"
            [ testProperty "MaybeT" $
                ioProperty
                  . concreteSEqOkProp @(MaybeT Maybe Integer)
                  . bimap MaybeT MaybeT,
              testGroup
                "MaybeT Maybe SymBool"
                [ testCase "MaybeT Nothing vs MaybeT Nothing" $
                    (MaybeT Nothing :: MaybeT Maybe SymBool)
                      ==~ MaybeT Nothing
                      @=? conBool True,
                  testCase "MaybeT Nothing vs MaybeT (Just Nothing)" $
                    (MaybeT Nothing :: MaybeT Maybe SymBool)
                      ==~ MaybeT (Just Nothing)
                      @=? conBool False,
                  testCase "MaybeT Nothing vs MaybeT (Just (Just v))" $
                    (MaybeT Nothing :: MaybeT Maybe SymBool)
                      ==~ MaybeT (Just (Just (ssymBool "a")))
                      @=? conBool False,
                  testCase "MaybeT (Just Nothing) vs MaybeT Nothing" $
                    MaybeT (Just Nothing)
                      ==~ (MaybeT Nothing :: MaybeT Maybe SymBool)
                      @=? conBool False,
                  testCase "MaybeT (Just (Just v)) vs MaybeT Nothing" $
                    MaybeT (Just (Just (ssymBool "a")))
                      ==~ (MaybeT Nothing :: MaybeT Maybe SymBool)
                      @=? conBool False,
                  testCase "MaybeT (Just Nothing) vs MaybeT (Just Nothing)" $
                    MaybeT (Just Nothing)
                      ==~ (MaybeT (Just Nothing) :: MaybeT Maybe SymBool)
                      @=? conBool True,
                  testCase "MaybeT (Just (Just v)) vs MaybeT (Just Nothing)" $
                    MaybeT (Just (Just (ssymBool "a")))
                      ==~ (MaybeT (Just Nothing) :: MaybeT Maybe SymBool)
                      @=? conBool False,
                  testCase "MaybeT (Just Nothing) vs MaybeT (Just (Just v))" $
                    MaybeT (Just Nothing)
                      ==~ ( MaybeT (Just (Just (ssymBool "b"))) ::
                              MaybeT Maybe SymBool
                          )
                      @=? conBool False,
                  testCase "MaybeT (Just (Just v)) vs MaybeT (Just (Just v))" $
                    MaybeT (Just (Just (ssymBool "a")))
                      ==~ ( MaybeT (Just (Just (ssymBool "b"))) ::
                              MaybeT Maybe SymBool
                          )
                      @=? ssymBool "a"
                        ==~ ssymBool "b"
                ]
            ],
          testGroup
            "ExceptT"
            [ testProperty "ExceptT Integer Maybe Itneger" $
                ioProperty
                  . concreteSEqOkProp @(ExceptT Integer Maybe Integer)
                  . bimap ExceptT ExceptT,
              testGroup
                "ExceptT SymBool Maybe SymBool"
                [ testCase "ExceptT Nothing vs ExceptT Nothing" $
                    (ExceptT Nothing :: ExceptT SymBool Maybe SymBool)
                      ==~ ExceptT Nothing
                      @=? conBool True,
                  testCase "ExceptT Nothing vs ExceptT (Just (Left v))" $
                    (ExceptT Nothing :: ExceptT SymBool Maybe SymBool)
                      ==~ ExceptT (Just (Left (ssymBool "a")))
                      @=? conBool False,
                  testCase "ExceptT Nothing vs ExceptT (Just (Right v))" $
                    (ExceptT Nothing :: ExceptT SymBool Maybe SymBool)
                      ==~ ExceptT (Just (Right (ssymBool "a")))
                      @=? conBool False,
                  testCase "ExceptT (Just (Left v)) vs ExceptT Nothing" $
                    ExceptT (Just (Left (ssymBool "a")))
                      ==~ (ExceptT Nothing :: ExceptT SymBool Maybe SymBool)
                      @=? conBool False,
                  testCase "ExceptT (Just (Right v)) vs ExceptT Nothing" $
                    ExceptT (Just (Right (ssymBool "a")))
                      ==~ (ExceptT Nothing :: ExceptT SymBool Maybe SymBool)
                      @=? conBool False,
                  testCase
                    "ExceptT (Just (Left v)) vs ExceptT (Just (Left v))"
                    $ ExceptT (Just (Left (ssymBool "a")))
                      ==~ ( ExceptT (Just (Left (ssymBool "b"))) ::
                              ExceptT SymBool Maybe SymBool
                          )
                      @=? ssymBool "a"
                        ==~ ssymBool "b",
                  testCase
                    "ExceptT (Just (Right v)) vs ExceptT (Just (Left v))"
                    $ ExceptT (Just (Right (ssymBool "a")))
                      ==~ ( ExceptT (Just (Left (ssymBool "b"))) ::
                              ExceptT SymBool Maybe SymBool
                          )
                      @=? conBool False,
                  testCase
                    "ExceptT (Just (Left v)) vs ExceptT (Just (Right v))"
                    $ ExceptT (Just (Left (ssymBool "a")))
                      ==~ ( ExceptT (Just (Right (ssymBool "b"))) ::
                              ExceptT SymBool Maybe SymBool
                          )
                      @=? conBool False,
                  testCase
                    "ExceptT (Just (Right v)) vs ExceptT (Just (Right v))"
                    $ ExceptT (Just (Right (ssymBool "a")))
                      ==~ ( ExceptT (Just (Right (ssymBool "b"))) ::
                              ExceptT SymBool Maybe SymBool
                          )
                      @=? ssymBool "a"
                        ==~ ssymBool "b"
                ]
            ],
          testProperty "()" (ioProperty . concreteSEqOkProp @()),
          testGroup
            "(,)"
            [ testProperty "(Integer, Integer)" $
                ioProperty . concreteSEqOkProp @(Integer, Integer),
              testCase "(SymBool, SymBool)" $ do
                (ssymBool "a", ssymBool "c")
                  ==~ (ssymBool "b", ssymBool "d")
                  @=? ssymBool "a"
                    ==~ ssymBool "b"
                    &&~ ssymBool "c"
                    ==~ ssymBool "d"
            ],
          testGroup
            "(,,)"
            [ testProperty "(Integer, Integer, Integer)" $
                ioProperty . concreteSEqOkProp @(Integer, Integer, Integer),
              testCase "(SymBool, SymBool, SymBool)" $
                (ssymBool "a", ssymBool "c", ssymBool "e")
                  ==~ (ssymBool "b", ssymBool "d", ssymBool "f")
                  @=? (ssymBool "a" ==~ ssymBool "b")
                    &&~ ( (ssymBool "c" ==~ ssymBool "d")
                            &&~ (ssymBool "e" ==~ ssymBool "f")
                        )
            ],
          testGroup
            "(,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer)"
                $ ioProperty
                  . concreteSEqOkProp @(Integer, Integer, Integer, Integer),
              testCase "(SymBool, SymBool, SymBool, SymBool)" $ do
                (ssymBool "a", ssymBool "c", ssymBool "e", ssymBool "g")
                  ==~ (ssymBool "b", ssymBool "d", ssymBool "f", ssymBool "h")
                  @=? ( (ssymBool "a" ==~ ssymBool "b")
                          &&~ (ssymBool "c" ==~ ssymBool "d")
                      )
                    &&~ ( (ssymBool "e" ==~ ssymBool "f")
                            &&~ (ssymBool "g" ==~ ssymBool "h")
                        )
            ],
          testGroup
            "(,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer)"
                $ ioProperty
                  . concreteSEqOkProp
                    @(Integer, Integer, Integer, Integer, Integer),
              testCase "(SymBool, SymBool, SymBool, SymBool, SymBool)" $ do
                ( ssymBool "a",
                  ssymBool "c",
                  ssymBool "e",
                  ssymBool "g",
                  ssymBool "i"
                  )
                  ==~ ( ssymBool "b",
                        ssymBool "d",
                        ssymBool "f",
                        ssymBool "h",
                        ssymBool "j"
                      )
                  @=? ( (ssymBool "a" ==~ ssymBool "b")
                          &&~ (ssymBool "c" ==~ ssymBool "d")
                      )
                    &&~ ( (ssymBool "e" ==~ ssymBool "f")
                            &&~ ( (ssymBool "g" ==~ ssymBool "h")
                                    &&~ (ssymBool "i" ==~ ssymBool "j")
                                )
                        )
            ],
          testGroup
            "(,,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer, Integer)"
                $ ioProperty
                  . concreteSEqOkProp
                    @( Integer,
                       Integer,
                       Integer,
                       Integer,
                       Integer,
                       Integer
                     ),
              testCase
                "(SymBool, SymBool, SymBool, SymBool, SymBool, SymBool)"
                $ ( ssymBool "a",
                    ssymBool "c",
                    ssymBool "e",
                    ssymBool "g",
                    ssymBool "i",
                    ssymBool "k"
                  )
                  ==~ ( ssymBool "b",
                        ssymBool "d",
                        ssymBool "f",
                        ssymBool "h",
                        ssymBool "j",
                        ssymBool "l"
                      )
                  @=? ( (ssymBool "a" ==~ ssymBool "b")
                          &&~ ( (ssymBool "c" ==~ ssymBool "d")
                                  &&~ (ssymBool "e" ==~ ssymBool "f")
                              )
                      )
                    &&~ ( (ssymBool "g" ==~ ssymBool "h")
                            &&~ ( (ssymBool "i" ==~ ssymBool "j")
                                    &&~ (ssymBool "k" ==~ ssymBool "l")
                                )
                        )
            ],
          testGroup
            "(,,,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer, Integer, Integer)"
                $ ioProperty
                  . concreteSEqOkProp
                    @( Integer,
                       Integer,
                       Integer,
                       Integer,
                       Integer,
                       Integer,
                       Integer
                     ),
              testCase
                "(SymBool, SymBool, SymBool, SymBool, SymBool, SymBool, SymBool)"
                $ do
                  ( ssymBool "a",
                    ssymBool "c",
                    ssymBool "e",
                    ssymBool "g",
                    ssymBool "i",
                    ssymBool "k",
                    ssymBool "m"
                    )
                    ==~ ( ssymBool "b",
                          ssymBool "d",
                          ssymBool "f",
                          ssymBool "h",
                          ssymBool "j",
                          ssymBool "l",
                          ssymBool "n"
                        )
                    @=? ( (ssymBool "a" ==~ ssymBool "b")
                            &&~ ( (ssymBool "c" ==~ ssymBool "d")
                                    &&~ (ssymBool "e" ==~ ssymBool "f")
                                )
                        )
                      &&~ ( ( (ssymBool "g" ==~ ssymBool "h")
                                &&~ (ssymBool "i" ==~ ssymBool "j")
                            )
                              &&~ ( (ssymBool "k" ==~ ssymBool "l")
                                      &&~ (ssymBool "m" ==~ ssymBool "n")
                                  )
                          )
            ],
          testGroup
            "(,,,,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer)"
                $ ioProperty
                  . concreteSEqOkProp
                    @( Integer,
                       Integer,
                       Integer,
                       Integer,
                       Integer,
                       Integer,
                       Integer,
                       Integer
                     ),
              testCase
                "(SymBool, SymBool, SymBool, SymBool, SymBool, SymBool, SymBool, SymBool)"
                $ ( ssymBool "a",
                    ssymBool "c",
                    ssymBool "e",
                    ssymBool "g",
                    ssymBool "i",
                    ssymBool "k",
                    ssymBool "m",
                    ssymBool "o"
                  )
                  ==~ ( ssymBool "b",
                        ssymBool "d",
                        ssymBool "f",
                        ssymBool "h",
                        ssymBool "j",
                        ssymBool "l",
                        ssymBool "n",
                        ssymBool "p"
                      )
                  @=? ( ( (ssymBool "a" ==~ ssymBool "b")
                            &&~ (ssymBool "c" ==~ ssymBool "d")
                        )
                          &&~ ( (ssymBool "e" ==~ ssymBool "f")
                                  &&~ (ssymBool "g" ==~ ssymBool "h")
                              )
                      )
                    &&~ ( ( (ssymBool "i" ==~ ssymBool "j")
                              &&~ (ssymBool "k" ==~ ssymBool "l")
                          )
                            &&~ ( (ssymBool "m" ==~ ssymBool "n")
                                    &&~ (ssymBool "o" ==~ ssymBool "p")
                                )
                        )
            ],
          testCase "ByteString" $ do
            let bytestrings :: [B.ByteString] = ["", "a", "ab"]
            traverse_
              (\(i, j) -> i ==~ j @=? conBool (i == j))
              [(x, y) | x <- bytestrings, y <- bytestrings],
          testGroup
            "Sum"
            [ testProperty "Sum Maybe Maybe Integer" $
                ioProperty
                  . ( \v ->
                        let eitherToSum ::
                              Either (Maybe Integer) (Maybe Integer) ->
                              Sum Maybe Maybe Integer
                            eitherToSum (Left x) = InL x
                            eitherToSum (Right x) = InR x
                         in concreteSEqOkProp (bimap eitherToSum eitherToSum v)
                    ),
              testGroup
                "Sum Maybe Maybe SymBool"
                [ testCase "InL (Just v) vs InL (Just v)" $
                    (InL $ Just $ ssymBool "a" :: Sum Maybe Maybe SymBool)
                      ==~ InL (Just $ ssymBool "b")
                      @=? ssymBool "a"
                        ==~ ssymBool "b",
                  testCase "InL (Just v) vs InR (Just v)" $
                    (InL $ Just $ ssymBool "a" :: Sum Maybe Maybe SymBool)
                      ==~ InR (Just $ ssymBool "b")
                      @=? conBool False,
                  testCase "InR (Just v) vs InR (Just v)" $
                    (InR $ Just $ ssymBool "a" :: Sum Maybe Maybe SymBool)
                      ==~ InR (Just $ ssymBool "b")
                      @=? ssymBool "a"
                        ==~ ssymBool "b",
                  testCase "InR (Just v) vs InL (Just v)" $
                    (InR $ Just $ ssymBool "a" :: Sum Maybe Maybe SymBool)
                      ==~ InL (Just $ ssymBool "b")
                      @=? conBool False
                ]
            ],
          testGroup
            "Writer"
            [ testGroup
                "Lazy"
                [ testProperty
                    "WriterT Integer (Either Integer) Integer"
                    ( ioProperty
                        . \( v1 :: Either Integer (Integer, Integer),
                             v2 :: Either Integer (Integer, Integer)
                             ) ->
                            concreteSEqOkProp
                              ( WriterLazy.WriterT v1,
                                WriterLazy.WriterT v2
                              )
                    ),
                  testGroup
                    "WriterT SymBool (Either SymBool) SymBool"
                    [ testCase "WriterT (Left v) vs WriterT (Left v)" $
                        ( WriterLazy.WriterT (Left $ ssymBool "a") ::
                            WriterLazy.WriterT SymBool (Either SymBool) SymBool
                        )
                          ==~ WriterLazy.WriterT (Left $ ssymBool "b")
                          @=? ssymBool "a"
                            ==~ ssymBool "b",
                      testCase "WriterT (Left v) vs WriterT (Right v)" $
                        ( WriterLazy.WriterT (Left $ ssymBool "a") ::
                            WriterLazy.WriterT SymBool (Either SymBool) SymBool
                        )
                          ==~ WriterLazy.WriterT
                            (Right (ssymBool "b", ssymBool "c"))
                          @=? conBool False,
                      testCase "WriterT (Right v) vs WriterT (Left v)" $
                        ( WriterLazy.WriterT
                            (Right (ssymBool "b", ssymBool "c")) ::
                            WriterLazy.WriterT SymBool (Either SymBool) SymBool
                        )
                          ==~ WriterLazy.WriterT (Left $ ssymBool "a")
                          @=? conBool False,
                      testCase "WriterT (Right v) vs WriterT (Right v)" $
                        ( WriterLazy.WriterT
                            (Right (ssymBool "a", ssymBool "b")) ::
                            WriterLazy.WriterT SymBool (Either SymBool) SymBool
                        )
                          ==~ WriterLazy.WriterT
                            (Right (ssymBool "c", ssymBool "d"))
                          @=? (ssymBool "a" ==~ ssymBool "c")
                            &&~ (ssymBool "b" ==~ ssymBool "d")
                    ]
                ],
              testGroup
                "Strict"
                [ testProperty
                    "WriterT Integer (Either Integer) Integer"
                    ( ioProperty
                        . \( v1 :: Either Integer (Integer, Integer),
                             v2 :: Either Integer (Integer, Integer)
                             ) ->
                            concreteSEqOkProp
                              ( WriterStrict.WriterT v1,
                                WriterStrict.WriterT v2
                              )
                    ),
                  testGroup
                    "WriterT SymBool (Either SymBool) SymBool"
                    [ testCase "WriterT (Left v) vs WriterT (Left v)" $
                        ( WriterStrict.WriterT (Left $ ssymBool "a") ::
                            WriterStrict.WriterT
                              SymBool
                              (Either SymBool)
                              SymBool
                        )
                          ==~ WriterStrict.WriterT (Left $ ssymBool "b")
                          @=? ssymBool "a"
                            ==~ ssymBool "b",
                      testCase "WriterT (Left v) vs WriterT (Right v)" $
                        ( WriterStrict.WriterT (Left $ ssymBool "a") ::
                            WriterStrict.WriterT
                              SymBool
                              (Either SymBool)
                              SymBool
                        )
                          ==~ WriterStrict.WriterT
                            (Right (ssymBool "b", ssymBool "c"))
                          @=? conBool False,
                      testCase "WriterT (Right v) vs WriterT (Left v)" $
                        ( WriterStrict.WriterT
                            (Right (ssymBool "b", ssymBool "c")) ::
                            WriterStrict.WriterT
                              SymBool
                              (Either SymBool)
                              SymBool
                        )
                          ==~ WriterStrict.WriterT (Left $ ssymBool "a")
                          @=? conBool False,
                      testCase "WriterT (Right v) vs WriterT (Right v)" $
                        ( WriterStrict.WriterT
                            (Right (ssymBool "a", ssymBool "b")) ::
                            WriterStrict.WriterT
                              SymBool
                              (Either SymBool)
                              SymBool
                        )
                          ==~ WriterStrict.WriterT
                            (Right (ssymBool "c", ssymBool "d"))
                          @=? ssymBool "a"
                            ==~ ssymBool "c"
                            &&~ ssymBool "b"
                            ==~ ssymBool "d"
                    ]
                ]
            ],
          testGroup
            "Identity"
            [ testProperty
                "Identity Integer"
                ( ioProperty . \(v1 :: Integer, v2) ->
                    concreteSEqOkProp (Identity v1, Identity v2)
                ),
              testCase "Identity SymBool" $ do
                (Identity $ ssymBool "a" :: Identity SymBool)
                  ==~ Identity (ssymBool "b")
                  @=? ssymBool "a"
                    ==~ ssymBool "b"
            ],
          testGroup
            "IdentityT"
            [ testProperty
                "IdentityT (Either Integer) Integer"
                ( ioProperty . \(v1 :: Either Integer Integer, v2) ->
                    concreteSEqOkProp (IdentityT v1, IdentityT v2)
                ),
              testGroup
                "IdentityT (Either SymBool) SymBool"
                [ testCase "IdentityT (Left v) vs IdentityT (Left v)" $
                    ( IdentityT $ Left $ ssymBool "a" ::
                        IdentityT (Either SymBool) SymBool
                    )
                      ==~ IdentityT (Left $ ssymBool "b")
                      @=? ssymBool "a"
                        ==~ ssymBool "b",
                  testCase "IdentityT (Left v) vs IdentityT (Right v)" $
                    ( IdentityT $ Left $ ssymBool "a" ::
                        IdentityT (Either SymBool) SymBool
                    )
                      ==~ IdentityT (Right $ ssymBool "b")
                      @=? conBool False,
                  testCase "IdentityT (Right v) vs IdentityT (Left v)" $
                    ( IdentityT $ Right $ ssymBool "a" ::
                        IdentityT (Either SymBool) SymBool
                    )
                      ==~ IdentityT (Left $ ssymBool "b")
                      @=? conBool False,
                  testCase "IdentityT (Right v) vs IdentityT (Right v)" $
                    ( IdentityT $ Right $ ssymBool "a" ::
                        IdentityT (Either SymBool) SymBool
                    )
                      ==~ IdentityT (Right $ ssymBool "b")
                      @=? ssymBool "a"
                        ==~ ssymBool "b"
                ]
            ]
        ],
      testGroup
        "deriving SEq for ADT"
        [ testGroup
            "Simple ADT"
            [ testCase "A1 vs A1" $
                A1 ==~ A1 @=? conBool True,
              testCase "A1 vs A2" $
                A1 ==~ A2 (ssymBool "a") @=? conBool False,
              testCase "A1 vs A3" $
                A1 ==~ A3 (ssymBool "a") (ssymBool "b") @=? conBool False,
              testCase "A2 vs A1" $
                A2 (ssymBool "a") ==~ A1 @=? conBool False,
              testCase "A2 vs A2" $
                A2 (ssymBool "a")
                  ==~ A2 (ssymBool "b")
                  @=? ssymBool "a"
                    ==~ ssymBool "b",
              testCase "A2 vs A3" $
                A2 (ssymBool "a")
                  ==~ A3 (ssymBool "b") (ssymBool "c")
                  @=? conBool False,
              testCase "A3 vs A1" $
                A3 (ssymBool "a") (ssymBool "b") ==~ A1 @=? conBool False,
              testCase "A3 vs A2" $
                A3 (ssymBool "a") (ssymBool "b")
                  ==~ A2 (ssymBool "c")
                  @=? conBool False,
              testCase "A3 vs A3" $
                A3 (ssymBool "a") (ssymBool "b")
                  ==~ A3 (ssymBool "c") (ssymBool "d")
                  @=? (ssymBool "a" ==~ ssymBool "c")
                    &&~ (ssymBool "b" ==~ ssymBool "d")
            ]
        ]
    ]
