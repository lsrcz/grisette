{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Pizza.Core.Data.Class.EvaluateSymTests where

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
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

evaluateSymTests :: TestTree
evaluateSymTests =
  testGroup
    "EvaluateSymTests"
    [ testGroup
        "EvaluateSym for common types"
        [ testGroup
            "SBool"
            [ let model = M.empty :: M.HashMap Symbol Bool
               in testGroup
                    "Empty model / no fill default"
                    [ testCase "CBool" $
                        evaluateSym False model (CBool True) @=? CBool True,
                      testCase "SSBool" $
                        evaluateSym False model (SSBool "a") @=? SSBool "a",
                      testCase "ISBool" $
                        evaluateSym False model (ISBool "a" 1) @=? ISBool "a" 1,
                      testCase "Or" $
                        evaluateSym False model (Or (SSBool "a") (SSBool "b"))
                          @=? Or (SSBool "a") (SSBool "b"),
                      testCase "And" $
                        evaluateSym False model (And (SSBool "a") (SSBool "b"))
                          @=? And (SSBool "a") (SSBool "b"),
                      testCase "Equal" $
                        evaluateSym False model (Equal (SSBool "a") (SSBool "b"))
                          @=? Equal (SSBool "a") (SSBool "b"),
                      testCase "Not" $
                        evaluateSym False model (Not (SSBool "a"))
                          @=? Not (SSBool "a"),
                      testCase "ITE" $
                        evaluateSym False model (ITE (SSBool "a") (SSBool "b") (SSBool "c"))
                          @=? ITE (SSBool "a") (SSBool "b") (SSBool "c")
                    ],
              let model = M.empty :: M.HashMap Symbol Bool
               in testGroup
                    "Empty model / with fill default"
                    [ testCase "CBool" $
                        evaluateSym True model (CBool True) @=? CBool True,
                      testCase "SSBool" $
                        evaluateSym True model (SSBool "a") @=? CBool False,
                      testCase "ISBool" $
                        evaluateSym True model (ISBool "a" 1) @=? CBool False,
                      testCase "Or" $
                        evaluateSym True model (Or (SSBool "a") (SSBool "b")) @=? CBool False,
                      testCase "And" $
                        evaluateSym True model (And (SSBool "a") (SSBool "b")) @=? CBool False,
                      testCase "Equal" $
                        evaluateSym True model (Equal (SSBool "a") (SSBool "b")) @=? CBool True,
                      testCase "Not" $
                        evaluateSym True model (Not (SSBool "a")) @=? CBool True,
                      testCase "ITE" $
                        evaluateSym True model (ITE (SSBool "a") (SSBool "b") (SSBool "c")) @=? CBool False
                    ],
              let model =
                    M.fromList
                      [ (SSymbol "a", True),
                        (ISymbol "a" 1, False),
                        (SSymbol "b", False),
                        (SSymbol "c", True)
                      ] ::
                      M.HashMap Symbol Bool
               in testGroup
                    "Some model"
                    [ testCase "CBool" $
                        evaluateSym True model (CBool True) @=? CBool True,
                      testCase "SSBool" $
                        evaluateSym True model (SSBool "a") @=? CBool True,
                      testCase "ISBool" $
                        evaluateSym True model (ISBool "a" 1) @=? CBool False,
                      testCase "Or" $
                        evaluateSym True model (Or (SSBool "a") (SSBool "b")) @=? CBool True,
                      testCase "And" $
                        evaluateSym True model (And (SSBool "a") (SSBool "b")) @=? CBool False,
                      testCase "Equal" $
                        evaluateSym True model (Equal (SSBool "a") (SSBool "b")) @=? CBool False,
                      testCase "Not" $
                        evaluateSym True model (Not (SSBool "a")) @=? CBool False,
                      testCase "ITE" $
                        evaluateSym True model (ITE (SSBool "a") (SSBool "b") (SSBool "c")) @=? CBool False
                    ]
            ],
          testProperty "Bool" (ioProperty . concreteEvaluateSymOkProp @Bool),
          testProperty "Integer" (ioProperty . concreteEvaluateSymOkProp @Integer),
          testProperty "Char" (ioProperty . concreteEvaluateSymOkProp @Char),
          testProperty "Int" (ioProperty . concreteEvaluateSymOkProp @Int),
          testProperty "Int8" (ioProperty . concreteEvaluateSymOkProp @Int8),
          testProperty "Int16" (ioProperty . concreteEvaluateSymOkProp @Int16),
          testProperty "Int32" (ioProperty . concreteEvaluateSymOkProp @Int32),
          testProperty "Int64" (ioProperty . concreteEvaluateSymOkProp @Int64),
          testProperty "Word" (ioProperty . concreteEvaluateSymOkProp @Word),
          testProperty "Word8" (ioProperty . concreteEvaluateSymOkProp @Word8),
          testProperty "Word16" (ioProperty . concreteEvaluateSymOkProp @Word16),
          testProperty "Word32" (ioProperty . concreteEvaluateSymOkProp @Word32),
          testProperty "Word64" (ioProperty . concreteEvaluateSymOkProp @Word64),
          testGroup
            "List"
            [ testProperty "[Integer]" (ioProperty . concreteEvaluateSymOkProp @[Integer]),
              let model =
                    M.fromList
                      [ (SSymbol "a", True),
                        (SSymbol "b", False)
                      ] ::
                      M.HashMap Symbol Bool
               in testGroup
                    "[SymBool]"
                    [ testGroup
                        "No fill default"
                        [ testCase "Empty list" $
                            evaluateSym False model ([] :: [SBool]) @=? [],
                          testCase "Non-empty list" $
                            evaluateSym False model [SSBool "a", SSBool "b", SSBool "c"] @=? [CBool True, CBool False, SSBool "c"]
                        ],
                      testGroup
                        "Fill default"
                        [ testCase "Empty list" $
                            evaluateSym True model ([] :: [SBool]) @=? [],
                          testCase "Non-empty list" $
                            evaluateSym True model [SSBool "a", SSBool "b", SSBool "c"] @=? [CBool True, CBool False, CBool False]
                        ]
                    ]
            ],
          testGroup
            "Maybe"
            [ testProperty "Maybe Integer" (ioProperty . concreteEvaluateSymOkProp @(Maybe Integer)),
              let model = M.fromList [(SSymbol "a", True)] :: M.HashMap Symbol Bool
               in testGroup
                    "Maybe SymBool"
                    [ testGroup
                        "No fill default"
                        [ testCase "Nothing" $
                            evaluateSym False model (Nothing :: Maybe SBool) @=? Nothing,
                          testCase "Just v when v is in the model" $
                            evaluateSym False model (Just (SSBool "a")) @=? Just (CBool True),
                          testCase "Just v when v is not in the model" $
                            evaluateSym False model (Just (SSBool "b")) @=? Just (SSBool "b")
                        ],
                      testGroup
                        "Fill default"
                        [ testCase "Nothing" $
                            evaluateSym True model (Nothing :: Maybe SBool) @=? Nothing,
                          testCase "Just v when v is in the model" $
                            evaluateSym True model (Just (SSBool "a")) @=? Just (CBool True),
                          testCase "Just v when v is not in the model" $
                            evaluateSym True model (Just (SSBool "b")) @=? Just (CBool False)
                        ]
                    ]
            ],
          testGroup
            "Either"
            [ testProperty "Either Integer Integer" (ioProperty . concreteEvaluateSymOkProp @(Either Integer Integer)),
              let model = M.fromList [(SSymbol "a", True)] :: M.HashMap Symbol Bool
               in testGroup
                    "Either SBool SBool"
                    [ testGroup
                        "No fill default"
                        [ testCase "Left v when v is in the model" $
                            evaluateSym False model (Left (SSBool "a") :: Either SBool SBool) @=? Left (CBool True),
                          testCase "Left v when v is not in the model" $
                            evaluateSym False model (Left (SSBool "b") :: Either SBool SBool) @=? Left (SSBool "b"),
                          testCase "Right v when v is in the model" $
                            evaluateSym False model (Right (SSBool "a") :: Either SBool SBool) @=? Right (CBool True),
                          testCase "Right v when v is not in the model" $
                            evaluateSym False model (Right (SSBool "b") :: Either SBool SBool) @=? Right (SSBool "b")
                        ],
                      testGroup
                        "Fill default"
                        [ testCase "Left v when v is in the model" $
                            evaluateSym True model (Left (SSBool "a") :: Either SBool SBool) @=? Left (CBool True),
                          testCase "Left v when v is not in the model" $
                            evaluateSym True model (Left (SSBool "b") :: Either SBool SBool) @=? Left (CBool False),
                          testCase "Right v when v is in the model" $
                            evaluateSym True model (Right (SSBool "a") :: Either SBool SBool) @=? Right (CBool True),
                          testCase "Right v when v is not in the model" $
                            evaluateSym True model (Right (SSBool "b") :: Either SBool SBool) @=? Right (CBool False)
                        ]
                    ]
            ],
          testGroup
            "MaybeT"
            [ testProperty "MaybeT Maybe Integer" (ioProperty . concreteEvaluateSymOkProp @(MaybeT Maybe Integer) . MaybeT),
              let model = M.fromList [(SSymbol "a", True)] :: M.HashMap Symbol Bool
               in testGroup
                    "MaybeT should work"
                    [ testGroup
                        "No fill default"
                        [ testCase "MaybeT Nothing" $
                            evaluateSym False model (MaybeT Nothing :: MaybeT Maybe SBool) @=? MaybeT Nothing,
                          testCase "MaybeT (Just Nothing)" $
                            evaluateSym False model (MaybeT $ Just Nothing :: MaybeT Maybe SBool) @=? MaybeT (Just Nothing),
                          testCase "MaybeT (Just v) when v is in the model" $
                            evaluateSym False model (MaybeT $ Just $ Just $ SSBool "a") @=? MaybeT (Just (Just (CBool True))),
                          testCase "MaybeT (Just v) when v is not in the model" $
                            evaluateSym False model (MaybeT $ Just $ Just $ SSBool "b") @=? MaybeT (Just (Just (SSBool "b")))
                        ],
                      testGroup
                        "Fill default"
                        [ testCase "MaybeT Nothing" $
                            evaluateSym True model (MaybeT Nothing :: MaybeT Maybe SBool) @=? MaybeT Nothing,
                          testCase "MaybeT (Just Nothing)" $
                            evaluateSym True model (MaybeT $ Just Nothing :: MaybeT Maybe SBool) @=? MaybeT (Just Nothing),
                          testCase "MaybeT (Just v) when v is in the model" $
                            evaluateSym True model (MaybeT $ Just $ Just $ SSBool "a") @=? MaybeT (Just (Just (CBool True))),
                          testCase "MaybeT (Just v) when v is not in the model" $
                            evaluateSym True model (MaybeT $ Just $ Just $ SSBool "b") @=? MaybeT (Just (Just (CBool False)))
                        ]
                    ]
            ],
          testGroup
            "ExceptT"
            [ testProperty "ExceptT Integer Maybe Integer" (ioProperty . concreteEvaluateSymOkProp @(ExceptT Integer Maybe Integer) . ExceptT),
              let model = M.fromList [(SSymbol "a", True)] :: M.HashMap Symbol Bool
               in testGroup
                    "ExceptT SBool Maybe SBool"
                    [ testGroup
                        "No fill default"
                        [ testCase "ExceptT Nothing" $
                            evaluateSym False model (ExceptT Nothing :: ExceptT SBool Maybe SBool) @=? ExceptT Nothing,
                          testCase "ExceptT (Just (Left v)) when v is in the model" $
                            evaluateSym False model (ExceptT $ Just $ Left $ SSBool "a" :: ExceptT SBool Maybe SBool)
                              @=? ExceptT (Just $ Left $ CBool True),
                          testCase "ExceptT (Just (Left v)) when v is not in the model" $
                            evaluateSym False model (ExceptT $ Just $ Left $ SSBool "b" :: ExceptT SBool Maybe SBool)
                              @=? ExceptT (Just $ Left $ SSBool "b"),
                          testCase "ExceptT (Just (Right v)) when v is in the model" $
                            evaluateSym False model (ExceptT $ Just $ Right $ SSBool "a" :: ExceptT SBool Maybe SBool)
                              @=? ExceptT (Just $ Right $ CBool True),
                          testCase "ExceptT (Just (Right v)) when v is not in the model" $
                            evaluateSym False model (ExceptT $ Just $ Right $ SSBool "b" :: ExceptT SBool Maybe SBool)
                              @=? ExceptT (Just $ Right $ SSBool "b")
                        ],
                      testGroup
                        "Fill default"
                        [ testCase "ExceptT Nothing" $
                            evaluateSym True model (ExceptT Nothing :: ExceptT SBool Maybe SBool) @=? ExceptT Nothing,
                          testCase "ExceptT (Just (Left v)) when v is in the model" $
                            evaluateSym True model (ExceptT $ Just $ Left $ SSBool "a" :: ExceptT SBool Maybe SBool)
                              @=? ExceptT (Just $ Left $ CBool True),
                          testCase "ExceptT (Just (Left v)) when v is not in the model" $
                            evaluateSym True model (ExceptT $ Just $ Left $ SSBool "b" :: ExceptT SBool Maybe SBool)
                              @=? ExceptT (Just $ Left $ CBool False),
                          testCase "ExceptT (Just (Right v)) when v is in the model" $
                            evaluateSym True model (ExceptT $ Just $ Right $ SSBool "a" :: ExceptT SBool Maybe SBool)
                              @=? ExceptT (Just $ Right $ CBool True),
                          testCase "ExceptT (Just (Right v)) when v is not in the model" $
                            evaluateSym True model (ExceptT $ Just $ Right $ SSBool "b" :: ExceptT SBool Maybe SBool)
                              @=? ExceptT (Just $ Right $ CBool False)
                        ]
                    ]
            ],
          testProperty "()" (ioProperty . concreteEvaluateSymOkProp @()),
          testGroup
            "(,)"
            [ testProperty "(Integer, Integer)" (ioProperty . concreteEvaluateSymOkProp @(Integer, Integer)),
              let model = M.fromList [(SSymbol "a", True)] :: M.HashMap Symbol Bool
               in testGroup
                    "(SBool, SBool)"
                    [ testCase "No fill default" $
                        evaluateSym False model (SSBool "a", SSBool "b") @=? (CBool True, SSBool "b"),
                      testCase "Fill default" $
                        evaluateSym True model (SSBool "a", SSBool "b") @=? (CBool True, CBool False)
                    ]
            ],
          testGroup
            "(,,)"
            [ testProperty "(Integer, Integer, Integer)" (ioProperty . concreteEvaluateSymOkProp @(Integer, Integer, Integer)),
              let model = M.fromList [(SSymbol "a", True)] :: M.HashMap Symbol Bool
               in testGroup
                    "(SBool, SBool, SBool)"
                    [ testCase "No fill default" $
                        evaluateSym False model (SSBool "a", SSBool "b", SSBool "c") @=? (CBool True, SSBool "b", SSBool "c"),
                      testCase "Fill default" $
                        evaluateSym True model (SSBool "a", SSBool "b", SSBool "c") @=? (CBool True, CBool False, CBool False)
                    ]
            ],
          testGroup
            "(,,,)"
            [ testProperty "(Integer, Integer, Integer, Integer)" (ioProperty . concreteEvaluateSymOkProp @(Integer, Integer, Integer, Integer)),
              let model = M.fromList [(SSymbol "a", True)] :: M.HashMap Symbol Bool
               in testGroup
                    "(SBool, SBool, SBool, SBool)"
                    [ testCase "No fill default" $
                        evaluateSym False model (SSBool "a", SSBool "b", SSBool "c", SSBool "d")
                          @=? (CBool True, SSBool "b", SSBool "c", SSBool "d"),
                      testCase "Fill default" $
                        evaluateSym True model (SSBool "a", SSBool "b", SSBool "c", SSBool "d")
                          @=? (CBool True, CBool False, CBool False, CBool False)
                    ]
            ],
          testGroup
            "(,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer)"
                (ioProperty . concreteEvaluateSymOkProp @(Integer, Integer, Integer, Integer, Integer)),
              let model = M.fromList [(SSymbol "a", True)] :: M.HashMap Symbol Bool
               in testGroup
                    "(SBool, SBool, SBool, SBool, SBool)"
                    [ testCase "No fill default" $
                        evaluateSym False model (SSBool "a", SSBool "b", SSBool "c", SSBool "d", SSBool "e")
                          @=? (CBool True, SSBool "b", SSBool "c", SSBool "d", SSBool "e"),
                      testCase "Fill default" $
                        evaluateSym True model (SSBool "a", SSBool "b", SSBool "c", SSBool "d", SSBool "e")
                          @=? (CBool True, CBool False, CBool False, CBool False, CBool False)
                    ]
            ],
          testGroup
            "(,,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer, Integer)"
                (ioProperty . concreteEvaluateSymOkProp @(Integer, Integer, Integer, Integer, Integer, Integer)),
              let model = M.fromList [(SSymbol "a", True)] :: M.HashMap Symbol Bool
               in testGroup
                    "(SBool, SBool, SBool, SBool, SBool, SBool)"
                    [ testCase "No fill default" $
                        evaluateSym False model (SSBool "a", SSBool "b", SSBool "c", SSBool "d", SSBool "e", SSBool "f")
                          @=? (CBool True, SSBool "b", SSBool "c", SSBool "d", SSBool "e", SSBool "f"),
                      testCase "Fill default" $
                        evaluateSym True model (SSBool "a", SSBool "b", SSBool "c", SSBool "d", SSBool "e", SSBool "f")
                          @=? (CBool True, CBool False, CBool False, CBool False, CBool False, CBool False)
                    ]
            ],
          testGroup
            "(,,,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer, Integer, Integer)"
                (ioProperty . concreteEvaluateSymOkProp @(Integer, Integer, Integer, Integer, Integer, Integer, Integer)),
              let model = M.fromList [(SSymbol "a", True)] :: M.HashMap Symbol Bool
               in testGroup
                    "(SBool, SBool, SBool, SBool, SBool, SBool, SBool)"
                    [ testCase "No fill default" $
                        evaluateSym
                          False
                          model
                          (SSBool "a", SSBool "b", SSBool "c", SSBool "d", SSBool "e", SSBool "f", SSBool "g")
                          @=? (CBool True, SSBool "b", SSBool "c", SSBool "d", SSBool "e", SSBool "f", SSBool "g"),
                      testCase "Fill default" $
                        evaluateSym
                          True
                          model
                          (SSBool "a", SSBool "b", SSBool "c", SSBool "d", SSBool "e", SSBool "f", SSBool "h")
                          @=? (CBool True, CBool False, CBool False, CBool False, CBool False, CBool False, CBool False)
                    ]
            ],
          testGroup
            "(,,,,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer)"
                (ioProperty . concreteEvaluateSymOkProp @(Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer)),
              let model = M.fromList [(SSymbol "a", True)] :: M.HashMap Symbol Bool
               in testGroup
                    "(SBool, SBool, SBool, SBool, SBool, SBool, SBool, SBool) should work"
                    [ testCase "No fill default" $
                        evaluateSym
                          False
                          model
                          (SSBool "a", SSBool "b", SSBool "c", SSBool "d", SSBool "e", SSBool "f", SSBool "g", SSBool "h")
                          @=? (CBool True, SSBool "b", SSBool "c", SSBool "d", SSBool "e", SSBool "f", SSBool "g", SSBool "h"),
                      testCase "Fill default" $
                        evaluateSym
                          True
                          model
                          (SSBool "a", SSBool "b", SSBool "c", SSBool "d", SSBool "e", SSBool "f", SSBool "h", SSBool "h")
                          @=? (CBool True, CBool False, CBool False, CBool False, CBool False, CBool False, CBool False, CBool False)
                    ]
            ],
          let model = M.fromList [(SSymbol "a", True)] :: M.HashMap Symbol Bool
           in testGroup
                "ByteString should work"
                [ testGroup
                    "No fill default"
                    [ testCase "\"\"" $
                        evaluateSym False model ("" :: B.ByteString) @=? "",
                      testCase "\"a\"" $
                        evaluateSym False model ("a" :: B.ByteString) @=? "a"
                    ],
                  testGroup
                    "Fill default"
                    [ testCase "\"\"" $
                        evaluateSym True model ("" :: B.ByteString) @=? "",
                      testCase "\"a\"" $
                        evaluateSym True model ("a" :: B.ByteString) @=? "a"
                    ]
                ],
          testGroup
            "Sum"
            [ testProperty
                "Sum Maybe Maybe Integer"
                ( ioProperty . \(x :: Either (Maybe Integer) (Maybe Integer)) -> case x of
                    Left val -> concreteEvaluateSymOkProp @(Sum Maybe Maybe Integer) $ InL val
                    Right val -> concreteEvaluateSymOkProp @(Sum Maybe Maybe Integer) $ InR val
                ),
              let model = M.fromList [(SSymbol "a", True)] :: M.HashMap Symbol Bool
               in testGroup
                    "Sum Maybe Maybe SBool"
                    [ testGroup
                        "No fill default"
                        [ testCase "InL Nothing" $
                            evaluateSym False model (InL Nothing :: Sum Maybe Maybe SBool) @=? InL Nothing,
                          testCase "InR Nothing" $
                            evaluateSym False model (InR Nothing :: Sum Maybe Maybe SBool) @=? InR Nothing,
                          testCase "InL (Just v) when v is in the model" $
                            evaluateSym False model (InL (Just $ SSBool "a") :: Sum Maybe Maybe SBool) @=? InL (Just $ CBool True),
                          testCase "InL (Just v) when v is not in the model" $
                            evaluateSym False model (InL (Just $ SSBool "b") :: Sum Maybe Maybe SBool) @=? InL (Just $ SSBool "b"),
                          testCase "InR (Just v) when v is in the model" $
                            evaluateSym False model (InR (Just $ SSBool "a") :: Sum Maybe Maybe SBool) @=? InR (Just $ CBool True),
                          testCase "InR (Just v) when v is not in the model" $
                            evaluateSym False model (InR (Just $ SSBool "b") :: Sum Maybe Maybe SBool) @=? InR (Just $ SSBool "b")
                        ],
                      testGroup
                        "Fill default"
                        [ testCase "InL Nothing" $
                            evaluateSym True model (InL Nothing :: Sum Maybe Maybe SBool) @=? InL Nothing,
                          testCase "InR Nothing" $
                            evaluateSym True model (InR Nothing :: Sum Maybe Maybe SBool) @=? InR Nothing,
                          testCase "InL (Just v) when v is in the model" $
                            evaluateSym True model (InL (Just $ SSBool "a") :: Sum Maybe Maybe SBool) @=? InL (Just $ CBool True),
                          testCase "InL (Just v) when v is not in the model" $
                            evaluateSym True model (InL (Just $ SSBool "b") :: Sum Maybe Maybe SBool) @=? InL (Just $ CBool False),
                          testCase "InR (Just v) when v is in the model" $
                            evaluateSym True model (InR (Just $ SSBool "a") :: Sum Maybe Maybe SBool) @=? InR (Just $ CBool True),
                          testCase "InR (Just v) when v is not in the model" $
                            evaluateSym True model (InR (Just $ SSBool "b") :: Sum Maybe Maybe SBool) @=? InR (Just $ CBool False)
                        ]
                    ]
            ],
          testGroup
            "WriterT"
            [ testGroup
                "Lazy"
                [ testProperty
                    "WriterT Integer (Either Integer) Integer"
                    (ioProperty . \(x :: Either Integer (Integer, Integer)) -> concreteEvaluateSymOkProp (WriterLazy.WriterT x)),
                  let model = M.fromList [(SSymbol "a", True)] :: M.HashMap Symbol Bool
                   in testGroup
                        "WriterT SBool (Either SBool) SBool"
                        [ testGroup
                            "No fill default"
                            [ testCase "WriterT (Left v) when v is in the model" $
                                evaluateSym False model (WriterLazy.WriterT $ Left $ SSBool "a" :: WriterLazy.WriterT SBool (Either SBool) SBool)
                                  @=? WriterLazy.WriterT (Left $ CBool True),
                              testCase "WriterT (Left v) when v is not in the model" $
                                evaluateSym False model (WriterLazy.WriterT $ Left $ SSBool "b" :: WriterLazy.WriterT SBool (Either SBool) SBool)
                                  @=? WriterLazy.WriterT (Left $ SSBool "b"),
                              testCase "WriterT (Right (v1, v2))" $
                                evaluateSym False model (WriterLazy.WriterT $ Right (SSBool "a", SSBool "b") :: WriterLazy.WriterT SBool (Either SBool) SBool)
                                  @=? WriterLazy.WriterT (Right (CBool True, SSBool "b"))
                            ],
                          testGroup
                            "Fill default"
                            [ testCase "WriterT (Left v) when v is in the model" $
                                evaluateSym True model (WriterLazy.WriterT $ Left $ SSBool "a" :: WriterLazy.WriterT SBool (Either SBool) SBool)
                                  @=? WriterLazy.WriterT (Left $ CBool True),
                              testCase "WriterT (Left v) when v is not in the model" $
                                evaluateSym True model (WriterLazy.WriterT $ Left $ SSBool "b" :: WriterLazy.WriterT SBool (Either SBool) SBool)
                                  @=? WriterLazy.WriterT (Left $ CBool False),
                              testCase "WriterT (Right (v1, v2))" $
                                evaluateSym True model (WriterLazy.WriterT $ Right (SSBool "a", SSBool "b") :: WriterLazy.WriterT SBool (Either SBool) SBool)
                                  @=? WriterLazy.WriterT (Right (CBool True, CBool False))
                            ]
                        ]
                ],
              testGroup
                "Strict"
                [ testProperty
                    "WriterT Integer (Either Integer) Integer"
                    (ioProperty . \(x :: Either Integer (Integer, Integer)) -> concreteEvaluateSymOkProp (WriterStrict.WriterT x)),
                  let model = M.fromList [(SSymbol "a", True)] :: M.HashMap Symbol Bool
                   in testGroup
                        "WriterT SBool (Either SBool) SBool"
                        [ testGroup
                            "No fill default"
                            [ testCase "WriterT (Left v) when v is in the model" $
                                evaluateSym False model (WriterStrict.WriterT $ Left $ SSBool "a" :: WriterStrict.WriterT SBool (Either SBool) SBool)
                                  @=? WriterStrict.WriterT (Left $ CBool True),
                              testCase "WriterT (Left v) when v is not in the model" $
                                evaluateSym False model (WriterStrict.WriterT $ Left $ SSBool "b" :: WriterStrict.WriterT SBool (Either SBool) SBool)
                                  @=? WriterStrict.WriterT (Left $ SSBool "b"),
                              testCase "WriterT (Right (v1, v2))" $
                                evaluateSym False model (WriterStrict.WriterT $ Right (SSBool "a", SSBool "b") :: WriterStrict.WriterT SBool (Either SBool) SBool)
                                  @=? WriterStrict.WriterT (Right (CBool True, SSBool "b"))
                            ],
                          testGroup
                            "Fill default"
                            [ testCase "WriterT (Left v) when v is in the model" $
                                evaluateSym True model (WriterStrict.WriterT $ Left $ SSBool "a" :: WriterStrict.WriterT SBool (Either SBool) SBool)
                                  @=? WriterStrict.WriterT (Left $ CBool True),
                              testCase "WriterT (Left v) when v is not in the model" $
                                evaluateSym True model (WriterStrict.WriterT $ Left $ SSBool "b" :: WriterStrict.WriterT SBool (Either SBool) SBool)
                                  @=? WriterStrict.WriterT (Left $ CBool False),
                              testCase "WriterT (Right (v1, v2))" $
                                evaluateSym True model (WriterStrict.WriterT $ Right (SSBool "a", SSBool "b") :: WriterStrict.WriterT SBool (Either SBool) SBool)
                                  @=? WriterStrict.WriterT (Right (CBool True, CBool False))
                            ]
                        ]
                ]
            ],
          testGroup
            "Identity"
            [ testProperty
                "Identity Integer"
                (ioProperty . \(x :: Integer) -> concreteEvaluateSymOkProp $ Identity x),
              let model = M.fromList [(SSymbol "a", True)] :: M.HashMap Symbol Bool
               in testGroup
                    "Identity SBool"
                    [ testGroup
                        "No fill default"
                        [ testCase "Identity v when v is in the model" $
                            evaluateSym False model (Identity $ SSBool "a") @=? Identity (CBool True),
                          testCase "Identity v when v is not in the model" $
                            evaluateSym False model (Identity $ SSBool "b") @=? Identity (SSBool "b")
                        ],
                      testGroup
                        "Fill default"
                        [ testCase "Identity v when v is in the model" $
                            evaluateSym True model (Identity $ SSBool "a") @=? Identity (CBool True),
                          testCase "Identity v when v is not in the model" $
                            evaluateSym True model (Identity $ SSBool "b") @=? Identity (CBool False)
                        ]
                    ]
            ],
          testGroup
            "IdentityT"
            [ testProperty
                "IdentityT (Either Integer) Integer"
                (ioProperty . \(x :: Either Integer Integer) -> concreteEvaluateSymOkProp $ IdentityT x),
              let model = M.fromList [(SSymbol "a", True)] :: M.HashMap Symbol Bool
               in testGroup
                    "IdentityT (Either SBool) SBool"
                    [ testGroup
                        "No fill default"
                        [ testCase "IdentityT (Left v) when v is in the model" $
                            evaluateSym False model (IdentityT $ Left $ SSBool "a" :: IdentityT (Either SBool) SBool)
                              @=? IdentityT (Left $ CBool True),
                          testCase "IdentityT (Left v) when v is not in the model" $
                            evaluateSym False model (IdentityT $ Left $ SSBool "b" :: IdentityT (Either SBool) SBool)
                              @=? IdentityT (Left $ SSBool "b"),
                          testCase "IdentityT (Right v) when v is in the model" $
                            evaluateSym False model (IdentityT $ Right $ SSBool "a" :: IdentityT (Either SBool) SBool)
                              @=? IdentityT (Right $ CBool True),
                          testCase "IdentityT (Right v) when v is not in the model" $
                            evaluateSym False model (IdentityT $ Right $ SSBool "b" :: IdentityT (Either SBool) SBool)
                              @=? IdentityT (Right $ SSBool "b")
                        ],
                      testGroup
                        "Fill default"
                        [ testCase "IdentityT (Left v) when v is in the model" $
                            evaluateSym True model (IdentityT $ Left $ SSBool "a" :: IdentityT (Either SBool) SBool)
                              @=? IdentityT (Left $ CBool True),
                          testCase "IdentityT (Left v) when v is not in the model" $
                            evaluateSym True model (IdentityT $ Left $ SSBool "b" :: IdentityT (Either SBool) SBool)
                              @=? IdentityT (Left $ CBool False),
                          testCase "IdentityT (Right v) when v is in the model" $
                            evaluateSym True model (IdentityT $ Right $ SSBool "a" :: IdentityT (Either SBool) SBool)
                              @=? IdentityT (Right $ CBool True),
                          testCase "IdentityT (Right v) when v is not in the model" $
                            evaluateSym True model (IdentityT $ Right $ SSBool "b" :: IdentityT (Either SBool) SBool)
                              @=? IdentityT (Right $ CBool False)
                        ]
                    ]
            ]
        ]
    ]
