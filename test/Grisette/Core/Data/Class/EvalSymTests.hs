{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Core.Data.Class.EvalSymTests (evalSymTests) where

import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Identity
  ( Identity (Identity),
    IdentityT (IdentityT),
  )
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.ByteString as B
import Data.Functor.Sum (Sum (InL, InR))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Stack (HasCallStack)
import Grisette
  ( AsKey (AsKey),
    EvalSym (evalSym),
    ITEOp (symIte),
    LogicalOp (symNot, (.&&), (.||)),
    ModelOps (emptyModel),
    ModelRep (buildModel),
    ModelValuePair ((::=)),
    Solvable (con, isym, ssym),
    SymBool,
    SymEq ((.==)),
    SymInteger,
    Symbol (IndexedSymbol),
    solve,
    typedAnySymbol,
    z3,
  )
import Test.Framework (Test, TestOptions' (topt_timeout), plusTestOptions, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (Assertion, (@?=))
import Test.QuickCheck (ioProperty)

concreteEvalSymOkProp ::
  (HasCallStack, EvalSym a, Show a, Eq a) => a -> Assertion
concreteEvalSymOkProp x = evalSym True emptyModel x @?= x

evalSymTests :: Test
evalSymTests =
  testGroup
    "EvalSym"
    [ plusTestOptions (mempty {topt_timeout = Just (Just 1000000)}) $
        testCase "proper memo" $ do
          let pair = ("a" :: AsKey SymInteger, "b" :: AsKey SymInteger)
          let iter (x, y) = (y, x + y)
          let r = iterate iter pair !! 100
          Right m <- solve z3 $ snd r .== 0
          evalSym True m (snd r) @?= 0,
      testGroup
        "EvalSym for common types"
        [ testGroup
            "SymBool"
            [ let model = emptyModel
                  eval :: AsKey SymBool -> AsKey SymBool
                  eval = evalSym False model
               in testGroup
                    "Empty model / no fill default"
                    [ testCase "con" $
                        eval (con True) @?= con True,
                      testCase "ssym" $
                        eval (ssym "a") @?= ssym "a",
                      testCase "isym" $
                        eval (isym "a" 1) @?= isym "a" 1,
                      testCase ".||" $
                        eval (ssym "a" .|| ssym "b")
                          @?= ssym "a"
                          .|| ssym "b",
                      testCase ".&&" $
                        eval (ssym "a" .&& ssym "b")
                          @?= ssym "a"
                          .&& ssym "b",
                      testCase ".==" $
                        eval (AsKey $ (ssym "a" :: SymBool) .== ssym "b")
                          @?= AsKey ((ssym "a" :: SymBool) .== ssym "b"),
                      testCase "symNot" $
                        eval (symNot (ssym "a"))
                          @?= symNot (ssym "a"),
                      testCase "symIte" $
                        eval (symIte (ssym "a") (ssym "b") (ssym "c"))
                          @?= symIte (ssym "a") (ssym "b") (ssym "c")
                    ],
              let model = emptyModel
                  eval :: AsKey SymBool -> AsKey SymBool
                  eval = evalSym True model
               in testGroup
                    "Empty model / with fill default"
                    [ testCase "con" $
                        eval (con True) @?= con True,
                      testCase "ssym" $
                        eval (ssym "a") @?= con False,
                      testCase "isym" $
                        eval (isym "a" 1) @?= con False,
                      testCase ".||" $
                        eval (ssym "a" .|| ssym "b") @?= con False,
                      testCase ".&&" $
                        eval (ssym "a" .&& ssym "b") @?= con False,
                      testCase ".==" $
                        eval (AsKey $ (ssym "a" :: SymBool) .== ssym "b") @?= con True,
                      testCase "symNot" $
                        eval (symNot (ssym "a")) @?= con True,
                      testCase "symIte" $
                        eval (symIte (ssym "a") (ssym "b") (ssym "c"))
                          @?= con False
                    ],
              let model =
                    buildModel
                      ( "a" ::= True,
                        typedAnySymbol (IndexedSymbol "a" 1) ::= False,
                        "b" ::= False,
                        "c" ::= True
                      )
                  eval :: AsKey SymBool -> AsKey SymBool
                  eval = evalSym True model
               in testGroup
                    "Some model"
                    [ testCase "con" $
                        eval (con True) @?= con True,
                      testCase "ssym" $
                        eval (ssym "a") @?= con True,
                      testCase "isym" $
                        eval (isym "a" 1) @?= con False,
                      testCase ".||" $
                        eval (ssym "a" .|| ssym "b") @?= con True,
                      testCase ".&&" $
                        eval (ssym "a" .&& ssym "b") @?= con False,
                      testCase ".==" $
                        eval (AsKey $ (ssym "a" :: SymBool) .== ssym "b") @?= con False,
                      testCase "symNot" $
                        eval (symNot (ssym "a")) @?= con False,
                      testCase "symIte" $
                        eval (symIte (ssym "a") (ssym "b") (ssym "c"))
                          @?= con False
                    ]
            ],
          testProperty "Bool" $ ioProperty . concreteEvalSymOkProp @Bool,
          testProperty "Integer" $
            ioProperty . concreteEvalSymOkProp @Integer,
          testProperty "Char" $ ioProperty . concreteEvalSymOkProp @Char,
          testProperty "Int" $ ioProperty . concreteEvalSymOkProp @Int,
          testProperty "Int8" $ ioProperty . concreteEvalSymOkProp @Int8,
          testProperty "Int16" $ ioProperty . concreteEvalSymOkProp @Int16,
          testProperty "Int32" $ ioProperty . concreteEvalSymOkProp @Int32,
          testProperty "Int64" $ ioProperty . concreteEvalSymOkProp @Int64,
          testProperty "Word" $ ioProperty . concreteEvalSymOkProp @Word,
          testProperty "Word8" $ ioProperty . concreteEvalSymOkProp @Word8,
          testProperty "Word16" $
            ioProperty . concreteEvalSymOkProp @Word16,
          testProperty "Word32" $
            ioProperty . concreteEvalSymOkProp @Word32,
          testProperty "Word64" $
            ioProperty . concreteEvalSymOkProp @Word64,
          testGroup
            "List"
            [ testProperty "[Integer]" $
                ioProperty . concreteEvalSymOkProp @[Integer],
              let model =
                    buildModel ("a" ::= True, "b" ::= False)
                  eval :: Bool -> [AsKey SymBool] -> [AsKey SymBool]
                  eval = flip evalSym model
               in testGroup
                    "[SymBool]"
                    [ testGroup
                        "No fill default"
                        [ testCase "Empty list" $
                            eval False [] @?= [],
                          testCase "Non-empty list" $
                            eval False [ssym "a", ssym "b", ssym "c"]
                              @?= [con True, con False, ssym "c"]
                        ],
                      testGroup
                        "Fill default"
                        [ testCase "Empty list" $
                            eval True [] @?= [],
                          testCase "Non-empty list" $
                            eval True [ssym "a", ssym "b", ssym "c"]
                              @?= [con True, con False, con False]
                        ]
                    ]
            ],
          testGroup
            "Maybe"
            [ testProperty "Maybe Integer" $
                ioProperty . concreteEvalSymOkProp @(Maybe Integer),
              let model = buildModel ("a" ::= True)
                  eval :: Bool -> Maybe (AsKey SymBool) -> Maybe (AsKey SymBool)
                  eval = flip evalSym model
               in testGroup
                    "Maybe SymBool"
                    [ testGroup
                        "No fill default"
                        [ testCase "Nothing" $
                            eval False Nothing @?= Nothing,
                          testCase "Just v: v in model" $
                            eval False (Just (ssym "a")) @?= Just (con True),
                          testCase "Just v: v not in model" $
                            eval False (Just (ssym "b")) @?= Just (ssym "b")
                        ],
                      testGroup
                        "Fill default"
                        [ testCase "Nothing" $
                            eval True (Nothing) @?= Nothing,
                          testCase "Just v: v in model" $
                            eval True (Just (ssym "a")) @?= Just (con True),
                          testCase "Just v: v not in model" $
                            eval True (Just (ssym "b")) @?= Just (con False)
                        ]
                    ]
            ],
          testGroup
            "Either"
            [ testProperty "Either Integer Integer" $
                ioProperty
                  . concreteEvalSymOkProp @(Either Integer Integer),
              let model = buildModel ("a" ::= True)
                  eval ::
                    Bool ->
                    Either (AsKey SymBool) (AsKey SymBool) ->
                    Either (AsKey SymBool) (AsKey SymBool)
                  eval = flip evalSym model
               in testGroup
                    "Either SymBool SymBool"
                    [ testGroup
                        "No fill default"
                        [ testCase "Left v: v in model" $
                            eval False (Left (ssym "a")) @?= Left (con True),
                          testCase "Left v: v not in model" $
                            eval False (Left (ssym "b")) @?= Left (ssym "b"),
                          testCase "Right v: v in model" $
                            eval False (Right (ssym "a")) @?= Right (con True),
                          testCase "Right v: v not in model" $
                            eval False (Right (ssym "b")) @?= Right (ssym "b")
                        ],
                      testGroup
                        "Fill default"
                        [ testCase "Left v: v in model" $
                            eval True (Left (ssym "a")) @?= Left (con True),
                          testCase "Left v: v not in model" $
                            eval True (Left (ssym "b")) @?= Left (con False),
                          testCase "Right v: v in model" $
                            eval True (Right (ssym "a")) @?= Right (con True),
                          testCase "Right v: v not in model" $
                            eval True (Right (ssym "b")) @?= Right (con False)
                        ]
                    ]
            ],
          testGroup
            "MaybeT"
            [ testProperty "MaybeT Maybe Integer" $
                ioProperty
                  . concreteEvalSymOkProp @(MaybeT Maybe Integer)
                  . MaybeT,
              let model = buildModel ("a" ::= True)
                  eval ::
                    Bool ->
                    MaybeT Maybe (AsKey SymBool) ->
                    MaybeT Maybe (AsKey SymBool)
                  eval = flip evalSym model
               in testGroup
                    "MaybeT should work"
                    [ testGroup
                        "No fill default"
                        [ testCase "MaybeT Nothing" $
                            eval False (MaybeT Nothing) @?= MaybeT Nothing,
                          testCase "MaybeT (Just Nothing)" $
                            eval False (MaybeT $ Just Nothing)
                              @?= MaybeT (Just Nothing),
                          testCase "MaybeT (Just v): v in model" $
                            eval False (MaybeT $ Just $ Just (ssym "a"))
                              @?= MaybeT (Just (Just (con True))),
                          testCase "MaybeT (Just v): v not in model" $
                            eval False (MaybeT $ Just $ Just (ssym "b"))
                              @?= MaybeT (Just (Just (ssym "b")))
                        ],
                      testGroup
                        "Fill default"
                        [ testCase "MaybeT Nothing" $
                            eval True (MaybeT Nothing) @?= MaybeT Nothing,
                          testCase "MaybeT (Just Nothing)" $
                            eval True (MaybeT $ Just Nothing)
                              @?= MaybeT (Just Nothing),
                          testCase "MaybeT (Just v): v in model" $
                            eval True (MaybeT $ Just $ Just (ssym "a"))
                              @?= MaybeT (Just (Just (con True))),
                          testCase "MaybeT (Just v): v not in model" $
                            eval True (MaybeT $ Just $ Just (ssym "b"))
                              @?= MaybeT (Just (Just (con False)))
                        ]
                    ]
            ],
          testGroup
            "ExceptT"
            [ testProperty "ExceptT Integer Maybe Integer" $
                ioProperty
                  . concreteEvalSymOkProp @(ExceptT Integer Maybe Integer)
                  . ExceptT,
              let model = buildModel ("a" ::= True)
                  eval ::
                    Bool ->
                    ExceptT (AsKey SymBool) Maybe (AsKey SymBool) ->
                    ExceptT (AsKey SymBool) Maybe (AsKey SymBool)
                  eval = flip evalSym model
               in testGroup
                    "ExceptT SymBool Maybe SymBool"
                    [ testGroup
                        "No fill default"
                        [ testCase "ExceptT Nothing" $
                            eval False (ExceptT Nothing) @?= ExceptT Nothing,
                          testCase "ExceptT (Just (Left v)): v in model" $
                            eval False (ExceptT $ Just $ Left $ ssym "a")
                              @?= ExceptT (Just $ Left $ con True),
                          testCase "ExceptT (Just (Left v)): v not in model" $
                            eval False (ExceptT $ Just $ Left $ ssym "b")
                              @?= ExceptT (Just $ Left $ ssym "b"),
                          testCase "ExceptT (Just (Right v)): v in model" $
                            eval False (ExceptT $ Just $ Right $ ssym "a")
                              @?= ExceptT (Just $ Right $ con True),
                          testCase "ExceptT (Just (Right v)): v not in model" $
                            eval False (ExceptT $ Just $ Right $ ssym "b")
                              @?= ExceptT (Just $ Right $ ssym "b")
                        ],
                      testGroup
                        "Fill default"
                        [ testCase "ExceptT Nothing" $
                            eval True (ExceptT Nothing) @?= ExceptT Nothing,
                          testCase "ExceptT (Just (Left v)): v in model" $
                            eval True (ExceptT $ Just $ Left $ ssym "a")
                              @?= ExceptT (Just $ Left $ con True),
                          testCase "ExceptT (Just (Left v)): v not in model" $
                            eval True (ExceptT $ Just $ Left $ ssym "b")
                              @?= ExceptT (Just $ Left $ con False),
                          testCase "ExceptT (Just (Right v)): v in model" $
                            eval True (ExceptT $ Just $ Right $ ssym "a")
                              @?= ExceptT (Just $ Right $ con True),
                          testCase "ExceptT (Just (Right v)): v not in model" $
                            eval True (ExceptT $ Just $ Right $ ssym "b")
                              @?= ExceptT (Just $ Right $ con False)
                        ]
                    ]
            ],
          testProperty "()" (ioProperty . concreteEvalSymOkProp @()),
          testGroup
            "(,)"
            [ testProperty "(Integer, Integer)" $
                ioProperty . concreteEvalSymOkProp @(Integer, Integer),
              let model = buildModel ("a" ::= True)
                  eval ::
                    Bool ->
                    (AsKey SymBool, AsKey SymBool) ->
                    (AsKey SymBool, AsKey SymBool)
                  eval = flip evalSym model
               in testGroup
                    "(SymBool, SymBool)"
                    [ testCase "No fill default" $
                        eval False (ssym "a", ssym "b")
                          @?= (con True, ssym "b"),
                      testCase "Fill default" $
                        eval True (ssym "a", ssym "b")
                          @?= (con True, con False)
                    ]
            ],
          testGroup
            "(,,)"
            [ testProperty "(Integer, Integer, Integer)" $
                ioProperty
                  . concreteEvalSymOkProp @(Integer, Integer, Integer),
              let model = buildModel ("a" ::= True)
                  eval ::
                    Bool ->
                    (AsKey SymBool, AsKey SymBool, AsKey SymBool) ->
                    (AsKey SymBool, AsKey SymBool, AsKey SymBool)
                  eval = flip evalSym model
               in testGroup
                    "(SymBool, SymBool, SymBool)"
                    [ testCase "No fill default" $
                        eval False (ssym "a", ssym "b", ssym "c")
                          @?= (con True, ssym "b", ssym "c"),
                      testCase "Fill default" $
                        eval True (ssym "a", ssym "b", ssym "c")
                          @?= (con True, con False, con False)
                    ]
            ],
          testGroup
            "(,,,)"
            [ testProperty "(Integer, Integer, Integer, Integer)" $
                ioProperty
                  . concreteEvalSymOkProp
                    @(Integer, Integer, Integer, Integer),
              let model = buildModel ("a" ::= True)
                  eval ::
                    Bool ->
                    (AsKey SymBool, AsKey SymBool, AsKey SymBool, AsKey SymBool) ->
                    (AsKey SymBool, AsKey SymBool, AsKey SymBool, AsKey SymBool)
                  eval = flip evalSym model
               in testGroup
                    "(SymBool, SymBool, SymBool, SymBool)"
                    [ testCase "No fill default" $
                        eval False (ssym "a", ssym "b", ssym "c", ssym "d")
                          @?= (con True, ssym "b", ssym "c", ssym "d"),
                      testCase "Fill default" $
                        eval True (ssym "a", ssym "b", ssym "c", ssym "d")
                          @?= (con True, con False, con False, con False)
                    ]
            ],
          testGroup
            "(,,,,)"
            [ testProperty "(Integer, Integer, Integer, Integer, Integer)" $
                ioProperty
                  . concreteEvalSymOkProp
                    @(Integer, Integer, Integer, Integer, Integer),
              let model = buildModel ("a" ::= True)
                  eval ::
                    Bool ->
                    ( AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool
                    ) ->
                    ( AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool
                    )
                  eval = flip evalSym model
               in testGroup
                    "(SymBool, SymBool, SymBool, SymBool, SymBool)"
                    [ testCase "No fill default" $
                        eval
                          False
                          (ssym "a", ssym "b", ssym "c", ssym "d", ssym "e")
                          @?= ( con True,
                                ssym "b",
                                ssym "c",
                                ssym "d",
                                ssym "e"
                              ),
                      testCase "Fill default" $
                        eval
                          True
                          (ssym "a", ssym "b", ssym "c", ssym "d", ssym "e")
                          @?= ( con True,
                                con False,
                                con False,
                                con False,
                                con False
                              )
                    ]
            ],
          testGroup
            "(,,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer, Integer)"
                $ ioProperty
                  . concreteEvalSymOkProp
                    @(Integer, Integer, Integer, Integer, Integer, Integer),
              let model = buildModel ("a" ::= True)
                  eval ::
                    Bool ->
                    ( AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool
                    ) ->
                    ( AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool
                    )
                  eval = flip evalSym model
               in testGroup
                    "(SymBool, SymBool, SymBool, SymBool, SymBool, SymBool)"
                    [ testCase "No fill default" $
                        eval
                          False
                          ( ssym "a",
                            ssym "b",
                            ssym "c",
                            ssym "d",
                            ssym "e",
                            ssym "f"
                          )
                          @?= ( con True,
                                ssym "b",
                                ssym "c",
                                ssym "d",
                                ssym "e",
                                ssym "f"
                              ),
                      testCase "Fill default" $
                        eval
                          True
                          ( ssym "a",
                            ssym "b",
                            ssym "c",
                            ssym "d",
                            ssym "e",
                            ssym "f"
                          )
                          @?= ( con True,
                                con False,
                                con False,
                                con False,
                                con False,
                                con False
                              )
                    ]
            ],
          testGroup
            "(,,,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer, Integer, Integer)"
                $ ioProperty
                  . concreteEvalSymOkProp
                    @( Integer,
                       Integer,
                       Integer,
                       Integer,
                       Integer,
                       Integer,
                       Integer
                     ),
              let model = buildModel ("a" ::= True)
                  eval ::
                    Bool ->
                    ( AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool
                    ) ->
                    ( AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool
                    )
                  eval = flip evalSym model
               in testGroup
                    "(SymBool, SymBool, SymBool, SymBool, SymBool, SymBool, SymBool)"
                    [ testCase "No fill default" $
                        eval
                          False
                          ( ssym "a",
                            ssym "b",
                            ssym "c",
                            ssym "d",
                            ssym "e",
                            ssym "f",
                            ssym "g"
                          )
                          @?= ( con True,
                                ssym "b",
                                ssym "c",
                                ssym "d",
                                ssym "e",
                                ssym "f",
                                ssym "g"
                              ),
                      testCase "Fill default" $
                        eval
                          True
                          ( ssym "a",
                            ssym "b",
                            ssym "c",
                            ssym "d",
                            ssym "e",
                            ssym "f",
                            ssym "h"
                          )
                          @?= ( con True,
                                con False,
                                con False,
                                con False,
                                con False,
                                con False,
                                con False
                              )
                    ]
            ],
          testGroup
            "(,,,,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer)"
                $ ioProperty
                  . concreteEvalSymOkProp
                    @( Integer,
                       Integer,
                       Integer,
                       Integer,
                       Integer,
                       Integer,
                       Integer,
                       Integer
                     ),
              let model = buildModel ("a" ::= True)
                  eval ::
                    Bool ->
                    ( AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool
                    ) ->
                    ( AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool,
                      AsKey SymBool
                    )
                  eval = flip evalSym model
               in testGroup
                    "(SymBool, SymBool, SymBool, SymBool, SymBool, SymBool, SymBool, SymBool)"
                    [ testCase "No fill default" $
                        eval
                          False
                          ( ssym "a",
                            ssym "b",
                            ssym "c",
                            ssym "d",
                            ssym "e",
                            ssym "f",
                            ssym "g",
                            ssym "h"
                          )
                          @?= ( con True,
                                ssym "b",
                                ssym "c",
                                ssym "d",
                                ssym "e",
                                ssym "f",
                                ssym "g",
                                ssym "h"
                              ),
                      testCase "Fill default" $
                        eval
                          True
                          ( ssym "a",
                            ssym "b",
                            ssym "c",
                            ssym "d",
                            ssym "e",
                            ssym "f",
                            ssym "h",
                            ssym "h"
                          )
                          @?= ( con True,
                                con False,
                                con False,
                                con False,
                                con False,
                                con False,
                                con False,
                                con False
                              )
                    ]
            ],
          let model = buildModel ("a" ::= True)
              eval :: Bool -> B.ByteString -> B.ByteString
              eval = flip evalSym model
           in testGroup
                "ByteString should work"
                [ testGroup
                    "No fill default"
                    [ testCase "\"\"" $
                        eval False ("" :: B.ByteString) @?= "",
                      testCase "\"a\"" $
                        eval False ("a" :: B.ByteString) @?= "a"
                    ],
                  testGroup
                    "Fill default"
                    [ testCase "\"\"" $
                        eval True ("" :: B.ByteString) @?= "",
                      testCase "\"a\"" $
                        eval True ("a" :: B.ByteString) @?= "a"
                    ]
                ],
          testGroup
            "Sum"
            [ testProperty
                "Sum Maybe Maybe Integer"
                ( ioProperty . \(x :: Either (Maybe Integer) (Maybe Integer)) ->
                    case x of
                      Left val ->
                        concreteEvalSymOkProp
                          @(Sum Maybe Maybe Integer)
                          $ InL val
                      Right val ->
                        concreteEvalSymOkProp
                          @(Sum Maybe Maybe Integer)
                          $ InR val
                ),
              let model = buildModel ("a" ::= True)
                  eval ::
                    Bool ->
                    Sum Maybe Maybe (AsKey SymBool) ->
                    Sum Maybe Maybe (AsKey SymBool)
                  eval = flip evalSym model
               in testGroup
                    "Sum Maybe Maybe SymBool"
                    [ testGroup
                        "No fill default"
                        [ testCase "InL Nothing" $
                            eval False (InL Nothing) @?= InL Nothing,
                          testCase "InR Nothing" $
                            eval False (InR Nothing) @?= InR Nothing,
                          testCase "InL (Just v): v in model" $
                            eval False (InL (Just $ ssym "a"))
                              @?= InL (Just $ con True),
                          testCase "InL (Just v): v not in model" $
                            eval False (InL (Just $ ssym "b"))
                              @?= InL (Just $ ssym "b"),
                          testCase "InR (Just v): v in model" $
                            eval False (InR (Just $ ssym "a"))
                              @?= InR (Just $ con True),
                          testCase "InR (Just v): v not in model" $
                            eval False (InR (Just $ ssym "b"))
                              @?= InR (Just $ ssym "b")
                        ],
                      testGroup
                        "Fill default"
                        [ testCase "InL Nothing" $
                            eval True (InL Nothing) @?= InL Nothing,
                          testCase "InR Nothing" $
                            eval True (InR Nothing) @?= InR Nothing,
                          testCase "InL (Just v): v in model" $
                            eval True (InL (Just $ ssym "a"))
                              @?= InL (Just $ con True),
                          testCase "InL (Just v): v not in model" $
                            eval True (InL (Just $ ssym "b"))
                              @?= InL (Just $ con False),
                          testCase "InR (Just v): v in model" $
                            eval True (InR (Just $ ssym "a"))
                              @?= InR (Just $ con True),
                          testCase "InR (Just v): v not in model" $
                            eval True (InR (Just $ ssym "b"))
                              @?= InR (Just $ con False)
                        ]
                    ]
            ],
          testGroup
            "WriterT"
            [ testGroup
                "Lazy"
                [ testProperty
                    "WriterT Integer (Either Integer) Integer"
                    $ ioProperty . \(x :: Either Integer (Integer, Integer)) ->
                      concreteEvalSymOkProp (WriterLazy.WriterT x),
                  let model = buildModel ("a" ::= True)
                      eval ::
                        Bool ->
                        WriterLazy.WriterT (AsKey SymBool) (Either (AsKey SymBool)) (AsKey SymBool) ->
                        WriterLazy.WriterT (AsKey SymBool) (Either (AsKey SymBool)) (AsKey SymBool)
                      eval = flip evalSym model
                   in testGroup
                        "WriterT SymBool (Either SymBool) SymBool"
                        [ testGroup
                            "No fill default"
                            [ testCase "WriterT (Left v): v in model" $
                                eval
                                  False
                                  (WriterLazy.WriterT $ Left $ ssym "a")
                                  @?= WriterLazy.WriterT (Left $ con True),
                              testCase "WriterT (Left v): v not in model" $
                                eval
                                  False
                                  (WriterLazy.WriterT $ Left $ ssym "b")
                                  @?= WriterLazy.WriterT (Left $ ssym "b"),
                              testCase "WriterT (Right (v1, v2))" $
                                eval
                                  False
                                  ( WriterLazy.WriterT $
                                      Right (ssym "a", ssym "b")
                                  )
                                  @?= WriterLazy.WriterT
                                    (Right (con True, ssym "b"))
                            ],
                          testGroup
                            "Fill default"
                            [ testCase "WriterT (Left v): v in model" $
                                eval True (WriterLazy.WriterT $ Left $ ssym "a")
                                  @?= WriterLazy.WriterT (Left $ con True),
                              testCase "WriterT (Left v): v not in model" $
                                eval True (WriterLazy.WriterT $ Left $ ssym "b")
                                  @?= WriterLazy.WriterT (Left $ con False),
                              testCase "WriterT (Right (v1, v2))" $
                                eval
                                  True
                                  ( WriterLazy.WriterT $
                                      Right (ssym "a", ssym "b")
                                  )
                                  @?= WriterLazy.WriterT
                                    (Right (con True, con False))
                            ]
                        ]
                ],
              testGroup
                "Strict"
                [ testProperty
                    "WriterT Integer (Either Integer) Integer"
                    $ ioProperty
                      . \(x :: Either Integer (Integer, Integer)) ->
                        concreteEvalSymOkProp (WriterStrict.WriterT x),
                  let model = buildModel ("a" ::= True)
                      eval ::
                        Bool ->
                        WriterStrict.WriterT (AsKey SymBool) (Either (AsKey SymBool)) (AsKey SymBool) ->
                        WriterStrict.WriterT (AsKey SymBool) (Either (AsKey SymBool)) (AsKey SymBool)
                      eval = flip evalSym model
                   in testGroup
                        "WriterT SymBool (Either SymBool) SymBool"
                        [ testGroup
                            "No fill default"
                            [ testCase "WriterT (Left v): v in model" $
                                eval
                                  False
                                  (WriterStrict.WriterT $ Left $ ssym "a")
                                  @?= WriterStrict.WriterT (Left $ con True),
                              testCase "WriterT (Left v): v not in model" $
                                eval
                                  False
                                  (WriterStrict.WriterT $ Left $ ssym "b")
                                  @?= WriterStrict.WriterT (Left $ ssym "b"),
                              testCase "WriterT (Right (v1, v2))" $
                                eval
                                  False
                                  ( WriterStrict.WriterT $
                                      Right (ssym "a", ssym "b")
                                  )
                                  @?= WriterStrict.WriterT
                                    (Right (con True, ssym "b"))
                            ],
                          testGroup
                            "Fill default"
                            [ testCase "WriterT (Left v): v in model" $
                                eval
                                  True
                                  (WriterStrict.WriterT $ Left $ ssym "a")
                                  @?= WriterStrict.WriterT (Left $ con True),
                              testCase "WriterT (Left v): v not in model" $
                                eval
                                  True
                                  (WriterStrict.WriterT $ Left $ ssym "b")
                                  @?= WriterStrict.WriterT (Left $ con False),
                              testCase "WriterT (Right (v1, v2))" $
                                eval
                                  True
                                  ( WriterStrict.WriterT $
                                      Right (ssym "a", ssym "b")
                                  )
                                  @?= WriterStrict.WriterT
                                    (Right (con True, con False))
                            ]
                        ]
                ]
            ],
          testGroup
            "Identity"
            [ testProperty
                "Identity Integer"
                $ ioProperty
                  . \(x :: Integer) -> concreteEvalSymOkProp $ Identity x,
              let model = buildModel ("a" ::= True)
                  eval :: Bool -> Identity (AsKey SymBool) -> Identity (AsKey SymBool)
                  eval = flip evalSym model
               in testGroup
                    "Identity SymBool"
                    [ testGroup
                        "No fill default"
                        [ testCase "Identity v: v in model" $
                            eval False (Identity $ ssym "a")
                              @?= Identity (con True),
                          testCase "Identity v: v not in model" $
                            eval False (Identity $ ssym "b")
                              @?= Identity (ssym "b")
                        ],
                      testGroup
                        "Fill default"
                        [ testCase "Identity v: v in model" $
                            eval True (Identity $ ssym "a")
                              @?= Identity (con True),
                          testCase "Identity v: v not in model" $
                            eval True (Identity $ ssym "b")
                              @?= Identity (con False)
                        ]
                    ]
            ],
          testGroup
            "IdentityT"
            [ testProperty
                "IdentityT (Either Integer) Integer"
                $ ioProperty
                  . \(x :: Either Integer Integer) ->
                    concreteEvalSymOkProp $ IdentityT x,
              let model = buildModel ("a" ::= True)
                  eval ::
                    Bool ->
                    IdentityT (Either (AsKey SymBool)) (AsKey SymBool) ->
                    IdentityT (Either (AsKey SymBool)) (AsKey SymBool)
                  eval = flip evalSym model
               in testGroup
                    "IdentityT (Either SymBool) SymBool"
                    [ testGroup
                        "No fill default"
                        [ testCase "IdentityT (Left v): v in model" $
                            eval False (IdentityT $ Left $ ssym "a")
                              @?= IdentityT (Left $ con True),
                          testCase "IdentityT (Left v): v not in model" $
                            eval False (IdentityT $ Left $ ssym "b")
                              @?= IdentityT (Left $ ssym "b"),
                          testCase "IdentityT (Right v): v in model" $
                            eval False (IdentityT $ Right $ ssym "a")
                              @?= IdentityT (Right $ con True),
                          testCase "IdentityT (Right v): v not in model" $
                            eval False (IdentityT $ Right $ ssym "b")
                              @?= IdentityT (Right $ ssym "b")
                        ],
                      testGroup
                        "Fill default"
                        [ testCase "IdentityT (Left v): v in model" $
                            eval True (IdentityT $ Left $ ssym "a")
                              @?= IdentityT (Left $ con True),
                          testCase "IdentityT (Left v): v not in model" $
                            eval True (IdentityT $ Left $ ssym "b")
                              @?= IdentityT (Left $ con False),
                          testCase "IdentityT (Right v): v in model" $
                            eval True (IdentityT $ Right $ ssym "a")
                              @?= IdentityT (Right $ con True),
                          testCase "IdentityT (Right v): v not in model" $
                            eval True (IdentityT $ Right $ ssym "b")
                              @?= IdentityT (Right $ con False)
                        ]
                    ]
            ]
        ]
    ]
