{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Core.Data.Class.ToConTests (toConTests) where

import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Identity
  ( Identity (Identity),
    IdentityT (IdentityT),
  )
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.ByteString.Char8 as C
import Data.Foldable (traverse_)
import Data.Functor.Sum (Sum (InL, InR))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Stack (HasCallStack)
import Grisette.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Core.Data.Class.LogicalOp (LogicalOp (symNot, (.&&), (.||)))
import Grisette.Core.Data.Class.SEq (SEq ((.==)))
import Grisette.Core.Data.Class.TestValues
  ( conBool,
    isymBool,
    ssymBool,
    symFalse,
    symTrue,
  )
import Grisette.Core.Data.Class.ToCon (ToCon (toCon))
import Grisette.SymPrim.SymBool (SymBool)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (Assertion, (@?=))
import Test.QuickCheck (ioProperty)

toConForConcreteOkProp ::
  (HasCallStack, ToCon v v, Show v, Eq v) => v -> Assertion
toConForConcreteOkProp v = toCon v @?= Just v

toConTests :: Test
toConTests =
  testGroup
    "ToCon"
    [ testGroup
        "ToCon for common types"
        [ testGroup
            "SymBool"
            [ testGroup
                "To Bool"
                [ testCase "con" $ do
                    let bools :: [Bool] = [True, False]
                    traverse_ (\v -> toCon (conBool v) @?= Just v) bools,
                  testCase "Symbolic SymBools" $ do
                    let sbools :: [SymBool] =
                          [ ssymBool "a",
                            isymBool "a" 1,
                            ssymBool "a" .&& ssymBool "b",
                            ssymBool "a" .|| ssymBool "b",
                            symNot $ ssymBool "a",
                            ssymBool "a" .== ssymBool "b",
                            symIte (ssymBool "a") (ssymBool "b") (ssymBool "c")
                          ]
                    traverse_ (\v -> toCon v @?= (Nothing :: Maybe Bool)) sbools
                ],
              testCase "To SymBool" $ do
                let sbools :: [SymBool] =
                      [ symTrue,
                        ssymBool "a",
                        isymBool "a" 1,
                        ssymBool "a" .&& ssymBool "b",
                        ssymBool "a" .|| ssymBool "b",
                        symNot $ ssymBool "a",
                        ssymBool "a" .== ssymBool "b",
                        symIte (ssymBool "a") (ssymBool "b") (ssymBool "c")
                      ]
                traverse_ (\v -> toCon v @?= Just v) sbools
            ],
          testProperty "Bool" $ ioProperty . toConForConcreteOkProp @Bool,
          testProperty "Integer" $ ioProperty . toConForConcreteOkProp @Integer,
          testProperty "Char" $ ioProperty . toConForConcreteOkProp @Char,
          testProperty "Int" $ ioProperty . toConForConcreteOkProp @Int,
          testProperty "Int8" $ ioProperty . toConForConcreteOkProp @Int8,
          testProperty "Int16" $ ioProperty . toConForConcreteOkProp @Int16,
          testProperty "Int32" $ ioProperty . toConForConcreteOkProp @Int32,
          testProperty "Int64" $ ioProperty . toConForConcreteOkProp @Int64,
          testProperty "Word" $ ioProperty . toConForConcreteOkProp @Word,
          testProperty "Word8" $ ioProperty . toConForConcreteOkProp @Word8,
          testProperty "Word16" $ ioProperty . toConForConcreteOkProp @Word16,
          testProperty "Word32" $ ioProperty . toConForConcreteOkProp @Word32,
          testProperty "Word64" $ ioProperty . toConForConcreteOkProp @Word64,
          testProperty "()" $ ioProperty . toConForConcreteOkProp @(),
          testProperty "ByteString" $
            ioProperty
              . \(v :: String) -> toConForConcreteOkProp (C.pack v),
          testGroup
            "List"
            [ testProperty "[Integer]" $
                ioProperty . toConForConcreteOkProp @[Integer],
              testCase "[SymBool]" $ do
                toCon ([] :: [SymBool]) @?= (Just [] :: Maybe [Bool])
                toCon ([symTrue] :: [SymBool]) @?= (Just [True] :: Maybe [Bool])
                toCon ([ssymBool "a"] :: [SymBool])
                  @?= (Nothing :: Maybe [Bool])
                toCon ([symTrue, symFalse] :: [SymBool])
                  @?= (Just [True, False] :: Maybe [Bool])
                toCon ([symTrue, ssymBool "a"] :: [SymBool])
                  @?= (Nothing :: Maybe [Bool])
            ],
          testGroup
            "Maybe"
            [ testProperty "Maybe Integer" $
                ioProperty . toConForConcreteOkProp @(Maybe Integer),
              testCase "Maybe SymBool" $ do
                let toConMaybe :: Maybe SymBool -> Maybe (Maybe Bool)
                    toConMaybe = toCon
                toConMaybe Nothing @?= (Just Nothing)
                toConMaybe (Just symTrue) @?= (Just (Just True))
                toConMaybe (Just $ ssymBool "a") @?= Nothing
            ],
          testGroup
            "Either"
            [ testProperty "Either Integer Integer" $
                ioProperty . toConForConcreteOkProp @(Either Integer Integer),
              testCase "Either SymBool SymBool" $ do
                let toConEither ::
                      Either SymBool SymBool ->
                      Maybe (Either Bool Bool)
                    toConEither = toCon
                toConEither (Left symTrue) @?= (Just (Left True))
                toConEither (Right symTrue) @?= (Just (Right True))
                toConEither (Left $ ssymBool "a") @?= Nothing
                toConEither (Right $ ssymBool "a") @?= Nothing
            ],
          testGroup
            "MaybeT"
            [ testProperty "MaybeT Maybe Integer" $
                ioProperty . \(v :: Maybe (Maybe Integer)) ->
                  toConForConcreteOkProp (MaybeT v),
              testCase "MaybeT Maybe SymBool" $ do
                toCon (MaybeT Nothing :: MaybeT Maybe SymBool)
                  @?= (Just $ MaybeT Nothing :: Maybe (MaybeT Maybe Bool))
                toCon (MaybeT $ Just Nothing :: MaybeT Maybe SymBool)
                  @?= ( Just $ MaybeT $ Just Nothing ::
                          Maybe (MaybeT Maybe Bool)
                      )
                toCon
                  ( MaybeT $ Just $ Just symTrue ::
                      MaybeT Maybe SymBool
                  )
                  @?= ( Just $ MaybeT $ Just $ Just True ::
                          Maybe (MaybeT Maybe Bool)
                      )
                toCon
                  ( MaybeT $ Just $ Just $ ssymBool "a" ::
                      MaybeT Maybe SymBool
                  )
                  @?= (Nothing :: Maybe (MaybeT Maybe Bool))
            ],
          testGroup
            "ExceptT"
            [ testProperty "ExceptT Integer Maybe Integer" $
                ioProperty . \(v :: Maybe (Either Integer Integer)) ->
                  toConForConcreteOkProp (ExceptT v),
              testCase "ExceptT SymBool Maybe SymBool" $ do
                let toConExceptT ::
                      ExceptT SymBool Maybe SymBool ->
                      Maybe (ExceptT Bool Maybe Bool)
                    toConExceptT = toCon
                toConExceptT (ExceptT Nothing) @?= Just (ExceptT Nothing)
                toConExceptT (ExceptT $ Just $ Left symTrue)
                  @?= Just (ExceptT $ Just $ Left True)
                toConExceptT (ExceptT $ Just $ Left $ ssymBool "a") @?= Nothing
                toConExceptT (ExceptT $ Just $ Right symTrue)
                  @?= Just (ExceptT $ Just $ Right True)
                toConExceptT (ExceptT $ Just $ Right $ ssymBool "a") @?= Nothing
            ],
          testGroup
            "(,)"
            [ testProperty "(Integer, Integer)" $
                ioProperty . toConForConcreteOkProp @(Integer, Integer),
              testCase "(SymBool, SymBool)" $ do
                let toConTuple2 :: (SymBool, SymBool) -> Maybe (Bool, Bool)
                    toConTuple2 = toCon
                toConTuple2 (symTrue, symFalse) @?= Just (True, False)
                toConTuple2 (symTrue, ssymBool "a") @?= Nothing
            ],
          testGroup
            "(,,)"
            [ testProperty "(Integer, Integer, Integer)" $
                ioProperty
                  . toConForConcreteOkProp @(Integer, Integer, Integer),
              testCase "(SymBool, SymBool, SymBool)" $ do
                let toConTuple3 ::
                      (SymBool, SymBool, SymBool) ->
                      Maybe (Bool, Bool, Bool)
                    toConTuple3 = toCon
                toConTuple3 (symFalse, symTrue, symFalse)
                  @?= Just (False, True, False)
                toConTuple3 (symFalse, symTrue, ssymBool "a")
                  @?= Nothing
            ],
          testGroup
            "(,,,)"
            [ testProperty "(Integer, Integer, Integer, Integer)" $
                ioProperty
                  . toConForConcreteOkProp
                    @(Integer, Integer, Integer, Integer),
              testCase "(SymBool, SymBool, SymBool, SymBool)" $ do
                let toConTuple4 ::
                      (SymBool, SymBool, SymBool, SymBool) ->
                      Maybe (Bool, Bool, Bool, Bool)
                    toConTuple4 = toCon
                toConTuple4 (symTrue, symFalse, symTrue, symFalse)
                  @?= Just (True, False, True, False)
                toConTuple4 (symTrue, symFalse, symTrue, ssymBool "a")
                  @?= Nothing
            ],
          testGroup
            "(,,,,)"
            [ testProperty "(Integer, Integer, Integer, Integer, Integer)" $
                ioProperty
                  . toConForConcreteOkProp
                    @(Integer, Integer, Integer, Integer, Integer),
              testCase "(SymBool, SymBool, SymBool, SymBool, SymBool)" $ do
                let toConTuple5 ::
                      (SymBool, SymBool, SymBool, SymBool, SymBool) ->
                      Maybe (Bool, Bool, Bool, Bool, Bool)
                    toConTuple5 = toCon
                toConTuple5 (symFalse, symTrue, symFalse, symTrue, symFalse)
                  @?= Just (False, True, False, True, False)
                toConTuple5 (symFalse, symTrue, symFalse, symTrue, ssymBool "a")
                  @?= Nothing
            ],
          testGroup
            "(,,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer, Integer)"
                $ ioProperty
                  . toConForConcreteOkProp
                    @(Integer, Integer, Integer, Integer, Integer, Integer),
              testCase
                "(SymBool, SymBool, SymBool, SymBool, SymBool, SymBool)"
                $ do
                  let toConTuple6 ::
                        ( SymBool,
                          SymBool,
                          SymBool,
                          SymBool,
                          SymBool,
                          SymBool
                        ) ->
                        Maybe (Bool, Bool, Bool, Bool, Bool, Bool)
                      toConTuple6 = toCon
                  toConTuple6
                    (symTrue, symFalse, symTrue, symFalse, symTrue, symFalse)
                    @?= Just (True, False, True, False, True, False)
                  toConTuple6
                    (symTrue, symFalse, symTrue, symFalse, symTrue, ssymBool "a")
                    @?= Nothing
            ],
          testGroup
            "(,,,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer, Integer, Integer)"
                $ ioProperty
                  . toConForConcreteOkProp
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
                  let toConTuple7 ::
                        ( SymBool,
                          SymBool,
                          SymBool,
                          SymBool,
                          SymBool,
                          SymBool,
                          SymBool
                        ) ->
                        Maybe (Bool, Bool, Bool, Bool, Bool, Bool, Bool)
                      toConTuple7 = toCon
                  toConTuple7
                    ( symFalse,
                      symTrue,
                      symFalse,
                      symTrue,
                      symFalse,
                      symTrue,
                      symFalse
                    )
                    @?= Just (False, True, False, True, False, True, False)
                  toConTuple7
                    ( symFalse,
                      symTrue,
                      symFalse,
                      symTrue,
                      symFalse,
                      symTrue,
                      ssymBool "a"
                    )
                    @?= Nothing
            ],
          testGroup
            "(,,,,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer)"
                $ ioProperty
                  . toConForConcreteOkProp
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
                $ do
                  let toConTuple8 ::
                        ( SymBool,
                          SymBool,
                          SymBool,
                          SymBool,
                          SymBool,
                          SymBool,
                          SymBool,
                          SymBool
                        ) ->
                        Maybe (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool)
                      toConTuple8 = toCon
                  toConTuple8
                    ( symTrue,
                      symFalse,
                      symTrue,
                      symFalse,
                      symTrue,
                      symFalse,
                      symTrue,
                      symFalse
                    )
                    @?= Just
                      ( True,
                        False,
                        True,
                        False,
                        True,
                        False,
                        True,
                        False
                      )
                  toConTuple8
                    ( symTrue,
                      symFalse,
                      symTrue,
                      symFalse,
                      symTrue,
                      symFalse,
                      symTrue,
                      ssymBool "a"
                    )
                    @?= Nothing
            ],
          testGroup
            "Sum"
            [ testProperty "Sum Maybe Maybe Integer" $
                ioProperty . \(v :: Either (Maybe Integer) (Maybe Integer)) ->
                  toConForConcreteOkProp $ case v of
                    Left x -> InL x
                    Right x -> InR x,
              testCase "Sum Maybe (Either SymBool) SymBool" $ do
                let toConSum ::
                      Sum Maybe (Either SymBool) SymBool ->
                      Maybe (Sum Maybe (Either Bool) Bool)
                    toConSum = toCon
                toConSum (InL (Just symTrue)) @?= Just (InL (Just True))
                toConSum (InL (Just $ ssymBool "a")) @?= Nothing
                toConSum (InR (Left symTrue)) @?= Just (InR (Left True))
                toConSum (InR (Right symTrue)) @?= Just (InR (Right True))
                toConSum (InR (Left $ ssymBool "a")) @?= Nothing
                toConSum (InR (Right $ ssymBool "a")) @?= Nothing
            ],
          testGroup
            "WriterT"
            [ testGroup
                "Lazy"
                [ testProperty "WriterT Integer (Either Integer) Integer" $
                    ioProperty . \(v :: Either Integer (Integer, Integer)) ->
                      toConForConcreteOkProp $ WriterLazy.WriterT v,
                  testCase "WriterT SymBool (Either SymBool) SymBool" $ do
                    let toConWriterT ::
                          WriterLazy.WriterT SymBool (Either SymBool) SymBool ->
                          Maybe (WriterLazy.WriterT Bool (Either Bool) Bool)
                        toConWriterT = toCon
                    toConWriterT (WriterLazy.WriterT $ Left symTrue)
                      @?= Just (WriterLazy.WriterT $ Left True)
                    toConWriterT (WriterLazy.WriterT $ Left $ ssymBool "a")
                      @?= Nothing
                    toConWriterT (WriterLazy.WriterT $ Right (symTrue, symTrue))
                      @?= Just (WriterLazy.WriterT $ Right (True, True))
                    toConWriterT
                      (WriterLazy.WriterT $ Right (ssymBool "a", symTrue))
                      @?= Nothing
                    toConWriterT
                      (WriterLazy.WriterT $ Right (symTrue, ssymBool "a"))
                      @?= Nothing
                    toConWriterT
                      (WriterLazy.WriterT $ Right (ssymBool "a", ssymBool "b"))
                      @?= Nothing
                ],
              testGroup
                "Strict"
                [ testProperty "WriterT Integer (Either Integer) Integer" $
                    ioProperty . \(v :: Either Integer (Integer, Integer)) ->
                      toConForConcreteOkProp $ WriterStrict.WriterT v,
                  testCase "WriterT SymBool (Either SymBool) SymBool" $ do
                    let toConWriterT ::
                          WriterStrict.WriterT
                            SymBool
                            (Either SymBool)
                            SymBool ->
                          Maybe (WriterStrict.WriterT Bool (Either Bool) Bool)
                        toConWriterT = toCon
                    toConWriterT (WriterStrict.WriterT $ Left symTrue)
                      @?= Just (WriterStrict.WriterT $ Left True)
                    toConWriterT (WriterStrict.WriterT $ Left $ ssymBool "a")
                      @?= Nothing
                    toConWriterT
                      (WriterStrict.WriterT $ Right (symTrue, symTrue))
                      @?= Just (WriterStrict.WriterT $ Right (True, True))
                    toConWriterT
                      (WriterStrict.WriterT $ Right (ssymBool "a", symTrue))
                      @?= Nothing
                    toConWriterT
                      (WriterStrict.WriterT $ Right (symTrue, ssymBool "a"))
                      @?= Nothing
                    toConWriterT
                      (WriterStrict.WriterT $ Right (ssymBool "a", ssymBool "b"))
                      @?= Nothing
                ]
            ],
          testGroup
            "Identity"
            [ testProperty "Identity Integer" $
                ioProperty . \(v :: Integer) ->
                  toConForConcreteOkProp $ Identity v,
              testCase "Identity SymBool" $ do
                toCon (Identity symTrue) @?= Just (Identity True)
                toCon (Identity $ ssymBool "a") @?= (Nothing :: Maybe (Identity Bool))
            ],
          testGroup
            "IdentityT"
            [ testProperty "IdentityT (Either Integer) Integer" $
                ioProperty . \(v :: Either Integer Integer) ->
                  toConForConcreteOkProp $ IdentityT v,
              testCase "IdentityT (Either SymBool) SymBool" $ do
                let toConIdentityT ::
                      IdentityT (Either SymBool) SymBool ->
                      Maybe (IdentityT (Either Bool) Bool)
                    toConIdentityT = toCon
                toConIdentityT (IdentityT $ Left symTrue)
                  @?= Just (IdentityT $ Left True)
                toConIdentityT (IdentityT $ Left $ ssymBool "a")
                  @?= Nothing
                toConIdentityT (IdentityT $ Right symTrue)
                  @?= Just (IdentityT $ Right True)
                toConIdentityT (IdentityT $ Right $ ssymBool "a")
                  @?= Nothing
            ]
        ]
    ]
