{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Core.Data.Class.SubstituteSymTests (substituteSymTests) where

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
import Grisette.Core.Data.Class.LogicalOp (LogicalOp ((.||)))
import Grisette.Core.Data.Class.SubstituteSym (SubstituteSym (substituteSym))
import Grisette.Core.Data.Class.TestValues (ssymBool, ssymbolBool)
import Grisette.IR.SymPrim.Data.SymBool (SymBool)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (Assertion, (@?=))
import Test.QuickCheck (ioProperty)

concreteSubstituteSymOkProp ::
  (HasCallStack, SubstituteSym a, Show a, Eq a) => a -> Assertion
concreteSubstituteSymOkProp x =
  substituteSym (ssymbolBool "a") (ssymBool "b") x @?= x

substituteSymTests :: Test
substituteSymTests =
  testGroup
    "SubstituteSym"
    [ testGroup
        "SubstituteSym for common types"
        [ testCase "SymBool" $ do
            let asym = ssymbolBool "a"
            let a = ssymBool "a"
            let b = ssymBool "b"
            let c = ssymBool "c"
            let subst = substituteSym asym b
            subst a @?= b
            subst c @?= c
            subst (a .|| c) @?= b .|| c,
          testProperty "Bool" $ ioProperty . concreteSubstituteSymOkProp @Bool,
          testProperty "Integer" $
            ioProperty . concreteSubstituteSymOkProp @Integer,
          testProperty "Char" $ ioProperty . concreteSubstituteSymOkProp @Char,
          testProperty "Int" $ ioProperty . concreteSubstituteSymOkProp @Int,
          testProperty "Int8" $ ioProperty . concreteSubstituteSymOkProp @Int8,
          testProperty "Int16" $
            ioProperty . concreteSubstituteSymOkProp @Int16,
          testProperty "Int32" $
            ioProperty . concreteSubstituteSymOkProp @Int32,
          testProperty "Int64" $
            ioProperty . concreteSubstituteSymOkProp @Int64,
          testProperty "Word" $ ioProperty . concreteSubstituteSymOkProp @Word,
          testProperty "Word8" $
            ioProperty . concreteSubstituteSymOkProp @Word8,
          testProperty "Word16" $
            ioProperty . concreteSubstituteSymOkProp @Word16,
          testProperty "Word32" $
            ioProperty . concreteSubstituteSymOkProp @Word32,
          testProperty "Word64" $
            ioProperty . concreteSubstituteSymOkProp @Word64,
          testGroup
            "List"
            [ testProperty "[Integer]" $
                ioProperty . concreteSubstituteSymOkProp @[Integer],
              testCase "[SymBool]" $ do
                let asym = ssymbolBool "a"
                let a = ssymBool "a"
                let b = ssymBool "b"
                let c = ssymBool "c"
                let subst = substituteSym asym b
                subst [a, c] @?= [b, c]
            ],
          testGroup
            "Maybe"
            [ testProperty "Maybe Integer" $
                ioProperty . concreteSubstituteSymOkProp @(Maybe Integer),
              testCase "Maybe SymBool" $ do
                let asym = ssymbolBool "a"
                let a = ssymBool "a"
                let b = ssymBool "b"
                let c = ssymBool "c"
                let subst :: Maybe SymBool -> Maybe SymBool
                    subst = substituteSym asym b
                subst (Just a) @?= Just b
                subst (Just c) @?= Just c
                subst Nothing @?= Nothing
            ],
          testGroup
            "Either"
            [ testProperty "Either Integer Integer" $
                ioProperty
                  . concreteSubstituteSymOkProp @(Either Integer Integer),
              testCase "Either SymBool SymBool" $ do
                let asym = ssymbolBool "a"
                let a = ssymBool "a"
                let b = ssymBool "b"
                let c = ssymBool "c"
                let subst :: Either SymBool SymBool -> Either SymBool SymBool
                    subst = substituteSym asym b
                subst (Left a) @?= Left b
                subst (Left c) @?= Left c
                subst (Right a) @?= Right b
                subst (Right c) @?= Right c
            ],
          testGroup
            "MaybeT"
            [ testProperty "MaybeT Maybe Integer" $
                ioProperty
                  . concreteSubstituteSymOkProp @(MaybeT Maybe Integer)
                  . MaybeT,
              testCase "MaybeT Maybe SymBool" $ do
                let asym = ssymbolBool "a"
                let a = ssymBool "a"
                let b = ssymBool "b"
                let c = ssymBool "c"
                let subst :: MaybeT Maybe SymBool -> MaybeT Maybe SymBool
                    subst = substituteSym asym b
                subst (MaybeT Nothing) @?= MaybeT Nothing
                subst (MaybeT (Just Nothing)) @?= MaybeT (Just Nothing)
                subst (MaybeT (Just (Just a))) @?= MaybeT (Just (Just b))
                subst (MaybeT (Just (Just c))) @?= MaybeT (Just (Just c))
            ],
          testGroup
            "ExceptT"
            [ testProperty "ExceptT Maybe Integer" $
                ioProperty
                  . concreteSubstituteSymOkProp @(ExceptT Integer Maybe Integer)
                  . ExceptT,
              testCase "ExceptT SymBool Maybe SymBool" $ do
                let asym = ssymbolBool "a"
                let a = ssymBool "a"
                let b = ssymBool "b"
                let c = ssymBool "c"
                let subst ::
                      ExceptT SymBool Maybe SymBool ->
                      ExceptT SymBool Maybe SymBool
                    subst = substituteSym asym b
                subst (ExceptT Nothing) @?= ExceptT Nothing
                subst (ExceptT $ Just $ Left a) @?= ExceptT (Just $ Left b)
                subst (ExceptT $ Just $ Left c) @?= ExceptT (Just $ Left c)
                subst (ExceptT $ Just $ Right a) @?= ExceptT (Just $ Right b)
                subst (ExceptT $ Just $ Right c) @?= ExceptT (Just $ Right c)
            ],
          testProperty "()" (ioProperty . concreteSubstituteSymOkProp @()),
          testGroup
            "(,)"
            [ testProperty "(Integer, Integer)" $
                ioProperty . concreteSubstituteSymOkProp @(Integer, Integer),
              testCase "(SymBool, SymBool)" $ do
                let asym = ssymbolBool "a"
                let a = ssymBool "a"
                let b = ssymBool "b"
                let c = ssymBool "c"
                substituteSym asym b (a, c) @?= (b, c)
            ],
          testGroup
            "(,,)"
            [ testProperty "(Integer, Integer, Integer)" $
                ioProperty
                  . concreteSubstituteSymOkProp @(Integer, Integer, Integer),
              testCase "(SymBool, SymBool, SymBool)" $ do
                let asym = ssymbolBool "a"
                let a = ssymBool "a"
                let b = ssymBool "b"
                let c = ssymBool "c"
                substituteSym asym b (a, c, a) @?= (b, c, b)
            ],
          testGroup
            "(,,,)"
            [ testProperty "(Integer, Integer, Integer, Integer)" $
                ioProperty
                  . concreteSubstituteSymOkProp
                    @(Integer, Integer, Integer, Integer),
              testCase "(SymBool, SymBool, SymBool, SymBool)" $ do
                let asym = ssymbolBool "a"
                let a = ssymBool "a"
                let b = ssymBool "b"
                let c = ssymBool "c"
                substituteSym asym b (a, c, a, c) @?= (b, c, b, c)
            ],
          testGroup
            "(,,,,)"
            [ testProperty "(Integer, Integer, Integer, Integer, Integer)" $
                ioProperty
                  . concreteSubstituteSymOkProp
                    @(Integer, Integer, Integer, Integer, Integer),
              testCase "(SymBool, SymBool, SymBool, SymBool, SymBool)" $ do
                let asym = ssymbolBool "a"
                let a = ssymBool "a"
                let b = ssymBool "b"
                let c = ssymBool "c"
                substituteSym asym b (a, c, a, c, a) @?= (b, c, b, c, b)
            ],
          testGroup
            "(,,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer, Integer)"
                $ ioProperty
                  . concreteSubstituteSymOkProp
                    @(Integer, Integer, Integer, Integer, Integer, Integer),
              testCase
                "(SymBool, SymBool, SymBool, SymBool, SymBool, SymBool)"
                $ do
                  let asym = ssymbolBool "a"
                  let a = ssymBool "a"
                  let b = ssymBool "b"
                  let c = ssymBool "c"
                  substituteSym asym b (a, c, a, c, a, c) @?= (b, c, b, c, b, c)
            ],
          testGroup
            "(,,,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer, Integer, Integer)"
                $ ioProperty
                  . concreteSubstituteSymOkProp
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
                  let asym = ssymbolBool "a"
                  let a = ssymBool "a"
                  let b = ssymBool "b"
                  let c = ssymBool "c"
                  substituteSym asym b (a, c, a, c, a, c, a)
                    @?= (b, c, b, c, b, c, b)
            ],
          testGroup
            "(,,,,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer)"
                $ ioProperty
                  . concreteSubstituteSymOkProp
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
                  let asym = ssymbolBool "a"
                  let a = ssymBool "a"
                  let b = ssymBool "b"
                  let c = ssymBool "c"
                  substituteSym asym b (a, c, a, c, a, c, a, c)
                    @?= (b, c, b, c, b, c, b, c)
            ],
          testProperty "ByteString" $
            ioProperty
              . concreteSubstituteSymOkProp @B.ByteString
              . B.pack,
          testGroup
            "Sum"
            [ testProperty
                "Sum Maybe Maybe Integer"
                ( ioProperty
                    . concreteSubstituteSymOkProp @(Sum Maybe Maybe Integer)
                    . ( \case
                          Left x -> InL x
                          Right x -> InL x
                      )
                ),
              testCase
                "Sum Maybe Maybe SymBool"
                ( do
                    let asym = ssymbolBool "a"
                    let a = ssymBool "a"
                    let b = ssymBool "b"
                    let c = ssymBool "c"
                    let subst ::
                          Sum Maybe Maybe SymBool ->
                          Sum Maybe Maybe SymBool
                        subst = substituteSym asym b
                    subst (InL Nothing) @?= InL Nothing
                    subst (InL (Just a)) @?= InL (Just b)
                    subst (InL (Just c)) @?= InL (Just c)
                    subst (InR Nothing) @?= InR Nothing
                    subst (InR (Just a)) @?= InR (Just b)
                    subst (InR (Just c)) @?= InR (Just c)
                )
            ],
          testGroup
            "WriterT"
            [ testGroup
                "Lazy"
                [ testProperty
                    "WriterT Integer (Either Integer) Integer"
                    ( ioProperty
                        . concreteSubstituteSymOkProp
                          @(WriterLazy.WriterT Integer (Either Integer) Integer)
                        . WriterLazy.WriterT
                    ),
                  testCase "WriterT SymBool (Either SymBool) SymBool" $ do
                    let asym = ssymbolBool "a"
                    let a = ssymBool "a"
                    let b = ssymBool "b"
                    let c = ssymBool "c"
                    let subst ::
                          WriterLazy.WriterT SymBool (Either SymBool) SymBool ->
                          WriterLazy.WriterT SymBool (Either SymBool) SymBool
                        subst = substituteSym asym b
                    subst
                      (WriterLazy.WriterT (Left a))
                      @?= WriterLazy.WriterT (Left b)
                    subst
                      (WriterLazy.WriterT (Left c))
                      @?= WriterLazy.WriterT (Left c)
                    subst
                      (WriterLazy.WriterT (Right (a, a)))
                      @?= WriterLazy.WriterT (Right (b, b))
                    subst
                      (WriterLazy.WriterT (Right (c, c)))
                      @?= WriterLazy.WriterT (Right (c, c))
                ],
              testGroup
                "Strict"
                [ testProperty
                    "WriterT Integer (Either Integer) Integer"
                    ( ioProperty
                        . concreteSubstituteSymOkProp
                          @( WriterStrict.WriterT
                               Integer
                               (Either Integer)
                               Integer
                           )
                        . WriterStrict.WriterT
                    ),
                  testCase "WriterT SymBool (Either SymBool) SymBool" $ do
                    let asym = ssymbolBool "a"
                    let a = ssymBool "a"
                    let b = ssymBool "b"
                    let c = ssymBool "c"
                    let subst ::
                          WriterStrict.WriterT
                            SymBool
                            (Either SymBool)
                            SymBool ->
                          WriterStrict.WriterT
                            SymBool
                            (Either SymBool)
                            SymBool
                        subst = substituteSym asym b
                    subst
                      (WriterStrict.WriterT (Left a))
                      @?= WriterStrict.WriterT (Left b)
                    subst
                      (WriterStrict.WriterT (Left c))
                      @?= WriterStrict.WriterT (Left c)
                    subst
                      (WriterStrict.WriterT (Right (a, a)))
                      @?= WriterStrict.WriterT (Right (b, b))
                    subst
                      (WriterStrict.WriterT (Right (c, c)))
                      @?= WriterStrict.WriterT (Right (c, c))
                ]
            ],
          testGroup
            "Identity"
            [ testProperty
                "Identity Integer"
                (ioProperty . concreteSubstituteSymOkProp @(Identity Integer)),
              testCase "Identity SymBool" $ do
                let asym = ssymbolBool "a"
                let a = ssymBool "a"
                let b = ssymBool "b"
                let c = ssymBool "c"
                let subst :: Identity SymBool -> Identity SymBool
                    subst = substituteSym asym b
                subst (Identity a) @?= Identity b
                subst (Identity c) @?= Identity c
            ],
          testGroup
            "IdentityT"
            [ testProperty
                "IdentityT (Either Integer) Integer"
                $ ioProperty
                  . concreteSubstituteSymOkProp
                    @(IdentityT (Either Integer) Integer)
                  . IdentityT,
              testCase "IdentityT (Either SymBool) SymBool" $ do
                let asym = ssymbolBool "a"
                let a = ssymBool "a"
                let b = ssymBool "b"
                let c = ssymBool "c"
                let subst ::
                      IdentityT (Either SymBool) SymBool ->
                      IdentityT (Either SymBool) SymBool
                    subst = substituteSym asym b
                subst (IdentityT (Left a)) @?= IdentityT (Left b)
                subst (IdentityT (Left c)) @?= IdentityT (Left c)
                subst (IdentityT (Right a)) @?= IdentityT (Right b)
                subst (IdentityT (Right c)) @?= IdentityT (Right c)
            ]
        ]
    ]
