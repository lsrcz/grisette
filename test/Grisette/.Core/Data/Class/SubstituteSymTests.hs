{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Core.Data.Class.SubstituteSymTests where

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
import Grisette.Core.Data.Class.Substitute
import Grisette.TestUtils.SBool
import Grisette.TestUtils.Substitute
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

gsubstituteSymTests :: TestTree
gsubstituteSymTests =
  testGroup
    "GSubstituteSymTests"
    [ testGroup
        "GSubstituteSym for common types"
        [ testCase "SBool" $ do
            let asym = TSymbol $ SSymbol "a"
            let a = SSBool "a"
            let b = SSBool "b"
            let c = SSBool "c"
            gsubstituteSym asym (TSBool b) a @?= b
            gsubstituteSym asym (TSBool b) c @?= c
            gsubstituteSym asym (TSBool b) (Or a c) @?= Or b c,
          testProperty "Bool" (ioProperty . concreteGSubstituteSymOkProp @Bool),
          testProperty "Integer" (ioProperty . concreteGSubstituteSymOkProp @Integer),
          testProperty "Char" (ioProperty . concreteGSubstituteSymOkProp @Char),
          testProperty "Int" (ioProperty . concreteGSubstituteSymOkProp @Int),
          testProperty "Int8" (ioProperty . concreteGSubstituteSymOkProp @Int8),
          testProperty "Int16" (ioProperty . concreteGSubstituteSymOkProp @Int16),
          testProperty "Int32" (ioProperty . concreteGSubstituteSymOkProp @Int32),
          testProperty "Int64" (ioProperty . concreteGSubstituteSymOkProp @Int64),
          testProperty "Word" (ioProperty . concreteGSubstituteSymOkProp @Word),
          testProperty "Word8" (ioProperty . concreteGSubstituteSymOkProp @Word8),
          testProperty "Word16" (ioProperty . concreteGSubstituteSymOkProp @Word16),
          testProperty "Word32" (ioProperty . concreteGSubstituteSymOkProp @Word32),
          testProperty "Word64" (ioProperty . concreteGSubstituteSymOkProp @Word64),
          testGroup
            "List"
            [ testProperty "[Integer]" (ioProperty . concreteGSubstituteSymOkProp @[Integer]),
              testCase "[SBool]" $ do
                let asym = TSymbol $ SSymbol "a"
                let a = SSBool "a"
                let b = SSBool "b"
                let c = SSBool "c"
                gsubstituteSym asym (TSBool b) [a, c] @?= [b, c]
            ],
          testGroup
            "Maybe"
            [ testProperty "Maybe Integer" (ioProperty . concreteGSubstituteSymOkProp @(Maybe Integer)),
              testCase "Maybe SBool" $ do
                let asym = TSymbol $ SSymbol "a"
                let a = SSBool "a"
                let b = SSBool "b"
                let c = SSBool "c"
                gsubstituteSym asym (TSBool b) (Just a) @?= Just b
                gsubstituteSym asym (TSBool b) (Just c) @?= Just c
                gsubstituteSym asym (TSBool b) (Nothing :: Maybe SBool) @?= Nothing
            ],
          testGroup
            "Either"
            [ testProperty "Either Integer Integer" (ioProperty . concreteGSubstituteSymOkProp @(Either Integer Integer)),
              testCase "Either SBool SBool" $ do
                let asym = TSymbol $ SSymbol "a"
                let a = SSBool "a"
                let b = SSBool "b"
                let c = SSBool "c"
                gsubstituteSym asym (TSBool b) (Left a :: Either SBool SBool) @?= Left b
                gsubstituteSym asym (TSBool b) (Left c :: Either SBool SBool) @?= Left c
                gsubstituteSym asym (TSBool b) (Right a :: Either SBool SBool) @?= Right a
                gsubstituteSym asym (TSBool b) (Right c :: Either SBool SBool) @?= Right c
            ],
          testGroup
            "MaybeT"
            [ testProperty "MaybeT Maybe Integer" (ioProperty . concreteGSubstituteSymOkProp @(MaybeT Maybe Integer) . MaybeT),
              testCase "MaybeT Maybe SBool" $ do
                let asym = TSymbol $ SSymbol "a"
                let a = SSBool "a"
                let b = SSBool "b"
                let c = SSBool "c"
                gsubstituteSym asym (TSBool b) (MaybeT Nothing :: MaybeT Maybe SBool) @?= MaybeT Nothing
                gsubstituteSym asym (TSBool b) (MaybeT (Just Nothing) :: MaybeT Maybe SBool) @?= MaybeT (Just Nothing)
                gsubstituteSym asym (TSBool b) (MaybeT (Just (Just a))) @?= MaybeT (Just (Just b))
                gsubstituteSym asym (TSBool b) (MaybeT (Just (Just c))) @?= MaybeT (Just (Just c))
            ],
          testGroup
            "ExceptT"
            [ testProperty "ExceptT Maybe Integer" (ioProperty . concreteGSubstituteSymOkProp @(ExceptT Integer Maybe Integer) . ExceptT),
              testCase "ExceptT SBool Maybe SBool" $ do
                let asym = TSymbol $ SSymbol "a"
                let a = SSBool "a"
                let b = SSBool "b"
                let c = SSBool "c"
                gsubstituteSym asym (TSBool b) (ExceptT Nothing :: ExceptT SBool Maybe SBool) @?= ExceptT Nothing
                gsubstituteSym asym (TSBool b) (ExceptT $ Just $ Left "a" :: ExceptT SBool Maybe SBool) @?= ExceptT (Just $ Left "b")
                gsubstituteSym asym (TSBool b) (ExceptT $ Just $ Left "c" :: ExceptT SBool Maybe SBool) @?= ExceptT (Just $ Left "c")
                gsubstituteSym asym (TSBool b) (ExceptT $ Just $ Right "a" :: ExceptT SBool Maybe SBool) @?= ExceptT (Just $ Right "b")
                gsubstituteSym asym (TSBool b) (ExceptT $ Just $ Right "c" :: ExceptT SBool Maybe SBool) @?= ExceptT (Just $ Right "c")
            ],
          testProperty "()" (ioProperty . concreteGSubstituteSymOkProp @()),
          testGroup
            "(,)"
            [ testProperty "(Integer, Integer)" (ioProperty . concreteGSubstituteSymOkProp @(Integer, Integer)),
              testCase "(SBool, SBool)" $ do
                let asym = TSymbol $ SSymbol "a"
                let a = SSBool "a"
                let b = SSBool "b"
                let c = SSBool "c"
                gsubstituteSym asym (TSBool b) (a, c) @?= (b, c)
            ],
          testGroup
            "(,,)"
            [ testProperty
                "(Integer, Integer, Integer)"
                (ioProperty . concreteGSubstituteSymOkProp @(Integer, Integer, Integer)),
              testCase "(SBool, SBool, SBool)" $ do
                let asym = TSymbol $ SSymbol "a"
                let a = SSBool "a"
                let b = SSBool "b"
                let c = SSBool "c"
                gsubstituteSym asym (TSBool b) (a, c, a) @?= (b, c, b)
            ],
          testGroup
            "(,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer)"
                (ioProperty . concreteGSubstituteSymOkProp @(Integer, Integer, Integer, Integer)),
              testCase "(SBool, SBool, SBool, SBool)" $ do
                let asym = TSymbol $ SSymbol "a"
                let a = SSBool "a"
                let b = SSBool "b"
                let c = SSBool "c"
                gsubstituteSym asym (TSBool b) (a, c, a, c) @?= (b, c, b, c)
            ],
          testGroup
            "(,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer)"
                (ioProperty . concreteGSubstituteSymOkProp @(Integer, Integer, Integer, Integer, Integer)),
              testCase "(SBool, SBool, SBool, SBool, SBool)" $ do
                let asym = TSymbol $ SSymbol "a"
                let a = SSBool "a"
                let b = SSBool "b"
                let c = SSBool "c"
                gsubstituteSym asym (TSBool b) (a, c, a, c, a) @?= (b, c, b, c, b)
            ],
          testGroup
            "(,,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer, Integer)"
                (ioProperty . concreteGSubstituteSymOkProp @(Integer, Integer, Integer, Integer, Integer, Integer)),
              testCase "(SBool, SBool, SBool, SBool, SBool, SBool)" $ do
                let asym = TSymbol $ SSymbol "a"
                let a = SSBool "a"
                let b = SSBool "b"
                let c = SSBool "c"
                gsubstituteSym asym (TSBool b) (a, c, a, c, a, c) @?= (b, c, b, c, b, c)
            ],
          testGroup
            "(,,,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer, Integer, Integer)"
                (ioProperty . concreteGSubstituteSymOkProp @(Integer, Integer, Integer, Integer, Integer, Integer, Integer)),
              testCase "(SBool, SBool, SBool, SBool, SBool, SBool, SBool)" $ do
                let asym = TSymbol $ SSymbol "a"
                let a = SSBool "a"
                let b = SSBool "b"
                let c = SSBool "c"
                gsubstituteSym asym (TSBool b) (a, c, a, c, a, c, a) @?= (b, c, b, c, b, c, b)
            ],
          testGroup
            "(,,,,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer)"
                (ioProperty . concreteGSubstituteSymOkProp @(Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer)),
              testCase "(SBool, SBool, SBool, SBool, SBool, SBool, SBool, SBool)" $ do
                let asym = TSymbol $ SSymbol "a"
                let a = SSBool "a"
                let b = SSBool "b"
                let c = SSBool "c"
                gsubstituteSym asym (TSBool b) (a, c, a, c, a, c, a, c) @?= (b, c, b, c, b, c, b, c)
            ],
          testProperty "ByteString" (ioProperty . concreteGSubstituteSymOkProp @B.ByteString . B.pack),
          testGroup
            "Sum"
            [ testProperty
                "Sum Maybe Maybe Integer"
                ( ioProperty
                    . concreteGSubstituteSymOkProp @(Sum Maybe Maybe Integer)
                    . ( \case
                          Left x -> InL x
                          Right x -> InL x
                      )
                ),
              testCase
                "Sum Maybe Maybe SBool"
                ( do
                    let asym = TSymbol $ SSymbol "a"
                    let a = SSBool "a"
                    let b = SSBool "b"
                    let c = SSBool "c"
                    gsubstituteSym asym (TSBool b) (InL Nothing :: Sum Maybe Maybe SBool) @?= InL Nothing
                    gsubstituteSym asym (TSBool b) (InL (Just a) :: Sum Maybe Maybe SBool) @?= InL (Just b)
                    gsubstituteSym asym (TSBool b) (InL (Just c) :: Sum Maybe Maybe SBool) @?= InL (Just c)
                    gsubstituteSym asym (TSBool b) (InR Nothing :: Sum Maybe Maybe SBool) @?= InR Nothing
                    gsubstituteSym asym (TSBool b) (InR (Just a) :: Sum Maybe Maybe SBool) @?= InR (Just b)
                    gsubstituteSym asym (TSBool b) (InR (Just c) :: Sum Maybe Maybe SBool) @?= InR (Just c)
                )
            ],
          testGroup
            "WriterT"
            [ testGroup
                "Lazy"
                [ testProperty
                    "WriterT Integer (Either Integer) Integer"
                    ( ioProperty
                        . concreteGSubstituteSymOkProp
                          @(WriterLazy.WriterT Integer (Either Integer) Integer)
                        . WriterLazy.WriterT
                    ),
                  testCase "WriterT SBool (Either SBool) SBool" $ do
                    let asym = TSymbol $ SSymbol "a"
                    let a = SSBool "a"
                    let b = SSBool "b"
                    let c = SSBool "c"
                    gsubstituteSym
                      asym
                      (TSBool b)
                      (WriterLazy.WriterT (Left a) :: WriterLazy.WriterT SBool (Either SBool) SBool)
                      @?= WriterLazy.WriterT (Left b)
                    gsubstituteSym
                      asym
                      (TSBool b)
                      (WriterLazy.WriterT (Left c) :: WriterLazy.WriterT SBool (Either SBool) SBool)
                      @?= WriterLazy.WriterT (Left c)
                    gsubstituteSym
                      asym
                      (TSBool b)
                      (WriterLazy.WriterT (Right (a, a)) :: WriterLazy.WriterT SBool (Either SBool) SBool)
                      @?= WriterLazy.WriterT (Right (b, b))
                    gsubstituteSym
                      asym
                      (TSBool b)
                      (WriterLazy.WriterT (Right (c, c)) :: WriterLazy.WriterT SBool (Either SBool) SBool)
                      @?= WriterLazy.WriterT (Right (c, c))
                ],
              testGroup
                "Strict"
                [ testProperty
                    "WriterT Integer (Either Integer) Integer"
                    ( ioProperty
                        . concreteGSubstituteSymOkProp
                          @(WriterStrict.WriterT Integer (Either Integer) Integer)
                        . WriterStrict.WriterT
                    ),
                  testCase "WriterT SBool (Either SBool) SBool" $ do
                    let asym = TSymbol $ SSymbol "a"
                    let a = SSBool "a"
                    let b = SSBool "b"
                    let c = SSBool "c"
                    gsubstituteSym
                      asym
                      (TSBool b)
                      (WriterStrict.WriterT (Left a) :: WriterStrict.WriterT SBool (Either SBool) SBool)
                      @?= WriterStrict.WriterT (Left b)
                    gsubstituteSym
                      asym
                      (TSBool b)
                      (WriterStrict.WriterT (Left c) :: WriterStrict.WriterT SBool (Either SBool) SBool)
                      @?= WriterStrict.WriterT (Left c)
                    gsubstituteSym
                      asym
                      (TSBool b)
                      (WriterStrict.WriterT (Right (a, a)) :: WriterStrict.WriterT SBool (Either SBool) SBool)
                      @?= WriterStrict.WriterT (Right (b, b))
                    gsubstituteSym
                      asym
                      (TSBool b)
                      (WriterStrict.WriterT (Right (c, c)) :: WriterStrict.WriterT SBool (Either SBool) SBool)
                      @?= WriterStrict.WriterT (Right (c, c))
                ]
            ],
          testGroup
            "Identity"
            [ testProperty
                "Identity Integer"
                (ioProperty . concreteGSubstituteSymOkProp @(Identity Integer)),
              testCase "Identity SBool" $ do
                let asym = TSymbol $ SSymbol "a"
                let a = SSBool "a"
                let b = SSBool "b"
                let c = SSBool "c"
                gsubstituteSym asym (TSBool b) (Identity a) @?= Identity b
                gsubstituteSym asym (TSBool b) (Identity c) @?= Identity c
            ],
          testGroup
            "IdentityT"
            [ testProperty
                "IdentityT (Either Integer) Integer"
                (ioProperty . concreteGSubstituteSymOkProp @(IdentityT (Either Integer) Integer) . IdentityT),
              testCase "IdentityT (Either SBool) SBool" $ do
                let asym = TSymbol $ SSymbol "a"
                let a = SSBool "a"
                let b = SSBool "b"
                let c = SSBool "c"
                gsubstituteSym asym (TSBool b) (IdentityT (Left a) :: IdentityT (Either SBool) SBool) @?= IdentityT (Left b)
                gsubstituteSym asym (TSBool b) (IdentityT (Left c) :: IdentityT (Either SBool) SBool) @?= IdentityT (Left c)
                gsubstituteSym asym (TSBool b) (IdentityT (Right a) :: IdentityT (Either SBool) SBool) @?= IdentityT (Right b)
                gsubstituteSym asym (TSBool b) (IdentityT (Right c) :: IdentityT (Either SBool) SBool) @?= IdentityT (Right c)
            ]
        ]
    ]
