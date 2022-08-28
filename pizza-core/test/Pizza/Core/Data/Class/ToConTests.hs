{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Pizza.Core.Data.Class.ToConTests where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.ByteString.Char8 as C
import Data.Foldable
import Data.Functor.Sum
import Data.Int
import Data.Word
import Pizza.Core.Data.Class.ToCon
import Pizza.TestUtils.SBool
import Pizza.TestUtils.ToCon
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

toConTests :: TestTree
toConTests =
  testGroup
    "ToConTests"
    [ testGroup
        "ToCon for common types"
        [ testGroup
            "SBool"
            [ testGroup
                "To Bool"
                [ testCase "CBool" $ do
                    let bools :: [Bool] = [True, False]
                    traverse_ (\v -> toCon (CBool v) @=? Just v) bools,
                  testCase "Symbolic SBools" $ do
                    let sbools :: [SBool] =
                          [ SSBool "a",
                            ISBool "a" 1,
                            And (SSBool "a") (SSBool "b"),
                            Or (SSBool "a") (SSBool "b"),
                            Not (SSBool "a"),
                            Equal (SSBool "a") (SSBool "b"),
                            ITE (SSBool "a") (SSBool "b") (SSBool "c")
                          ]
                    traverse_ (\v -> toCon v @=? (Nothing :: Maybe Bool)) sbools
                ],
              testCase "To SBool" $ do
                let sbools :: [SBool] =
                      [ CBool True,
                        SSBool "a",
                        ISBool "a" 1,
                        And (SSBool "a") (SSBool "b"),
                        Or (SSBool "a") (SSBool "b"),
                        Not (SSBool "a"),
                        Equal (SSBool "a") (SSBool "b"),
                        ITE (SSBool "a") (SSBool "b") (SSBool "c")
                      ]
                traverse_ (\v -> toCon v @=? Just v) sbools
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
          testProperty "ByteString" $ ioProperty . \(v :: String) -> toConForConcreteOkProp (C.pack v),
          testGroup
            "List"
            [ testProperty "[Integer]" $ ioProperty . toConForConcreteOkProp @[Integer],
              testCase "[SBool]" $ do
                toCon ([] :: [SBool]) @=? (Just [] :: Maybe [Bool])
                toCon ([CBool True] :: [SBool]) @=? (Just [True] :: Maybe [Bool])
                toCon ([SSBool "a"] :: [SBool]) @=? (Nothing :: Maybe [Bool])
                toCon ([CBool True, CBool False] :: [SBool]) @=? (Just [True, False] :: Maybe [Bool])
                toCon ([CBool True, SSBool "a"] :: [SBool]) @=? (Nothing :: Maybe [Bool])
            ],
          testGroup
            "Maybe"
            [ testProperty "Maybe Integer" $ ioProperty . toConForConcreteOkProp @(Maybe Integer),
              testCase "Maybe SBool" $ do
                toCon (Nothing :: Maybe SBool) @=? (Just Nothing :: Maybe (Maybe Bool))
                toCon (Just (CBool True) :: Maybe SBool) @=? (Just (Just True) :: Maybe (Maybe Bool))
                toCon (Just (SSBool "a") :: Maybe SBool) @=? (Nothing :: Maybe (Maybe Bool))
            ],
          testGroup
            "Either"
            [ testProperty "Either Integer Integer" $ ioProperty . toConForConcreteOkProp @(Either Integer Integer),
              testCase "Either SBool SBool" $ do
                toCon (Left (CBool True) :: Either SBool SBool) @=? (Just (Left True) :: Maybe (Either Bool Bool))
                toCon (Right (CBool True) :: Either SBool SBool) @=? (Just (Right True) :: Maybe (Either Bool Bool))
                toCon (Left (SSBool "a") :: Either SBool SBool) @=? (Nothing :: Maybe (Either Bool Bool))
                toCon (Right (SSBool "a") :: Either SBool SBool) @=? (Nothing :: Maybe (Either Bool Bool))
            ],
          testGroup
            "MaybeT"
            [ testProperty "MaybeT Maybe Integer" $
                ioProperty . \(v :: Maybe (Maybe Integer)) -> toConForConcreteOkProp (MaybeT v),
              testCase "MaybeT Maybe SBool" $ do
                toCon (MaybeT Nothing :: MaybeT Maybe SBool) @=? (Just $ MaybeT Nothing :: Maybe (MaybeT Maybe Bool))
                toCon (MaybeT $ Just Nothing :: MaybeT Maybe SBool)
                  @=? (Just $ MaybeT $ Just Nothing :: Maybe (MaybeT Maybe Bool))
                toCon (MaybeT $ Just $ Just $ CBool True :: MaybeT Maybe SBool)
                  @=? (Just $ MaybeT $ Just $ Just True :: Maybe (MaybeT Maybe Bool))
                toCon (MaybeT $ Just $ Just $ SSBool "a" :: MaybeT Maybe SBool)
                  @=? (Nothing :: Maybe (MaybeT Maybe Bool))
            ],
          testGroup
            "ExceptT"
            [ testProperty "ExceptT Integer Maybe Integer" $
                ioProperty . \(v :: Maybe (Either Integer Integer)) -> toConForConcreteOkProp (ExceptT v),
              testCase "ExceptT SBool Maybe SBool" $ do
                toCon (ExceptT Nothing :: ExceptT SBool Maybe SBool)
                  @=? (Just $ ExceptT Nothing :: Maybe (ExceptT Bool Maybe Bool))
                toCon (ExceptT $ Just $ Left $ CBool True :: ExceptT SBool Maybe SBool)
                  @=? (Just $ ExceptT $ Just $ Left True :: Maybe (ExceptT Bool Maybe Bool))
                toCon (ExceptT $ Just $ Left $ SSBool "a" :: ExceptT SBool Maybe SBool)
                  @=? (Nothing :: Maybe (ExceptT Bool Maybe Bool))
                toCon (ExceptT $ Just $ Right $ CBool True :: ExceptT SBool Maybe SBool)
                  @=? (Just $ ExceptT $ Just $ Right True :: Maybe (ExceptT Bool Maybe Bool))
                toCon (ExceptT $ Just $ Right $ SSBool "a" :: ExceptT SBool Maybe SBool)
                  @=? (Nothing :: Maybe (ExceptT Bool Maybe Bool))
            ],
          testGroup
            "(,)"
            [ testProperty "(Integer, Integer)" $
                ioProperty . toConForConcreteOkProp @(Integer, Integer),
              testCase "(SBool, SBool)" $ do
                toCon (CBool True, CBool False) @=? Just (True, False)
                toCon (CBool True, SSBool "a") @=? (Nothing :: Maybe (Bool, Bool))
            ],
          testGroup
            "(,,)"
            [ testProperty "(Integer, Integer, Integer)" $
                ioProperty . toConForConcreteOkProp @(Integer, Integer, Integer),
              testCase "(SBool, SBool, SBool)" $ do
                toCon (CBool False, CBool True, CBool False) @=? Just (False, True, False)
                toCon (CBool False, CBool True, SSBool "a") @=? (Nothing :: Maybe (Bool, Bool, Bool))
            ],
          testGroup
            "(,,,)"
            [ testProperty "(Integer, Integer, Integer, Integer)" $
                ioProperty . toConForConcreteOkProp @(Integer, Integer, Integer, Integer),
              testCase "(SBool, SBool, SBool, SBool)" $ do
                toCon (CBool True, CBool False, CBool True, CBool False) @=? Just (True, False, True, False)
                toCon (CBool True, CBool False, CBool True, SSBool "a") @=? (Nothing :: Maybe (Bool, Bool, Bool, Bool))
            ],
          testGroup
            "(,,,,)"
            [ testProperty "(Integer, Integer, Integer, Integer, Integer)" $
                ioProperty . toConForConcreteOkProp @(Integer, Integer, Integer, Integer, Integer),
              testCase "(SBool, SBool, SBool, SBool, SBool)" $ do
                toCon (CBool False, CBool True, CBool False, CBool True, CBool False)
                  @=? Just (False, True, False, True, False)
                toCon (CBool False, CBool True, CBool False, CBool True, SSBool "a")
                  @=? (Nothing :: Maybe (Bool, Bool, Bool, Bool, Bool))
            ],
          testGroup
            "(,,,,,)"
            [ testProperty "(Integer, Integer, Integer, Integer, Integer, Integer)" $
                ioProperty . toConForConcreteOkProp @(Integer, Integer, Integer, Integer, Integer, Integer),
              testCase "(SBool, SBool, SBool, SBool, SBool, SBool)" $ do
                toCon (CBool True, CBool False, CBool True, CBool False, CBool True, CBool False)
                  @=? Just (True, False, True, False, True, False)
                toCon (CBool True, CBool False, CBool True, CBool False, CBool True, SSBool "a")
                  @=? (Nothing :: Maybe (Bool, Bool, Bool, Bool, Bool, Bool))
            ],
          testGroup
            "(,,,,,,)"
            [ testProperty "(Integer, Integer, Integer, Integer, Integer, Integer, Integer)" $
                ioProperty . toConForConcreteOkProp @(Integer, Integer, Integer, Integer, Integer, Integer, Integer),
              testCase "(SBool, SBool, SBool, SBool, SBool, SBool, SBool)" $ do
                toCon (CBool False, CBool True, CBool False, CBool True, CBool False, CBool True, CBool False)
                  @=? Just (False, True, False, True, False, True, False)
                toCon (CBool False, CBool True, CBool False, CBool True, CBool False, CBool True, SSBool "a")
                  @=? (Nothing :: Maybe (Bool, Bool, Bool, Bool, Bool, Bool, Bool))
            ],
          testGroup
            "(,,,,,,,)"
            [ testProperty "(Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer)" $
                ioProperty . toConForConcreteOkProp @(Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer),
              testCase "(SBool, SBool, SBool, SBool, SBool, SBool, SBool, SBool)" $ do
                toCon (CBool True, CBool False, CBool True, CBool False, CBool True, CBool False, CBool True, CBool False)
                  @=? Just (True, False, True, False, True, False, True, False)
                toCon (CBool True, CBool False, CBool True, CBool False, CBool True, CBool False, CBool True, SSBool "a")
                  @=? (Nothing :: Maybe (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool))
            ],
          testGroup
            "Sum"
            [ testProperty "Sum Maybe Maybe Integer" $
                ioProperty . \(v :: Either (Maybe Integer) (Maybe Integer)) -> toConForConcreteOkProp $ case v of
                  Left x -> InL x
                  Right x -> InR x,
              testCase "Sum Maybe (Either SBool) SBool" $ do
                toCon (InL (Just (CBool True)) :: Sum Maybe (Either SBool) SBool)
                  @=? (Just (InL (Just True)) :: Maybe (Sum Maybe (Either Bool) Bool))
                toCon (InL (Just (SSBool "a")) :: Sum Maybe (Either SBool) SBool)
                  @=? (Nothing :: Maybe (Sum Maybe (Either Bool) Bool))
                toCon (InR (Left (CBool True)) :: Sum Maybe (Either SBool) SBool)
                  @=? (Just (InR (Left True)) :: Maybe (Sum Maybe (Either Bool) Bool))
                toCon (InR (Right (CBool True)) :: Sum Maybe (Either SBool) SBool)
                  @=? (Just (InR (Right True)) :: Maybe (Sum Maybe (Either Bool) Bool))
                toCon (InR (Left (SSBool "a")) :: Sum Maybe (Either SBool) SBool)
                  @=? (Nothing :: Maybe (Sum Maybe (Either Bool) Bool))
                toCon (InR (Right (SSBool "a")) :: Sum Maybe (Either SBool) SBool)
                  @=? (Nothing :: Maybe (Sum Maybe (Either Bool) Bool))
            ],
          testGroup
            "WriterT"
            [ testGroup
                "Lazy"
                [ testProperty "WriterT Integer (Either Integer) Integer" $
                    ioProperty . \(v :: Either Integer (Integer, Integer)) -> toConForConcreteOkProp $ WriterLazy.WriterT v,
                  testCase "WriterT SBool (Either SBool) SBool" $ do
                    toCon (WriterLazy.WriterT $ Left $ CBool True :: WriterLazy.WriterT SBool (Either SBool) SBool)
                      @=? Just (WriterLazy.WriterT $ Left True :: WriterLazy.WriterT Bool (Either Bool) Bool)
                    toCon (WriterLazy.WriterT $ Left $ SSBool "a" :: WriterLazy.WriterT SBool (Either SBool) SBool)
                      @=? (Nothing :: Maybe (WriterLazy.WriterT Bool (Either Bool) Bool))
                    toCon (WriterLazy.WriterT $ Right (CBool True, CBool True) :: WriterLazy.WriterT SBool (Either SBool) SBool)
                      @=? Just (WriterLazy.WriterT $ Right (True, True) :: WriterLazy.WriterT Bool (Either Bool) Bool)
                    toCon (WriterLazy.WriterT $ Right (SSBool "a", CBool True) :: WriterLazy.WriterT SBool (Either SBool) SBool)
                      @=? (Nothing :: Maybe (WriterLazy.WriterT Bool (Either Bool) Bool))
                    toCon (WriterLazy.WriterT $ Right (CBool True, SSBool "a") :: WriterLazy.WriterT SBool (Either SBool) SBool)
                      @=? (Nothing :: Maybe (WriterLazy.WriterT Bool (Either Bool) Bool))
                    toCon (WriterLazy.WriterT $ Right (SSBool "a", SSBool "b") :: WriterLazy.WriterT SBool (Either SBool) SBool)
                      @=? (Nothing :: Maybe (WriterLazy.WriterT Bool (Either Bool) Bool))
                ],
              testGroup
                "Strict"
                [ testProperty "WriterT Integer (Either Integer) Integer" $
                    ioProperty . \(v :: Either Integer (Integer, Integer)) -> toConForConcreteOkProp $ WriterStrict.WriterT v,
                  testCase "WriterT SBool (Either SBool) SBool" $ do
                    toCon (WriterStrict.WriterT $ Left $ CBool True :: WriterStrict.WriterT SBool (Either SBool) SBool)
                      @=? Just (WriterStrict.WriterT $ Left True :: WriterStrict.WriterT Bool (Either Bool) Bool)
                    toCon (WriterStrict.WriterT $ Left $ SSBool "a" :: WriterStrict.WriterT SBool (Either SBool) SBool)
                      @=? (Nothing :: Maybe (WriterStrict.WriterT Bool (Either Bool) Bool))
                    toCon (WriterStrict.WriterT $ Right (CBool True, CBool True) :: WriterStrict.WriterT SBool (Either SBool) SBool)
                      @=? Just (WriterStrict.WriterT $ Right (True, True) :: WriterStrict.WriterT Bool (Either Bool) Bool)
                    toCon (WriterStrict.WriterT $ Right (SSBool "a", CBool True) :: WriterStrict.WriterT SBool (Either SBool) SBool)
                      @=? (Nothing :: Maybe (WriterStrict.WriterT Bool (Either Bool) Bool))
                    toCon (WriterStrict.WriterT $ Right (CBool True, SSBool "a") :: WriterStrict.WriterT SBool (Either SBool) SBool)
                      @=? (Nothing :: Maybe (WriterStrict.WriterT Bool (Either Bool) Bool))
                    toCon (WriterStrict.WriterT $ Right (SSBool "a", SSBool "b") :: WriterStrict.WriterT SBool (Either SBool) SBool)
                      @=? (Nothing :: Maybe (WriterStrict.WriterT Bool (Either Bool) Bool))
                ]
            ],
          testGroup
            "Identity"
            [ testProperty "Identity Integer" $
                ioProperty . \(v :: Integer) -> toConForConcreteOkProp $ Identity v,
              testCase "Identity SBool" $ do
                toCon (Identity $ CBool True) @=? Just (Identity True)
                toCon (Identity $ SSBool "a") @=? (Nothing :: Maybe (Identity Bool))
            ],
          testGroup
            "IdentityT"
            [ testProperty "IdentityT (Either Integer) Integer" $
                ioProperty . \(v :: Either Integer Integer) -> toConForConcreteOkProp $ IdentityT v,
              testCase "IdentityT (Either SBool) SBool" $ do
                toCon (IdentityT $ Left $ CBool True :: IdentityT (Either SBool) SBool)
                  @=? Just (IdentityT $ Left True :: IdentityT (Either Bool) Bool)
                toCon (IdentityT $ Left $ SSBool "a" :: IdentityT (Either SBool) SBool)
                  @=? (Nothing :: Maybe (IdentityT (Either Bool) Bool))
                toCon (IdentityT $ Right $ CBool True :: IdentityT (Either SBool) SBool)
                  @=? Just (IdentityT $ Right True :: IdentityT (Either Bool) Bool)
                toCon (IdentityT $ Right $ SSBool "a" :: IdentityT (Either SBool) SBool)
                  @=? (Nothing :: Maybe (IdentityT (Either Bool) Bool))
            ]
        ]
    ]
