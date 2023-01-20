{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Core.Data.Class.ToSymTests where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import qualified Control.Monad.State.Lazy as StateLazy
import qualified Control.Monad.State.Strict as StateStrict
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.ByteString.Char8 as C
import Data.Foldable
import Data.Functor.Sum
import Data.Int
import Data.Word
import Grisette.Core.Data.Class.ToSym
import Grisette.TestUtils.SBool
import Grisette.TestUtils.ToSym
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

toSymTests :: TestTree
toSymTests =
  testGroup
    "ToSymTests"
    [ testGroup
        "ToSym for common types"
        [ testGroup
            "SBool"
            [ testCase "From Bool" $ do
                let bools = [True, False]
                traverse_ (\v -> toSym v @=? CBool v) bools,
              testCase "From SBool" $ do
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
                traverse_ (\v -> toSym v @=? v) sbools
            ],
          testProperty "Bool" $ ioProperty . toSymForConcreteOkProp @Bool,
          testProperty "Integer" $ ioProperty . toSymForConcreteOkProp @Integer,
          testProperty "Char" $ ioProperty . toSymForConcreteOkProp @Char,
          testProperty "Int" $ ioProperty . toSymForConcreteOkProp @Int,
          testProperty "Int8" $ ioProperty . toSymForConcreteOkProp @Int8,
          testProperty "Int16" $ ioProperty . toSymForConcreteOkProp @Int16,
          testProperty "Int32" $ ioProperty . toSymForConcreteOkProp @Int32,
          testProperty "Int64" $ ioProperty . toSymForConcreteOkProp @Int64,
          testProperty "Word" $ ioProperty . toSymForConcreteOkProp @Word,
          testProperty "Word8" $ ioProperty . toSymForConcreteOkProp @Word8,
          testProperty "Word16" $ ioProperty . toSymForConcreteOkProp @Word16,
          testProperty "Word32" $ ioProperty . toSymForConcreteOkProp @Word32,
          testProperty "Word64" $ ioProperty . toSymForConcreteOkProp @Word64,
          testProperty "()" $ ioProperty . toSymForConcreteOkProp @(),
          testProperty "ByteString" $ ioProperty . \(v :: String) -> toSymForConcreteOkProp (C.pack v),
          testGroup
            "List"
            [ testProperty "[Integer]" $ ioProperty . toSymForConcreteOkProp @[Integer],
              testCase "[SBool]" $ do
                toSym ([] :: [Bool]) @=? ([] :: [SBool])
                toSym ([True, False] :: [Bool]) @=? ([CBool True, CBool False] :: [SBool])
            ],
          testGroup
            "Maybe"
            [ testProperty "Maybe Integer" $ ioProperty . toSymForConcreteOkProp @(Maybe Integer),
              testCase "Maybe SBool" $ do
                toSym (Nothing :: Maybe Bool) @=? (Nothing :: Maybe SBool)
                toSym (Just True :: Maybe Bool) @=? (Just $ CBool True :: Maybe SBool)
            ],
          testGroup
            "Either"
            [ testProperty "Either Integer Integer" $
                ioProperty . toSymForConcreteOkProp @(Either Integer Integer),
              testCase "Eithe SBool SBool" $ do
                toSym (Left True :: Either Bool Bool) @=? (Left $ CBool True :: Either SBool SBool)
                toSym (Right True :: Either Bool Bool) @=? (Right $ CBool True :: Either SBool SBool)
            ],
          testGroup
            "MaybeT"
            [ testProperty "MaybeT Maybe Integer" $
                ioProperty . \(v :: Maybe (Maybe Integer)) -> toSymForConcreteOkProp $ MaybeT v,
              testCase "MaybeT Maybe SBool" $ do
                toSym (MaybeT Nothing :: MaybeT Maybe Bool) @=? (MaybeT Nothing :: MaybeT Maybe SBool)
                toSym (MaybeT $ Just Nothing :: MaybeT Maybe Bool) @=? (MaybeT $ Just Nothing :: MaybeT Maybe SBool)
                toSym (MaybeT $ Just $ Just True :: MaybeT Maybe Bool)
                  @=? (MaybeT $ Just $ Just $ CBool True :: MaybeT Maybe SBool)
            ],
          testGroup
            "ExceptT"
            [ testProperty "ExceptT Integer Maybe Integer" $
                ioProperty . \(v :: Maybe (Either Integer Integer)) -> toSymForConcreteOkProp $ ExceptT v,
              testCase "ExceptT SBool Maybe SBool" $ do
                toSym (ExceptT Nothing :: ExceptT Bool Maybe Bool) @=? (ExceptT Nothing :: ExceptT SBool Maybe SBool)
                toSym (ExceptT $ Just $ Left True :: ExceptT Bool Maybe Bool)
                  @=? (ExceptT $ Just $ Left $ CBool True :: ExceptT SBool Maybe SBool)
                toSym (ExceptT $ Just $ Right False :: ExceptT Bool Maybe Bool)
                  @=? (ExceptT $ Just $ Right $ CBool False :: ExceptT SBool Maybe SBool)
            ],
          testGroup
            "(,)"
            [ testProperty "(Integer, Integer)" $
                ioProperty . toSymForConcreteOkProp @(Integer, Integer),
              testCase "(SBool, SBool)" $
                toSym (True, False) @=? (CBool True, CBool False)
            ],
          testGroup
            "(,,)"
            [ testProperty "(Integer, Integer, Integer)" $
                ioProperty . toSymForConcreteOkProp @(Integer, Integer, Integer),
              testCase "(SBool, SBool, SBool)" $
                toSym (True, False, True) @=? (CBool True, CBool False, CBool True)
            ],
          testGroup
            "(,,,)"
            [ testProperty "(Integer, Integer, Integer, Integer)" $
                ioProperty . toSymForConcreteOkProp @(Integer, Integer, Integer, Integer),
              testCase "(SBool, SBool, SBool, SBool)" $
                toSym (True, False, True, False) @=? (CBool True, CBool False, CBool True, CBool False)
            ],
          testGroup
            "(,,,,)"
            [ testProperty "(Integer, Integer, Integer, Integer, Integer)" $
                ioProperty . toSymForConcreteOkProp @(Integer, Integer, Integer, Integer, Integer),
              testCase "(SBool, SBool, SBool, SBool, SBool)" $
                toSym (True, False, True, False, True) @=? (CBool True, CBool False, CBool True, CBool False, CBool True)
            ],
          testGroup
            "(,,,,,)"
            [ testProperty "(Integer, Integer, Integer, Integer, Integer, Integer)" $
                ioProperty . toSymForConcreteOkProp @(Integer, Integer, Integer, Integer, Integer, Integer),
              testCase "(SBool, SBool, SBool, SBool, SBool, SBool)" $
                toSym (True, False, True, False, True, False)
                  @=? (CBool True, CBool False, CBool True, CBool False, CBool True, CBool False)
            ],
          testGroup
            "(,,,,,,)"
            [ testProperty "(Integer, Integer, Integer, Integer, Integer, Integer, Integer)" $
                ioProperty . toSymForConcreteOkProp @(Integer, Integer, Integer, Integer, Integer, Integer, Integer),
              testCase "(SBool, SBool, SBool, SBool, SBool, SBool, SBool)" $
                toSym (True, False, True, False, True, False, True)
                  @=? (CBool True, CBool False, CBool True, CBool False, CBool True, CBool False, CBool True)
            ],
          testGroup
            "(,,,,,,,)"
            [ testProperty "(Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer)" $
                ioProperty . toSymForConcreteOkProp @(Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer),
              testCase "(SBool, SBool, SBool, SBool, SBool, SBool, SBool, SBool)" $
                toSym (True, False, True, False, True, False, True, False)
                  @=? (CBool True, CBool False, CBool True, CBool False, CBool True, CBool False, CBool True, CBool False)
            ],
          testGroup
            "Sum"
            [ testProperty "Sum Maybe (Either Integer) Integer" $
                ioProperty . \(x :: Either (Maybe Integer) (Either Integer Integer)) ->
                  case x of
                    Left v -> toSymForConcreteOkProp @(Sum Maybe (Either Integer) Integer) (InL v)
                    Right v -> toSymForConcreteOkProp @(Sum Maybe (Either Integer) Integer) (InR v),
              testCase "Sum Maybe (Either SBool) SBool" $ do
                toSym (InL $ Just True :: Sum Maybe (Either Bool) Bool)
                  @=? (InL $ Just $ CBool True :: Sum Maybe (Either SBool) SBool)
                toSym (InR $ Left True :: Sum Maybe (Either Bool) Bool)
                  @=? (InR $ Left $ CBool True :: Sum Maybe (Either SBool) SBool)
                toSym (InR $ Right False :: Sum Maybe (Either Bool) Bool)
                  @=? (InR $ Right $ CBool False :: Sum Maybe (Either SBool) SBool)
            ],
          testProperty "functions" $
            ioProperty . \(f :: [(Either Bool Bool, Either Bool Bool)], x :: Either Bool Bool) ->
              let func [] _ = Left False
                  func ((fk, fv) : _) xv | fk == xv = fv
                  func (_ : fs) xv = func fs xv
               in (toSym (func f x) :: Either SBool SBool) @=? toSym (func f) x,
          testGroup
            "StateT"
            [ testProperty "Lazy" $
                ioProperty . \(f :: [(Bool, Either Bool (Bool, Bool))], x :: Bool) ->
                  let func [] _ = Left False
                      func ((fk, fv) : _) xv | fk == xv = fv
                      func (_ : fs) xv = func fs xv
                      st :: StateLazy.StateT Bool (Either Bool) Bool = StateLazy.StateT (func f)
                   in (StateLazy.runStateT (toSym st) x :: Either SBool (SBool, Bool)) @=? toSym (func f) x,
              testProperty "Strict" $
                ioProperty . \(f :: [(Bool, Either Bool (Bool, Bool))], x :: Bool) ->
                  let func [] _ = Left False
                      func ((fk, fv) : _) xv | fk == xv = fv
                      func (_ : fs) xv = func fs xv
                      st :: StateStrict.StateT Bool (Either Bool) Bool = StateStrict.StateT (func f)
                   in (StateStrict.runStateT (toSym st) x :: Either SBool (SBool, Bool)) @=? toSym (func f) x
            ],
          testGroup
            "WriterT"
            [ testProperty "Lazy" $
                ioProperty . \(f :: Either Bool (Bool, Integer)) ->
                  let w :: WriterLazy.WriterT Integer (Either Bool) Bool = WriterLazy.WriterT f
                   in (WriterLazy.runWriterT (toSym w) :: Either SBool (SBool, Integer)) @=? toSym f,
              testProperty "Strict" $
                ioProperty . \(f :: Either Bool (Bool, Integer)) ->
                  let w :: WriterStrict.WriterT Integer (Either Bool) Bool = WriterStrict.WriterT f
                   in (WriterStrict.runWriterT (toSym w) :: Either SBool (SBool, Integer)) @=? toSym f
            ],
          testProperty "ReaderT" $
            ioProperty . \(f :: [(Bool, Either Bool Bool)], x :: Bool) ->
              let func [] _ = Left False
                  func ((fk, fv) : _) xv | fk == xv = fv
                  func (_ : fs) xv = func fs xv
                  st :: ReaderT Bool (Either Bool) Bool = ReaderT (func f)
               in (runReaderT (toSym st) x :: Either SBool SBool) @=? toSym (func f) x,
          testGroup
            "Identity"
            [ testProperty "Identity Integer" $
                ioProperty
                  . toSymForConcreteOkProp @(Identity Integer),
              testCase "Identity SBool" $ do
                toSym (Identity True :: Identity Bool)
                  @=? (Identity $ CBool True :: Identity SBool)
            ],
          testGroup
            "IdentityT"
            [ testProperty "IdentityT Maybe Integer" $
                ioProperty . \x ->
                  toSymForConcreteOkProp @(IdentityT Maybe Integer) (IdentityT x),
              testCase "IdentityT Maybe SBool" $ do
                toSym (IdentityT (Just True) :: IdentityT Maybe Bool)
                  @=? (IdentityT $ Just $ CBool True :: IdentityT Maybe SBool)
                toSym (IdentityT Nothing :: IdentityT Maybe Bool)
                  @=? (IdentityT Nothing :: IdentityT Maybe SBool)
            ]
        ]
    ]
