{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Core.Data.Class.ToSymTests (toSymTests) where

import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Identity
  ( Identity (Identity),
    IdentityT (IdentityT),
  )
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT))
import qualified Control.Monad.State.Lazy as StateLazy
import qualified Control.Monad.State.Strict as StateStrict
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.ByteString.Char8 as C
import Data.Foldable (traverse_)
import Data.Functor.Sum (Sum (InL, InR))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Stack (HasCallStack)
import Grisette
  ( ITEOp (symIte),
    LogicalOp (symNot, (.&&), (.||)),
    Solvable (con, isym, ssym),
    SymBool,
    SymEq ((.==)),
    ToSym (toSym), Mergeable
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (Assertion, (@?=))
import Test.QuickCheck (ioProperty)

toSymForConcreteOkProp ::
  (HasCallStack, Show v, Eq v, Mergeable v) => v -> Assertion
toSymForConcreteOkProp v = toSym v @?= v

toSymTests :: Test
toSymTests =
  testGroup
    "ToSym"
    [ testGroup
        "ToSym for common types"
        [ testGroup
            "SymBool"
            [ testCase "From Bool" $ do
                let bools = [True, False]
                traverse_ (\v -> toSym v @?= (con v :: SymBool)) bools,
              testCase "From SymBool" $ do
                let sbools :: [SymBool] =
                      [ con True,
                        ssym "a",
                        isym "a" 1,
                        ssym "a" .&& ssym "b",
                        ssym "a" .|| ssym "b",
                        symNot (ssym "a"),
                        (ssym "a" :: SymBool) .== ssym "b",
                        symIte (ssym "a") (ssym "b") (ssym "c")
                      ]
                traverse_ (\v -> toSym v @?= v) sbools
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
          testProperty "ByteString" $
            ioProperty . \(v :: String) ->
              toSymForConcreteOkProp (C.pack v),
          testGroup
            "List"
            [ testProperty "[Integer]" $
                ioProperty
                  . toSymForConcreteOkProp @[Integer],
              testCase "[SymBool]" $ do
                toSym ([] :: [Bool]) @?= ([] :: [SymBool])
                toSym ([True, False] :: [Bool])
                  @?= ([con True, con False] :: [SymBool])
            ],
          testGroup
            "Maybe"
            [ testProperty "Maybe Integer" $
                ioProperty
                  . toSymForConcreteOkProp @(Maybe Integer),
              testCase "Maybe SymBool" $ do
                toSym (Nothing :: Maybe Bool) @?= (Nothing :: Maybe SymBool)
                toSym (Just True :: Maybe Bool)
                  @?= (Just $ con True :: Maybe SymBool)
            ],
          testGroup
            "Either"
            [ testProperty "Either Integer Integer" $
                ioProperty . toSymForConcreteOkProp @(Either Integer Integer),
              testCase "Eithe SymBool SymBool" $ do
                toSym (Left True :: Either Bool Bool)
                  @?= (Left $ con True :: Either SymBool SymBool)
                toSym (Right True :: Either Bool Bool)
                  @?= (Right $ con True :: Either SymBool SymBool)
            ],
          testGroup
            "MaybeT"
            [ testProperty "MaybeT Maybe Integer" $
                ioProperty . \(v :: Maybe (Maybe Integer)) ->
                  toSymForConcreteOkProp $ MaybeT v,
              testCase "MaybeT Maybe SymBool" $ do
                toSym (MaybeT Nothing :: MaybeT Maybe Bool)
                  @?= (MaybeT Nothing :: MaybeT Maybe SymBool)
                toSym (MaybeT $ Just Nothing :: MaybeT Maybe Bool)
                  @?= (MaybeT $ Just Nothing :: MaybeT Maybe SymBool)
                toSym (MaybeT $ Just $ Just True :: MaybeT Maybe Bool)
                  @?= (MaybeT $ Just $ Just $ con True :: MaybeT Maybe SymBool)
            ],
          testGroup
            "ExceptT"
            [ testProperty "ExceptT Integer Maybe Integer" $
                ioProperty . \(v :: Maybe (Either Integer Integer)) ->
                  toSymForConcreteOkProp $ ExceptT v,
              testCase "ExceptT SymBool Maybe SymBool" $ do
                toSym (ExceptT Nothing :: ExceptT Bool Maybe Bool)
                  @?= (ExceptT Nothing :: ExceptT SymBool Maybe SymBool)
                toSym (ExceptT $ Just $ Left True :: ExceptT Bool Maybe Bool)
                  @?= ( ExceptT $ Just $ Left $ con True ::
                          ExceptT SymBool Maybe SymBool
                      )
                toSym (ExceptT $ Just $ Right False :: ExceptT Bool Maybe Bool)
                  @?= ( ExceptT $ Just $ Right $ con False ::
                          ExceptT SymBool Maybe SymBool
                      )
            ],
          testGroup
            "(,)"
            [ testProperty "(Integer, Integer)" $
                ioProperty . toSymForConcreteOkProp @(Integer, Integer),
              testCase "(SymBool, SymBool)" $
                toSym (True, False)
                  @?= (con True :: SymBool, con False :: SymBool)
            ],
          testGroup
            "(,,)"
            [ testProperty "(Integer, Integer, Integer)" $
                ioProperty
                  . toSymForConcreteOkProp
                    @(Integer, Integer, Integer),
              testCase "(SymBool, SymBool, SymBool)" $
                toSym (True, False, True)
                  @?= ( con True :: SymBool,
                        con False :: SymBool,
                        con True :: SymBool
                      )
            ],
          testGroup
            "(,,,)"
            [ testProperty "(Integer, Integer, Integer, Integer)" $
                ioProperty
                  . toSymForConcreteOkProp
                    @(Integer, Integer, Integer, Integer),
              testCase "(SymBool, SymBool, SymBool, SymBool)" $
                toSym (True, False, True, False)
                  @?= ( con True :: SymBool,
                        con False :: SymBool,
                        con True :: SymBool,
                        con False :: SymBool
                      )
            ],
          testGroup
            "(,,,,)"
            [ testProperty "(Integer, Integer, Integer, Integer, Integer)" $
                ioProperty
                  . toSymForConcreteOkProp
                    @(Integer, Integer, Integer, Integer, Integer),
              testCase "(SymBool, SymBool, SymBool, SymBool, SymBool)" $
                toSym (True, False, True, False, True)
                  @?= ( con True :: SymBool,
                        con False :: SymBool,
                        con True :: SymBool,
                        con False :: SymBool,
                        con True :: SymBool
                      )
            ],
          testGroup
            "(,,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer, Integer)"
                $ ioProperty
                  . toSymForConcreteOkProp
                    @(Integer, Integer, Integer, Integer, Integer, Integer),
              testCase
                "(SymBool, SymBool, SymBool, SymBool, SymBool, SymBool)"
                $ toSym (True, False, True, False, True, False)
                  @?= ( con True :: SymBool,
                        con False :: SymBool,
                        con True :: SymBool,
                        con False :: SymBool,
                        con True :: SymBool,
                        con False :: SymBool
                      )
            ],
          testGroup
            "(,,,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer, Integer, Integer)"
                $ ioProperty
                  . toSymForConcreteOkProp
                    @( Integer,
                       Integer,
                       Integer,
                       Integer,
                       Integer,
                       Integer,
                       Integer
                     ),
              testCase "(SymBool, SymBool, SymBool, SymBool, SymBool, SymBool, SymBool)" $
                toSym (True, False, True, False, True, False, True)
                  @?= ( con True :: SymBool,
                        con False :: SymBool,
                        con True :: SymBool,
                        con False :: SymBool,
                        con True :: SymBool,
                        con False :: SymBool,
                        con True :: SymBool
                      )
            ],
          testGroup
            "(,,,,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer)"
                $ ioProperty
                  . toSymForConcreteOkProp
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
                $ toSym (True, False, True, False, True, False, True, False)
                  @?= ( con True :: SymBool,
                        con False :: SymBool,
                        con True :: SymBool,
                        con False :: SymBool,
                        con True :: SymBool,
                        con False :: SymBool,
                        con True :: SymBool,
                        con False :: SymBool
                      )
            ],
          testGroup
            "Sum"
            [ testProperty "Sum Maybe (Either Integer) Integer" $
                ioProperty
                  . \(x :: Either (Maybe Integer) (Either Integer Integer)) ->
                    case x of
                      Left v ->
                        toSymForConcreteOkProp
                          @(Sum Maybe (Either Integer) Integer)
                          (InL v)
                      Right v ->
                        toSymForConcreteOkProp
                          @(Sum Maybe (Either Integer) Integer)
                          (InR v),
              testCase "Sum Maybe (Either SymBool) SymBool" $ do
                toSym (InL $ Just True :: Sum Maybe (Either Bool) Bool)
                  @?= ( InL $ Just $ con True ::
                          Sum Maybe (Either SymBool) SymBool
                      )
                toSym (InR $ Left True :: Sum Maybe (Either Bool) Bool)
                  @?= ( InR $ Left $ con True ::
                          Sum Maybe (Either SymBool) SymBool
                      )
                toSym (InR $ Right False :: Sum Maybe (Either Bool) Bool)
                  @?= ( InR $ Right $ con False ::
                          Sum Maybe (Either SymBool) SymBool
                      )
            ],
          testProperty "functions" $
            ioProperty
              . \( f :: [(Either Bool Bool, Either Bool Bool)],
                   x :: Either Bool Bool
                   ) ->
                  let func [] _ = Left False
                      func ((fk, fv) : _) xv | fk == xv = fv
                      func (_ : fs) xv = func fs xv
                   in (toSym (func f x) :: Either SymBool SymBool)
                        @?= toSym (func f) x,
          testGroup
            "StateT"
            [ testProperty "Lazy" $
                ioProperty
                  . \( f :: [(Bool, Either Bool (Bool, Bool))],
                       x :: Bool
                       ) ->
                      let func [] _ = Left False
                          func ((fk, fv) : _) xv | fk == xv = fv
                          func (_ : fs) xv = func fs xv
                          st :: StateLazy.StateT Bool (Either Bool) Bool =
                            StateLazy.StateT (func f)
                       in ( StateLazy.runStateT (toSym st) x ::
                              Either SymBool (SymBool, Bool)
                          )
                            @?= toSym (func f) x,
              testProperty "Strict" $
                ioProperty
                  . \( f :: [(Bool, Either Bool (Bool, Bool))],
                       x :: Bool
                       ) ->
                      let func [] _ = Left False
                          func ((fk, fv) : _) xv | fk == xv = fv
                          func (_ : fs) xv = func fs xv
                          st :: StateStrict.StateT Bool (Either Bool) Bool =
                            StateStrict.StateT (func f)
                       in ( StateStrict.runStateT (toSym st) x ::
                              Either SymBool (SymBool, Bool)
                          )
                            @?= toSym (func f) x
            ],
          testGroup
            "WriterT"
            [ testProperty "Lazy" $
                ioProperty . \(f :: Either Bool (Bool, Integer)) ->
                  let w :: WriterLazy.WriterT Integer (Either Bool) Bool =
                        WriterLazy.WriterT f
                   in ( WriterLazy.runWriterT (toSym w) ::
                          Either SymBool (SymBool, Integer)
                      )
                        @?= toSym f,
              testProperty "Strict" $
                ioProperty . \(f :: Either Bool (Bool, Integer)) ->
                  let w :: WriterStrict.WriterT Integer (Either Bool) Bool =
                        WriterStrict.WriterT f
                   in ( WriterStrict.runWriterT (toSym w) ::
                          Either SymBool (SymBool, Integer)
                      )
                        @?= toSym f
            ],
          testProperty "ReaderT" $
            ioProperty . \(f :: [(Bool, Either Bool Bool)], x :: Bool) ->
              let func [] _ = Left False
                  func ((fk, fv) : _) xv | fk == xv = fv
                  func (_ : fs) xv = func fs xv
                  st :: ReaderT Bool (Either Bool) Bool = ReaderT (func f)
               in (runReaderT (toSym st) x :: Either SymBool SymBool)
                    @?= toSym (func f) x,
          testGroup
            "Identity"
            [ testProperty "Identity Integer" $
                ioProperty
                  . toSymForConcreteOkProp @(Identity Integer),
              testCase "Identity SymBool" $ do
                toSym (Identity True :: Identity Bool)
                  @?= (Identity $ con True :: Identity SymBool)
            ],
          testGroup
            "IdentityT"
            [ testProperty "IdentityT Maybe Integer" $
                ioProperty . \x ->
                  toSymForConcreteOkProp @(IdentityT Maybe Integer)
                    (IdentityT x),
              testCase "IdentityT Maybe SymBool" $ do
                toSym (IdentityT (Just True) :: IdentityT Maybe Bool)
                  @?= (IdentityT $ Just $ con True :: IdentityT Maybe SymBool)
                toSym (IdentityT Nothing :: IdentityT Maybe Bool)
                  @?= (IdentityT Nothing :: IdentityT Maybe SymBool)
            ]
        ]
    ]
