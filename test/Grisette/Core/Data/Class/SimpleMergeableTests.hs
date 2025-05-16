{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Core.Data.Class.SimpleMergeableTests
  ( simpleMergeableTests,
  )
where

import Control.Monad.Cont (ContT (ContT, runContT))
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Identity
  ( Identity (Identity, runIdentity),
    IdentityT (IdentityT, runIdentityT),
  )
import qualified Control.Monad.RWS.Lazy as RWSTLazy
import qualified Control.Monad.RWS.Strict as RWSTStrict
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT))
import qualified Control.Monad.State.Lazy as StateLazy
import qualified Control.Monad.State.Strict as StateStrict
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.Monoid as Monoid
import GHC.Generics (Generic)
import Generics.Deriving (Default (Default))
import Grisette
  ( ITEOp (symIte),
    LogicalOp (symNot, (.&&), (.||)),
    Mergeable,
    SimpleMergeable (mrgIte),
    Solvable (con, ssym),
    SymBool,
    Union,
    mrgIf,
    mrgIte1,
    mrgSingle,
  )
import Grisette.Core.Data.Class.TestValues (ssymBool)
import Grisette.Internal.Core.Data.Class.AsKey (AsKey, AsKey1)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

newtype AndMonoidSymBool = AndMonoidSymBool (AsKey SymBool)
  deriving (Show, Generic, Eq)
  deriving (Mergeable) via (Default AndMonoidSymBool)

instance Semigroup AndMonoidSymBool where
  (AndMonoidSymBool a) <> (AndMonoidSymBool b) = AndMonoidSymBool (a .&& b)

instance Monoid AndMonoidSymBool where
  mempty = AndMonoidSymBool $ con True

simpleMergeableTests :: Test
simpleMergeableTests =
  testGroup
    "SimpleMergeable"
    [ testGroup
        "SimpleMergeable for common types"
        [ testCase "SymBool" $ do
            mrgIte (ssym "a") (ssym "b" :: AsKey SymBool) (ssym "c")
              @?= symIte (ssym "a") (ssym "b") (ssym "c"),
          testCase "()" $ do
            mrgIte (ssym "a") () () @?= (),
          testCase "(SymBool, SymBool)" $ do
            mrgIte
              (ssym "a")
              (ssym "b" :: AsKey SymBool, ssym "d" :: AsKey SymBool)
              (ssym "c", ssym "e")
              @?= ( symIte (ssym "a") (ssym "b") (ssym "c"),
                    symIte (ssym "a") (ssym "d") (ssym "e")
                  ),
          testCase "(SymBool, SymBool, SymBool)" $ do
            mrgIte
              (ssym "a")
              ( ssym "b" :: AsKey SymBool,
                ssym "d" :: AsKey SymBool,
                ssym "f" :: AsKey SymBool
              )
              (ssym "c", ssym "e", ssym "g")
              @?= ( symIte (ssym "a") (ssym "b") (ssym "c"),
                    symIte (ssym "a") (ssym "d") (ssym "e"),
                    symIte (ssym "a") (ssym "f") (ssym "g")
                  ),
          testCase "(SymBool, SymBool, SymBool, SymBool)" $ do
            mrgIte
              (ssym "a")
              ( ssym "b" :: AsKey SymBool,
                ssym "d" :: AsKey SymBool,
                ssym "f" :: AsKey SymBool,
                ssym "h" :: AsKey SymBool
              )
              (ssym "c", ssym "e", ssym "g", ssym "i")
              @?= ( symIte (ssym "a") (ssym "b") (ssym "c"),
                    symIte (ssym "a") (ssym "d") (ssym "e"),
                    symIte (ssym "a") (ssym "f") (ssym "g"),
                    symIte (ssym "a") (ssym "h") (ssym "i")
                  ),
          testCase "(SymBool, SymBool, SymBool, SymBool, SymBool)" $ do
            mrgIte
              (ssym "a")
              ( ssym "b" :: AsKey SymBool,
                ssym "d" :: AsKey SymBool,
                ssym "f" :: AsKey SymBool,
                ssym "h" :: AsKey SymBool,
                ssym "j" :: AsKey SymBool
              )
              ( ssym "c",
                ssym "e",
                ssym "g",
                ssym "i",
                ssym "k"
              )
              @?= ( symIte (ssym "a") (ssym "b") (ssym "c"),
                    symIte (ssym "a") (ssym "d") (ssym "e"),
                    symIte (ssym "a") (ssym "f") (ssym "g"),
                    symIte (ssym "a") (ssym "h") (ssym "i"),
                    symIte (ssym "a") (ssym "j") (ssym "k")
                  ),
          testCase "(SymBool, SymBool, SymBool, SymBool, SymBool, SymBool)" $ do
            mrgIte
              (ssym "a")
              ( ssym "b" :: AsKey SymBool,
                ssym "d" :: AsKey SymBool,
                ssym "f" :: AsKey SymBool,
                ssym "h" :: AsKey SymBool,
                ssym "j" :: AsKey SymBool,
                ssym "l" :: AsKey SymBool
              )
              ( ssym "c",
                ssym "e",
                ssym "g",
                ssym "i",
                ssym "k",
                ssym "m"
              )
              @?= ( symIte (ssym "a") (ssym "b") (ssym "c"),
                    symIte (ssym "a") (ssym "d") (ssym "e"),
                    symIte (ssym "a") (ssym "f") (ssym "g"),
                    symIte (ssym "a") (ssym "h") (ssym "i"),
                    symIte (ssym "a") (ssym "j") (ssym "k"),
                    symIte (ssym "a") (ssym "l") (ssym "m")
                  ),
          testCase
            "(SymBool, SymBool, SymBool, SymBool, SymBool, SymBool, SymBool)"
            $ do
              mrgIte
                (ssym "a")
                ( ssym "b" :: AsKey SymBool,
                  ssym "d" :: AsKey SymBool,
                  ssym "f" :: AsKey SymBool,
                  ssym "h" :: AsKey SymBool,
                  ssym "j" :: AsKey SymBool,
                  ssym "l" :: AsKey SymBool,
                  ssym "n" :: AsKey SymBool
                )
                ( ssym "c",
                  ssym "e",
                  ssym "g",
                  ssym "i",
                  ssym "k",
                  ssym "m",
                  ssym "o"
                )
                @?= ( symIte (ssym "a") (ssym "b") (ssym "c"),
                      symIte (ssym "a") (ssym "d") (ssym "e"),
                      symIte (ssym "a") (ssym "f") (ssym "g"),
                      symIte (ssym "a") (ssym "h") (ssym "i"),
                      symIte (ssym "a") (ssym "j") (ssym "k"),
                      symIte (ssym "a") (ssym "l") (ssym "m"),
                      symIte (ssym "a") (ssym "n") (ssym "o")
                    ),
          testCase
            "(SymBool, SymBool, SymBool, SymBool, SymBool, SymBool, SymBool, SymBool)"
            $ do
              mrgIte
                (ssymBool "a")
                ( ssym "b" :: AsKey SymBool,
                  ssym "d" :: AsKey SymBool,
                  ssym "f" :: AsKey SymBool,
                  ssym "h" :: AsKey SymBool,
                  ssym "j" :: AsKey SymBool,
                  ssym "l" :: AsKey SymBool,
                  ssym "n" :: AsKey SymBool,
                  ssym "p" :: AsKey SymBool
                )
                ( ssym "c" :: AsKey SymBool,
                  ssym "e" :: AsKey SymBool,
                  ssym "g" :: AsKey SymBool,
                  ssym "i" :: AsKey SymBool,
                  ssym "k" :: AsKey SymBool,
                  ssym "m" :: AsKey SymBool,
                  ssym "o" :: AsKey SymBool,
                  ssym "q" :: AsKey SymBool
                )
                @?= ( symIte (ssym "a") (ssym "b") (ssym "c"),
                      symIte (ssym "a") (ssym "d") (ssym "e"),
                      symIte (ssym "a") (ssym "f") (ssym "g"),
                      symIte (ssym "a") (ssym "h") (ssym "i"),
                      symIte (ssym "a") (ssym "j") (ssym "k"),
                      symIte (ssym "a") (ssym "l") (ssym "m"),
                      symIte (ssym "a") (ssym "n") (ssym "o"),
                      symIte (ssym "a") (ssym "p") (ssym "q")
                    ),
          testCase "SymBool -> SymBool" $ do
            let f = mrgIte (ssym "a") symNot ((ssym "b" :: AsKey SymBool) .&&)
            f (ssym "c")
              @?= symIte
                (ssym "a")
                (symNot $ ssym "c")
                ((ssym "b") .&& (ssym "c")),
          testCase "MaybeT (Union) SymBool" $ do
            let l :: MaybeT (AsKey1 Union) (AsKey SymBool) =
                  MaybeT
                    ( mrgIf
                        (ssym "b")
                        (mrgSingle Nothing)
                        (mrgSingle $ Just $ ssym "c")
                    )
            let r :: MaybeT (AsKey1 Union) (AsKey SymBool) =
                  MaybeT
                    ( mrgIf
                        (ssym "d")
                        (mrgSingle Nothing)
                        (mrgSingle $ Just $ ssym "e")
                    )
            let res :: MaybeT (AsKey1 Union) (AsKey SymBool) =
                  MaybeT
                    ( mrgIf
                        (ssym "a")
                        ( mrgIf
                            (ssym "b")
                            (mrgSingle Nothing)
                            (mrgSingle $ Just $ ssym "c")
                        )
                        ( mrgIf
                            (ssym "d")
                            (mrgSingle Nothing)
                            (mrgSingle $ Just $ ssym "e")
                        )
                    )
            mrgIte (ssym "a") l r @?= res
            mrgIte1 (ssym "a") l r @?= res
            mrgIf (ssym "a") l r @?= res,
          testCase "ExceptT SymBool (Union) SymBool" $ do
            let l :: ExceptT (AsKey SymBool) (AsKey1 Union) (AsKey SymBool) =
                  ExceptT
                    ( mrgIf
                        (ssym "b")
                        (mrgSingle $ Left $ ssym "c")
                        (mrgSingle $ Right $ ssym "d")
                    )
            let r =
                  ExceptT
                    ( mrgIf
                        (ssym "e")
                        (mrgSingle $ Left $ ssym "f")
                        (mrgSingle $ Right $ ssym "g")
                    )
            let res =
                  ExceptT
                    ( mrgIf
                        (ssym "a")
                        ( mrgIf
                            (ssym "b")
                            (mrgSingle $ Left $ ssym "c")
                            (mrgSingle $ Right $ ssym "d")
                        )
                        ( mrgIf
                            (ssym "e")
                            (mrgSingle $ Left $ ssym "f")
                            (mrgSingle $ Right $ ssym "g")
                        )
                    )
            mrgIte (ssym "a") l r @?= res
            mrgIte1 (ssym "a") l r @?= res
            mrgIf (ssym "a") l r @?= res,
          testGroup
            "StateT Integer (Union) SymBool"
            [ testCase "Lazy" $ do
                let st1 :: StateLazy.StateT Integer (AsKey1 Union) (AsKey SymBool) =
                      StateLazy.StateT $ \(x :: Integer) ->
                        mrgSingle (ssym "a", x + 2)
                let st2 :: StateLazy.StateT Integer (AsKey1 Union) (AsKey SymBool) =
                      StateLazy.StateT $ \(x :: Integer) ->
                        mrgSingle (ssym "b", x * 2)
                let st3 = mrgIte (ssym "c") st1 st2
                let st31 = mrgIte1 (ssym "c") st1 st2
                let st3u1 = mrgIf (ssym "c") st1 st2
                StateLazy.runStateT st3 2
                  @?= mrgSingle
                    (symIte (ssym "c") (ssym "a") (ssym "b"), 4)
                StateLazy.runStateT st3 3
                  @?= mrgIf
                    (ssym "c")
                    (mrgSingle (ssym "a", 5))
                    (mrgSingle (ssym "b", 6))
                StateLazy.runStateT st31 2
                  @?= mrgSingle
                    (symIte (ssym "c") (ssym "a") (ssym "b"), 4)
                StateLazy.runStateT st31 3
                  @?= mrgIf
                    (ssym "c")
                    (mrgSingle (ssym "a", 5))
                    (mrgSingle (ssym "b", 6))
                StateLazy.runStateT st3u1 2
                  @?= mrgSingle
                    (symIte (ssym "c") (ssym "a") (ssym "b"), 4)
                StateLazy.runStateT st3u1 3
                  @?= mrgIf
                    (ssym "c")
                    (mrgSingle (ssym "a", 5))
                    (mrgSingle (ssym "b", 6)),
              testCase "Strict" $ do
                let st1 :: StateStrict.StateT Integer (AsKey1 Union) (AsKey SymBool) =
                      StateStrict.StateT $ \(x :: Integer) ->
                        mrgSingle (ssym "a", x + 2)
                let st2 :: StateStrict.StateT Integer (AsKey1 Union) (AsKey SymBool) =
                      StateStrict.StateT $ \(x :: Integer) ->
                        mrgSingle (ssym "b", x * 2)
                let st3 = mrgIte (ssym "c") st1 st2
                let st31 = mrgIte1 (ssym "c") st1 st2
                let st3u1 = mrgIf (ssym "c") st1 st2
                StateStrict.runStateT st3 2
                  @?= mrgSingle
                    (symIte (ssym "c") (ssym "a") (ssym "b"), 4)
                StateStrict.runStateT st3 3
                  @?= mrgIf
                    (ssym "c")
                    (mrgSingle (ssym "a", 5))
                    (mrgSingle (ssym "b", 6))
                StateStrict.runStateT st31 2
                  @?= mrgSingle
                    (symIte (ssym "c") (ssym "a") (ssym "b"), 4)
                StateStrict.runStateT st31 3
                  @?= mrgIf
                    (ssym "c")
                    (mrgSingle (ssym "a", 5))
                    (mrgSingle (ssym "b", 6))
                StateStrict.runStateT st3u1 2
                  @?= mrgSingle
                    (symIte (ssym "c") (ssym "a") (ssym "b"), 4)
                StateStrict.runStateT st3u1 3
                  @?= mrgIf
                    (ssym "c")
                    (mrgSingle (ssym "a", 5))
                    (mrgSingle (ssym "b", 6))
            ],
          testCase "ContT (SymBool, Integer) (Union) (SymBool, Integer)" $ do
            let c1 :: ContT (AsKey SymBool, Integer) (AsKey1 Union) (AsKey SymBool, Integer) =
                  ContT $ \f -> f (ssym "a", 2)
            let c2 :: ContT (AsKey SymBool, Integer) (AsKey1 Union) (AsKey SymBool, Integer) =
                  ContT $ \f -> f (ssym "b", 3)
            let c3 = mrgIte (ssym "c") c1 c2
            let c3u1 = mrgIf (ssym "c") c1 c2
            let r =
                  mrgIf
                    (ssym "c")
                    ( mrgIf
                        (ssym "p")
                        (mrgSingle (ssym "a", 2))
                        (mrgSingle (symNot $ ssym "a", 3))
                    )
                    ( mrgIf
                        (ssym "p")
                        (mrgSingle (ssym "b", 3))
                        (mrgSingle (symNot $ ssym "b", 4))
                    )
            let f (a, x) =
                  mrgIf
                    (ssym "p")
                    (mrgSingle (a, x))
                    (mrgSingle (symNot a, x + 1)) ::
                    AsKey1 Union (AsKey SymBool, Integer)
            runContT c3 f @?= r
            runContT c3u1 f @?= r,
          testGroup
            "RWST (Integer, SymBool) (Monoid.Sum Integer, AndMonoidSymBool) (Integer, SymBool) (Union) (Integer, SymBool)"
            [ testCase "Lazy" $ do
                let rws1 ::
                      RWSTLazy.RWST
                        (Integer, AsKey SymBool)
                        (Monoid.Sum Integer, AndMonoidSymBool)
                        (Integer, AsKey SymBool)
                        (AsKey1 Union)
                        (Integer, AsKey SymBool) =
                        RWSTLazy.RWST $ \(ir, br) (is, bs) ->
                          mrgSingle
                            ( (ir + is, br .&& bs),
                              (ir - is, br .|| bs),
                              ( Monoid.Sum $ ir * is,
                                AndMonoidSymBool $ bs .&& br
                              )
                            )
                let rws2 ::
                      RWSTLazy.RWST
                        (Integer, AsKey SymBool)
                        (Monoid.Sum Integer, AndMonoidSymBool)
                        (Integer, AsKey SymBool)
                        (AsKey1 Union)
                        (Integer, AsKey SymBool) =
                        RWSTLazy.RWST $ \(ir, br) (is, bs) ->
                          mrgSingle
                            ( (ir + is, br .|| bs),
                              (ir - is, br .&& bs),
                              ( Monoid.Sum $ ir * is,
                                AndMonoidSymBool $ bs .|| br
                              )
                            )
                let rws3 = mrgIte (ssymBool "c") rws1 rws2
                let rws3u1 = mrgIf (ssymBool "c") rws1 rws2

                let res1 ::
                      AsKey1
                        Union
                        ( (Integer, AsKey SymBool),
                          (Integer, AsKey SymBool),
                          (Monoid.Sum Integer, AndMonoidSymBool)
                        ) =
                        mrgIf
                          (ssym "c")
                          ( mrgSingle
                              ( (1, ssym "a" .&& ssym "b"),
                                (-1, ssym "a" .|| ssym "b"),
                                ( 0,
                                  AndMonoidSymBool $
                                    ssym "b" .&& ssym "a"
                                )
                              )
                          )
                          ( mrgSingle
                              ( (1, ssym "a" .|| ssym "b"),
                                (-1, ssym "a" .&& ssym "b"),
                                ( 0,
                                  AndMonoidSymBool $
                                    ssym "b" .|| ssym "a"
                                )
                              )
                          )
                RWSTLazy.runRWST rws3 (0, ssym "a") (1, ssym "b")
                  @?= res1
                RWSTLazy.runRWST rws3u1 (0, ssym "a") (1, ssym "b")
                  @?= res1,
              testCase "Strict" $ do
                let rws1 ::
                      RWSTStrict.RWST
                        (Integer, AsKey SymBool)
                        (Monoid.Sum Integer, AndMonoidSymBool)
                        (Integer, AsKey SymBool)
                        (AsKey1 Union)
                        (Integer, AsKey SymBool) =
                        RWSTStrict.RWST $ \(ir, br) (is, bs) ->
                          mrgSingle
                            ( (ir + is, br .&& bs),
                              (ir - is, br .|| bs),
                              ( Monoid.Sum $ ir * is,
                                AndMonoidSymBool $ bs .&& br
                              )
                            )
                let rws2 ::
                      RWSTStrict.RWST
                        (Integer, AsKey SymBool)
                        (Monoid.Sum Integer, AndMonoidSymBool)
                        (Integer, AsKey SymBool)
                        (AsKey1 Union)
                        (Integer, AsKey SymBool) =
                        RWSTStrict.RWST $ \(ir, br) (is, bs) ->
                          mrgSingle
                            ( (ir + is, br .|| bs),
                              (ir - is, br .&& bs),
                              ( Monoid.Sum $ ir * is,
                                AndMonoidSymBool $ bs .|| br
                              )
                            )
                let rws3 = mrgIte (ssymBool "c") rws1 rws2
                let rws3u1 = mrgIf (ssymBool "c") rws1 rws2

                let res1 ::
                      AsKey1
                        Union
                        ( (Integer, AsKey SymBool),
                          (Integer, AsKey SymBool),
                          (Monoid.Sum Integer, AndMonoidSymBool)
                        ) =
                        mrgIf
                          (ssymBool "c")
                          ( mrgSingle
                              ( (1, ssym "a" .&& ssym "b"),
                                (-1, ssym "a" .|| ssym "b"),
                                ( 0,
                                  AndMonoidSymBool $
                                    ssym "b" .&& ssym "a"
                                )
                              )
                          )
                          ( mrgSingle
                              ( (1, ssym "a" .|| ssym "b"),
                                (-1, ssym "a" .&& ssym "b"),
                                ( 0,
                                  AndMonoidSymBool $
                                    ssym "b" .|| ssym "a"
                                )
                              )
                          )
                RWSTStrict.runRWST rws3 (0, ssym "a") (1, ssym "b")
                  @?= res1
                RWSTStrict.runRWST rws3u1 (0, ssym "a") (1, ssym "b")
                  @?= res1
            ],
          testGroup
            "WriterT (Monoid.Sum Integer) (Union) SymBool"
            [ testCase "Lazy" $ do
                let st1 ::
                      WriterLazy.WriterT
                        (Monoid.Sum Integer)
                        (AsKey1 Union)
                        (AsKey SymBool) =
                        WriterLazy.WriterT $ mrgSingle (ssym "a", 1)
                let st2 ::
                      WriterLazy.WriterT
                        (Monoid.Sum Integer)
                        (AsKey1 Union)
                        (AsKey SymBool) =
                        WriterLazy.WriterT $ mrgSingle (ssym "b", 2)
                let st3 ::
                      WriterLazy.WriterT
                        (Monoid.Sum Integer)
                        (AsKey1 Union)
                        (AsKey SymBool) =
                        WriterLazy.WriterT $ mrgSingle (ssym "c", 1)
                let st4 = mrgIte (ssym "d") st1 st2
                let st41 = mrgIte1 (ssym "d") st1 st2
                let st4u1 = mrgIf (ssym "d") st1 st2
                let st5 = mrgIte (ssym "d") st1 st3
                let st51 = mrgIte1 (ssym "d") st1 st3
                let st5u1 = mrgIf (ssym "d") st1 st3
                WriterLazy.runWriterT st4
                  @?= mrgIf
                    (ssym "d")
                    (mrgSingle (ssym "a", 1))
                    (mrgSingle (ssym "b", 2))
                WriterLazy.runWriterT st41
                  @?= mrgIf
                    (ssym "d")
                    (mrgSingle (ssym "a", 1))
                    (mrgSingle (ssym "b", 2))
                WriterLazy.runWriterT st4u1
                  @?= mrgIf
                    (ssym "d")
                    (mrgSingle (ssym "a", 1))
                    (mrgSingle (ssym "b", 2))
                WriterLazy.runWriterT st5
                  @?= mrgSingle
                    (symIte (ssym "d") (ssym "a") (ssym "c"), 1)
                WriterLazy.runWriterT st51
                  @?= mrgSingle
                    (symIte (ssym "d") (ssym "a") (ssym "c"), 1)
                WriterLazy.runWriterT st5u1
                  @?= mrgSingle
                    (symIte (ssym "d") (ssym "a") (ssym "c"), 1),
              testCase "Strict" $ do
                let st1 ::
                      WriterStrict.WriterT
                        (Monoid.Sum Integer)
                        (AsKey1 Union)
                        (AsKey SymBool) =
                        WriterStrict.WriterT $ mrgSingle (ssym "a", 1)
                let st2 ::
                      WriterStrict.WriterT
                        (Monoid.Sum Integer)
                        (AsKey1 Union)
                        (AsKey SymBool) =
                        WriterStrict.WriterT $ mrgSingle (ssym "b", 2)
                let st3 ::
                      WriterStrict.WriterT
                        (Monoid.Sum Integer)
                        (AsKey1 Union)
                        (AsKey SymBool) =
                        WriterStrict.WriterT $ mrgSingle (ssym "c", 1)
                let st4 = mrgIte (ssym "d") st1 st2
                let st41 = mrgIte1 (ssym "d") st1 st2
                let st4u1 = mrgIf (ssym "d") st1 st2
                let st5 = mrgIte (ssym "d") st1 st3
                let st51 = mrgIte1 (ssym "d") st1 st3
                let st5u1 = mrgIf (ssym "d") st1 st3
                WriterStrict.runWriterT st4
                  @?= mrgIf
                    (ssym "d")
                    (mrgSingle (ssym "a", 1))
                    (mrgSingle (ssym "b", 2))
                WriterStrict.runWriterT st41
                  @?= mrgIf
                    (ssym "d")
                    (mrgSingle (ssym "a", 1))
                    (mrgSingle (ssym "b", 2))
                WriterStrict.runWriterT st4u1
                  @?= mrgIf
                    (ssym "d")
                    (mrgSingle (ssym "a", 1))
                    (mrgSingle (ssym "b", 2))
                WriterStrict.runWriterT st5
                  @?= mrgSingle
                    (symIte (ssym "d") (ssym "a") (ssym "c"), 1)
                WriterStrict.runWriterT st51
                  @?= mrgSingle
                    (symIte (ssym "d") (ssym "a") (ssym "c"), 1)
                WriterStrict.runWriterT st5u1
                  @?= mrgSingle
                    (symIte (ssym "d") (ssym "a") (ssym "c"), 1)
            ],
          testCase "ReaderT Integer (Union) Integer" $ do
            let r1 :: ReaderT Integer (AsKey1 Union) Integer =
                  ReaderT $ \(x :: Integer) -> mrgSingle $ x + 2
            let r2 :: ReaderT Integer (AsKey1 Union) Integer =
                  ReaderT $ \(x :: Integer) -> mrgSingle $ x * 2
            let r3 = mrgIte (ssym "c") r1 r2
            let r3u1 = mrgIf (ssym "c") r1 r2
            runReaderT r3 2 @?= mrgSingle 4
            runReaderT r3 3 @?= mrgIf (ssym "c") (mrgSingle 5) (mrgSingle 6)
            runReaderT r3u1 2 @?= mrgSingle 4
            runReaderT r3u1 3
              @?= mrgIf
                (ssym "c")
                (mrgSingle 5)
                (mrgSingle 6)

            let r4 :: ReaderT (AsKey SymBool) (AsKey1 Union) (AsKey SymBool) =
                  ReaderT $ \x -> mrgSingle $ x .&& ssym "x"
            let r5 :: ReaderT (AsKey SymBool) (AsKey1 Union) (AsKey SymBool) =
                  ReaderT $ \x -> mrgSingle $ x .|| ssym "y"
            let r61 = mrgIte1 (ssym "c") r4 r5
            runReaderT r61 (ssym "a")
              @?= mrgSingle
                ( symIte
                    (ssym "c")
                    (ssym "a" .&& ssym "x")
                    (ssym "a" .|| ssym "y")
                ),
          testCase "Identity SymBool" $ do
            let i1 :: Identity (AsKey SymBool) = Identity $ ssym "a"
            let i2 :: Identity (AsKey SymBool) = Identity $ ssym "b"
            let i3 = mrgIte (ssym "c") i1 i2
            let i31 = mrgIte1 (ssym "c") i1 i2
            runIdentity i3 @?= symIte (ssym "c") (ssym "a") (ssym "b")
            runIdentity i31
              @?= symIte (ssym "c") (ssym "a") (ssym "b"),
          testCase "IdentityT (Union) SymBool" $ do
            let i1 :: IdentityT (AsKey1 Union) (AsKey SymBool) =
                  IdentityT $ mrgSingle $ ssym "a"
            let i2 :: IdentityT (AsKey1 Union) (AsKey SymBool) =
                  IdentityT $ mrgSingle $ ssym "b"
            let i3 = mrgIte (ssym "c") i1 i2
            let i31 = mrgIte1 (ssym "c") i1 i2
            let i3u1 = mrgIf (ssym "c") i1 i2
            runIdentityT i3
              @?= mrgSingle (symIte (ssym "c") (ssym "a") (ssym "b"))
            runIdentityT i31
              @?= mrgSingle (symIte (ssym "c") (ssym "a") (ssym "b"))
            runIdentityT i3u1
              @?= mrgSingle (symIte (ssym "c") (ssym "a") (ssym "b"))
        ]
    ]
