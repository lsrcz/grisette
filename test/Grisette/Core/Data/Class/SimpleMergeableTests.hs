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
import Grisette.Core.Control.Monad.UnionM (UnionM)
import Grisette.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Core.Data.Class.LogicalOp (LogicalOp (symNot, (.&&), (.||)))
import Grisette.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Core.Data.Class.SimpleMergeable
  ( SimpleMergeable (mrgIte),
    mrgIf,
    mrgIte1,
  )
import Grisette.Core.Data.Class.TestValues (conBool, ssymBool)
import Grisette.Core.Data.Class.TryMerge
  ( mrgSingle,
  )
import Grisette.SymPrim.SymBool (SymBool)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

newtype AndMonoidSymBool = AndMonoidSymBool SymBool
  deriving (Show, Generic, Eq)
  deriving (Mergeable) via (Default AndMonoidSymBool)

instance Semigroup AndMonoidSymBool where
  (AndMonoidSymBool a) <> (AndMonoidSymBool b) = AndMonoidSymBool (a .&& b)

instance Monoid AndMonoidSymBool where
  mempty = AndMonoidSymBool $ conBool True

simpleMergeableTests :: Test
simpleMergeableTests =
  testGroup
    "SimpleMergeable"
    [ testGroup
        "SimpleMergeable for common types"
        [ testCase "SymBool" $ do
            mrgIte (ssymBool "a") (ssymBool "b") (ssymBool "c")
              @?= symIte (ssymBool "a") (ssymBool "b") (ssymBool "c"),
          testCase "()" $ do
            mrgIte (ssymBool "a") () () @?= (),
          testCase "(SymBool, SymBool)" $ do
            mrgIte
              (ssymBool "a")
              (ssymBool "b", ssymBool "d")
              (ssymBool "c", ssymBool "e")
              @?= ( symIte (ssymBool "a") (ssymBool "b") (ssymBool "c"),
                    symIte (ssymBool "a") (ssymBool "d") (ssymBool "e")
                  ),
          testCase "(SymBool, SymBool, SymBool)" $ do
            mrgIte
              (ssymBool "a")
              (ssymBool "b", ssymBool "d", ssymBool "f")
              (ssymBool "c", ssymBool "e", ssymBool "g")
              @?= ( symIte (ssymBool "a") (ssymBool "b") (ssymBool "c"),
                    symIte (ssymBool "a") (ssymBool "d") (ssymBool "e"),
                    symIte (ssymBool "a") (ssymBool "f") (ssymBool "g")
                  ),
          testCase "(SymBool, SymBool, SymBool, SymBool)" $ do
            mrgIte
              (ssymBool "a")
              (ssymBool "b", ssymBool "d", ssymBool "f", ssymBool "h")
              (ssymBool "c", ssymBool "e", ssymBool "g", ssymBool "i")
              @?= ( symIte (ssymBool "a") (ssymBool "b") (ssymBool "c"),
                    symIte (ssymBool "a") (ssymBool "d") (ssymBool "e"),
                    symIte (ssymBool "a") (ssymBool "f") (ssymBool "g"),
                    symIte (ssymBool "a") (ssymBool "h") (ssymBool "i")
                  ),
          testCase "(SymBool, SymBool, SymBool, SymBool, SymBool)" $ do
            mrgIte
              (ssymBool "a")
              ( ssymBool "b",
                ssymBool "d",
                ssymBool "f",
                ssymBool "h",
                ssymBool "j"
              )
              ( ssymBool "c",
                ssymBool "e",
                ssymBool "g",
                ssymBool "i",
                ssymBool "k"
              )
              @?= ( symIte (ssymBool "a") (ssymBool "b") (ssymBool "c"),
                    symIte (ssymBool "a") (ssymBool "d") (ssymBool "e"),
                    symIte (ssymBool "a") (ssymBool "f") (ssymBool "g"),
                    symIte (ssymBool "a") (ssymBool "h") (ssymBool "i"),
                    symIte (ssymBool "a") (ssymBool "j") (ssymBool "k")
                  ),
          testCase "(SymBool, SymBool, SymBool, SymBool, SymBool, SymBool)" $ do
            mrgIte
              (ssymBool "a")
              ( ssymBool "b",
                ssymBool "d",
                ssymBool "f",
                ssymBool "h",
                ssymBool "j",
                ssymBool "l"
              )
              ( ssymBool "c",
                ssymBool "e",
                ssymBool "g",
                ssymBool "i",
                ssymBool "k",
                ssymBool "m"
              )
              @?= ( symIte (ssymBool "a") (ssymBool "b") (ssymBool "c"),
                    symIte (ssymBool "a") (ssymBool "d") (ssymBool "e"),
                    symIte (ssymBool "a") (ssymBool "f") (ssymBool "g"),
                    symIte (ssymBool "a") (ssymBool "h") (ssymBool "i"),
                    symIte (ssymBool "a") (ssymBool "j") (ssymBool "k"),
                    symIte (ssymBool "a") (ssymBool "l") (ssymBool "m")
                  ),
          testCase
            "(SymBool, SymBool, SymBool, SymBool, SymBool, SymBool, SymBool)"
            $ do
              mrgIte
                (ssymBool "a")
                ( ssymBool "b",
                  ssymBool "d",
                  ssymBool "f",
                  ssymBool "h",
                  ssymBool "j",
                  ssymBool "l",
                  ssymBool "n"
                )
                ( ssymBool "c",
                  ssymBool "e",
                  ssymBool "g",
                  ssymBool "i",
                  ssymBool "k",
                  ssymBool "m",
                  ssymBool "o"
                )
                @?= ( symIte (ssymBool "a") (ssymBool "b") (ssymBool "c"),
                      symIte (ssymBool "a") (ssymBool "d") (ssymBool "e"),
                      symIte (ssymBool "a") (ssymBool "f") (ssymBool "g"),
                      symIte (ssymBool "a") (ssymBool "h") (ssymBool "i"),
                      symIte (ssymBool "a") (ssymBool "j") (ssymBool "k"),
                      symIte (ssymBool "a") (ssymBool "l") (ssymBool "m"),
                      symIte (ssymBool "a") (ssymBool "n") (ssymBool "o")
                    ),
          testCase
            "(SymBool, SymBool, SymBool, SymBool, SymBool, SymBool, SymBool, SymBool)"
            $ do
              mrgIte
                (ssymBool "a")
                ( ssymBool "b",
                  ssymBool "d",
                  ssymBool "f",
                  ssymBool "h",
                  ssymBool "j",
                  ssymBool "l",
                  ssymBool "n",
                  ssymBool "p"
                )
                ( ssymBool "c",
                  ssymBool "e",
                  ssymBool "g",
                  ssymBool "i",
                  ssymBool "k",
                  ssymBool "m",
                  ssymBool "o",
                  ssymBool "q"
                )
                @?= ( symIte (ssymBool "a") (ssymBool "b") (ssymBool "c"),
                      symIte (ssymBool "a") (ssymBool "d") (ssymBool "e"),
                      symIte (ssymBool "a") (ssymBool "f") (ssymBool "g"),
                      symIte (ssymBool "a") (ssymBool "h") (ssymBool "i"),
                      symIte (ssymBool "a") (ssymBool "j") (ssymBool "k"),
                      symIte (ssymBool "a") (ssymBool "l") (ssymBool "m"),
                      symIte (ssymBool "a") (ssymBool "n") (ssymBool "o"),
                      symIte (ssymBool "a") (ssymBool "p") (ssymBool "q")
                    ),
          testCase "SymBool -> SymBool" $ do
            let f = mrgIte (ssymBool "a") symNot ((ssymBool "b") .&&)
            f (ssymBool "c")
              @?= symIte
                (ssymBool "a")
                (symNot $ ssymBool "c")
                ((ssymBool "b") .&& (ssymBool "c")),
          testCase "MaybeT (UnionM) SymBool" $ do
            let l :: MaybeT (UnionM) SymBool =
                  MaybeT
                    ( mrgIf
                        (ssymBool "b")
                        (mrgSingle Nothing)
                        (mrgSingle $ Just $ ssymBool "c")
                    )
            let r :: MaybeT (UnionM) SymBool =
                  MaybeT
                    ( mrgIf
                        (ssymBool "d")
                        (mrgSingle Nothing)
                        (mrgSingle $ Just $ ssymBool "e")
                    )
            let res :: MaybeT (UnionM) SymBool =
                  MaybeT
                    ( mrgIf
                        (ssymBool "a")
                        ( mrgIf
                            (ssymBool "b")
                            (mrgSingle Nothing)
                            (mrgSingle $ Just $ ssymBool "c")
                        )
                        ( mrgIf
                            (ssymBool "d")
                            (mrgSingle Nothing)
                            (mrgSingle $ Just $ ssymBool "e")
                        )
                    )
            mrgIte (ssymBool "a") l r @?= res
            mrgIte1 (ssymBool "a") l r @?= res
            mrgIf (ssymBool "a") l r @?= res,
          testCase "ExceptT SymBool (UnionM) SymBool" $ do
            let l :: ExceptT SymBool (UnionM) SymBool =
                  ExceptT
                    ( mrgIf
                        (ssymBool "b")
                        (mrgSingle $ Left $ ssymBool "c")
                        (mrgSingle $ Right $ ssymBool "d")
                    )
            let r =
                  ExceptT
                    ( mrgIf
                        (ssymBool "e")
                        (mrgSingle $ Left $ ssymBool "f")
                        (mrgSingle $ Right $ ssymBool "g")
                    )
            let res =
                  ExceptT
                    ( mrgIf
                        (ssymBool "a")
                        ( mrgIf
                            (ssymBool "b")
                            (mrgSingle $ Left $ ssymBool "c")
                            (mrgSingle $ Right $ ssymBool "d")
                        )
                        ( mrgIf
                            (ssymBool "e")
                            (mrgSingle $ Left $ ssymBool "f")
                            (mrgSingle $ Right $ ssymBool "g")
                        )
                    )
            mrgIte (ssymBool "a") l r @?= res
            mrgIte1 (ssymBool "a") l r @?= res
            mrgIf (ssymBool "a") l r @?= res,
          testGroup
            "StateT Integer (UnionM) SymBool"
            [ testCase "Lazy" $ do
                let st1 :: StateLazy.StateT Integer (UnionM) SymBool =
                      StateLazy.StateT $ \(x :: Integer) ->
                        mrgSingle (ssymBool "a", x + 2)
                let st2 :: StateLazy.StateT Integer (UnionM) SymBool =
                      StateLazy.StateT $ \(x :: Integer) ->
                        mrgSingle (ssymBool "b", x * 2)
                let st3 = mrgIte (ssymBool "c") st1 st2
                let st31 = mrgIte1 (ssymBool "c") st1 st2
                let st3u1 = mrgIf (ssymBool "c") st1 st2
                StateLazy.runStateT st3 2
                  @?= mrgSingle
                    (symIte (ssymBool "c") (ssymBool "a") (ssymBool "b"), 4)
                StateLazy.runStateT st3 3
                  @?= mrgIf
                    (ssymBool "c")
                    (mrgSingle (ssymBool "a", 5))
                    (mrgSingle (ssymBool "b", 6))
                StateLazy.runStateT st31 2
                  @?= mrgSingle
                    (symIte (ssymBool "c") (ssymBool "a") (ssymBool "b"), 4)
                StateLazy.runStateT st31 3
                  @?= mrgIf
                    (ssymBool "c")
                    (mrgSingle (ssymBool "a", 5))
                    (mrgSingle (ssymBool "b", 6))
                StateLazy.runStateT st3u1 2
                  @?= mrgSingle
                    (symIte (ssymBool "c") (ssymBool "a") (ssymBool "b"), 4)
                StateLazy.runStateT st3u1 3
                  @?= mrgIf
                    (ssymBool "c")
                    (mrgSingle (ssymBool "a", 5))
                    (mrgSingle (ssymBool "b", 6)),
              testCase "Strict" $ do
                let st1 :: StateStrict.StateT Integer (UnionM) SymBool =
                      StateStrict.StateT $ \(x :: Integer) ->
                        mrgSingle (ssymBool "a", x + 2)
                let st2 :: StateStrict.StateT Integer (UnionM) SymBool =
                      StateStrict.StateT $ \(x :: Integer) ->
                        mrgSingle (ssymBool "b", x * 2)
                let st3 = mrgIte (ssymBool "c") st1 st2
                let st31 = mrgIte1 (ssymBool "c") st1 st2
                let st3u1 = mrgIf (ssymBool "c") st1 st2
                StateStrict.runStateT st3 2
                  @?= mrgSingle
                    (symIte (ssymBool "c") (ssymBool "a") (ssymBool "b"), 4)
                StateStrict.runStateT st3 3
                  @?= mrgIf
                    (ssymBool "c")
                    (mrgSingle (ssymBool "a", 5))
                    (mrgSingle (ssymBool "b", 6))
                StateStrict.runStateT st31 2
                  @?= mrgSingle
                    (symIte (ssymBool "c") (ssymBool "a") (ssymBool "b"), 4)
                StateStrict.runStateT st31 3
                  @?= mrgIf
                    (ssymBool "c")
                    (mrgSingle (ssymBool "a", 5))
                    (mrgSingle (ssymBool "b", 6))
                StateStrict.runStateT st3u1 2
                  @?= mrgSingle
                    (symIte (ssymBool "c") (ssymBool "a") (ssymBool "b"), 4)
                StateStrict.runStateT st3u1 3
                  @?= mrgIf
                    (ssymBool "c")
                    (mrgSingle (ssymBool "a", 5))
                    (mrgSingle (ssymBool "b", 6))
            ],
          testCase "ContT (SymBool, Integer) (UnionM) (SymBool, Integer)" $ do
            let c1 :: ContT (SymBool, Integer) (UnionM) (SymBool, Integer) =
                  ContT $ \f -> f (ssymBool "a", 2)
            let c2 :: ContT (SymBool, Integer) (UnionM) (SymBool, Integer) =
                  ContT $ \f -> f (ssymBool "b", 3)
            let c3 = mrgIte (ssymBool "c") c1 c2
            let c3u1 = mrgIf (ssymBool "c") c1 c2
            let r =
                  mrgIf
                    (ssymBool "c")
                    ( mrgIf
                        (ssymBool "p")
                        (mrgSingle (ssymBool "a", 2))
                        (mrgSingle (symNot $ ssymBool "a", 3))
                    )
                    ( mrgIf
                        (ssymBool "p")
                        (mrgSingle (ssymBool "b", 3))
                        (mrgSingle (symNot $ ssymBool "b", 4))
                    )
            let f (a, x) =
                  mrgIf
                    (ssymBool "p")
                    (mrgSingle (a, x))
                    (mrgSingle (symNot a, x + 1))
            runContT c3 f @?= r
            runContT c3u1 f @?= r,
          testGroup
            "RWST (Integer, SymBool) (Monoid.Sum Integer, AndMonoidSymBool) (Integer, SymBool) (UnionM) (Integer, SymBool)"
            [ testCase "Lazy" $ do
                let rws1 ::
                      RWSTLazy.RWST
                        (Integer, SymBool)
                        (Monoid.Sum Integer, AndMonoidSymBool)
                        (Integer, SymBool)
                        (UnionM)
                        (Integer, SymBool) =
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
                        (Integer, SymBool)
                        (Monoid.Sum Integer, AndMonoidSymBool)
                        (Integer, SymBool)
                        (UnionM)
                        (Integer, SymBool) =
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
                      UnionM
                        ( (Integer, SymBool),
                          (Integer, SymBool),
                          (Monoid.Sum Integer, AndMonoidSymBool)
                        ) =
                        mrgIf
                          (ssymBool "c")
                          ( mrgSingle
                              ( (1, ssymBool "a" .&& ssymBool "b"),
                                (-1, ssymBool "a" .|| ssymBool "b"),
                                ( 0,
                                  AndMonoidSymBool $
                                    ssymBool "b" .&& ssymBool "a"
                                )
                              )
                          )
                          ( mrgSingle
                              ( (1, ssymBool "a" .|| ssymBool "b"),
                                (-1, ssymBool "a" .&& ssymBool "b"),
                                ( 0,
                                  AndMonoidSymBool $
                                    ssymBool "b" .|| ssymBool "a"
                                )
                              )
                          )
                RWSTLazy.runRWST rws3 (0, ssymBool "a") (1, ssymBool "b")
                  @?= res1
                RWSTLazy.runRWST rws3u1 (0, ssymBool "a") (1, ssymBool "b")
                  @?= res1,
              testCase "Strict" $ do
                let rws1 ::
                      RWSTStrict.RWST
                        (Integer, SymBool)
                        (Monoid.Sum Integer, AndMonoidSymBool)
                        (Integer, SymBool)
                        (UnionM)
                        (Integer, SymBool) =
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
                        (Integer, SymBool)
                        (Monoid.Sum Integer, AndMonoidSymBool)
                        (Integer, SymBool)
                        (UnionM)
                        (Integer, SymBool) =
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
                      UnionM
                        ( (Integer, SymBool),
                          (Integer, SymBool),
                          (Monoid.Sum Integer, AndMonoidSymBool)
                        ) =
                        mrgIf
                          (ssymBool "c")
                          ( mrgSingle
                              ( (1, ssymBool "a" .&& ssymBool "b"),
                                (-1, ssymBool "a" .|| ssymBool "b"),
                                ( 0,
                                  AndMonoidSymBool $
                                    ssymBool "b" .&& ssymBool "a"
                                )
                              )
                          )
                          ( mrgSingle
                              ( (1, ssymBool "a" .|| ssymBool "b"),
                                (-1, ssymBool "a" .&& ssymBool "b"),
                                ( 0,
                                  AndMonoidSymBool $
                                    ssymBool "b" .|| ssymBool "a"
                                )
                              )
                          )
                RWSTStrict.runRWST rws3 (0, ssymBool "a") (1, ssymBool "b")
                  @?= res1
                RWSTStrict.runRWST rws3u1 (0, ssymBool "a") (1, ssymBool "b")
                  @?= res1
            ],
          testGroup
            "WriterT (Monoid.Sum Integer) (UnionM) SymBool"
            [ testCase "Lazy" $ do
                let st1 ::
                      WriterLazy.WriterT
                        (Monoid.Sum Integer)
                        (UnionM)
                        SymBool =
                        WriterLazy.WriterT $ mrgSingle (ssymBool "a", 1)
                let st2 ::
                      WriterLazy.WriterT
                        (Monoid.Sum Integer)
                        (UnionM)
                        SymBool =
                        WriterLazy.WriterT $ mrgSingle (ssymBool "b", 2)
                let st3 ::
                      WriterLazy.WriterT
                        (Monoid.Sum Integer)
                        (UnionM)
                        SymBool =
                        WriterLazy.WriterT $ mrgSingle (ssymBool "c", 1)
                let st4 = mrgIte (ssymBool "d") st1 st2
                let st41 = mrgIte1 (ssymBool "d") st1 st2
                let st4u1 = mrgIf (ssymBool "d") st1 st2
                let st5 = mrgIte (ssymBool "d") st1 st3
                let st51 = mrgIte1 (ssymBool "d") st1 st3
                let st5u1 = mrgIf (ssymBool "d") st1 st3
                WriterLazy.runWriterT st4
                  @?= mrgIf
                    (ssymBool "d")
                    (mrgSingle (ssymBool "a", 1))
                    (mrgSingle (ssymBool "b", 2))
                WriterLazy.runWriterT st41
                  @?= mrgIf
                    (ssymBool "d")
                    (mrgSingle (ssymBool "a", 1))
                    (mrgSingle (ssymBool "b", 2))
                WriterLazy.runWriterT st4u1
                  @?= mrgIf
                    (ssymBool "d")
                    (mrgSingle (ssymBool "a", 1))
                    (mrgSingle (ssymBool "b", 2))
                WriterLazy.runWriterT st5
                  @?= mrgSingle
                    (symIte (ssymBool "d") (ssymBool "a") (ssymBool "c"), 1)
                WriterLazy.runWriterT st51
                  @?= mrgSingle
                    (symIte (ssymBool "d") (ssymBool "a") (ssymBool "c"), 1)
                WriterLazy.runWriterT st5u1
                  @?= mrgSingle
                    (symIte (ssymBool "d") (ssymBool "a") (ssymBool "c"), 1),
              testCase "Strict" $ do
                let st1 ::
                      WriterStrict.WriterT
                        (Monoid.Sum Integer)
                        (UnionM)
                        SymBool =
                        WriterStrict.WriterT $ mrgSingle (ssymBool "a", 1)
                let st2 ::
                      WriterStrict.WriterT
                        (Monoid.Sum Integer)
                        (UnionM)
                        SymBool =
                        WriterStrict.WriterT $ mrgSingle (ssymBool "b", 2)
                let st3 ::
                      WriterStrict.WriterT
                        (Monoid.Sum Integer)
                        (UnionM)
                        SymBool =
                        WriterStrict.WriterT $ mrgSingle (ssymBool "c", 1)
                let st4 = mrgIte (ssymBool "d") st1 st2
                let st41 = mrgIte1 (ssymBool "d") st1 st2
                let st4u1 = mrgIf (ssymBool "d") st1 st2
                let st5 = mrgIte (ssymBool "d") st1 st3
                let st51 = mrgIte1 (ssymBool "d") st1 st3
                let st5u1 = mrgIf (ssymBool "d") st1 st3
                WriterStrict.runWriterT st4
                  @?= mrgIf
                    (ssymBool "d")
                    (mrgSingle (ssymBool "a", 1))
                    (mrgSingle (ssymBool "b", 2))
                WriterStrict.runWriterT st41
                  @?= mrgIf
                    (ssymBool "d")
                    (mrgSingle (ssymBool "a", 1))
                    (mrgSingle (ssymBool "b", 2))
                WriterStrict.runWriterT st4u1
                  @?= mrgIf
                    (ssymBool "d")
                    (mrgSingle (ssymBool "a", 1))
                    (mrgSingle (ssymBool "b", 2))
                WriterStrict.runWriterT st5
                  @?= mrgSingle
                    (symIte (ssymBool "d") (ssymBool "a") (ssymBool "c"), 1)
                WriterStrict.runWriterT st51
                  @?= mrgSingle
                    (symIte (ssymBool "d") (ssymBool "a") (ssymBool "c"), 1)
                WriterStrict.runWriterT st5u1
                  @?= mrgSingle
                    (symIte (ssymBool "d") (ssymBool "a") (ssymBool "c"), 1)
            ],
          testCase "ReaderT Integer (UnionM) Integer" $ do
            let r1 :: ReaderT Integer (UnionM) Integer =
                  ReaderT $ \(x :: Integer) -> mrgSingle $ x + 2
            let r2 :: ReaderT Integer (UnionM) Integer =
                  ReaderT $ \(x :: Integer) -> mrgSingle $ x * 2
            let r3 = mrgIte (ssymBool "c") r1 r2
            let r3u1 = mrgIf (ssymBool "c") r1 r2
            runReaderT r3 2 @?= mrgSingle 4
            runReaderT r3 3 @?= mrgIf (ssymBool "c") (mrgSingle 5) (mrgSingle 6)
            runReaderT r3u1 2 @?= mrgSingle 4
            runReaderT r3u1 3
              @?= mrgIf
                (ssymBool "c")
                (mrgSingle 5)
                (mrgSingle 6)

            let r4 :: ReaderT SymBool (UnionM) SymBool =
                  ReaderT $ \x -> mrgSingle $ x .&& ssymBool "x"
            let r5 :: ReaderT SymBool (UnionM) SymBool =
                  ReaderT $ \x -> mrgSingle $ x .|| ssymBool "y"
            let r61 = mrgIte1 (ssymBool "c") r4 r5
            runReaderT r61 (ssymBool "a")
              @?= mrgSingle
                ( symIte
                    (ssymBool "c")
                    (ssymBool "a" .&& ssymBool "x")
                    (ssymBool "a" .|| ssymBool "y")
                ),
          testCase "Identity SymBool" $ do
            let i1 :: Identity SymBool = Identity $ ssymBool "a"
            let i2 :: Identity SymBool = Identity $ ssymBool "b"
            let i3 = mrgIte (ssymBool "c") i1 i2
            let i31 = mrgIte1 (ssymBool "c") i1 i2
            runIdentity i3 @?= symIte (ssymBool "c") (ssymBool "a") (ssymBool "b")
            runIdentity i31
              @?= symIte (ssymBool "c") (ssymBool "a") (ssymBool "b"),
          testCase "IdentityT (UnionM) SymBool" $ do
            let i1 :: IdentityT (UnionM) SymBool =
                  IdentityT $ mrgSingle $ ssymBool "a"
            let i2 :: IdentityT (UnionM) SymBool =
                  IdentityT $ mrgSingle $ ssymBool "b"
            let i3 = mrgIte (ssymBool "c") i1 i2
            let i31 = mrgIte1 (ssymBool "c") i1 i2
            let i3u1 = mrgIf (ssymBool "c") i1 i2
            runIdentityT i3
              @?= mrgSingle (symIte (ssymBool "c") (ssymBool "a") (ssymBool "b"))
            runIdentityT i31
              @?= mrgSingle (symIte (ssymBool "c") (ssymBool "a") (ssymBool "b"))
            runIdentityT i3u1
              @?= mrgSingle (symIte (ssymBool "c") (ssymBool "a") (ssymBool "b"))
        ]
    ]
