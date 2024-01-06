{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Core.Data.Class.UnionLikeTests (unionLikeTests) where

import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Identity (IdentityT (IdentityT, runIdentityT))
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Control.Monad.Trans.State.Lazy as StateLazy
import qualified Control.Monad.Trans.State.Strict as StateStrict
import qualified Control.Monad.Trans.Writer.Lazy as WriterLazy
import qualified Control.Monad.Trans.Writer.Strict as WriterStrict
import Grisette.Core.Control.Monad.UnionM (UnionM)
import Grisette.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Core.Data.Class.LogicalOp (LogicalOp (symNot))
import Grisette.Core.Data.Class.SimpleMergeable
  ( UnionLike (single, unionIf),
    merge,
    mrgIf,
    mrgSingle,
    simpleMerge,
  )
import Grisette.Core.Data.Class.Solvable (Solvable (ssym))
import Grisette.IR.SymPrim.Data.SymPrim (SymBool)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

unionLikeTests :: Test
unionLikeTests =
  testGroup
    "UnionLike"
    [ testCase "simpleMerge" $
        simpleMerge
          ( unionIf
              (ssym "a")
              (single $ ssym "b")
              (single $ ssym "c") ::
              UnionM SymBool
          )
          @?= symIte (ssym "a") (ssym "b") (ssym "c"),
      testGroup
        "UnionLike"
        [ testGroup
            "MaybeT"
            [ testCase "merge" $ do
                merge
                  ( MaybeT
                      ( unionIf
                          (ssym "a")
                          (single $ Just $ ssym "b")
                          (single $ Just $ ssym "c") ::
                          UnionM (Maybe SymBool)
                      )
                  )
                  @?= MaybeT
                    (mrgSingle $ Just $ symIte (ssym "a") (ssym "b") (ssym "c")),
              testCase "mrgSingle" $ do
                (mrgSingle 1 :: MaybeT UnionM Integer)
                  @?= MaybeT (mrgSingle $ Just 1),
              testCase "mrgIf" $
                do
                  mrgIf (ssym "a") (mrgSingle $ ssym "b") (mrgSingle $ ssym "c")
                  @?= MaybeT
                    ( mrgSingle $
                        Just $
                          symIte (ssym "a") (ssym "b") (ssym "c") ::
                        UnionM (Maybe SymBool)
                    )
            ],
          testGroup
            "ExceptT"
            [ testCase "merge" $ do
                merge
                  ( ExceptT
                      ( unionIf
                          (ssym "a")
                          (single $ Left $ ssym "b")
                          (single $ Left $ ssym "c") ::
                          UnionM (Either SymBool SymBool)
                      )
                  )
                  @?= ExceptT
                    (mrgSingle $ Left $ symIte (ssym "a") (ssym "b") (ssym "c")),
              testCase "mrgSingle" $ do
                (mrgSingle 1 :: ExceptT SymBool UnionM Integer)
                  @?= ExceptT (mrgSingle $ Right 1),
              testCase "mrgIf" $
                do
                  mrgIf (ssym "a") (mrgSingle $ ssym "b") (mrgSingle $ ssym "c")
                  @?= ExceptT
                    ( mrgSingle $
                        Right $
                          symIte (ssym "a") (ssym "b") (ssym "c") ::
                        UnionM (Either SymBool SymBool)
                    )
            ],
          testGroup
            "StateT"
            [ testGroup
                "Lazy"
                [ testCase "merge" $ do
                    let s :: StateLazy.StateT SymBool UnionM SymBool =
                          merge $ StateLazy.StateT $ \(x :: SymBool) ->
                            unionIf
                              (ssym "a")
                              (single (x, symNot x))
                              (single (symNot x, x))
                    StateLazy.runStateT s (ssym "b")
                      @?= mrgSingle
                        ( symIte (ssym "a") (ssym "b") (symNot $ ssym "b"),
                          symIte (ssym "a") (symNot $ ssym "b") (ssym "b")
                        ),
                  testCase "mrgSingle" $ do
                    let s :: StateLazy.StateT SymBool UnionM SymBool =
                          mrgSingle (ssym "x")
                    StateLazy.runStateT s (ssym "b")
                      @?= mrgSingle (ssym "x", ssym "b"),
                  testCase "mrgIf" $ do
                    let s :: StateLazy.StateT SymBool UnionM SymBool =
                          mrgIf
                            (ssym "a")
                            ( StateLazy.StateT $ \(x :: SymBool) ->
                                single (x, symNot x)
                            )
                            ( StateLazy.StateT $ \(x :: SymBool) ->
                                single (symNot x, x)
                            )
                    StateLazy.runStateT s (ssym "b")
                      @?= mrgSingle
                        ( symIte (ssym "a") (ssym "b") (symNot $ ssym "b"),
                          symIte (ssym "a") (symNot $ ssym "b") (ssym "b")
                        )
                ],
              testGroup
                "Strict"
                [ testCase "merge" $ do
                    let s :: StateStrict.StateT SymBool UnionM SymBool =
                          merge $ StateStrict.StateT $ \(x :: SymBool) ->
                            unionIf
                              (ssym "a")
                              (single (x, symNot x))
                              (single (symNot x, x))
                    StateStrict.runStateT s (ssym "b")
                      @?= mrgSingle
                        ( symIte (ssym "a") (ssym "b") (symNot $ ssym "b"),
                          symIte (ssym "a") (symNot $ ssym "b") (ssym "b")
                        ),
                  testCase "mrgSingle" $ do
                    let s :: StateStrict.StateT SymBool UnionM SymBool =
                          mrgSingle (ssym "x")
                    StateStrict.runStateT s (ssym "b")
                      @?= mrgSingle (ssym "x", ssym "b"),
                  testCase "mrgIf" $ do
                    let s :: StateStrict.StateT SymBool UnionM SymBool =
                          mrgIf
                            (ssym "a")
                            ( StateStrict.StateT $ \(x :: SymBool) ->
                                single (x, symNot x)
                            )
                            ( StateStrict.StateT $ \(x :: SymBool) ->
                                single (symNot x, x)
                            )
                    StateStrict.runStateT s (ssym "b")
                      @?= mrgSingle
                        ( symIte (ssym "a") (ssym "b") (symNot $ ssym "b"),
                          symIte (ssym "a") (symNot $ ssym "b") (ssym "b")
                        )
                ]
            ],
          testGroup
            "WriterT"
            [ testGroup
                "Lazy"
                [ testCase "merge" $ do
                    let s :: WriterLazy.WriterT [SymBool] UnionM SymBool =
                          merge $
                            WriterLazy.WriterT $
                              unionIf
                                (ssym "a")
                                (single (ssym "b", [ssym "c"]))
                                (single (ssym "d", [ssym "e"]))
                    WriterLazy.runWriterT s
                      @?= mrgSingle
                        ( symIte (ssym "a") (ssym "b") (ssym "d"),
                          [symIte (ssym "a") (ssym "c") (ssym "e")]
                        ),
                  testCase "mrgSingle" $ do
                    let s :: WriterLazy.WriterT [SymBool] UnionM SymBool =
                          mrgSingle (ssym "x")
                    WriterLazy.runWriterT s @?= mrgSingle (ssym "x", []),
                  testCase "mrgIf" $ do
                    let s :: WriterLazy.WriterT [SymBool] UnionM SymBool =
                          mrgIf
                            (ssym "a")
                            (WriterLazy.WriterT $ single (ssym "b", [ssym "c"]))
                            (WriterLazy.WriterT $ single (ssym "d", [ssym "e"]))
                    WriterLazy.runWriterT s
                      @?= mrgSingle
                        ( symIte (ssym "a") (ssym "b") (ssym "d"),
                          [symIte (ssym "a") (ssym "c") (ssym "e")]
                        )
                ],
              testGroup
                "Strict"
                [ testCase "merge" $ do
                    let s :: WriterStrict.WriterT [SymBool] UnionM SymBool =
                          merge $
                            WriterStrict.WriterT $
                              unionIf
                                (ssym "a")
                                (single (ssym "b", [ssym "c"]))
                                (single (ssym "d", [ssym "e"]))
                    WriterStrict.runWriterT s
                      @?= mrgSingle
                        ( symIte (ssym "a") (ssym "b") (ssym "d"),
                          [symIte (ssym "a") (ssym "c") (ssym "e")]
                        ),
                  testCase "mrgSingle" $ do
                    let s :: WriterStrict.WriterT [SymBool] UnionM SymBool =
                          mrgSingle (ssym "x")
                    WriterStrict.runWriterT s @?= mrgSingle (ssym "x", []),
                  testCase "mrgIf" $ do
                    let s :: WriterStrict.WriterT [SymBool] UnionM SymBool =
                          mrgIf
                            (ssym "a")
                            ( WriterStrict.WriterT $
                                single (ssym "b", [ssym "c"])
                            )
                            ( WriterStrict.WriterT $
                                single (ssym "d", [ssym "e"])
                            )
                    WriterStrict.runWriterT s
                      @?= mrgSingle
                        ( symIte (ssym "a") (ssym "b") (ssym "d"),
                          [symIte (ssym "a") (ssym "c") (ssym "e")]
                        )
                ]
            ],
          testGroup
            "ReaderT"
            [ testCase "merge" $
                do
                  let s :: ReaderT SymBool UnionM SymBool =
                        merge $ ReaderT $ \(x :: SymBool) ->
                          unionIf (ssym "a") (single x) (single $ symNot x)
                  runReaderT s (ssym "b")
                    @?= mrgSingle
                      (symIte (ssym "a") (ssym "b") (symNot $ ssym "b")),
              testCase
                "mrgSingle"
                $ do
                  let s :: ReaderT SymBool UnionM SymBool = mrgSingle (ssym "x")
                  runReaderT s (ssym "b") @?= mrgSingle (ssym "x"),
              testCase
                "mrgIf"
                $ do
                  let s :: ReaderT SymBool UnionM SymBool =
                        mrgIf
                          (ssym "a")
                          (ReaderT $ \(x :: SymBool) -> single x)
                          (ReaderT $ \(x :: SymBool) -> single $ symNot x)
                  runReaderT s (ssym "b")
                    @?= mrgSingle
                      (symIte (ssym "a") (ssym "b") (symNot $ ssym "b"))
            ],
          testGroup
            "IdentityT"
            [ testCase "merge" $
                do
                  let s :: IdentityT UnionM SymBool =
                        merge $
                          IdentityT $
                            unionIf
                              (ssym "a")
                              (single $ ssym "b")
                              (single $ ssym "c")
                  runIdentityT s
                    @?= mrgSingle (symIte (ssym "a") (ssym "b") (ssym "c")),
              testCase
                "mrgSingle"
                $ do
                  let s :: IdentityT UnionM SymBool = mrgSingle (ssym "x")
                  runIdentityT s @?= mrgSingle (ssym "x"),
              testCase
                "mrgIf"
                $ do
                  let s :: IdentityT UnionM SymBool =
                        mrgIf
                          (ssym "a")
                          (IdentityT $ single (ssym "b"))
                          (IdentityT $ single (ssym "c"))
                  runIdentityT s
                    @?= mrgSingle (symIte (ssym "a") (ssym "b") (ssym "c"))
            ]
        ]
    ]
