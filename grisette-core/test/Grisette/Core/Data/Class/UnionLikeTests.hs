{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Core.Data.Class.UnionLikeTests where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans.State.Lazy as StateLazy
import qualified Control.Monad.Trans.State.Strict as StateStrict
import qualified Control.Monad.Trans.Writer.Lazy as WriterLazy
import qualified Control.Monad.Trans.Writer.Strict as WriterStrict
import Grisette.Core.Control.Monad.UnionMBase
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Lib.Control.Monad
import Grisette.TestUtils.SBool
import Test.Tasty
import Test.Tasty.HUnit

unionLikeTests :: TestTree
unionLikeTests =
  testGroup
    "UnionLikeTests"
    [ testCase "simpleMerge" $
        simpleMerge (unionIf (SSBool "a") (single $ SSBool "b") (single $ SSBool "c") :: UnionMBase SBool SBool)
          @=? ITE (SSBool "a") (SSBool "b") (SSBool "c"),
      testGroup
        "UnionLike"
        [ testGroup
            "MaybeT"
            [ testCase "merge" $ do
                merge
                  ( MaybeT
                      ( unionIf (SSBool "a") (single $ Just $ SSBool "b") (single $ Just $ SSBool "c") ::
                          UnionMBase SBool (Maybe SBool)
                      )
                  )
                  @=? MaybeT (mrgSingle $ Just $ ITE (SSBool "a") (SSBool "b") (SSBool "c")),
              testCase "mrgSingle" $ do
                (mrgSingle 1 :: MaybeT (UnionMBase SBool) Integer) @=? MaybeT (mrgSingle $ Just 1),
              testCase "mrgIf" $
                do
                  mrgIf (SSBool "a") (mrgSingle $ SSBool "b") (mrgSingle $ SSBool "c")
                  @=? MaybeT
                    ( mrgSingle $ Just $ ITE (SSBool "a") (SSBool "b") (SSBool "c") ::
                        UnionMBase SBool (Maybe SBool)
                    )
            ],
          testGroup
            "ExceptT"
            [ testCase "merge" $ do
                merge
                  ( ExceptT
                      ( unionIf (SSBool "a") (single $ Left $ SSBool "b") (single $ Left $ SSBool "c") ::
                          UnionMBase SBool (Either SBool SBool)
                      )
                  )
                  @=? ExceptT (mrgSingle $ Left $ ITE (SSBool "a") (SSBool "b") (SSBool "c")),
              testCase "mrgSingle" $ do
                (mrgSingle 1 :: ExceptT SBool (UnionMBase SBool) Integer) @=? ExceptT (mrgSingle $ Right 1),
              testCase "mrgIf" $
                do
                  mrgIf (SSBool "a") (mrgSingle $ SSBool "b") (mrgSingle $ SSBool "c")
                  @=? ExceptT
                    ( mrgSingle $ Right $ ITE (SSBool "a") (SSBool "b") (SSBool "c") ::
                        UnionMBase SBool (Either SBool SBool)
                    )
            ],
          testGroup
            "StateT"
            [ testGroup
                "Lazy"
                [ testCase "merge" $ do
                    let s :: StateLazy.StateT SBool (UnionMBase SBool) SBool =
                          merge $ StateLazy.StateT $ \(x :: SBool) -> unionIf (SSBool "a") (single (x, Not x)) (single (Not x, x))
                    StateLazy.runStateT s (SSBool "b")
                      @=? mrgSingle
                        ( ITE (SSBool "a") (SSBool "b") (Not $ SSBool "b"),
                          ITE (SSBool "a") (Not $ SSBool "b") (SSBool "b")
                        ),
                  testCase "mrgSingle" $ do
                    let s :: StateLazy.StateT SBool (UnionMBase SBool) SBool = mrgSingle (SSBool "x")
                    StateLazy.runStateT s (SSBool "b") @=? mrgSingle (SSBool "x", SSBool "b"),
                  testCase "mrgIf" $ do
                    let s :: StateLazy.StateT SBool (UnionMBase SBool) SBool =
                          mrgIf
                            (SSBool "a")
                            (StateLazy.StateT $ \(x :: SBool) -> single (x, Not x))
                            (StateLazy.StateT $ \(x :: SBool) -> single (Not x, x))
                    StateLazy.runStateT s (SSBool "b")
                      @=? mrgSingle
                        ( ITE (SSBool "a") (SSBool "b") (Not $ SSBool "b"),
                          ITE (SSBool "a") (Not $ SSBool "b") (SSBool "b")
                        )
                ],
              testGroup
                "Strict"
                [ testCase "merge" $ do
                    let s :: StateStrict.StateT SBool (UnionMBase SBool) SBool =
                          merge $ StateStrict.StateT $ \(x :: SBool) -> unionIf (SSBool "a") (single (x, Not x)) (single (Not x, x))
                    StateStrict.runStateT s (SSBool "b")
                      @=? mrgSingle
                        ( ITE (SSBool "a") (SSBool "b") (Not $ SSBool "b"),
                          ITE (SSBool "a") (Not $ SSBool "b") (SSBool "b")
                        ),
                  testCase "mrgSingle" $ do
                    let s :: StateStrict.StateT SBool (UnionMBase SBool) SBool = mrgSingle (SSBool "x")
                    StateStrict.runStateT s (SSBool "b") @=? mrgSingle (SSBool "x", SSBool "b"),
                  testCase "mrgIf" $ do
                    let s :: StateStrict.StateT SBool (UnionMBase SBool) SBool =
                          mrgIf
                            (SSBool "a")
                            (StateStrict.StateT $ \(x :: SBool) -> single (x, Not x))
                            (StateStrict.StateT $ \(x :: SBool) -> single (Not x, x))
                    StateStrict.runStateT s (SSBool "b")
                      @=? mrgSingle
                        ( ITE (SSBool "a") (SSBool "b") (Not $ SSBool "b"),
                          ITE (SSBool "a") (Not $ SSBool "b") (SSBool "b")
                        )
                ]
            ],
          testGroup
            "WriterT"
            [ testGroup
                "Lazy"
                [ testCase "merge" $ do
                    let s :: WriterLazy.WriterT [SBool] (UnionMBase SBool) SBool =
                          merge $
                            WriterLazy.WriterT $
                              unionIf
                                (SSBool "a")
                                (single (SSBool "b", [SSBool "c"]))
                                (single (SSBool "d", [SSBool "e"]))
                    WriterLazy.runWriterT s
                      @=? mrgSingle
                        ( ITE (SSBool "a") (SSBool "b") (SSBool "d"),
                          [ITE (SSBool "a") (SSBool "c") (SSBool "e")]
                        ),
                  testCase "mrgSingle" $ do
                    let s :: WriterLazy.WriterT [SBool] (UnionMBase SBool) SBool = mrgSingle (SSBool "x")
                    WriterLazy.runWriterT s @=? mrgSingle (SSBool "x", []),
                  testCase "mrgIf" $ do
                    let s :: WriterLazy.WriterT [SBool] (UnionMBase SBool) SBool =
                          mrgIf
                            (SSBool "a")
                            (WriterLazy.WriterT $ single (SSBool "b", [SSBool "c"]))
                            (WriterLazy.WriterT $ single (SSBool "d", [SSBool "e"]))
                    WriterLazy.runWriterT s
                      @=? mrgSingle
                        ( ITE (SSBool "a") (SSBool "b") (SSBool "d"),
                          [ITE (SSBool "a") (SSBool "c") (SSBool "e")]
                        )
                ],
              testGroup
                "Strict"
                [ testCase "merge" $ do
                    let s :: WriterStrict.WriterT [SBool] (UnionMBase SBool) SBool =
                          merge $
                            WriterStrict.WriterT $
                              unionIf
                                (SSBool "a")
                                (single (SSBool "b", [SSBool "c"]))
                                (single (SSBool "d", [SSBool "e"]))
                    WriterStrict.runWriterT s
                      @=? mrgSingle
                        ( ITE (SSBool "a") (SSBool "b") (SSBool "d"),
                          [ITE (SSBool "a") (SSBool "c") (SSBool "e")]
                        ),
                  testCase "mrgSingle" $ do
                    let s :: WriterStrict.WriterT [SBool] (UnionMBase SBool) SBool = mrgSingle (SSBool "x")
                    WriterStrict.runWriterT s @=? mrgSingle (SSBool "x", []),
                  testCase "mrgIf" $ do
                    let s :: WriterStrict.WriterT [SBool] (UnionMBase SBool) SBool =
                          mrgIf
                            (SSBool "a")
                            (WriterStrict.WriterT $ single (SSBool "b", [SSBool "c"]))
                            (WriterStrict.WriterT $ single (SSBool "d", [SSBool "e"]))
                    WriterStrict.runWriterT s
                      @=? mrgSingle
                        ( ITE (SSBool "a") (SSBool "b") (SSBool "d"),
                          [ITE (SSBool "a") (SSBool "c") (SSBool "e")]
                        )
                ]
            ],
          testGroup
            "ReaderT"
            [ testCase "merge" $
                do
                  let s :: ReaderT SBool (UnionMBase SBool) SBool =
                        merge $ ReaderT $ \(x :: SBool) -> unionIf (SSBool "a") (single x) (single $ Not x)
                  runReaderT s (SSBool "b")
                    @=? mrgSingle
                      (ITE (SSBool "a") (SSBool "b") (Not $ SSBool "b")),
              testCase
                "mrgSingle"
                $ do
                  let s :: ReaderT SBool (UnionMBase SBool) SBool = mrgSingle (SSBool "x")
                  runReaderT s (SSBool "b") @=? mrgSingle (SSBool "x"),
              testCase
                "mrgIf"
                $ do
                  let s :: ReaderT SBool (UnionMBase SBool) SBool =
                        mrgIf
                          (SSBool "a")
                          (ReaderT $ \(x :: SBool) -> single x)
                          (ReaderT $ \(x :: SBool) -> single $ Not x)
                  runReaderT s (SSBool "b")
                    @=? mrgSingle
                      (ITE (SSBool "a") (SSBool "b") (Not $ SSBool "b"))
            ],
          testGroup
            "IdentityT"
            [ testCase "merge" $
                do
                  let s :: IdentityT (UnionMBase SBool) SBool =
                        merge $
                          IdentityT $
                            unionIf
                              (SSBool "a")
                              (single $ SSBool "b")
                              (single $ SSBool "c")
                  runIdentityT s @=? mrgSingle (ITE (SSBool "a") (SSBool "b") (SSBool "c")),
              testCase
                "mrgSingle"
                $ do
                  let s :: IdentityT (UnionMBase SBool) SBool = mrgSingle (SSBool "x")
                  runIdentityT s @=? mrgSingle (SSBool "x"),
              testCase
                "mrgIf"
                $ do
                  let s :: IdentityT (UnionMBase SBool) SBool =
                        mrgIf
                          (SSBool "a")
                          (IdentityT $ single (SSBool "b"))
                          (IdentityT $ single (SSBool "c"))
                  runIdentityT s @=? mrgSingle (ITE (SSBool "a") (SSBool "b") (SSBool "c"))
            ]
        ]
    ]
