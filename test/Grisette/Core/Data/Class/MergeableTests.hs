{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Grisette.Core.Data.Class.MergeableTests (mergeableTests) where

import Control.Monad.Cont (ContT (ContT, runContT))
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Identity
  ( Identity (Identity),
    IdentityT (IdentityT),
  )
import qualified Control.Monad.RWS.Lazy as RWSTLazy
import qualified Control.Monad.RWS.Strict as RWSTStrict
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT))
import qualified Control.Monad.State.Lazy as StateLazy
import qualified Control.Monad.State.Strict as StateStrict
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.ByteString.Char8 as C
import Data.Functor.Sum (Sum (InL, InR))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Stack (HasCallStack)
import Grisette.Core.Control.Monad.UnionM (UnionM)
import Grisette.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Core.Data.Class.LogicalOp (LogicalOp (symNot, (.&&), (.||)))
import Grisette.Core.Data.Class.Mergeable
  ( DynamicSortedIdx (DynamicSortedIdx),
    Mergeable (rootStrategy),
    MergingStrategy (NoStrategy, SimpleStrategy),
    StrategyList (StrategyList),
    buildStrategyList,
    resolveStrategy,
  )
import Grisette.Core.Data.Class.SimpleMergeable
  ( mrgIf,
    mrgSingle,
  )
import Grisette.Core.Data.Class.Solvable (Solvable (con, ssym))
import Grisette.IR.SymPrim.Data.SymPrim (SymBool)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (Assertion, assertFailure, (@?=))
import Test.QuickCheck (ioProperty)

testMergeableSimpleEquivClass ::
  (HasCallStack, Mergeable x, Show x, Eq x) =>
  x ->
  [DynamicSortedIdx] ->
  [(SymBool, x, x, x)] ->
  Assertion
testMergeableSimpleEquivClass x idxs cases = do
  let (idxsT, s) = resolveStrategy rootStrategy x
  case s of
    SimpleStrategy m -> do
      idxsT @?= idxs
      go cases
      where
        go [] = return ()
        go ((c, t, f, r) : xs) = do
          fst (resolveStrategy rootStrategy t) @?= idxs
          fst (resolveStrategy rootStrategy f) @?= idxs
          fst (resolveStrategy rootStrategy r) @?= idxs
          m c t f @?= r
          go xs
    _ -> assertFailure $ "Bad strategy type for " ++ show x

mergeableTests :: Test
mergeableTests =
  testGroup
    "Mergeable"
    [ testGroup
        "Mergeable for common types"
        [ let SimpleStrategy f = rootStrategy :: MergingStrategy SymBool
           in testGroup
                "Mergeable for SymBool"
                [ testCase "true condition" $
                    f (con True) (ssym "a") (ssym "b") @?= ssym "a",
                  testCase "false condition" $
                    f (con False) (ssym "a") (ssym "b") @?= ssym "b",
                  testCase "general condition" $
                    f (ssym "a") (ssym "b") (ssym "c")
                      @?= symIte (ssym "a") (ssym "b") (ssym "c")
                ],
          testProperty "Bool" $
            ioProperty . \(x :: Bool) ->
              testMergeableSimpleEquivClass
                x
                [DynamicSortedIdx x]
                [(ssym "a", x, x, x)],
          testProperty "Integer" $
            ioProperty . \(x :: Integer) ->
              testMergeableSimpleEquivClass
                x
                [DynamicSortedIdx x]
                [(ssym "a", x, x, x)],
          testProperty "Char" $
            ioProperty . \(x :: Char) ->
              testMergeableSimpleEquivClass
                x
                [DynamicSortedIdx x]
                [(ssym "a", x, x, x)],
          testProperty "Int" $
            ioProperty . \(x :: Int) ->
              testMergeableSimpleEquivClass
                x
                [DynamicSortedIdx x]
                [(ssym "a", x, x, x)],
          testProperty "Int8" $
            ioProperty . \(x :: Int8) ->
              testMergeableSimpleEquivClass
                x
                [DynamicSortedIdx x]
                [(ssym "a", x, x, x)],
          testProperty "Int16" $
            ioProperty . \(x :: Int16) ->
              testMergeableSimpleEquivClass
                x
                [DynamicSortedIdx x]
                [(ssym "a", x, x, x)],
          testProperty "Int32" $
            ioProperty . \(x :: Int32) ->
              testMergeableSimpleEquivClass
                x
                [DynamicSortedIdx x]
                [(ssym "a", x, x, x)],
          testProperty "Int64" $
            ioProperty . \(x :: Int64) ->
              testMergeableSimpleEquivClass
                x
                [DynamicSortedIdx x]
                [(ssym "a", x, x, x)],
          testProperty "Word" $
            ioProperty . \(x :: Word) ->
              testMergeableSimpleEquivClass
                x
                [DynamicSortedIdx x]
                [(ssym "a", x, x, x)],
          testProperty "Word8" $
            ioProperty . \(x :: Word8) ->
              testMergeableSimpleEquivClass
                x
                [DynamicSortedIdx x]
                [(ssym "a", x, x, x)],
          testProperty "Word16" $
            ioProperty . \(x :: Word16) ->
              testMergeableSimpleEquivClass
                x
                [DynamicSortedIdx x]
                [(ssym "a", x, x, x)],
          testProperty "Word32" $
            ioProperty . \(x :: Word32) ->
              testMergeableSimpleEquivClass
                x
                [DynamicSortedIdx x]
                [(ssym "a", x, x, x)],
          testProperty "Word64" $
            ioProperty . \(x :: Word64) ->
              testMergeableSimpleEquivClass
                x
                [DynamicSortedIdx x]
                [(ssym "a", x, x, x)],
          testProperty "()" $
            ioProperty . \(x :: ()) ->
              testMergeableSimpleEquivClass x [] [(ssym "a", x, x, x)],
          testProperty "ByteString" $
            ioProperty . \(x :: String) ->
              let b = C.pack x
               in testMergeableSimpleEquivClass
                    b
                    [DynamicSortedIdx b]
                    [(ssym "a", b, b, b)],
          testGroup
            "Either"
            [ testGroup
                "Either Integer Integer"
                [ testProperty "Left x" $
                    ioProperty . \(x :: Integer) -> do
                      testMergeableSimpleEquivClass
                        (Left x :: Either Integer Integer)
                        [DynamicSortedIdx False, DynamicSortedIdx x]
                        [(ssym "a", Left x, Left x, Left x)],
                  testProperty "Right x" $
                    ioProperty . \(x :: Integer) -> do
                      testMergeableSimpleEquivClass
                        (Right x :: Either Integer Integer)
                        [DynamicSortedIdx True, DynamicSortedIdx x]
                        [(ssym "a", Right x, Right x, Right x)]
                ],
              testGroup
                "Either SymBool SymBool"
                [ testCase "Left v" $ do
                    let (idxsL, SimpleStrategy fL) =
                          resolveStrategy
                            rootStrategy
                            (Left (ssym "a") :: Either SymBool SymBool)
                    idxsL @?= [DynamicSortedIdx False]
                    fL (ssym "a") (Left $ ssym "b") (Left $ ssym "c")
                      @?= Left (symIte (ssym "a") (ssym "b") (ssym "c")),
                  testCase "Right v" $ do
                    let (idxsR, SimpleStrategy fR) =
                          resolveStrategy
                            rootStrategy
                            (Right (ssym "a") :: Either SymBool SymBool)
                    idxsR @?= [DynamicSortedIdx True]
                    fR (ssym "a") (Right $ ssym "b") (Right $ ssym "c")
                      @?= Right (symIte (ssym "a") (ssym "b") (ssym "c"))
                ]
            ],
          testGroup
            "Maybe"
            [ testGroup
                "Maybe Integer"
                [ testProperty "Nothing" $
                    ioProperty . \(_ :: Integer) -> do
                      testMergeableSimpleEquivClass
                        (Nothing :: Maybe Integer)
                        [DynamicSortedIdx False]
                        [(ssym "a", Nothing, Nothing, Nothing)],
                  testProperty "Just v" $
                    ioProperty . \(x :: Integer) -> do
                      testMergeableSimpleEquivClass
                        (Just x :: Maybe Integer)
                        [DynamicSortedIdx True, DynamicSortedIdx x]
                        [(ssym "a", Just x, Just x, Just x)]
                ],
              testCase "Maybe SymBool / Just v" $ do
                let (idxsJ, SimpleStrategy fJ) =
                      resolveStrategy
                        rootStrategy
                        (Just (ssym "a") :: Maybe SymBool)
                idxsJ @?= [DynamicSortedIdx True]
                fJ (ssym "a") (Just $ ssym "b") (Just $ ssym "c")
                  @?= Just (symIte (ssym "a") (ssym "b") (ssym "c"))
            ],
          testGroup
            "List"
            [ testCase "BuildStrategyList" $ do
                case buildStrategyList @Integer rootStrategy [1, 2, 3] of
                  StrategyList idxs _ -> do
                    idxs
                      @?= [ [DynamicSortedIdx (1 :: Integer)],
                            [DynamicSortedIdx (2 :: Integer)],
                            [DynamicSortedIdx (3 :: Integer)]
                          ],
              testProperty "List for ordered type" $
                ioProperty . \(x :: [Integer]) -> do
                  testMergeableSimpleEquivClass
                    x
                    [ DynamicSortedIdx (length x),
                      DynamicSortedIdx $ buildStrategyList rootStrategy x
                    ]
                    [(ssym "a", x, x, x)],
              testProperty "Nested List for ordered type" $
                ioProperty . \(x :: [[Integer]]) -> do
                  testMergeableSimpleEquivClass
                    x
                    [ DynamicSortedIdx (length x),
                      DynamicSortedIdx $ buildStrategyList rootStrategy x
                    ]
                    [(ssym "a", x, x, x)],
              testGroup
                "[SymBool]"
                [ testCase "[]" $
                    testMergeableSimpleEquivClass
                      ([] :: [SymBool])
                      [DynamicSortedIdx (0 :: Int)]
                      [(ssym "a", [], [], [])],
                  testCase "[v1, v2]" $
                    testMergeableSimpleEquivClass
                      [ssym "a" :: SymBool, ssym "b"]
                      [DynamicSortedIdx (2 :: Int)]
                      [ ( ssym "a",
                          [ssym "b", ssym "c"],
                          [ssym "d", ssym "e"],
                          [ symIte (ssym "a") (ssym "b") (ssym "d"),
                            symIte (ssym "a") (ssym "c") (ssym "e")
                          ]
                        )
                      ]
                ]
            ],
          testCase "(,)" $
            testMergeableSimpleEquivClass
              ([1 :: Integer], [ssym "b" :: SymBool, ssym "c"])
              [ DynamicSortedIdx (1 :: Int),
                DynamicSortedIdx $
                  buildStrategyList rootStrategy [1 :: Integer],
                DynamicSortedIdx (2 :: Int)
              ]
              [ ( ssym "a",
                  ([1], [ssym "c", ssym "d"]),
                  ([1], [ssym "f", ssym "g"]),
                  ( [1],
                    [ symIte (ssym "a") (ssym "c") (ssym "f"),
                      symIte (ssym "a") (ssym "d") (ssym "g")
                    ]
                  )
                )
              ],
          testCase "(,,)" $
            testMergeableSimpleEquivClass
              ( [1 :: Integer],
                [ssym "b" :: SymBool, ssym "c"],
                ssym "d" :: SymBool
              )
              [ DynamicSortedIdx (1 :: Int),
                DynamicSortedIdx $
                  buildStrategyList rootStrategy [1 :: Integer],
                DynamicSortedIdx (2 :: Int)
              ]
              [ ( ssym "a",
                  ([1], [ssym "c", ssym "d"], ssym "e"),
                  ([1], [ssym "f", ssym "g"], ssym "h"),
                  ( [1],
                    [ symIte (ssym "a") (ssym "c") (ssym "f"),
                      symIte (ssym "a") (ssym "d") (ssym "g")
                    ],
                    symIte (ssym "a") (ssym "e") (ssym "h")
                  )
                )
              ],
          testCase "(,,,)" $
            testMergeableSimpleEquivClass
              ( [1 :: Integer],
                [ssym "b" :: SymBool, ssym "c"],
                ssym "d" :: SymBool,
                [ssym "f" :: SymBool]
              )
              [ DynamicSortedIdx (1 :: Int),
                DynamicSortedIdx $
                  buildStrategyList rootStrategy [1 :: Integer],
                DynamicSortedIdx (2 :: Int),
                DynamicSortedIdx (1 :: Int)
              ]
              [ ( ssym "a",
                  ([1], [ssym "c", ssym "d"], ssym "e", [ssym "i"]),
                  ([1], [ssym "f", ssym "g"], ssym "h", [ssym "j"]),
                  ( [1],
                    [ symIte (ssym "a") (ssym "c") (ssym "f"),
                      symIte (ssym "a") (ssym "d") (ssym "g")
                    ],
                    symIte (ssym "a") (ssym "e") (ssym "h"),
                    [symIte (ssym "a") (ssym "i") (ssym "j")]
                  )
                )
              ],
          testCase "(,,,,)" $
            testMergeableSimpleEquivClass
              ( [1 :: Integer],
                [ssym "b" :: SymBool, ssym "c"],
                ssym "d" :: SymBool,
                [ssym "f" :: SymBool],
                [2 :: Integer, 3]
              )
              [ DynamicSortedIdx (1 :: Int),
                DynamicSortedIdx $
                  buildStrategyList rootStrategy [1 :: Integer],
                DynamicSortedIdx (2 :: Int),
                DynamicSortedIdx (1 :: Int),
                DynamicSortedIdx (2 :: Int),
                DynamicSortedIdx $
                  buildStrategyList rootStrategy [2 :: Integer, 3]
              ]
              [ ( ssym "a",
                  ([1], [ssym "c", ssym "d"], ssym "e", [ssym "i"], [2, 3]),
                  ([1], [ssym "f", ssym "g"], ssym "h", [ssym "j"], [2, 3]),
                  ( [1],
                    [ symIte (ssym "a") (ssym "c") (ssym "f"),
                      symIte (ssym "a") (ssym "d") (ssym "g")
                    ],
                    symIte (ssym "a") (ssym "e") (ssym "h"),
                    [symIte (ssym "a") (ssym "i") (ssym "j")],
                    [2, 3]
                  )
                )
              ],
          testCase "(,,,,,)" $
            testMergeableSimpleEquivClass
              ( [1 :: Integer],
                [ssym "b" :: SymBool, ssym "c"],
                ssym "d" :: SymBool,
                [ssym "f" :: SymBool],
                [2 :: Integer, 3],
                2 :: Integer
              )
              [ DynamicSortedIdx (1 :: Int),
                DynamicSortedIdx $
                  buildStrategyList rootStrategy [1 :: Integer],
                DynamicSortedIdx (2 :: Int),
                DynamicSortedIdx (1 :: Int),
                DynamicSortedIdx (2 :: Int),
                DynamicSortedIdx $
                  buildStrategyList rootStrategy [2 :: Integer, 3],
                DynamicSortedIdx (2 :: Integer)
              ]
              [ ( ssym "a",
                  ([1], [ssym "c", ssym "d"], ssym "e", [ssym "i"], [2, 3], 2),
                  ([1], [ssym "f", ssym "g"], ssym "h", [ssym "j"], [2, 3], 2),
                  ( [1],
                    [ symIte (ssym "a") (ssym "c") (ssym "f"),
                      symIte (ssym "a") (ssym "d") (ssym "g")
                    ],
                    symIte (ssym "a") (ssym "e") (ssym "h"),
                    [symIte (ssym "a") (ssym "i") (ssym "j")],
                    [2, 3],
                    2
                  )
                )
              ],
          testCase "(,,,,,,)" $
            testMergeableSimpleEquivClass
              ( [1 :: Integer],
                [ssym "b" :: SymBool, ssym "c"],
                ssym "d" :: SymBool,
                [ssym "f" :: SymBool],
                [2 :: Integer, 3],
                2 :: Integer,
                Just (ssym "a" :: SymBool)
              )
              [ DynamicSortedIdx (1 :: Int),
                DynamicSortedIdx $
                  buildStrategyList rootStrategy [1 :: Integer],
                DynamicSortedIdx (2 :: Int),
                DynamicSortedIdx (1 :: Int),
                DynamicSortedIdx (2 :: Int),
                DynamicSortedIdx $
                  buildStrategyList rootStrategy [2 :: Integer, 3],
                DynamicSortedIdx (2 :: Integer),
                DynamicSortedIdx True
              ]
              [ ( ssym "a",
                  ( [1],
                    [ssym "c", ssym "d"],
                    ssym "e",
                    [ssym "i"],
                    [2, 3],
                    2,
                    Just (ssym "k")
                  ),
                  ( [1],
                    [ssym "f", ssym "g"],
                    ssym "h",
                    [ssym "j"],
                    [2, 3],
                    2,
                    Just (ssym "l")
                  ),
                  ( [1],
                    [ symIte (ssym "a") (ssym "c") (ssym "f"),
                      symIte (ssym "a") (ssym "d") (ssym "g")
                    ],
                    symIte (ssym "a") (ssym "e") (ssym "h"),
                    [symIte (ssym "a") (ssym "i") (ssym "j")],
                    [2, 3],
                    2,
                    Just $ symIte (ssym "a") (ssym "k") (ssym "l")
                  )
                )
              ],
          testCase "(,,,,,,,)" $
            testMergeableSimpleEquivClass
              ( [1 :: Integer],
                [ssym "b" :: SymBool, ssym "c"],
                ssym "d" :: SymBool,
                [ssym "f" :: SymBool],
                [2 :: Integer, 3],
                2 :: Integer,
                Just (ssym "a" :: SymBool),
                Left 1 :: Either Integer Integer
              )
              [ DynamicSortedIdx (1 :: Int),
                DynamicSortedIdx $
                  buildStrategyList rootStrategy [1 :: Integer],
                DynamicSortedIdx (2 :: Int),
                DynamicSortedIdx (1 :: Int),
                DynamicSortedIdx (2 :: Int),
                DynamicSortedIdx $
                  buildStrategyList rootStrategy [2 :: Integer, 3],
                DynamicSortedIdx (2 :: Integer),
                DynamicSortedIdx True,
                DynamicSortedIdx False,
                DynamicSortedIdx (1 :: Integer)
              ]
              [ ( ssym "a",
                  ( [1],
                    [ssym "c", ssym "d"],
                    ssym "e",
                    [ssym "i"],
                    [2, 3],
                    2,
                    Just (ssym "k"),
                    Left 1
                  ),
                  ( [1],
                    [ssym "f", ssym "g"],
                    ssym "h",
                    [ssym "j"],
                    [2, 3],
                    2,
                    Just (ssym "l"),
                    Left 1
                  ),
                  ( [1],
                    [ symIte (ssym "a") (ssym "c") (ssym "f"),
                      symIte (ssym "a") (ssym "d") (ssym "g")
                    ],
                    symIte (ssym "a") (ssym "e") (ssym "h"),
                    [symIte (ssym "a") (ssym "i") (ssym "j")],
                    [2, 3],
                    2,
                    Just $ symIte (ssym "a") (ssym "k") (ssym "l"),
                    Left 1
                  )
                )
              ],
          let f1 :: Maybe SymBool -> SymBool =
                \case Just x -> x; Nothing -> (con True)
              f2 :: Maybe SymBool -> SymBool =
                \case Just x -> (symNot x); Nothing -> (con False)
           in testGroup
                "Function"
                [ testCase "Simply mergeable result" $ do
                    case rootStrategy ::
                           MergingStrategy (Maybe SymBool -> SymBool) of
                      SimpleStrategy f -> do
                        let r = f (ssym "a") f1 f2
                        r (Just (ssym "x"))
                          @?= symIte (ssym "a") (ssym "x") (symNot (ssym "x"))
                        r Nothing @?= symIte (ssym "a") (con True) (con False)
                      _ -> assertFailure "Bad mergeable strategy type",
                  testCase "Other mergeable result" $ do
                    case rootStrategy ::
                           MergingStrategy (Maybe SymBool -> Integer) of
                      NoStrategy -> return ()
                      _ -> assertFailure "Bad mergeable strategy type"
                ],
          testGroup
            "MaybeT"
            [ testGroup
                "MaybeT Maybe Integer"
                [ testCase "MaybeT Nothing" $
                    testMergeableSimpleEquivClass
                      (MaybeT Nothing :: MaybeT Maybe Integer)
                      [DynamicSortedIdx False]
                      [ ( ssym "a",
                          MaybeT Nothing,
                          MaybeT Nothing,
                          MaybeT Nothing
                        )
                      ],
                  testCase "MaybeT (Just Nothing)" $
                    testMergeableSimpleEquivClass
                      (MaybeT $ Just Nothing :: MaybeT Maybe Integer)
                      [DynamicSortedIdx True, DynamicSortedIdx False]
                      [ ( ssym "a",
                          MaybeT $ Just Nothing,
                          MaybeT $ Just Nothing,
                          MaybeT $ Just Nothing
                        )
                      ],
                  testProperty "MaybeT (Just (Just v))" $
                    ioProperty . \(x :: Integer) -> do
                      testMergeableSimpleEquivClass
                        (MaybeT $ Just $ Just x :: MaybeT Maybe Integer)
                        [ DynamicSortedIdx True,
                          DynamicSortedIdx True,
                          DynamicSortedIdx x
                        ]
                        [ ( ssym "a",
                            MaybeT $ Just $ Just x,
                            MaybeT $ Just $ Just x,
                            MaybeT $ Just $ Just x
                          )
                        ]
                ],
              testCase "MaybeT Maybe SymBool / MaybeT (Just (Just v))" $ do
                let (idxsJ, SimpleStrategy fJ) =
                      resolveStrategy
                        rootStrategy
                        ( MaybeT (Just (Just (ssym "a"))) ::
                            MaybeT Maybe SymBool
                        )
                idxsJ @?= [DynamicSortedIdx True, DynamicSortedIdx True]
                fJ
                  (ssym "a")
                  (MaybeT $ Just $ Just $ ssym "b")
                  (MaybeT $ Just $ Just $ ssym "c")
                  @?= MaybeT
                    (Just (Just (symIte (ssym "a") (ssym "b") (ssym "c"))))
            ],
          testGroup
            "ExceptT"
            [ testGroup
                "ExceptT Integer Maybe Integer"
                [ testCase "ExceptT Nothing" $
                    testMergeableSimpleEquivClass
                      (ExceptT Nothing :: ExceptT Integer Maybe Integer)
                      [DynamicSortedIdx False]
                      [ ( ssym "a",
                          ExceptT Nothing,
                          ExceptT Nothing,
                          ExceptT Nothing
                        )
                      ],
                  testProperty "ExceptT (Just (Left v))" $
                    ioProperty . \(x :: Integer) -> do
                      testMergeableSimpleEquivClass
                        ( ExceptT $ Just $ Left x ::
                            ExceptT Integer Maybe Integer
                        )
                        [ DynamicSortedIdx True,
                          DynamicSortedIdx False,
                          DynamicSortedIdx x
                        ]
                        [ ( ssym "a",
                            ExceptT $ Just $ Left x,
                            ExceptT $ Just $ Left x,
                            ExceptT $ Just $ Left x
                          )
                        ],
                  testProperty "ExceptT (Just (Right v))" $
                    ioProperty . \(x :: Integer) -> do
                      testMergeableSimpleEquivClass
                        ( ExceptT $ Just $ Right x ::
                            ExceptT Integer Maybe Integer
                        )
                        [ DynamicSortedIdx True,
                          DynamicSortedIdx True,
                          DynamicSortedIdx x
                        ]
                        [ ( ssym "a",
                            ExceptT $ Just $ Right x,
                            ExceptT $ Just $ Right x,
                            ExceptT $ Just $ Right x
                          )
                        ]
                ],
              testGroup
                "ExceptT SymBool Maybe SymBool"
                [ testCase "ExceptT (Just (Left v))" $ do
                    let (idxsJL, SimpleStrategy fJL) =
                          resolveStrategy
                            rootStrategy
                            ( ExceptT (Just (Left (ssym "a"))) ::
                                ExceptT SymBool Maybe SymBool
                            )
                    idxsJL @?= [DynamicSortedIdx True, DynamicSortedIdx False]
                    fJL
                      (ssym "a")
                      (ExceptT $ Just $ Left $ ssym "b")
                      (ExceptT $ Just $ Left $ ssym "c")
                      @?= ExceptT
                        (Just (Left (symIte (ssym "a") (ssym "b") (ssym "c")))),
                  testCase "ExceptT (Just (Right v))" $ do
                    let (idxsJR, SimpleStrategy fJR) =
                          resolveStrategy
                            rootStrategy
                            ( ExceptT (Just (Right (ssym "a"))) ::
                                ExceptT SymBool Maybe SymBool
                            )
                    idxsJR @?= [DynamicSortedIdx True, DynamicSortedIdx True]
                    fJR
                      (ssym "a")
                      (ExceptT $ Just $ Right $ ssym "b")
                      (ExceptT $ Just $ Right $ ssym "c")
                      @?= ExceptT
                        (Just (Right (symIte (ssym "a") (ssym "b") (ssym "c"))))
                ]
            ],
          testGroup
            "StateT"
            [ testCase "Lazy StateT" $ do
                let SimpleStrategy s =
                      rootStrategy ::
                        MergingStrategy
                          (StateLazy.StateT Integer UnionM SymBool)
                let st1 :: StateLazy.StateT Integer UnionM SymBool =
                      StateLazy.StateT $ \(x :: Integer) ->
                        mrgSingle (ssym "a", x + 2)
                let st2 :: StateLazy.StateT Integer UnionM SymBool =
                      StateLazy.StateT $ \(x :: Integer) ->
                        mrgSingle (ssym "b", x * 2)
                let st3 = s (ssym "c") st1 st2
                StateLazy.runStateT st3 2
                  @?= mrgSingle (symIte (ssym "c") (ssym "a") (ssym "b"), 4)
                StateLazy.runStateT st3 3
                  @?= mrgIf
                    (ssym "c")
                    (mrgSingle (ssym "a", 5))
                    (mrgSingle (ssym "b", 6)),
              testCase "Strict StateT" $ do
                let SimpleStrategy s =
                      rootStrategy ::
                        MergingStrategy
                          (StateStrict.StateT Integer UnionM SymBool)
                let st1 :: StateStrict.StateT Integer UnionM SymBool =
                      StateStrict.StateT $
                        \(x :: Integer) -> mrgSingle (ssym "a", x + 2)
                let st2 :: StateStrict.StateT Integer UnionM SymBool =
                      StateStrict.StateT $
                        \(x :: Integer) -> mrgSingle (ssym "b", x * 2)
                let st3 = s (ssym "c") st1 st2
                StateStrict.runStateT st3 2
                  @?= mrgSingle (symIte (ssym "c") (ssym "a") (ssym "b"), 4)
                StateStrict.runStateT st3 3
                  @?= mrgIf
                    (ssym "c")
                    (mrgSingle (ssym "a", 5))
                    (mrgSingle (ssym "b", 6))
            ],
          testCase "ContT" $ do
            let SimpleStrategy s =
                  rootStrategy ::
                    MergingStrategy
                      (ContT (SymBool, Integer) UnionM (SymBool, Integer))
            let c1 :: ContT (SymBool, Integer) UnionM (SymBool, Integer) =
                  ContT $ \f -> f (ssym "a", 2)
            let c2 :: ContT (SymBool, Integer) UnionM (SymBool, Integer) =
                  ContT $ \f -> f (ssym "b", 3)
            let c3 = s (ssym "c") c1 c2
            runContT
              c3
              ( \(a, x) ->
                  mrgIf
                    (ssym "p")
                    (mrgSingle (a, x))
                    (mrgSingle (symNot a, x + 1))
              )
              @?= mrgIf
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
                ),
          testGroup
            "RWST"
            [ testCase "Lazy RWST" $ do
                let SimpleStrategy s =
                      rootStrategy ::
                        MergingStrategy
                          ( RWSTLazy.RWST
                              (Integer, SymBool)
                              (Integer, SymBool)
                              (Integer, SymBool)
                              UnionM
                              (Integer, SymBool)
                          )
                let rws1 ::
                      RWSTLazy.RWST
                        (Integer, SymBool)
                        (Integer, SymBool)
                        (Integer, SymBool)
                        UnionM
                        (Integer, SymBool) =
                        RWSTLazy.RWST $ \(ir, br) (is, bs) ->
                          mrgSingle
                            ( (ir + is, br .&& bs),
                              (ir - is, br .|| bs),
                              (ir * is, bs .&& br)
                            )
                let rws2 ::
                      RWSTLazy.RWST
                        (Integer, SymBool)
                        (Integer, SymBool)
                        (Integer, SymBool)
                        UnionM
                        (Integer, SymBool) =
                        RWSTLazy.RWST $ \(ir, br) (is, bs) ->
                          mrgSingle
                            ( (ir + is, br .|| bs),
                              (ir - is, br .&& bs),
                              (ir * is, bs .|| br)
                            )
                let rws3 = s (ssym "c") rws1 rws2

                let res1 ::
                      UnionM
                        ( (Integer, SymBool),
                          (Integer, SymBool),
                          (Integer, SymBool)
                        ) =
                        mrgIf
                          (ssym "c")
                          ( mrgSingle
                              ( (1, ssym "a" .&& ssym "b"),
                                (-1, ssym "a" .|| ssym "b"),
                                (0, ssym "b" .&& ssym "a")
                              )
                          )
                          ( mrgSingle
                              ( (1, ssym "a" .|| ssym "b"),
                                (-1, ssym "a" .&& ssym "b"),
                                (0, ssym "b" .|| ssym "a")
                              )
                          )
                RWSTLazy.runRWST rws3 (0, ssym "a") (1, ssym "b") @?= res1,
              testCase "Strict RWST" $ do
                let SimpleStrategy s =
                      rootStrategy ::
                        MergingStrategy
                          ( RWSTStrict.RWST
                              (Integer, SymBool)
                              (Integer, SymBool)
                              (Integer, SymBool)
                              UnionM
                              (Integer, SymBool)
                          )
                let rws1 ::
                      RWSTStrict.RWST
                        (Integer, SymBool)
                        (Integer, SymBool)
                        (Integer, SymBool)
                        UnionM
                        (Integer, SymBool) =
                        RWSTStrict.RWST $ \(ir, br) (is, bs) ->
                          mrgSingle
                            ( (ir + is, br .&& bs),
                              (ir - is, br .|| bs),
                              (ir * is, bs .&& br)
                            )
                let rws2 ::
                      RWSTStrict.RWST
                        (Integer, SymBool)
                        (Integer, SymBool)
                        (Integer, SymBool)
                        UnionM
                        (Integer, SymBool) =
                        RWSTStrict.RWST $ \(ir, br) (is, bs) ->
                          mrgSingle
                            ( (ir + is, br .|| bs),
                              (ir - is, br .&& bs),
                              (ir * is, bs .|| br)
                            )
                let rws3 = s (ssym "c") rws1 rws2

                let res1 ::
                      UnionM
                        ( (Integer, SymBool),
                          (Integer, SymBool),
                          (Integer, SymBool)
                        ) =
                        mrgIf
                          (ssym "c")
                          ( mrgSingle
                              ( (1, "a" .&& "b"),
                                (-1, "a" .|| "b"),
                                (0, "b" .&& "a")
                              )
                          )
                          ( mrgSingle
                              ( (1, "a" .|| "b"),
                                (-1, "a" .&& "b"),
                                (0, "b" .|| "a")
                              )
                          )
                RWSTStrict.runRWST rws3 (0, ssym "a") (1, ssym "b") @?= res1
            ],
          testGroup
            "WriterT"
            [ testCase "Lazy WriterT" $ do
                let SimpleStrategy s =
                      rootStrategy ::
                        MergingStrategy
                          (WriterLazy.WriterT Integer UnionM SymBool)
                let w1 :: WriterLazy.WriterT Integer UnionM SymBool =
                      WriterLazy.WriterT $ mrgSingle (ssym "a", 1)
                let w2 :: WriterLazy.WriterT Integer UnionM SymBool =
                      WriterLazy.WriterT $ mrgSingle (ssym "b", 2)
                let w3 :: WriterLazy.WriterT Integer UnionM SymBool =
                      WriterLazy.WriterT $ mrgSingle (ssym "c", 1)
                let w4 = s (ssym "d") w1 w2
                let w5 = s (ssym "d") w1 w3
                WriterLazy.runWriterT w4
                  @?= mrgIf
                    (ssym "d")
                    (mrgSingle (ssym "a", 1))
                    (mrgSingle (ssym "b", 2))
                WriterLazy.runWriterT w5
                  @?= mrgSingle (symIte (ssym "d") (ssym "a") (ssym "c"), 1),
              testCase "Strict WriterT" $ do
                let SimpleStrategy s =
                      rootStrategy ::
                        MergingStrategy
                          (WriterStrict.WriterT Integer UnionM SymBool)
                let w1 :: WriterStrict.WriterT Integer UnionM SymBool =
                      WriterStrict.WriterT $ mrgSingle (ssym "a", 1)
                let w2 :: WriterStrict.WriterT Integer UnionM SymBool =
                      WriterStrict.WriterT $ mrgSingle (ssym "b", 2)
                let w3 :: WriterStrict.WriterT Integer UnionM SymBool =
                      WriterStrict.WriterT $ mrgSingle (ssym "c", 1)
                let w4 = s (ssym "d") w1 w2
                let w5 = s (ssym "d") w1 w3
                WriterStrict.runWriterT w4
                  @?= mrgIf
                    (ssym "d")
                    (mrgSingle (ssym "a", 1))
                    (mrgSingle (ssym "b", 2))
                WriterStrict.runWriterT w5
                  @?= mrgSingle (symIte (ssym "d") (ssym "a") (ssym "c"), 1)
            ],
          testCase "ReaderT" $ do
            let SimpleStrategy s =
                  rootStrategy ::
                    MergingStrategy (ReaderT Integer UnionM Integer)
            let r1 :: ReaderT Integer UnionM Integer =
                  ReaderT $ \(x :: Integer) -> mrgSingle $ x + 2
            let r2 :: ReaderT Integer UnionM Integer =
                  ReaderT $ \(x :: Integer) -> mrgSingle $ x * 2
            let r3 = s (ssym "c") r1 r2
            runReaderT r3 2 @?= mrgSingle 4
            runReaderT r3 3 @?= mrgIf (ssym "c") (mrgSingle 5) (mrgSingle 6),
          testGroup
            "Identity"
            [ testProperty "Identity Integer" $
                ioProperty . \x -> do
                  testMergeableSimpleEquivClass
                    (Identity x :: Identity Integer)
                    [DynamicSortedIdx x]
                    [(ssym "a", Identity x, Identity x, Identity x)],
              testCase "Identity SymBool" $ do
                testMergeableSimpleEquivClass
                  (Identity (ssym "a" :: SymBool))
                  []
                  [ ( ssym "a",
                      Identity $ ssym "b",
                      Identity $ ssym "c",
                      Identity $ symIte (ssym "a") (ssym "b") (ssym "c")
                    )
                  ]
            ],
          testGroup
            "IdentityT Maybe Integer"
            [ testGroup
                "IdentityT Maybe Integer"
                [ testCase "IdentityT Nothing" $
                    testMergeableSimpleEquivClass
                      (IdentityT Nothing :: IdentityT Maybe Integer)
                      [DynamicSortedIdx False]
                      [ ( ssym "a",
                          IdentityT Nothing,
                          IdentityT Nothing,
                          IdentityT Nothing
                        )
                      ],
                  testProperty "IdentityT (Just v)" $
                    ioProperty . \x -> do
                      testMergeableSimpleEquivClass
                        (IdentityT $ Just x :: IdentityT Maybe Integer)
                        [DynamicSortedIdx True, DynamicSortedIdx x]
                        [ ( ssym "a",
                            IdentityT $ Just x,
                            IdentityT $ Just x,
                            IdentityT $ Just x
                          )
                        ]
                ],
              testGroup
                "IdentityT Maybe SymBool"
                [ testCase "IdentityT Nothing" $
                    testMergeableSimpleEquivClass
                      (IdentityT Nothing :: IdentityT Maybe SymBool)
                      [DynamicSortedIdx False]
                      [ ( ssym "a",
                          IdentityT Nothing,
                          IdentityT Nothing,
                          IdentityT Nothing
                        )
                      ],
                  testCase "IdentityT (Just v)" $
                    testMergeableSimpleEquivClass
                      (IdentityT $ Just $ ssym "a" :: IdentityT Maybe SymBool)
                      [DynamicSortedIdx True]
                      [ ( ssym "a",
                          IdentityT $ Just $ ssym "b",
                          IdentityT $ Just $ ssym "c",
                          IdentityT $
                            Just $
                              symIte (ssym "a") (ssym "b") (ssym "c")
                        )
                      ]
                ]
            ],
          testGroup
            "Sum"
            [ testGroup
                "Sum Maybe Maybe Integer"
                [ testCase "InL Nothing" $
                    testMergeableSimpleEquivClass
                      (InL Nothing :: Sum Maybe Maybe Integer)
                      [DynamicSortedIdx False, DynamicSortedIdx False]
                      [(ssym "a", InL Nothing, InL Nothing, InL Nothing)],
                  testProperty "InL (Just v)" $
                    ioProperty . \x -> do
                      testMergeableSimpleEquivClass
                        (InL $ Just x :: Sum Maybe Maybe Integer)
                        [ DynamicSortedIdx False,
                          DynamicSortedIdx True,
                          DynamicSortedIdx x
                        ]
                        [(ssym "a", InL $ Just x, InL $ Just x, InL $ Just x)],
                  testCase "InR Nothing" $
                    testMergeableSimpleEquivClass
                      (InR Nothing :: Sum Maybe Maybe Integer)
                      [DynamicSortedIdx True, DynamicSortedIdx False]
                      [(ssym "a", InR Nothing, InR Nothing, InR Nothing)],
                  testProperty "InR (Just v)" $
                    ioProperty . \x -> do
                      testMergeableSimpleEquivClass
                        (InR $ Just x :: Sum Maybe Maybe Integer)
                        [ DynamicSortedIdx True,
                          DynamicSortedIdx True,
                          DynamicSortedIdx x
                        ]
                        [(ssym "a", InR $ Just x, InR $ Just x, InR $ Just x)]
                ],
              testGroup
                "Sum Maybe Maybe SymBool"
                [ testCase "InL Nothing" $
                    testMergeableSimpleEquivClass
                      (InL Nothing :: Sum Maybe Maybe SymBool)
                      [DynamicSortedIdx False, DynamicSortedIdx False]
                      [(ssym "a", InL Nothing, InL Nothing, InL Nothing)],
                  testCase "InL (Just v)" $
                    testMergeableSimpleEquivClass
                      (InL $ Just $ ssym "a" :: Sum Maybe Maybe SymBool)
                      [DynamicSortedIdx False, DynamicSortedIdx True]
                      [ ( ssym "a",
                          InL $ Just $ ssym "b",
                          InL $ Just $ ssym "c",
                          InL $ Just $ symIte (ssym "a") (ssym "b") (ssym "c")
                        )
                      ],
                  testCase "InR Nothing" $
                    testMergeableSimpleEquivClass
                      (InR Nothing :: Sum Maybe Maybe SymBool)
                      [DynamicSortedIdx True, DynamicSortedIdx False]
                      [(ssym "a", InR Nothing, InR Nothing, InR Nothing)],
                  testCase "InR (Just v)" $
                    testMergeableSimpleEquivClass
                      (InR $ Just $ ssym "a" :: Sum Maybe Maybe SymBool)
                      [DynamicSortedIdx True, DynamicSortedIdx True]
                      [ ( ssym "a",
                          InR $ Just $ ssym "b",
                          InR $ Just $ ssym "c",
                          InR $ Just $ symIte (ssym "a") (ssym "b") (ssym "c")
                        )
                      ]
                ]
            ],
          testGroup
            "Ordering"
            [ testCase "LT" $
                testMergeableSimpleEquivClass
                  LT
                  [DynamicSortedIdx False]
                  [(ssym "a", LT, LT, LT)],
              testCase "EQ" $
                testMergeableSimpleEquivClass
                  EQ
                  [DynamicSortedIdx True, DynamicSortedIdx False]
                  [(ssym "a", EQ, EQ, EQ)],
              testCase "GT" $
                testMergeableSimpleEquivClass
                  GT
                  [DynamicSortedIdx True, DynamicSortedIdx True]
                  [(ssym "a", GT, GT, GT)]
            ]
        ]
    ]
