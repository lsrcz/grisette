{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Grisette.Core.Data.Class.MergeableTests where

import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Identity
import qualified Control.Monad.RWS.Lazy as RWSTLazy
import qualified Control.Monad.RWS.Strict as RWSTStrict
import Control.Monad.Reader
import qualified Control.Monad.State.Lazy as StateLazy
import qualified Control.Monad.State.Strict as StateStrict
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.ByteString.Char8 as C
import Data.Functor.Sum
import Data.Int
import Data.Word
import Grisette.Core.Control.Monad.UnionMBase
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.TestUtils.Mergeable
import Grisette.TestUtils.SBool
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

mergeableTests :: TestTree
mergeableTests =
  testGroup
    "MergeableTests"
    [ testGroup
        "Mergeable for common types"
        [ let SimpleStrategy f = mergingStrategy :: MergingStrategy SBool SBool
           in testGroup
                "Mergeable for SBool"
                [ testCase "true condition" $
                    f (CBool True) (SSBool "a") (SSBool "b") @=? SSBool "a",
                  testCase "false condition" $
                    f (CBool False) (SSBool "a") (SSBool "b") @=? SSBool "b",
                  testCase "general condition" $
                    f (SSBool "a") (SSBool "b") (SSBool "c") @=? ITE (SSBool "a") (SSBool "b") (SSBool "c")
                ],
          testProperty "Bool" $
            ioProperty . \(x :: Bool) ->
              testMergeableSimpleEquivClass x [DynamicSortedIdx x] [(SSBool "a", x, x, x)],
          testProperty "Integer" $
            ioProperty . \(x :: Integer) ->
              testMergeableSimpleEquivClass x [DynamicSortedIdx x] [(SSBool "a", x, x, x)],
          testProperty "Char" $
            ioProperty . \(x :: Char) ->
              testMergeableSimpleEquivClass x [DynamicSortedIdx x] [(SSBool "a", x, x, x)],
          testProperty "Int" $
            ioProperty . \(x :: Int) ->
              testMergeableSimpleEquivClass x [DynamicSortedIdx x] [(SSBool "a", x, x, x)],
          testProperty "Int8" $
            ioProperty . \(x :: Int8) ->
              testMergeableSimpleEquivClass x [DynamicSortedIdx x] [(SSBool "a", x, x, x)],
          testProperty "Int16" $
            ioProperty . \(x :: Int16) ->
              testMergeableSimpleEquivClass x [DynamicSortedIdx x] [(SSBool "a", x, x, x)],
          testProperty "Int32" $
            ioProperty . \(x :: Int32) ->
              testMergeableSimpleEquivClass x [DynamicSortedIdx x] [(SSBool "a", x, x, x)],
          testProperty "Int64" $
            ioProperty . \(x :: Int64) ->
              testMergeableSimpleEquivClass x [DynamicSortedIdx x] [(SSBool "a", x, x, x)],
          testProperty "Word" $
            ioProperty . \(x :: Word) ->
              testMergeableSimpleEquivClass x [DynamicSortedIdx x] [(SSBool "a", x, x, x)],
          testProperty "Word8" $
            ioProperty . \(x :: Word8) ->
              testMergeableSimpleEquivClass x [DynamicSortedIdx x] [(SSBool "a", x, x, x)],
          testProperty "Word16" $
            ioProperty . \(x :: Word16) ->
              testMergeableSimpleEquivClass x [DynamicSortedIdx x] [(SSBool "a", x, x, x)],
          testProperty "Word32" $
            ioProperty . \(x :: Word32) ->
              testMergeableSimpleEquivClass x [DynamicSortedIdx x] [(SSBool "a", x, x, x)],
          testProperty "Word64" $
            ioProperty . \(x :: Word64) ->
              testMergeableSimpleEquivClass x [DynamicSortedIdx x] [(SSBool "a", x, x, x)],
          testProperty "()" $
            ioProperty . \(x :: ()) ->
              testMergeableSimpleEquivClass x [] [(SSBool "a", x, x, x)],
          testProperty "ByteString" $
            ioProperty . \(x :: String) ->
              let b = C.pack x
               in testMergeableSimpleEquivClass b [DynamicSortedIdx b] [(SSBool "a", b, b, b)],
          testGroup
            "Either"
            [ testGroup
                "Either Integer Integer"
                [ testProperty "Left x" $
                    ioProperty . \(x :: Integer) -> do
                      testMergeableSimpleEquivClass
                        (Left x :: Either Integer Integer)
                        [DynamicSortedIdx False, DynamicSortedIdx x]
                        [(SSBool "a", Left x, Left x, Left x)],
                  testProperty "Right x" $
                    ioProperty . \(x :: Integer) -> do
                      testMergeableSimpleEquivClass
                        (Right x :: Either Integer Integer)
                        [DynamicSortedIdx True, DynamicSortedIdx x]
                        [(SSBool "a", Right x, Right x, Right x)]
                ],
              testGroup
                "Either SBool SBool"
                [ testCase "Left v" $ do
                    let (idxsL, SimpleStrategy fL) = resolveStrategy @SBool mergingStrategy (Left (SSBool "a") :: Either SBool SBool)
                    idxsL @=? [DynamicSortedIdx False]
                    fL (SSBool "a") (Left $ SSBool "b") (Left $ SSBool "c")
                      @=? Left (ITE (SSBool "a") (SSBool "b") (SSBool "c")),
                  testCase "Right v" $ do
                    let (idxsR, SimpleStrategy fR) = resolveStrategy @SBool mergingStrategy (Right (SSBool "a") :: Either SBool SBool)
                    idxsR @=? [DynamicSortedIdx True]
                    fR (SSBool "a") (Right $ SSBool "b") (Right $ SSBool "c")
                      @=? Right (ITE (SSBool "a") (SSBool "b") (SSBool "c"))
                ]
            ],
          testGroup
            "Maybe"
            [ testGroup
                "Maybe Integer"
                [ testProperty "Nothing" $
                    ioProperty . \(x :: Integer) -> do
                      testMergeableSimpleEquivClass
                        (Nothing :: Maybe Integer)
                        [DynamicSortedIdx False]
                        [(SSBool "a", Nothing, Nothing, Nothing)],
                  testProperty "Just v" $
                    ioProperty . \(x :: Integer) -> do
                      testMergeableSimpleEquivClass
                        (Just x :: Maybe Integer)
                        [DynamicSortedIdx True, DynamicSortedIdx x]
                        [(SSBool "a", Just x, Just x, Just x)]
                ],
              testCase "Maybe SBool / Just v" $ do
                let (idxsJ, SimpleStrategy fJ) = resolveStrategy @SBool mergingStrategy (Just (SSBool "a") :: Maybe SBool)
                idxsJ @=? [DynamicSortedIdx True]
                fJ (SSBool "a") (Just $ SSBool "b") (Just $ SSBool "c")
                  @=? Just (ITE (SSBool "a") (SSBool "b") (SSBool "c"))
            ],
          testGroup
            "List"
            [ testCase "BuildStrategyList" $ do
                case buildStrategyList @SBool @Integer mergingStrategy [1, 2, 3] of
                  StrategyList idxs _ -> do
                    idxs
                      @=? [ [DynamicSortedIdx (1 :: Integer)],
                            [DynamicSortedIdx (2 :: Integer)],
                            [DynamicSortedIdx (3 :: Integer)]
                          ],
              testProperty "List for ordered type" $
                ioProperty . \(x :: [Integer]) -> do
                  testMergeableSimpleEquivClass
                    x
                    [DynamicSortedIdx (length x), DynamicSortedIdx $ buildStrategyList @SBool mergingStrategy x]
                    [(SSBool "a", x, x, x)],
              testProperty "Nested List for ordered type" $
                ioProperty . \(x :: [[Integer]]) -> do
                  testMergeableSimpleEquivClass
                    x
                    [DynamicSortedIdx (length x), DynamicSortedIdx $ buildStrategyList @SBool mergingStrategy x]
                    [(SSBool "a", x, x, x)],
              testGroup
                "[SBool]"
                [ testCase "[]" $
                    testMergeableSimpleEquivClass
                      ([] :: [SBool])
                      [DynamicSortedIdx (0 :: Int)]
                      [(SSBool "a", [], [], [])],
                  testCase "[v1, v2]" $
                    testMergeableSimpleEquivClass
                      [SSBool "a", SSBool "b"]
                      [DynamicSortedIdx (2 :: Int)]
                      [ ( SSBool "a",
                          [SSBool "b", SSBool "c"],
                          [SSBool "d", SSBool "e"],
                          [ITE (SSBool "a") (SSBool "b") (SSBool "d"), ITE (SSBool "a") (SSBool "c") (SSBool "e")]
                        )
                      ]
                ]
            ],
          testCase "(,)" $
            testMergeableSimpleEquivClass
              ([1 :: Integer], [SSBool "b", SSBool "c"])
              [ DynamicSortedIdx (1 :: Int),
                DynamicSortedIdx $ buildStrategyList @SBool mergingStrategy [1 :: Integer],
                DynamicSortedIdx (2 :: Int)
              ]
              [ ( SSBool "a",
                  ([1], [SSBool "c", SSBool "d"]),
                  ([1], [SSBool "f", SSBool "g"]),
                  ( [1],
                    [ ITE (SSBool "a") (SSBool "c") (SSBool "f"),
                      ITE (SSBool "a") (SSBool "d") (SSBool "g")
                    ]
                  )
                )
              ],
          testCase "(,,)" $
            testMergeableSimpleEquivClass
              ([1 :: Integer], [SSBool "b", SSBool "c"], SSBool "d")
              [ DynamicSortedIdx (1 :: Int),
                DynamicSortedIdx $ buildStrategyList @SBool mergingStrategy [1 :: Integer],
                DynamicSortedIdx (2 :: Int)
              ]
              [ ( SSBool "a",
                  ([1], [SSBool "c", SSBool "d"], SSBool "e"),
                  ([1], [SSBool "f", SSBool "g"], SSBool "h"),
                  ( [1],
                    [ ITE (SSBool "a") (SSBool "c") (SSBool "f"),
                      ITE (SSBool "a") (SSBool "d") (SSBool "g")
                    ],
                    ITE (SSBool "a") (SSBool "e") (SSBool "h")
                  )
                )
              ],
          testCase "(,,,)" $
            testMergeableSimpleEquivClass
              ([1 :: Integer], [SSBool "b", SSBool "c"], SSBool "d", [SSBool "f"])
              [ DynamicSortedIdx (1 :: Int),
                DynamicSortedIdx $ buildStrategyList @SBool mergingStrategy [1 :: Integer],
                DynamicSortedIdx (2 :: Int),
                DynamicSortedIdx (1 :: Int)
              ]
              [ ( SSBool "a",
                  ([1], [SSBool "c", SSBool "d"], SSBool "e", [SSBool "i"]),
                  ([1], [SSBool "f", SSBool "g"], SSBool "h", [SSBool "j"]),
                  ( [1],
                    [ ITE (SSBool "a") (SSBool "c") (SSBool "f"),
                      ITE (SSBool "a") (SSBool "d") (SSBool "g")
                    ],
                    ITE (SSBool "a") (SSBool "e") (SSBool "h"),
                    [ITE (SSBool "a") (SSBool "i") (SSBool "j")]
                  )
                )
              ],
          testCase "(,,,,)" $
            testMergeableSimpleEquivClass
              ([1 :: Integer], [SSBool "b", SSBool "c"], SSBool "d", [SSBool "f"], [2 :: Integer, 3])
              [ DynamicSortedIdx (1 :: Int),
                DynamicSortedIdx $ buildStrategyList @SBool mergingStrategy [1 :: Integer],
                DynamicSortedIdx (2 :: Int),
                DynamicSortedIdx (1 :: Int),
                DynamicSortedIdx (2 :: Int),
                DynamicSortedIdx $ buildStrategyList @SBool mergingStrategy [2 :: Integer, 3]
              ]
              [ ( SSBool "a",
                  ([1], [SSBool "c", SSBool "d"], SSBool "e", [SSBool "i"], [2, 3]),
                  ([1], [SSBool "f", SSBool "g"], SSBool "h", [SSBool "j"], [2, 3]),
                  ( [1],
                    [ ITE (SSBool "a") (SSBool "c") (SSBool "f"),
                      ITE (SSBool "a") (SSBool "d") (SSBool "g")
                    ],
                    ITE (SSBool "a") (SSBool "e") (SSBool "h"),
                    [ITE (SSBool "a") (SSBool "i") (SSBool "j")],
                    [2, 3]
                  )
                )
              ],
          testCase "(,,,,,)" $
            testMergeableSimpleEquivClass
              ([1 :: Integer], [SSBool "b", SSBool "c"], SSBool "d", [SSBool "f"], [2 :: Integer, 3], 2 :: Integer)
              [ DynamicSortedIdx (1 :: Int),
                DynamicSortedIdx $ buildStrategyList @SBool mergingStrategy [1 :: Integer],
                DynamicSortedIdx (2 :: Int),
                DynamicSortedIdx (1 :: Int),
                DynamicSortedIdx (2 :: Int),
                DynamicSortedIdx $ buildStrategyList @SBool mergingStrategy [2 :: Integer, 3],
                DynamicSortedIdx (2 :: Integer)
              ]
              [ ( SSBool "a",
                  ([1], [SSBool "c", SSBool "d"], SSBool "e", [SSBool "i"], [2, 3], 2),
                  ([1], [SSBool "f", SSBool "g"], SSBool "h", [SSBool "j"], [2, 3], 2),
                  ( [1],
                    [ ITE (SSBool "a") (SSBool "c") (SSBool "f"),
                      ITE (SSBool "a") (SSBool "d") (SSBool "g")
                    ],
                    ITE (SSBool "a") (SSBool "e") (SSBool "h"),
                    [ITE (SSBool "a") (SSBool "i") (SSBool "j")],
                    [2, 3],
                    2
                  )
                )
              ],
          testCase "(,,,,,,)" $
            testMergeableSimpleEquivClass
              ( [1 :: Integer],
                [SSBool "b", SSBool "c"],
                SSBool "d",
                [SSBool "f"],
                [2 :: Integer, 3],
                2 :: Integer,
                Just (SSBool "a")
              )
              [ DynamicSortedIdx (1 :: Int),
                DynamicSortedIdx $ buildStrategyList @SBool mergingStrategy [1 :: Integer],
                DynamicSortedIdx (2 :: Int),
                DynamicSortedIdx (1 :: Int),
                DynamicSortedIdx (2 :: Int),
                DynamicSortedIdx $ buildStrategyList @SBool mergingStrategy [2 :: Integer, 3],
                DynamicSortedIdx (2 :: Integer),
                DynamicSortedIdx True
              ]
              [ ( SSBool "a",
                  ([1], [SSBool "c", SSBool "d"], SSBool "e", [SSBool "i"], [2, 3], 2, Just (SSBool "k")),
                  ([1], [SSBool "f", SSBool "g"], SSBool "h", [SSBool "j"], [2, 3], 2, Just (SSBool "l")),
                  ( [1],
                    [ ITE (SSBool "a") (SSBool "c") (SSBool "f"),
                      ITE (SSBool "a") (SSBool "d") (SSBool "g")
                    ],
                    ITE (SSBool "a") (SSBool "e") (SSBool "h"),
                    [ITE (SSBool "a") (SSBool "i") (SSBool "j")],
                    [2, 3],
                    2,
                    Just $ ITE (SSBool "a") (SSBool "k") (SSBool "l")
                  )
                )
              ],
          testCase "(,,,,,,,)" $
            testMergeableSimpleEquivClass
              ( [1 :: Integer],
                [SSBool "b", SSBool "c"],
                SSBool "d",
                [SSBool "f"],
                [2 :: Integer, 3],
                2 :: Integer,
                Just (SSBool "a"),
                Left 1 :: Either Integer Integer
              )
              [ DynamicSortedIdx (1 :: Int),
                DynamicSortedIdx $ buildStrategyList @SBool mergingStrategy [1 :: Integer],
                DynamicSortedIdx (2 :: Int),
                DynamicSortedIdx (1 :: Int),
                DynamicSortedIdx (2 :: Int),
                DynamicSortedIdx $ buildStrategyList @SBool mergingStrategy [2 :: Integer, 3],
                DynamicSortedIdx (2 :: Integer),
                DynamicSortedIdx True,
                DynamicSortedIdx False,
                DynamicSortedIdx (1 :: Integer)
              ]
              [ ( SSBool "a",
                  ([1], [SSBool "c", SSBool "d"], SSBool "e", [SSBool "i"], [2, 3], 2, Just (SSBool "k"), Left 1),
                  ([1], [SSBool "f", SSBool "g"], SSBool "h", [SSBool "j"], [2, 3], 2, Just (SSBool "l"), Left 1),
                  ( [1],
                    [ ITE (SSBool "a") (SSBool "c") (SSBool "f"),
                      ITE (SSBool "a") (SSBool "d") (SSBool "g")
                    ],
                    ITE (SSBool "a") (SSBool "e") (SSBool "h"),
                    [ITE (SSBool "a") (SSBool "i") (SSBool "j")],
                    [2, 3],
                    2,
                    Just $ ITE (SSBool "a") (SSBool "k") (SSBool "l"),
                    Left 1
                  )
                )
              ],
          let f1 :: Maybe SBool -> SBool = \case Just x -> x; Nothing -> (CBool True)
              f2 :: Maybe SBool -> SBool = \case Just x -> (nots x); Nothing -> (CBool False)
           in testGroup
                "Function"
                [ testCase "Simply mergeable result" $ do
                    case mergingStrategy :: MergingStrategy SBool (Maybe SBool -> SBool) of
                      SimpleStrategy f -> do
                        let r = f (SSBool "a") f1 f2
                        r (Just (SSBool "x")) @=? ITE (SSBool "a") (SSBool "x") (Not (SSBool "x"))
                        r Nothing @=? ITE (SSBool "a") (CBool True) (CBool False)
                      _ -> assertFailure "Bad mergeable strategy type",
                  testCase "Other mergeable result" $ do
                    case mergingStrategy :: MergingStrategy SBool (Maybe SBool -> Integer) of
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
                      [(SSBool "a", MaybeT Nothing, MaybeT Nothing, MaybeT Nothing)],
                  testCase "MaybeT (Just Nothing)" $
                    testMergeableSimpleEquivClass
                      (MaybeT $ Just Nothing :: MaybeT Maybe Integer)
                      [DynamicSortedIdx True, DynamicSortedIdx False]
                      [(SSBool "a", MaybeT $ Just Nothing, MaybeT $ Just Nothing, MaybeT $ Just Nothing)],
                  testProperty "MaybeT (Just (Just v))" $
                    ioProperty . \(x :: Integer) -> do
                      testMergeableSimpleEquivClass
                        (MaybeT $ Just $ Just x :: MaybeT Maybe Integer)
                        [DynamicSortedIdx True, DynamicSortedIdx True, DynamicSortedIdx x]
                        [(SSBool "a", MaybeT $ Just $ Just x, MaybeT $ Just $ Just x, MaybeT $ Just $ Just x)]
                ],
              testCase "MaybeT Maybe SBool / MaybeT (Just (Just v))" $ do
                let (idxsJ, SimpleStrategy fJ) =
                      resolveStrategy @SBool
                        mergingStrategy
                        (MaybeT (Just (Just (SSBool "a"))) :: MaybeT Maybe SBool)
                idxsJ @=? [DynamicSortedIdx True, DynamicSortedIdx True]
                fJ (SSBool "a") (MaybeT $ Just $ Just $ SSBool "b") (MaybeT $ Just $ Just $ SSBool "c")
                  @=? MaybeT (Just (Just (ITE (SSBool "a") (SSBool "b") (SSBool "c"))))
            ],
          testGroup
            "ExceptT"
            [ testGroup
                "ExceptT Integer Maybe Integer"
                [ testCase "ExceptT Nothing" $
                    testMergeableSimpleEquivClass
                      (ExceptT Nothing :: ExceptT Integer Maybe Integer)
                      [DynamicSortedIdx False]
                      [(SSBool "a", ExceptT Nothing, ExceptT Nothing, ExceptT Nothing)],
                  testProperty "ExceptT (Just (Left v))" $
                    ioProperty . \(x :: Integer) -> do
                      testMergeableSimpleEquivClass
                        (ExceptT $ Just $ Left x :: ExceptT Integer Maybe Integer)
                        [DynamicSortedIdx True, DynamicSortedIdx False, DynamicSortedIdx x]
                        [(SSBool "a", ExceptT $ Just $ Left x, ExceptT $ Just $ Left x, ExceptT $ Just $ Left x)],
                  testProperty "ExceptT (Just (Right v))" $
                    ioProperty . \(x :: Integer) -> do
                      testMergeableSimpleEquivClass
                        (ExceptT $ Just $ Right x :: ExceptT Integer Maybe Integer)
                        [DynamicSortedIdx True, DynamicSortedIdx True, DynamicSortedIdx x]
                        [(SSBool "a", ExceptT $ Just $ Right x, ExceptT $ Just $ Right x, ExceptT $ Just $ Right x)]
                ],
              testGroup
                "ExceptT SBool Maybe SBool"
                [ testCase "ExceptT (Just (Left v))" $ do
                    let (idxsJL, SimpleStrategy fJL) =
                          resolveStrategy @SBool
                            mergingStrategy
                            (ExceptT (Just (Left (SSBool "a"))) :: ExceptT SBool Maybe SBool)
                    idxsJL @=? [DynamicSortedIdx True, DynamicSortedIdx False]
                    fJL (SSBool "a") (ExceptT $ Just $ Left $ SSBool "b") (ExceptT $ Just $ Left $ SSBool "c")
                      @=? ExceptT (Just (Left (ITE (SSBool "a") (SSBool "b") (SSBool "c")))),
                  testCase "ExceptT (Just (Right v))" $ do
                    let (idxsJR, SimpleStrategy fJR) =
                          resolveStrategy @SBool
                            mergingStrategy
                            (ExceptT (Just (Right (SSBool "a"))) :: ExceptT SBool Maybe SBool)
                    idxsJR @=? [DynamicSortedIdx True, DynamicSortedIdx True]
                    fJR (SSBool "a") (ExceptT $ Just $ Right $ SSBool "b") (ExceptT $ Just $ Right $ SSBool "c")
                      @=? ExceptT (Just (Right (ITE (SSBool "a") (SSBool "b") (SSBool "c"))))
                ]
            ],
          testGroup
            "StateT"
            [ testCase "Lazy StateT" $ do
                let SimpleStrategy s = mergingStrategy :: MergingStrategy SBool (StateLazy.StateT Integer (UnionMBase SBool) SBool)
                let st1 :: StateLazy.StateT Integer (UnionMBase SBool) SBool =
                      StateLazy.StateT $ \(x :: Integer) -> mrgSingle (SSBool "a", x + 2)
                let st2 :: StateLazy.StateT Integer (UnionMBase SBool) SBool =
                      StateLazy.StateT $ \(x :: Integer) -> mrgSingle (SSBool "b", x * 2)
                let st3 = s (SSBool "c") st1 st2
                StateLazy.runStateT st3 2 @=? mrgSingle (ITE (SSBool "c") (SSBool "a") (SSBool "b"), 4)
                StateLazy.runStateT st3 3 @=? mrgIf (SSBool "c") (mrgSingle (SSBool "a", 5)) (mrgSingle (SSBool "b", 6)),
              testCase "Strict StateT" $ do
                let SimpleStrategy s = mergingStrategy :: MergingStrategy SBool (StateStrict.StateT Integer (UnionMBase SBool) SBool)
                let st1 :: StateStrict.StateT Integer (UnionMBase SBool) SBool =
                      StateStrict.StateT $ \(x :: Integer) -> mrgSingle (SSBool "a", x + 2)
                let st2 :: StateStrict.StateT Integer (UnionMBase SBool) SBool =
                      StateStrict.StateT $ \(x :: Integer) -> mrgSingle (SSBool "b", x * 2)
                let st3 = s (SSBool "c") st1 st2
                StateStrict.runStateT st3 2 @=? mrgSingle (ITE (SSBool "c") (SSBool "a") (SSBool "b"), 4)
                StateStrict.runStateT st3 3 @=? mrgIf (SSBool "c") (mrgSingle (SSBool "a", 5)) (mrgSingle (SSBool "b", 6))
            ],
          testCase "ContT" $ do
            let SimpleStrategy s = mergingStrategy :: MergingStrategy SBool (ContT (SBool, Integer) (UnionMBase SBool) (SBool, Integer))
            let c1 :: ContT (SBool, Integer) (UnionMBase SBool) (SBool, Integer) = ContT $ \f -> f (SSBool "a", 2)
            let c2 :: ContT (SBool, Integer) (UnionMBase SBool) (SBool, Integer) = ContT $ \f -> f (SSBool "b", 3)
            let c3 = s (SSBool "c") c1 c2
            runContT c3 (\(a, x) -> mrgIf (SSBool "p") (mrgSingle (a, x)) (mrgSingle (nots a, x + 1)))
              @=? mrgIf
                (SSBool "c")
                (mrgIf (SSBool "p") (mrgSingle (SSBool "a", 2)) (mrgSingle (Not $ SSBool "a", 3)))
                (mrgIf (SSBool "p") (mrgSingle (SSBool "b", 3)) (mrgSingle (Not $ SSBool "b", 4))),
          testGroup
            "RWST"
            [ testCase "Lazy RWST" $ do
                let SimpleStrategy s =
                      mergingStrategy ::
                        MergingStrategy
                          SBool
                          ( RWSTLazy.RWST
                              (Integer, SBool)
                              (Integer, SBool)
                              (Integer, SBool)
                              (UnionMBase SBool)
                              (Integer, SBool)
                          )
                let rws1 ::
                      RWSTLazy.RWST
                        (Integer, SBool)
                        (Integer, SBool)
                        (Integer, SBool)
                        (UnionMBase SBool)
                        (Integer, SBool) =
                        RWSTLazy.RWST $ \(ir, br) (is, bs) ->
                          mrgSingle ((ir + is, br &&~ bs), (ir - is, br ||~ bs), (ir * is, bs &&~ br))
                let rws2 ::
                      RWSTLazy.RWST
                        (Integer, SBool)
                        (Integer, SBool)
                        (Integer, SBool)
                        (UnionMBase SBool)
                        (Integer, SBool) =
                        RWSTLazy.RWST $ \(ir, br) (is, bs) ->
                          mrgSingle ((ir + is, br ||~ bs), (ir - is, br &&~ bs), (ir * is, bs ||~ br))
                let rws3 = s (SSBool "c") rws1 rws2

                let res1 :: UnionMBase SBool ((Integer, SBool), (Integer, SBool), (Integer, SBool)) =
                      mrgIf
                        (SSBool "c")
                        (mrgSingle ((1, And (SSBool "a") (SSBool "b")), (-1, Or (SSBool "a") (SSBool "b")), (0, And (SSBool "b") (SSBool "a"))))
                        (mrgSingle ((1, Or (SSBool "a") (SSBool "b")), (-1, And (SSBool "a") (SSBool "b")), (0, Or (SSBool "b") (SSBool "a"))))
                RWSTLazy.runRWST rws3 (0, SSBool "a") (1, SSBool "b") @=? res1,
              testCase "Strict RWST" $ do
                let SimpleStrategy s =
                      mergingStrategy ::
                        MergingStrategy
                          SBool
                          ( RWSTStrict.RWST
                              (Integer, SBool)
                              (Integer, SBool)
                              (Integer, SBool)
                              (UnionMBase SBool)
                              (Integer, SBool)
                          )
                let rws1 ::
                      RWSTStrict.RWST
                        (Integer, SBool)
                        (Integer, SBool)
                        (Integer, SBool)
                        (UnionMBase SBool)
                        (Integer, SBool) =
                        RWSTStrict.RWST $ \(ir, br) (is, bs) ->
                          mrgSingle ((ir + is, br &&~ bs), (ir - is, br ||~ bs), (ir * is, bs &&~ br))
                let rws2 ::
                      RWSTStrict.RWST
                        (Integer, SBool)
                        (Integer, SBool)
                        (Integer, SBool)
                        (UnionMBase SBool)
                        (Integer, SBool) =
                        RWSTStrict.RWST $ \(ir, br) (is, bs) ->
                          mrgSingle ((ir + is, br ||~ bs), (ir - is, br &&~ bs), (ir * is, bs ||~ br))
                let rws3 = s (SSBool "c") rws1 rws2

                let res1 :: UnionMBase SBool ((Integer, SBool), (Integer, SBool), (Integer, SBool)) =
                      mrgIf
                        (SSBool "c")
                        (mrgSingle ((1, And (SSBool "a") (SSBool "b")), (-1, Or (SSBool "a") (SSBool "b")), (0, And (SSBool "b") (SSBool "a"))))
                        (mrgSingle ((1, Or (SSBool "a") (SSBool "b")), (-1, And (SSBool "a") (SSBool "b")), (0, Or (SSBool "b") (SSBool "a"))))
                RWSTStrict.runRWST rws3 (0, SSBool "a") (1, SSBool "b") @=? res1
            ],
          testGroup
            "WriterT"
            [ testCase "Lazy WriterT" $ do
                let SimpleStrategy s = mergingStrategy :: MergingStrategy SBool (WriterLazy.WriterT Integer (UnionMBase SBool) SBool)
                let w1 :: WriterLazy.WriterT Integer (UnionMBase SBool) SBool =
                      WriterLazy.WriterT $ mrgSingle (SSBool "a", 1)
                let w2 :: WriterLazy.WriterT Integer (UnionMBase SBool) SBool =
                      WriterLazy.WriterT $ mrgSingle (SSBool "b", 2)
                let w3 :: WriterLazy.WriterT Integer (UnionMBase SBool) SBool =
                      WriterLazy.WriterT $ mrgSingle (SSBool "c", 1)
                let w4 = s (SSBool "d") w1 w2
                let w5 = s (SSBool "d") w1 w3
                WriterLazy.runWriterT w4 @=? mrgIf (SSBool "d") (mrgSingle (SSBool "a", 1)) (mrgSingle (SSBool "b", 2))
                WriterLazy.runWriterT w5 @=? mrgSingle (ITE (SSBool "d") (SSBool "a") (SSBool "c"), 1),
              testCase "Strict WriterT" $ do
                let SimpleStrategy s = mergingStrategy :: MergingStrategy SBool (WriterStrict.WriterT Integer (UnionMBase SBool) SBool)
                let w1 :: WriterStrict.WriterT Integer (UnionMBase SBool) SBool =
                      WriterStrict.WriterT $ mrgSingle (SSBool "a", 1)
                let w2 :: WriterStrict.WriterT Integer (UnionMBase SBool) SBool =
                      WriterStrict.WriterT $ mrgSingle (SSBool "b", 2)
                let w3 :: WriterStrict.WriterT Integer (UnionMBase SBool) SBool =
                      WriterStrict.WriterT $ mrgSingle (SSBool "c", 1)
                let w4 = s (SSBool "d") w1 w2
                let w5 = s (SSBool "d") w1 w3
                WriterStrict.runWriterT w4 @=? mrgIf (SSBool "d") (mrgSingle (SSBool "a", 1)) (mrgSingle (SSBool "b", 2))
                WriterStrict.runWriterT w5 @=? mrgSingle (ITE (SSBool "d") (SSBool "a") (SSBool "c"), 1)
            ],
          testCase "ReaderT" $ do
            let SimpleStrategy s = mergingStrategy :: MergingStrategy SBool (ReaderT Integer (UnionMBase SBool) Integer)
            let r1 :: ReaderT Integer (UnionMBase SBool) Integer =
                  ReaderT $ \(x :: Integer) -> mrgSingle $ x + 2
            let r2 :: ReaderT Integer (UnionMBase SBool) Integer =
                  ReaderT $ \(x :: Integer) -> mrgSingle $ x * 2
            let r3 = s (SSBool "c") r1 r2
            runReaderT r3 2 @=? mrgSingle 4
            runReaderT r3 3 @=? mrgIf (SSBool "c") (mrgSingle 5) (mrgSingle 6),
          testGroup
            "Identity"
            [ testProperty "Identity Integer" $
                ioProperty . \x -> do
                  testMergeableSimpleEquivClass
                    (Identity x :: Identity Integer)
                    [DynamicSortedIdx x]
                    [(SSBool "a", Identity x, Identity x, Identity x)],
              testCase "Identity SBool" $ do
                testMergeableSimpleEquivClass
                  (Identity $ SSBool "a")
                  []
                  [ ( SSBool "a",
                      Identity $ SSBool "b",
                      Identity $ SSBool "c",
                      Identity $ ITE (SSBool "a") (SSBool "b") (SSBool "c")
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
                      [(SSBool "a", IdentityT Nothing, IdentityT Nothing, IdentityT Nothing)],
                  testProperty "IdentityT (Just v)" $
                    ioProperty . \x -> do
                      testMergeableSimpleEquivClass
                        (IdentityT $ Just x :: IdentityT Maybe Integer)
                        [DynamicSortedIdx True, DynamicSortedIdx x]
                        [(SSBool "a", IdentityT $ Just x, IdentityT $ Just x, IdentityT $ Just x)]
                ],
              testGroup
                "IdentityT Maybe SBool"
                [ testCase "IdentityT Nothing" $
                    testMergeableSimpleEquivClass
                      (IdentityT Nothing :: IdentityT Maybe SBool)
                      [DynamicSortedIdx False]
                      [(SSBool "a", IdentityT Nothing, IdentityT Nothing, IdentityT Nothing)],
                  testCase "IdentityT (Just v)" $
                    testMergeableSimpleEquivClass
                      (IdentityT $ Just $ SSBool "a" :: IdentityT Maybe SBool)
                      [DynamicSortedIdx True]
                      [ ( SSBool "a",
                          IdentityT $ Just $ SSBool "b",
                          IdentityT $ Just $ SSBool "c",
                          IdentityT $ Just $ ITE (SSBool "a") (SSBool "b") (SSBool "c")
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
                      [(SSBool "a", InL Nothing, InL Nothing, InL Nothing)],
                  testProperty "InL (Just v)" $
                    ioProperty . \x -> do
                      testMergeableSimpleEquivClass
                        (InL $ Just x :: Sum Maybe Maybe Integer)
                        [DynamicSortedIdx False, DynamicSortedIdx True, DynamicSortedIdx x]
                        [(SSBool "a", InL $ Just x, InL $ Just x, InL $ Just x)],
                  testCase "InR Nothing" $
                    testMergeableSimpleEquivClass
                      (InR Nothing :: Sum Maybe Maybe Integer)
                      [DynamicSortedIdx True, DynamicSortedIdx False]
                      [(SSBool "a", InR Nothing, InR Nothing, InR Nothing)],
                  testProperty "InR (Just v)" $
                    ioProperty . \x -> do
                      testMergeableSimpleEquivClass
                        (InR $ Just x :: Sum Maybe Maybe Integer)
                        [DynamicSortedIdx True, DynamicSortedIdx True, DynamicSortedIdx x]
                        [(SSBool "a", InR $ Just x, InR $ Just x, InR $ Just x)]
                ],
              testGroup
                "Sum Maybe Maybe SBool"
                [ testCase "InL Nothing" $
                    testMergeableSimpleEquivClass
                      (InL Nothing :: Sum Maybe Maybe SBool)
                      [DynamicSortedIdx False, DynamicSortedIdx False]
                      [(SSBool "a", InL Nothing, InL Nothing, InL Nothing)],
                  testCase "InL (Just v)" $
                    testMergeableSimpleEquivClass
                      (InL $ Just $ SSBool "a" :: Sum Maybe Maybe SBool)
                      [DynamicSortedIdx False, DynamicSortedIdx True]
                      [ ( SSBool "a",
                          InL $ Just $ SSBool "b",
                          InL $ Just $ SSBool "c",
                          InL $ Just $ ITE (SSBool "a") (SSBool "b") (SSBool "c")
                        )
                      ],
                  testCase "InR Nothing" $
                    testMergeableSimpleEquivClass
                      (InR Nothing :: Sum Maybe Maybe SBool)
                      [DynamicSortedIdx True, DynamicSortedIdx False]
                      [(SSBool "a", InR Nothing, InR Nothing, InR Nothing)],
                  testCase "InR (Just v)" $
                    testMergeableSimpleEquivClass
                      (InR $ Just $ SSBool "a" :: Sum Maybe Maybe SBool)
                      [DynamicSortedIdx True, DynamicSortedIdx True]
                      [ ( SSBool "a",
                          InR $ Just $ SSBool "b",
                          InR $ Just $ SSBool "c",
                          InR $ Just $ ITE (SSBool "a") (SSBool "b") (SSBool "c")
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
                  [(SSBool "a", LT, LT, LT)],
              testCase "EQ" $
                testMergeableSimpleEquivClass
                  EQ
                  [DynamicSortedIdx True, DynamicSortedIdx False]
                  [(SSBool "a", EQ, EQ, EQ)],
              testCase "GT" $
                testMergeableSimpleEquivClass
                  GT
                  [DynamicSortedIdx True, DynamicSortedIdx True]
                  [(SSBool "a", GT, GT, GT)]
            ]
        ]
    ]
