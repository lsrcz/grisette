{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Data.TraversableTests (traversableFunctionTests) where

import Control.Monad.Except
  ( ExceptT (ExceptT),
    MonadError (throwError),
  )
import Grisette.Core.Control.Monad.UnionM (UnionM, mergePropagatedIf)
import Grisette.Core.Data.Class.SimpleMergeable
  ( mrgIf,
  )
import Grisette.Core.Data.Class.TryMerge
  ( mrgSingle,
  )
import Grisette.Lib.Control.Monad (mrgReturn)
import Grisette.Lib.Data.Traversable
  ( mrgFor,
    mrgForAccumM,
    mrgForM,
    mrgMapAccumM,
    mrgMapM,
    mrgSequence,
    mrgSequenceA,
    mrgTraverse,
  )
import Grisette.TestUtil.NoMerge (oneNotMerged)
import Test.Framework
  ( Test,
    TestOptions' (topt_timeout),
    plusTestOptions,
    testGroup,
  )
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

traversableFunctionTests :: Test
traversableFunctionTests =
  testGroup
    "Traversable"
    [ plusTestOptions (mempty {topt_timeout = Just (Just 1000000)}) $
        testGroup "mrgTraverse, mrgMapM, mrgFor, mrgForM" $ do
          (name, func0, func1) <-
            [ ("mrgTraverse", mrgTraverse, mrgTraverse),
              ("mrgMapM", mrgMapM, mrgMapM),
              ("mrgFor", flip mrgFor, flip mrgFor),
              ("mrgForM", flip mrgForM, flip mrgForM)
              ]
          return $
            testGroup
              name
              [ testCase "semantics" $ do
                  let actual =
                        func0
                          ( \(c, d, x, y, z) ->
                              ExceptT $
                                mergePropagatedIf
                                  c
                                  (return $ Left x)
                                  ( mergePropagatedIf
                                      d
                                      (return $ Right y)
                                      (return $ Right z)
                                  )
                          )
                          [("a", "c", 3, 4, 5), ("b", "d", 2, 3, 6)] ::
                          ExceptT Integer UnionM [Integer]
                  let expected = do
                        a <-
                          mrgIf
                            "a"
                            (throwError 3)
                            (mrgIf "c" (return 4) (return 5))
                        b <-
                          mrgIf
                            "b"
                            (throwError 2)
                            (mrgIf "d" (return 3) (return 6))
                        mrgSingle [a, b]
                  actual @?= expected,
                testCase "merge intermediate" $ do
                  let actual = func1 (const oneNotMerged) [1 .. 1000]
                  let expected = mrgReturn $ replicate 1000 1
                  actual @?= expected
              ],
      plusTestOptions (mempty {topt_timeout = Just (Just 1000000)}) $
        testGroup "mrgSequenceA, mrgSequence" $ do
          (name, func0, func1) <-
            [ ("mrgSequenceA", mrgSequenceA, mrgSequenceA),
              ("mrgSequence", mrgSequence, mrgSequence)
              ]
          return $
            testGroup
              name
              [ testCase "semantics" $ do
                  let actual =
                        func0
                          [ ExceptT $
                              mergePropagatedIf
                                "a"
                                (return $ Left 3)
                                ( mergePropagatedIf
                                    "c"
                                    (return $ Right 4)
                                    (return $ Right 5)
                                ),
                            ExceptT $
                              mergePropagatedIf
                                "b"
                                (return $ Left 2)
                                ( mergePropagatedIf
                                    "d"
                                    (return $ Right 3)
                                    (return $ Right 6)
                                )
                          ] ::
                          ExceptT Integer UnionM [Integer]
                  let expected = do
                        a <-
                          mrgIf
                            "a"
                            (throwError 3)
                            (mrgIf "c" (return 4) (return 5))
                        b <-
                          mrgIf
                            "b"
                            (throwError 2)
                            (mrgIf "d" (return 3) (return 6))
                        mrgSingle [a, b]
                  actual @?= expected,
                testCase "merge intermediate" $ do
                  let actual = func1 (replicate 1000 oneNotMerged)
                  let expected = mrgReturn $ replicate 1000 1
                  actual @?= expected
              ],
      plusTestOptions (mempty {topt_timeout = Just (Just 1000000)}) $
        testGroup "mrgMapAccumM, mrgForAccumM" $ do
          (name, func0, func1) <-
            [ ("mrgMapAccumM", mrgMapAccumM, mrgMapAccumM),
              ( "mrgForAccumM",
                \f s t -> mrgForAccumM s t f,
                \f s t -> mrgForAccumM s t f
              )
              ]
          return $
            testGroup
              name
              [ testCase "semantics" $ do
                  let actual =
                        func0 (\a b -> mrgSingle (a + b, a - b)) 0 [1 .. 1000]
                  let expected =
                        mrgReturn
                          ( 500500,
                            [(i - 1) * i `div` 2 - i | i <- [1 .. 1000]]
                          ) ::
                          UnionM (Integer, [Integer])
                  actual @?= expected,
                testCase "merge intermediate" $ do
                  let actual =
                        func1
                          ( \_ _ ->
                              mergePropagatedIf
                                "a"
                                (return (1, 1))
                                (return (1, 1))
                          )
                          0
                          [1 .. 1000]
                  let expected =
                        mrgReturn (1, replicate 1000 1) ::
                          UnionM (Integer, [Integer])
                  actual @?= expected
              ]
    ]
