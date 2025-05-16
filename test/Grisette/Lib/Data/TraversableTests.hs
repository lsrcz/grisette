{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Data.TraversableTests (traversableFunctionTests) where

import Control.Monad.Except
  ( ExceptT (ExceptT),
    MonadError (throwError),
  )
import Grisette
  ( SymBranching (mrgIfPropagatedStrategy),
    Union,
    mrgIf,
    mrgSingle,
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
import Grisette.TestUtil.SymbolicAssertion ((.@?=))
import Test.Framework
  ( Test,
    TestOptions' (topt_timeout),
    plusTestOptions,
    testGroup,
  )
import Test.Framework.Providers.HUnit (testCase)

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
                                mrgIfPropagatedStrategy
                                  c
                                  (return $ Left x)
                                  ( mrgIfPropagatedStrategy
                                      d
                                      (return $ Right y)
                                      (return $ Right z)
                                  )
                          )
                          [("a", "c", 3, 4, 5), ("b", "d", 2, 3, 6)] ::
                          ExceptT Integer Union [Integer]
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
                  actual .@?= expected,
                testCase "merge intermediate" $ do
                  let actual = func1 (const oneNotMerged) [1 .. 1000]
                  let expected = mrgReturn $ replicate 1000 1
                  actual .@?= expected
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
                              mrgIfPropagatedStrategy
                                "a"
                                (return $ Left 3)
                                ( mrgIfPropagatedStrategy
                                    "c"
                                    (return $ Right 4)
                                    (return $ Right 5)
                                ),
                            ExceptT $
                              mrgIfPropagatedStrategy
                                "b"
                                (return $ Left 2)
                                ( mrgIfPropagatedStrategy
                                    "d"
                                    (return $ Right 3)
                                    (return $ Right 6)
                                )
                          ] ::
                          ExceptT Integer Union [Integer]
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
                  actual .@?= expected,
                testCase "merge intermediate" $ do
                  let actual = func1 (replicate 1000 oneNotMerged)
                  let expected = mrgReturn $ replicate 1000 1
                  actual .@?= expected
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
                          Union (Integer, [Integer])
                  actual .@?= expected,
                testCase "merge intermediate" $ do
                  let actual =
                        func1
                          ( \_ _ ->
                              mrgIfPropagatedStrategy
                                "a"
                                (return (1, 1))
                                (return (1, 1))
                          )
                          0
                          [1 .. 1000]
                  let expected =
                        mrgReturn (1, replicate 1000 1) ::
                          Union (Integer, [Integer])
                  actual .@?= expected
              ]
    ]
