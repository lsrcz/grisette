{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Data.TraversableTests (traversableFunctionTests) where

import Control.Monad.Except
  ( ExceptT (ExceptT),
    MonadError (throwError),
    runExceptT,
  )
import Grisette.Core.Control.Monad.UnionM (UnionM)
import Grisette.Core.Data.Class.SimpleMergeable
  ( UnionLike (unionIf),
    mrgIf,
    mrgSingle,
  )
import Grisette.Lib.Data.Traversable
  ( mrgFor,
    mrgForM,
    mrgMapM,
    mrgSequence,
    mrgSequenceA,
    mrgTraverse,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

traversableFunctionTests :: Test
traversableFunctionTests =
  testGroup
    "Traversable"
    [ testCase "mrgTraverse" $ do
        runExceptT
          ( mrgTraverse
              (\(c, d, x, y, z) -> ExceptT $ unionIf c (return $ Left x) (unionIf d (return $ Right y) (return $ Right z)))
              [("a", "c", 3, 4, 5), ("b", "d", 2, 3, 6)] ::
              ExceptT Integer UnionM [Integer]
          )
          @?= runExceptT
            ( do
                a <- mrgIf "a" (throwError 3) (mrgIf "c" (return 4) (return 5))
                b <- mrgIf "b" (throwError 2) (mrgIf "d" (return 3) (return 6))
                mrgSingle [a, b]
            ),
      testCase "mrgSequenceA" $ do
        runExceptT
          ( mrgSequenceA
              [ ExceptT $ unionIf "a" (return $ Left 3) (unionIf "c" (return $ Right 4) (return $ Right 5)),
                ExceptT $ unionIf "b" (return $ Left 2) (unionIf "d" (return $ Right 3) (return $ Right 6))
              ] ::
              ExceptT Integer UnionM [Integer]
          )
          @?= runExceptT
            ( do
                a <- mrgIf "a" (throwError 3) (mrgIf "c" (return 4) (return 5))
                b <- mrgIf "b" (throwError 2) (mrgIf "d" (return 3) (return 6))
                mrgSingle [a, b]
            ),
      testCase "mrgMapM" $ do
        runExceptT
          ( mrgMapM
              (\(c, d, x, y, z) -> ExceptT $ unionIf c (return $ Left x) (unionIf d (return $ Right y) (return $ Right z)))
              [("a", "c", 3, 4, 5), ("b", "d", 2, 3, 6)] ::
              ExceptT Integer UnionM [Integer]
          )
          @?= runExceptT
            ( do
                a <- mrgIf "a" (throwError 3) (mrgIf "c" (return 4) (return 5))
                b <- mrgIf "b" (throwError 2) (mrgIf "d" (return 3) (return 6))
                mrgSingle [a, b]
            ),
      testCase "mrgSequence" $ do
        runExceptT
          ( mrgSequence
              [ ExceptT $ unionIf "a" (return $ Left 3) (unionIf "c" (return $ Right 4) (return $ Right 5)),
                ExceptT $ unionIf "b" (return $ Left 2) (unionIf "d" (return $ Right 3) (return $ Right 6))
              ] ::
              ExceptT Integer UnionM [Integer]
          )
          @?= runExceptT
            ( do
                a <- mrgIf "a" (throwError 3) (mrgIf "c" (return 4) (return 5))
                b <- mrgIf "b" (throwError 2) (mrgIf "d" (return 3) (return 6))
                mrgSingle [a, b]
            ),
      testCase "mrgFor" $ do
        runExceptT
          ( mrgFor
              [("a", "c", 3, 4, 5), ("b", "d", 2, 3, 6)]
              (\(c, d, x, y, z) -> ExceptT $ unionIf c (return $ Left x) (unionIf d (return $ Right y) (return $ Right z))) ::
              ExceptT Integer UnionM [Integer]
          )
          @?= runExceptT
            ( do
                a <- mrgIf "a" (throwError 3) (mrgIf "c" (return 4) (return 5))
                b <- mrgIf "b" (throwError 2) (mrgIf "d" (return 3) (return 6))
                mrgSingle [a, b]
            ),
      testCase "mrgForM" $ do
        runExceptT
          ( mrgForM
              [("a", "c", 3, 4, 5), ("b", "d", 2, 3, 6)]
              (\(c, d, x, y, z) -> ExceptT $ unionIf c (return $ Left x) (unionIf d (return $ Right y) (return $ Right z))) ::
              ExceptT Integer UnionM [Integer]
          )
          @?= runExceptT
            ( do
                a <- mrgIf "a" (throwError 3) (mrgIf "c" (return 4) (return 5))
                b <- mrgIf "b" (throwError 2) (mrgIf "d" (return 3) (return 6))
                mrgSingle [a, b]
            )
    ]
