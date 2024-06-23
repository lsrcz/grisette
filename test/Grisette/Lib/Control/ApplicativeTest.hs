{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Control.ApplicativeTest
  ( applicativeFunctionTests,
  )
where

import Control.Applicative (Alternative (empty))
import Control.Monad.State
  ( MonadState (get, put),
    MonadTrans (lift),
    StateT (runStateT),
  )
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Grisette
  ( SymBranching (mrgIfPropagatedStrategy),
    UnionM,
    mrgAsum,
    mrgEmpty,
    mrgPure,
    mrgReturn,
    mrgSingle,
  )
import Grisette.Lib.Control.Applicative
  ( mrgLiftA,
    mrgLiftA2,
    mrgLiftA3,
    mrgMany,
    mrgOptional,
    mrgSome,
    (.*>),
    (.<*),
    (.<**>),
    (.<*>),
    (.<|>),
  )
import Grisette.TestUtil.NoMerge
  ( NoMerge (NoMerge),
    noMergeNotMerged,
    oneNotMerged,
  )
import Test.Framework
  ( Test,
    TestOptions' (topt_timeout),
    plusTestOptions,
    testGroup,
  )
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

applicativeFunctionTests :: Test
applicativeFunctionTests =
  testGroup
    "Applicative"
    [ testCase "mrgPure" $ (mrgPure 1 :: UnionM Integer) @?= mrgSingle 1,
      testGroup
        ".<*>"
        [ testCase "merge result" $ do
            let actual =
                  (return (\x -> x * x))
                    .<*> mrgIfPropagatedStrategy "a" (return $ -1) (return 1)
            actual @?= (mrgSingle 1 :: UnionM Integer),
          testCase "merge arguments" $ do
            let actual = (return (const NoMerge)) .<*> oneNotMerged
            actual @?= (mrgSingle NoMerge :: UnionM NoMerge)
        ],
      testGroup
        "mrgLiftA2"
        [ testCase "merge result" $ do
            let actual =
                  mrgLiftA2 (const $ const 1) noMergeNotMerged noMergeNotMerged
            let expected = mrgPure 1 :: UnionM Int
            actual @?= expected,
          testCase "merge argument" $ do
            let actual =
                  mrgLiftA2 (const $ const NoMerge) oneNotMerged oneNotMerged
            let expected = mrgPure NoMerge
            actual @?= expected
        ],
      testGroup
        ".*>"
        [ testCase "merge result" $
            noMergeNotMerged .*> oneNotMerged @?= mrgSingle 1,
          testCase "merge arguments" $
            oneNotMerged .*> return NoMerge @?= mrgSingle NoMerge
        ],
      testGroup
        ".<*"
        [ testCase "merge result" $
            oneNotMerged .<* noMergeNotMerged @?= mrgSingle 1,
          testCase "merge arguments" $
            return NoMerge .<* oneNotMerged @?= mrgSingle NoMerge
        ],
      testCase "mrgEmpty" $
        (mrgEmpty :: MaybeT UnionM Integer) @?= MaybeT (mrgReturn Nothing),
      testGroup
        ".<|>"
        [ testCase "merge result" $
            return 1 .<|> return 2 @?= (mrgSingle 1 :: MaybeT UnionM Integer),
          testCase "merge lhs" $ do
            let lhs =
                  MaybeT $
                    mrgIfPropagatedStrategy "a" (return Nothing) (return Nothing)
            let expected = mrgSingle NoMerge :: MaybeT UnionM NoMerge
            lhs .<|> return NoMerge @?= expected
        ],
      plusTestOptions (mempty {topt_timeout = Just (Just 1000000)}) $
        testGroup
          "mrgSome"
          [ testCase "merge" $
              runStateT (mrgSome f) 100 @?= (mrgSingle (replicate 100 (), 0)),
            testCase "single" $
              runStateT (mrgSome f) 1 @?= (mrgSingle ([()], 0)),
            testCase "zero" $
              runStateT (mrgSome f) 0 @?= MaybeT (mrgReturn Nothing)
          ],
      plusTestOptions (mempty {topt_timeout = Just (Just 1000000)}) $
        testGroup
          "mrgMany"
          [ testCase "merge" $
              runStateT (mrgMany f) 100 @?= (mrgSingle (replicate 100 (), 0)),
            testCase "single" $
              runStateT (mrgMany f) 1 @?= (mrgSingle ([()], 0)),
            testCase "zero" $
              runStateT (mrgMany f) 0 @?= (mrgSingle ([], 0))
          ],
      testGroup
        ".<**>"
        [ testCase "merge result" $ do
            let actual =
                  mrgIfPropagatedStrategy "a" (return $ -1) (return 1)
                    .<**> (return (\x -> x * x))
            actual @?= (mrgSingle 1 :: UnionM Integer),
          testCase "merge arguments" $ do
            let actual = oneNotMerged .<**> (return (const NoMerge))
            actual @?= (mrgSingle NoMerge :: UnionM NoMerge)
        ],
      testGroup
        "mrgLiftA"
        [ testCase "merge result" $ do
            let actual = mrgLiftA (const 1) noMergeNotMerged
            let expected = mrgReturn 1 :: UnionM Int
            actual @?= expected,
          testCase "merge argument" $ do
            let actual = mrgLiftA (const NoMerge) oneNotMerged
            let expected = mrgReturn NoMerge
            actual @?= expected
        ],
      testGroup
        "mrgLiftA3"
        [ testCase "merge result" $ do
            let actual =
                  mrgLiftA3
                    (const $ const $ const 1)
                    noMergeNotMerged
                    noMergeNotMerged
                    noMergeNotMerged
            let expected = mrgReturn 1 :: UnionM Int
            actual @?= expected,
          testCase "merge argument" $ do
            let actual =
                  mrgLiftA3
                    (const $ const $ const NoMerge)
                    oneNotMerged
                    oneNotMerged
                    oneNotMerged
            let expected = mrgReturn NoMerge
            actual @?= expected
        ],
      testGroup
        "mrgOptional"
        [ testCase "one" $ do
            let actual =
                  mrgOptional
                    ( MaybeT $
                        mrgIfPropagatedStrategy
                          "a"
                          (return $ Just 1)
                          (return $ Just 1)
                    )
            let expected = mrgSingle (Just 1) :: MaybeT UnionM (Maybe Int)
            actual @?= expected,
          testCase "none" $ do
            let actual =
                  mrgOptional
                    ( MaybeT $
                        mrgIfPropagatedStrategy "a" (return Nothing) (return Nothing)
                    )
            let expected = mrgSingle Nothing :: MaybeT UnionM (Maybe Int)
            actual @?= expected
        ],
      plusTestOptions (mempty {topt_timeout = Just (Just 1000000)}) $
        testGroup
          "mrgAsum"
          [ testCase "merge" $ do
              let none =
                    MaybeT $
                      mrgIfPropagatedStrategy "a" (return Nothing) (return Nothing)
              let expected =
                    MaybeT (mrgSingle Nothing) ::
                      MaybeT UnionM (Maybe Int)
              mrgAsum (replicate 100 none) @?= expected,
            testCase "semantics" $ do
              (mrgAsum [mrgEmpty, mrgEmpty] :: MaybeT UnionM Integer)
                @?= mrgEmpty
              (mrgAsum [mrgPure 1, mrgEmpty] :: MaybeT UnionM Integer)
                @?= mrgPure 1
              (mrgAsum [mrgEmpty, mrgPure 1] :: MaybeT UnionM Integer)
                @?= mrgPure 1
              (mrgAsum [mrgPure 2, mrgPure 1] :: MaybeT UnionM Integer)
                @?= mrgPure 2
          ]
    ]

f :: StateT Int (MaybeT UnionM) ()
f = do
  i <- get
  if (i == 0)
    then empty
    else do
      put (i - 1)
      lift . lift $
        mrgIfPropagatedStrategy "a" (return ()) (return ())
