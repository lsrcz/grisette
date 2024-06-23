{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Data.FunctorTests (functorFunctionTests) where

import Grisette
  ( ITEOp (symIte),
    SymBranching (mrgIfPropagatedStrategy),
    SymInteger,
    UnionM,
    mrgIf,
    mrgSingle,
    mrgVoid,
  )
import Grisette.Lib.Data.Functor
  ( mrgFmap,
    mrgUnzip,
    (.$>),
    (.<$),
    (.<$>),
    (.<&>),
  )
import Grisette.TestUtil.NoMerge
  ( NoMerge (NoMerge),
    noMergeNotMerged,
    oneNotMerged,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

functorFunctionTests :: Test
functorFunctionTests =
  testGroup
    "Functor"
    [ testGroup
        "mrgFmap"
        [ testCase "merge result" $ do
            let actual =
                  mrgFmap (\x -> x * x) $
                    mrgIfPropagatedStrategy "a" (return $ -1) (return 1)
            actual @?= (mrgSingle 1 :: UnionM Integer),
          testCase "merge arguments" $ do
            let actual = mrgFmap (const NoMerge) oneNotMerged
            actual @?= (mrgSingle NoMerge :: UnionM NoMerge)
        ],
      testGroup
        ".<$>"
        [ testCase "merge result" $ do
            let actual =
                  (\x -> x * x)
                    .<$> mrgIfPropagatedStrategy "a" (return $ -1) (return 1)
            actual @?= (mrgSingle 1 :: UnionM Integer),
          testCase "merge arguments" $ do
            let actual = (const NoMerge) .<$> oneNotMerged
            actual @?= (mrgSingle NoMerge :: UnionM NoMerge)
        ],
      testGroup
        ".<$"
        [ testCase "merge result" $
            1 .<$ noMergeNotMerged @?= (mrgSingle 1 :: UnionM Integer),
          testCase "merge arguments" $
            NoMerge .<$ oneNotMerged @?= (mrgSingle NoMerge :: UnionM NoMerge)
        ],
      testGroup
        ".$>"
        [ testCase "merge result" $
            noMergeNotMerged .$> 1 @?= (mrgSingle 1 :: UnionM Integer),
          testCase "merge arguments" $
            oneNotMerged .$> NoMerge @?= (mrgSingle NoMerge :: UnionM NoMerge)
        ],
      testGroup
        ".<&>"
        [ testCase "merge result" $ do
            let actual =
                  mrgIfPropagatedStrategy "a" (return $ -1) (return 1)
                    .<&> (\x -> x * x)
            actual @?= (mrgSingle 1 :: UnionM Integer),
          testCase "merge arguments" $ do
            let actual = oneNotMerged .<&> (const NoMerge)
            actual @?= (mrgSingle NoMerge :: UnionM NoMerge)
        ],
      testCase "mrgUnzip" $ do
        let actual =
              mrgUnzip
                (mrgIfPropagatedStrategy "x" (return ("a", 1)) (return ("b", 2)))
        let expected =
              ( mrgSingle (symIte "x" "a" "b"),
                mrgIf "x" 1 2
              ) ::
                (UnionM SymInteger, UnionM Integer)
        actual @?= expected,
      testCase "mrgVoid" $ do
        let actual = mrgVoid noMergeNotMerged
        let expected = mrgSingle () :: UnionM ()
        actual @?= expected
    ]
