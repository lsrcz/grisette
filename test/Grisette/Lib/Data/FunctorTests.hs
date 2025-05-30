{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Data.FunctorTests (functorFunctionTests) where

import Grisette
  ( AsKey,
    AsKey1,
    ITEOp (symIte),
    SymBranching (mrgIfPropagatedStrategy),
    SymInteger,
    Union,
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
            actual @?= (mrgSingle 1 :: AsKey1 Union Integer),
          testCase "merge arguments" $ do
            let actual = mrgFmap (const NoMerge) oneNotMerged
            actual @?= (mrgSingle NoMerge :: AsKey1 Union NoMerge)
        ],
      testGroup
        ".<$>"
        [ testCase "merge result" $ do
            let actual =
                  (\x -> x * x)
                    .<$> mrgIfPropagatedStrategy "a" (return $ -1) (return 1)
            actual @?= (mrgSingle 1 :: AsKey1 Union Integer),
          testCase "merge arguments" $ do
            let actual = (const NoMerge) .<$> oneNotMerged
            actual @?= (mrgSingle NoMerge :: AsKey1 Union NoMerge)
        ],
      testGroup
        ".<$"
        [ testCase "merge result" $
            1 .<$ noMergeNotMerged @?= (mrgSingle 1 :: AsKey1 Union Integer),
          testCase "merge arguments" $
            NoMerge .<$ oneNotMerged @?= (mrgSingle NoMerge :: AsKey1 Union NoMerge)
        ],
      testGroup
        ".$>"
        [ testCase "merge result" $
            noMergeNotMerged .$> 1 @?= (mrgSingle 1 :: AsKey1 Union Integer),
          testCase "merge arguments" $
            oneNotMerged .$> NoMerge @?= (mrgSingle NoMerge :: AsKey1 Union NoMerge)
        ],
      testGroup
        ".<&>"
        [ testCase "merge result" $ do
            let actual =
                  mrgIfPropagatedStrategy "a" (return $ -1) (return 1)
                    .<&> (\x -> x * x)
            actual @?= (mrgSingle 1 :: AsKey1 Union Integer),
          testCase "merge arguments" $ do
            let actual = oneNotMerged .<&> (const NoMerge)
            actual @?= (mrgSingle NoMerge :: AsKey1 Union NoMerge)
        ],
      testCase "mrgUnzip" $ do
        let actual =
              mrgUnzip
                (mrgIfPropagatedStrategy "x" (return ("a", 1)) (return ("b", 2)))
        let expected =
              ( mrgSingle (symIte "x" "a" "b"),
                mrgIf "x" 1 2
              ) ::
                (AsKey1 Union (AsKey SymInteger), AsKey1 Union Integer)
        actual @?= expected,
      testCase "mrgVoid" $ do
        let actual = mrgVoid noMergeNotMerged
        let expected = mrgSingle () :: AsKey1 Union ()
        actual @?= expected
    ]
