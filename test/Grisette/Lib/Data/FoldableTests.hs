{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Data.FoldableTests (foldableFunctionTests) where

import Control.Monad.Except
  ( ExceptT (ExceptT),
    MonadError (throwError),
    runExceptT,
  )
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Grisette
  ( ITEOp (symIte),
    SOrd (symCompare),
    symAll,
    symAny,
    symOr,
  )
import Grisette.Core.Control.Monad.UnionM (UnionM)
import Grisette.Core.Data.Class.LogicalOp (LogicalOp ((.&&), (.||)))
import Grisette.Core.Data.Class.SEq (SEq ((./=), (.==)))
import Grisette.Core.Data.Class.SOrd (SOrd ((.<=), (.>)))
import Grisette.Core.Data.Class.SimpleMergeable
  ( UnionMergeable1 (mrgIfPropagatedStrategy),
    mrgIf,
  )
import Grisette.Lib.Control.Monad (mrgMzero, mrgReturn)
import Grisette.Lib.Data.Foldable
  ( mrgFind,
    mrgFoldlM,
    mrgFoldrM,
    mrgForM_,
    mrgFor_,
    mrgMapM_,
    mrgMaximum,
    mrgMaximumBy,
    mrgMinimum,
    mrgMinimumBy,
    mrgMsum,
    mrgSequenceA_,
    mrgSequence_,
    mrgTraverse_,
    symAnd,
    symElem,
    symMaximum,
    symMaximumBy,
    symMinimum,
    symMinimumBy,
    symNotElem,
  )
import Grisette.SymPrim (SymBool, SymInteger)
import Grisette.TestUtil.NoMerge (noMergeNotMerged, oneNotMerged)
import Grisette.TestUtil.SymbolicAssertion ((.@?=))
import Test.Framework
  ( Test,
    TestOptions' (topt_timeout),
    plusTestOptions,
    testGroup,
  )
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

foldableFunctionTests :: Test
foldableFunctionTests =
  testGroup
    "Foldable"
    [ testCase "symElem" $ do
        let [a, b, c] = ["a", "b", "c"] :: [SymInteger]
        let actual = symElem a [b, c]
        actual .@?= (a .== b .|| a .== c),
      testCase "mrgMaximum" $ do
        let [a, b, c] = ["a", "b", "c"] :: [SymInteger]
        let actual = mrgMaximum [[a], [b, a], [c, a, b]]
        let expected =
              mrgIf
                (a .> b .&& a .> c)
                (return [a])
                (mrgIf (b .> c) (return [b, a]) (return [c, a, b])) ::
                UnionM [SymInteger]
        actual .@?= expected,
      testCase "symMaximum" $ do
        let [a, b, c] = ["a", "b", "c"] :: [SymInteger]
        let actual = symMaximum [a, b, c]
        let expected = symIte (a .> b .&& a .> c) a (symIte (b .> c) b c)
        actual .@?= expected,
      testCase "mrgMinimum" $ do
        let [a, b, c] = ["a", "b", "c"] :: [SymInteger]
        let actual = mrgMinimum [[a], [b, a], [c, a, b]]
        let expected =
              mrgIf
                (a .<= b .&& a .<= c)
                (return [a])
                (mrgIf (b .<= c) (return [b, a]) (return [c, a, b])) ::
                UnionM [SymInteger]
        actual .@?= expected,
      testCase "symMinimum" $ do
        let [a, b, c] = ["a", "b", "c"] :: [SymInteger]
        let actual = symMinimum [a, b, c]
        let expected = symIte (a .<= b .&& a .<= c) a (symIte (b .<= c) b c)
        actual .@?= expected,
      plusTestOptions (mempty {topt_timeout = Just (Just 1000000)}) $
        testGroup
          "mrgFoldrM"
          [ testCase "semantics" $ do
              let actual =
                    mrgFoldrM
                      ( \(c, v) acc ->
                          mrgIfPropagatedStrategy
                            c
                            (return $ acc + v)
                            (return $ acc * v)
                      )
                      10
                      [("a", 2), ("b", 3)] ::
                      UnionM Integer
              let expected =
                    mrgIf
                      "b"
                      (mrgIf "a" (mrgReturn 15) (mrgReturn 26))
                      (mrgIf "a" (mrgReturn 32) (mrgReturn 60))
              actual @?= expected,
            testCase "merge intermediate" $ do
              let actual = mrgFoldrM (const $ const oneNotMerged) 1 [1 .. 1000]
              let expected = mrgReturn 1 :: UnionM Int
              actual @?= expected
          ],
      plusTestOptions (mempty {topt_timeout = Just (Just 1000000)}) $
        testGroup
          "mrgFoldlM"
          [ testCase "semantics" $ do
              let actual =
                    mrgFoldlM
                      ( \acc (c, v) ->
                          mrgIfPropagatedStrategy
                            c
                            (return $ acc + v)
                            (return $ acc * v)
                      )
                      10
                      [("a", 2), ("b", 3)] ::
                      UnionM Integer
              let expected =
                    mrgIf
                      "a"
                      (mrgIf "b" (mrgReturn 15) (mrgReturn 36))
                      (mrgIf "b" (mrgReturn 23) (mrgReturn 60))
              actual @?= expected,
            testCase "merge intermediate" $ do
              let actual = mrgFoldlM (const $ const oneNotMerged) 1 [1 .. 1000]
              let expected = mrgReturn 1 :: UnionM Int
              actual @?= expected
          ],
      plusTestOptions (mempty {topt_timeout = Just (Just 1000000)}) $
        testGroup "mrgTraverse_, mrgMapM_, mrgFor_, mrgForM_" $ do
          (name, func0, func1) <-
            [ ("mrgTraverse_", mrgTraverse_, mrgTraverse_),
              ("mrgMapM_", mrgMapM_, mrgMapM_),
              ("mrgFor_", flip mrgFor_, flip mrgFor_),
              ("mrgForM_", flip mrgForM_, flip mrgForM_)
              ]
          [ testGroup
              name
              [ testCase "semantics" $ do
                  let actual =
                        runExceptT
                          ( func0
                              ( \(c, x) ->
                                  ExceptT $
                                    mrgIfPropagatedStrategy
                                      c
                                      (return $ Left x)
                                      (return $ Right c)
                              )
                              [("a", 3), ("b", 2)] ::
                              ExceptT Integer UnionM ()
                          )
                  let expected = runExceptT $ do
                        _ <- mrgIf "a" (throwError 3) (return ())
                        _ <- mrgIf "b" (throwError 2) (return ())
                        mrgReturn ()
                  actual @?= expected,
                testCase "discard and merge intermediate" $ do
                  let actual = func1 (const noMergeNotMerged) [1 .. 1000]
                  let expected = mrgReturn () :: UnionM ()
                  actual @?= expected
              ]
            ],
      plusTestOptions (mempty {topt_timeout = Just (Just 1000000)}) $
        testGroup "mrgSequence_, mrgSequenceA_" $ do
          (name, func0, func1) <-
            [ ("mrgSequence_", mrgSequence_, mrgSequence_),
              ("mrgSequenceA_", mrgSequenceA_, mrgSequenceA_)
              ]
          [ testGroup
              name
              [ testCase "semantics" $ do
                  let actual =
                        runExceptT
                          ( func0 $
                              ( \(c, x) ->
                                  ExceptT $
                                    mrgIfPropagatedStrategy
                                      c
                                      (return $ Left x)
                                      (return $ Right c)
                              )
                                <$> [("a", 3), ("b", 2)] ::
                              ExceptT Integer UnionM ()
                          )
                  let expected = runExceptT $ do
                        _ <- mrgIf "a" (throwError 3) (return ())
                        _ <- mrgIf "b" (throwError 2) (return ())
                        mrgReturn ()
                  actual @?= expected,
                testCase "discard and merge intermediate" $ do
                  let actual = func1 (replicate 1000 noMergeNotMerged)
                  let expected = mrgReturn () :: UnionM ()
                  actual @?= expected
              ]
            ],
      plusTestOptions (mempty {topt_timeout = Just (Just 1000000)}) $
        testGroup
          "mrgMsum"
          [ testCase "merge" $ do
              let none =
                    MaybeT $
                      mrgIfPropagatedStrategy "a" (return Nothing) (return Nothing)
              let expected =
                    MaybeT (mrgReturn Nothing) ::
                      MaybeT UnionM (Maybe Int)
              mrgMsum (replicate 100 none) @?= expected,
            testCase "semantics" $ do
              (mrgMsum [mrgMzero, mrgMzero] :: MaybeT UnionM Integer)
                @?= mrgMzero
              (mrgMsum [mrgReturn 1, mrgMzero] :: MaybeT UnionM Integer)
                @?= mrgReturn 1
              (mrgMsum [mrgMzero, mrgReturn 1] :: MaybeT UnionM Integer)
                @?= mrgReturn 1
              (mrgMsum [mrgReturn 2, mrgReturn 1] :: MaybeT UnionM Integer)
                @?= mrgReturn 2
          ],
      testCase "symAnd" $ do
        let [a, b, c] = ["a", "b", "c"] :: [SymBool]
        symAnd [a, b, c] .@?= (a .&& b .&& c),
      testCase "symOr" $ do
        let [a, b, c] = ["a", "b", "c"] :: [SymBool]
        symOr [a, b, c] .@?= (a .|| b .|| c),
      testCase "symAny" $ do
        let [a, b, c] = ["a", "b", "c"] :: [SymInteger]
        symAny (.== 0) [a, b, c] .@?= (a .== 0 .|| b .== 0 .|| c .== 0),
      testCase "symAll" $ do
        let [a, b, c] = ["a", "b", "c"] :: [SymInteger]
        symAll (.== 0) [a, b, c] .@?= (a .== 0 .&& b .== 0 .&& c .== 0),
      testCase "mrgMaximumBy" $ do
        let [a, b, c] = ["a", "b", "c"] :: [SymInteger]
        let actual = mrgMaximumBy symCompare [[a], [b, a], [c, a, b]]
        let expected =
              mrgIf
                (a .> b .&& a .> c)
                (return [a])
                (mrgIf (b .> c) (return [b, a]) (return [c, a, b])) ::
                UnionM [SymInteger]
        actual .@?= expected,
      testCase "symMaximumBy" $ do
        let [a, b, c] = ["a", "b", "c"] :: [SymInteger]
        let actual = symMaximumBy symCompare [a, b, c]
        let expected = symIte (a .> b .&& a .> c) a (symIte (b .> c) b c)
        actual .@?= expected,
      testCase "mrgMinimumBy" $ do
        let [a, b, c] = ["a", "b", "c"] :: [SymInteger]
        let actual = mrgMinimumBy symCompare [[a], [b, a], [c, a, b]]
        let expected =
              mrgIf
                (a .<= b .&& a .<= c)
                (return [a])
                (mrgIf (b .<= c) (return [b, a]) (return [c, a, b])) ::
                UnionM [SymInteger]
        actual .@?= expected,
      testCase "symMinimumBy" $ do
        let [a, b, c] = ["a", "b", "c"] :: [SymInteger]
        let actual = symMinimumBy symCompare [a, b, c]
        let expected = symIte (a .<= b .&& a .<= c) a (symIte (b .<= c) b c)
        actual .@?= expected,
      testCase "symNotElem" $ do
        let [a, b, c] = ["a", "b", "c"] :: [SymInteger]
        let actual = symNotElem a [b, c]
        actual .@?= (a ./= b .&& a ./= c),
      testCase "mrgFind" $ do
        let [a, b, c] = ["a", "b", "c"] :: [SymInteger]
        let actual = mrgFind (.== 0) [a, b, c] :: UnionM (Maybe SymInteger)
        actual
          .@?= mrgIf
            (a .== 0)
            (return $ Just a)
            ( mrgIf
                (b .== 0)
                (return $ Just b)
                (mrgIf (c .== 0) (return $ Just c) (return Nothing))
            )
    ]
