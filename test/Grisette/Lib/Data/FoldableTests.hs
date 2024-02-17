{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Data.FoldableTests (foldableFunctionTests) where

import Control.Monad.Except
  ( ExceptT (ExceptT),
    MonadError (throwError),
    runExceptT,
  )
import Control.Monad.Trans.Maybe (MaybeT)
import Grisette.Core.Control.Monad.UnionM (UnionM, mergePropagatedIf)
import Grisette.Core.Data.Class.SimpleMergeable
  ( mrgIf,
  )
import Grisette.Lib.Control.Monad (mrgMzero, mrgReturn)
import Grisette.Lib.Data.Foldable
  ( mrgFoldlM,
    mrgFoldrM,
    mrgForM_,
    mrgFor_,
    mrgMapM_,
    mrgMsum,
    mrgSequence_,
    mrgTraverse_,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

foldableFunctionTests :: Test
foldableFunctionTests =
  testGroup
    "Foldable"
    [ testCase "mrgFoldlM" $ do
        ( mrgFoldlM
            (\acc (c, v) -> mergePropagatedIf c (return $ acc + v) (return $ acc * v))
            10
            [("a", 2), ("b", 3)] ::
            UnionM Integer
          )
          @?= mrgIf
            "a"
            (mrgIf "b" (mrgReturn 15) (mrgReturn 36))
            ( mrgIf
                "b"
                (mrgReturn 23)
                (mrgReturn 60)
            ),
      testCase "mrgFoldrM" $ do
        ( mrgFoldrM
            (\(c, v) acc -> mergePropagatedIf c (return $ acc + v) (return $ acc * v))
            10
            [("a", 2), ("b", 3)] ::
            UnionM Integer
          )
          @?= mrgIf
            "b"
            (mrgIf "a" (mrgReturn 15) (mrgReturn 26))
            (mrgIf "a" (mrgReturn 32) (mrgReturn 60)),
      testCase "mrgTraverse_" $ do
        runExceptT
          ( mrgTraverse_
              (\(c, x) -> ExceptT $ mergePropagatedIf c (return $ Left x) (return $ Right c))
              [("a", 3), ("b", 2)] ::
              ExceptT Integer (UnionM) ()
          )
          @?= runExceptT
            ( do
                _ <- mrgIf "a" (throwError 3) (return ())
                _ <- mrgIf "b" (throwError 2) (return ())
                mrgReturn ()
            ),
      testCase "mrgFor_" $ do
        runExceptT
          ( mrgFor_
              [("a", 3), ("b", 2)]
              (\(c, x) -> ExceptT $ mergePropagatedIf c (return $ Left x) (return $ Right c)) ::
              ExceptT Integer UnionM ()
          )
          @?= runExceptT
            ( do
                _ <- mrgIf "a" (throwError 3) (return ())
                _ <- mrgIf "b" (throwError 2) (return ())
                mrgReturn ()
            ),
      testCase "mrgMapM_" $ do
        runExceptT
          ( mrgMapM_
              (\(c, x) -> ExceptT $ mergePropagatedIf c (return $ Left x) (return $ Right c))
              [("a", 3), ("b", 2)] ::
              ExceptT Integer UnionM ()
          )
          @?= runExceptT
            ( do
                _ <- mrgIf "a" (throwError 3) (return ())
                _ <- mrgIf "b" (throwError 2) (return ())
                mrgReturn ()
            ),
      testCase "mrgForM_" $ do
        runExceptT
          ( mrgForM_
              [("a", 3), ("b", 2)]
              (\(c, x) -> ExceptT $ mergePropagatedIf c (return $ Left x) (return $ Right c)) ::
              ExceptT Integer UnionM ()
          )
          @?= runExceptT
            ( do
                _ <- mrgIf "a" (throwError 3) (return ())
                _ <- mrgIf "b" (throwError 2) (return ())
                mrgReturn ()
            ),
      testCase "mrgSequence_" $ do
        runExceptT
          ( mrgSequence_
              [mrgIf "a" (throwError 3) (return ()), mrgIf "b" (throwError 2) (return ())] ::
              ExceptT Integer UnionM ()
          )
          @?= runExceptT
            ( do
                _ <- mrgIf "a" (throwError 3) (return ())
                _ <- mrgIf "b" (throwError 2) (return ())
                mrgReturn ()
            ),
      testCase "mrgMsum" $ do
        (mrgMsum [mrgMzero, mrgMzero] :: MaybeT UnionM Integer) @?= mrgMzero
        (mrgMsum [mrgReturn 1, mrgMzero] :: MaybeT UnionM Integer) @?= mrgReturn 1
        (mrgMsum [mrgMzero, mrgReturn 1] :: MaybeT UnionM Integer) @?= mrgReturn 1
        (mrgMsum [mrgReturn 2, mrgReturn 1] :: MaybeT UnionM Integer) @?= mrgReturn 2
    ]
