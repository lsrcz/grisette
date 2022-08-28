module Pizza.Lib.Data.FoldableTests where

import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Pizza.Core.Control.Monad.UnionMBase
import Pizza.Core.Data.Class.SimpleMergeable
import Pizza.Lib.Control.Monad
import Pizza.Lib.Data.Foldable
import Pizza.TestUtils.SBool
import Test.Tasty
import Test.Tasty.HUnit

foldableFunctionTests :: TestTree
foldableFunctionTests =
  testGroup
    "FoldableTests"
    [ testCase "mrgFoldlM" $ do
        ( mrgFoldlM
            (\acc (c, v) -> unionIf c (single $ acc + v) (single $ acc * v))
            10
            [(SSBool "a", 2), (SSBool "b", 3)] ::
            UnionMBase SBool Integer
          )
          @=? mrgIf
            (SSBool "a")
            (mrgIf (SSBool "b") (mrgReturn 15) (mrgReturn 36))
            (mrgIf (SSBool "b") (mrgReturn 23) (mrgReturn 60)),
      testCase "mrgFoldrM" $ do
        ( mrgFoldrM
            (\(c, v) acc -> unionIf c (single $ acc + v) (single $ acc * v))
            10
            [(SSBool "a", 2), (SSBool "b", 3)] ::
            UnionMBase SBool Integer
          )
          @=? mrgIf
            (SSBool "b")
            (mrgIf (SSBool "a") (mrgReturn 15) (mrgReturn 26))
            (mrgIf (SSBool "a") (mrgReturn 32) (mrgReturn 60)),
      testCase "mrgTraverse_" $ do
        runExceptT
          ( mrgTraverse_
              (\(c, x) -> ExceptT $ unionIf c (return $ Left x) (return $ Right c))
              [(SSBool "a", 3), (SSBool "b", 2)] ::
              ExceptT Integer (UnionMBase SBool) ()
          )
          @=? runExceptT
            ( do
                _ <- mrgIf (SSBool "a") (throwError 3) (return ())
                _ <- mrgIf (SSBool "b") (throwError 2) (return ())
                mrgReturn ()
            ),
      testCase "mrgFor_" $ do
        runExceptT
          ( mrgFor_
              [(SSBool "a", 3), (SSBool "b", 2)]
              (\(c, x) -> ExceptT $ unionIf c (return $ Left x) (return $ Right c)) ::
              ExceptT Integer (UnionMBase SBool) ()
          )
          @=? runExceptT
            ( do
                _ <- mrgIf (SSBool "a") (throwError 3) (return ())
                _ <- mrgIf (SSBool "b") (throwError 2) (return ())
                mrgReturn ()
            ),
      testCase "mrgMapM_" $ do
        runExceptT
          ( mrgMapM_
              (\(c, x) -> ExceptT $ unionIf c (return $ Left x) (return $ Right c))
              [(SSBool "a", 3), (SSBool "b", 2)] ::
              ExceptT Integer (UnionMBase SBool) ()
          )
          @=? runExceptT
            ( do
                _ <- mrgIf (SSBool "a") (throwError 3) (return ())
                _ <- mrgIf (SSBool "b") (throwError 2) (return ())
                mrgReturn ()
            ),
      testCase "mrgForM_" $ do
        runExceptT
          ( mrgForM_
              [(SSBool "a", 3), (SSBool "b", 2)]
              (\(c, x) -> ExceptT $ unionIf c (return $ Left x) (return $ Right c)) ::
              ExceptT Integer (UnionMBase SBool) ()
          )
          @=? runExceptT
            ( do
                _ <- mrgIf (SSBool "a") (throwError 3) (return ())
                _ <- mrgIf (SSBool "b") (throwError 2) (return ())
                mrgReturn ()
            ),
      testCase "mrgSequence_" $ do
        runExceptT
          ( mrgSequence_
              [mrgIf (SSBool "a") (throwError 3) (return ()), mrgIf (SSBool "b") (throwError 2) (return ())] ::
              ExceptT Integer (UnionMBase SBool) ()
          )
          @=? runExceptT
            ( do
                _ <- mrgIf (SSBool "a") (throwError 3) (return ())
                _ <- mrgIf (SSBool "b") (throwError 2) (return ())
                mrgReturn ()
            ),
      testCase "mrgMsum" $ do
        (mrgMsum [mrgMzero, mrgMzero] :: MaybeT (UnionMBase SBool) Integer) @=? mrgMzero
        (mrgMsum [mrgReturn 1, mrgMzero] :: MaybeT (UnionMBase SBool) Integer) @=? mrgReturn 1
        (mrgMsum [mrgMzero, mrgReturn 1] :: MaybeT (UnionMBase SBool) Integer) @=? mrgReturn 1
        (mrgMsum [mrgReturn 2, mrgReturn 1] :: MaybeT (UnionMBase SBool) Integer) @=? mrgReturn 2
    ]
