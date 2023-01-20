module Grisette.Lib.Data.TraversableTests where

import Control.Monad.Except
import Grisette.Core.Control.Monad.UnionMBase
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Lib.Data.Traversable
import Grisette.TestUtils.SBool
import Test.Tasty
import Test.Tasty.HUnit

traversableFunctionTests :: TestTree
traversableFunctionTests =
  testGroup
    "TraversableSpec"
    [ testCase "mrgTraverse" $ do
        runExceptT
          ( mrgTraverse
              (\(c, d, x, y, z) -> ExceptT $ unionIf c (return $ Left x) (unionIf d (return $ Right y) (return $ Right z)))
              [(SSBool "a", SSBool "c", 3, 4, 5), (SSBool "b", SSBool "d", 2, 3, 6)] ::
              ExceptT Integer (UnionMBase SBool) [Integer]
          )
          @=? runExceptT
            ( do
                a <- mrgIf (SSBool "a") (throwError 3) (mrgIf (SSBool "c") (return 4) (return 5))
                b <- mrgIf (SSBool "b") (throwError 2) (mrgIf (SSBool "d") (return 3) (return 6))
                mrgSingle [a, b]
            ),
      testCase "mrgSequenceA" $ do
        runExceptT
          ( mrgSequenceA
              [ ExceptT $ unionIf (SSBool "a") (return $ Left 3) (unionIf (SSBool "c") (return $ Right 4) (return $ Right 5)),
                ExceptT $ unionIf (SSBool "b") (return $ Left 2) (unionIf (SSBool "d") (return $ Right 3) (return $ Right 6))
              ] ::
              ExceptT Integer (UnionMBase SBool) [Integer]
          )
          @=? runExceptT
            ( do
                a <- mrgIf (SSBool "a") (throwError 3) (mrgIf (SSBool "c") (return 4) (return 5))
                b <- mrgIf (SSBool "b") (throwError 2) (mrgIf (SSBool "d") (return 3) (return 6))
                mrgSingle [a, b]
            ),
      testCase "mrgMapM" $ do
        runExceptT
          ( mrgMapM
              (\(c, d, x, y, z) -> ExceptT $ unionIf c (return $ Left x) (unionIf d (return $ Right y) (return $ Right z)))
              [(SSBool "a", SSBool "c", 3, 4, 5), (SSBool "b", SSBool "d", 2, 3, 6)] ::
              ExceptT Integer (UnionMBase SBool) [Integer]
          )
          @=? runExceptT
            ( do
                a <- mrgIf (SSBool "a") (throwError 3) (mrgIf (SSBool "c") (return 4) (return 5))
                b <- mrgIf (SSBool "b") (throwError 2) (mrgIf (SSBool "d") (return 3) (return 6))
                mrgSingle [a, b]
            ),
      testCase "mrgSequence" $ do
        runExceptT
          ( mrgSequence
              [ ExceptT $ unionIf (SSBool "a") (return $ Left 3) (unionIf (SSBool "c") (return $ Right 4) (return $ Right 5)),
                ExceptT $ unionIf (SSBool "b") (return $ Left 2) (unionIf (SSBool "d") (return $ Right 3) (return $ Right 6))
              ] ::
              ExceptT Integer (UnionMBase SBool) [Integer]
          )
          @=? runExceptT
            ( do
                a <- mrgIf (SSBool "a") (throwError 3) (mrgIf (SSBool "c") (return 4) (return 5))
                b <- mrgIf (SSBool "b") (throwError 2) (mrgIf (SSBool "d") (return 3) (return 6))
                mrgSingle [a, b]
            ),
      testCase "mrgFor" $ do
        runExceptT
          ( mrgFor
              [(SSBool "a", SSBool "c", 3, 4, 5), (SSBool "b", SSBool "d", 2, 3, 6)]
              (\(c, d, x, y, z) -> ExceptT $ unionIf c (return $ Left x) (unionIf d (return $ Right y) (return $ Right z))) ::
              ExceptT Integer (UnionMBase SBool) [Integer]
          )
          @=? runExceptT
            ( do
                a <- mrgIf (SSBool "a") (throwError 3) (mrgIf (SSBool "c") (return 4) (return 5))
                b <- mrgIf (SSBool "b") (throwError 2) (mrgIf (SSBool "d") (return 3) (return 6))
                mrgSingle [a, b]
            ),
      testCase "mrgForM" $ do
        runExceptT
          ( mrgForM
              [(SSBool "a", SSBool "c", 3, 4, 5), (SSBool "b", SSBool "d", 2, 3, 6)]
              (\(c, d, x, y, z) -> ExceptT $ unionIf c (return $ Left x) (unionIf d (return $ Right y) (return $ Right z))) ::
              ExceptT Integer (UnionMBase SBool) [Integer]
          )
          @=? runExceptT
            ( do
                a <- mrgIf (SSBool "a") (throwError 3) (mrgIf (SSBool "c") (return 4) (return 5))
                b <- mrgIf (SSBool "b") (throwError 2) (mrgIf (SSBool "d") (return 3) (return 6))
                mrgSingle [a, b]
            )
    ]
