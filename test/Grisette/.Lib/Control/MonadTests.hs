{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Control.MonadTests where

import Control.Monad.Trans.Maybe
import Grisette.Core.Control.Monad.UnionMBase
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Lib.Control.Monad
import Grisette.TestUtils.SBool
import Test.Tasty
import Test.Tasty.HUnit

monadFunctionTests :: TestTree
monadFunctionTests =
  testGroup
    "MonadTests"
    [ testCase "mrgReturn" $ do
        (mrgReturn 1 :: UnionMBase SBool Integer) @?= mrgSingle 1,
      testCase "mrgFoldM" $ do
        ( mrgFoldM
            (\acc (c, v) -> unionIf c (single $ acc + v) (single $ acc * v))
            10
            [(SSBool "a", 2), (SSBool "b", 3)] ::
            UnionMBase SBool Integer
          )
          @=? mrgIf
            (SSBool "a")
            (mrgIf (SSBool "b") (mrgReturn 15) (mrgReturn 36))
            (mrgIf (SSBool "b") (mrgReturn 23) (mrgReturn 60)),
      testCase "mrgMzero" $ do
        (mrgMzero :: MaybeT (UnionMBase SBool) Integer) @=? MaybeT (mrgReturn Nothing),
      testCase "mrgMplus" $ do
        (mrgMzero `mrgMplus` mrgMzero :: MaybeT (UnionMBase SBool) Integer) @=? MaybeT (mrgReturn Nothing)
        (mrgReturn 1 `mrgMplus` mrgMzero :: MaybeT (UnionMBase SBool) Integer)
          @=? mrgReturn 1
        (mrgMzero `mrgMplus` mrgReturn 1 :: MaybeT (UnionMBase SBool) Integer)
          @=? mrgReturn 1
        (mrgReturn 2 `mrgMplus` mrgReturn 1 :: MaybeT (UnionMBase SBool) Integer)
          @=? mrgReturn 2,
      testCase "mrgFmap" $ do
        mrgFmap (\x -> x * x) (mrgIf (SSBool "a") (mrgReturn $ -1) (mrgReturn 1) :: UnionMBase SBool Integer)
          @=? mrgReturn 1,
      testCase ">>~" $ do
        (unionIf (SSBool "a") (single $ -1) (single 1) :: UnionMBase SBool Integer)
          >>~ unionIf (SSBool "a") (single $ -1) (single 1)
          @=? (mrgIf (SSBool "a") (mrgReturn $ -1) (mrgReturn 1) :: UnionMBase SBool Integer),
      testCase ">>=~" $ do
        unionIf (SSBool "a") (single $ -1) (single 1)
          >>=~ (\x -> return $ x * x)
          @=? (mrgSingle 1 :: UnionMBase SBool Integer)
    ]
