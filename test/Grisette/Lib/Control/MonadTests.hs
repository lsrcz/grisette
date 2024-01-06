{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Control.MonadTests (monadFunctionTests) where

import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Grisette.Core.Control.Monad.UnionM (UnionM)
import Grisette.Core.Data.Class.SimpleMergeable
  ( UnionLike (single, unionIf),
    mrgIf,
    mrgSingle,
  )
import Grisette.Lib.Control.Monad
  ( mrgFmap,
    mrgFoldM,
    mrgMplus,
    mrgMzero,
    mrgReturn,
    (.>>),
    (.>>=),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

monadFunctionTests :: Test
monadFunctionTests =
  testGroup
    "Monad"
    [ testCase "mrgReturn" $ do
        (mrgReturn 1 :: UnionM Integer) @?= mrgSingle 1,
      testCase "mrgFoldM" $ do
        ( mrgFoldM
            (\acc (c, v) -> unionIf c (single $ acc + v) (single $ acc * v))
            10
            [("a", 2), ("b", 3)] ::
            UnionM Integer
          )
          @?= mrgIf
            "a"
            (mrgIf "b" (mrgReturn 15) (mrgReturn 36))
            (mrgIf "b" (mrgReturn 23) (mrgReturn 60)),
      testCase "mrgMzero" $ do
        (mrgMzero :: MaybeT UnionM Integer) @?= MaybeT (mrgReturn Nothing),
      testCase "mrgMplus" $ do
        (mrgMzero `mrgMplus` mrgMzero :: MaybeT UnionM Integer) @?= MaybeT (mrgReturn Nothing)
        (mrgReturn 1 `mrgMplus` mrgMzero :: MaybeT UnionM Integer)
          @?= mrgReturn 1
        (mrgMzero `mrgMplus` mrgReturn 1 :: MaybeT UnionM Integer)
          @?= mrgReturn 1
        (mrgReturn 2 `mrgMplus` mrgReturn 1 :: MaybeT UnionM Integer)
          @?= mrgReturn 2,
      testCase "mrgFmap" $ do
        mrgFmap (\x -> x * x) (mrgIf "a" (mrgReturn $ -1) (mrgReturn 1) :: UnionM Integer)
          @?= mrgReturn 1,
      testCase ".>>" $ do
        (unionIf "a" (single $ -1) (single 1) :: UnionM Integer)
          .>> unionIf "a" (single $ -1) (single 1)
          @?= (mrgIf "a" (mrgReturn $ -1) (mrgReturn 1) :: UnionM Integer),
      testCase ".>>=" $ do
        unionIf "a" (single $ -1) (single 1)
          .>>= (\x -> return $ x * x)
          @?= (mrgSingle 1 :: UnionM Integer)
    ]
