{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Control.MonadTests (monadFunctionTests) where

import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Grisette.Core.Control.Monad.UnionM (UnionM, mergePropagatedIf)
import Grisette.Core.Data.Class.SimpleMergeable
  ( mrgIf,
  )
import Grisette.Core.Data.Class.TryMerge
  ( mrgPure,
  )
import Grisette.Lib.Control.Monad
  ( mrgFoldM,
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
        (mrgReturn 1 :: UnionM Integer) @?= mrgPure 1,
      testCase "mrgFoldM" $ do
        ( mrgFoldM
            (\acc (c, v) -> mergePropagatedIf c (return $ acc + v) (return $ acc * v))
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
      testCase ".>>" $ do
        (mergePropagatedIf "a" (return $ -1) (return 1) :: UnionM Integer)
          .>> mergePropagatedIf "a" (return $ -1) (return 1)
          @?= (mrgIf "a" (mrgReturn $ -1) (mrgReturn 1) :: UnionM Integer),
      testCase ".>>=" $ do
        mergePropagatedIf "a" (return $ -1) (return 1)
          .>>= (\x -> return $ x * x)
          @?= (mrgPure 1 :: UnionM Integer)
    ]
