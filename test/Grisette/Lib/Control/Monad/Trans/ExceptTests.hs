{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Control.Monad.Trans.ExceptTests (exceptTests) where

import Control.Monad.Except
  ( ExceptT (ExceptT),
    MonadError (throwError),
    runExceptT,
  )
import Grisette (mrgIf)
import Grisette.Core.Control.Monad.UnionM (UnionM, mergePropagatedIf)
import Grisette.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Core.Data.Class.TryMerge (mrgSingle)
import Grisette.IR.SymPrim.Data.SymPrim (SymBool, SymInteger)
import Grisette.Lib.Control.Monad.Trans.Except
  ( mrgCatchE,
    mrgExcept,
    mrgRunExceptT,
    mrgThrowE,
    mrgWithExceptT,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

unmergedExceptT :: ExceptT SymInteger UnionM SymBool
unmergedExceptT =
  ExceptT
    ( mergePropagatedIf
        "e"
        (mergePropagatedIf "c" (return $ Left "a") (return $ Left "b"))
        (return $ Right "d")
    )

mergedExceptT :: ExceptT SymInteger UnionM SymBool
mergedExceptT =
  ExceptT $
    mrgIf "e" (mrgSingle (Left (symIte "c" "a" "b"))) (mrgSingle (Right "d"))

mergedExceptTPlus1 :: ExceptT SymInteger UnionM SymBool
mergedExceptTPlus1 =
  ExceptT $
    mrgIf "e" (mrgSingle (Left (symIte "c" "a" "b" + 1))) (mrgSingle (Right "d"))

exceptTests :: Test
exceptTests =
  testGroup
    "Except"
    [ testCase "mrgExcept" $ do
        let actual = mrgExcept (Left "a") :: ExceptT SymInteger UnionM SymBool
        let expected = ExceptT (mrgSingle (Left "a"))
        actual @?= expected,
      testCase "mrgRunExceptT" $ do
        mrgRunExceptT unmergedExceptT @?= runExceptT mergedExceptT,
      testCase "mrgWithExceptT" $ do
        mrgWithExceptT (+ 1) unmergedExceptT @?= mergedExceptTPlus1,
      testCase "mrgThrowE" $ do
        let actual = mrgThrowE "a" :: ExceptT SymInteger UnionM SymBool
        actual @?= ExceptT (mrgSingle (Left "a")),
      testCase "mrgCatchE" $ do
        let actual = mrgCatchE unmergedExceptT (throwError . (+ 1))
        actual @?= mergedExceptTPlus1
    ]
