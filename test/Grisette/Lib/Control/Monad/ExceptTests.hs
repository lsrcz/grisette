{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Control.Monad.ExceptTests (monadExceptFunctionTests) where

import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Grisette.Core.Control.Monad.UnionM (UnionM, mergePropagatedIf)
import Grisette.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Core.Data.Class.SEq (SEq ((.==)))
import Grisette.Core.Data.Class.SimpleMergeable (mrgIf)
import Grisette.Core.Data.Class.TryMerge
  ( mrgPure,
  )
import Grisette.IR.SymPrim.Data.SymPrim (SymBool, SymInteger)
import Grisette.Lib.Control.Monad.Except
  ( mrgCatchError,
    mrgHandleError,
    mrgLiftEither,
    mrgMapError,
    mrgModifyError,
    mrgThrowError,
    mrgTryError,
    mrgWithError,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

exceptUnion :: ExceptT SymInteger UnionM SymInteger
exceptUnion =
  ExceptT (mergePropagatedIf "a" (return $ Left "b") (return $ Right "c"))

monadExceptFunctionTests :: Test
monadExceptFunctionTests =
  testGroup
    "Except"
    [ testCase "mrgThrowError" $
        runExceptT (mrgThrowError 1 :: ExceptT Integer UnionM ())
          @?= mrgPure (Left 1),
      testCase "mrgCatchError" $
        ( ExceptT
            (mergePropagatedIf "a" (return $ Left "b") (return $ Right "c")) ::
            ExceptT SymBool UnionM SymBool
        )
          `mrgCatchError` return
          @?= mrgPure (symIte "a" "b" "c"),
      testCase "mrgLiftEither" $ do
        runExceptT (mrgLiftEither (Left "a") :: ExceptT SymBool UnionM ())
          @?= mrgPure (Left "a"),
      testCase "mrgTryError" $ do
        let expected = mrgIf "a" (mrgPure (Left "b")) (mrgPure (Right "c"))
        mrgTryError exceptUnion @?= expected,
      testCase "mrgWithError" $ do
        let expected = mrgIf "a" (mrgThrowError $ "b" + 1) (mrgPure "c")
        mrgWithError (+ 1) exceptUnion @?= expected,
      testCase "mrgCatchError" $
        mrgHandleError
          return
          ( ExceptT
              (mergePropagatedIf "a" (return $ Left "b") (return $ Right "c")) ::
              ExceptT SymBool UnionM SymBool
          )
          @?= mrgPure (symIte "a" "b" "c"),
      testCase "mrgMapError" $ do
        let expected =
              ( mrgIf
                  "a"
                  (mrgThrowError (Just "b"))
                  (mrgPure $ ("c" :: SymInteger) .== 1) ::
                  ExceptT (Maybe SymInteger) UnionM SymBool
              )
        mrgMapError
          ( \m -> ExceptT $ do
              v <- runExceptT m
              case v of
                Left _ -> error "Should not happen"
                Right (Left e) -> return $ Right $ Left $ Just e
                Right (Right v) -> return $ Right $ Right $ v .== 1
          )
          exceptUnion
          @?= expected,
      testCase "mrgModifyError" $ do
        let original =
              mrgIf "a" (mrgThrowError "b") (mrgPure "c") ::
                ExceptT SymInteger (ExceptT SymBool UnionM) SymInteger
        let expected =
              mrgIf
                "a"
                (mrgThrowError $ ("b" :: SymInteger) .== 1)
                (mrgPure "c")
        mrgModifyError (.== 1) original @?= expected
    ]
