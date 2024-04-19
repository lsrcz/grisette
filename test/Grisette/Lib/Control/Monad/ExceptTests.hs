{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Control.Monad.ExceptTests (monadExceptFunctionTests) where

import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Grisette.Core.Control.Monad.UnionM (UnionM)
import Grisette.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Core.Data.Class.SEq (SEq ((.==)))
import Grisette.Core.Data.Class.SimpleMergeable
  ( UnionMergeable1 (mrgIfPropagatedStrategy),
    mrgIf,
  )
import Grisette.Core.Data.Class.TryMerge
  ( mrgSingle,
  )
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
import Grisette.SymPrim (SymBool, SymInteger)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

exceptUnion :: ExceptT SymInteger UnionM SymInteger
exceptUnion = mrgIfPropagatedStrategy "a" (throwError "b") (return "c")

monadExceptFunctionTests :: Test
monadExceptFunctionTests =
  testGroup
    "Except"
    [ testCase "mrgThrowError" $
        runExceptT (mrgThrowError 1 :: ExceptT Integer UnionM ())
          @?= mrgSingle (Left 1),
      testCase "mrgCatchError" $
        ( mrgIfPropagatedStrategy "a" (throwError "b") (return "c") ::
            ExceptT SymBool UnionM SymBool
        )
          `mrgCatchError` return
          @?= mrgSingle (symIte "a" "b" "c"),
      testCase "mrgLiftEither" $ do
        runExceptT (mrgLiftEither (Left "a") :: ExceptT SymBool UnionM ())
          @?= mrgSingle (Left "a"),
      testCase "mrgTryError" $ do
        let expected = mrgIf "a" (mrgSingle (Left "b")) (mrgSingle (Right "c"))
        mrgTryError exceptUnion @?= expected,
      testCase "mrgWithError" $ do
        let expected = mrgIf "a" (mrgThrowError $ "b" + 1) (mrgSingle "c")
        mrgWithError (+ 1) exceptUnion @?= expected,
      testCase "mrgCatchError" $
        mrgHandleError
          return
          ( mrgIfPropagatedStrategy "a" (throwError "b") (return "c") ::
              ExceptT SymBool UnionM SymBool
          )
          @?= mrgSingle (symIte "a" "b" "c"),
      testCase "mrgMapError" $ do
        let expected =
              ( mrgIf
                  "a"
                  (mrgThrowError (Just "b"))
                  (mrgSingle $ ("c" :: SymInteger) .== 1) ::
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
              mrgIf "a" (mrgThrowError "b") (mrgSingle "c") ::
                ExceptT SymInteger (ExceptT SymBool UnionM) SymInteger
        let expected =
              mrgIf
                "a"
                (mrgThrowError $ ("b" :: SymInteger) .== 1)
                (mrgSingle "c")
        mrgModifyError (.== 1) original @?= expected
    ]
