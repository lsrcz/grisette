module Grisette.Lib.Control.Monad.ExceptTests where

import Control.Monad.Trans.Except
import Grisette.Core.Control.Monad.UnionMBase
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Lib.Control.Monad.Except
import Grisette.TestUtils.SBool
import Test.Tasty
import Test.Tasty.HUnit

monadExceptFunctionTests :: TestTree
monadExceptFunctionTests =
  testGroup
    "ExceptTests"
    [ testCase "mrgThrowError" $
        runExceptT (mrgThrowError 1 :: ExceptT Integer (UnionMBase SBool) ())
          @=? mrgSingle (Left 1),
      testCase "mrgCatchError" $
        ( ExceptT (unionIf (SSBool "a") (return $ Left (SSBool "b")) (return $ Right (SSBool "c"))) ::
            ExceptT SBool (UnionMBase SBool) SBool
        )
          `mrgCatchError` return
          @=? mrgSingle (ITE (SSBool "a") (SSBool "b") (SSBool "c"))
    ]
