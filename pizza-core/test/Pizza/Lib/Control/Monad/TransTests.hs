module Pizza.Lib.Control.Monad.TransTests where

import Control.Monad.Except
import Pizza.Core.Control.Monad.UnionMBase
import Pizza.Core.Data.Class.SimpleMergeable
import Pizza.Lib.Control.Monad.Trans
import Pizza.TestUtils.SBool
import Test.Tasty
import Test.Tasty.HUnit

monadTransFunctionTests :: TestTree
monadTransFunctionTests =
  testGroup
    "TransSpec"
    [ testCase "mrgLift" $ do
        ( mrgLift
            ( unionIf (SSBool "a") (single (SSBool "b")) (single (SSBool "c")) ::
                UnionMBase SBool SBool
            ) ::
            ExceptT SBool (UnionMBase SBool) SBool
          )
          @=? mrgSingle (ITE (SSBool "a") (SSBool "b") (SSBool "c"))
    ]
