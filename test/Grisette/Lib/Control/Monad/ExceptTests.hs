{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Control.Monad.ExceptTests (monadExceptFunctionTests) where

import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Grisette.Core.Control.Monad.UnionM (UnionM)
import Grisette.Core.Data.Class.Bool (ITEOp (ites))
import Grisette.Core.Data.Class.SimpleMergeable
  ( UnionLike (unionIf),
    mrgSingle,
  )
import Grisette.IR.SymPrim.Data.SymPrim (SymBool)
import Grisette.Lib.Control.Monad.Except
  ( mrgCatchError,
    mrgThrowError,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

monadExceptFunctionTests :: Test
monadExceptFunctionTests =
  testGroup
    "Except"
    [ testCase "mrgThrowError" $
        runExceptT (mrgThrowError 1 :: ExceptT Integer UnionM ())
          @?= mrgSingle (Left 1),
      testCase "mrgCatchError" $
        ( ExceptT (unionIf "a" (return $ Left "b") (return $ Right "c")) ::
            ExceptT SymBool UnionM SymBool
        )
          `mrgCatchError` return
          @?= mrgSingle (ites "a" "b" "c")
    ]
