{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Control.Monad.TransTests (monadTransFunctionTests) where

import Control.Monad.Except (ExceptT)
import Grisette.Core.Control.Monad.UnionM (UnionM)
import Grisette.Core.Data.Class.Bool (ITEOp (ites))
import Grisette.Core.Data.Class.SimpleMergeable
  ( UnionLike (single, unionIf),
    mrgSingle,
  )
import Grisette.IR.SymPrim.Data.SymPrim (SymBool)
import Grisette.Lib.Control.Monad.Trans (mrgLift)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

monadTransFunctionTests :: Test
monadTransFunctionTests =
  testGroup
    "Trans"
    [ testCase "mrgLift" $ do
        ( mrgLift
            ( unionIf "a" (single "b") (single "c") ::
                UnionM SymBool
            ) ::
            ExceptT SymBool UnionM SymBool
          )
          @?= mrgSingle (ites "a" "b" "c")
    ]
