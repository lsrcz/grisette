{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Control.Monad.Trans.ClassTests
  ( monadTransClassTests,
  )
where

import Control.Monad.Except (ExceptT)
import Grisette
  ( AsKey,
    AsKey1,
    ITEOp (symIte),
    SymBranching (mrgIfPropagatedStrategy),
    Union,
    mrgSingle,
  )
import Grisette.Lib.Control.Monad.Trans (mrgLift)
import Grisette.SymPrim (SymBool)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

monadTransClassTests :: Test
monadTransClassTests =
  testGroup
    "Class"
    [ testCase "mrgLift" $ do
        ( mrgLift
            ( mrgIfPropagatedStrategy "a" (return "b") (return "c") ::
                AsKey1 Union (AsKey SymBool)
            ) ::
            ExceptT (AsKey SymBool) (AsKey1 Union) (AsKey SymBool)
          )
          @?= mrgSingle (symIte "a" "b" "c")
    ]
