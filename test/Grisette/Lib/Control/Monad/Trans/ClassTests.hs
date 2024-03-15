{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Control.Monad.Trans.ClassTests
  ( monadTransClassTests,
  )
where

import Control.Monad.Except (ExceptT)
import Grisette.Core.Control.Monad.UnionM (UnionM)
import Grisette.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Core.Data.Class.SimpleMergeable
  ( UnionMergeable1 (mrgIfPropagatedStrategy),
  )
import Grisette.Core.Data.Class.TryMerge
  ( mrgSingle,
  )
import Grisette.IR.SymPrim.Data.SymPrim (SymBool)
import Grisette.Lib.Control.Monad.Trans (mrgLift)
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
                UnionM SymBool
            ) ::
            ExceptT SymBool UnionM SymBool
          )
          @?= mrgSingle (symIte "a" "b" "c")
    ]
