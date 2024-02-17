{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Data.FunctorTests (functorFunctionTests) where

import Grisette.Core.Control.Monad.UnionM (UnionM, mergePropagatedIf)
import Grisette.Core.Data.Class.TryMerge (tryMerge)
import Grisette.Lib.Data.Functor (mrgFmap)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

functorFunctionTests :: Test
functorFunctionTests =
  testGroup
    "Functor"
    [ testCase "mrgFmap" $ do
        mrgFmap
          (\x -> x * x)
          (mergePropagatedIf "a" (return $ -1) (return 1) :: UnionM Integer)
          @?= tryMerge (return 1)
    ]
