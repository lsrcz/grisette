module Grisette.Lib.Control.Monad.State.ClassTests
  ( monadStateClassTests,
  )
where

import Control.Monad.State.Lazy (state)
import Control.Monad.Trans.State.Lazy (runStateT)
import Grisette.Lib.Control.Monad.State.Class
  ( mrgGet,
    mrgGets,
    mrgModify,
    mrgModify',
    mrgPut,
    mrgState,
  )
import Grisette.Lib.Control.Monad.Trans.State.Common
  ( mrgGetTest,
    mrgGetsTest,
    mrgModifyTest,
    mrgPutTest,
    mrgStateTest,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

monadStateClassTests :: Test
monadStateClassTests =
  testGroup
    "Class"
    [ testCase "mrgState" $ mrgStateTest mrgState runStateT,
      testCase "mrgGet" $ mrgGetTest state runStateT mrgGet,
      testCase "mrgPut" $ mrgPutTest state runStateT mrgPut,
      testCase "mrgModify" $ mrgModifyTest state runStateT mrgModify,
      testCase "mrgModify'" $ mrgModifyTest state runStateT mrgModify',
      testCase "mrgGets" $ mrgGetsTest state runStateT mrgGets
    ]
