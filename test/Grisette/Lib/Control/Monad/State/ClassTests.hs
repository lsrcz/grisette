module Grisette.Lib.Control.Monad.State.ClassTests
  ( monadStateClassTests,
  )
where

import Control.Monad.Trans.State.Lazy (StateT (StateT), runStateT)
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
      testCase "mrgGet" $ mrgGetTest StateT runStateT mrgGet,
      testCase "mrgPut" $ mrgPutTest StateT runStateT mrgPut,
      testCase "mrgModify" $ mrgModifyTest StateT runStateT mrgModify,
      testCase "mrgModify'" $ mrgModifyTest StateT runStateT mrgModify',
      testCase "mrgGets" $ mrgGetsTest StateT runStateT mrgGets
    ]
