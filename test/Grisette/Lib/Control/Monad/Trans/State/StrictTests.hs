module Grisette.Lib.Control.Monad.Trans.State.StrictTests
  ( monadTransStateStrictTests,
  )
where

import Control.Monad.Trans.State.Strict (StateT (StateT), runStateT)
import Grisette.Lib.Control.Monad.Trans.State.Common
  ( mrgEvalStateTTest,
    mrgExecStateTTest,
    mrgGetTest,
    mrgGetsTest,
    mrgMapStateTTest,
    mrgModifyTest,
    mrgPutTest,
    mrgRunStateTTest,
    mrgStateTest,
    mrgWithStateTTest,
  )
import Grisette.Lib.Control.Monad.Trans.State.Strict
  ( mrgEvalStateT,
    mrgExecStateT,
    mrgGet,
    mrgGets,
    mrgMapStateT,
    mrgModify,
    mrgModify',
    mrgPut,
    mrgRunStateT,
    mrgState,
    mrgWithStateT,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

monadTransStateStrictTests :: Test
monadTransStateStrictTests =
  testGroup
    "Strict"
    [ testCase "mrgState" $ mrgStateTest mrgState runStateT,
      testCase "mrgRunStateT" $ mrgRunStateTTest StateT mrgRunStateT,
      testCase "mrgEvalStateT" $ mrgEvalStateTTest StateT mrgEvalStateT,
      testCase "mrgExecStateT" $ mrgExecStateTTest StateT mrgExecStateT,
      testCase "mrgMapStateT" $ mrgMapStateTTest StateT runStateT mrgMapStateT,
      testCase "mrgWithStateT" $
        mrgWithStateTTest StateT runStateT mrgWithStateT,
      testCase "mrgGet" $ mrgGetTest StateT runStateT mrgGet,
      testCase "mrgPut" $ mrgPutTest StateT runStateT mrgPut,
      testCase "mrgModify" $ mrgModifyTest StateT runStateT mrgModify,
      testCase "mrgModify'" $ mrgModifyTest StateT runStateT mrgModify',
      testCase "mrgGets" $ mrgGetsTest StateT runStateT mrgGets
    ]
