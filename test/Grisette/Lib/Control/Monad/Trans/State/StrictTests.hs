module Grisette.Lib.Control.Monad.Trans.State.StrictTests
  ( monadTransStateStrictTests,
  )
where

import Control.Monad.State.Strict (state)
import Control.Monad.Trans.State.Strict (runStateT)
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
      testCase "mrgRunStateT" $ mrgRunStateTTest state mrgRunStateT,
      testCase "mrgEvalStateT" $ mrgEvalStateTTest state mrgEvalStateT,
      testCase "mrgExecStateT" $ mrgExecStateTTest state mrgExecStateT,
      testCase "mrgMapStateT" $ mrgMapStateTTest state runStateT mrgMapStateT,
      testCase "mrgWithStateT" $
        mrgWithStateTTest state runStateT mrgWithStateT,
      testCase "mrgGet" $ mrgGetTest state runStateT mrgGet,
      testCase "mrgPut" $ mrgPutTest state runStateT mrgPut,
      testCase "mrgModify" $ mrgModifyTest state runStateT mrgModify,
      testCase "mrgModify'" $ mrgModifyTest state runStateT mrgModify',
      testCase "mrgGets" $ mrgGetsTest state runStateT mrgGets
    ]
