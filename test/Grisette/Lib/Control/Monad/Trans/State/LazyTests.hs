module Grisette.Lib.Control.Monad.Trans.State.LazyTests
  ( monadTransStateLazyTests,
  )
where

import Control.Monad.Trans.State.Lazy (StateT (StateT), runStateT)
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
import Grisette.Lib.Control.Monad.Trans.State.Lazy
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

monadTransStateLazyTests :: Test
monadTransStateLazyTests =
  testGroup
    "Lazy"
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
