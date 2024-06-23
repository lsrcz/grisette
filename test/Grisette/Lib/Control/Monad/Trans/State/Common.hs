{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Control.Monad.Trans.State.Common
  ( mrgStateTest,
    mrgRunStateTTest,
    mrgEvalStateTTest,
    mrgExecStateTTest,
    mrgMapStateTTest,
    mrgWithStateTTest,
    mrgGetTest,
    mrgPutTest,
    mrgModifyTest,
    mrgGetsTest,
  )
where

import Grisette
  ( LogicalOp ((.&&)),
    MonadUnion,
    SimpleMergeable (mrgIte),
    SymBool,
    SymBranching (mrgIfPropagatedStrategy),
    UnionM,
    mrgSingle,
    unionSize,
  )
import Grisette.Core.Data.Class.TestValues
  ( ssymBool,
  )
import Grisette.TestUtil.SymbolicAssertion ((@?=~))
import Test.HUnit (Assertion, (@?=))

type StateConstructor stateT s a = (s -> UnionM (a, s)) -> stateT s UnionM a

type StateFunc stateT s a = (s -> (a, s)) -> stateT s UnionM a

type RunStateFunc stateT s a = stateT s UnionM a -> s -> UnionM (a, s)

type EvalStateFunc stateT s a = stateT s UnionM a -> s -> UnionM a

type ExecStateFunc stateT s a = stateT s UnionM a -> s -> UnionM s

type MapStateFunc stateT s a =
  (UnionM (a, s) -> UnionM (a, s)) ->
  stateT s UnionM a ->
  stateT s UnionM a

type WithStateFunc stateT s a =
  (s -> s) ->
  stateT s UnionM a ->
  stateT s UnionM a

type GetFunc stateT s a = stateT s UnionM s

type PutFunc stateT s a = s -> stateT s UnionM ()

type ModifyFunc stateT s a = (s -> s) -> stateT s UnionM ()

type GetsFunc stateT s a = (s -> a) -> stateT s UnionM a

bodyA :: SymBool -> UnionM (SymBool, SymBool)
bodyA s = return (s .&& ssymBool "av", s .&& ssymBool "as")

stateA ::
  StateConstructor stateT SymBool SymBool -> stateT SymBool UnionM SymBool
stateA state = state bodyA

bodyB :: SymBool -> UnionM (SymBool, SymBool)
bodyB s = return (s .&& ssymBool "bv", s .&& ssymBool "bs")

stateB ::
  StateConstructor stateT SymBool SymBool -> stateT SymBool UnionM SymBool
stateB state = state bodyB

stateAB ::
  (SymBranching (stateT SymBool UnionM)) =>
  StateConstructor stateT SymBool SymBool ->
  stateT SymBool UnionM SymBool
stateAB state = mrgIfPropagatedStrategy (ssymBool "c") (state bodyA) (state bodyB)

mrgStateTest ::
  (MonadUnion (stateT SymBool UnionM)) =>
  StateFunc stateT SymBool SymBool ->
  RunStateFunc stateT SymBool SymBool ->
  Assertion
mrgStateTest mrgState runStateT = do
  let a =
        mrgState (\s -> (s .&& ssymBool "av", s .&& ssymBool "as"))
  let b =
        mrgState (\s -> (s .&& ssymBool "bv", s .&& ssymBool "bs"))
  let actual =
        runStateT (mrgIfPropagatedStrategy (ssymBool "c") a b) (ssymBool "d")
  let expected =
        mrgSingle
          ( mrgIte
              (ssymBool "c")
              ( ssymBool "d" .&& ssymBool "av",
                ssymBool "d" .&& ssymBool "as"
              )
              ( ssymBool "d" .&& ssymBool "bv",
                ssymBool "d" .&& ssymBool "bs"
              )
          )
  unionSize actual @?= 1
  actual @?=~ expected

mrgRunStateTTest ::
  (MonadUnion (stateT SymBool UnionM)) =>
  StateConstructor stateT SymBool SymBool ->
  RunStateFunc stateT SymBool SymBool ->
  Assertion
mrgRunStateTTest state mrgRunStateT = do
  let actual = mrgRunStateT (stateAB state) (ssymBool "d")
  let expected =
        mrgSingle
          ( mrgIte
              (ssymBool "c")
              ( ssymBool "d" .&& ssymBool "av",
                ssymBool "d" .&& ssymBool "as"
              )
              ( ssymBool "d" .&& ssymBool "bv",
                ssymBool "d" .&& ssymBool "bs"
              )
          )
  unionSize actual @?= 1
  actual @?=~ expected

mrgEvalStateTTest ::
  (MonadUnion (stateT SymBool UnionM)) =>
  StateConstructor stateT SymBool SymBool ->
  EvalStateFunc stateT SymBool SymBool ->
  Assertion
mrgEvalStateTTest state mrgEvalStateT = do
  let actual = mrgEvalStateT (stateAB state) (ssymBool "d")
  let expected =
        mrgSingle
          ( mrgIte
              (ssymBool "c")
              (ssymBool "d" .&& ssymBool "av")
              (ssymBool "d" .&& ssymBool "bv")
          )
  unionSize actual @?= 1
  actual @?=~ expected

mrgExecStateTTest ::
  (MonadUnion (stateT SymBool UnionM)) =>
  StateConstructor stateT SymBool SymBool ->
  ExecStateFunc stateT SymBool SymBool ->
  Assertion
mrgExecStateTTest state mrgExecStateT = do
  let actual = mrgExecStateT (stateAB state) (ssymBool "d")
  let expected =
        mrgSingle
          ( mrgIte
              (ssymBool "c")
              (ssymBool "d" .&& ssymBool "as")
              (ssymBool "d" .&& ssymBool "bs")
          )
  unionSize actual @?= 1
  actual @?=~ expected

mrgMapStateTTest ::
  (MonadUnion (stateT SymBool UnionM)) =>
  StateConstructor stateT SymBool SymBool ->
  RunStateFunc stateT SymBool SymBool ->
  MapStateFunc stateT SymBool SymBool ->
  Assertion
mrgMapStateTTest state runStateT mrgMapStateT = do
  let a = mrgMapStateT id (stateA state)
  let b = mrgMapStateT id (stateB state)
  let actual = runStateT (mrgIfPropagatedStrategy (ssymBool "c") a b) (ssymBool "d")
  let expected =
        mrgSingle
          ( mrgIte
              (ssymBool "c")
              ( ssymBool "d" .&& ssymBool "av",
                ssymBool "d" .&& ssymBool "as"
              )
              ( ssymBool "d" .&& ssymBool "bv",
                ssymBool "d" .&& ssymBool "bs"
              )
          )
  unionSize actual @?= 1
  actual @?=~ expected

mrgWithStateTTest ::
  (MonadUnion (stateT SymBool UnionM)) =>
  StateConstructor stateT SymBool SymBool ->
  RunStateFunc stateT SymBool SymBool ->
  WithStateFunc stateT SymBool SymBool ->
  Assertion
mrgWithStateTTest state runStateT mrgWithStateT = do
  let a = mrgWithStateT (.&& ssymBool "x") (stateA state)
  let b = mrgWithStateT (.&& ssymBool "y") (stateB state)
  let actual = runStateT (mrgIfPropagatedStrategy (ssymBool "c") a b) (ssymBool "d")
  let expected =
        mrgSingle
          ( mrgIte
              (ssymBool "c")
              ( ssymBool "d" .&& ssymBool "av" .&& ssymBool "x",
                ssymBool "d" .&& ssymBool "as" .&& ssymBool "x"
              )
              ( ssymBool "d" .&& ssymBool "bv" .&& ssymBool "y",
                ssymBool "d" .&& ssymBool "bs" .&& ssymBool "y"
              )
          )
  unionSize actual @?= 1
  actual @?=~ expected

mrgGetTest ::
  (MonadUnion (stateT SymBool UnionM), Monad (stateT SymBool UnionM)) =>
  StateConstructor stateT SymBool SymBool ->
  RunStateFunc stateT SymBool SymBool ->
  GetFunc stateT SymBool SymBool ->
  Assertion
mrgGetTest state runStateT mrgGet = do
  let a = do stateA state; mrgGet
  let b = do stateB state; mrgGet
  let actual = runStateT (mrgIfPropagatedStrategy (ssymBool "c") a b) (ssymBool "d")
  let expected =
        mrgSingle
          ( mrgIte
              (ssymBool "c")
              ( ssymBool "d" .&& ssymBool "as",
                ssymBool "d" .&& ssymBool "as"
              )
              ( ssymBool "d" .&& ssymBool "bs",
                ssymBool "d" .&& ssymBool "bs"
              )
          )
  unionSize actual @?= 1
  actual @?=~ expected

mrgPutTest ::
  (MonadUnion (stateT SymBool UnionM), Monad (stateT SymBool UnionM)) =>
  StateConstructor stateT SymBool SymBool ->
  RunStateFunc stateT SymBool () ->
  PutFunc stateT SymBool SymBool ->
  Assertion
mrgPutTest state runStateT mrgPut = do
  let a = do stateA state; mrgPut (ssymBool "x")
  let b = do stateB state; mrgPut (ssymBool "y")
  let actual = runStateT (mrgIfPropagatedStrategy (ssymBool "c") a b) (ssymBool "d")
  let expected =
        mrgSingle
          ( mrgIte (ssymBool "c") ((), ssymBool "x") ((), ssymBool "y")
          )
  unionSize actual @?= 1
  actual @?=~ expected

mrgModifyTest ::
  (MonadUnion (stateT SymBool UnionM), Monad (stateT SymBool UnionM)) =>
  StateConstructor stateT SymBool SymBool ->
  RunStateFunc stateT SymBool () ->
  ModifyFunc stateT SymBool SymBool ->
  Assertion
mrgModifyTest state runStateT mrgModify = do
  let a = do stateA state; mrgModify (.&& ssymBool "x")
  let b = do stateB state; mrgModify (.&& ssymBool "y")
  let actual = runStateT (mrgIfPropagatedStrategy (ssymBool "c") a b) (ssymBool "d")
  let expected =
        mrgSingle
          ( mrgIte
              (ssymBool "c")
              ( (),
                ssymBool "d" .&& ssymBool "as" .&& ssymBool "x"
              )
              ( (),
                ssymBool "d" .&& ssymBool "bs" .&& ssymBool "y"
              )
          )
  unionSize actual @?= 1
  actual @?=~ expected

mrgGetsTest ::
  (MonadUnion (stateT SymBool UnionM), Monad (stateT SymBool UnionM)) =>
  StateConstructor stateT SymBool SymBool ->
  RunStateFunc stateT SymBool SymBool ->
  GetsFunc stateT SymBool SymBool ->
  Assertion
mrgGetsTest state runStateT mrgGets = do
  let a = do stateA state; mrgGets (.&& ssymBool "x")
  let b = do stateB state; mrgGets (.&& ssymBool "y")
  let actual =
        runStateT (mrgIfPropagatedStrategy (ssymBool "c") a b) (ssymBool "d")
  let expected =
        mrgSingle
          ( mrgIte
              (ssymBool "c")
              ( ssymBool "d" .&& ssymBool "as" .&& ssymBool "x",
                ssymBool "d" .&& ssymBool "as"
              )
              ( ssymBool "d" .&& ssymBool "bs" .&& ssymBool "y",
                ssymBool "d" .&& ssymBool "bs"
              )
          )
  unionSize actual @?= 1
  actual @?=~ expected
