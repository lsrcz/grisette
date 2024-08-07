module Grisette.TestUtil.SymbolicAssertion ((@?=~), (.@?=), symShouldEq) where

import GHC.Stack (HasCallStack)
import Grisette
  ( EvalSym (evalSym),
    LogicalOp (symNot),
    Model,
    SolvingFailure (Unsat),
    SymEq ((./=), (.==)),
    solve,
    z3,
  )
import Test.HUnit (Assertion)

(@?=~) :: (HasCallStack, SymEq a, Show a, EvalSym a) => a -> a -> Assertion
actual @?=~ expected = do
  cex <- solve z3 (symNot $ actual .== expected)
  case cex of
    Left Unsat -> return ()
    Left err -> error $ "Solver isn't working: " ++ show err
    Right model ->
      error $
        unlines
          [ "Symbolic assertion failed:",
            "  Counterexample model: " ++ show model,
            "  Expected value under the model: "
              ++ show (evalSym True model expected),
            "  Actual value under the model: "
              ++ show (evalSym True model actual),
            "  Expected value: " ++ show expected,
            "  Actual value: " ++ show actual
          ]

infix 1 .@?=

(.@?=) :: (HasCallStack, Show a, SymEq a, EvalSym a) => a -> a -> IO ()
(.@?=) actual expected =
  symShouldEq
    actual
    expected
    ( \m ->
        "Can be not equal, model: "
          <> show m
          <> ". Actual value: "
          <> show (evalSym False m actual)
          <> ". Expected value: "
          <> show (evalSym False m expected)
    )

symShouldEq ::
  (HasCallStack, SymEq a) =>
  a ->
  a ->
  (Model -> String) ->
  IO ()
symShouldEq actual expected notEqualCaseMessage = do
  canBeNotEqual <- solve z3 $ actual ./= expected
  canBeEqual <- solve z3 $ actual .== expected
  case (canBeNotEqual, canBeEqual) of
    (Left _, Right _) -> return ()
    (Right m, _) -> error $ notEqualCaseMessage m
    (_, Left _) -> error "Cannot be equal"
