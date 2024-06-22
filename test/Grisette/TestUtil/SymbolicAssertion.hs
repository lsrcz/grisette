module Grisette.TestUtil.SymbolicAssertion ((@?=~), (.@?=), symShouldEq) where

import GHC.Stack (HasCallStack)
import Grisette
  ( EvaluateSym (evaluateSym),
    LogicalOp (symNot),
    Model,
    SEq ((./=), (.==)),
    SolvingFailure (Unsat),
    precise,
    solve,
    z3,
  )
import Test.HUnit (Assertion)

(@?=~) :: (HasCallStack, SEq a, Show a, EvaluateSym a) => a -> a -> Assertion
actual @?=~ expected = do
  cex <- solve (precise z3) (symNot $ actual .== expected)
  case cex of
    Left Unsat -> return ()
    Left err -> error $ "Solver isn't working: " ++ show err
    Right model ->
      error $
        unlines
          [ "Symbolic assertion failed:",
            "  Counterexample model: " ++ show model,
            "  Expected value under the model: "
              ++ show (evaluateSym True model expected),
            "  Actual value under the model: "
              ++ show (evaluateSym True model actual),
            "  Expected value: " ++ show expected,
            "  Actual value: " ++ show actual
          ]

infix 1 .@?=
(.@?=) :: (HasCallStack, Show a, SEq a, EvaluateSym a) => a -> a -> IO ()
(.@?=) actual expected =
  symShouldEq
    actual
    expected
    ( \m ->
        "Can be not equal, model: "
          <> show m
          <> ". Actual value: "
          <> show (evaluateSym False m actual)
          <> ". Expected value: "
          <> show (evaluateSym False m expected)
    )

symShouldEq ::
  (HasCallStack, Show a, SEq a, EvaluateSym a) =>
  a ->
  a ->
  (Model -> String) ->
  IO ()
symShouldEq actual expected notEqualCaseMessage = do
  canBeNotEqual <- solve (precise z3) $ actual ./= expected
  canBeEqual <- solve (precise z3) $ actual .== expected
  case (canBeNotEqual, canBeEqual) of
    (Left _, Right _) -> return ()
    (Right m, _) -> fail $ notEqualCaseMessage m
    (_, Left _) -> fail "Cannot be equal"
