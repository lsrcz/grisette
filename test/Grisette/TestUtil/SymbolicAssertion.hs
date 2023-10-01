module Grisette.TestUtil.SymbolicAssertion ((@?=~)) where

import GHC.Stack (HasCallStack)
import Grisette.Backend.SBV (z3)
import Grisette.Backend.SBV.Data.SMT.Solving (SolvingFailure (Unsat), precise)
import Grisette.Core.Data.Class.Bool (LogicalOp (nots), SEq ((==~)))
import Grisette.Core.Data.Class.Evaluate (EvaluateSym (evaluateSym))
import Grisette.Core.Data.Class.Solver (Solver (solve))
import Test.HUnit (Assertion)

(@?=~) :: (HasCallStack, SEq a, Show a, EvaluateSym a) => a -> a -> Assertion
actual @?=~ expected = do
  cex <- solve (precise z3) (nots $ actual ==~ expected)
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
