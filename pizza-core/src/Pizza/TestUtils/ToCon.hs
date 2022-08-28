module Pizza.TestUtils.ToCon where

import GHC.Stack
import Pizza.Core.Data.Class.ToCon
import Pizza.TestUtils.Assertions

toConForConcreteOkProp :: (HasCallStack, ToCon v v, Show v, Eq v) => v -> Assertion
toConForConcreteOkProp v = toCon v @=? Just v
