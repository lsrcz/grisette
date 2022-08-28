module Pizza.TestUtils.ToSym where

import GHC.Stack
import Pizza.Core.Data.Class.ToSym
import Pizza.TestUtils.Assertions

toSymForConcreteOkProp :: (HasCallStack, ToSym v v, Show v, Eq v) => v -> Assertion
toSymForConcreteOkProp v = toSym v @=? v
