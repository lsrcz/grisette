module Pizza.TestUtils.ToSym where

import Pizza.Core.Data.Class.ToSym
import Test.Tasty.HUnit

toSymForConcreteOkProp :: (HasCallStack, ToSym v v, Show v, Eq v) => v -> Assertion
toSymForConcreteOkProp v = toSym v @=? v
