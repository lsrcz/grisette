module Pizza.TestUtils.ToCon where

import Pizza.Core.Data.Class.ToCon
import Test.Tasty.HUnit

toConForConcreteOkProp :: (HasCallStack, ToCon v v, Show v, Eq v) => v -> Assertion
toConForConcreteOkProp v = toCon v @=? Just v
