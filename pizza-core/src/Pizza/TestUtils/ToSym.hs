module Pizza.TestUtils.ToSym where

import Pizza.Core.Data.Class.ToSym
import Test.Hspec

toSymForConcreteOkProp :: (HasCallStack, ToSym v v, Show v, Eq v) => v -> Expectation
toSymForConcreteOkProp v = toSym v `shouldBe` v
