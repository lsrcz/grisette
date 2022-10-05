module Grisette.TestUtils.ToCon where

import GHC.Stack
import Grisette.Core.Data.Class.ToCon
import Grisette.TestUtils.Assertions

toConForConcreteOkProp :: (HasCallStack, ToCon v v, Show v, Eq v) => v -> Assertion
toConForConcreteOkProp v = toCon v @=? Just v
