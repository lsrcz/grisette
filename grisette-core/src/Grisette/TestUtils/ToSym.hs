module Grisette.TestUtils.ToSym where

import GHC.Stack
import Grisette.Core.Data.Class.ToSym
import Grisette.TestUtils.Assertions

toSymForConcreteOkProp :: (HasCallStack, ToSym v v, Show v, Eq v) => v -> Assertion
toSymForConcreteOkProp v = toSym v @=? v
