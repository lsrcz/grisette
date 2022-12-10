{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.TestUtils.ToSym
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only

module Grisette.TestUtils.ToSym where

import GHC.Stack
import Grisette.Core.Data.Class.ToSym
import Grisette.TestUtils.Assertions

toSymForConcreteOkProp :: (HasCallStack, ToSym v v, Show v, Eq v) => v -> Assertion
toSymForConcreteOkProp v = toSym v @=? v
