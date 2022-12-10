{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.TestUtils.ToCon
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only

module Grisette.TestUtils.ToCon where

import GHC.Stack
import Grisette.Core.Data.Class.ToCon
import Grisette.TestUtils.Assertions

toConForConcreteOkProp :: (HasCallStack, ToCon v v, Show v, Eq v) => v -> Assertion
toConForConcreteOkProp v = toCon v @=? Just v
