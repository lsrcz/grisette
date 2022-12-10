{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.TestUtils.SEq
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only

module Grisette.TestUtils.SEq where

import GHC.Stack
import Grisette.Core.Data.Class.Bool
import Grisette.TestUtils.Assertions
import Grisette.TestUtils.SBool

concreteSEqOkProp :: (HasCallStack, GSEq SBool a, Eq a) => (a, a) -> Assertion
concreteSEqOkProp (i, j) = do
  i `gsymeq` j @=? CBool (i == j)
  i `gsymne` j @=? CBool (i /= j)
