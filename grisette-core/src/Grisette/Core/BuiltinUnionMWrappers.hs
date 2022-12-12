{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.Core.BuiltinUnionMWrapper
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.BuiltinUnionMWrappers
  ( -- * Builtin constructor wrappers for some common data types
    uTrue,
    uFalse,
    uunit,
    uTuple2,
    uTuple3,
    uJust,
    uNothing,
    uLeft,
    uRight,
    uInL,
    uInR,
    uAssertionViolation,
    uAssumptionViolation,
  )
where

import Data.Functor.Sum
import Grisette.Core.Control.Exception
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Core.TH

$(makeUnionMWrapper "u" ''Bool)
$(makeUnionMWrapper' ["uunit"] ''())
$(makeUnionMWrapper' ["uTuple2"] ''(,))
$(makeUnionMWrapper' ["uTuple3"] ''(,,))
$(makeUnionMWrapper "u" ''Maybe)
$(makeUnionMWrapper "u" ''Either)
$(makeUnionMWrapper "u" ''Sum)
$(makeUnionMWrapper "u" ''VerificationConditions)
