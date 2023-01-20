{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.Core.BuiltinUnionWrapper
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.BuiltinUnionWrappers
  ( -- * Builtin constructor wrappers for some common data types
    mrgTrue,
    mrgFalse,
    mrgUnit,
    mrgTuple2,
    mrgTuple3,
    mrgJust,
    mrgNothing,
    mrgLeft,
    mrgRight,
    mrgInL,
    mrgInR,
    mrgAssertionViolation,
    mrgAssumptionViolation,
  )
where

import Data.Functor.Sum
import Grisette.Core.Control.Exception
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Core.TH

$(makeUnionWrapper "mrg" ''Bool)
$(makeUnionWrapper' ["mrgUnit"] ''())
$(makeUnionWrapper' ["mrgTuple2"] ''(,))
$(makeUnionWrapper' ["mrgTuple3"] ''(,,))
$(makeUnionWrapper "mrg" ''Maybe)
$(makeUnionWrapper "mrg" ''Either)
$(makeUnionWrapper "mrg" ''Sum)
$(makeUnionWrapper "mrg" ''VerificationConditions)
