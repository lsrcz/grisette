{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Grisette.Core.BuiltinUnionMWrappers
  ( uTrue,
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
