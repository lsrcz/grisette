{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Grisette.Core.BuiltinUnionWrappers
  ( mrgTrue,
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
