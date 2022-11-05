{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Grisette.IR.SymPrim.Data.Class.Evaluate
  ( EvaluateSym,
    evaluateSym,
    evaluateSymToCon,
  )
where

import Grisette.Core.Data.Class.Evaluate
import Grisette.Core.Data.Class.ToCon
import Grisette.IR.SymPrim.Data.Prim.Model

type EvaluateSym a = GEvaluateSym Model a

evaluateSym :: (EvaluateSym a) => Bool -> Model -> a -> a
evaluateSym = gevaluateSym

evaluateSymToCon :: (EvaluateSym a, ToCon a b) => Model -> a -> b
evaluateSymToCon = gevaluateSymToCon
