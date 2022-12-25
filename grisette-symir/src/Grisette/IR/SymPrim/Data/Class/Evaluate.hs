{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.Class.Evaluate
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.Class.Evaluate
  ( EvaluateSym,
    evaluateSym,
    evaluateSymToCon,
  )
where

import Grisette.Core.Data.Class.Evaluate
import Grisette.Core.Data.Class.ToCon
import Grisette.IR.SymPrim.Data.Prim.Model

-- | 'GEvaluateSym' specialized with 'Model'
type EvaluateSym a = GEvaluateSym Model a

-- | 'gevaluateSym' specialized with 'Model'
evaluateSym :: (EvaluateSym a) => Bool -> Model -> a -> a
evaluateSym = gevaluateSym
{-# INLINE evaluateSym #-}

-- | 'gevaluateSymToCon' specialized with 'Model'
evaluateSymToCon :: (EvaluateSym a, ToCon a b) => Model -> a -> b
evaluateSymToCon = gevaluateSymToCon
{-# INLINE evaluateSymToCon #-}
