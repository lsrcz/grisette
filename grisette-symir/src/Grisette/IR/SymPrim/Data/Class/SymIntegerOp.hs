{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.Class.SymIntegerOp
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.Class.SymIntegerOp
  ( SymIntegerOp,
  )
where

import Grisette.Core.Data.Class.Integer
import Grisette.IR.SymPrim.Data.SymPrim

-- | 'GSymIntegerOp' specialized with 'SymBool'
type SymIntegerOp a = GSymIntegerOp SymBool a
