-- |
-- Module      :   Grisette.IR.SymPrim.Control.Monad.UnionM
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Control.Monad.UnionM
  ( UnionM,
  )
where

import Grisette.Core.Control.Monad.UnionMBase
import Grisette.IR.SymPrim.Data.SymPrim

-- | 'GUnionMBase' specialized with 'SymBool'
type UnionM = UnionMBase SymBool
