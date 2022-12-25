-- |
-- Module      :   Grisette.IR.SymPrim.Data.Union
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.Union
  ( Union,
  )
where

import Grisette.Core.Data.UnionBase
import Grisette.IR.SymPrim.Data.SymPrim

-- | 'UnionBase' specialized with 'SymBool'
type Union = UnionBase SymBool
