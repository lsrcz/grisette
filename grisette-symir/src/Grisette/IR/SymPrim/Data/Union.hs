module Grisette.IR.SymPrim.Data.Union
  ( Union,
  )
where

import Grisette.Core.Data.UnionBase
import Grisette.IR.SymPrim.Data.SymPrim

type Union = UnionBase SymBool
