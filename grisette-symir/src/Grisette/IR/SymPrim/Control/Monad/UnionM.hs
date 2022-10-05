module Grisette.IR.SymPrim.Control.Monad.UnionM
  ( UnionM,
  )
where

import Grisette.Core.Control.Monad.UnionMBase
import Grisette.IR.SymPrim.Data.SymPrim

type UnionM = UnionMBase SymBool
