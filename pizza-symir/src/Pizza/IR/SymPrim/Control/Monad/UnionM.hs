module Pizza.IR.SymPrim.Control.Monad.UnionM
  ( UnionM,
  )
where

import Pizza.Core.Control.Monad.UnionMBase
import Pizza.IR.SymPrim.Data.SymPrim

type UnionM = UnionMBase SymBool
