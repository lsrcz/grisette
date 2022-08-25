module Pizza.IR.SymPrim.Data.Union
  ( Union,
  )
where

import Pizza.Core.Data.UnionBase
import Pizza.IR.SymPrim.Data.SymPrim

type Union = UnionBase SymBool
