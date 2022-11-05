{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Grisette.IR.SymPrim.Data.Class.SymIntegerOp
  ( SymIntegerOp,
  )
where

import Grisette.Core.Data.Class.Integer
import Grisette.IR.SymPrim.Data.SymPrim

type SymIntegerOp a = GSymIntegerOp SymBool a
