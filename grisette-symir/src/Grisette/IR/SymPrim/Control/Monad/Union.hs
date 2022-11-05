{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Grisette.IR.SymPrim.Control.Monad.Union
  ( MonadUnion,
  )
where

import Grisette.Core.Control.Monad.Union
import Grisette.IR.SymPrim.Data.SymPrim

type MonadUnion u = GMonadUnion SymBool u
