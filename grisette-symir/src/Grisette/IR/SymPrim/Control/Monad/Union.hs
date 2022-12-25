{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Control.Monad.Union
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Control.Monad.Union
  ( MonadUnion,
  )
where

import Grisette.Core.Control.Monad.Union
import Grisette.IR.SymPrim.Data.SymPrim

-- | 'GMonadUnion' specialized with 'SymBool'
type MonadUnion u = GMonadUnion SymBool u
