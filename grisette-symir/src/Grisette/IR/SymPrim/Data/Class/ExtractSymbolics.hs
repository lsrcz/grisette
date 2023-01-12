{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.Class.ExtractSymbolics
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.Class.ExtractSymbolics
  ( ExtractSymbolics,
    extractSymbolics,
  )
where

import Grisette.Core.Data.Class.ExtractSymbolics
import Grisette.IR.SymPrim.Data.Prim.Model

-- | 'GExtractSymbolics' specialized with 'SymbolSet'
type ExtractSymbolics a = GExtractSymbolics SymbolSet a

-- | 'gextractSymbolics' specialized with 'SymbolSet'
extractSymbolics :: (ExtractSymbolics a) => a -> SymbolSet
extractSymbolics = gextractSymbolics
{-# INLINE extractSymbolics #-}
