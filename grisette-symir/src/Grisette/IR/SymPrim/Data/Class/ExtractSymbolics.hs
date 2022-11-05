{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Grisette.IR.SymPrim.Data.Class.ExtractSymbolics
  ( ExtractSymbolics,
    extractSymbolics,
  )
where

import Grisette.Core.Data.Class.ExtractSymbolics
import Grisette.IR.SymPrim.Data.Prim.Model

type ExtractSymbolics a = GExtractSymbolics SymbolSet a

extractSymbolics :: (ExtractSymbolics a) => a -> SymbolSet
extractSymbolics = gextractSymbolics
{-# INLINE extractSymbolics #-}
