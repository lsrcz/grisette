{-# LANGUAGE ExplicitNamespaces #-}

-- |
-- Module      :   Grisette.IR.SymPrim
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim
  ( -- * Symbolic type implementation

    -- ** Extended types
    IntN,
    WordN,
    type (=->) (..),
    type (-->),
    (-->),

    -- ** Symbolic types
    SupportedPrim,
    Sym,
    TypedSymbol (..),
    symSize,
    symsSize,

    -- ** Symbolic type synonyms
    SymBool,
    SymInteger,
    SymIntN,
    SymWordN,
    type (=~>),
    type (-~>),

    -- ** Symbolic constant sets and models
    SymbolSet (..),
    Model (..),
    ModelValuePair (..),
  )
where

import Grisette.IR.SymPrim.Data.BV
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Grisette.IR.SymPrim.Data.Prim.Model
import Grisette.IR.SymPrim.Data.SymPrim
import Grisette.IR.SymPrim.Data.TabularFun
