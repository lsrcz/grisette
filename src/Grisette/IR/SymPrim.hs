{-# LANGUAGE ExplicitNamespaces #-}
-- Disable this warning because we are re-exporting things.
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- |
-- Module      :   Grisette.IR.SymPrim
-- Copyright   :   (c) Sirui Lu 2021-2023
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
    SomeWordN (..),
    SomeIntN (..),
    type (=->) (..),
    type (-->),
    (-->),

    -- ** Symbolic types
    SupportedPrim,
    SymRep (SymType),
    ConRep (ConType),
    LinkedRep,
    SymBool (..),
    SymInteger (..),
    SymWordN (..),
    SymIntN (..),
    SomeSymWordN (..),
    SomeSymIntN (..),
    type (=~>) (..),
    type (-~>) (..),
    TypedSymbol (..),
    symSize,
    symsSize,
    AllSyms (..),
    allSymsSize,

    -- ** Symbolic constant sets and models
    SymbolSet (..),
    Model (..),
    ModelValuePair (..),
    ModelSymPair (..),
  )
where

import Grisette.Core.Data.BV
  ( IntN,
    SomeIntN (..),
    SomeWordN (..),
    WordN,
  )
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
  ( ConRep (..),
    LinkedRep,
    SupportedPrim,
    SymRep (..),
    TypedSymbol (..),
    type (-->),
  )
import Grisette.IR.SymPrim.Data.Prim.Model
  ( Model (..),
    ModelValuePair (..),
    SymbolSet (..),
  )
import Grisette.IR.SymPrim.Data.SymPrim
  ( AllSyms (..),
    ModelSymPair (..),
    SomeSymIntN (..),
    SomeSymWordN (..),
    SymBool (..),
    SymIntN (..),
    SymInteger (..),
    SymWordN (..),
    allSymsSize,
    symSize,
    symsSize,
    (-->),
    type (-~>) (..),
    type (=~>) (..),
  )
import Grisette.IR.SymPrim.Data.TabularFun (type (=->) (..))
