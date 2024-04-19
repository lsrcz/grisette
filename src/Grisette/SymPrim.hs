{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}
-- Disable this warning because we are re-exporting things.
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- |
-- Module      :   Grisette.SymPrim
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.SymPrim
  ( -- * Symbolic type implementation

    -- ** Extended types
    IntN,
    WordN,
    SomeBV (..),
    pattern SomeIntN,
    type SomeIntN,
    pattern SomeWordN,
    type SomeWordN,
    type (=->) (..),
    type (-->),
    (-->),
    unsafeSomeBV,
    unarySomeBV,
    unarySomeBVR1,
    binSomeBV,
    binSomeBVR1,
    binSomeBVR2,
    binSomeBVSafe,
    binSomeBVSafeR1,
    binSomeBVSafeR2,
    conBV,
    conBVView,
    pattern ConBV,
    symBV,
    ssymBV,
    isymBV,
    arbitraryBV,

    -- ** Symbolic types
    SupportedPrim,
    SymRep (SymType),
    ConRep (ConType),
    LinkedRep,
    SymBool (..),
    SymInteger (..),
    SymWordN (..),
    SymIntN (..),
    SomeSymIntN,
    SomeSymWordN,
    pattern SomeSymIntN,
    pattern SomeSymWordN,
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
    WordN,
  )
import Grisette.Core.Data.SomeBV
  ( SomeBV (..),
    arbitraryBV,
    binSomeBV,
    binSomeBVR1,
    binSomeBVR2,
    binSomeBVSafe,
    binSomeBVSafeR1,
    binSomeBVSafeR2,
    conBV,
    conBVView,
    isymBV,
    ssymBV,
    symBV,
    unarySomeBV,
    unarySomeBVR1,
    unsafeSomeBV,
    pattern ConBV,
    pattern SomeIntN,
    pattern SomeSymIntN,
    pattern SomeSymWordN,
    pattern SomeWordN,
    type SomeIntN,
    type SomeSymIntN,
    type SomeSymWordN,
    type SomeWordN,
  )
import Grisette.SymPrim.AllSyms
  ( AllSyms (..),
    allSymsSize,
    symSize,
    symsSize,
  )
import Grisette.SymPrim.GeneralFun (type (-->))
import Grisette.SymPrim.ModelRep (ModelSymPair (..))
import Grisette.SymPrim.Prim.Model
  ( Model (..),
    ModelValuePair (..),
    SymbolSet (..),
  )
import Grisette.SymPrim.Prim.Term
  ( ConRep (..),
    LinkedRep,
    SupportedPrim,
    SymRep (..),
    TypedSymbol (..),
  )
import Grisette.SymPrim.SymBV
  ( SymIntN (..),
    SymWordN (..),
  )
import Grisette.SymPrim.SymBool
  ( SymBool (..),
  )
import Grisette.SymPrim.SymGeneralFun ((-->), type (-~>) (..))
import Grisette.SymPrim.SymInteger
  ( SymInteger (..),
  )
import Grisette.SymPrim.SymTabularFun (type (=~>) (..))
import Grisette.SymPrim.TabularFun (type (=->) (..))
