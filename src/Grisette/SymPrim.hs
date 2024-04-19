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
    BitwidthMismatch (..),
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

import Grisette.Internal.SymPrim.AllSyms
  ( AllSyms (..),
    allSymsSize,
    symSize,
    symsSize,
  )
import Grisette.Internal.SymPrim.BV
  ( BitwidthMismatch (..),
    IntN,
    WordN,
  )
import Grisette.Internal.SymPrim.GeneralFun (type (-->))
import Grisette.Internal.SymPrim.ModelRep (ModelSymPair (..))
import Grisette.Internal.SymPrim.Prim.Model
  ( Model (..),
    ModelValuePair (..),
    SymbolSet (..),
  )
import Grisette.Internal.SymPrim.Prim.Term
  ( ConRep (..),
    LinkedRep,
    SupportedPrim,
    SymRep (..),
    TypedSymbol (..),
  )
import Grisette.Internal.SymPrim.SomeBV
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
import Grisette.Internal.SymPrim.SymBV
  ( SymIntN (..),
    SymWordN (..),
  )
import Grisette.Internal.SymPrim.SymBool
  ( SymBool (..),
  )
import Grisette.Internal.SymPrim.SymGeneralFun ((-->), type (-~>) (..))
import Grisette.Internal.SymPrim.SymInteger
  ( SymInteger (..),
  )
import Grisette.Internal.SymPrim.SymTabularFun (type (=~>) (..))
import Grisette.Internal.SymPrim.TabularFun (type (=->) (..))