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
    IntN8,
    IntN16,
    IntN32,
    IntN64,
    WordN,
    WordN8,
    WordN16,
    WordN32,
    WordN64,
    ValidFP,
    FP,
    FP16,
    FP32,
    FP64,
    withValidFPProofs,
    FPRoundingMode (..),
    allFPRoundingMode,
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
    SymWordN8,
    SymWordN16,
    SymWordN32,
    SymWordN64,
    SymIntN (..),
    SymIntN8,
    SymIntN16,
    SymIntN32,
    SymIntN64,
    SymFP (..),
    SymFPRoundingMode (..),
    SymFP16,
    SymFP32,
    SymFP64,
    SomeSymIntN,
    SomeSymWordN,
    pattern SomeSymIntN,
    pattern SomeSymWordN,
    type (=~>) (..),
    type (-~>) (..),
    TypedSymbol (..),

    -- ** Extract symbolic values
    SomeSym (..),
    AllSyms (..),
    AllSyms1 (..),
    allSymsS1,
    AllSyms2 (..),
    allSymsS2,
    allSymsSize,
    symSize,
    symsSize,

    -- *** Generic 'AllSyms'
    AllSymsArgs (..),
    GAllSyms (..),
    genericAllSymsS,
    genericLiftAllSymsS,

    -- ** Symbolic constant sets and models
    SymbolSet (..),
    Model (..),
    ModelValuePair (..),
    ModelSymPair (..),
  )
where

import Grisette.Internal.SymPrim.AllSyms
  ( AllSyms (..),
    AllSyms1 (..),
    AllSyms2 (..),
    AllSymsArgs (..),
    GAllSyms (..),
    SomeSym (..),
    allSymsS1,
    allSymsS2,
    allSymsSize,
    genericAllSymsS,
    genericLiftAllSymsS,
    symSize,
    symsSize,
  )
import Grisette.Internal.SymPrim.BV
  ( BitwidthMismatch (..),
    IntN,
    IntN16,
    IntN32,
    IntN64,
    IntN8,
    WordN,
    WordN16,
    WordN32,
    WordN64,
    WordN8,
  )
import Grisette.Internal.SymPrim.FP
  ( FP,
    FP16,
    FP32,
    FP64,
    FPRoundingMode (..),
    ValidFP,
    allFPRoundingMode,
    withValidFPProofs,
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
    SymIntN16,
    SymIntN32,
    SymIntN64,
    SymIntN8,
    SymWordN (..),
    SymWordN16,
    SymWordN32,
    SymWordN64,
    SymWordN8,
  )
import Grisette.Internal.SymPrim.SymBool
  ( SymBool (..),
  )
import Grisette.Internal.SymPrim.SymFP
  ( SymFP (..),
    SymFP16,
    SymFP32,
    SymFP64,
    SymFPRoundingMode (..),
  )
import Grisette.Internal.SymPrim.SymGeneralFun ((-->), type (-~>) (..))
import Grisette.Internal.SymPrim.SymInteger
  ( SymInteger (..),
  )
import Grisette.Internal.SymPrim.SymTabularFun (type (=~>) (..))
import Grisette.Internal.SymPrim.TabularFun (type (=->) (..))
