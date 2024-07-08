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
  ( -- | Grisette introduces new primitive types:
    --
    -- * @'IntN' n@: signed bit vectors of bit width @n@.
    -- * @'WordN' n@: unsigned bit vectors of bit width @n@.
    -- * @'FP' eb sb@: IEEE-754 floating point numbers with @eb@ exponent bits
    --   and @sb@ significand bits.
    -- * 'AlgReal': algebraic real numbers. Can represent rational numbers.
    --   If come from solver's response, it may also represented by roots of
    --   polynomials or intervals.
    -- * @'Bool' t'=->' 'Bool'@: functions represented as a table for the
    --   input-output relations.
    -- * @'Bool' t'-->' 'Bool'@: functions represented as a formula over some
    --   bound variables.
    --
    -- We also provide symbolic counterparts for these types, along with the
    -- basic types 'Bool' and 'Integer'. These symbolic types can be directly
    -- translated to constraints in the SMT solver.
    --
    -- * 'SymBool' ('Bool', symbolic Booleans)
    -- * 'SymInteger' ('Integer', symbolic unbounded integers)
    -- * @'SymIntN' n@ (@'IntN' n@, symbolic signed bit vectors of bit width
    --   @n@)
    -- * @'SymWordN' n@ (@'WordN' n@, symbolic unsigned bit vectors of bit width
    --   @n@)
    -- * @'SymFP' eb sb@ (@'FP' eb sb@, symbolic IEEE-754 floating point numbers
    --   with @eb@ exponent bits and @sb@ significand bits)
    -- * 'SymAlgReal': symbolic algebraic real numbers.
    -- * @'SymBool' t'=~>' 'SymBool'@ (@'Bool' t'=->' 'Bool'@, symbolic
    --   functions, uninterpreted or represented as a table for the
    --   input-output relations).
    -- * @'SymBool' t'-~>' 'SymBool'@ (@'Bool' t'-->' 'Bool'@, symbolic
    --   functions, uninterpreted or represented as a formula over some
    --   bound variables).
    --
    -- This module provides an operation to extract all primitive values from a
    -- symbolic value, with 'AllSyms'. The module also provides the
    -- representation for symbols ('TypedSymbol'), symbol sets ('SymbolSet'),
    -- and models ('Model'). They are useful when working with
    -- t'Grisette.Core.EvalSym', t'Grisette.Core.ExtractSym', and
    -- t'Grisette.Core.SubstSym'.

    -- * Extended types

    -- ** Size-tagged bit-vector types
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

    -- ** Runtime-sized bit-vector types
    SomeBV (..),
    BitwidthMismatch (..),
    pattern SomeIntN,
    type SomeIntN,
    pattern SomeWordN,
    type SomeWordN,
    conBV,
    conBVView,
    pattern ConBV,
    symBV,
    ssymBV,
    isymBV,
    arbitraryBV,

    -- *** Some low-level helpers for writing instances for 'SomeBV'

    -- | The functions here will check the bitwidths of the input bit-vectors
    -- and raise 'BitwidthMismatch' if they do not match.
    unsafeSomeBV,
    unarySomeBV,
    unarySomeBVR1,
    binSomeBV,
    binSomeBVR1,
    binSomeBVR2,
    binSomeBVSafe,
    binSomeBVSafeR1,
    binSomeBVSafeR2,

    -- ** Floating point
    ValidFP,
    FP,
    FP16,
    FP32,
    FP64,
    withValidFPProofs,
    FPRoundingMode (..),
    allFPRoundingMode,

    -- ** Algebraic real numbers
    AlgReal (..),
    AlgRealPoly (..),
    RealPoint (..),
    UnsupportedAlgRealOperation (..),

    -- ** Functions
    type (=->) (..),
    type (-->),
    (-->),

    -- * Symbolic types

    -- ** Symbolic bool and integer types
    SymBool (..),
    SymInteger (..),

    -- ** Symbolic bit-vector types
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
    SomeSymIntN,
    SomeSymWordN,
    pattern SomeSymIntN,
    pattern SomeSymWordN,

    -- ** Symbolic floating point
    SymFP (..),
    SymFPRoundingMode (..),
    SymFP16,
    SymFP32,
    SymFP64,

    -- ** Symbolic algebraic real numbers
    SymAlgReal (..),

    -- ** Symbolic function, possibly uninterpreted
    type (=~>) (..),
    type (-~>) (..),

    -- ** Basic constraints
    SupportedPrim,
    SymRep (SymType),
    ConRep (ConType),
    LinkedRep,

    -- * Extract symbolic values
    SomeSym (..),
    AllSyms (..),
    AllSyms1 (..),
    allSymsS1,
    AllSyms2 (..),
    allSymsS2,
    allSymsSize,
    symSize,
    symsSize,

    -- ** Generic 'AllSyms'
    AllSymsArgs (..),
    GAllSyms (..),
    genericAllSymsS,
    genericLiftAllSymsS,

    -- * Symbolic constant sets and models
    TypedSymbol (..),
    SymbolSet,
    Model,
    ModelValuePair (..),
    ModelSymPair (..),
  )
where

import Grisette.Internal.SymPrim.AlgReal
  ( AlgReal (..),
    AlgRealPoly (..),
    RealPoint (..),
    UnsupportedAlgRealOperation (..),
  )
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
import Grisette.Internal.SymPrim.SymAlgReal (SymAlgReal (..))
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
