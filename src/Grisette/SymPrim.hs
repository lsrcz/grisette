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
    -- * @'Bool' t'Grisette.SymPrim.=->' 'Bool'@: functions represented as a
    --   table for the input-output relations.
    -- * @'Bool' t'Grisette.SymPrim.-->' 'Bool'@: functions represented as a
    --   formula over some bound variables.
    --
    -- We also provide symbolic counterparts for these types, along with the
    -- basic types 'Bool' and 'Integer'. These symbolic types can be directly
    -- translated to constraints in the SMT solver.
    --
    -- * t'SymBool' ('Bool', symbolic Booleans)
    -- * t'SymInteger' ('Integer', symbolic unbounded integers)
    -- * @t'SymIntN' n@ (@'IntN' n@, symbolic signed bit vectors of bit width
    --   @n@)
    -- * @t'SymWordN' n@ (@'WordN' n@, symbolic unsigned bit vectors of bit width
    --   @n@)
    -- * @t'SymFP' eb sb@ (@'FP' eb sb@, symbolic IEEE-754 floating point numbers
    --   with @eb@ exponent bits and @sb@ significand bits)
    -- * t'SymAlgReal': symbolic algebraic real numbers.
    -- * @t'SymBool' t'Grisette.SymPrim.=~>' t'SymBool'@
    --   (@'Bool' t'Grisette.SymPrim.=->' 'Bool'@, symbolic
    --   functions, uninterpreted or represented as a table for the
    --   input-output relations).
    -- * @t'SymBool' t'Grisette.SymPrim.-~>' t'SymBool'@
    --   (@'Bool' t'Grisette.SymPrim.-->' 'Bool'@, symbolic
    --   functions, uninterpreted or represented as a formula over some
    --   bound variables).
    --
    -- This module provides an operation to extract all primitive values from a
    -- symbolic value, with 'AllSyms'. The module also provides the
    -- representation for symbols (@t'TypedSymbol'@), symbol sets
    -- (@t'SymbolSet'@), and models (@t'Model'@). They are useful when working
    -- with t'Grisette.Core.EvalSym', t'Grisette.Core.ExtractSym', and
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
    SomeBVKey,
    SomeBVException (..),
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

    -- *** Some low-level helpers for writing instances for t'SomeBV'

    -- | The functions here will check the bitwidths of the input bit-vectors
    -- and raise v'BitwidthMismatch' if they do not match.
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
    SymBoolKey,
    SymInteger (..),
    SymIntegerKey,

    -- ** Symbolic bit-vector types
    SymWordN (..),
    SymWordNKey,
    SymWordN8,
    SymWordN8Key,
    SymWordN16,
    SymWordN16Key,
    SymWordN32,
    SymWordN32Key,
    SymWordN64,
    SymWordN64Key,
    SymIntN (..),
    SymIntNKey,
    SymIntN8,
    SymIntN8Key,
    SymIntN16,
    SymIntN16Key,
    SymIntN32,
    SymIntN32Key,
    SymIntN64,
    SymIntN64Key,
    SomeSymIntN,
    SomeSymIntNKey,
    SomeSymWordN,
    SomeSymWordNKey,
    pattern SomeSymIntN,
    pattern SomeSymWordN,
    pattern SomeSymIntNKey,
    pattern SomeSymWordNKey,

    -- ** Symbolic floating point
    SymFP (..),
    SymFPKey,
    SymFPRoundingMode (..),
    SymFPRoundingModeKey,
    SymFP16,
    SymFP16Key,
    SymFP32,
    SymFP32Key,
    SymFP64,
    SymFP64Key,

    -- ** Symbolic algebraic real numbers
    SymAlgReal (..),
    SymAlgRealKey,

    -- ** Symbolic function, possibly uninterpreted
    type (=~>) (..),
    type (-~>) (..),

    -- ** Shared constraints
    Prim,
    SymPrim,
    BasicSymPrim,

    -- ** Quantifiers
    forallSet,
    forallSym,
    forallFresh,
    existsSet,
    existsSym,
    existsFresh,

    -- ** Basic constraints
    SupportedPrim,
    SymRep (SymType),
    ConRep (ConType),
    LinkedRep (..),

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
    SymbolKind (..),
    IsSymbolKind (..),
    TypedSymbol (..),
    typedAnySymbol,
    TypedAnySymbol,
    typedConstantSymbol,
    TypedConstantSymbol,
    SomeTypedSymbol (..),
    SomeTypedAnySymbol,
    SomeTypedConstantSymbol,
    SymbolSet,
    AnySymbolSet,
    ConstantSymbolSet,
    Model,
    ModelValuePair (..),
    ModelSymPair (..),

    -- * Analysis on the terms
    Term,
    SomeTerm (..),
    termSize,
    someTermSize,
    termsSize,
    someTermsSize,
    pattern SupportedTerm,
    pattern SupportedTypedSymbol,
    pattern SupportedConstantTypedSymbol,
    pattern ConTerm,
    pattern SymTerm,
    pattern ForallTerm,
    pattern ExistsTerm,
    pattern NotTerm,
    pattern OrTerm,
    pattern AndTerm,
    pattern EqTerm,
    pattern DistinctTerm,
    pattern ITETerm,
    pattern AddNumTerm,
    pattern NegNumTerm,
    pattern MulNumTerm,
    pattern AbsNumTerm,
    pattern SignumNumTerm,
    pattern LtOrdTerm,
    pattern LeOrdTerm,
    pattern AndBitsTerm,
    pattern OrBitsTerm,
    pattern XorBitsTerm,
    pattern ComplementBitsTerm,
    pattern ShiftLeftTerm,
    pattern RotateLeftTerm,
    pattern ShiftRightTerm,
    pattern RotateRightTerm,
    pattern BitCastTerm,
    pattern BitCastOrTerm,
    pattern BVConcatTerm,
    pattern BVSelectTerm,
    pattern BVExtendTerm,
    pattern ApplyTerm,
    pattern DivIntegralTerm,
    pattern ModIntegralTerm,
    pattern QuotIntegralTerm,
    pattern RemIntegralTerm,
    pattern FPTraitTerm,
    pattern FdivTerm,
    pattern RecipTerm,
    pattern FloatingUnaryTerm,
    pattern PowerTerm,
    pattern FPUnaryTerm,
    pattern FPBinaryTerm,
    pattern FPRoundingUnaryTerm,
    pattern FPRoundingBinaryTerm,
    pattern FPFMATerm,
    pattern FromIntegralTerm,
    pattern FromFPOrTerm,
    pattern ToFPTerm,
    pattern SelectTerm,
    pattern StoreTerm,
    pattern ConstArrayTerm,
    pattern SubTerms,
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
  ( IntN,
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
  ( AnySymbolSet,
    ConstantSymbolSet,
    Model (..),
    ModelValuePair (..),
    SymbolSet (..),
  )
import Grisette.Internal.SymPrim.Prim.Pattern (pattern SubTerms)
import Grisette.Internal.SymPrim.Prim.SomeTerm
  ( SomeTerm (..),
  )
import Grisette.Internal.SymPrim.Prim.Term
  ( ConRep (..),
    IsSymbolKind (..),
    LinkedRep (..),
    SomeTypedAnySymbol,
    SomeTypedConstantSymbol,
    SomeTypedSymbol (..),
    SupportedPrim,
    SymRep (..),
    SymbolKind (..),
    Term,
    TypedAnySymbol,
    TypedConstantSymbol,
    TypedSymbol (..),
    typedAnySymbol,
    typedConstantSymbol,
    pattern AbsNumTerm,
    pattern AddNumTerm,
    pattern AndBitsTerm,
    pattern AndTerm,
    pattern ApplyTerm,
    pattern BVConcatTerm,
    pattern BVExtendTerm,
    pattern BVSelectTerm,
    pattern BitCastOrTerm,
    pattern BitCastTerm,
    pattern ComplementBitsTerm,
    pattern ConTerm,
    pattern DistinctTerm,
    pattern DivIntegralTerm,
    pattern EqTerm,
    pattern ExistsTerm,
    pattern FPBinaryTerm,
    pattern FPFMATerm,
    pattern FPRoundingBinaryTerm,
    pattern FPRoundingUnaryTerm,
    pattern FPTraitTerm,
    pattern FPUnaryTerm,
    pattern FdivTerm,
    pattern FloatingUnaryTerm,
    pattern ForallTerm,
    pattern FromFPOrTerm,
    pattern FromIntegralTerm,
    pattern ITETerm,
    pattern LeOrdTerm,
    pattern LtOrdTerm,
    pattern ModIntegralTerm,
    pattern MulNumTerm,
    pattern NegNumTerm,
    pattern NotTerm,
    pattern OrBitsTerm,
    pattern OrTerm,
    pattern PowerTerm,
    pattern QuotIntegralTerm,
    pattern RecipTerm,
    pattern RemIntegralTerm,
    pattern RotateLeftTerm,
    pattern RotateRightTerm,
    pattern ShiftLeftTerm,
    pattern ShiftRightTerm,
    pattern SignumNumTerm,
    pattern SupportedConstantTypedSymbol,
    pattern SupportedTerm,
    pattern SupportedTypedSymbol,
    pattern SymTerm,
    pattern ToFPTerm,
    pattern SelectTerm,
    pattern StoreTerm,
    pattern ConstArrayTerm,
    pattern XorBitsTerm,
  )
import Grisette.Internal.SymPrim.Prim.TermUtils
  ( someTermSize,
    someTermsSize,
    termSize,
    termsSize,
  )
import Grisette.Internal.SymPrim.Quantifier
  ( existsFresh,
    existsSet,
    existsSym,
    forallFresh,
    forallSet,
    forallSym,
  )
import Grisette.Internal.SymPrim.SomeBV
  ( SomeBV (..),
    SomeBVException (..),
    SomeBVKey,
    SomeSymIntNKey,
    SomeSymWordNKey,
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
    pattern SomeSymIntNKey,
    pattern SomeSymWordN,
    pattern SomeSymWordNKey,
    pattern SomeWordN,
    type SomeIntN,
    type SomeSymIntN,
    type SomeSymWordN,
    type SomeWordN,
  )
import Grisette.Internal.SymPrim.SymAlgReal (SymAlgReal (..), SymAlgRealKey)
import Grisette.Internal.SymPrim.SymBV
  ( SymIntN (..),
    SymIntN16,
    SymIntN16Key,
    SymIntN32,
    SymIntN32Key,
    SymIntN64,
    SymIntN64Key,
    SymIntN8,
    SymIntN8Key,
    SymIntNKey,
    SymWordN (..),
    SymWordN16,
    SymWordN16Key,
    SymWordN32,
    SymWordN32Key,
    SymWordN64,
    SymWordN64Key,
    SymWordN8,
    SymWordN8Key,
    SymWordNKey,
  )
import Grisette.Internal.SymPrim.SymBool
  ( SymBool (..),
    SymBoolKey,
  )
import Grisette.Internal.SymPrim.SymFP
  ( SymFP (..),
    SymFP16,
    SymFP16Key,
    SymFP32,
    SymFP32Key,
    SymFP64,
    SymFP64Key,
    SymFPKey,
    SymFPRoundingMode (..),
    SymFPRoundingModeKey,
  )
import Grisette.Internal.SymPrim.SymGeneralFun ((-->), type (-~>) (..))
import Grisette.Internal.SymPrim.SymInteger
  ( SymInteger (..),
    SymIntegerKey,
  )
import Grisette.Internal.SymPrim.SymPrim
  ( BasicSymPrim,
    Prim,
    SymPrim,
  )
import Grisette.Internal.SymPrim.SymTabularFun (type (=~>) (..))
import Grisette.Internal.SymPrim.TabularFun (type (=->) (..))
