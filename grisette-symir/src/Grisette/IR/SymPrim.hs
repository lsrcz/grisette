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

    -- * Specialized functions and types from the [grisette-core](https://hackage.haskell.org/package/grisette-core) package.

    -- ** UnionM
    UnionM,

    -- ** Symbolic equality
    SEq,
    symeq,
    symne,
    (==~),
    (/=~),

    -- ** Symbolic ordering
    SOrd,
    symlt,
    symle,
    symgt,
    symge,
    symCompare,
    (>~),
    (>=~),
    (<~),
    (<=~),

    -- ** Symbolic integer operation
    SymIntegerOp,

    -- ** Merging
    MergingStrategy,
    Mergeable,
    Mergeable',
    rootStrategy,
    Mergeable1,
    liftRootStrategy,
    rootStrategy1,
    Mergeable2,
    liftRootStrategy2,
    rootStrategy2,
    Mergeable3,
    liftRootStrategy3,
    rootStrategy3,
    derivedRootStrategy,
    wrapStrategy,
    product2Strategy,
    buildStrategyList,
    resolveStrategy,
    resolveStrategy',

    -- ** Simple mergeable and union operations
    SimpleMergeable,
    mrgIte,
    SimpleMergeable1,
    liftMrgIte,
    mrgIte1,
    SimpleMergeable2,
    liftMrgIte2,
    mrgIte2,
    UnionLike,
    UnionPrjOp,
    MonadUnion,

    -- ** Symbolic evaluation
    EvaluateSym,
    evaluateSym,
    evaluateSymToCon,

    -- ** Symbolic constant extraction
    ExtractSymbolics,
    extractSymbolics,

    -- ** Term substitution
    SubstituteSym,
    substituteSym,

    -- ** Symbolic choices
    chooseFresh,
    chooseSimpleFresh,
    chooseUnionFresh,
    choose,
    chooseSimple,
    chooseUnion,
  )
where

import Grisette.IR.SymPrim.Control.Monad.Union
import Grisette.IR.SymPrim.Control.Monad.UnionM
import Grisette.IR.SymPrim.Data.BV
import Grisette.IR.SymPrim.Data.Class.Evaluate
import Grisette.IR.SymPrim.Data.Class.ExtractSymbolics
import Grisette.IR.SymPrim.Data.Class.GenSym
import Grisette.IR.SymPrim.Data.Class.Mergeable
import Grisette.IR.SymPrim.Data.Class.SEq
import Grisette.IR.SymPrim.Data.Class.SOrd
import Grisette.IR.SymPrim.Data.Class.SimpleMergeable
import Grisette.IR.SymPrim.Data.Class.Substitute
import Grisette.IR.SymPrim.Data.Class.SymIntegerOp
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Grisette.IR.SymPrim.Data.Prim.Model
import Grisette.IR.SymPrim.Data.SymPrim
import Grisette.IR.SymPrim.Data.TabularFun
