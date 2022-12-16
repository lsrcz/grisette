{-# LANGUAGE ExplicitNamespaces #-}

module Grisette.IR.SymPrim
  ( UnionM,
    type (=->) (..),
    type (-->),
    FuncArg (..),
    Sym (..),
    symSize,
    symsSize,
    SymBool,
    SymInteger,
    type (=~>),
    type (-~>),
    IntN,
    WordN,
    SymIntN,
    SymWordN,
    SymbolSet (..),
    Model (..),
    ModelValuePair (..),
    TypedSymbol (..),
    showUntyped,
    withSymbolSupported,
    SomeTypedSymbol (..),
    someTypedSymbol,
    evaluateTerm,
    SupportedPrim,

    -- * Specialized functions from the [grisette-core](https://hackage.haskell.org/package/grisette-core) package.

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
    mergingStrategy,
    Mergeable1,
    liftMergingStrategy,
    mergingStrategy1,
    Mergeable2,
    liftMergingStrategy2,
    mergingStrategy2,
    Mergeable3,
    liftMergingStrategy3,
    mergingStrategy3,
    derivedMergingStrategy,
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
import Grisette.IR.SymPrim.Data.TabularFunc
