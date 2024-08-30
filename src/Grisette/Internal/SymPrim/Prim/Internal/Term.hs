{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Prim.Internal.Term
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.Prim.Internal.Term
  ( -- * Supported primitive types
    SupportedPrimConstraint (..),
    SupportedPrim (..),
    SymRep (..),
    ConRep (..),
    LinkedRep (..),

    -- * Partial evaluation for the terms
    PEvalApplyTerm (..),
    PEvalBitwiseTerm (..),
    PEvalShiftTerm (..),
    PEvalRotateTerm (..),
    PEvalNumTerm (..),
    pevalSubNumTerm,
    PEvalOrdTerm (..),
    pevalGtOrdTerm,
    pevalGeOrdTerm,
    pevalNEqTerm,
    PEvalDivModIntegralTerm (..),
    PEvalBitCastTerm (..),
    PEvalBitCastOrTerm (..),
    PEvalBVTerm (..),
    PEvalFractionalTerm (..),
    PEvalFloatingTerm (..),
    PEvalFromIntegralTerm (..),
    PEvalIEEEFPConvertibleTerm (..),

    -- * Typed symbols
    SymbolKind (..),
    TypedSymbol (..),
    TypedConstantSymbol,
    TypedAnySymbol,
    SomeTypedSymbol (..),
    SomeTypedConstantSymbol,
    SomeTypedAnySymbol,
    IsSymbolKind (..),
    showUntyped,
    withSymbolSupported,
    someTypedSymbol,
    eqHeteroSymbol,
    castSomeTypedSymbol,
    withSymbolKind,

    -- * Terms
    FPTrait (..),
    FPUnaryOp (..),
    FPBinaryOp (..),
    FPRoundingUnaryOp (..),
    FPRoundingBinaryOp (..),
    FloatingUnaryOp (..),
    Term (..),
    identity,
    typeHashId,
    introSupportedPrimConstraint,
    pformatTerm,

    -- * Interning
    UTerm (..),
    prettyPrintTerm,
    forallTerm,
    existsTerm,
    conTerm,
    symTerm,
    ssymTerm,
    isymTerm,
    notTerm,
    orTerm,
    andTerm,
    eqTerm,
    distinctTerm,
    iteTerm,
    addNumTerm,
    negNumTerm,
    mulNumTerm,
    absNumTerm,
    signumNumTerm,
    ltOrdTerm,
    leOrdTerm,
    andBitsTerm,
    orBitsTerm,
    xorBitsTerm,
    complementBitsTerm,
    shiftLeftTerm,
    shiftRightTerm,
    rotateLeftTerm,
    rotateRightTerm,
    bitCastTerm,
    bitCastOrTerm,
    bvconcatTerm,
    bvselectTerm,
    bvextendTerm,
    bvsignExtendTerm,
    bvzeroExtendTerm,
    applyTerm,
    divIntegralTerm,
    modIntegralTerm,
    quotIntegralTerm,
    remIntegralTerm,
    fpTraitTerm,
    fdivTerm,
    recipTerm,
    floatingUnaryTerm,
    powerTerm,
    fpUnaryTerm,
    fpBinaryTerm,
    fpRoundingUnaryTerm,
    fpRoundingBinaryTerm,
    fpFMATerm,
    fromIntegralTerm,
    fromFPOrTerm,
    toFPTerm,

    -- * Support for boolean type
    trueTerm,
    falseTerm,
    pattern BoolConTerm,
    pattern TrueTerm,
    pattern FalseTerm,
    pattern BoolTerm,
    pevalNotTerm,
    pevalOrTerm,
    pevalAndTerm,
    pevalImplyTerm,
    pevalXorTerm,
    pevalITEBasic,
    pevalITEBasicTerm,
    pevalDefaultEqTerm,
    NonFuncPrimConstraint,
    NonFuncSBVRep (..),
    SupportedNonFuncPrim (..),
    SBVRep (..),
    SBVFreshMonad (..),
    translateTypeError,
    parseSMTModelResultError,
    partitionCVArg,
    parseScalarSMTModelResult,
  )
where

#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter
  ( column,
    pageWidth,
    Doc,
    PageWidth(Unbounded, AvailablePerLine),
    Pretty(pretty),
  )
#else
import Data.Text.Prettyprint.Doc
  ( column,
    pageWidth,
    Doc,
    PageWidth(Unbounded, AvailablePerLine),
    Pretty(pretty),
  )
#endif

#if !MIN_VERSION_sbv(10, 0, 0)
#define SMTDefinable Uninterpreted
#endif

import Control.DeepSeq (NFData (rnf))
import Control.Monad (msum)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.RWS (RWST)
import Control.Monad.Reader (MonadTrans (lift), ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Writer (WriterT)
import Data.Bits (Bits)
import Data.Hashable (Hashable (hashWithSalt))
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import Data.Maybe (fromMaybe)
import qualified Data.SBV as SBV
import qualified Data.SBV.Dynamic as SBVD
import qualified Data.SBV.Trans as SBVT
import qualified Data.SBV.Trans.Control as SBVTC
import Data.String (IsString (fromString))
import Data.Typeable (Proxy (Proxy), cast, typeRepFingerprint)
import GHC.Exts (sortWith)
import GHC.Fingerprint (Fingerprint)
import GHC.Generics (Generic)
import GHC.IO (unsafePerformIO)
import GHC.Stack (HasCallStack)
import GHC.TypeNats (KnownNat, Nat, type (+), type (<=))
import Grisette.Internal.Core.Data.Class.BitCast (BitCast, BitCastOr)
import Grisette.Internal.Core.Data.Class.BitVector
  ( SizedBV,
  )
import Grisette.Internal.Core.Data.Class.SymRotate (SymRotate)
import Grisette.Internal.Core.Data.Class.SymShift (SymShift)
import Grisette.Internal.Core.Data.Symbol
  ( Identifier,
    Symbol (IndexedSymbol, SimpleSymbol),
  )
import Grisette.Internal.SymPrim.FP (FP, FPRoundingMode, ValidFP)
import Grisette.Internal.SymPrim.Prim.Internal.Caches
  ( Digest,
    Id,
    Interned
      ( Description,
        Uninterned,
        describe,
        descriptionDigest,
        identify,
        threadId
      ),
    intern,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Utils
  ( WeakThreadId,
    eqTypeRepBool,
    myWeakThreadId,
    pattern Dyn,
  )
import Grisette.Internal.SymPrim.Prim.ModelValue
  ( ModelValue,
    toModelValue,
  )
import Language.Haskell.TH.Syntax (Lift (liftTyped))
import Type.Reflection
  ( SomeTypeRep (SomeTypeRep),
    TypeRep,
    Typeable,
    eqTypeRep,
    someTypeRep,
    typeRep,
    type (:~~:) (HRefl),
  )
import Unsafe.Coerce (unsafeCoerce)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim

-- | Monads that supports generating sbv fresh variables.
class (MonadIO m) => SBVFreshMonad m where
  sbvFresh :: (SBV.SymVal a) => String -> m (SBV.SBV a)

instance (MonadIO m) => SBVFreshMonad (SBVT.SymbolicT m) where
  sbvFresh = SBVT.free
  {-# INLINE sbvFresh #-}

instance (MonadIO m) => SBVFreshMonad (SBVTC.QueryT m) where
  sbvFresh = SBVTC.freshVar
  {-# INLINE sbvFresh #-}

instance (SBVFreshMonad m) => SBVFreshMonad (ReaderT r m) where
  sbvFresh = lift . sbvFresh
  {-# INLINE sbvFresh #-}

instance (SBVFreshMonad m, Monoid w) => SBVFreshMonad (WriterT w m) where
  sbvFresh = lift . sbvFresh
  {-# INLINE sbvFresh #-}

instance (SBVFreshMonad m, Monoid w) => SBVFreshMonad (RWST r w s m) where
  sbvFresh = lift . sbvFresh
  {-# INLINE sbvFresh #-}

instance (SBVFreshMonad m) => SBVFreshMonad (StateT s m) where
  sbvFresh = lift . sbvFresh
  {-# INLINE sbvFresh #-}

-- | Error message for unsupported types.
translateTypeError :: (HasCallStack) => Maybe String -> TypeRep a -> b
translateTypeError Nothing ta =
  error $
    "Don't know how to translate the type " ++ show ta ++ " to SMT"
translateTypeError (Just reason) ta =
  error $
    "Don't know how to translate the type " ++ show ta ++ " to SMT: " <> reason

-- | Type class for resolving the base type for the SBV type for the primitive
-- type.
class (SupportedPrim a, Ord a) => NonFuncSBVRep a where
  type NonFuncSBVBaseType a

-- | Type class for resolving the constraint for a supported non-function
-- primitive type.
type NonFuncPrimConstraint a =
  ( SBV.SymVal (NonFuncSBVBaseType a),
    SBV.EqSymbolic (SBVType a),
    SBV.Mergeable (SBVType a),
    SBV.SMTDefinable (SBVType a),
    SBV.Mergeable (SBVType a),
    SBVType a ~ SBV.SBV (NonFuncSBVBaseType a),
    PrimConstraint a
  )

-- | Indicates that a type is supported, can be represented as a symbolic term,
-- is not a function type, and can be lowered to an SBV term.
class (NonFuncSBVRep a) => SupportedNonFuncPrim a where
  conNonFuncSBVTerm :: a -> SBV.SBV (NonFuncSBVBaseType a)
  symNonFuncSBVTerm ::
    (SBVFreshMonad m) => String -> m (SBV.SBV (NonFuncSBVBaseType a))
  withNonFuncPrim :: ((NonFuncPrimConstraint a) => r) -> r

-- | Partition the list of CVs for models for functions.
partitionCVArg ::
  forall a.
  (SupportedNonFuncPrim a) =>
  [([SBVD.CV], SBVD.CV)] ->
  [(a, [([SBVD.CV], SBVD.CV)])]
partitionCVArg cv =
  partitionOrdCVArg $
    parseFirstCVArg cv
  where
    parseFirstCVArg ::
      forall a.
      (SupportedNonFuncPrim a) =>
      [([SBVD.CV], SBVD.CV)] ->
      [(a, [([SBVD.CV], SBVD.CV)])]
    parseFirstCVArg =
      fmap
        ( \case
            (x : xs, v) ->
              (parseSMTModelResult 0 ([], x), [(xs, v)])
            _ -> error "impossible"
        )
    partitionOrdCVArg ::
      forall a.
      (SupportedNonFuncPrim a) =>
      [(a, [([SBVD.CV], SBVD.CV)])] ->
      [(a, [([SBVD.CV], SBVD.CV)])]
    partitionOrdCVArg v = go sorted
      where
        sorted = sortWith fst v :: [(a, [([SBVD.CV], SBVD.CV)])]
        go (x : x1 : xs) =
          if fst x == fst x1
            then go $ (fst x, snd x ++ snd x1) : xs
            else x : go (x1 : xs)
        go x = x

-- | Parse the scalar model result.
parseScalarSMTModelResult ::
  forall v r.
  (SBV.SatModel r, Typeable v) =>
  (r -> v) ->
  ([([SBVD.CV], SBVD.CV)], SBVD.CV) ->
  v
parseScalarSMTModelResult convert cvs@([], v) = case SBV.parseCVs [v] of
  Just (x, _) -> convert x
  Nothing -> parseSMTModelResultError (typeRep @v) cvs
parseScalarSMTModelResult _ cv = parseSMTModelResultError (typeRep @v) cv

-- | Type class for resolving the SBV type for the primitive type.
class SBVRep t where
  type SBVType t

-- | Type class for resolving the constraint for a supported primitive type.
class SupportedPrimConstraint t where
  type PrimConstraint t :: Constraint
  type PrimConstraint _ = ()

-- | Indicates that a type is supported, can be represented as a symbolic term,
-- and can be lowered to an SBV term.
class
  ( Lift t,
    Typeable t,
    Hashable t,
    Eq t,
    Show t,
    NFData t,
    SupportedPrimConstraint t,
    SBVRep t
  ) =>
  SupportedPrim t
  where
  pformatCon :: t -> String
  default pformatCon :: (Show t) => t -> String
  pformatCon = show
  pformatSym :: TypedSymbol 'AnyKind t -> String
  pformatSym = showUntyped
  defaultValue :: t
  defaultValueDynamic :: proxy t -> ModelValue
  defaultValueDynamic _ = toModelValue (defaultValue @t)
  pevalITETerm :: Term Bool -> Term t -> Term t -> Term t
  pevalEqTerm :: Term t -> Term t -> Term Bool
  pevalDistinctTerm :: NonEmpty (Term t) -> Term Bool
  conSBVTerm :: t -> SBVType t
  symSBVName :: TypedSymbol 'AnyKind t -> Int -> String
  symSBVTerm :: (SBVFreshMonad m) => String -> m (SBVType t)
  default withPrim ::
    ( PrimConstraint t,
      SBV.SMTDefinable (SBVType t),
      SBV.Mergeable (SBVType t),
      Typeable (SBVType t)
    ) =>
    ( ( PrimConstraint t,
        SBV.SMTDefinable (SBVType t),
        SBV.Mergeable (SBVType t),
        Typeable (SBVType t)
      ) =>
      a
    ) ->
    a
  withPrim ::
    ( ( PrimConstraint t,
        SBV.SMTDefinable (SBVType t),
        SBV.Mergeable (SBVType t),
        Typeable (SBVType t)
      ) =>
      a
    ) ->
    a
  withPrim i = i
  sbvIte :: SBV.SBV Bool -> SBVType t -> SBVType t -> SBVType t
  sbvIte = withPrim @t SBV.ite
  sbvEq :: SBVType t -> SBVType t -> SBV.SBV Bool
  default sbvEq ::
    (SBVT.EqSymbolic (SBVType t)) => SBVType t -> SBVType t -> SBV.SBV Bool
  sbvEq = (SBV..==)
  sbvDistinct :: NonEmpty (SBVType t) -> SBV.SBV Bool
  default sbvDistinct ::
    (SBVT.EqSymbolic (SBVType t)) => NonEmpty (SBVType t) -> SBV.SBV Bool
  sbvDistinct = SBV.distinct . toList
  parseSMTModelResult :: Int -> ([([SBVD.CV], SBVD.CV)], SBVD.CV) -> t
  castTypedSymbol ::
    (IsSymbolKind knd') => TypedSymbol knd t -> Maybe (TypedSymbol knd' t)
  isFuncType :: Bool
  funcDummyConstraint :: SBVType t -> SBV.SBV Bool

-- | Cast a typed symbol to a different kind. Check if the kind is compatible.
castSomeTypedSymbol ::
  (IsSymbolKind knd') => SomeTypedSymbol knd -> Maybe (SomeTypedSymbol knd')
castSomeTypedSymbol (SomeTypedSymbol ty s@TypedSymbol {}) =
  SomeTypedSymbol ty <$> castTypedSymbol s
{-# INLINE castSomeTypedSymbol #-}

-- | Error message for failure to parse the SBV model result.
parseSMTModelResultError ::
  (HasCallStack) => TypeRep a -> ([([SBVD.CV], SBVD.CV)], SBVD.CV) -> a
parseSMTModelResultError ty cv =
  error $
    "BUG: cannot parse SBV model value \""
      <> show cv
      <> "\" to Grisette model value with the type "
      <> show ty

-- | Partial evaluation for inequality terms.
pevalNEqTerm :: (SupportedPrim a) => Term a -> Term a -> Term Bool
pevalNEqTerm l r = pevalNotTerm $ pevalEqTerm l r
{-# INLINE pevalNEqTerm #-}

-- | Type family to resolve the concrete type associated with a symbolic type.
class ConRep sym where
  type ConType sym

-- | Type family to resolve the symbolic type associated with a concrete type.
class (SupportedPrim con) => SymRep con where
  type SymType con

-- | One-to-one mapping between symbolic types and concrete types.
class
  (ConRep sym, SymRep con, sym ~ SymType con, con ~ ConType sym) =>
  LinkedRep con sym
    | con -> sym,
      sym -> con
  where
  underlyingTerm :: sym -> Term con
  wrapTerm :: Term con -> sym

-- | Partial evaluation and lowering for function application terms.
class
  (SupportedPrim f, SupportedPrim a, SupportedPrim b) =>
  PEvalApplyTerm f a b
    | f -> a b
  where
  pevalApplyTerm :: Term f -> Term a -> Term b
  sbvApplyTerm :: SBVType f -> SBVType a -> SBVType b

-- | Partial evaluation and lowering for bitwise operation terms.
class (SupportedNonFuncPrim t, Bits t) => PEvalBitwiseTerm t where
  pevalAndBitsTerm :: Term t -> Term t -> Term t
  pevalOrBitsTerm :: Term t -> Term t -> Term t
  pevalXorBitsTerm :: Term t -> Term t -> Term t
  pevalComplementBitsTerm :: Term t -> Term t
  withSbvBitwiseTermConstraint :: (((Bits (SBVType t)) => r)) -> r
  sbvAndBitsTerm :: SBVType t -> SBVType t -> SBVType t
  sbvAndBitsTerm = withSbvBitwiseTermConstraint @t (SBV..&.)
  sbvOrBitsTerm :: SBVType t -> SBVType t -> SBVType t
  sbvOrBitsTerm = withSbvBitwiseTermConstraint @t (SBV..|.)
  sbvXorBitsTerm :: SBVType t -> SBVType t -> SBVType t
  sbvXorBitsTerm = withSbvBitwiseTermConstraint @t SBV.xor
  sbvComplementBitsTerm :: SBVType t -> SBVType t
  sbvComplementBitsTerm = withSbvBitwiseTermConstraint @t SBV.complement

-- | Partial evaluation and lowering for symbolic shifting terms.
class (SupportedNonFuncPrim t, SymShift t) => PEvalShiftTerm t where
  pevalShiftLeftTerm :: Term t -> Term t -> Term t
  pevalShiftRightTerm :: Term t -> Term t -> Term t
  withSbvShiftTermConstraint ::
    (((SBV.SIntegral (NonFuncSBVBaseType t)) => r)) -> r
  sbvShiftLeftTerm :: SBVType t -> SBVType t -> SBVType t
  sbvShiftLeftTerm l r =
    withNonFuncPrim @t $ withSbvShiftTermConstraint @t $ SBV.sShiftLeft l r
  sbvShiftRightTerm :: SBVType t -> SBVType t -> SBVType t
  sbvShiftRightTerm l r =
    withNonFuncPrim @t $ withSbvShiftTermConstraint @t $ SBV.sShiftRight l r

-- | Partial evaluation and lowering for symbolic rotate terms.
class (SupportedNonFuncPrim t, SymRotate t) => PEvalRotateTerm t where
  pevalRotateLeftTerm :: Term t -> Term t -> Term t
  pevalRotateRightTerm :: Term t -> Term t -> Term t
  withSbvRotateTermConstraint ::
    (((SBV.SIntegral (NonFuncSBVBaseType t)) => r)) -> r
  sbvRotateLeftTerm :: SBVType t -> SBVType t -> SBVType t
  sbvRotateLeftTerm l r =
    withNonFuncPrim @t $ withSbvRotateTermConstraint @t $ SBV.sRotateLeft l r
  sbvRotateRightTerm :: SBVType t -> SBVType t -> SBVType t
  sbvRotateRightTerm l r =
    withNonFuncPrim @t $ withSbvRotateTermConstraint @t $ SBV.sRotateRight l r

-- | Partial evaluation and lowering for number terms.
class (SupportedNonFuncPrim t, Num t) => PEvalNumTerm t where
  pevalAddNumTerm :: Term t -> Term t -> Term t
  pevalNegNumTerm :: Term t -> Term t
  pevalMulNumTerm :: Term t -> Term t -> Term t
  pevalAbsNumTerm :: Term t -> Term t
  pevalSignumNumTerm :: Term t -> Term t
  withSbvNumTermConstraint :: (((Num (SBVType t)) => r)) -> r
  sbvAddNumTerm ::
    SBVType t ->
    SBVType t ->
    SBVType t
  sbvAddNumTerm l r = withSbvNumTermConstraint @t $ l + r
  sbvNegNumTerm ::
    SBVType t ->
    SBVType t
  sbvNegNumTerm l = withSbvNumTermConstraint @t $ -l
  sbvMulNumTerm ::
    SBVType t ->
    SBVType t ->
    SBVType t
  sbvMulNumTerm l r = withSbvNumTermConstraint @t $ l * r
  sbvAbsNumTerm ::
    SBVType t ->
    SBVType t
  sbvAbsNumTerm l = withSbvNumTermConstraint @t $ abs l
  sbvSignumNumTerm ::
    SBVType t ->
    SBVType t
  sbvSignumNumTerm l = withSbvNumTermConstraint @t $ signum l

-- | Partial evaluation for subtraction terms.
pevalSubNumTerm :: (PEvalNumTerm a) => Term a -> Term a -> Term a
pevalSubNumTerm l r = pevalAddNumTerm l (pevalNegNumTerm r)

-- | Partial evaluation and lowering for comparison terms.
class (SupportedNonFuncPrim t, Ord t) => PEvalOrdTerm t where
  pevalLtOrdTerm :: Term t -> Term t -> Term Bool
  pevalLeOrdTerm :: Term t -> Term t -> Term Bool
  withSbvOrdTermConstraint :: (((SBV.OrdSymbolic (SBVType t)) => r)) -> r
  sbvLtOrdTerm ::
    SBVType t ->
    SBVType t ->
    SBV.SBV Bool
  sbvLtOrdTerm l r = withSbvOrdTermConstraint @t $ l SBV..< r
  sbvLeOrdTerm :: SBVType t -> SBVType t -> SBV.SBV Bool
  sbvLeOrdTerm l r = withSbvOrdTermConstraint @t $ l SBV..<= r

-- | Partial evaluation for greater than terms.
pevalGtOrdTerm :: (PEvalOrdTerm a) => Term a -> Term a -> Term Bool
pevalGtOrdTerm = flip pevalLtOrdTerm
{-# INLINE pevalGtOrdTerm #-}

-- | Partial evaluation for greater than or equal to terms.
pevalGeOrdTerm :: (PEvalOrdTerm a) => Term a -> Term a -> Term Bool
pevalGeOrdTerm = flip pevalLeOrdTerm
{-# INLINE pevalGeOrdTerm #-}

-- | Partial evaluation and lowering for integer division and modulo terms.
class (SupportedNonFuncPrim t, Integral t) => PEvalDivModIntegralTerm t where
  pevalDivIntegralTerm :: Term t -> Term t -> Term t
  pevalModIntegralTerm :: Term t -> Term t -> Term t
  pevalQuotIntegralTerm :: Term t -> Term t -> Term t
  pevalRemIntegralTerm :: Term t -> Term t -> Term t
  withSbvDivModIntegralTermConstraint ::
    (((SBV.SDivisible (SBVType t)) => r)) -> r
  sbvDivIntegralTerm :: SBVType t -> SBVType t -> SBVType t
  sbvDivIntegralTerm l r =
    withSbvDivModIntegralTermConstraint @t $ l `SBV.sDiv` r
  sbvModIntegralTerm :: SBVType t -> SBVType t -> SBVType t
  sbvModIntegralTerm l r =
    withSbvDivModIntegralTermConstraint @t $ l `SBV.sMod` r
  sbvQuotIntegralTerm :: SBVType t -> SBVType t -> SBVType t
  sbvQuotIntegralTerm l r =
    withSbvDivModIntegralTermConstraint @t $ l `SBV.sQuot` r
  sbvRemIntegralTerm :: SBVType t -> SBVType t -> SBVType t
  sbvRemIntegralTerm l r =
    withSbvDivModIntegralTermConstraint @t $ l `SBV.sRem` r

-- | Partial evaluation and lowering for bitcast terms.
class
  (SupportedNonFuncPrim a, SupportedNonFuncPrim b, BitCast a b) =>
  PEvalBitCastTerm a b
  where
  pevalBitCastTerm :: Term a -> Term b
  sbvBitCast :: SBVType a -> SBVType b

-- | Partial evaluation and lowering for bitcast or default value terms.
class
  (SupportedNonFuncPrim a, SupportedNonFuncPrim b, BitCastOr a b) =>
  PEvalBitCastOrTerm a b
  where
  pevalBitCastOrTerm :: Term b -> Term a -> Term b
  sbvBitCastOr :: SBVType b -> SBVType a -> SBVType b

-- | Partial evaluation and lowering for bit-vector terms.
class
  ( forall n. (KnownNat n, 1 <= n) => SupportedNonFuncPrim (bv n),
    SizedBV bv,
    Typeable bv
  ) =>
  PEvalBVTerm bv
  where
  pevalBVConcatTerm ::
    (KnownNat l, KnownNat r, 1 <= l, 1 <= r) =>
    Term (bv l) ->
    Term (bv r) ->
    Term (bv (l + r))
  pevalBVExtendTerm ::
    (KnownNat l, KnownNat r, 1 <= l, 1 <= r, l <= r) =>
    Bool ->
    proxy r ->
    Term (bv l) ->
    Term (bv r)
  pevalBVSelectTerm ::
    (KnownNat n, KnownNat ix, KnownNat w, 1 <= n, 1 <= w, ix + w <= n) =>
    p ix ->
    q w ->
    Term (bv n) ->
    Term (bv w)
  sbvBVConcatTerm ::
    (KnownNat l, KnownNat r, 1 <= l, 1 <= r) =>
    p1 l ->
    p2 r ->
    SBVType (bv l) ->
    SBVType (bv r) ->
    SBVType (bv (l + r))
  sbvBVExtendTerm ::
    (KnownNat l, KnownNat r, 1 <= l, 1 <= r, l <= r) =>
    p1 l ->
    p2 r ->
    Bool ->
    SBVType (bv l) ->
    SBVType (bv r)
  sbvBVSelectTerm ::
    ( KnownNat ix,
      KnownNat w,
      KnownNat n,
      1 <= n,
      1 <= w,
      ix + w <= n
    ) =>
    p1 ix ->
    p2 w ->
    p3 n ->
    SBVType (bv n) ->
    SBVType (bv w)

-- | Partial evaluation and lowering for fractional terms.
class (SupportedNonFuncPrim t, Fractional t) => PEvalFractionalTerm t where
  pevalFdivTerm :: Term t -> Term t -> Term t
  pevalRecipTerm :: Term t -> Term t
  withSbvFractionalTermConstraint ::
    (((Fractional (SBVType t)) => r)) ->
    r
  sbvFdivTerm ::
    SBVType t ->
    SBVType t ->
    SBVType t
  sbvFdivTerm l r = withSbvFractionalTermConstraint @t $ l / r
  sbvRecipTerm ::
    SBVType t ->
    SBVType t
  sbvRecipTerm l = withSbvFractionalTermConstraint @t $ recip l

-- | Unary floating point operations.
data FloatingUnaryOp
  = FloatingExp
  | FloatingLog
  | FloatingSqrt
  | FloatingSin
  | FloatingCos
  | FloatingTan
  | FloatingAsin
  | FloatingAcos
  | FloatingAtan
  | FloatingSinh
  | FloatingCosh
  | FloatingTanh
  | FloatingAsinh
  | FloatingAcosh
  | FloatingAtanh
  deriving (Eq, Ord, Generic, Hashable, Lift, NFData)

instance Show FloatingUnaryOp where
  show FloatingExp = "exp"
  show FloatingLog = "log"
  show FloatingSqrt = "sqrt"
  show FloatingSin = "sin"
  show FloatingCos = "cos"
  show FloatingTan = "tan"
  show FloatingAsin = "asin"
  show FloatingAcos = "acos"
  show FloatingAtan = "atan"
  show FloatingSinh = "sinh"
  show FloatingCosh = "cosh"
  show FloatingTanh = "tanh"
  show FloatingAsinh = "asinh"
  show FloatingAcosh = "acosh"
  show FloatingAtanh = "atanh"

-- | Partial evaluation and lowering for floating point terms.
class (SupportedNonFuncPrim t) => PEvalFloatingTerm t where
  pevalFloatingUnaryTerm :: FloatingUnaryOp -> Term t -> Term t
  pevalPowerTerm :: Term t -> Term t -> Term t
  withSbvFloatingTermConstraint ::
    (((Floating (SBVType t)) => r)) ->
    r
  sbvPowerTerm ::
    SBVType t ->
    SBVType t ->
    SBVType t
  sbvPowerTerm = withSbvFloatingTermConstraint @t (**)
  sbvFloatingUnaryTerm ::
    FloatingUnaryOp ->
    SBVType t ->
    SBVType t
  sbvFloatingUnaryTerm op l =
    withSbvFloatingTermConstraint @t $
      case op of
        FloatingExp -> exp l
        FloatingLog -> log l
        FloatingSqrt -> sqrt l
        FloatingSin -> sin l
        FloatingCos -> cos l
        FloatingTan -> tan l
        FloatingAsin -> asin l
        FloatingAcos -> acos l
        FloatingAtan -> atan l
        FloatingSinh -> sinh l
        FloatingCosh -> cosh l
        FloatingTanh -> tanh l
        FloatingAsinh -> asinh l
        FloatingAcosh -> acosh l
        FloatingAtanh -> atanh l

-- | Partial evaluation and lowering for integral terms.
class
  ( SupportedNonFuncPrim a,
    SupportedNonFuncPrim b,
    Integral a,
    Num b
  ) =>
  PEvalFromIntegralTerm a b
  where
  pevalFromIntegralTerm :: Term a -> Term b
  sbvFromIntegralTerm :: SBVType a -> SBVType b

-- | Partial evaluation and lowering for converting from and to IEEE floating
-- point terms.
class (SupportedNonFuncPrim a) => PEvalIEEEFPConvertibleTerm a where
  pevalFromFPOrTerm ::
    (ValidFP eb sb) =>
    Term a ->
    Term FPRoundingMode ->
    Term (FP eb sb) ->
    Term a
  pevalToFPTerm ::
    (ValidFP eb sb) => Term FPRoundingMode -> Term a -> Term (FP eb sb)
  sbvFromFPOrTerm ::
    (ValidFP eb sb) =>
    SBVType a ->
    SBVType FPRoundingMode ->
    SBVType (FP eb sb) ->
    SBVType a
  sbvToFPTerm ::
    (ValidFP eb sb) =>
    SBVType FPRoundingMode ->
    SBVType a ->
    SBVType (FP eb sb)

-- Typed Symbols

-- | The kind of a symbol.
--
-- All symbols are 'AnyKind', and all symbols other than general/tabular
-- functions are 'ConstantKind'.
data SymbolKind = ConstantKind | AnyKind

-- | Decision procedure for symbol kinds.
class IsSymbolKind (ty :: SymbolKind) where
  type SymbolKindConstraint ty :: Type -> Constraint
  decideSymbolKind :: Either (ty :~~: 'ConstantKind) (ty :~~: 'AnyKind)

instance IsSymbolKind 'ConstantKind where
  type SymbolKindConstraint 'ConstantKind = SupportedNonFuncPrim
  decideSymbolKind = Left HRefl

instance IsSymbolKind 'AnyKind where
  type SymbolKindConstraint 'AnyKind = SupportedPrim
  decideSymbolKind = Right HRefl

-- | A typed symbol is a symbol that is associated with a type. Note that the
-- same symbol bodies with different types are considered different symbols
-- and can coexist in a term.
--
-- Simple symbols can be created with the @OverloadedStrings@ extension:
--
-- >>> "a" :: TypedSymbol 'AnyKind Bool
-- a :: Bool
data TypedSymbol (knd :: SymbolKind) t where
  TypedSymbol ::
    ( SupportedPrim t,
      SymbolKindConstraint knd t,
      IsSymbolKind knd
    ) =>
    {unTypedSymbol :: Symbol} ->
    TypedSymbol knd t

-- | Constant symbol
type TypedConstantSymbol = TypedSymbol 'ConstantKind

-- | Any symbol
type TypedAnySymbol = TypedSymbol 'AnyKind

instance Eq (TypedSymbol knd t) where
  TypedSymbol x == TypedSymbol y = x == y

instance Ord (TypedSymbol knd t) where
  TypedSymbol x <= TypedSymbol y = x <= y

instance Lift (TypedSymbol knd t) where
  liftTyped (TypedSymbol x) = [||TypedSymbol x||]

instance Show (TypedSymbol knd t) where
  show (TypedSymbol symbol) = show symbol ++ " :: " ++ show (typeRep @t)

-- | Show a typed symbol without the type information.
showUntyped :: TypedSymbol knd t -> String
showUntyped (TypedSymbol symbol) = show symbol

instance Hashable (TypedSymbol knd t) where
  s `hashWithSalt` TypedSymbol x = s `hashWithSalt` x

instance NFData (TypedSymbol knd t) where
  rnf (TypedSymbol str) = rnf str

instance
  ( SupportedPrim t,
    SymbolKindConstraint knd t,
    IsSymbolKind knd
  ) =>
  IsString (TypedSymbol knd t)
  where
  fromString = TypedSymbol . fromString

-- | Introduce the 'SupportedPrim' constraint from the t'TypedSymbol'.
withSymbolSupported :: TypedSymbol knd t -> ((SupportedPrim t) => a) -> a
withSymbolSupported (TypedSymbol _) a = a
{-# INLINE withSymbolSupported #-}

-- | Introduce the 'IsSymbolKind' constraint from the t'TypedSymbol'.
withSymbolKind :: TypedSymbol knd t -> ((IsSymbolKind knd) => a) -> a
withSymbolKind (TypedSymbol _) a = a
{-# INLINE withSymbolKind #-}

-- | A non-indexed symbol. Type information are checked at runtime.
data SomeTypedSymbol knd where
  SomeTypedSymbol ::
    forall knd t.
    TypeRep t ->
    TypedSymbol knd t ->
    SomeTypedSymbol knd

-- | Non-indexed constant symbol
type SomeTypedConstantSymbol = SomeTypedSymbol 'ConstantKind

-- | Non-indexed any symbol
type SomeTypedAnySymbol = SomeTypedSymbol 'AnyKind

instance NFData (SomeTypedSymbol knd) where
  rnf (SomeTypedSymbol p s) = rnf (SomeTypeRep p) `seq` rnf s
  {-# INLINE rnf #-}

instance Eq (SomeTypedSymbol knd) where
  (SomeTypedSymbol t1 s1) == (SomeTypedSymbol t2 s2) = case eqTypeRep t1 t2 of
    Just HRefl -> s1 == s2
    _ -> False
  {-# INLINE (==) #-}

instance Ord (SomeTypedSymbol knd) where
  (SomeTypedSymbol t1 s1) <= (SomeTypedSymbol t2 s2) =
    SomeTypeRep t1 < SomeTypeRep t2
      || ( case eqTypeRep t1 t2 of
             Just HRefl -> s1 <= s2
             _ -> False
         )

instance Hashable (SomeTypedSymbol knd) where
  hashWithSalt s (SomeTypedSymbol t1 s1) = s `hashWithSalt` s1 `hashWithSalt` t1
  {-# INLINE hashWithSalt #-}

instance Show (SomeTypedSymbol knd) where
  show (SomeTypedSymbol _ s) = show s

-- | Construct a t'SomeTypedSymbol' from a t'TypedSymbol'.
someTypedSymbol :: forall knd t. TypedSymbol knd t -> SomeTypedSymbol knd
someTypedSymbol s@(TypedSymbol _) = SomeTypedSymbol (typeRep @t) s
{-# INLINE someTypedSymbol #-}

-- Terms

-- | Traits for IEEE floating point numbers.
data FPTrait
  = FPIsNaN
  | FPIsPositive
  | FPIsNegative
  | FPIsPositiveInfinite
  | FPIsNegativeInfinite
  | FPIsInfinite
  | FPIsPositiveZero
  | FPIsNegativeZero
  | FPIsZero
  | FPIsNormal
  | FPIsSubnormal
  | FPIsPoint
  deriving (Eq, Ord, Generic, Hashable, Lift, NFData)

instance Show FPTrait where
  show FPIsNaN = "is_nan"
  show FPIsPositive = "is_pos"
  show FPIsNegative = "is_neg"
  show FPIsPositiveInfinite = "is_pos_inf"
  show FPIsNegativeInfinite = "is_neg_inf"
  show FPIsInfinite = "is_inf"
  show FPIsPositiveZero = "is_pos_zero"
  show FPIsNegativeZero = "is_neg_zero"
  show FPIsZero = "is_zero"
  show FPIsNormal = "is_normal"
  show FPIsSubnormal = "is_subnormal"
  show FPIsPoint = "is_point"

-- | Unary floating point operations.
data FPUnaryOp = FPAbs | FPNeg
  deriving (Eq, Ord, Generic, Hashable, Lift, NFData)

instance Show FPUnaryOp where
  show FPAbs = "fp.abs"
  show FPNeg = "fp.neg"

-- | Binary floating point operations.
data FPBinaryOp
  = FPRem
  | FPMinimum
  | FPMinimumNumber
  | FPMaximum
  | FPMaximumNumber
  deriving (Eq, Ord, Generic, Hashable, Lift, NFData)

instance Show FPBinaryOp where
  show FPRem = "fp.rem"
  show FPMinimum = "fp.minimum"
  show FPMinimumNumber = "fp.minimumNumber"
  show FPMaximum = "fp.maximum"
  show FPMaximumNumber = "fp.maximumNumber"

-- | Unary floating point operations with rounding modes.
data FPRoundingUnaryOp = FPSqrt | FPRoundToIntegral
  deriving (Eq, Ord, Generic, Hashable, Lift, NFData)

instance Show FPRoundingUnaryOp where
  show FPSqrt = "fp.sqrt"
  show FPRoundToIntegral = "fp.roundToIntegral"

-- | Binary floating point operations with rounding modes.
data FPRoundingBinaryOp = FPAdd | FPSub | FPMul | FPDiv
  deriving (Eq, Ord, Generic, Hashable, Lift, NFData)

instance Show FPRoundingBinaryOp where
  show FPAdd = "fp.add"
  show FPSub = "fp.sub"
  show FPMul = "fp.mul"
  show FPDiv = "fp.div"

-- | Internal representation for Grisette symbolic terms.
data Term t where
  ConTerm ::
    (SupportedPrim t) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !t ->
    Term t
  SymTerm ::
    (SupportedPrim t) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(TypedSymbol 'AnyKind t) ->
    Term t
  ForallTerm ::
    (SupportedNonFuncPrim t) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(TypedSymbol 'ConstantKind t) ->
    !(Term Bool) ->
    Term Bool
  ExistsTerm ::
    (SupportedNonFuncPrim t) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(TypedSymbol 'ConstantKind t) ->
    !(Term Bool) ->
    Term Bool
  NotTerm ::
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term Bool) ->
    Term Bool
  OrTerm ::
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term Bool) ->
    !(Term Bool) ->
    Term Bool
  AndTerm ::
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term Bool) ->
    !(Term Bool) ->
    Term Bool
  EqTerm ::
    (SupportedNonFuncPrim t) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    !(Term t) ->
    Term Bool
  DistinctTerm ::
    (SupportedNonFuncPrim t) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(NonEmpty (Term t)) ->
    Term Bool
  ITETerm ::
    (SupportedPrim t) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term Bool) ->
    !(Term t) ->
    !(Term t) ->
    Term t
  AddNumTerm ::
    (PEvalNumTerm t) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    !(Term t) ->
    Term t
  NegNumTerm ::
    (PEvalNumTerm t) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    Term t
  MulNumTerm ::
    (PEvalNumTerm t) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    !(Term t) ->
    Term t
  AbsNumTerm ::
    (PEvalNumTerm t) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    Term t
  SignumNumTerm ::
    (PEvalNumTerm t) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    Term t
  LtOrdTerm ::
    (PEvalOrdTerm t) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    !(Term t) ->
    Term Bool
  LeOrdTerm ::
    (PEvalOrdTerm t) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    !(Term t) ->
    Term Bool
  AndBitsTerm ::
    (PEvalBitwiseTerm t) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    !(Term t) ->
    Term t
  OrBitsTerm ::
    (PEvalBitwiseTerm t) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    !(Term t) ->
    Term t
  XorBitsTerm ::
    (PEvalBitwiseTerm t) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    !(Term t) ->
    Term t
  ComplementBitsTerm ::
    (PEvalBitwiseTerm t) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    Term t
  ShiftLeftTerm ::
    (PEvalShiftTerm t) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    !(Term t) ->
    Term t
  ShiftRightTerm ::
    (PEvalShiftTerm t) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    !(Term t) ->
    Term t
  RotateLeftTerm ::
    (PEvalRotateTerm t) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    !(Term t) ->
    Term t
  RotateRightTerm ::
    (PEvalRotateTerm t) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    !(Term t) ->
    Term t
  BitCastTerm ::
    (PEvalBitCastTerm a b) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term a) ->
    Term b
  BitCastOrTerm ::
    (PEvalBitCastOrTerm a b) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term b) ->
    !(Term a) ->
    Term b
  BVConcatTerm ::
    ( PEvalBVTerm bv,
      KnownNat l,
      KnownNat r,
      KnownNat (l + r),
      1 <= l,
      1 <= r,
      1 <= l + r
    ) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term (bv l)) ->
    !(Term (bv r)) ->
    Term (bv (l + r))
  BVSelectTerm ::
    ( PEvalBVTerm bv,
      KnownNat n,
      KnownNat ix,
      KnownNat w,
      1 <= n,
      1 <= w,
      ix + w <= n
    ) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(TypeRep ix) ->
    !(TypeRep w) ->
    !(Term (bv n)) ->
    Term (bv w)
  BVExtendTerm ::
    (PEvalBVTerm bv, KnownNat l, KnownNat r, 1 <= l, 1 <= r, l <= r) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !Bool ->
    !(Proxy r) ->
    !(Term (bv l)) ->
    Term (bv r)
  ApplyTerm ::
    ( SupportedPrim a,
      SupportedPrim b,
      SupportedPrim f,
      PEvalApplyTerm f a b
    ) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term f) ->
    !(Term a) ->
    Term b
  DivIntegralTerm ::
    (PEvalDivModIntegralTerm t) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    !(Term t) ->
    Term t
  ModIntegralTerm ::
    (PEvalDivModIntegralTerm t) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    !(Term t) ->
    Term t
  QuotIntegralTerm ::
    (PEvalDivModIntegralTerm t) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    !(Term t) ->
    Term t
  RemIntegralTerm ::
    (PEvalDivModIntegralTerm t) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    !(Term t) ->
    Term t
  FPTraitTerm ::
    (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !FPTrait ->
    !(Term (FP eb sb)) ->
    Term Bool
  FdivTerm ::
    (PEvalFractionalTerm t) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    !(Term t) ->
    Term t
  RecipTerm ::
    (PEvalFractionalTerm t) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    Term t
  FloatingUnaryTerm ::
    (PEvalFloatingTerm t) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !FloatingUnaryOp ->
    !(Term t) ->
    Term t
  PowerTerm ::
    (PEvalFloatingTerm t) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    !(Term t) ->
    Term t
  FPUnaryTerm ::
    (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !FPUnaryOp ->
    !(Term (FP eb sb)) ->
    Term (FP eb sb)
  FPBinaryTerm ::
    (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !FPBinaryOp ->
    !(Term (FP eb sb)) ->
    !(Term (FP eb sb)) ->
    Term (FP eb sb)
  FPRoundingUnaryTerm ::
    (ValidFP eb sb, SupportedPrim (FP eb sb), SupportedPrim FPRoundingMode) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !FPRoundingUnaryOp ->
    !(Term FPRoundingMode) ->
    !(Term (FP eb sb)) ->
    Term (FP eb sb)
  FPRoundingBinaryTerm ::
    (ValidFP eb sb, SupportedPrim (FP eb sb), SupportedPrim FPRoundingMode) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !FPRoundingBinaryOp ->
    !(Term FPRoundingMode) ->
    !(Term (FP eb sb)) ->
    !(Term (FP eb sb)) ->
    Term (FP eb sb)
  FPFMATerm ::
    (ValidFP eb sb, SupportedPrim (FP eb sb), SupportedPrim FPRoundingMode) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term FPRoundingMode) ->
    !(Term (FP eb sb)) ->
    !(Term (FP eb sb)) ->
    !(Term (FP eb sb)) ->
    Term (FP eb sb)
  FromIntegralTerm ::
    (PEvalFromIntegralTerm a b) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term a) ->
    Term b
  FromFPOrTerm ::
    ( PEvalIEEEFPConvertibleTerm a,
      ValidFP eb sb,
      SupportedPrim (FP eb sb),
      SupportedPrim FPRoundingMode
    ) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term a) ->
    !(Term FPRoundingMode) ->
    !(Term (FP eb sb)) ->
    Term a
  ToFPTerm ::
    ( PEvalIEEEFPConvertibleTerm a,
      ValidFP eb sb,
      SupportedPrim (FP eb sb),
      SupportedPrim FPRoundingMode
    ) =>
    WeakThreadId ->
    {-# UNPACK #-} !Digest ->
    {-# UNPACK #-} !Id ->
    !(Term FPRoundingMode) ->
    !(Term a) ->
    Proxy eb ->
    Proxy sb ->
    Term (FP eb sb)

-- | Return the ID of a term.
identity :: Term t -> Id
identity t = case hashId t of
  HashId _ i -> i
{-# INLINE identity #-}

baseHash :: Term t -> Digest
baseHash t = case hashId t of
  HashId h _ -> h
{-# INLINE baseHash #-}

data HashId = HashId {-# UNPACK #-} !Digest {-# UNPACK #-} !Id

instance Eq HashId where
  HashId _ l == HashId _ r = l == r
  {-# INLINE (==) #-}

instance Hashable HashId where
  hashWithSalt s (HashId i _) = hashWithSalt s i
  {-# INLINE hashWithSalt #-}

eqHashId :: HashId -> HashId -> Bool
eqHashId = (==)
{-# INLINE eqHashId #-}

-- eqTypeHashId :: TypeHashId a -> TypeHashId b -> Bool
-- eqTypeHashId (TypeHashId lrep l) (TypeHashId rrep r) =
--   eqTypeRepBool lrep rrep && l == r
-- {-# INLINE eqTypeHashId #-}

data TypeHashId = TypeHashId !Fingerprint {-# UNPACK #-} !HashId

instance Eq TypeHashId where
  TypeHashId l li == TypeHashId r ri = l == r && li == ri
  {-# INLINE (==) #-}

instance Hashable TypeHashId where
  hashWithSalt s (TypeHashId tp i) = s `hashWithSalt` tp `hashWithSalt` i
  {-# INLINE hashWithSalt #-}

hashId :: Term t -> HashId
hashId t = case typeHashId t of
  TypeHashId _ hi -> hi
{-# INLINE hashId #-}

typeFingerprint :: forall t p. (Typeable t) => p t -> Fingerprint
typeFingerprint p = typeRepFingerprint $ someTypeRep p
{-# INLINE typeFingerprint #-}

-- | Return the ID and the type representation of a term.
typeHashId :: forall t. Term t -> TypeHashId
typeHashId (ConTerm _ ha i _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (SymTerm _ ha i _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (ForallTerm _ ha i _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (ExistsTerm _ ha i _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (NotTerm _ ha i _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (OrTerm _ ha i _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (AndTerm _ ha i _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (EqTerm _ ha i _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (DistinctTerm _ ha i _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (ITETerm _ ha i _ _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (AddNumTerm _ ha i _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (NegNumTerm _ ha i _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (MulNumTerm _ ha i _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (AbsNumTerm _ ha i _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (SignumNumTerm _ ha i _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (LtOrdTerm _ ha i _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (LeOrdTerm _ ha i _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (AndBitsTerm _ ha i _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (OrBitsTerm _ ha i _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (XorBitsTerm _ ha i _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (ComplementBitsTerm _ ha i _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (ShiftLeftTerm _ ha i _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (ShiftRightTerm _ ha i _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (RotateLeftTerm _ ha i _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (RotateRightTerm _ ha i _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (BitCastTerm _ ha i _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (BitCastOrTerm _ ha i _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (BVConcatTerm _ ha i _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (BVSelectTerm _ ha i _ _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (BVExtendTerm _ ha i _ _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (ApplyTerm _ ha i _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (DivIntegralTerm _ ha i _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (ModIntegralTerm _ ha i _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (QuotIntegralTerm _ ha i _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (RemIntegralTerm _ ha i _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (FPTraitTerm _ ha i _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (FdivTerm _ ha i _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (RecipTerm _ ha i _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (FloatingUnaryTerm _ ha i _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (PowerTerm _ ha i _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (FPUnaryTerm _ ha i _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (FPBinaryTerm _ ha i _ _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (FPRoundingUnaryTerm _ ha i _ _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (FPRoundingBinaryTerm _ ha i _ _ _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (FPFMATerm _ ha i _ _ _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (FromIntegralTerm _ ha i _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (FromFPOrTerm _ ha i _ _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
typeHashId (ToFPTerm _ ha i _ _ _ _) = TypeHashId (typeFingerprint (Proxy @t)) $ HashId ha i
{-# INLINE typeHashId #-}

-- | Introduce the 'SupportedPrim' constraint from a term.
introSupportedPrimConstraint :: forall t a. Term t -> ((SupportedPrim t) => a) -> a
introSupportedPrimConstraint ConTerm {} x = x
introSupportedPrimConstraint SymTerm {} x = x
introSupportedPrimConstraint ForallTerm {} x = x
introSupportedPrimConstraint ExistsTerm {} x = x
introSupportedPrimConstraint NotTerm {} x = x
introSupportedPrimConstraint OrTerm {} x = x
introSupportedPrimConstraint AndTerm {} x = x
introSupportedPrimConstraint EqTerm {} x = x
introSupportedPrimConstraint DistinctTerm {} x = x
introSupportedPrimConstraint ITETerm {} x = x
introSupportedPrimConstraint AddNumTerm {} x = x
introSupportedPrimConstraint NegNumTerm {} x = x
introSupportedPrimConstraint MulNumTerm {} x = x
introSupportedPrimConstraint AbsNumTerm {} x = x
introSupportedPrimConstraint SignumNumTerm {} x = x
introSupportedPrimConstraint LtOrdTerm {} x = x
introSupportedPrimConstraint LeOrdTerm {} x = x
introSupportedPrimConstraint AndBitsTerm {} x = x
introSupportedPrimConstraint OrBitsTerm {} x = x
introSupportedPrimConstraint XorBitsTerm {} x = x
introSupportedPrimConstraint ComplementBitsTerm {} x = x
introSupportedPrimConstraint ShiftLeftTerm {} x = x
introSupportedPrimConstraint RotateLeftTerm {} x = x
introSupportedPrimConstraint ShiftRightTerm {} x = x
introSupportedPrimConstraint RotateRightTerm {} x = x
introSupportedPrimConstraint BitCastTerm {} x = x
introSupportedPrimConstraint BitCastOrTerm {} x = x
introSupportedPrimConstraint BVConcatTerm {} x = x
introSupportedPrimConstraint BVSelectTerm {} x = x
introSupportedPrimConstraint BVExtendTerm {} x = x
introSupportedPrimConstraint ApplyTerm {} x = x
introSupportedPrimConstraint DivIntegralTerm {} x = x
introSupportedPrimConstraint ModIntegralTerm {} x = x
introSupportedPrimConstraint QuotIntegralTerm {} x = x
introSupportedPrimConstraint RemIntegralTerm {} x = x
introSupportedPrimConstraint FPTraitTerm {} x = x
introSupportedPrimConstraint FdivTerm {} x = x
introSupportedPrimConstraint RecipTerm {} x = x
introSupportedPrimConstraint FloatingUnaryTerm {} x = x
introSupportedPrimConstraint PowerTerm {} x = x
introSupportedPrimConstraint FPUnaryTerm {} x = x
introSupportedPrimConstraint FPBinaryTerm {} x = x
introSupportedPrimConstraint FPRoundingUnaryTerm {} x = x
introSupportedPrimConstraint FPRoundingBinaryTerm {} x = x
introSupportedPrimConstraint FPFMATerm {} x = x
introSupportedPrimConstraint FromIntegralTerm {} x = x
introSupportedPrimConstraint FromFPOrTerm {} x = x
introSupportedPrimConstraint ToFPTerm {} x = x
{-# INLINE introSupportedPrimConstraint #-}

-- | Pretty-print a term.
pformatTerm :: forall t. (SupportedPrim t) => Term t -> String
pformatTerm (ConTerm _ _ _ t) = pformatCon t
pformatTerm (SymTerm _ _ _ sym) = pformatSym sym
pformatTerm (ForallTerm _ _ _ sym arg) = "(forall " ++ show sym ++ " " ++ pformatTerm arg ++ ")"
pformatTerm (ExistsTerm _ _ _ sym arg) = "(exists " ++ show sym ++ " " ++ pformatTerm arg ++ ")"
pformatTerm (NotTerm _ _ _ arg) = "(! " ++ pformatTerm arg ++ ")"
pformatTerm (OrTerm _ _ _ arg1 arg2) = "(|| " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (AndTerm _ _ _ arg1 arg2) = "(&& " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (EqTerm _ _ _ arg1 arg2) = "(= " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (DistinctTerm _ _ _ args) = "(distinct " ++ unwords (map pformatTerm $ toList args) ++ ")"
pformatTerm (ITETerm _ _ _ cond arg1 arg2) = "(ite " ++ pformatTerm cond ++ " " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (AddNumTerm _ _ _ arg1 arg2) = "(+ " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (NegNumTerm _ _ _ arg) = "(- " ++ pformatTerm arg ++ ")"
pformatTerm (MulNumTerm _ _ _ arg1 arg2) = "(* " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (AbsNumTerm _ _ _ arg) = "(abs " ++ pformatTerm arg ++ ")"
pformatTerm (SignumNumTerm _ _ _ arg) = "(signum " ++ pformatTerm arg ++ ")"
pformatTerm (LtOrdTerm _ _ _ arg1 arg2) = "(< " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (LeOrdTerm _ _ _ arg1 arg2) = "(<= " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (AndBitsTerm _ _ _ arg1 arg2) = "(& " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (OrBitsTerm _ _ _ arg1 arg2) = "(| " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (XorBitsTerm _ _ _ arg1 arg2) = "(^ " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (ComplementBitsTerm _ _ _ arg) = "(~ " ++ pformatTerm arg ++ ")"
pformatTerm (ShiftLeftTerm _ _ _ arg n) = "(shl " ++ pformatTerm arg ++ " " ++ pformatTerm n ++ ")"
pformatTerm (ShiftRightTerm _ _ _ arg n) = "(shr " ++ pformatTerm arg ++ " " ++ pformatTerm n ++ ")"
pformatTerm (RotateLeftTerm _ _ _ arg n) = "(rotl " ++ pformatTerm arg ++ " " ++ pformatTerm n ++ ")"
pformatTerm (RotateRightTerm _ _ _ arg n) = "(rotr " ++ pformatTerm arg ++ " " ++ pformatTerm n ++ ")"
pformatTerm (BitCastTerm _ _ _ arg) = "(bitcast " ++ pformatTerm arg ++ ")"
pformatTerm (BitCastOrTerm _ _ _ d arg) = "(bitcast_or " ++ pformatTerm d ++ " " ++ pformatTerm arg ++ ")"
pformatTerm (BVConcatTerm _ _ _ arg1 arg2) = "(bvconcat " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (BVSelectTerm _ _ _ ix w arg) = "(bvselect " ++ show ix ++ " " ++ show w ++ " " ++ pformatTerm arg ++ ")"
pformatTerm (BVExtendTerm _ _ _ signed n arg) =
  (if signed then "(bvsext " else "(bvzext ") ++ show n ++ " " ++ pformatTerm arg ++ ")"
pformatTerm (ApplyTerm _ _ _ func arg) = "(apply " ++ pformatTerm func ++ " " ++ pformatTerm arg ++ ")"
pformatTerm (DivIntegralTerm _ _ _ arg1 arg2) = "(div " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (ModIntegralTerm _ _ _ arg1 arg2) = "(mod " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (QuotIntegralTerm _ _ _ arg1 arg2) = "(quot " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (RemIntegralTerm _ _ _ arg1 arg2) = "(rem " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (FPTraitTerm _ _ _ trait arg) = "(" ++ show trait ++ " " ++ pformatTerm arg ++ ")"
pformatTerm (FdivTerm _ _ _ arg1 arg2) = "(fdiv " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (RecipTerm _ _ _ arg) = "(recip " ++ pformatTerm arg ++ ")"
pformatTerm (FloatingUnaryTerm _ _ _ op arg) = "(" ++ show op ++ " " ++ pformatTerm arg ++ ")"
pformatTerm (PowerTerm _ _ _ arg1 arg2) = "(** " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (FPUnaryTerm _ _ _ op arg) = "(" ++ show op ++ " " ++ pformatTerm arg ++ ")"
pformatTerm (FPBinaryTerm _ _ _ op arg1 arg2) = "(" ++ show op ++ " " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (FPRoundingUnaryTerm _ _ _ op mode arg) = "(" ++ show op ++ " " ++ pformatTerm mode ++ " " ++ pformatTerm arg ++ ")"
pformatTerm (FPRoundingBinaryTerm _ _ _ op mode arg1 arg2) =
  "(" ++ show op ++ " " ++ pformatTerm mode ++ " " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (FPFMATerm _ _ _ mode arg1 arg2 arg3) =
  "(fp.fma " ++ pformatTerm mode ++ " " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ " " ++ pformatTerm arg3 ++ ")"
pformatTerm (FromIntegralTerm _ _ _ arg) = "(from_integral " ++ pformatTerm arg ++ ")"
pformatTerm (FromFPOrTerm _ _ _ d r arg) = "(from_fp_or " ++ pformatTerm d ++ " " ++ pformatTerm r ++ " " ++ pformatTerm arg ++ ")"
pformatTerm (ToFPTerm _ _ _ r arg _ _) = "(to_fp " ++ pformatTerm r ++ " " ++ pformatTerm arg ++ ")"
{-# INLINE pformatTerm #-}

instance NFData (Term a) where
  rnf i = identity i `seq` ()
  {-# INLINE rnf #-}

instance Lift (Term t) where
  liftTyped t =
    [||
    introSupportedPrimConstraint t $
      unsafePerformIO $
        fullReconstructTerm t
    ||]

instance Show (Term ty) where
  show (ConTerm tid _ i v) =
    "ConTerm{tid=" ++ show tid ++ ", id=" ++ show i ++ ", v=" ++ show v ++ "}"
  show (SymTerm tid _ i name) =
    "SymTerm{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", name="
      ++ show name
      ++ ", type="
      ++ show (typeRep @ty)
      ++ "}"
  show (ForallTerm tid _ i sym arg) =
    "Forall{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", sym="
      ++ show sym
      ++ ", arg="
      ++ show arg
      ++ "}"
  show (ExistsTerm tid _ i sym arg) =
    "Exists{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", sym="
      ++ show sym
      ++ ", arg="
      ++ show arg
      ++ "}"
  show (NotTerm tid _ i arg) =
    "Not{tid=" ++ show tid ++ ", id=" ++ show i ++ ", arg=" ++ show arg ++ "}"
  show (OrTerm tid _ i arg1 arg2) =
    "Or{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show (AndTerm tid _ i arg1 arg2) =
    "And{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show (EqTerm tid _ i arg1 arg2) =
    "Eqv{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show (DistinctTerm tid _ i args) =
    "Distinct{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", args="
      ++ show args
      ++ "}"
  show (ITETerm tid _ i cond l r) =
    "ITE{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", cond="
      ++ show cond
      ++ ", then="
      ++ show l
      ++ ", else="
      ++ show r
      ++ "}"
  show (AddNumTerm tid _ i arg1 arg2) =
    "AddNum{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show (NegNumTerm tid _ i arg) =
    "NegNum{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", arg="
      ++ show arg
      ++ "}"
  show (MulNumTerm tid _ i arg1 arg2) =
    "MulNum{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show (AbsNumTerm tid _ i arg) =
    "AbsNum{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", arg="
      ++ show arg
      ++ "}"
  show (SignumNumTerm tid _ i arg) =
    "SignumNum{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", arg="
      ++ show arg
      ++ "}"
  show (LtOrdTerm tid _ i arg1 arg2) =
    "LTNum{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show (LeOrdTerm tid _ i arg1 arg2) =
    "LENum{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show (AndBitsTerm tid _ i arg1 arg2) =
    "AndBits{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show (OrBitsTerm tid _ i arg1 arg2) =
    "OrBits{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show (XorBitsTerm tid _ i arg1 arg2) =
    "XorBits{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show (ComplementBitsTerm tid _ i arg) =
    "ComplementBits{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", arg="
      ++ show arg
      ++ "}"
  show (ShiftLeftTerm tid _ i arg n) =
    "ShiftLeft{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", arg="
      ++ show arg
      ++ ", n="
      ++ show n
      ++ "}"
  show (ShiftRightTerm tid _ i arg n) =
    "ShiftRight{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", arg="
      ++ show arg
      ++ ", n="
      ++ show n
      ++ "}"
  show (RotateLeftTerm tid _ i arg n) =
    "RotateLeft{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", arg="
      ++ show arg
      ++ ", n="
      ++ show n
      ++ "}"
  show (RotateRightTerm tid _ i arg n) =
    "RotateRight{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", arg="
      ++ show arg
      ++ ", n="
      ++ show n
      ++ "}"
  show (BitCastTerm tid _ i arg) =
    "BitCast{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", arg="
      ++ show arg
      ++ "}"
  show (BitCastOrTerm tid _ i d arg) =
    "BitCastOr{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", default="
      ++ show d
      ++ ", arg="
      ++ show arg
      ++ "}"
  show (BVConcatTerm tid _ i arg1 arg2) =
    "BVConcat{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show (BVSelectTerm tid _ i ix w arg) =
    "BVSelect{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", ix="
      ++ show ix
      ++ ", w="
      ++ show w
      ++ ", arg="
      ++ show arg
      ++ "}"
  show (BVExtendTerm tid _ i signed n arg) =
    "BVExtend{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", signed="
      ++ show signed
      ++ ", n="
      ++ show n
      ++ ", arg="
      ++ show arg
      ++ "}"
  show (ApplyTerm tid _ i f arg) =
    "Apply{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", f="
      ++ show f
      ++ ", arg="
      ++ show arg
      ++ "}"
  show (DivIntegralTerm tid _ i arg1 arg2) =
    "DivIntegral{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show (ModIntegralTerm tid _ i arg1 arg2) =
    "ModIntegral{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show (QuotIntegralTerm tid _ i arg1 arg2) =
    "QuotIntegral{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show (RemIntegralTerm tid _ i arg1 arg2) =
    "RemIntegral{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show (FPTraitTerm tid _ i trait arg) =
    "FPTrait{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", trait="
      ++ show trait
      ++ ", arg="
      ++ show arg
      ++ "}"
  show (FdivTerm tid _ i arg1 arg2) =
    "Fdiv{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show (RecipTerm tid _ i arg) =
    "Recip{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", arg="
      ++ show arg
      ++ "}"
  show (FloatingUnaryTerm tid _ i op arg) =
    "FloatingUnary{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", op="
      ++ show op
      ++ ", arg="
      ++ show arg
      ++ "}"
  show (PowerTerm tid _ i arg1 arg2) =
    "Power{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show (FPUnaryTerm tid _ i op arg) =
    "FPUnary{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", op="
      ++ show op
      ++ ", arg="
      ++ show arg
      ++ "}"
  show (FPBinaryTerm tid _ i op arg1 arg2) =
    "FPBinary{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", op="
      ++ show op
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show (FPRoundingUnaryTerm tid _ i op mode arg) =
    "FPRoundingUnary{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", op="
      ++ show op
      ++ ", mode="
      ++ show mode
      ++ ", arg="
      ++ show arg
      ++ "}"
  show (FPRoundingBinaryTerm tid _ i op mode arg1 arg2) =
    "FPRoundingBinary{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", op="
      ++ show op
      ++ ", mode="
      ++ show mode
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show (FPFMATerm tid _ i mode arg1 arg2 arg3) =
    "FPFMA{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", mode="
      ++ show mode
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ ", arg3="
      ++ show arg3
      ++ "}"
  show (FromIntegralTerm tid _ i arg) =
    "FromIntegral{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", arg="
      ++ show arg
      ++ "}"
  show (FromFPOrTerm tid _ i d mode arg) =
    "FromFPTerm{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", default="
      ++ show d
      ++ ", mode="
      ++ show mode
      ++ ", arg="
      ++ show arg
      ++ "}"
  show (ToFPTerm tid _ i mode arg _ _) =
    "ToFPTerm{tid="
      ++ show tid
      ++ ", id="
      ++ show i
      ++ ", mode="
      ++ show mode
      ++ ", arg="
      ++ show arg
      ++ "}"
  {-# INLINE show #-}

-- | Pretty-print a term, possibly eliding parts of it.
prettyPrintTerm :: Term t -> Doc ann
prettyPrintTerm v =
  column
    ( \c ->
        pageWidth $ \case
          AvailablePerLine i r ->
            if fromIntegral (c + len) > fromIntegral i * r
              then "..."
              else pretty formatted
          Unbounded -> pretty formatted
    )
  where
    formatted = introSupportedPrimConstraint v $ pformatTerm v
    len = length formatted

instance (SupportedPrim t) => Eq (Term t) where
  a == b =
    if threadId a == threadId b
      then identity a == identity b
      else unsafePerformIO $ do
        a' <- fullReconstructTerm a
        b' <- fullReconstructTerm b
        return $ identity a' == identity b'

instance (SupportedPrim t) => Hashable (Term t) where
  hashWithSalt s t = hashWithSalt s $ baseHash t
  {-# INLINE hashWithSalt #-}

-- | Term without identity (before internalizing).
data UTerm t where
  UConTerm :: (SupportedPrim t) => !t -> UTerm t
  USymTerm :: (SupportedPrim t) => !(TypedSymbol 'AnyKind t) -> UTerm t
  UForallTerm :: (SupportedNonFuncPrim t) => !(TypedSymbol 'ConstantKind t) -> !(Term Bool) -> UTerm Bool
  UExistsTerm :: (SupportedNonFuncPrim t) => !(TypedSymbol 'ConstantKind t) -> !(Term Bool) -> UTerm Bool
  UNotTerm :: !(Term Bool) -> UTerm Bool
  UOrTerm :: !(Term Bool) -> !(Term Bool) -> UTerm Bool
  UAndTerm :: !(Term Bool) -> !(Term Bool) -> UTerm Bool
  UEqTerm :: (SupportedNonFuncPrim t) => !(Term t) -> !(Term t) -> UTerm Bool
  UDistinctTerm :: (SupportedNonFuncPrim t) => !(NonEmpty (Term t)) -> UTerm Bool
  UITETerm ::
    (SupportedPrim t) =>
    !(Term Bool) ->
    !(Term t) ->
    !(Term t) ->
    UTerm t
  UAddNumTerm :: (PEvalNumTerm t) => !(Term t) -> !(Term t) -> UTerm t
  UNegNumTerm :: (PEvalNumTerm t) => !(Term t) -> UTerm t
  UMulNumTerm :: (PEvalNumTerm t) => !(Term t) -> !(Term t) -> UTerm t
  UAbsNumTerm :: (PEvalNumTerm t) => !(Term t) -> UTerm t
  USignumNumTerm :: (PEvalNumTerm t) => !(Term t) -> UTerm t
  ULtOrdTerm :: (PEvalOrdTerm t) => !(Term t) -> !(Term t) -> UTerm Bool
  ULeOrdTerm :: (PEvalOrdTerm t) => !(Term t) -> !(Term t) -> UTerm Bool
  UAndBitsTerm :: (PEvalBitwiseTerm t) => !(Term t) -> !(Term t) -> UTerm t
  UOrBitsTerm :: (PEvalBitwiseTerm t) => !(Term t) -> !(Term t) -> UTerm t
  UXorBitsTerm :: (PEvalBitwiseTerm t) => !(Term t) -> !(Term t) -> UTerm t
  UComplementBitsTerm :: (PEvalBitwiseTerm t) => !(Term t) -> UTerm t
  UShiftLeftTerm :: (PEvalShiftTerm t) => !(Term t) -> !(Term t) -> UTerm t
  UShiftRightTerm :: (PEvalShiftTerm t) => !(Term t) -> !(Term t) -> UTerm t
  URotateLeftTerm :: (PEvalRotateTerm t) => !(Term t) -> !(Term t) -> UTerm t
  URotateRightTerm :: (PEvalRotateTerm t) => !(Term t) -> !(Term t) -> UTerm t
  UBitCastTerm ::
    (PEvalBitCastTerm a b) =>
    !(Term a) ->
    UTerm b
  UBitCastOrTerm ::
    (PEvalBitCastOrTerm a b) =>
    !(Term b) ->
    !(Term a) ->
    UTerm b
  UBVConcatTerm ::
    ( PEvalBVTerm bv,
      KnownNat l,
      KnownNat r,
      KnownNat (l + r),
      1 <= l,
      1 <= r,
      1 <= l + r
    ) =>
    !(Term (bv l)) ->
    !(Term (bv r)) ->
    UTerm (bv (l + r))
  UBVSelectTerm ::
    ( PEvalBVTerm bv,
      KnownNat n,
      KnownNat ix,
      KnownNat w,
      1 <= n,
      1 <= w,
      ix + w <= n
    ) =>
    !(TypeRep ix) ->
    !(TypeRep w) ->
    !(Term (bv n)) ->
    UTerm (bv w)
  UBVExtendTerm ::
    (PEvalBVTerm bv, KnownNat l, KnownNat r, 1 <= l, 1 <= r, l <= r) =>
    !Bool ->
    !(Proxy r) ->
    !(Term (bv l)) ->
    UTerm (bv r)
  UApplyTerm ::
    ( SupportedPrim a,
      SupportedPrim b,
      SupportedPrim f,
      PEvalApplyTerm f a b
    ) =>
    Term f ->
    Term a ->
    UTerm b
  UDivIntegralTerm ::
    (PEvalDivModIntegralTerm t) => !(Term t) -> !(Term t) -> UTerm t
  UModIntegralTerm ::
    (PEvalDivModIntegralTerm t) => !(Term t) -> !(Term t) -> UTerm t
  UQuotIntegralTerm ::
    (PEvalDivModIntegralTerm t) => !(Term t) -> !(Term t) -> UTerm t
  URemIntegralTerm ::
    (PEvalDivModIntegralTerm t) => !(Term t) -> !(Term t) -> UTerm t
  UFPTraitTerm ::
    (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
    !FPTrait ->
    !(Term (FP eb sb)) ->
    UTerm Bool
  UFdivTerm :: (PEvalFractionalTerm t) => !(Term t) -> !(Term t) -> UTerm t
  URecipTerm :: (PEvalFractionalTerm t) => !(Term t) -> UTerm t
  UFloatingUnaryTerm :: (PEvalFloatingTerm t) => !FloatingUnaryOp -> !(Term t) -> UTerm t
  UPowerTerm :: (PEvalFloatingTerm t) => !(Term t) -> !(Term t) -> UTerm t
  UFPUnaryTerm ::
    (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
    !FPUnaryOp ->
    !(Term (FP eb sb)) ->
    UTerm (FP eb sb)
  UFPBinaryTerm ::
    (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
    !FPBinaryOp ->
    !(Term (FP eb sb)) ->
    !(Term (FP eb sb)) ->
    UTerm (FP eb sb)
  UFPRoundingUnaryTerm ::
    (ValidFP eb sb, SupportedPrim (FP eb sb), SupportedPrim FPRoundingMode) =>
    !FPRoundingUnaryOp ->
    !(Term FPRoundingMode) ->
    !(Term (FP eb sb)) ->
    UTerm (FP eb sb)
  UFPRoundingBinaryTerm ::
    (ValidFP eb sb, SupportedPrim (FP eb sb), SupportedPrim FPRoundingMode) =>
    !FPRoundingBinaryOp ->
    !(Term FPRoundingMode) ->
    !(Term (FP eb sb)) ->
    !(Term (FP eb sb)) ->
    UTerm (FP eb sb)
  UFPFMATerm ::
    (ValidFP eb sb, SupportedPrim (FP eb sb), SupportedPrim FPRoundingMode) =>
    !(Term FPRoundingMode) ->
    !(Term (FP eb sb)) ->
    !(Term (FP eb sb)) ->
    !(Term (FP eb sb)) ->
    UTerm (FP eb sb)
  UFromIntegralTerm ::
    (PEvalFromIntegralTerm a b) =>
    !(Term a) ->
    UTerm b
  UFromFPOrTerm ::
    ( PEvalIEEEFPConvertibleTerm a,
      ValidFP eb sb,
      SupportedPrim FPRoundingMode,
      SupportedPrim (FP eb sb)
    ) =>
    Term a ->
    !(Term FPRoundingMode) ->
    !(Term (FP eb sb)) ->
    UTerm a
  UToFPTerm ::
    ( PEvalIEEEFPConvertibleTerm a,
      ValidFP eb sb,
      SupportedPrim FPRoundingMode,
      SupportedPrim (FP eb sb)
    ) =>
    !(Term FPRoundingMode) ->
    !(Term a) ->
    Proxy eb ->
    Proxy sb ->
    UTerm (FP eb sb)

-- | Compare two t'TypedSymbol's for equality.
eqHeteroSymbol :: forall ta a tb b. TypedSymbol ta a -> TypedSymbol tb b -> Bool
eqHeteroSymbol (TypedSymbol taga) (TypedSymbol tagb) =
  case eqTypeRep (typeRep @a) (typeRep @b) of
    Just HRefl -> taga == tagb
    Nothing -> False
{-# INLINE eqHeteroSymbol #-}

preHashConDescription :: (Hashable t) => t -> Digest
preHashConDescription = fromIntegral . hashWithSalt 0
{-# INLINE preHashConDescription #-}

preHashSymDescription :: (Hashable t) => TypedSymbol 'AnyKind t -> Digest
preHashSymDescription = fromIntegral . hashWithSalt 1
{-# INLINE preHashSymDescription #-}

preHashForallDescription ::
  (Hashable t) => TypedSymbol 'ConstantKind t -> HashId -> Digest
preHashForallDescription sym h =
  fromIntegral
    ( 2
        `hashWithSalt` sym
        `hashWithSalt` h
    )
{-# INLINE preHashForallDescription #-}

preHashExistsDescription ::
  (Hashable t) => TypedSymbol 'ConstantKind t -> HashId -> Digest
preHashExistsDescription sym h =
  fromIntegral
    ( 3
        `hashWithSalt` sym
        `hashWithSalt` h
    )
{-# INLINE preHashExistsDescription #-}

preHashNotDescription :: HashId -> Digest
preHashNotDescription = fromIntegral . hashWithSalt 7
{-# INLINE preHashNotDescription #-}

preHashOrDescription :: HashId -> HashId -> Digest
preHashOrDescription h1 h2 =
  fromIntegral (8 `hashWithSalt` h1 `hashWithSalt` h2)
{-# INLINE preHashOrDescription #-}

preHashAndDescription :: HashId -> HashId -> Digest
preHashAndDescription h1 h2 =
  fromIntegral (9 `hashWithSalt` h1 `hashWithSalt` h2)
{-# INLINE preHashAndDescription #-}

preHashEqDescription :: Fingerprint -> HashId -> HashId -> Digest
preHashEqDescription tp h1 h2 =
  fromIntegral (10 `hashWithSalt` tp `hashWithSalt` h1 `hashWithSalt` h2)
{-# INLINE preHashEqDescription #-}

preHashDistinctDescription :: Fingerprint -> NonEmpty HashId -> Digest
preHashDistinctDescription tp hs =
  fromIntegral (11 `hashWithSalt` tp `hashWithSalt` hs)
{-# INLINE preHashDistinctDescription #-}

preHashITEDescription :: HashId -> HashId -> HashId -> Digest
preHashITEDescription h1 h2 h3 =
  fromIntegral (12 `hashWithSalt` h1 `hashWithSalt` h2 `hashWithSalt` h3)
{-# INLINE preHashITEDescription #-}

preHashAddNumDescription :: HashId -> HashId -> Digest
preHashAddNumDescription h1 h2 =
  fromIntegral (13 `hashWithSalt` h1 `hashWithSalt` h2)
{-# INLINE preHashAddNumDescription #-}

preHashNegNumDescription :: HashId -> Digest
preHashNegNumDescription =
  fromIntegral . hashWithSalt 14
{-# INLINE preHashNegNumDescription #-}

preHashMulNumDescription :: HashId -> HashId -> Digest
preHashMulNumDescription h1 h2 =
  fromIntegral (15 `hashWithSalt` h1 `hashWithSalt` h2)
{-# INLINE preHashMulNumDescription #-}

preHashAbsNumDescription :: HashId -> Digest
preHashAbsNumDescription = fromIntegral . hashWithSalt 16
{-# INLINE preHashAbsNumDescription #-}

preHashSignumNumDescription :: HashId -> Digest
preHashSignumNumDescription = fromIntegral . hashWithSalt 17
{-# INLINE preHashSignumNumDescription #-}

preHashLtOrdDescription :: TypeRep t -> HashId -> HashId -> Digest
preHashLtOrdDescription tp h1 h2 =
  fromIntegral (18 `hashWithSalt` tp `hashWithSalt` h1 `hashWithSalt` h2)
{-# INLINE preHashLtOrdDescription #-}

preHashLeOrdDescription :: TypeRep t -> HashId -> HashId -> Digest
preHashLeOrdDescription tp h1 h2 =
  fromIntegral (19 `hashWithSalt` tp `hashWithSalt` h1 `hashWithSalt` h2)
{-# INLINE preHashLeOrdDescription #-}

preHashAndBitsDescription :: HashId -> HashId -> Digest
preHashAndBitsDescription h1 h2 =
  fromIntegral (20 `hashWithSalt` h1 `hashWithSalt` h2)
{-# INLINE preHashAndBitsDescription #-}

preHashOrBitsDescription :: HashId -> HashId -> Digest
preHashOrBitsDescription h1 h2 =
  fromIntegral (21 `hashWithSalt` h1 `hashWithSalt` h2)
{-# INLINE preHashOrBitsDescription #-}

preHashXorBitsDescription :: HashId -> HashId -> Digest
preHashXorBitsDescription h1 h2 =
  fromIntegral (22 `hashWithSalt` h1 `hashWithSalt` h2)
{-# INLINE preHashXorBitsDescription #-}

preHashComplementBitsDescription :: HashId -> Digest
preHashComplementBitsDescription = fromIntegral . hashWithSalt 23
{-# INLINE preHashComplementBitsDescription #-}

preHashShiftLeftDescription :: HashId -> HashId -> Digest
preHashShiftLeftDescription h1 h2 =
  fromIntegral (24 `hashWithSalt` h1 `hashWithSalt` h2)
{-# INLINE preHashShiftLeftDescription #-}

preHashShiftRightDescription :: HashId -> HashId -> Digest
preHashShiftRightDescription h1 h2 =
  fromIntegral (25 `hashWithSalt` h1 `hashWithSalt` h2)
{-# INLINE preHashShiftRightDescription #-}

preHashRotateLeftDescription :: HashId -> HashId -> Digest
preHashRotateLeftDescription h1 h2 =
  fromIntegral (26 `hashWithSalt` h1 `hashWithSalt` h2)
{-# INLINE preHashRotateLeftDescription #-}

preHashRotateRightDescription :: HashId -> HashId -> Digest
preHashRotateRightDescription h1 h2 =
  fromIntegral (27 `hashWithSalt` h1 `hashWithSalt` h2)
{-# INLINE preHashRotateRightDescription #-}

preHashBVConcatDescription :: TypeHashId -> TypeHashId -> Digest
preHashBVConcatDescription h1 h2 =
  fromIntegral
    ( 28
        `hashWithSalt` h1
        `hashWithSalt` h2
    )

preHashBVSelectDescription :: Fingerprint -> TypeHashId -> Digest
preHashBVSelectDescription tp h =
  fromIntegral (29 `hashWithSalt` tp `hashWithSalt` h)

preHashBVExtendDescription :: Bool -> TypeHashId -> Digest
preHashBVExtendDescription signed h =
  fromIntegral (30 `hashWithSalt` signed `hashWithSalt` h)

preHashBitCastDescription :: TypeHashId -> Digest
preHashBitCastDescription = fromIntegral . hashWithSalt 31
{-# INLINE preHashBitCastDescription #-}

preHashBitCastOrDescription :: HashId -> TypeHashId -> Digest
preHashBitCastOrDescription h1 h2 =
  fromIntegral (32 `hashWithSalt` h1 `hashWithSalt` h2)
{-# INLINE preHashBitCastOrDescription #-}

preHashApplyDescription :: TypeHashId -> TypeHashId -> Digest
preHashApplyDescription h1 h2 =
  fromIntegral (33 `hashWithSalt` h1 `hashWithSalt` h2)
{-# INLINE preHashApplyDescription #-}

preHashDivIntegralDescription :: HashId -> HashId -> Digest
preHashDivIntegralDescription h1 h2 =
  fromIntegral (34 `hashWithSalt` h1 `hashWithSalt` h2)
{-# INLINE preHashDivIntegralDescription #-}

preHashModIntegralDescription :: HashId -> HashId -> Digest
preHashModIntegralDescription h1 h2 =
  fromIntegral (35 `hashWithSalt` h1 `hashWithSalt` h2)
{-# INLINE preHashModIntegralDescription #-}

preHashQuotIntegralDescription :: HashId -> HashId -> Digest
preHashQuotIntegralDescription h1 h2 =
  fromIntegral (36 `hashWithSalt` h1 `hashWithSalt` h2)
{-# INLINE preHashQuotIntegralDescription #-}

preHashRemIntegralDescription :: HashId -> HashId -> Digest
preHashRemIntegralDescription h1 h2 =
  fromIntegral (37 `hashWithSalt` h1 `hashWithSalt` h2)
{-# INLINE preHashRemIntegralDescription #-}

preHashFPTraitDescription :: FPTrait -> TypeHashId -> Digest
preHashFPTraitDescription trait h =
  fromIntegral (38 `hashWithSalt` trait `hashWithSalt` h)
{-# INLINE preHashFPTraitDescription #-}

preHashFdivDescription :: HashId -> HashId -> Digest
preHashFdivDescription h1 h2 =
  fromIntegral (39 `hashWithSalt` h1 `hashWithSalt` h2)
{-# INLINE preHashFdivDescription #-}

preHashRecipDescription :: HashId -> Digest
preHashRecipDescription = fromIntegral . hashWithSalt 40
{-# INLINE preHashRecipDescription #-}

preHashFloatingUnaryDescription :: FloatingUnaryOp -> HashId -> Digest
preHashFloatingUnaryDescription op h =
  fromIntegral (41 `hashWithSalt` op `hashWithSalt` h)
{-# INLINE preHashFloatingUnaryDescription #-}

preHashPowerDescription :: HashId -> HashId -> Digest
preHashPowerDescription h1 h2 =
  fromIntegral (42 `hashWithSalt` h1 `hashWithSalt` h2)
{-# INLINE preHashPowerDescription #-}

preHashFPUnaryDescription :: FPUnaryOp -> HashId -> Digest
preHashFPUnaryDescription op h =
  fromIntegral (43 `hashWithSalt` op `hashWithSalt` h)
{-# INLINE preHashFPUnaryDescription #-}

preHashFPBinaryDescription :: FPBinaryOp -> HashId -> HashId -> Digest
preHashFPBinaryDescription op h1 h2 =
  fromIntegral (44 `hashWithSalt` op `hashWithSalt` h1 `hashWithSalt` h2)
{-# INLINE preHashFPBinaryDescription #-}

preHashFPRoundingUnaryDescription ::
  FPRoundingUnaryOp -> HashId -> HashId -> Digest
preHashFPRoundingUnaryDescription op mode h =
  fromIntegral (45 `hashWithSalt` op `hashWithSalt` mode `hashWithSalt` h)
{-# INLINE preHashFPRoundingUnaryDescription #-}

preHashFPRoundingBinaryDescription ::
  FPRoundingBinaryOp -> HashId -> HashId -> HashId -> Digest
preHashFPRoundingBinaryDescription op mode h1 h2 =
  fromIntegral
    ( 46
        `hashWithSalt` op
        `hashWithSalt` mode
        `hashWithSalt` h1
        `hashWithSalt` h2
    )

preHashFPFMADescription ::
  HashId -> HashId -> HashId -> HashId -> Digest
preHashFPFMADescription mode h1 h2 h3 =
  fromIntegral
    ( 47
        `hashWithSalt` mode
        `hashWithSalt` h1
        `hashWithSalt` h2
        `hashWithSalt` h3
    )
{-# INLINE preHashFPFMADescription #-}

preHashFromIntegralDescription :: TypeHashId -> Digest
preHashFromIntegralDescription = fromIntegral . hashWithSalt 48
{-# INLINE preHashFromIntegralDescription #-}

preHashFromFPOrDescription ::
  HashId -> HashId -> TypeHashId -> Digest
preHashFromFPOrDescription h1 h2 h3 =
  fromIntegral (49 `hashWithSalt` h1 `hashWithSalt` h2 `hashWithSalt` h3)
{-# INLINE preHashFromFPOrDescription #-}

preHashToFPTermDescription :: HashId -> TypeHashId -> Digest
preHashToFPTermDescription h1 h2 =
  fromIntegral (50 `hashWithSalt` h1 `hashWithSalt` h2)
{-# INLINE preHashToFPTermDescription #-}

instance (SupportedPrim t) => Interned (Term t) where
  type Uninterned (Term t) = UTerm t
  data Description (Term t) where
    DConTerm :: {-# UNPACK #-} !Digest -> t -> Description (Term t)
    DSymTerm ::
      {-# UNPACK #-} !Digest ->
      TypedSymbol 'AnyKind t ->
      Description (Term t)
    DForallTerm ::
      {-# UNPACK #-} !Digest ->
      {-# UNPACK #-} !(TypedSymbol 'ConstantKind t) ->
      {-# UNPACK #-} !HashId ->
      Description (Term Bool)
    DExistsTerm ::
      {-# UNPACK #-} !Digest ->
      {-# UNPACK #-} !(TypedSymbol 'ConstantKind t) ->
      {-# UNPACK #-} !HashId ->
      Description (Term Bool)
    DNotTerm ::
      {-# UNPACK #-} !Digest ->
      {-# UNPACK #-} !HashId ->
      Description (Term Bool)
    DOrTerm ::
      {-# UNPACK #-} !Digest ->
      {-# UNPACK #-} !HashId ->
      {-# UNPACK #-} !HashId ->
      Description (Term Bool)
    DAndTerm ::
      {-# UNPACK #-} !Digest ->
      {-# UNPACK #-} !HashId ->
      {-# UNPACK #-} !HashId ->
      Description (Term Bool)
    DEqTerm ::
      {-# UNPACK #-} !Digest ->
      Fingerprint ->
      {-# UNPACK #-} !HashId ->
      {-# UNPACK #-} !HashId ->
      Description (Term Bool)
    DDistinctTerm ::
      {-# UNPACK #-} !Digest ->
      Fingerprint ->
      !(NonEmpty HashId) ->
      Description (Term Bool)
    DITETerm ::
      {-# UNPACK #-} !Digest ->
      {-# UNPACK #-} !HashId ->
      {-# UNPACK #-} !HashId ->
      {-# UNPACK #-} !HashId ->
      Description (Term t)
    DAddNumTerm ::
      {-# UNPACK #-} !Digest ->
      {-# UNPACK #-} !HashId ->
      {-# UNPACK #-} !HashId ->
      Description (Term t)
    DNegNumTerm ::
      {-# UNPACK #-} !Digest ->
      {-# UNPACK #-} !HashId ->
      Description (Term t)
    DMulNumTerm ::
      {-# UNPACK #-} !Digest ->
      {-# UNPACK #-} !HashId ->
      {-# UNPACK #-} !HashId ->
      Description (Term t)
    DAbsNumTerm ::
      {-# UNPACK #-} !Digest ->
      {-# UNPACK #-} !HashId ->
      Description (Term t)
    DSignumNumTerm ::
      {-# UNPACK #-} !Digest -> {-# UNPACK #-} !HashId -> Description (Term t)
    DLtOrdTerm ::
      {-# UNPACK #-} !Digest ->
      TypeRep args ->
      {-# UNPACK #-} !HashId ->
      {-# UNPACK #-} !HashId ->
      Description (Term Bool)
    DLeOrdTerm ::
      {-# UNPACK #-} !Digest ->
      TypeRep args ->
      {-# UNPACK #-} !HashId ->
      {-# UNPACK #-} !HashId ->
      Description (Term Bool)
    DAndBitsTerm ::
      {-# UNPACK #-} !Digest ->
      {-# UNPACK #-} !HashId ->
      {-# UNPACK #-} !HashId ->
      Description (Term t)
    DOrBitsTerm ::
      {-# UNPACK #-} !Digest ->
      {-# UNPACK #-} !HashId ->
      {-# UNPACK #-} !HashId ->
      Description (Term t)
    DXorBitsTerm ::
      {-# UNPACK #-} !Digest ->
      {-# UNPACK #-} !HashId ->
      {-# UNPACK #-} !HashId ->
      Description (Term t)
    DComplementBitsTerm ::
      {-# UNPACK #-} !Digest ->
      {-# UNPACK #-} !HashId ->
      Description (Term t)
    DShiftLeftTerm ::
      {-# UNPACK #-} !Digest ->
      {-# UNPACK #-} !HashId ->
      {-# UNPACK #-} !HashId ->
      Description (Term t)
    DShiftRightTerm ::
      {-# UNPACK #-} !Digest ->
      {-# UNPACK #-} !HashId ->
      {-# UNPACK #-} !HashId ->
      Description (Term t)
    DRotateLeftTerm ::
      {-# UNPACK #-} !Digest ->
      {-# UNPACK #-} !HashId ->
      {-# UNPACK #-} !HashId ->
      Description (Term t)
    DRotateRightTerm ::
      {-# UNPACK #-} !Digest ->
      {-# UNPACK #-} !HashId ->
      {-# UNPACK #-} !HashId ->
      Description (Term t)
    DBVConcatTerm ::
      {-# UNPACK #-} !Digest ->
      {-# UNPACK #-} !TypeHashId ->
      {-# UNPACK #-} !TypeHashId ->
      Description (Term t)
    DBitCastTerm ::
      {-# UNPACK #-} !Digest ->
      {-# UNPACK #-} !TypeHashId ->
      Description (Term b)
    DBitCastOrTerm ::
      {-# UNPACK #-} !Digest ->
      {-# UNPACK #-} !HashId ->
      {-# UNPACK #-} !TypeHashId ->
      Description (Term b)
    DBVSelectTerm ::
      forall bv (w :: Nat).
      {-# UNPACK #-} !Digest ->
      !Fingerprint ->
      {-# UNPACK #-} !TypeHashId ->
      Description (Term (bv w))
    DBVExtendTerm ::
      forall bv (r :: Nat).
      {-# UNPACK #-} !Digest ->
      !Bool ->
      !(Proxy r) ->
      {-# UNPACK #-} !TypeHashId ->
      Description (Term (bv r))
    DApplyTerm ::
      {-# UNPACK #-} !Digest ->
      {-# UNPACK #-} !TypeHashId ->
      {-# UNPACK #-} !TypeHashId ->
      Description (Term b)
    DDivIntegralTerm ::
      {-# UNPACK #-} !Digest ->
      {-# UNPACK #-} !HashId ->
      {-# UNPACK #-} !HashId ->
      Description (Term a)
    DModIntegralTerm ::
      {-# UNPACK #-} !Digest ->
      {-# UNPACK #-} !HashId ->
      {-# UNPACK #-} !HashId ->
      Description (Term a)
    DQuotIntegralTerm ::
      {-# UNPACK #-} !Digest ->
      {-# UNPACK #-} !HashId ->
      {-# UNPACK #-} !HashId ->
      Description (Term a)
    DRemIntegralTerm ::
      {-# UNPACK #-} !Digest ->
      {-# UNPACK #-} !HashId ->
      {-# UNPACK #-} !HashId ->
      Description (Term a)
    DFPTraitTerm ::
      {-# UNPACK #-} !Digest ->
      FPTrait ->
      {-# UNPACK #-} !TypeHashId ->
      Description (Term Bool)
    DFdivTerm ::
      {-# UNPACK #-} !Digest ->
      {-# UNPACK #-} !HashId ->
      {-# UNPACK #-} !HashId ->
      Description (Term a)
    DRecipTerm ::
      {-# UNPACK #-} !Digest -> {-# UNPACK #-} !HashId -> Description (Term a)
    DFloatingUnaryTerm ::
      {-# UNPACK #-} !Digest ->
      FloatingUnaryOp ->
      {-# UNPACK #-} !HashId ->
      Description (Term a)
    DPowerTerm ::
      {-# UNPACK #-} !Digest ->
      {-# UNPACK #-} !HashId ->
      {-# UNPACK #-} !HashId ->
      Description (Term a)
    DFPUnaryTerm ::
      {-# UNPACK #-} !Digest ->
      FPUnaryOp ->
      {-# UNPACK #-} !HashId ->
      Description (Term (FP eb sb))
    DFPBinaryTerm ::
      {-# UNPACK #-} !Digest ->
      FPBinaryOp ->
      {-# UNPACK #-} !HashId ->
      {-# UNPACK #-} !HashId ->
      Description (Term (FP eb sb))
    DFPRoundingUnaryTerm ::
      {-# UNPACK #-} !Digest ->
      FPRoundingUnaryOp ->
      {-# UNPACK #-} !HashId ->
      {-# UNPACK #-} !HashId ->
      Description (Term (FP eb sb))
    DFPRoundingBinaryTerm ::
      {-# UNPACK #-} !Digest ->
      FPRoundingBinaryOp ->
      {-# UNPACK #-} !HashId ->
      {-# UNPACK #-} !HashId ->
      {-# UNPACK #-} !HashId ->
      Description (Term (FP eb sb))
    DFPFMATerm ::
      {-# UNPACK #-} !Digest ->
      {-# UNPACK #-} !HashId ->
      {-# UNPACK #-} !HashId ->
      {-# UNPACK #-} !HashId ->
      {-# UNPACK #-} !HashId ->
      Description (Term (FP eb sb))
    DFromIntegralTerm ::
      {-# UNPACK #-} !Digest ->
      {-# UNPACK #-} !TypeHashId ->
      Description (Term b)
    DFromFPOrTerm ::
      {-# UNPACK #-} !Digest ->
      {-# UNPACK #-} !HashId ->
      {-# UNPACK #-} !HashId ->
      {-# UNPACK #-} !TypeHashId ->
      Description (Term a)
    DToFPTerm ::
      {-# UNPACK #-} !Digest ->
      {-# UNPACK #-} !HashId ->
      {-# UNPACK #-} !TypeHashId ->
      Description (Term (FP eb sb))

  describe (UConTerm v) = DConTerm (preHashConDescription v) v
  describe ((USymTerm name) :: UTerm t) =
    DSymTerm @t (preHashSymDescription name) name
  describe (UForallTerm (sym :: TypedSymbol 'ConstantKind arg) arg) =
    let argHashId = hashId arg
     in DForallTerm (preHashForallDescription sym argHashId) sym argHashId
  describe (UExistsTerm (sym :: TypedSymbol 'ConstantKind arg) arg) =
    let argHashId = hashId arg
     in DExistsTerm (preHashExistsDescription sym argHashId) sym argHashId
  describe (UNotTerm arg) =
    let argHashId = hashId arg
     in DNotTerm (preHashNotDescription argHashId) argHashId
  describe (UOrTerm arg1 arg2) =
    let arg1HashId = hashId arg1
        arg2HashId = hashId arg2
     in DOrTerm
          (preHashOrDescription arg1HashId arg2HashId)
          arg1HashId
          arg2HashId
  describe (UAndTerm arg1 arg2) =
    let arg1HashId = hashId arg1
        arg2HashId = hashId arg2
     in DAndTerm
          (preHashAndDescription arg1HashId arg2HashId)
          arg1HashId
          arg2HashId
  describe (UEqTerm (arg1 :: Term arg) arg2) = do
    let fingerprint = typeFingerprint (Proxy @arg)
        arg1HashId = hashId arg1
        arg2HashId = hashId arg2
     in DEqTerm
          (preHashEqDescription fingerprint arg1HashId arg2HashId)
          fingerprint
          arg1HashId
          arg2HashId
  describe (UDistinctTerm args@((_ :: Term arg) :| _)) =
    let fingerprint = typeFingerprint (Proxy @arg)
        argsHashId = hashId <$> args
     in DDistinctTerm
          (preHashDistinctDescription fingerprint argsHashId)
          fingerprint
          argsHashId
  describe (UITETerm cond (l :: Term arg) r) =
    let condHashId = hashId cond
        lHashId = hashId l
        rHashId = hashId r
     in DITETerm
          (preHashITEDescription condHashId lHashId rHashId)
          condHashId
          lHashId
          rHashId
  describe (UAddNumTerm arg1 arg2) =
    let arg1HashId = hashId arg1
        arg2HashId = hashId arg2
     in DAddNumTerm
          (preHashAddNumDescription arg1HashId arg2HashId)
          arg1HashId
          arg2HashId
  describe (UNegNumTerm arg) =
    let argHashId = hashId arg
     in DNegNumTerm (preHashNegNumDescription argHashId) argHashId
  describe (UMulNumTerm arg1 arg2) =
    let arg1HashId = hashId arg1
        arg2HashId = hashId arg2
     in DMulNumTerm
          (preHashMulNumDescription arg1HashId arg2HashId)
          arg1HashId
          arg2HashId
  describe (UAbsNumTerm arg) =
    let argHashId = hashId arg
     in DAbsNumTerm (preHashAbsNumDescription argHashId) argHashId
  describe (USignumNumTerm arg) =
    let argHashId = hashId arg
     in DSignumNumTerm (preHashSignumNumDescription argHashId) argHashId
  describe (ULtOrdTerm (arg1 :: arg) arg2) =
    let tr = typeRep @arg
        arg1HashId = hashId arg1
        arg2HashId = hashId arg2
     in DLtOrdTerm
          (preHashLtOrdDescription tr arg1HashId arg2HashId)
          tr
          arg1HashId
          arg2HashId
  describe (ULeOrdTerm (arg1 :: arg) arg2) =
    let tr = typeRep @arg
        arg1HashId = hashId arg1
        arg2HashId = hashId arg2
     in DLeOrdTerm
          (preHashLeOrdDescription tr arg1HashId arg2HashId)
          tr
          arg1HashId
          arg2HashId
  describe (UAndBitsTerm arg1 arg2) =
    let arg1HashId = hashId arg1
        arg2HashId = hashId arg2
     in DAndBitsTerm
          (preHashAndBitsDescription arg1HashId arg2HashId)
          arg1HashId
          arg2HashId
  describe (UOrBitsTerm arg1 arg2) =
    let arg1HashId = hashId arg1
        arg2HashId = hashId arg2
     in DOrBitsTerm
          (preHashOrBitsDescription arg1HashId arg2HashId)
          arg1HashId
          arg2HashId
  describe (UXorBitsTerm arg1 arg2) =
    let arg1HashId = hashId arg1
        arg2HashId = hashId arg2
     in DXorBitsTerm
          (preHashXorBitsDescription arg1HashId arg2HashId)
          arg1HashId
          arg2HashId
  describe (UComplementBitsTerm arg) =
    let argHashId = hashId arg
     in DComplementBitsTerm
          (preHashComplementBitsDescription argHashId)
          argHashId
  describe (UShiftLeftTerm arg n) =
    let argHashId = hashId arg
        nHashId = hashId n
     in DShiftLeftTerm
          (preHashShiftLeftDescription argHashId nHashId)
          argHashId
          nHashId
  describe (UShiftRightTerm arg n) =
    let argHashId = hashId arg
        nHashId = hashId n
     in DShiftRightTerm
          (preHashShiftRightDescription argHashId nHashId)
          argHashId
          nHashId
  describe (URotateLeftTerm arg n) =
    let argHashId = hashId arg
        nHashId = hashId n
     in DRotateLeftTerm
          (preHashRotateLeftDescription argHashId nHashId)
          argHashId
          nHashId
  describe (URotateRightTerm arg n) =
    let argHashId = hashId arg
        nHashId = hashId n
     in DRotateRightTerm
          (preHashRotateRightDescription argHashId nHashId)
          argHashId
          nHashId
  describe (UBitCastTerm (arg :: Term a)) =
    let argHashId = typeHashId arg
     in DBitCastTerm (preHashBitCastDescription argHashId) argHashId
  describe (UBitCastOrTerm d (arg :: Term a)) =
    let dHashId = hashId d
        argHashId = typeHashId arg
     in DBitCastOrTerm
          (preHashBitCastOrDescription dHashId argHashId)
          dHashId
          argHashId
  describe (UBVConcatTerm (arg1 :: Term bv1) (arg2 :: Term bv2)) =
    let arg1HashId = typeHashId arg1
        arg2HashId = typeHashId arg2
     in DBVConcatTerm
          (preHashBVConcatDescription arg1HashId arg2HashId)
          arg1HashId
          arg2HashId
  describe (UBVSelectTerm (ix :: TypeRep ix) _ (arg :: Term arg)) =
    let ixFingerprint = typeRepFingerprint $ someTypeRep ix
        argHashId = typeHashId arg
     in DBVSelectTerm
          (preHashBVSelectDescription ixFingerprint argHashId)
          ixFingerprint
          argHashId
  describe (UBVExtendTerm signed (n :: Proxy n) (arg :: Term arg)) =
    let argHashId = typeHashId arg
     in DBVExtendTerm
          (preHashBVExtendDescription signed argHashId)
          signed
          n
          argHashId
  describe (UApplyTerm (f :: Term f) (arg :: Term a)) =
    let fHashId = typeHashId f
        argHashId = typeHashId arg
     in DApplyTerm
          (preHashApplyDescription fHashId argHashId)
          fHashId
          argHashId
  describe (UDivIntegralTerm arg1 arg2) =
    let arg1HashId = hashId arg1
        arg2HashId = hashId arg2
     in DDivIntegralTerm
          (preHashDivIntegralDescription arg1HashId arg2HashId)
          arg1HashId
          arg2HashId
  describe (UModIntegralTerm arg1 arg2) =
    let arg1HashId = hashId arg1
        arg2HashId = hashId arg2
     in DModIntegralTerm
          (preHashModIntegralDescription arg1HashId arg2HashId)
          arg1HashId
          arg2HashId
  describe (UQuotIntegralTerm arg1 arg2) =
    let arg1HashId = hashId arg1
        arg2HashId = hashId arg2
     in DQuotIntegralTerm
          (preHashQuotIntegralDescription arg1HashId arg2HashId)
          arg1HashId
          arg2HashId
  describe (URemIntegralTerm arg1 arg2) =
    let arg1HashId = hashId arg1
        arg2HashId = hashId arg2
     in DRemIntegralTerm
          (preHashRemIntegralDescription arg1HashId arg2HashId)
          arg1HashId
          arg2HashId
  describe (UFPTraitTerm trait (arg :: Term arg)) =
    let argHashId = typeHashId arg
     in DFPTraitTerm
          (preHashFPTraitDescription trait argHashId)
          trait
          argHashId
  describe (UFdivTerm arg1 arg2) =
    let arg1HashId = hashId arg1
        arg2HashId = hashId arg2
     in DFdivTerm
          (preHashFdivDescription arg1HashId arg2HashId)
          arg1HashId
          arg2HashId
  describe (URecipTerm arg) =
    let argHashId = hashId arg
     in DRecipTerm (preHashRecipDescription argHashId) argHashId
  describe (UFloatingUnaryTerm op arg) =
    let argHashId = hashId arg
     in DFloatingUnaryTerm
          (preHashFloatingUnaryDescription op argHashId)
          op
          argHashId
  describe (UPowerTerm arg1 arg2) =
    let arg1HashId = hashId arg1
        arg2HashId = hashId arg2
     in DPowerTerm
          (preHashPowerDescription arg1HashId arg2HashId)
          arg1HashId
          arg2HashId
  describe (UFPUnaryTerm op arg) =
    let argHashId = hashId arg
     in DFPUnaryTerm
          (preHashFPUnaryDescription op argHashId)
          op
          argHashId
  describe (UFPBinaryTerm op arg1 arg2) =
    let arg1HashId = hashId arg1
        arg2HashId = hashId arg2
     in DFPBinaryTerm
          (preHashFPBinaryDescription op arg1HashId arg2HashId)
          op
          arg1HashId
          arg2HashId
  describe (UFPRoundingUnaryTerm op mode arg) =
    let modeHashId = hashId mode
        argHashId = hashId arg
     in DFPRoundingUnaryTerm
          (preHashFPRoundingUnaryDescription op modeHashId argHashId)
          op
          modeHashId
          argHashId
  describe (UFPRoundingBinaryTerm op mode arg1 arg2) =
    let modeHashId = hashId mode
        arg1HashId = hashId arg1
        arg2HashId = hashId arg2
     in DFPRoundingBinaryTerm
          ( preHashFPRoundingBinaryDescription
              op
              modeHashId
              arg1HashId
              arg2HashId
          )
          op
          modeHashId
          arg1HashId
          arg2HashId
  describe (UFPFMATerm mode arg1 arg2 arg3) =
    let modeHashId = hashId mode
        arg1HashId = hashId arg1
        arg2HashId = hashId arg2
        arg3HashId = hashId arg3
     in DFPFMATerm
          (preHashFPFMADescription modeHashId arg1HashId arg2HashId arg3HashId)
          modeHashId
          arg1HashId
          arg2HashId
          arg3HashId
  describe (UFromIntegralTerm (arg :: Term a)) =
    let argHashId = typeHashId arg
     in DFromIntegralTerm (preHashFromIntegralDescription argHashId) argHashId
  describe (UFromFPOrTerm d mode (arg :: Term a)) =
    let dHashId = hashId d
        modeHashId = hashId mode
        argHashId = typeHashId arg
     in DFromFPOrTerm
          (preHashFromFPOrDescription dHashId modeHashId argHashId)
          dHashId
          modeHashId
          argHashId
  describe (UToFPTerm mode (arg :: Term a) _ _) =
    let modeHashId = hashId mode
        argHashId = typeHashId arg
     in DToFPTerm
          (preHashToFPTermDescription modeHashId argHashId)
          modeHashId
          argHashId
  {-# INLINE describe #-}

  identify tid ha i = go
    where
      go (UConTerm v) = ConTerm tid ha i v
      go (USymTerm v) = SymTerm tid ha i v
      go (UForallTerm sym arg) = ForallTerm tid ha i sym arg
      go (UExistsTerm sym arg) = ExistsTerm tid ha i sym arg
      go (UNotTerm arg) = NotTerm tid ha i arg
      go (UOrTerm arg1 arg2) = OrTerm tid ha i arg1 arg2
      go (UAndTerm arg1 arg2) = AndTerm tid ha i arg1 arg2
      go (UEqTerm arg1 arg2) = EqTerm tid ha i arg1 arg2
      go (UDistinctTerm args) = DistinctTerm tid ha i args
      go (UITETerm cond l r) = ITETerm tid ha i cond l r
      go (UAddNumTerm arg1 arg2) = AddNumTerm tid ha i arg1 arg2
      go (UNegNumTerm arg) = NegNumTerm tid ha i arg
      go (UMulNumTerm arg1 arg2) = MulNumTerm tid ha i arg1 arg2
      go (UAbsNumTerm arg) = AbsNumTerm tid ha i arg
      go (USignumNumTerm arg) = SignumNumTerm tid ha i arg
      go (ULtOrdTerm arg1 arg2) = LtOrdTerm tid ha i arg1 arg2
      go (ULeOrdTerm arg1 arg2) = LeOrdTerm tid ha i arg1 arg2
      go (UAndBitsTerm arg1 arg2) = AndBitsTerm tid ha i arg1 arg2
      go (UOrBitsTerm arg1 arg2) = OrBitsTerm tid ha i arg1 arg2
      go (UXorBitsTerm arg1 arg2) = XorBitsTerm tid ha i arg1 arg2
      go (UComplementBitsTerm arg) = ComplementBitsTerm tid ha i arg
      go (UShiftLeftTerm arg n) = ShiftLeftTerm tid ha i arg n
      go (UShiftRightTerm arg n) = ShiftRightTerm tid ha i arg n
      go (URotateLeftTerm arg n) = RotateLeftTerm tid ha i arg n
      go (URotateRightTerm arg n) = RotateRightTerm tid ha i arg n
      go (UBitCastTerm arg) = BitCastTerm tid ha i arg
      go (UBitCastOrTerm d arg) = BitCastOrTerm tid ha i d arg
      go (UBVConcatTerm arg1 arg2) = BVConcatTerm tid ha i arg1 arg2
      go (UBVSelectTerm ix w arg) = BVSelectTerm tid ha i ix w arg
      go (UBVExtendTerm signed n arg) = BVExtendTerm tid ha i signed n arg
      go (UApplyTerm f arg) = ApplyTerm tid ha i f arg
      go (UDivIntegralTerm arg1 arg2) = DivIntegralTerm tid ha i arg1 arg2
      go (UModIntegralTerm arg1 arg2) = ModIntegralTerm tid ha i arg1 arg2
      go (UQuotIntegralTerm arg1 arg2) = QuotIntegralTerm tid ha i arg1 arg2
      go (URemIntegralTerm arg1 arg2) = RemIntegralTerm tid ha i arg1 arg2
      go (UFPTraitTerm trait arg) = FPTraitTerm tid ha i trait arg
      go (UFdivTerm arg1 arg2) = FdivTerm tid ha i arg1 arg2
      go (URecipTerm arg) = RecipTerm tid ha i arg
      go (UFloatingUnaryTerm op arg) = FloatingUnaryTerm tid ha i op arg
      go (UPowerTerm arg1 arg2) = PowerTerm tid ha i arg1 arg2
      go (UFPUnaryTerm op arg) = FPUnaryTerm tid ha i op arg
      go (UFPBinaryTerm op arg1 arg2) = FPBinaryTerm tid ha i op arg1 arg2
      go (UFPRoundingUnaryTerm op mode arg) = FPRoundingUnaryTerm tid ha i op mode arg
      go (UFPRoundingBinaryTerm op mode arg1 arg2) = FPRoundingBinaryTerm tid ha i op mode arg1 arg2
      go (UFPFMATerm mode arg1 arg2 arg3) = FPFMATerm tid ha i mode arg1 arg2 arg3
      go (UFromIntegralTerm arg) = FromIntegralTerm tid ha i arg
      go (UFromFPOrTerm d mode arg) = FromFPOrTerm tid ha i d mode arg
      go (UToFPTerm mode arg eb sb) = ToFPTerm tid ha i mode arg eb sb
      {-# INLINE go #-}
  {-# INLINE identify #-}
  threadId = termThreadId
  {-# INLINE threadId #-}

  descriptionDigest (DConTerm h _) = h
  descriptionDigest (DSymTerm h _) = h
  descriptionDigest (DForallTerm h _ _) = h
  descriptionDigest (DExistsTerm h _ _) = h
  descriptionDigest (DNotTerm h _) = h
  descriptionDigest (DOrTerm h _ _) = h
  descriptionDigest (DAndTerm h _ _) = h
  descriptionDigest (DEqTerm h _ _ _) = h
  descriptionDigest (DDistinctTerm h _ _) = h
  descriptionDigest (DITETerm h _ _ _) = h
  descriptionDigest (DAddNumTerm h _ _) = h
  descriptionDigest (DNegNumTerm h _) = h
  descriptionDigest (DMulNumTerm h _ _) = h
  descriptionDigest (DAbsNumTerm h _) = h
  descriptionDigest (DSignumNumTerm h _) = h
  descriptionDigest (DLtOrdTerm h _ _ _) = h
  descriptionDigest (DLeOrdTerm h _ _ _) = h
  descriptionDigest (DAndBitsTerm h _ _) = h
  descriptionDigest (DOrBitsTerm h _ _) = h
  descriptionDigest (DXorBitsTerm h _ _) = h
  descriptionDigest (DComplementBitsTerm h _) = h
  descriptionDigest (DShiftLeftTerm h _ _) = h
  descriptionDigest (DShiftRightTerm h _ _) = h
  descriptionDigest (DRotateLeftTerm h _ _) = h
  descriptionDigest (DRotateRightTerm h _ _) = h
  descriptionDigest (DBitCastTerm h _) = h
  descriptionDigest (DBitCastOrTerm h _ _) = h
  descriptionDigest (DBVConcatTerm h _ _) = h
  descriptionDigest (DBVSelectTerm h _ _) = h
  descriptionDigest (DBVExtendTerm h _ _ _) = h
  descriptionDigest (DDivIntegralTerm h _ _) = h
  descriptionDigest (DModIntegralTerm h _ _) = h
  descriptionDigest (DQuotIntegralTerm h _ _) = h
  descriptionDigest (DRemIntegralTerm h _ _) = h
  descriptionDigest (DApplyTerm h _ _) = h
  descriptionDigest (DFPTraitTerm h _ _) = h
  descriptionDigest (DFdivTerm h _ _) = h
  descriptionDigest (DRecipTerm h _) = h
  descriptionDigest (DFloatingUnaryTerm h _ _) = h
  descriptionDigest (DPowerTerm h _ _) = h
  descriptionDigest (DFPUnaryTerm h _ _) = h
  descriptionDigest (DFPBinaryTerm h _ _ _) = h
  descriptionDigest (DFPRoundingUnaryTerm h _ _ _) = h
  descriptionDigest (DFPRoundingBinaryTerm h _ _ _ _) = h
  descriptionDigest (DFPFMATerm h _ _ _ _) = h
  descriptionDigest (DFromIntegralTerm h _) = h
  descriptionDigest (DFromFPOrTerm h _ _ _) = h
  descriptionDigest (DToFPTerm h _ _) = h
  {-# INLINE descriptionDigest #-}

termThreadId :: Term t -> WeakThreadId
termThreadId (ConTerm tid _ _ _) = tid
termThreadId (SymTerm tid _ _ _) = tid
termThreadId (ForallTerm tid _ _ _ _) = tid
termThreadId (ExistsTerm tid _ _ _ _) = tid
termThreadId (NotTerm tid _ _ _) = tid
termThreadId (OrTerm tid _ _ _ _) = tid
termThreadId (AndTerm tid _ _ _ _) = tid
termThreadId (EqTerm tid _ _ _ _) = tid
termThreadId (DistinctTerm tid _ _ _) = tid
termThreadId (ITETerm tid _ _ _ _ _) = tid
termThreadId (AddNumTerm tid _ _ _ _) = tid
termThreadId (NegNumTerm tid _ _ _) = tid
termThreadId (MulNumTerm tid _ _ _ _) = tid
termThreadId (AbsNumTerm tid _ _ _) = tid
termThreadId (SignumNumTerm tid _ _ _) = tid
termThreadId (LtOrdTerm tid _ _ _ _) = tid
termThreadId (LeOrdTerm tid _ _ _ _) = tid
termThreadId (AndBitsTerm tid _ _ _ _) = tid
termThreadId (OrBitsTerm tid _ _ _ _) = tid
termThreadId (XorBitsTerm tid _ _ _ _) = tid
termThreadId (ComplementBitsTerm tid _ _ _) = tid
termThreadId (ShiftLeftTerm tid _ _ _ _) = tid
termThreadId (ShiftRightTerm tid _ _ _ _) = tid
termThreadId (RotateLeftTerm tid _ _ _ _) = tid
termThreadId (RotateRightTerm tid _ _ _ _) = tid
termThreadId (BitCastTerm tid _ _ _) = tid
termThreadId (BitCastOrTerm tid _ _ _ _) = tid
termThreadId (BVConcatTerm tid _ _ _ _) = tid
termThreadId (BVSelectTerm tid _ _ _ _ _) = tid
termThreadId (BVExtendTerm tid _ _ _ _ _) = tid
termThreadId (ApplyTerm tid _ _ _ _) = tid
termThreadId (DivIntegralTerm tid _ _ _ _) = tid
termThreadId (ModIntegralTerm tid _ _ _ _) = tid
termThreadId (QuotIntegralTerm tid _ _ _ _) = tid
termThreadId (RemIntegralTerm tid _ _ _ _) = tid
termThreadId (FPTraitTerm tid _ _ _ _) = tid
termThreadId (FdivTerm tid _ _ _ _) = tid
termThreadId (RecipTerm tid _ _ _) = tid
termThreadId (FloatingUnaryTerm tid _ _ _ _) = tid
termThreadId (PowerTerm tid _ _ _ _) = tid
termThreadId (FPUnaryTerm tid _ _ _ _) = tid
termThreadId (FPBinaryTerm tid _ _ _ _ _) = tid
termThreadId (FPRoundingUnaryTerm tid _ _ _ _ _) = tid
termThreadId (FPRoundingBinaryTerm tid _ _ _ _ _ _) = tid
termThreadId (FPFMATerm tid _ _ _ _ _ _) = tid
termThreadId (FromIntegralTerm tid _ _ _) = tid
termThreadId (FromFPOrTerm tid _ _ _ _ _) = tid
termThreadId (ToFPTerm tid _ _ _ _ _ _) = tid
{-# INLINE termThreadId #-}

instance (SupportedPrim t) => Eq (Description (Term t)) where
  DConTerm _ (l :: tyl) == DConTerm _ (r :: tyr) = cast @tyl @tyr l == Just r
  DSymTerm _ ls == DSymTerm _ rs = ls == rs
  DForallTerm _ ls li == DForallTerm _ rs ri =
    eqHeteroSymbol ls rs && eqHashId li ri
  DExistsTerm _ ls li == DExistsTerm _ rs ri =
    eqHeteroSymbol ls rs && eqHashId li ri
  DNotTerm _ li == DNotTerm _ ri = eqHashId li ri
  DOrTerm _ li1 li2 == DOrTerm _ ri1 ri2 = eqHashId li1 ri1 && eqHashId li2 ri2
  DAndTerm _ li1 li2 == DAndTerm _ ri1 ri2 = eqHashId li1 ri1 && eqHashId li2 ri2
  DEqTerm _ lfp li1 li2 == DEqTerm _ rfp ri1 ri2 = lfp == rfp && eqHashId li1 ri1 && eqHashId li2 ri2
  DDistinctTerm _ lfp li == DDistinctTerm _ rfp ri =
    lfp == rfp
      && length li == length ri
      && and (zipWith eqHashId (toList li) (toList ri))
  DITETerm _ lc li1 li2 == DITETerm _ rc ri1 ri2 = eqHashId lc rc && eqHashId li1 ri1 && eqHashId li2 ri2
  DAddNumTerm _ li1 li2 == DAddNumTerm _ ri1 ri2 = eqHashId li1 ri1 && eqHashId li2 ri2
  DNegNumTerm _ li == DNegNumTerm _ ri = eqHashId li ri
  DMulNumTerm _ li1 li2 == DMulNumTerm _ ri1 ri2 = eqHashId li1 ri1 && eqHashId li2 ri2
  DAbsNumTerm _ li == DAbsNumTerm _ ri = eqHashId li ri
  DSignumNumTerm _ li == DSignumNumTerm _ ri = eqHashId li ri
  DLtOrdTerm _ lrep li1 li2 == DLtOrdTerm _ rrep ri1 ri2 = eqTypeRepBool lrep rrep && eqHashId li1 ri1 && eqHashId li2 ri2
  DLeOrdTerm _ lrep li1 li2 == DLeOrdTerm _ rrep ri1 ri2 = eqTypeRepBool lrep rrep && eqHashId li1 ri1 && eqHashId li2 ri2
  DAndBitsTerm _ li1 li2 == DAndBitsTerm _ ri1 ri2 = eqHashId li1 ri1 && eqHashId li2 ri2
  DOrBitsTerm _ li1 li2 == DOrBitsTerm _ ri1 ri2 = eqHashId li1 ri1 && eqHashId li2 ri2
  DXorBitsTerm _ li1 li2 == DXorBitsTerm _ ri1 ri2 = eqHashId li1 ri1 && eqHashId li2 ri2
  DComplementBitsTerm _ li == DComplementBitsTerm _ ri = eqHashId li ri
  DShiftLeftTerm _ li ln == DShiftLeftTerm _ ri rn = eqHashId li ri && eqHashId ln rn
  DShiftRightTerm _ li ln == DShiftRightTerm _ ri rn = eqHashId li ri && eqHashId ln rn
  DRotateLeftTerm _ li ln == DRotateLeftTerm _ ri rn = eqHashId li ri && eqHashId ln rn
  DRotateRightTerm _ li ln == DRotateRightTerm _ ri rn = eqHashId li ri && eqHashId ln rn
  DBitCastTerm _ li == DBitCastTerm _ ri = li == ri
  DBitCastOrTerm _ ld li == DBitCastOrTerm _ rd ri = ld == rd && li == ri
  DBVConcatTerm _ li1 li2 == DBVConcatTerm _ ri1 ri2 = li1 == ri1 && li2 == ri2
  DBVSelectTerm _ lix li == DBVSelectTerm _ rix ri =
    lix == rix && li == ri
  DBVExtendTerm _ lIsSigned _ li == DBVExtendTerm _ rIsSigned _ ri =
    lIsSigned == rIsSigned
      && li == ri
  DApplyTerm _ lf li == DApplyTerm _ rf ri = lf == rf && li == ri
  DDivIntegralTerm _ li1 li2 == DDivIntegralTerm _ ri1 ri2 = eqHashId li1 ri1 && eqHashId li2 ri2
  DModIntegralTerm _ li1 li2 == DModIntegralTerm _ ri1 ri2 = eqHashId li1 ri1 && eqHashId li2 ri2
  DQuotIntegralTerm _ li1 li2 == DQuotIntegralTerm _ ri1 ri2 = eqHashId li1 ri1 && eqHashId li2 ri2
  DRemIntegralTerm _ li1 li2 == DRemIntegralTerm _ ri1 ri2 = eqHashId li1 ri1 && eqHashId li2 ri2
  DFPTraitTerm _ lt li == DFPTraitTerm _ rt ri = lt == rt && li == ri
  DFdivTerm _ li1 li2 == DFdivTerm _ ri1 ri2 = eqHashId li1 ri1 && eqHashId li2 ri2
  DRecipTerm _ li == DRecipTerm _ ri = eqHashId li ri
  DFloatingUnaryTerm _ lop li == DFloatingUnaryTerm _ rop ri = lop == rop && eqHashId li ri
  DPowerTerm _ li1 li2 == DPowerTerm _ ri1 ri2 = eqHashId li1 ri1 && eqHashId li2 ri2
  DFPUnaryTerm _ lop li == DFPUnaryTerm _ rop ri = lop == rop && eqHashId li ri
  DFPBinaryTerm _ lop li1 li2 == DFPBinaryTerm _ rop ri1 ri2 = lop == rop && eqHashId li1 ri1 && eqHashId li2 ri2
  DFPRoundingUnaryTerm _ lop lmode li == DFPRoundingUnaryTerm _ rop rmode ri =
    lop == rop && eqHashId lmode rmode && eqHashId li ri
  DFPRoundingBinaryTerm _ lop lmode li1 li2 == DFPRoundingBinaryTerm _ rop rmode ri1 ri2 =
    lop == rop && eqHashId lmode rmode && eqHashId li1 ri1 && eqHashId li2 ri2
  DFPFMATerm _ lmode li1 li2 li3 == DFPFMATerm _ rmode ri1 ri2 ri3 =
    eqHashId lmode rmode && eqHashId li1 ri1 && eqHashId li2 ri2 && eqHashId li3 ri3
  DFromIntegralTerm _ li == DFromIntegralTerm _ ri = li == ri
  DFromFPOrTerm _ ld li lai == DFromFPOrTerm _ rd ri rai = eqHashId ld rd && eqHashId li ri && lai == rai
  DToFPTerm _ li lai == DToFPTerm _ ri rai = eqHashId li ri && lai == rai
  _ == _ = False
  {-# INLINE (==) #-}

instance (SupportedPrim t) => Hashable (Description (Term t)) where
  hashWithSalt s = hashWithSalt s . descriptionDigest
  {-# INLINE hashWithSalt #-}

fullReconstructTerm1 ::
  forall a b.
  (Term a -> IO (Term b)) ->
  Term a ->
  IO (Term b)
fullReconstructTerm1 f x = fullReconstructTerm x >>= f
{-# INLINE fullReconstructTerm1 #-}

fullReconstructTerm2 ::
  forall a b c.
  (Term a -> Term b -> IO (Term c)) ->
  Term a ->
  Term b ->
  IO (Term c)
fullReconstructTerm2 f x y = do
  rx <- fullReconstructTerm x
  ry <- fullReconstructTerm y
  f rx ry
{-# INLINE fullReconstructTerm2 #-}

fullReconstructTerm3 ::
  forall a b c d.
  (Term a -> Term b -> Term c -> IO (Term d)) ->
  Term a ->
  Term b ->
  Term c ->
  IO (Term d)
fullReconstructTerm3 f x y z = do
  rx <- fullReconstructTerm x
  ry <- fullReconstructTerm y
  rz <- fullReconstructTerm z
  f rx ry rz
{-# INLINE fullReconstructTerm3 #-}

fullReconstructTerm :: forall t. Term t -> IO (Term t)
fullReconstructTerm (ConTerm _ _ _ i) = curThreadConTerm i
fullReconstructTerm (SymTerm _ _ _ sym) = curThreadSymTerm (unTypedSymbol sym)
fullReconstructTerm (ForallTerm _ _ _ sym arg) =
  fullReconstructTerm1 (curThreadForallTerm sym) arg
fullReconstructTerm (ExistsTerm _ _ _ sym arg) =
  fullReconstructTerm1 (curThreadExistsTerm sym) arg
fullReconstructTerm (NotTerm _ _ _ arg) =
  fullReconstructTerm1 curThreadNotTerm arg
fullReconstructTerm (OrTerm _ _ _ arg1 arg2) =
  fullReconstructTerm2 curThreadOrTerm arg1 arg2
fullReconstructTerm (AndTerm _ _ _ arg1 arg2) =
  fullReconstructTerm2 curThreadAndTerm arg1 arg2
fullReconstructTerm (EqTerm _ _ _ arg1 arg2) =
  fullReconstructTerm2 curThreadEqTerm arg1 arg2
fullReconstructTerm (DistinctTerm _ _ _ args) =
  traverse fullReconstructTerm args >>= curThreadDistinctTerm
fullReconstructTerm (ITETerm _ _ _ cond arg1 arg2) =
  fullReconstructTerm3 curThreadIteTerm cond arg1 arg2
fullReconstructTerm (AddNumTerm _ _ _ arg1 arg2) =
  fullReconstructTerm2 curThreadAddNumTerm arg1 arg2
fullReconstructTerm (NegNumTerm _ _ _ arg) =
  fullReconstructTerm1 curThreadNegNumTerm arg
fullReconstructTerm (MulNumTerm _ _ _ arg1 arg2) =
  fullReconstructTerm2 curThreadMulNumTerm arg1 arg2
fullReconstructTerm (AbsNumTerm _ _ _ arg) =
  fullReconstructTerm1 curThreadAbsNumTerm arg
fullReconstructTerm (SignumNumTerm _ _ _ arg) =
  fullReconstructTerm1 curThreadSignumNumTerm arg
fullReconstructTerm (LtOrdTerm _ _ _ arg1 arg2) =
  fullReconstructTerm2 curThreadLtOrdTerm arg1 arg2
fullReconstructTerm (LeOrdTerm _ _ _ arg1 arg2) =
  fullReconstructTerm2 curThreadLeOrdTerm arg1 arg2
fullReconstructTerm (AndBitsTerm _ _ _ arg1 arg2) =
  fullReconstructTerm2 curThreadAndBitsTerm arg1 arg2
fullReconstructTerm (OrBitsTerm _ _ _ arg1 arg2) =
  fullReconstructTerm2 curThreadOrBitsTerm arg1 arg2
fullReconstructTerm (XorBitsTerm _ _ _ arg1 arg2) =
  fullReconstructTerm2 curThreadXorBitsTerm arg1 arg2
fullReconstructTerm (ComplementBitsTerm _ _ _ arg) =
  fullReconstructTerm1 curThreadComplementBitsTerm arg
fullReconstructTerm (ShiftLeftTerm _ _ _ arg n) =
  fullReconstructTerm1 (curThreadShiftLeftTerm arg) n
fullReconstructTerm (ShiftRightTerm _ _ _ arg n) =
  fullReconstructTerm1 (curThreadShiftRightTerm arg) n
fullReconstructTerm (RotateLeftTerm _ _ _ arg n) =
  fullReconstructTerm1 (curThreadRotateLeftTerm arg) n
fullReconstructTerm (RotateRightTerm _ _ _ arg n) =
  fullReconstructTerm1 (curThreadRotateRightTerm arg) n
fullReconstructTerm (BitCastTerm _ _ _ v) =
  fullReconstructTerm1 curThreadBitCastTerm v
fullReconstructTerm (BitCastOrTerm _ _ _ d v) =
  fullReconstructTerm2 curThreadBitCastOrTerm d v
fullReconstructTerm (BVConcatTerm _ _ _ arg1 arg2) =
  fullReconstructTerm2 curThreadBvconcatTerm arg1 arg2
fullReconstructTerm (BVSelectTerm _ _ _ (_ :: TypeRep ix) (_ :: TypeRep w) arg) =
  fullReconstructTerm1 (curThreadBvselectTerm (Proxy @ix) (Proxy @w)) arg
fullReconstructTerm (BVExtendTerm _ _ _ signed p arg) =
  fullReconstructTerm1 (curThreadBvextendTerm signed p) arg
fullReconstructTerm (ApplyTerm _ _ _ f arg) =
  fullReconstructTerm2 curThreadApplyTerm f arg
fullReconstructTerm (DivIntegralTerm _ _ _ arg1 arg2) =
  fullReconstructTerm2 curThreadDivIntegralTerm arg1 arg2
fullReconstructTerm (ModIntegralTerm _ _ _ arg1 arg2) =
  fullReconstructTerm2 curThreadModIntegralTerm arg1 arg2
fullReconstructTerm (QuotIntegralTerm _ _ _ arg1 arg2) =
  fullReconstructTerm2 curThreadQuotIntegralTerm arg1 arg2
fullReconstructTerm (RemIntegralTerm _ _ _ arg1 arg2) =
  fullReconstructTerm2 curThreadRemIntegralTerm arg1 arg2
fullReconstructTerm (FPTraitTerm _ _ _ trait arg) =
  fullReconstructTerm1 (curThreadFpTraitTerm trait) arg
fullReconstructTerm (FdivTerm _ _ _ arg1 arg2) =
  fullReconstructTerm2 curThreadFdivTerm arg1 arg2
fullReconstructTerm (RecipTerm _ _ _ arg) =
  fullReconstructTerm1 curThreadRecipTerm arg
fullReconstructTerm (FloatingUnaryTerm _ _ _ op arg) =
  fullReconstructTerm1 (curThreadFloatingUnaryTerm op) arg
fullReconstructTerm (PowerTerm _ _ _ arg1 arg2) =
  fullReconstructTerm2 curThreadPowerTerm arg1 arg2
fullReconstructTerm (FPUnaryTerm _ _ _ op arg) =
  fullReconstructTerm1 (curThreadFpUnaryTerm op) arg
fullReconstructTerm (FPBinaryTerm _ _ _ op arg1 arg2) =
  fullReconstructTerm2 (curThreadFpBinaryTerm op) arg1 arg2
fullReconstructTerm (FPRoundingUnaryTerm _ _ _ op mode arg) =
  fullReconstructTerm2 (curThreadFpRoundingUnaryTerm op) mode arg
fullReconstructTerm (FPRoundingBinaryTerm _ _ _ op mode arg1 arg2) =
  fullReconstructTerm3 (curThreadFpRoundingBinaryTerm op) mode arg1 arg2
fullReconstructTerm (FPFMATerm _ _ _ mode arg1 arg2 arg3) = do
  rmode <- fullReconstructTerm mode
  rarg1 <- fullReconstructTerm arg1
  rarg2 <- fullReconstructTerm arg2
  rarg3 <- fullReconstructTerm arg3
  curThreadFpFMATerm rmode rarg1 rarg2 rarg3
fullReconstructTerm (FromIntegralTerm _ _ _ arg) =
  fullReconstructTerm1 curThreadFromIntegralTerm arg
fullReconstructTerm (FromFPOrTerm _ _ _ d r arg) =
  fullReconstructTerm3 curThreadFromFPOrTerm d r arg
fullReconstructTerm (ToFPTerm _ _ _ r arg _ _) =
  fullReconstructTerm2 curThreadToFPTerm r arg

toCurThreadImpl :: forall t. WeakThreadId -> Term t -> IO (Term t)
toCurThreadImpl tid t | termThreadId t == tid = return t
toCurThreadImpl _ t = fullReconstructTerm t
{-# INLINE toCurThreadImpl #-}

-- | Construct and internalizing a 'ConTerm'.
curThreadConTerm :: (SupportedPrim t) => t -> IO (Term t)
curThreadConTerm t = intern $ UConTerm t
{-# INLINE curThreadConTerm #-}

-- | Construct and internalizing a 'SymTerm'.
curThreadSymTerm :: (SupportedPrim t) => Symbol -> IO (Term t)
curThreadSymTerm t = intern $ USymTerm $ TypedSymbol t
{-# INLINE curThreadSymTerm #-}

-- | Construct and internalizing a 'ForallTerm'.
curThreadForallTerm ::
  (SupportedNonFuncPrim t) =>
  TypedSymbol 'ConstantKind t ->
  Term Bool ->
  IO (Term Bool)
curThreadForallTerm sym arg = intern $ UForallTerm sym arg
{-# INLINE curThreadForallTerm #-}

-- | Construct and internalizing a 'ExistsTerm'.
curThreadExistsTerm ::
  (SupportedNonFuncPrim t) =>
  TypedSymbol 'ConstantKind t ->
  Term Bool ->
  IO (Term Bool)
curThreadExistsTerm sym arg = intern $ UExistsTerm sym arg
{-# INLINE curThreadExistsTerm #-}

-- | Construct and internalizing a 'SymTerm' with an identifier, using simple
-- symbols.
curThreadSsymTerm :: (SupportedPrim t) => Identifier -> IO (Term t)
curThreadSsymTerm = curThreadSymTerm . SimpleSymbol
{-# INLINE curThreadSsymTerm #-}

-- | Construct and internalizing a 'SymTerm' with an identifier and an index,
-- using indexed symbols.
curThreadIsymTerm :: (SupportedPrim t) => Identifier -> Int -> IO (Term t)
curThreadIsymTerm str idx = curThreadSymTerm $ IndexedSymbol str idx
{-# INLINE curThreadIsymTerm #-}

-- | Construct and internalizing a 'NotTerm'.
curThreadNotTerm :: Term Bool -> IO (Term Bool)
curThreadNotTerm = intern . UNotTerm
{-# INLINE curThreadNotTerm #-}

-- | Construct and internalizing a 'OrTerm'.
curThreadOrTerm :: Term Bool -> Term Bool -> IO (Term Bool)
curThreadOrTerm l r = intern $ UOrTerm l r
{-# INLINE curThreadOrTerm #-}

-- | Construct and internalizing a 'AndTerm'.
curThreadAndTerm :: Term Bool -> Term Bool -> IO (Term Bool)
curThreadAndTerm l r = intern $ UAndTerm l r
{-# INLINE curThreadAndTerm #-}

-- | Construct and internalizing a 'EqTerm'.
curThreadEqTerm ::
  (SupportedNonFuncPrim a) => Term a -> Term a -> IO (Term Bool)
curThreadEqTerm l r = intern $ UEqTerm l r
{-# INLINE curThreadEqTerm #-}

-- | Construct and internalizing a 'DistinctTerm'.
curThreadDistinctTerm ::
  (SupportedNonFuncPrim a) => NonEmpty (Term a) -> IO (Term Bool)
curThreadDistinctTerm args = intern $ UDistinctTerm args
{-# INLINE curThreadDistinctTerm #-}

-- | Construct and internalizing a 'ITETerm'.
curThreadIteTerm ::
  (SupportedPrim a) => Term Bool -> Term a -> Term a -> IO (Term a)
curThreadIteTerm c l r = intern $ UITETerm c l r
{-# INLINE curThreadIteTerm #-}

-- | Construct and internalizing a 'AddNumTerm'.
curThreadAddNumTerm :: (PEvalNumTerm a) => Term a -> Term a -> IO (Term a)
curThreadAddNumTerm l r = intern $ UAddNumTerm l r
{-# INLINE curThreadAddNumTerm #-}

-- | Construct and internalizing a 'NegNumTerm'.
curThreadNegNumTerm :: (PEvalNumTerm a) => Term a -> IO (Term a)
curThreadNegNumTerm = intern . UNegNumTerm
{-# INLINE curThreadNegNumTerm #-}

-- | Construct and internalizing a 'MulNumTerm'.
curThreadMulNumTerm :: (PEvalNumTerm a) => Term a -> Term a -> IO (Term a)
curThreadMulNumTerm l r = intern $ UMulNumTerm l r
{-# INLINE curThreadMulNumTerm #-}

-- | Construct and internalizing a 'AbsNumTerm'.
curThreadAbsNumTerm :: (PEvalNumTerm a) => Term a -> IO (Term a)
curThreadAbsNumTerm = intern . UAbsNumTerm
{-# INLINE curThreadAbsNumTerm #-}

-- | Construct and internalizing a 'SignumNumTerm'.
curThreadSignumNumTerm :: (PEvalNumTerm a) => Term a -> IO (Term a)
curThreadSignumNumTerm = intern . USignumNumTerm
{-# INLINE curThreadSignumNumTerm #-}

-- | Construct and internalizing a 'LtOrdTerm'.
curThreadLtOrdTerm :: (PEvalOrdTerm a) => Term a -> Term a -> IO (Term Bool)
curThreadLtOrdTerm l r = intern $ ULtOrdTerm l r
{-# INLINE curThreadLtOrdTerm #-}

-- | Construct and internalizing a 'LeOrdTerm'.
curThreadLeOrdTerm :: (PEvalOrdTerm a) => Term a -> Term a -> IO (Term Bool)
curThreadLeOrdTerm l r = intern $ ULeOrdTerm l r
{-# INLINE curThreadLeOrdTerm #-}

-- | Construct and internalizing a 'AndBitsTerm'.
curThreadAndBitsTerm :: (PEvalBitwiseTerm a) => Term a -> Term a -> IO (Term a)
curThreadAndBitsTerm l r = intern $ UAndBitsTerm l r
{-# INLINE curThreadAndBitsTerm #-}

-- | Construct and internalizing a 'OrBitsTerm'.
curThreadOrBitsTerm :: (PEvalBitwiseTerm a) => Term a -> Term a -> IO (Term a)
curThreadOrBitsTerm l r = intern $ UOrBitsTerm l r
{-# INLINE curThreadOrBitsTerm #-}

-- | Construct and internalizing a 'XorBitsTerm'.
curThreadXorBitsTerm :: (PEvalBitwiseTerm a) => Term a -> Term a -> IO (Term a)
curThreadXorBitsTerm l r = intern $ UXorBitsTerm l r
{-# INLINE curThreadXorBitsTerm #-}

-- | Construct and internalizing a 'ComplementBitsTerm'.
curThreadComplementBitsTerm :: (PEvalBitwiseTerm a) => Term a -> IO (Term a)
curThreadComplementBitsTerm = intern . UComplementBitsTerm
{-# INLINE curThreadComplementBitsTerm #-}

-- | Construct and internalizing a 'ShiftLeftTerm'.
curThreadShiftLeftTerm :: (PEvalShiftTerm a) => Term a -> Term a -> IO (Term a)
curThreadShiftLeftTerm t n = intern $ UShiftLeftTerm t n
{-# INLINE curThreadShiftLeftTerm #-}

-- | Construct and internalizing a 'ShiftRightTerm'.
curThreadShiftRightTerm :: (PEvalShiftTerm a) => Term a -> Term a -> IO (Term a)
curThreadShiftRightTerm t n = intern $ UShiftRightTerm t n
{-# INLINE curThreadShiftRightTerm #-}

-- | Construct and internalizing a 'RotateLeftTerm'.
curThreadRotateLeftTerm ::
  (PEvalRotateTerm a) => Term a -> Term a -> IO (Term a)
curThreadRotateLeftTerm t n = intern $ URotateLeftTerm t n
{-# INLINE curThreadRotateLeftTerm #-}

-- | Construct and internalizing a 'RotateRightTerm'.
curThreadRotateRightTerm ::
  (PEvalRotateTerm a) => Term a -> Term a -> IO (Term a)
curThreadRotateRightTerm t n = intern $ URotateRightTerm t n
{-# INLINE curThreadRotateRightTerm #-}

-- | Construct and internalizing a 'BitCastTerm'.
curThreadBitCastTerm ::
  (PEvalBitCastTerm a b) =>
  Term a ->
  IO (Term b)
curThreadBitCastTerm = intern . UBitCastTerm
{-# INLINE curThreadBitCastTerm #-}

-- | Construct and internalizing a 'BitCastOrTerm'.
curThreadBitCastOrTerm ::
  (PEvalBitCastOrTerm a b) =>
  Term b ->
  Term a ->
  IO (Term b)
curThreadBitCastOrTerm d = intern . UBitCastOrTerm d
{-# INLINE curThreadBitCastOrTerm #-}

-- | Construct and internalizing a 'BVConcatTerm'.
curThreadBvconcatTerm ::
  ( PEvalBVTerm bv,
    KnownNat l,
    KnownNat r,
    KnownNat (l + r),
    1 <= l,
    1 <= r,
    1 <= l + r
  ) =>
  Term (bv l) ->
  Term (bv r) ->
  IO (Term (bv (l + r)))
curThreadBvconcatTerm l r = intern $ UBVConcatTerm l r
{-# INLINE curThreadBvconcatTerm #-}

-- | Construct and internalizing a 'BVSelectTerm'.
curThreadBvselectTerm ::
  forall bv n ix w p q.
  ( PEvalBVTerm bv,
    KnownNat n,
    KnownNat ix,
    KnownNat w,
    1 <= n,
    1 <= w,
    ix + w <= n
  ) =>
  p ix ->
  q w ->
  Term (bv n) ->
  IO (Term (bv w))
curThreadBvselectTerm _ _ v =
  intern $ UBVSelectTerm (typeRep @ix) (typeRep @w) v
{-# INLINE curThreadBvselectTerm #-}

-- | Construct and internalizing a 'BVExtendTerm'.
curThreadBvextendTerm ::
  forall bv l r proxy.
  (PEvalBVTerm bv, KnownNat l, KnownNat r, 1 <= l, 1 <= r, l <= r) =>
  Bool ->
  proxy r ->
  Term (bv l) ->
  IO (Term (bv r))
curThreadBvextendTerm signed _ v = intern $ UBVExtendTerm signed (Proxy @r) v
{-# INLINE curThreadBvextendTerm #-}

-- | Construct and internalizing a 'BVExtendTerm' with sign extension.
curThreadBvsignExtendTerm ::
  forall bv l r proxy.
  (PEvalBVTerm bv, KnownNat l, KnownNat r, 1 <= l, 1 <= r, l <= r) =>
  proxy r ->
  Term (bv l) ->
  IO (Term (bv r))
curThreadBvsignExtendTerm _ v = intern $ UBVExtendTerm True (Proxy @r) v
{-# INLINE curThreadBvsignExtendTerm #-}

-- | Construct and internalizing a 'BVExtendTerm' with zero extension.
curThreadBvzeroExtendTerm ::
  forall bv l r proxy.
  (PEvalBVTerm bv, KnownNat l, KnownNat r, 1 <= l, 1 <= r, l <= r) =>
  proxy r ->
  Term (bv l) ->
  IO (Term (bv r))
curThreadBvzeroExtendTerm _ v = intern $ UBVExtendTerm False (Proxy @r) v
{-# INLINE curThreadBvzeroExtendTerm #-}

-- | Construct and internalizing a 'ApplyTerm'.
curThreadApplyTerm ::
  (SupportedPrim a, SupportedPrim b, SupportedPrim f, PEvalApplyTerm f a b) =>
  Term f ->
  Term a ->
  IO (Term b)
curThreadApplyTerm f a = intern $ UApplyTerm f a
{-# INLINE curThreadApplyTerm #-}

-- | Construct and internalizing a 'DivIntegralTerm'.
curThreadDivIntegralTerm ::
  (PEvalDivModIntegralTerm a) => Term a -> Term a -> IO (Term a)
curThreadDivIntegralTerm l r = intern $ UDivIntegralTerm l r
{-# INLINE curThreadDivIntegralTerm #-}

-- | Construct and internalizing a 'ModIntegralTerm'.
curThreadModIntegralTerm ::
  (PEvalDivModIntegralTerm a) => Term a -> Term a -> IO (Term a)
curThreadModIntegralTerm l r = intern $ UModIntegralTerm l r
{-# INLINE curThreadModIntegralTerm #-}

-- | Construct and internalizing a 'QuotIntegralTerm'.
curThreadQuotIntegralTerm ::
  (PEvalDivModIntegralTerm a) => Term a -> Term a -> IO (Term a)
curThreadQuotIntegralTerm l r = intern $ UQuotIntegralTerm l r
{-# INLINE curThreadQuotIntegralTerm #-}

-- | Construct and internalizing a 'RemIntegralTerm'.
curThreadRemIntegralTerm ::
  (PEvalDivModIntegralTerm a) => Term a -> Term a -> IO (Term a)
curThreadRemIntegralTerm l r = intern $ URemIntegralTerm l r
{-# INLINE curThreadRemIntegralTerm #-}

-- | Construct and internalizing a 'FPTraitTerm'.
curThreadFpTraitTerm ::
  (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
  FPTrait ->
  Term (FP eb sb) ->
  IO (Term Bool)
curThreadFpTraitTerm trait v = intern $ UFPTraitTerm trait v
{-# INLINE curThreadFpTraitTerm #-}

-- | Construct and internalizing a 'FdivTerm'.
curThreadFdivTerm :: (PEvalFractionalTerm a) => Term a -> Term a -> IO (Term a)
curThreadFdivTerm l r = intern $ UFdivTerm l r
{-# INLINE curThreadFdivTerm #-}

-- | Construct and internalizing a 'RecipTerm'.
curThreadRecipTerm :: (PEvalFractionalTerm a) => Term a -> IO (Term a)
curThreadRecipTerm = intern . URecipTerm
{-# INLINE curThreadRecipTerm #-}

-- | Construct and internalizing a 'FloatingUnaryTerm'.
curThreadFloatingUnaryTerm ::
  (PEvalFloatingTerm a) => FloatingUnaryOp -> Term a -> IO (Term a)
curThreadFloatingUnaryTerm op = intern . UFloatingUnaryTerm op
{-# INLINE curThreadFloatingUnaryTerm #-}

-- | Construct and internalizing a 'PowerTerm'.
curThreadPowerTerm :: (PEvalFloatingTerm a) => Term a -> Term a -> IO (Term a)
curThreadPowerTerm l r = intern $ UPowerTerm l r
{-# INLINE curThreadPowerTerm #-}

-- | Construct and internalizing a 'FPUnaryTerm'.
curThreadFpUnaryTerm ::
  (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
  FPUnaryOp ->
  Term (FP eb sb) ->
  IO (Term (FP eb sb))
curThreadFpUnaryTerm op v = intern $ UFPUnaryTerm op v
{-# INLINE curThreadFpUnaryTerm #-}

-- | Construct and internalizing a 'FPBinaryTerm'.
curThreadFpBinaryTerm ::
  (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
  FPBinaryOp ->
  Term (FP eb sb) ->
  Term (FP eb sb) ->
  IO (Term (FP eb sb))
curThreadFpBinaryTerm op l r = intern $ UFPBinaryTerm op l r
{-# INLINE curThreadFpBinaryTerm #-}

-- | Construct and internalizing a 'FPRoundingUnaryTerm'.
curThreadFpRoundingUnaryTerm ::
  (ValidFP eb sb, SupportedPrim (FP eb sb), SupportedPrim FPRoundingMode) =>
  FPRoundingUnaryOp ->
  Term FPRoundingMode ->
  Term (FP eb sb) ->
  IO (Term (FP eb sb))
curThreadFpRoundingUnaryTerm op mode v = intern $ UFPRoundingUnaryTerm op mode v
{-# INLINE curThreadFpRoundingUnaryTerm #-}

-- | Construct and internalizing a 'FPRoundingBinaryTerm'.
curThreadFpRoundingBinaryTerm ::
  (ValidFP eb sb, SupportedPrim (FP eb sb), SupportedPrim FPRoundingMode) =>
  FPRoundingBinaryOp ->
  Term FPRoundingMode ->
  Term (FP eb sb) ->
  Term (FP eb sb) ->
  IO (Term (FP eb sb))
curThreadFpRoundingBinaryTerm op mode l r =
  intern $ UFPRoundingBinaryTerm op mode l r
{-# INLINE curThreadFpRoundingBinaryTerm #-}

-- | Construct and internalizing a 'FPFMATerm'.
curThreadFpFMATerm ::
  (ValidFP eb sb, SupportedPrim (FP eb sb), SupportedPrim FPRoundingMode) =>
  Term FPRoundingMode ->
  Term (FP eb sb) ->
  Term (FP eb sb) ->
  Term (FP eb sb) ->
  IO (Term (FP eb sb))
curThreadFpFMATerm mode l r s = intern $ UFPFMATerm mode l r s
{-# INLINE curThreadFpFMATerm #-}

-- | Construct and internalizing a 'FromIntegralTerm'.
curThreadFromIntegralTerm ::
  (PEvalFromIntegralTerm a b) => Term a -> IO (Term b)
curThreadFromIntegralTerm = intern . UFromIntegralTerm
{-# INLINE curThreadFromIntegralTerm #-}

-- | Construct and internalizing a 'FromFPOrTerm'.
curThreadFromFPOrTerm ::
  ( PEvalIEEEFPConvertibleTerm a,
    ValidFP eb sb,
    SupportedPrim FPRoundingMode,
    SupportedPrim (FP eb sb)
  ) =>
  Term a ->
  Term FPRoundingMode ->
  Term (FP eb sb) ->
  IO (Term a)
curThreadFromFPOrTerm d r f = intern $ UFromFPOrTerm d r f
{-# INLINE curThreadFromFPOrTerm #-}

-- | Construct and internalizing a 'ToFPTerm'.
curThreadToFPTerm ::
  forall a eb sb.
  ( PEvalIEEEFPConvertibleTerm a,
    ValidFP eb sb,
    SupportedPrim FPRoundingMode,
    SupportedPrim (FP eb sb)
  ) =>
  Term FPRoundingMode ->
  Term a ->
  IO (Term (FP eb sb))
curThreadToFPTerm r f = intern $ UToFPTerm r f (Proxy @eb) (Proxy @sb)
{-# INLINE curThreadToFPTerm #-}

inCurThread1 ::
  forall a b.
  (SupportedPrim a) =>
  (Term a -> IO (Term b)) ->
  Term a ->
  IO (Term b)
inCurThread1 f t = do
  tid <- myWeakThreadId
  toCurThreadImpl tid t >>= f
{-# INLINE inCurThread1 #-}

inCurThread2 ::
  forall a b c.
  (SupportedPrim a, SupportedPrim b) =>
  (Term a -> Term b -> IO (Term c)) ->
  Term a ->
  Term b ->
  IO (Term c)
inCurThread2 f a b = do
  tid <- myWeakThreadId
  ra <- toCurThreadImpl tid a
  rb <- toCurThreadImpl tid b
  f ra rb
{-# INLINE inCurThread2 #-}

inCurThread3 ::
  forall a b c d.
  (SupportedPrim a, SupportedPrim b, SupportedPrim c) =>
  (Term a -> Term b -> Term c -> IO (Term d)) ->
  Term a ->
  Term b ->
  Term c ->
  IO (Term d)
inCurThread3 f a b c = do
  tid <- myWeakThreadId
  ra <- toCurThreadImpl tid a
  rb <- toCurThreadImpl tid b
  rc <- toCurThreadImpl tid c
  f ra rb rc
{-# INLINE inCurThread3 #-}

unsafeInCurThread1 ::
  forall a b.
  (SupportedPrim a) =>
  (Term a -> IO (Term b)) ->
  Term a ->
  Term b
unsafeInCurThread1 f = unsafePerformIO . inCurThread1 f
{-# INLINE unsafeInCurThread1 #-}

unsafeInCurThread2 ::
  forall a b c.
  (SupportedPrim a, SupportedPrim b) =>
  (Term a -> Term b -> IO (Term c)) ->
  Term a ->
  Term b ->
  Term c
unsafeInCurThread2 f a b = unsafePerformIO $ inCurThread2 f a b
{-# INLINE unsafeInCurThread2 #-}

unsafeInCurThread3 ::
  forall a b c d.
  (SupportedPrim a, SupportedPrim b, SupportedPrim c) =>
  (Term a -> Term b -> Term c -> IO (Term d)) ->
  Term a ->
  Term b ->
  Term c ->
  Term d
unsafeInCurThread3 f a b c = unsafePerformIO $ inCurThread3 f a b c
{-# INLINE unsafeInCurThread3 #-}

-- | Construct and internalizing a 'ConTerm'.
conTerm :: (SupportedPrim t) => t -> Term t
conTerm = unsafePerformIO . curThreadConTerm
{-# NOINLINE conTerm #-}

-- | Construct and internalizing a 'SymTerm'.
symTerm :: forall t. (SupportedPrim t) => Symbol -> Term t
symTerm = unsafePerformIO . curThreadSymTerm
{-# NOINLINE symTerm #-}

-- | Construct and internalizing a 'ForallTerm'.
forallTerm ::
  (SupportedNonFuncPrim t) =>
  TypedSymbol 'ConstantKind t ->
  Term Bool ->
  Term Bool
forallTerm sym = unsafeInCurThread1 (curThreadForallTerm sym)
{-# NOINLINE forallTerm #-}

-- | Construct and internalizing a 'ExistsTerm'.
existsTerm ::
  (SupportedNonFuncPrim t) =>
  TypedSymbol 'ConstantKind t ->
  Term Bool ->
  Term Bool
existsTerm sym = unsafeInCurThread1 (curThreadExistsTerm sym)
{-# NOINLINE existsTerm #-}

-- | Construct and internalizing a 'SymTerm' with an identifier, using simple
-- symbols.
ssymTerm :: (SupportedPrim t) => Identifier -> Term t
ssymTerm = unsafePerformIO . curThreadSsymTerm
{-# NOINLINE ssymTerm #-}

-- | Construct and internalizing a 'SymTerm' with an identifier and an index,
-- using indexed symbols.
isymTerm :: (SupportedPrim t) => Identifier -> Int -> Term t
isymTerm ident index = unsafePerformIO $ curThreadIsymTerm ident index
{-# NOINLINE isymTerm #-}

-- | Construct and internalizing a 'NotTerm'.
notTerm :: Term Bool -> Term Bool
notTerm = unsafeInCurThread1 curThreadNotTerm
{-# NOINLINE notTerm #-}

-- | Construct and internalizing a 'OrTerm'.
orTerm :: Term Bool -> Term Bool -> Term Bool
orTerm = unsafeInCurThread2 curThreadOrTerm
{-# NOINLINE orTerm #-}

-- | Construct and internalizing a 'AndTerm'.
andTerm :: Term Bool -> Term Bool -> Term Bool
andTerm = unsafeInCurThread2 curThreadAndTerm
{-# NOINLINE andTerm #-}

-- | Construct and internalizing a 'EqTerm'.
eqTerm :: (SupportedNonFuncPrim a) => Term a -> Term a -> Term Bool
eqTerm = unsafeInCurThread2 curThreadEqTerm
{-# NOINLINE eqTerm #-}

-- | Construct and internalizing a 'DistinctTerm'.
distinctTerm :: (SupportedNonFuncPrim a) => NonEmpty (Term a) -> Term Bool
distinctTerm args =
  unsafePerformIO $ do
    tid <- myWeakThreadId
    traverse (toCurThreadImpl tid) args >>= curThreadDistinctTerm
{-# NOINLINE distinctTerm #-}

-- | Construct and internalizing a 'ITETerm'.
iteTerm :: (SupportedPrim a) => Term Bool -> Term a -> Term a -> Term a
iteTerm = unsafeInCurThread3 curThreadIteTerm
{-# NOINLINE iteTerm #-}

-- | Construct and internalizing a 'AddNumTerm'.
addNumTerm :: (PEvalNumTerm a) => Term a -> Term a -> Term a
addNumTerm = unsafeInCurThread2 curThreadAddNumTerm
{-# NOINLINE addNumTerm #-}

-- | Construct and internalizing a 'NegNumTerm'.
negNumTerm :: (PEvalNumTerm a) => Term a -> Term a
negNumTerm = unsafeInCurThread1 curThreadNegNumTerm
{-# NOINLINE negNumTerm #-}

-- | Construct and internalizing a 'MulNumTerm'.
mulNumTerm :: (PEvalNumTerm a) => Term a -> Term a -> Term a
mulNumTerm = unsafeInCurThread2 curThreadMulNumTerm
{-# NOINLINE mulNumTerm #-}

-- | Construct and internalizing a 'AbsNumTerm'.
absNumTerm :: (PEvalNumTerm a) => Term a -> Term a
absNumTerm = unsafeInCurThread1 curThreadAbsNumTerm
{-# NOINLINE absNumTerm #-}

-- | Construct and internalizing a 'SignumNumTerm'.
signumNumTerm :: (PEvalNumTerm a) => Term a -> Term a
signumNumTerm = unsafeInCurThread1 curThreadSignumNumTerm
{-# NOINLINE signumNumTerm #-}

-- | Construct and internalizing a 'LtOrdTerm'.
ltOrdTerm :: (PEvalOrdTerm a) => Term a -> Term a -> Term Bool
ltOrdTerm = unsafeInCurThread2 curThreadLtOrdTerm
{-# NOINLINE ltOrdTerm #-}

-- | Construct and internalizing a 'LeOrdTerm'.
leOrdTerm :: (PEvalOrdTerm a) => Term a -> Term a -> Term Bool
leOrdTerm = unsafeInCurThread2 curThreadLeOrdTerm
{-# NOINLINE leOrdTerm #-}

-- | Construct and internalizing a 'AndBitsTerm'.
andBitsTerm :: (PEvalBitwiseTerm a) => Term a -> Term a -> Term a
andBitsTerm = unsafeInCurThread2 curThreadAndBitsTerm
{-# NOINLINE andBitsTerm #-}

-- | Construct and internalizing a 'OrBitsTerm'.
orBitsTerm :: (PEvalBitwiseTerm a) => Term a -> Term a -> Term a
orBitsTerm = unsafeInCurThread2 curThreadOrBitsTerm
{-# NOINLINE orBitsTerm #-}

-- | Construct and internalizing a 'XorBitsTerm'.
xorBitsTerm :: (PEvalBitwiseTerm a) => Term a -> Term a -> Term a
xorBitsTerm = unsafeInCurThread2 curThreadXorBitsTerm
{-# NOINLINE xorBitsTerm #-}

-- | Construct and internalizing a 'ComplementBitsTerm'.
complementBitsTerm :: (PEvalBitwiseTerm a) => Term a -> Term a
complementBitsTerm = unsafeInCurThread1 curThreadComplementBitsTerm
{-# NOINLINE complementBitsTerm #-}

-- | Construct and internalizing a 'ShiftLeftTerm'.
shiftLeftTerm :: (PEvalShiftTerm a) => Term a -> Term a -> Term a
shiftLeftTerm = unsafeInCurThread2 curThreadShiftLeftTerm
{-# NOINLINE shiftLeftTerm #-}

-- | Construct and internalizing a 'ShiftRightTerm'.
shiftRightTerm :: (PEvalShiftTerm a) => Term a -> Term a -> Term a
shiftRightTerm = unsafeInCurThread2 curThreadShiftRightTerm
{-# NOINLINE shiftRightTerm #-}

-- | Construct and internalizing a 'RotateLeftTerm'.
rotateLeftTerm :: (PEvalRotateTerm a) => Term a -> Term a -> Term a
rotateLeftTerm = unsafeInCurThread2 curThreadRotateLeftTerm
{-# NOINLINE rotateLeftTerm #-}

-- | Construct and internalizing a 'RotateRightTerm'.
rotateRightTerm :: (PEvalRotateTerm a) => Term a -> Term a -> Term a
rotateRightTerm = unsafeInCurThread2 curThreadRotateRightTerm
{-# NOINLINE rotateRightTerm #-}

-- | Construct and internalizing a 'BitCastTerm'.
bitCastTerm ::
  (PEvalBitCastTerm a b) =>
  Term a ->
  Term b
bitCastTerm = unsafeInCurThread1 curThreadBitCastTerm
{-# NOINLINE bitCastTerm #-}

-- | Construct and internalizing a 'BitCastOrTerm'.
bitCastOrTerm ::
  (PEvalBitCastOrTerm a b) =>
  Term b ->
  Term a ->
  Term b
bitCastOrTerm = unsafeInCurThread2 curThreadBitCastOrTerm
{-# NOINLINE bitCastOrTerm #-}

-- | Construct and internalizing a 'BVConcatTerm'.
bvconcatTerm ::
  ( PEvalBVTerm bv,
    KnownNat l,
    KnownNat r,
    KnownNat (l + r),
    1 <= l,
    1 <= r,
    1 <= l + r
  ) =>
  Term (bv l) ->
  Term (bv r) ->
  Term (bv (l + r))
bvconcatTerm = unsafeInCurThread2 curThreadBvconcatTerm
{-# NOINLINE bvconcatTerm #-}

-- | Construct and internalizing a 'BVSelectTerm'.
bvselectTerm ::
  forall bv n ix w p q.
  ( PEvalBVTerm bv,
    KnownNat n,
    KnownNat ix,
    KnownNat w,
    1 <= n,
    1 <= w,
    ix + w <= n
  ) =>
  p ix ->
  q w ->
  Term (bv n) ->
  Term (bv w)
bvselectTerm ix w = unsafeInCurThread1 (curThreadBvselectTerm ix w)
{-# NOINLINE bvselectTerm #-}

-- | Construct and internalizing a 'BVExtendTerm'.
bvextendTerm ::
  forall bv l r proxy.
  (PEvalBVTerm bv, KnownNat l, KnownNat r, 1 <= l, 1 <= r, l <= r) =>
  Bool ->
  proxy r ->
  Term (bv l) ->
  Term (bv r)
bvextendTerm signed r = unsafeInCurThread1 (curThreadBvextendTerm signed r)
{-# NOINLINE bvextendTerm #-}

-- | Construct and internalizing a 'BVExtendTerm' with sign extension.
bvsignExtendTerm ::
  forall bv l r proxy.
  (PEvalBVTerm bv, KnownNat l, KnownNat r, 1 <= l, 1 <= r, l <= r) =>
  proxy r ->
  Term (bv l) ->
  Term (bv r)
bvsignExtendTerm r = unsafeInCurThread1 (curThreadBvsignExtendTerm r)
{-# NOINLINE bvsignExtendTerm #-}

-- | Construct and internalizing a 'BVExtendTerm' with zero extension.
bvzeroExtendTerm ::
  forall bv l r proxy.
  (PEvalBVTerm bv, KnownNat l, KnownNat r, 1 <= l, 1 <= r, l <= r) =>
  proxy r ->
  Term (bv l) ->
  Term (bv r)
bvzeroExtendTerm r = unsafeInCurThread1 (curThreadBvzeroExtendTerm r)
{-# NOINLINE bvzeroExtendTerm #-}

-- | Construct and internalizing a 'ApplyTerm'.
applyTerm ::
  (SupportedPrim a, SupportedPrim b, SupportedPrim f, PEvalApplyTerm f a b) =>
  Term f ->
  Term a ->
  Term b
applyTerm = unsafeInCurThread2 curThreadApplyTerm
{-# NOINLINE applyTerm #-}

-- | Construct and internalizing a 'DivIntegralTerm'.
divIntegralTerm :: (PEvalDivModIntegralTerm a) => Term a -> Term a -> Term a
divIntegralTerm = unsafeInCurThread2 curThreadDivIntegralTerm
{-# NOINLINE divIntegralTerm #-}

-- | Construct and internalizing a 'ModIntegralTerm'.
modIntegralTerm :: (PEvalDivModIntegralTerm a) => Term a -> Term a -> Term a
modIntegralTerm = unsafeInCurThread2 curThreadModIntegralTerm
{-# NOINLINE modIntegralTerm #-}

-- | Construct and internalizing a 'QuotIntegralTerm'.
quotIntegralTerm :: (PEvalDivModIntegralTerm a) => Term a -> Term a -> Term a
quotIntegralTerm = unsafeInCurThread2 curThreadQuotIntegralTerm
{-# NOINLINE quotIntegralTerm #-}

-- | Construct and internalizing a 'RemIntegralTerm'.
remIntegralTerm :: (PEvalDivModIntegralTerm a) => Term a -> Term a -> Term a
remIntegralTerm = unsafeInCurThread2 curThreadRemIntegralTerm
{-# NOINLINE remIntegralTerm #-}

-- | Construct and internalizing a 'FPTraitTerm'.
fpTraitTerm ::
  (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
  FPTrait ->
  Term (FP eb sb) ->
  Term Bool
fpTraitTerm trait = unsafeInCurThread1 (curThreadFpTraitTerm trait)
{-# NOINLINE fpTraitTerm #-}

-- | Construct and internalizing a 'FdivTerm'.
fdivTerm :: (PEvalFractionalTerm a) => Term a -> Term a -> Term a
fdivTerm = unsafeInCurThread2 curThreadFdivTerm
{-# NOINLINE fdivTerm #-}

-- | Construct and internalizing a 'RecipTerm'.
recipTerm :: (PEvalFractionalTerm a) => Term a -> Term a
recipTerm = unsafeInCurThread1 curThreadRecipTerm
{-# NOINLINE recipTerm #-}

-- | Construct and internalizing a 'FloatingUnaryTerm'.
floatingUnaryTerm :: (PEvalFloatingTerm a) => FloatingUnaryOp -> Term a -> Term a
floatingUnaryTerm op = unsafeInCurThread1 (curThreadFloatingUnaryTerm op)
{-# NOINLINE floatingUnaryTerm #-}

-- | Construct and internalizing a 'PowerTerm'.
powerTerm :: (PEvalFloatingTerm a) => Term a -> Term a -> Term a
powerTerm = unsafeInCurThread2 curThreadPowerTerm
{-# NOINLINE powerTerm #-}

-- | Construct and internalizing a 'FPUnaryTerm'.
fpUnaryTerm ::
  (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
  FPUnaryOp ->
  Term (FP eb sb) ->
  Term (FP eb sb)
fpUnaryTerm op = unsafeInCurThread1 (curThreadFpUnaryTerm op)
{-# NOINLINE fpUnaryTerm #-}

-- | Construct and internalizing a 'FPBinaryTerm'.
fpBinaryTerm ::
  (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
  FPBinaryOp ->
  Term (FP eb sb) ->
  Term (FP eb sb) ->
  Term (FP eb sb)
fpBinaryTerm op = unsafeInCurThread2 (curThreadFpBinaryTerm op)
{-# NOINLINE fpBinaryTerm #-}

-- | Construct and internalizing a 'FPRoundingUnaryTerm'.
fpRoundingUnaryTerm ::
  (ValidFP eb sb, SupportedPrim (FP eb sb), SupportedPrim FPRoundingMode) =>
  FPRoundingUnaryOp ->
  Term FPRoundingMode ->
  Term (FP eb sb) ->
  Term (FP eb sb)
fpRoundingUnaryTerm op = unsafeInCurThread2 (curThreadFpRoundingUnaryTerm op)
{-# NOINLINE fpRoundingUnaryTerm #-}

-- | Construct and internalizing a 'FPRoundingBinaryTerm'.
fpRoundingBinaryTerm ::
  (ValidFP eb sb, SupportedPrim (FP eb sb), SupportedPrim FPRoundingMode) =>
  FPRoundingBinaryOp ->
  Term FPRoundingMode ->
  Term (FP eb sb) ->
  Term (FP eb sb) ->
  Term (FP eb sb)
fpRoundingBinaryTerm op = unsafeInCurThread3 (curThreadFpRoundingBinaryTerm op)
{-# NOINLINE fpRoundingBinaryTerm #-}

-- | Construct and internalizing a 'FPFMATerm'.
fpFMATerm ::
  (ValidFP eb sb, SupportedPrim (FP eb sb), SupportedPrim FPRoundingMode) =>
  Term FPRoundingMode ->
  Term (FP eb sb) ->
  Term (FP eb sb) ->
  Term (FP eb sb) ->
  Term (FP eb sb)
fpFMATerm mode a b c = unsafePerformIO $ do
  tid <- myWeakThreadId
  mode' <- toCurThreadImpl tid mode
  a' <- toCurThreadImpl tid a
  b' <- toCurThreadImpl tid b
  c' <- toCurThreadImpl tid c
  curThreadFpFMATerm mode' a' b' c'
{-# NOINLINE fpFMATerm #-}

-- | Construct and internalizing a 'FromIntegralTerm'.
fromIntegralTerm :: (PEvalFromIntegralTerm a b) => Term a -> Term b
fromIntegralTerm = unsafeInCurThread1 curThreadFromIntegralTerm
{-# NOINLINE fromIntegralTerm #-}

-- | Construct and internalizing a 'FromFPOrTerm'.
fromFPOrTerm ::
  ( PEvalIEEEFPConvertibleTerm a,
    ValidFP eb sb,
    SupportedPrim FPRoundingMode,
    SupportedPrim (FP eb sb)
  ) =>
  Term a ->
  Term FPRoundingMode ->
  Term (FP eb sb) ->
  Term a
fromFPOrTerm = unsafeInCurThread3 curThreadFromFPOrTerm
{-# NOINLINE fromFPOrTerm #-}

-- | Construct and internalizing a 'ToFPTerm'.
toFPTerm ::
  forall a eb sb.
  ( PEvalIEEEFPConvertibleTerm a,
    ValidFP eb sb,
    SupportedPrim FPRoundingMode,
    SupportedPrim (FP eb sb)
  ) =>
  Term FPRoundingMode ->
  Term a ->
  Term (FP eb sb)
toFPTerm = unsafeInCurThread2 curThreadToFPTerm
{-# NOINLINE toFPTerm #-}

-- Support for boolean type
defaultValueForBool :: Bool
defaultValueForBool = False

defaultValueForBoolDyn :: ModelValue
defaultValueForBoolDyn = toModelValue defaultValueForBool

-- | Construct and internalizing 'True' term.
trueTerm :: Term Bool
trueTerm = conTerm True
{-# INLINE trueTerm #-}

-- | Construct and internalizing 'False' term.
falseTerm :: Term Bool
falseTerm = conTerm False
{-# INLINE falseTerm #-}

boolConTermView :: forall a. Term a -> Maybe Bool
boolConTermView (ConTerm _ _ _ b) = cast b
boolConTermView _ = Nothing
{-# INLINE boolConTermView #-}

-- | Pattern matcher for concrete 'Bool' terms.
pattern BoolConTerm :: Bool -> Term a
pattern BoolConTerm b <- (boolConTermView -> Just b)

-- | Pattern matcher for 'True' term.
pattern TrueTerm :: Term a
pattern TrueTerm <- BoolConTerm True

-- | Pattern matcher for 'False' term.
pattern FalseTerm :: Term a
pattern FalseTerm <- BoolConTerm False

boolTermView :: forall a. Term a -> Maybe (Term Bool)
boolTermView t = introSupportedPrimConstraint t $ cast t
{-# INLINE boolTermView #-}

-- | Pattern matcher for 'Bool' terms.
pattern BoolTerm :: Term Bool -> Term a
pattern BoolTerm b <- (boolTermView -> Just b)

-- | Partial evaluation for not terms.
pevalNotTerm :: Term Bool -> Term Bool
pevalNotTerm (NotTerm _ _ _ tm) = tm
pevalNotTerm (ConTerm _ _ _ a) = if a then falseTerm else trueTerm
pevalNotTerm (OrTerm _ _ _ (NotTerm _ _ _ n1) n2) = pevalAndTerm n1 (pevalNotTerm n2)
pevalNotTerm (OrTerm _ _ _ (DistinctTerm _ _ _ (n1 :| [n2])) n3) =
  pevalAndTerm (pevalEqTerm n1 n2) (pevalNotTerm n3)
pevalNotTerm (OrTerm _ _ _ n1 (NotTerm _ _ _ n2)) = pevalAndTerm (pevalNotTerm n1) n2
pevalNotTerm (OrTerm _ _ _ n1 (DistinctTerm _ _ _ (n2 :| [n3]))) =
  pevalAndTerm (pevalNotTerm n1) (pevalEqTerm n2 n3)
pevalNotTerm (AndTerm _ _ _ (NotTerm _ _ _ n1) n2) = pevalOrTerm n1 (pevalNotTerm n2)
pevalNotTerm (AndTerm _ _ _ (DistinctTerm _ _ _ (n1 :| [n2])) n3) =
  pevalOrTerm (pevalEqTerm n1 n2) (pevalNotTerm n3)
pevalNotTerm (AndTerm _ _ _ n1 (NotTerm _ _ _ n2)) = pevalOrTerm (pevalNotTerm n1) n2
pevalNotTerm (AndTerm _ _ _ n1 (DistinctTerm _ _ _ (n2 :| [n3]))) =
  pevalOrTerm (pevalNotTerm n1) $ pevalEqTerm n2 n3
pevalNotTerm (EqTerm _ _ _ a b) = distinctTerm $ a :| [b]
pevalNotTerm (DistinctTerm _ _ _ (a :| [b])) = eqTerm a b
pevalNotTerm tm = notTerm tm
{-# INLINEABLE pevalNotTerm #-}

orEqFirst :: Term Bool -> Term Bool -> Bool
orEqFirst _ (ConTerm _ _ _ False) = True
orEqFirst
  (DistinctTerm _ _ _ ((e1 :: Term a) :| [ec1@ConTerm {} :: Term b]))
  (EqTerm _ _ _ (Dyn (e2 :: Term a)) (Dyn (ec2@ConTerm {} :: Term b)))
    | e1 == e2 && ec1 /= ec2 = True
-- orEqFirst
--   (NotTerm _ (EqTerm _ (e1 :: Term a) (ec1@(ConTerm _ _ _) :: Term b)))
--   (EqTerm _ (Dyn (e2 :: Term a)) (Dyn (ec2@(ConTerm _ _ _) :: Term b)))
--     | e1 == e2 && ec1 /= ec2 = True
orEqFirst x y
  | x == y = True
  | otherwise = False
{-# INLINE orEqFirst #-}

orEqTrue :: Term Bool -> Term Bool -> Bool
orEqTrue (ConTerm _ _ _ True) _ = True
orEqTrue _ (ConTerm _ _ _ True) = True
-- orEqTrue (NotTerm _ e1) (NotTerm _ e2) = andEqFalse e1 e2
orEqTrue
  (DistinctTerm _ _ _ ((e1 :: Term a) :| [ec1@ConTerm {} :: Term b]))
  (DistinctTerm _ _ _ ((Dyn (e2 :: Term a)) :| [Dyn (ec2@ConTerm {} :: Term b)]))
    | e1 == e2 && ec1 /= ec2 = True
-- orEqTrue
--   (NotTerm _ (EqTerm _ (e1 :: Term a) (ec1@(ConTerm _ _ _) :: Term b)))
--   (NotTerm _ (EqTerm _ (Dyn (e2 :: Term a)) (Dyn (ec2@(ConTerm _ _ _) :: Term b))))
--     | e1 == e2 && ec1 /= ec2 = True
orEqTrue (NotTerm _ _ _ l) r | l == r = True
orEqTrue l (NotTerm _ _ _ r) | l == r = True
orEqTrue _ _ = False
{-# INLINE orEqTrue #-}

andEqFirst :: Term Bool -> Term Bool -> Bool
andEqFirst _ (ConTerm _ _ _ True) = True
-- andEqFirst x (NotTerm _ y) = andEqFalse x y
andEqFirst
  (EqTerm _ _ _ (e1 :: Term a) (ec1@ConTerm {} :: Term b))
  (DistinctTerm _ _ _ ((Dyn (e2 :: Term a)) :| [Dyn (ec2@ConTerm {} :: Term b)]))
    | e1 == e2 && ec1 /= ec2 = True
-- andEqFirst
--   (EqTerm _ (e1 :: Term a) (ec1@(ConTerm _ _ _) :: Term b))
--   (NotTerm _ (EqTerm _ (Dyn (e2 :: Term a)) (Dyn (ec2@(ConTerm _ _ _) :: Term b))))
--     | e1 == e2 && ec1 /= ec2 = True
andEqFirst x y
  | x == y = True
  | otherwise = False
{-# INLINE andEqFirst #-}

andEqFalse :: Term Bool -> Term Bool -> Bool
andEqFalse (ConTerm _ _ _ False) _ = True
andEqFalse _ (ConTerm _ _ _ False) = True
-- andEqFalse (NotTerm _ e1) (NotTerm _ e2) = orEqTrue e1 e2
andEqFalse
  (EqTerm _ _ _ (e1 :: Term a) (ec1@ConTerm {} :: Term b))
  (EqTerm _ _ _ (Dyn (e2 :: Term a)) (Dyn (ec2@ConTerm {} :: Term b)))
    | e1 == e2 && ec1 /= ec2 = True
andEqFalse (NotTerm _ _ _ x) y | x == y = True
andEqFalse x (NotTerm _ _ _ y) | x == y = True
andEqFalse _ _ = False
{-# INLINE andEqFalse #-}

-- | Partial evaluation for or terms.
pevalOrTerm :: Term Bool -> Term Bool -> Term Bool
pevalOrTerm l r
  | orEqTrue l r = trueTerm
  | orEqFirst l r = l
  | orEqFirst r l = r
pevalOrTerm l r@(OrTerm _ _ _ r1 r2)
  | orEqTrue l r1 = trueTerm
  | orEqTrue l r2 = trueTerm
  | orEqFirst r1 l = r
  | orEqFirst r2 l = r
  | orEqFirst l r1 = pevalOrTerm l r2
  | orEqFirst l r2 = pevalOrTerm l r1
pevalOrTerm l@(OrTerm _ _ _ l1 l2) r
  | orEqTrue l1 r = trueTerm
  | orEqTrue l2 r = trueTerm
  | orEqFirst l1 r = l
  | orEqFirst l2 r = l
  | orEqFirst r l1 = pevalOrTerm l2 r
  | orEqFirst r l2 = pevalOrTerm l1 r
pevalOrTerm l (AndTerm _ _ _ r1 r2)
  | orEqFirst l r1 = l
  | orEqFirst l r2 = l
  | orEqTrue l r1 = pevalOrTerm l r2
  | orEqTrue l r2 = pevalOrTerm l r1
pevalOrTerm (AndTerm _ _ _ l1 l2) r
  | orEqFirst r l1 = r
  | orEqFirst r l2 = r
  | orEqTrue l1 r = pevalOrTerm l2 r
  | orEqTrue l2 r = pevalOrTerm l1 r
pevalOrTerm
  (AndTerm _ _ _ nl1@(NotTerm _ _ _ l1) l2)
  (EqTerm _ _ _ (Dyn (e1 :: Term Bool)) (Dyn (e2 :: Term Bool)))
    | l1 == e1 && l2 == e2 = pevalOrTerm nl1 l2
pevalOrTerm (NotTerm _ _ _ nl) (NotTerm _ _ _ nr) =
  pevalNotTerm $ pevalAndTerm nl nr
pevalOrTerm l r = orTerm l r
{-# INLINEABLE pevalOrTerm #-}

-- | Partial evaluation for and terms.
pevalAndTerm :: Term Bool -> Term Bool -> Term Bool
pevalAndTerm l r
  | andEqFalse l r = falseTerm
  | andEqFirst l r = l
  | andEqFirst r l = r
pevalAndTerm l r@(AndTerm _ _ _ r1 r2)
  | andEqFalse l r1 = falseTerm
  | andEqFalse l r2 = falseTerm
  | andEqFirst r1 l = r
  | andEqFirst r2 l = r
  | andEqFirst l r1 = pevalAndTerm l r2
  | andEqFirst l r2 = pevalAndTerm l r1
pevalAndTerm l@(AndTerm _ _ _ l1 l2) r
  | andEqFalse l1 r = falseTerm
  | andEqFalse l2 r = falseTerm
  | andEqFirst l1 r = l
  | andEqFirst l2 r = l
  | andEqFirst r l1 = pevalAndTerm l2 r
  | andEqFirst r l2 = pevalAndTerm l1 r
pevalAndTerm l (OrTerm _ _ _ r1 r2)
  | andEqFirst l r1 = l
  | andEqFirst l r2 = l
  | andEqFalse l r1 = pevalAndTerm l r2
  | andEqFalse l r2 = pevalAndTerm l r1
pevalAndTerm (OrTerm _ _ _ l1 l2) r
  | andEqFirst r l1 = r
  | andEqFirst r l2 = r
  | andEqFalse l1 r = pevalAndTerm l2 r
  | andEqFalse l2 r = pevalAndTerm l1 r
pevalAndTerm
  (OrTerm _ _ _ l1 nl2@(NotTerm _ _ _ l2))
  (NotTerm _ _ _ (EqTerm _ _ _ (Dyn (e1 :: Term Bool)) (Dyn (e2 :: Term Bool))))
    | l1 == e1 && l2 == e2 = pevalAndTerm l1 nl2
pevalAndTerm (NotTerm _ _ _ nl) (NotTerm _ _ _ nr) = pevalNotTerm $ pevalOrTerm nl nr
pevalAndTerm l r = andTerm l r
{-# INLINEABLE pevalAndTerm #-}

-- | Partial evaluation for imply terms.
pevalImplyTerm :: Term Bool -> Term Bool -> Term Bool
pevalImplyTerm l = pevalOrTerm (pevalNotTerm l)

-- | Partial evaluation for xor terms.
pevalXorTerm :: Term Bool -> Term Bool -> Term Bool
pevalXorTerm l r = pevalOrTerm (pevalAndTerm (pevalNotTerm l) r) (pevalAndTerm l (pevalNotTerm r))

pevalImpliesTerm :: Term Bool -> Term Bool -> Bool
pevalImpliesTerm (ConTerm _ _ _ False) _ = True
pevalImpliesTerm _ (ConTerm _ _ _ True) = True
pevalImpliesTerm
  (EqTerm _ _ _ (e1 :: Term a) (ec1@ConTerm {} :: Term b))
  (DistinctTerm _ _ _ ((Dyn (e2 :: Term a)) :| [(Dyn (ec2@ConTerm {} :: Term b))]))
    | e1 == e2 && ec1 /= ec2 = True
-- pevalImpliesTerm
--   (EqTerm _ (e1 :: Term a) (ec1@(ConTerm _ _ _) :: Term b))
--   (NotTerm _ (EqTerm _ (Dyn (e2 :: Term a)) (Dyn (ec2@(ConTerm _ _ _) :: Term b))))
--     | e1 == e2 && ec1 /= ec2 = True
pevalImpliesTerm a b
  | a == b = True
  | otherwise = False
{-# INLINE pevalImpliesTerm #-}

pevalITEBoolLeftNot :: Term Bool -> Term Bool -> Term Bool -> Maybe (Term Bool)
pevalITEBoolLeftNot cond nIfTrue ifFalse
  -- need test
  | cond == nIfTrue = Just $ pevalAndTerm (pevalNotTerm cond) ifFalse
  | otherwise = case nIfTrue of
      AndTerm _ _ _ nt1 nt2 -> ra
        where
          ra
            | pevalImpliesTerm cond nt1 =
                Just $ pevalITETerm cond (pevalNotTerm nt2) ifFalse
            | pevalImpliesTerm cond nt2 =
                Just $ pevalITETerm cond (pevalNotTerm nt1) ifFalse
            | pevalImpliesTerm cond (pevalNotTerm nt1)
                || pevalImpliesTerm cond (pevalNotTerm nt2) =
                Just $ pevalOrTerm cond ifFalse
            | otherwise = Nothing
      OrTerm _ _ _ nt1 nt2 -> ra
        where
          ra
            | pevalImpliesTerm cond nt1 || pevalImpliesTerm cond nt2 =
                Just $ pevalAndTerm (pevalNotTerm cond) ifFalse
            | pevalImpliesTerm cond (pevalNotTerm nt1) =
                Just $ pevalITETerm cond (pevalNotTerm nt2) ifFalse
            | pevalImpliesTerm cond (pevalNotTerm nt2) =
                Just $ pevalITETerm cond (pevalNotTerm nt1) ifFalse
            | otherwise = Nothing
      _ -> Nothing

pevalITEBoolBothNot :: Term Bool -> Term Bool -> Term Bool -> Maybe (Term Bool)
pevalITEBoolBothNot cond nIfTrue nIfFalse =
  Just $ pevalNotTerm $ pevalITETerm cond nIfTrue nIfFalse

pevalITEBoolRightNot :: Term Bool -> Term Bool -> Term Bool -> Maybe (Term Bool)
pevalITEBoolRightNot cond ifTrue nIfFalse
  -- need test
  | cond == nIfFalse = Just $ pevalOrTerm (pevalNotTerm cond) ifTrue
  | otherwise = Nothing -- need work

pevalInferImplies :: Term Bool -> Term Bool -> Term Bool -> Term Bool -> Maybe (Term Bool)
pevalInferImplies cond (NotTerm _ _ _ nt1) _ falseRes
  | cond == nt1 = Just falseRes
  | otherwise = Nothing
-- \| otherwise = case (cond, nt1) of
--     ( EqTerm _ (e1 :: Term a) (ec1@(ConTerm _ _ _) :: Term b),
--       EqTerm _ (Dyn (e2 :: Term a)) (Dyn (ec2@(ConTerm _ _ _) :: Term b))
--       )
--         | e1 == e2 && ec1 /= ec2 -> Just trueRes
--     _ -> Nothing
pevalInferImplies
  (EqTerm _ _ _ (e1 :: Term a) (ec1@ConTerm {} :: Term b))
  (DistinctTerm _ _ _ ((Dyn (e2 :: Term a)) :| [Dyn (ec2@ConTerm {} :: Term b)]))
  trueRes
  _
    | e1 == e2 && ec1 /= ec2 = Just trueRes
pevalInferImplies
  (EqTerm _ _ _ (e1 :: Term a) (ec1@ConTerm {} :: Term b))
  (EqTerm _ _ _ (Dyn (e2 :: Term a)) (Dyn (ec2@ConTerm {} :: Term b)))
  _
  falseRes
    | e1 == e2 && ec1 /= ec2 = Just falseRes
pevalInferImplies _ _ _ _ = Nothing

pevalITEBoolLeftAnd :: Term Bool -> Term Bool -> Term Bool -> Term Bool -> Maybe (Term Bool)
pevalITEBoolLeftAnd cond t1 t2 ifFalse
  | t1 == ifFalse = Just $ pevalAndTerm t1 $ pevalImplyTerm cond t2
  | t2 == ifFalse = Just $ pevalAndTerm t2 $ pevalImplyTerm cond t1
  | cond == t1 = Just $ pevalITETerm cond t2 ifFalse
  | cond == t2 = Just $ pevalITETerm cond t1 ifFalse
  | otherwise =
      msum
        [ pevalInferImplies cond t1 (pevalITETerm cond t2 ifFalse) (pevalAndTerm (pevalNotTerm cond) ifFalse),
          pevalInferImplies cond t2 (pevalITETerm cond t1 ifFalse) (pevalAndTerm (pevalNotTerm cond) ifFalse)
        ]

pevalITEBoolBothAnd :: Term Bool -> Term Bool -> Term Bool -> Term Bool -> Term Bool -> Maybe (Term Bool)
pevalITEBoolBothAnd cond t1 t2 f1 f2
  | t1 == f1 = Just $ pevalAndTerm t1 $ pevalITETerm cond t2 f2
  | t1 == f2 = Just $ pevalAndTerm t1 $ pevalITETerm cond t2 f1
  | t2 == f1 = Just $ pevalAndTerm t2 $ pevalITETerm cond t1 f2
  | t2 == f2 = Just $ pevalAndTerm t2 $ pevalITETerm cond t1 f1
  | otherwise = Nothing

pevalITEBoolRightAnd :: Term Bool -> Term Bool -> Term Bool -> Term Bool -> Maybe (Term Bool)
pevalITEBoolRightAnd cond ifTrue f1 f2
  | f1 == ifTrue = Just $ pevalAndTerm f1 $ pevalOrTerm cond f2
  | f2 == ifTrue = Just $ pevalAndTerm f2 $ pevalOrTerm cond f1
  | otherwise = Nothing

pevalITEBoolLeftOr :: Term Bool -> Term Bool -> Term Bool -> Term Bool -> Maybe (Term Bool)
pevalITEBoolLeftOr cond t1 t2 ifFalse
  | t1 == ifFalse = Just $ pevalOrTerm t1 $ pevalAndTerm cond t2
  | t2 == ifFalse = Just $ pevalOrTerm t2 $ pevalAndTerm cond t1
  | cond == t1 = Just $ pevalOrTerm cond ifFalse
  | cond == t2 = Just $ pevalOrTerm cond ifFalse
  | otherwise =
      msum
        [ pevalInferImplies cond t1 (pevalOrTerm cond ifFalse) (pevalITETerm cond t2 ifFalse),
          pevalInferImplies cond t2 (pevalOrTerm cond ifFalse) (pevalITETerm cond t1 ifFalse)
        ]

pevalITEBoolBothOr :: Term Bool -> Term Bool -> Term Bool -> Term Bool -> Term Bool -> Maybe (Term Bool)
pevalITEBoolBothOr cond t1 t2 f1 f2
  | t1 == f1 = Just $ pevalOrTerm t1 $ pevalITETerm cond t2 f2
  | t1 == f2 = Just $ pevalOrTerm t1 $ pevalITETerm cond t2 f1
  | t2 == f1 = Just $ pevalOrTerm t2 $ pevalITETerm cond t1 f2
  | t2 == f2 = Just $ pevalOrTerm t2 $ pevalITETerm cond t1 f1
  | otherwise = Nothing

pevalITEBoolRightOr :: Term Bool -> Term Bool -> Term Bool -> Term Bool -> Maybe (Term Bool)
pevalITEBoolRightOr cond ifTrue f1 f2
  | f1 == ifTrue = Just $ pevalOrTerm f1 $ pevalAndTerm (pevalNotTerm cond) f2
  | f2 == ifTrue = Just $ pevalOrTerm f2 $ pevalAndTerm (pevalNotTerm cond) f1
  | otherwise = Nothing

pevalITEBoolLeft :: Term Bool -> Term Bool -> Term Bool -> Maybe (Term Bool)
pevalITEBoolLeft cond (AndTerm _ _ _ t1 t2) ifFalse =
  msum
    [ pevalITEBoolLeftAnd cond t1 t2 ifFalse,
      case ifFalse of
        AndTerm _ _ _ f1 f2 -> pevalITEBoolBothAnd cond t1 t2 f1 f2
        _ -> Nothing
    ]
pevalITEBoolLeft cond (OrTerm _ _ _ t1 t2) ifFalse =
  msum
    [ pevalITEBoolLeftOr cond t1 t2 ifFalse,
      case ifFalse of
        OrTerm _ _ _ f1 f2 -> pevalITEBoolBothOr cond t1 t2 f1 f2
        _ -> Nothing
    ]
pevalITEBoolLeft cond (NotTerm _ _ _ nIfTrue) ifFalse =
  msum
    [ pevalITEBoolLeftNot cond nIfTrue ifFalse,
      case ifFalse of
        NotTerm _ _ _ nIfFalse ->
          pevalITEBoolBothNot cond nIfTrue nIfFalse
        _ -> Nothing
    ]
pevalITEBoolLeft _ _ _ = Nothing

pevalITEBoolNoLeft :: Term Bool -> Term Bool -> Term Bool -> Maybe (Term Bool)
pevalITEBoolNoLeft cond ifTrue (AndTerm _ _ _ f1 f2) = pevalITEBoolRightAnd cond ifTrue f1 f2
pevalITEBoolNoLeft cond ifTrue (OrTerm _ _ _ f1 f2) = pevalITEBoolRightOr cond ifTrue f1 f2
pevalITEBoolNoLeft cond ifTrue (NotTerm _ _ _ nIfFalse) = pevalITEBoolRightNot cond ifTrue nIfFalse
pevalITEBoolNoLeft _ _ _ = Nothing

-- | Basic partial evaluation for ITE terms.
pevalITEBasic :: (SupportedPrim a) => Term Bool -> Term a -> Term a -> Maybe (Term a)
pevalITEBasic (ConTerm _ _ _ True) ~ifTrue ~_ = Just ifTrue
pevalITEBasic (ConTerm _ _ _ False) ~_ ~ifFalse = Just ifFalse
pevalITEBasic (NotTerm _ _ _ ncond) ifTrue ifFalse = Just $ pevalITETerm ncond ifFalse ifTrue
pevalITEBasic _ ifTrue ifFalse | ifTrue == ifFalse = Just ifTrue
pevalITEBasic (ITETerm _ _ _ cc ct cf) (ITETerm _ _ _ tc tt tf) (ITETerm _ _ _ fc ft ff) -- later
  | cc == tc && cc == fc = Just $ pevalITETerm cc (pevalITETerm ct tt ft) (pevalITETerm cf tf ff)
pevalITEBasic cond (ITETerm _ _ _ tc tt tf) ifFalse -- later
  | cond == tc = Just $ pevalITETerm cond tt ifFalse
  | tt == ifFalse = Just $ pevalITETerm (pevalOrTerm (pevalNotTerm cond) tc) tt tf
  | tf == ifFalse = Just $ pevalITETerm (pevalAndTerm cond tc) tt tf
pevalITEBasic cond ifTrue (ITETerm _ _ _ fc ft ff) -- later
  | ifTrue == ft = Just $ pevalITETerm (pevalOrTerm cond fc) ifTrue ff
  | ifTrue == ff = Just $ pevalITETerm (pevalOrTerm cond (pevalNotTerm fc)) ifTrue ft
  | pevalImpliesTerm fc cond = Just $ pevalITETerm cond ifTrue ff
pevalITEBasic _ _ _ = Nothing

pevalITEBoolBasic :: Term Bool -> Term Bool -> Term Bool -> Maybe (Term Bool)
pevalITEBoolBasic cond ifTrue ifFalse
  | cond == ifTrue = Just $ pevalOrTerm cond ifFalse
  | cond == ifFalse = Just $ pevalAndTerm cond ifTrue
pevalITEBoolBasic cond (ConTerm _ _ _ v) ifFalse
  | v = Just $ pevalOrTerm cond ifFalse
  | otherwise = Just $ pevalAndTerm (pevalNotTerm cond) ifFalse
pevalITEBoolBasic cond ifTrue (ConTerm _ _ _ v)
  | v = Just $ pevalOrTerm (pevalNotTerm cond) ifTrue
  | otherwise = Just $ pevalAndTerm cond ifTrue
pevalITEBoolBasic _ _ _ = Nothing

pevalITEBool :: Term Bool -> Term Bool -> Term Bool -> Maybe (Term Bool)
pevalITEBool cond ~ifTrue ~ifFalse =
  msum
    [ pevalITEBasic cond ifTrue ifFalse,
      pevalITEBoolBasic cond ifTrue ifFalse,
      pevalITEBoolLeft cond ifTrue ifFalse,
      pevalITEBoolNoLeft cond ifTrue ifFalse
    ]

-- | Basic partial evaluation for ITE terms.
pevalITEBasicTerm :: (SupportedPrim a) => Term Bool -> Term a -> Term a -> Term a
pevalITEBasicTerm cond ~ifTrue ~ifFalse =
  fromMaybe (iteTerm cond ifTrue ifFalse) $
    pevalITEBasic cond ifTrue ifFalse

-- | Default partial evaluation for equality terms.
pevalDefaultEqTerm :: (SupportedNonFuncPrim a) => Term a -> Term a -> Term Bool
pevalDefaultEqTerm l@ConTerm {} r@ConTerm {} = conTerm $ l == r
pevalDefaultEqTerm l@ConTerm {} r = pevalDefaultEqTerm r l
pevalDefaultEqTerm l (BoolConTerm rv) =
  if rv
    then unsafeCoerce l
    else pevalNotTerm (unsafeCoerce l)
pevalDefaultEqTerm (NotTerm _ _ _ lv) r
  | lv == r = falseTerm
pevalDefaultEqTerm l (NotTerm _ _ _ rv)
  | l == rv = falseTerm
pevalDefaultEqTerm (AddNumTerm _ _ _ (ConTerm _ _ _ c) v) (ConTerm _ _ _ c2) =
  pevalDefaultEqTerm v (conTerm $ c2 - c)
pevalDefaultEqTerm l (ITETerm _ _ _ c t f)
  | l == t = pevalOrTerm c (pevalDefaultEqTerm l f)
  | l == f = pevalOrTerm (pevalNotTerm c) (pevalDefaultEqTerm l t)
pevalDefaultEqTerm (ITETerm _ _ _ c t f) r
  | t == r = pevalOrTerm c (pevalDefaultEqTerm f r)
  | f == r = pevalOrTerm (pevalNotTerm c) (pevalDefaultEqTerm t r)
pevalDefaultEqTerm l r
  | l == r = trueTerm
  | otherwise = eqTerm l r
{-# INLINEABLE pevalDefaultEqTerm #-}

instance SBVRep Bool where
  type SBVType Bool = SBV.SBV Bool

instance SupportedPrimConstraint Bool

instance SupportedPrim Bool where
  pformatCon True = "true"
  pformatCon False = "false"
  defaultValue = defaultValueForBool
  defaultValueDynamic _ = defaultValueForBoolDyn
  pevalITETerm cond ~ifTrue ~ifFalse =
    fromMaybe (iteTerm cond ifTrue ifFalse) $
      pevalITEBool cond ifTrue ifFalse
  pevalEqTerm = pevalDefaultEqTerm
  pevalDistinctTerm (_ :| []) = conTerm True
  pevalDistinctTerm (a :| [b]) = pevalNotTerm $ pevalEqTerm a b
  pevalDistinctTerm _ = conTerm False
  conSBVTerm n = if n then SBV.sTrue else SBV.sFalse
  symSBVName symbol _ = show symbol
  symSBVTerm = sbvFresh
  withPrim r = r
  parseSMTModelResult _ = parseScalarSMTModelResult id
  castTypedSymbol ::
    forall knd knd'.
    (IsSymbolKind knd') =>
    TypedSymbol knd Bool ->
    Maybe (TypedSymbol knd' Bool)
  castTypedSymbol (TypedSymbol s) =
    case decideSymbolKind @knd' of
      Left HRefl -> Just $ TypedSymbol s
      Right HRefl -> Just $ TypedSymbol s
  isFuncType = False
  funcDummyConstraint _ = SBV.sTrue

instance NonFuncSBVRep Bool where
  type NonFuncSBVBaseType Bool = Bool

instance SupportedNonFuncPrim Bool where
  conNonFuncSBVTerm = conSBVTerm
  symNonFuncSBVTerm = symSBVTerm @Bool
  withNonFuncPrim r = r
