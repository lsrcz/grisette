{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
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
    TypedSymbol (TypedSymbol, unTypedSymbol),
    typedConstantSymbol,
    typedAnySymbol,
    TypedConstantSymbol,
    TypedAnySymbol,
    SomeTypedSymbol (..),
    SomeTypedConstantSymbol,
    SomeTypedAnySymbol,
    IsSymbolKind (..),
    showUntyped,
    someTypedSymbol,
    eqHeteroSymbol,
    castSomeTypedSymbol,

    -- * Terms
    FPTrait (..),
    FPUnaryOp (..),
    FPBinaryOp (..),
    FPRoundingUnaryOp (..),
    FPRoundingBinaryOp (..),
    FloatingUnaryOp (..),
    Term (..),
    defaultValueDynamic,
    pattern DynTerm,
    toCurThread,
    CachedInfo (..),
    termInfo,
    termThreadId,
    termDigest,
    termId,
    termStableIdent,
    pformatTerm,
    ModelValue (..),
    toModelValue,
    unsafeFromModelValue,

    -- * Interning
    UTerm (..),
    prettyPrintTerm,

    -- * Interned constructors
    conTerm,
    symTerm,
    ssymTerm,
    isymTerm,
    forallTerm,
    existsTerm,
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
    rotateLeftTerm,
    shiftRightTerm,
    rotateRightTerm,
    bitCastTerm,
    bitCastOrTerm,
    bvConcatTerm,
    bvSelectTerm,
    bvExtendTerm,
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

    -- * Patterns
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

#if !MIN_VERSION_sbv(10,0,0)
#define SMTDefinable Uninterpreted
#endif

#if MIN_VERSION_sbv(11,0,0)
import qualified Data.SBV as SBVTC
#endif

#if MIN_VERSION_base(4,15,0)
import Language.Haskell.TH (Code, Quote)
#else
import Language.Haskell.TH (TExpQ)
#endif

import Control.DeepSeq (NFData (rnf))
import Control.Monad (msum)
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.RWS.Lazy as Lazy
import qualified Control.Monad.RWS.Strict as Strict
import Control.Monad.Reader (MonadTrans (lift), ReaderT)
import qualified Control.Monad.State.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict
import qualified Control.Monad.Writer.Lazy as Lazy
import qualified Control.Monad.Writer.Strict as Strict
import Data.Atomics (atomicModifyIORefCAS_)
import qualified Data.Binary as Binary
import Data.Bits (Bits)
import Data.Bytes.Serial (Serial (deserialize, serialize))
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable (hashWithSalt))
import Data.IORef (IORef, newIORef, readIORef)
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import Data.Maybe (fromMaybe)
import qualified Data.SBV as SBV
import qualified Data.SBV.Dynamic as SBVD
import qualified Data.SBV.Trans as SBVT
import qualified Data.SBV.Trans.Control as SBVTC
import qualified Data.Serialize as Cereal
import Data.String (IsString (fromString))
import Data.Typeable (Proxy (Proxy), cast, typeRepFingerprint)
import GHC.Exts (Any, sortWith)
import GHC.Fingerprint (Fingerprint)
import GHC.Generics (Generic)
import GHC.IO (unsafePerformIO)
import GHC.Stack (HasCallStack)
import GHC.TypeNats (KnownNat, Nat, type (+), type (<=))
import Grisette.Internal.Core.Data.Class.BitCast (BitCast, BitCastOr)
import Grisette.Internal.Core.Data.Class.BitVector (SizedBV)
import Grisette.Internal.Core.Data.Symbol
  ( Identifier,
    Symbol (IndexedSymbol, SimpleSymbol),
  )
import Grisette.Internal.SymPrim.FP (FP, FPRoundingMode, ValidFP)
import Grisette.Internal.SymPrim.Prim.Internal.Caches
  ( CachedInfo
      ( CachedInfo,
        cachedDigest,
        cachedId,
        cachedStableIdent,
        cachedThreadId
      ),
    Digest,
    Id,
    Interned
      ( Description,
        Uninterned,
        describe,
        descriptionDigest,
        identify,
        threadId
      ),
    StableIdent,
    intern,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Utils
  ( WeakThreadId,
    myWeakThreadId,
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

instance (SBVFreshMonad m, Monoid w) => SBVFreshMonad (Lazy.WriterT w m) where
  sbvFresh = lift . sbvFresh
  {-# INLINE sbvFresh #-}

instance (SBVFreshMonad m, Monoid w) => SBVFreshMonad (Lazy.RWST r w s m) where
  sbvFresh = lift . sbvFresh
  {-# INLINE sbvFresh #-}

instance (SBVFreshMonad m) => SBVFreshMonad (Lazy.StateT s m) where
  sbvFresh = lift . sbvFresh
  {-# INLINE sbvFresh #-}

instance (SBVFreshMonad m, Monoid w) => SBVFreshMonad (Strict.WriterT w m) where
  sbvFresh = lift . sbvFresh
  {-# INLINE sbvFresh #-}

instance (SBVFreshMonad m, Monoid w) => SBVFreshMonad (Strict.RWST r w s m) where
  sbvFresh = lift . sbvFresh
  {-# INLINE sbvFresh #-}

instance (SBVFreshMonad m) => SBVFreshMonad (Strict.StateT s m) where
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
class
  ( SupportedPrim a,
    Ord a,
    Eq a,
    Show a,
    Hashable a,
    Typeable a
  ) =>
  NonFuncSBVRep a
  where
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
    NFData t,
    Typeable t,
    SupportedPrimConstraint t,
    SBVRep t
  ) =>
  SupportedPrim t
  where
  primTypeRep :: TypeRep t
  default primTypeRep :: (Typeable t) => TypeRep t
  primTypeRep = typeRep
  sameCon :: t -> t -> Bool
  default sameCon :: (Eq t) => t -> t -> Bool
  sameCon = (==)
  hashConWithSalt :: Int -> t -> Int
  default hashConWithSalt :: (Hashable t) => Int -> t -> Int
  hashConWithSalt = hashWithSalt
  pformatCon :: t -> String
  default pformatCon :: (Show t) => t -> String
  pformatCon = show
  defaultValue :: t
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
  {-# INLINE withPrim #-}
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
  funcDummyConstraint :: SBVType t -> SBV.SBV Bool

-- | The default value in a dynamic t'ModelValue'.
defaultValueDynamic ::
  forall t proxy. (SupportedPrim t) => proxy t -> ModelValue
defaultValueDynamic _ = toModelValue (defaultValue @t)

-- | A value with its type information.
data ModelValue where
  ModelValue :: forall v. (SupportedPrim v) => v -> ModelValue

instance NFData ModelValue where
  rnf (ModelValue v) = rnf v

instance Lift ModelValue where
  liftTyped (ModelValue v) = [||ModelValue v||]

instance Show ModelValue where
  show (ModelValue (v :: v)) = pformatCon v ++ " :: " ++ show (primTypeRep @v)

instance Eq ModelValue where
  (ModelValue (v1 :: v1)) == (ModelValue (v2 :: v2)) =
    case eqTypeRep (primTypeRep @v1) (primTypeRep @v2) of
      Just HRefl -> sameCon v1 v2
      _ -> False

instance Hashable ModelValue where
  s `hashWithSalt` (ModelValue (v :: v)) =
    (s `hashWithSalt` (primTypeRep @v)) `hashConWithSalt` v

-- | Convert from a model value. Crashes if the types does not match.
unsafeFromModelValue :: forall a. (Typeable a) => ModelValue -> a
unsafeFromModelValue (ModelValue (v :: v)) =
  case eqTypeRep (primTypeRep @v) (typeRep @a) of
    Just HRefl -> v
    _ ->
      error $
        "Bad model value type, expected type: "
          ++ show (typeRep @a)
          ++ ", but got: "
          ++ show (primTypeRep @v)

-- | Convert to a model value.
toModelValue :: forall a. (SupportedPrim a) => a -> ModelValue
toModelValue = ModelValue

-- | Cast a typed symbol to a different kind. Check if the kind is compatible.
castSomeTypedSymbol ::
  (IsSymbolKind knd') => SomeTypedSymbol knd -> Maybe (SomeTypedSymbol knd')
castSomeTypedSymbol (SomeTypedSymbol s@TypedSymbol {}) =
  SomeTypedSymbol <$> castTypedSymbol s
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
class PEvalApplyTerm f a b | f -> a b where
  pevalApplyTerm :: Term f -> Term a -> Term b
  sbvApplyTerm :: SBVType f -> SBVType a -> SBVType b

-- | Partial evaluation and lowering for bitwise operation terms.
class PEvalBitwiseTerm t where
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
class PEvalShiftTerm t where
  pevalShiftLeftTerm :: Term t -> Term t -> Term t
  pevalShiftRightTerm :: Term t -> Term t -> Term t
  withSbvShiftTermConstraint ::
    (((SBV.SIntegral (NonFuncSBVBaseType t)) => r)) -> r
  sbvShiftLeftTerm :: SBVType t -> SBVType t -> SBVType t
  default sbvShiftLeftTerm ::
    (SupportedNonFuncPrim t) => SBVType t -> SBVType t -> SBVType t
  sbvShiftLeftTerm l r =
    withNonFuncPrim @t $ withSbvShiftTermConstraint @t $ SBV.sShiftLeft l r
  default sbvShiftRightTerm ::
    (SupportedNonFuncPrim t) => SBVType t -> SBVType t -> SBVType t
  sbvShiftRightTerm :: SBVType t -> SBVType t -> SBVType t
  sbvShiftRightTerm l r =
    withNonFuncPrim @t $ withSbvShiftTermConstraint @t $ SBV.sShiftRight l r

-- | Partial evaluation and lowering for symbolic rotate terms.
class PEvalRotateTerm t where
  pevalRotateLeftTerm :: Term t -> Term t -> Term t
  pevalRotateRightTerm :: Term t -> Term t -> Term t
  withSbvRotateTermConstraint ::
    (((SBV.SIntegral (NonFuncSBVBaseType t)) => r)) -> r
  sbvRotateLeftTerm :: SBVType t -> SBVType t -> SBVType t
  default sbvRotateLeftTerm ::
    (SupportedNonFuncPrim t) => SBVType t -> SBVType t -> SBVType t
  sbvRotateLeftTerm l r =
    withNonFuncPrim @t $ withSbvRotateTermConstraint @t $ SBV.sRotateLeft l r
  sbvRotateRightTerm :: SBVType t -> SBVType t -> SBVType t
  default sbvRotateRightTerm ::
    (SupportedNonFuncPrim t) => SBVType t -> SBVType t -> SBVType t
  sbvRotateRightTerm l r =
    withNonFuncPrim @t $ withSbvRotateTermConstraint @t $ SBV.sRotateRight l r

-- | Partial evaluation and lowering for number terms.
class (Num t) => PEvalNumTerm t where
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
class PEvalOrdTerm t where
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
class PEvalDivModIntegralTerm t where
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
class (BitCast a b) => PEvalBitCastTerm a b where
  pevalBitCastTerm :: Term a -> Term b
  sbvBitCast :: SBVType a -> SBVType b

-- | Partial evaluation and lowering for bitcast or default value terms.
class
  (BitCastOr a b) =>
  PEvalBitCastOrTerm a b
  where
  pevalBitCastOrTerm :: Term b -> Term a -> Term b
  sbvBitCastOr :: SBVType b -> SBVType a -> SBVType b

-- | Partial evaluation and lowering for bit-vector terms.
class (SizedBV bv) => PEvalBVTerm bv where
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
class (Fractional t) => PEvalFractionalTerm t where
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
  deriving (Eq, Ord, Generic, Hashable, Lift, NFData, Serial)

instance Cereal.Serialize FloatingUnaryOp where
  put = serialize
  get = deserialize

instance Binary.Binary FloatingUnaryOp where
  put = serialize
  get = deserialize

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
class PEvalFloatingTerm t where
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
class (Integral a, Num b) => PEvalFromIntegralTerm a b where
  pevalFromIntegralTerm :: Term a -> Term b
  sbvFromIntegralTerm :: SBVType a -> SBVType b

-- | Partial evaluation and lowering for converting from and to IEEE floating
-- point terms.
class PEvalIEEEFPConvertibleTerm a where
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
class IsSymbolKind (knd :: SymbolKind) where
  type SymbolKindConstraint knd :: Type -> Constraint
  decideSymbolKind :: Either (knd :~~: 'ConstantKind) (knd :~~: 'AnyKind)
  withSymbolKindConstraint ::
    TypedSymbol knd t ->
    ((SymbolKindConstraint knd t) => a) ->
    a

instance IsSymbolKind 'ConstantKind where
  type SymbolKindConstraint 'ConstantKind = SupportedNonFuncPrim
  decideSymbolKind = Left HRefl
  withSymbolKindConstraint r = withConstantSymbolSupported r

instance IsSymbolKind 'AnyKind where
  type SymbolKindConstraint 'AnyKind = SupportedPrim
  decideSymbolKind = Right HRefl
  withSymbolKindConstraint r = withSymbolSupported r

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

-- | Create a typed symbol with constant kinds.
typedConstantSymbol ::
  forall t. (SupportedNonFuncPrim t) => Symbol -> TypedSymbol 'ConstantKind t
typedConstantSymbol = typedConstantSymbol' getPhantomNonFuncDict
{-# INLINE typedConstantSymbol #-}

{-# NOINLINE typedConstantSymbol' #-}
typedConstantSymbol' ::
  forall t. PhantomNonFuncDict t -> Symbol -> TypedSymbol 'ConstantKind t
typedConstantSymbol' PhantomNonFuncDict symbol = TypedSymbol symbol

-- | Create a typed symbol with any kinds.
typedAnySymbol ::
  forall t. (SupportedPrim t) => Symbol -> TypedSymbol 'AnyKind t
typedAnySymbol = typedAnySymbol' getPhantomDict
{-# INLINE typedAnySymbol #-}

{-# NOINLINE typedAnySymbol' #-}
typedAnySymbol' ::
  forall t. PhantomDict t -> Symbol -> TypedSymbol 'AnyKind t
typedAnySymbol' PhantomDict symbol = TypedSymbol symbol

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
  show (TypedSymbol symbol) = show symbol ++ " :: " ++ show (primTypeRep @t)

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
withSymbolSupported ::
  forall knd t a.
  TypedSymbol knd t ->
  ((SupportedPrim t) => a) ->
  a
withSymbolSupported (TypedSymbol _) a = a
{-# INLINE withSymbolSupported #-}

-- | Introduce the 'SupportedPrim' constraint from the t'TypedSymbol'.
withConstantSymbolSupported ::
  forall t a.
  TypedSymbol 'ConstantKind t ->
  ((SupportedNonFuncPrim t) => a) ->
  a
withConstantSymbolSupported (TypedSymbol _) a = a
{-# INLINE withConstantSymbolSupported #-}

-- | A non-indexed symbol. Type information are checked at runtime.
data SomeTypedSymbol knd where
  SomeTypedSymbol ::
    forall knd t.
    TypedSymbol knd t ->
    SomeTypedSymbol knd

-- | Non-indexed constant symbol
type SomeTypedConstantSymbol = SomeTypedSymbol 'ConstantKind

-- | Non-indexed any symbol
type SomeTypedAnySymbol = SomeTypedSymbol 'AnyKind

instance NFData (SomeTypedSymbol knd) where
  rnf (SomeTypedSymbol s) = rnf s
  {-# INLINE rnf #-}

instance Lift (SomeTypedSymbol knd) where
  liftTyped (SomeTypedSymbol s) = [||SomeTypedSymbol s||]

instance Eq (SomeTypedSymbol knd) where
  (SomeTypedSymbol (s1 :: TypedSymbol knd a))
    == (SomeTypedSymbol (s2 :: TypedSymbol knd b)) =
      withSymbolSupported s1 $
        withSymbolSupported s2 $
          case eqTypeRep (primTypeRep @a) (primTypeRep @b) of
            Just HRefl -> s1 == s2
            _ -> False
  {-# INLINE (==) #-}

instance Ord (SomeTypedSymbol knd) where
  (SomeTypedSymbol (s1 :: TypedSymbol knd a))
    <= (SomeTypedSymbol (s2 :: TypedSymbol knd b)) =
      withSymbolSupported s1 $
        withSymbolSupported s2 $
          let t1 = primTypeRep @a
              t2 = primTypeRep @b
           in SomeTypeRep t1 < SomeTypeRep t2
                || ( case eqTypeRep t1 t2 of
                       Just HRefl -> s1 <= s2
                       _ -> False
                   )

instance Hashable (SomeTypedSymbol knd) where
  hashWithSalt s (SomeTypedSymbol s1) = s `hashWithSalt` s1
  {-# INLINE hashWithSalt #-}

instance Show (SomeTypedSymbol knd) where
  show (SomeTypedSymbol s) = show s

-- | Construct a t'SomeTypedSymbol' from a t'TypedSymbol'.
someTypedSymbol :: forall knd t. TypedSymbol knd t -> SomeTypedSymbol knd
someTypedSymbol s@(TypedSymbol _) = SomeTypedSymbol s
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
  deriving (Eq, Ord, Generic, Hashable, Lift, NFData, Serial)

instance Cereal.Serialize FPTrait where
  put = serialize
  get = deserialize

instance Binary.Binary FPTrait where
  put = serialize
  get = deserialize

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
  deriving (Eq, Ord, Generic, Hashable, Lift, NFData, Serial)

instance Cereal.Serialize FPUnaryOp where
  put = serialize
  get = deserialize

instance Binary.Binary FPUnaryOp where
  put = serialize
  get = deserialize

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
  deriving (Eq, Ord, Generic, Hashable, Lift, NFData, Serial)

instance Cereal.Serialize FPBinaryOp where
  put = serialize
  get = deserialize

instance Binary.Binary FPBinaryOp where
  put = serialize
  get = deserialize

instance Show FPBinaryOp where
  show FPRem = "fp.rem"
  show FPMinimum = "fp.minimum"
  show FPMinimumNumber = "fp.minimumNumber"
  show FPMaximum = "fp.maximum"
  show FPMaximumNumber = "fp.maximumNumber"

-- | Unary floating point operations with rounding modes.
data FPRoundingUnaryOp = FPSqrt | FPRoundToIntegral
  deriving (Eq, Ord, Generic, Hashable, Lift, NFData, Serial)

instance Cereal.Serialize FPRoundingUnaryOp where
  put = serialize
  get = deserialize

instance Binary.Binary FPRoundingUnaryOp where
  put = serialize
  get = deserialize

instance Show FPRoundingUnaryOp where
  show FPSqrt = "fp.sqrt"
  show FPRoundToIntegral = "fp.roundToIntegral"

-- | Binary floating point operations with rounding modes.
data FPRoundingBinaryOp = FPAdd | FPSub | FPMul | FPDiv
  deriving (Eq, Ord, Generic, Hashable, Lift, NFData, Serial)

instance Cereal.Serialize FPRoundingBinaryOp where
  put = serialize
  get = deserialize

instance Binary.Binary FPRoundingBinaryOp where
  put = serialize
  get = deserialize

instance Show FPRoundingBinaryOp where
  show FPAdd = "fp.add"
  show FPSub = "fp.sub"
  show FPMul = "fp.mul"
  show FPDiv = "fp.div"

instance NFData CachedInfo where
  rnf (CachedInfo tid digest id stableIdent) =
    rnf tid `seq` rnf digest `seq` rnf id `seq` rnf stableIdent

-- | Internal representation for Grisette symbolic terms.
data Term t where
  ConTerm' ::
    (SupportedPrim t) =>
    {-# UNPACK #-} !CachedInfo ->
    !t ->
    Term t
  SymTerm' ::
    {-# UNPACK #-} !CachedInfo ->
    !(TypedSymbol 'AnyKind t) ->
    Term t
  ForallTerm' ::
    {-# UNPACK #-} !CachedInfo ->
    !(TypedSymbol 'ConstantKind t) ->
    !(Term Bool) ->
    Term Bool
  ExistsTerm' ::
    {-# UNPACK #-} !CachedInfo ->
    !(TypedSymbol 'ConstantKind t) ->
    !(Term Bool) ->
    Term Bool
  NotTerm' ::
    {-# UNPACK #-} !CachedInfo ->
    !(Term Bool) ->
    Term Bool
  OrTerm' ::
    {-# UNPACK #-} !CachedInfo ->
    !(Term Bool) ->
    !(Term Bool) ->
    Term Bool
  AndTerm' ::
    {-# UNPACK #-} !CachedInfo ->
    !(Term Bool) ->
    !(Term Bool) ->
    Term Bool
  EqTerm' ::
    {-# UNPACK #-} !CachedInfo ->
    !(Term t) ->
    !(Term t) ->
    Term Bool
  DistinctTerm' ::
    {-# UNPACK #-} !CachedInfo ->
    !(NonEmpty (Term t)) ->
    Term Bool
  ITETerm' ::
    (SupportedPrim t) =>
    {-# UNPACK #-} !CachedInfo ->
    !(Term Bool) ->
    !(Term t) ->
    !(Term t) ->
    Term t
  AddNumTerm' ::
    (SupportedPrim t, PEvalNumTerm t) =>
    {-# UNPACK #-} !CachedInfo ->
    !(Term t) ->
    !(Term t) ->
    Term t
  NegNumTerm' ::
    (SupportedPrim t, PEvalNumTerm t) =>
    {-# UNPACK #-} !CachedInfo ->
    !(Term t) ->
    Term t
  MulNumTerm' ::
    (SupportedPrim t, PEvalNumTerm t) =>
    {-# UNPACK #-} !CachedInfo ->
    !(Term t) ->
    !(Term t) ->
    Term t
  AbsNumTerm' ::
    (SupportedPrim t, PEvalNumTerm t) =>
    {-# UNPACK #-} !CachedInfo ->
    !(Term t) ->
    Term t
  SignumNumTerm' ::
    (SupportedPrim t, PEvalNumTerm t) =>
    {-# UNPACK #-} !CachedInfo ->
    !(Term t) ->
    Term t
  LtOrdTerm' ::
    (SupportedPrim t, PEvalOrdTerm t) =>
    {-# UNPACK #-} !CachedInfo ->
    !(Term t) ->
    !(Term t) ->
    Term Bool
  LeOrdTerm' ::
    (SupportedPrim t, PEvalOrdTerm t) =>
    {-# UNPACK #-} !CachedInfo ->
    !(Term t) ->
    !(Term t) ->
    Term Bool
  AndBitsTerm' ::
    (SupportedPrim t, PEvalBitwiseTerm t) =>
    {-# UNPACK #-} !CachedInfo ->
    !(Term t) ->
    !(Term t) ->
    Term t
  OrBitsTerm' ::
    (SupportedPrim t, PEvalBitwiseTerm t) =>
    {-# UNPACK #-} !CachedInfo ->
    !(Term t) ->
    !(Term t) ->
    Term t
  XorBitsTerm' ::
    (SupportedPrim t, PEvalBitwiseTerm t) =>
    {-# UNPACK #-} !CachedInfo ->
    !(Term t) ->
    !(Term t) ->
    Term t
  ComplementBitsTerm' ::
    (SupportedPrim t, PEvalBitwiseTerm t) =>
    {-# UNPACK #-} !CachedInfo ->
    !(Term t) ->
    Term t
  ShiftLeftTerm' ::
    (SupportedPrim t, PEvalShiftTerm t) =>
    {-# UNPACK #-} !CachedInfo ->
    !(Term t) ->
    !(Term t) ->
    Term t
  ShiftRightTerm' ::
    (SupportedPrim t, PEvalShiftTerm t) =>
    {-# UNPACK #-} !CachedInfo ->
    !(Term t) ->
    !(Term t) ->
    Term t
  RotateLeftTerm' ::
    (SupportedPrim t, PEvalRotateTerm t) =>
    {-# UNPACK #-} !CachedInfo ->
    !(Term t) ->
    !(Term t) ->
    Term t
  RotateRightTerm' ::
    (SupportedPrim t, PEvalRotateTerm t) =>
    {-# UNPACK #-} !CachedInfo ->
    !(Term t) ->
    !(Term t) ->
    Term t
  BitCastTerm' ::
    (SupportedPrim b, PEvalBitCastTerm a b) =>
    {-# UNPACK #-} !CachedInfo ->
    !(Term a) ->
    Term b
  BitCastOrTerm' ::
    (SupportedPrim b, PEvalBitCastOrTerm a b) =>
    {-# UNPACK #-} !CachedInfo ->
    !(Term b) ->
    !(Term a) ->
    Term b
  BVConcatTerm' ::
    ( PEvalBVTerm bv,
      KnownNat l,
      KnownNat r,
      KnownNat (l + r),
      1 <= l,
      1 <= r,
      1 <= l + r,
      SupportedPrim (bv (l + r))
    ) =>
    {-# UNPACK #-} !CachedInfo ->
    !(Term (bv l)) ->
    !(Term (bv r)) ->
    Term (bv (l + r))
  BVSelectTerm' ::
    ( PEvalBVTerm bv,
      KnownNat n,
      KnownNat ix,
      KnownNat w,
      1 <= n,
      1 <= w,
      ix + w <= n,
      SupportedPrim (bv w)
    ) =>
    {-# UNPACK #-} !CachedInfo ->
    !(Proxy ix) ->
    !(Proxy w) ->
    !(Term (bv n)) ->
    Term (bv w)
  BVExtendTerm' ::
    ( PEvalBVTerm bv,
      KnownNat l,
      KnownNat r,
      1 <= l,
      1 <= r,
      l <= r,
      SupportedPrim (bv r)
    ) =>
    {-# UNPACK #-} !CachedInfo ->
    !Bool ->
    !(Proxy r) ->
    !(Term (bv l)) ->
    Term (bv r)
  ApplyTerm' ::
    (PEvalApplyTerm f a b, SupportedPrim b) =>
    {-# UNPACK #-} !CachedInfo ->
    !(Term f) ->
    !(Term a) ->
    Term b
  DivIntegralTerm' ::
    (SupportedPrim t, PEvalDivModIntegralTerm t) =>
    {-# UNPACK #-} !CachedInfo ->
    !(Term t) ->
    !(Term t) ->
    Term t
  ModIntegralTerm' ::
    (SupportedPrim t, PEvalDivModIntegralTerm t) =>
    {-# UNPACK #-} !CachedInfo ->
    !(Term t) ->
    !(Term t) ->
    Term t
  QuotIntegralTerm' ::
    (SupportedPrim t, PEvalDivModIntegralTerm t) =>
    {-# UNPACK #-} !CachedInfo ->
    !(Term t) ->
    !(Term t) ->
    Term t
  RemIntegralTerm' ::
    (SupportedPrim t, PEvalDivModIntegralTerm t) =>
    {-# UNPACK #-} !CachedInfo ->
    !(Term t) ->
    !(Term t) ->
    Term t
  FPTraitTerm' ::
    (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
    {-# UNPACK #-} !CachedInfo ->
    !FPTrait ->
    !(Term (FP eb sb)) ->
    Term Bool
  FdivTerm' ::
    (SupportedPrim t, PEvalFractionalTerm t) =>
    {-# UNPACK #-} !CachedInfo ->
    !(Term t) ->
    !(Term t) ->
    Term t
  RecipTerm' ::
    (SupportedPrim t, PEvalFractionalTerm t) =>
    {-# UNPACK #-} !CachedInfo ->
    !(Term t) ->
    Term t
  FloatingUnaryTerm' ::
    (SupportedPrim t, PEvalFloatingTerm t) =>
    {-# UNPACK #-} !CachedInfo ->
    !FloatingUnaryOp ->
    !(Term t) ->
    Term t
  PowerTerm' ::
    (SupportedPrim t, PEvalFloatingTerm t) =>
    {-# UNPACK #-} !CachedInfo ->
    !(Term t) ->
    !(Term t) ->
    Term t
  FPUnaryTerm' ::
    (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
    {-# UNPACK #-} !CachedInfo ->
    !FPUnaryOp ->
    !(Term (FP eb sb)) ->
    Term (FP eb sb)
  FPBinaryTerm' ::
    (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
    {-# UNPACK #-} !CachedInfo ->
    !FPBinaryOp ->
    !(Term (FP eb sb)) ->
    !(Term (FP eb sb)) ->
    Term (FP eb sb)
  FPRoundingUnaryTerm' ::
    (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
    {-# UNPACK #-} !CachedInfo ->
    !FPRoundingUnaryOp ->
    !(Term FPRoundingMode) ->
    !(Term (FP eb sb)) ->
    Term (FP eb sb)
  FPRoundingBinaryTerm' ::
    (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
    {-# UNPACK #-} !CachedInfo ->
    !FPRoundingBinaryOp ->
    !(Term FPRoundingMode) ->
    !(Term (FP eb sb)) ->
    !(Term (FP eb sb)) ->
    Term (FP eb sb)
  FPFMATerm' ::
    (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
    {-# UNPACK #-} !CachedInfo ->
    !(Term FPRoundingMode) ->
    !(Term (FP eb sb)) ->
    !(Term (FP eb sb)) ->
    !(Term (FP eb sb)) ->
    Term (FP eb sb)
  FromIntegralTerm' ::
    (PEvalFromIntegralTerm a b, SupportedPrim b) =>
    {-# UNPACK #-} !CachedInfo ->
    !(Term a) ->
    Term b
  FromFPOrTerm' ::
    ( PEvalIEEEFPConvertibleTerm a,
      ValidFP eb sb,
      SupportedPrim a
    ) =>
    {-# UNPACK #-} !CachedInfo ->
    !(Term a) ->
    !(Term FPRoundingMode) ->
    !(Term (FP eb sb)) ->
    Term a
  ToFPTerm' ::
    ( PEvalIEEEFPConvertibleTerm a,
      ValidFP eb sb,
      SupportedPrim (FP eb sb)
    ) =>
    {-# UNPACK #-} !CachedInfo ->
    !(Term FPRoundingMode) ->
    !(Term a) ->
    Proxy eb ->
    Proxy sb ->
    Term (FP eb sb)

data SupportedPrimEvidence t where
  SupportedPrimEvidence :: (SupportedPrim t) => SupportedPrimEvidence t

pattern SupportedTerm :: forall t. () => (SupportedPrim t) => Term t
pattern SupportedTerm <-
  ( ( \v ->
        introSupportedPrimConstraint v $
          Just (SupportedPrimEvidence @t)
    ) ->
      Just SupportedPrimEvidence
    )

#if MIN_VERSION_base(4, 16, 4)
{-# COMPLETE SupportedTerm #-}
#endif

data SupportedTypedSymbolEvidence (k :: SymbolKind) t where
  SupportedTypedSymbolEvidence ::
    forall k t.
    (SupportedPrim t, SymbolKindConstraint k t, IsSymbolKind k) =>
    SupportedTypedSymbolEvidence k t

supportedTypedSymbolViewPat ::
  TypedSymbol k t -> Maybe (SupportedTypedSymbolEvidence k t)
supportedTypedSymbolViewPat (TypedSymbol _) = Just SupportedTypedSymbolEvidence

pattern SupportedTypedSymbol ::
  forall (k :: SymbolKind) t.
  () =>
  (SupportedPrim t, SymbolKindConstraint k t, IsSymbolKind k) =>
  TypedSymbol k t
pattern SupportedTypedSymbol <-
  (supportedTypedSymbolViewPat -> Just SupportedTypedSymbolEvidence)

#if MIN_VERSION_base(4, 16, 4)
{-# COMPLETE SupportedTypedSymbol #-}
#endif

data SupportedConstantTypedSymbolEvidence k t where
  SupportedConstantTypedSymbolEvidence ::
    forall k t.
    ( SupportedPrim t,
      SymbolKindConstraint k t,
      IsSymbolKind k,
      k ~ 'ConstantKind
    ) =>
    SupportedConstantTypedSymbolEvidence k t

supportedConstantTypedSymbolViewPat ::
  forall k t.
  TypedSymbol k t -> Maybe (SupportedConstantTypedSymbolEvidence k t)
supportedConstantTypedSymbolViewPat (TypedSymbol _) =
  case decideSymbolKind @k of
    Left HRefl -> Just SupportedConstantTypedSymbolEvidence
    Right _ -> Nothing

pattern SupportedConstantTypedSymbol ::
  forall k t.
  () =>
  ( SupportedPrim t,
    SymbolKindConstraint k t,
    IsSymbolKind k,
    k ~ 'ConstantKind
  ) =>
  TypedSymbol k t
pattern SupportedConstantTypedSymbol <-
  ( supportedConstantTypedSymbolViewPat ->
      Just SupportedConstantTypedSymbolEvidence
    )

pattern ConTerm :: forall t. () => (SupportedPrim t) => t -> Term t
pattern ConTerm t <- (ConTerm' _ t)
  where
    ConTerm = conTerm

pattern SymTerm ::
  forall t. () => (SupportedPrim t) => TypedSymbol 'AnyKind t -> Term t
pattern SymTerm t <- (SymTerm' _ t@SupportedTypedSymbol)
  where
    SymTerm = symTerm

pattern ForallTerm ::
  forall r.
  () =>
  forall t.
  (r ~ Bool, SupportedNonFuncPrim t) =>
  TypedSymbol 'ConstantKind t ->
  Term Bool ->
  Term r
pattern ForallTerm sym body <-
  (ForallTerm' _ sym@SupportedConstantTypedSymbol body)
  where
    ForallTerm = forallTerm

pattern ExistsTerm ::
  forall r.
  () =>
  forall t.
  (r ~ Bool, SupportedNonFuncPrim t) =>
  TypedSymbol 'ConstantKind t ->
  Term Bool ->
  Term r
pattern ExistsTerm sym body <-
  (ExistsTerm' _ sym@SupportedConstantTypedSymbol body)
  where
    ExistsTerm = existsTerm

pattern NotTerm :: forall r. () => (r ~ Bool) => Term Bool -> Term r
pattern NotTerm body <- (NotTerm' _ body)
  where
    NotTerm = notTerm

pattern OrTerm :: forall r. () => (r ~ Bool) => Term Bool -> Term Bool -> Term r
pattern OrTerm l r <- (OrTerm' _ l r)
  where
    OrTerm = orTerm

pattern AndTerm :: forall r. () => (r ~ Bool) => Term Bool -> Term Bool -> Term r
pattern AndTerm l r <- (AndTerm' _ l r)
  where
    AndTerm = andTerm

pattern EqTerm ::
  forall r.
  () =>
  forall t.
  (r ~ Bool, SupportedPrim t) =>
  Term t -> Term t -> Term r
pattern EqTerm l r <- (EqTerm' _ l r@SupportedTerm)
  where
    EqTerm = eqTerm

pattern DistinctTerm ::
  forall r.
  () =>
  forall t.
  (r ~ Bool, SupportedPrim t) =>
  NonEmpty (Term t) -> Term r
pattern DistinctTerm ts <- (DistinctTerm' _ ts@(SupportedTerm :| _))
  where
    DistinctTerm = distinctTerm

pattern ITETerm ::
  forall t.
  () =>
  (SupportedPrim t) =>
  Term Bool ->
  Term t ->
  Term t ->
  Term t
pattern ITETerm cond t1 t2 <- (ITETerm' _ cond t1 t2)
  where
    ITETerm = iteTerm

pattern AddNumTerm ::
  forall t.
  () =>
  (SupportedPrim t, PEvalNumTerm t) =>
  Term t ->
  Term t ->
  Term t
pattern AddNumTerm l r <- (AddNumTerm' _ l r)
  where
    AddNumTerm = addNumTerm

pattern NegNumTerm ::
  forall t.
  () =>
  (SupportedPrim t, PEvalNumTerm t) =>
  Term t -> Term t
pattern NegNumTerm t <- (NegNumTerm' _ t)
  where
    NegNumTerm = negNumTerm

pattern MulNumTerm ::
  forall t.
  () =>
  (SupportedPrim t, PEvalNumTerm t) =>
  Term t -> Term t -> Term t
pattern MulNumTerm l r <- (MulNumTerm' _ l r)
  where
    MulNumTerm = mulNumTerm

pattern AbsNumTerm ::
  forall t.
  () =>
  (SupportedPrim t, PEvalNumTerm t) =>
  Term t -> Term t
pattern AbsNumTerm t <- (AbsNumTerm' _ t)
  where
    AbsNumTerm = absNumTerm

pattern SignumNumTerm ::
  forall t.
  () =>
  (SupportedPrim t, PEvalNumTerm t) =>
  Term t -> Term t
pattern SignumNumTerm t <- (SignumNumTerm' _ t)
  where
    SignumNumTerm = signumNumTerm

pattern LtOrdTerm ::
  forall r.
  () =>
  forall t.
  (r ~ Bool, SupportedPrim t, PEvalOrdTerm t) =>
  Term t -> Term t -> Term r
pattern LtOrdTerm l r <- (LtOrdTerm' _ l r@SupportedTerm)
  where
    LtOrdTerm = ltOrdTerm

pattern LeOrdTerm ::
  forall r.
  () =>
  forall t.
  (r ~ Bool, SupportedPrim t, PEvalOrdTerm t) =>
  Term t -> Term t -> Term r
pattern LeOrdTerm l r <- (LeOrdTerm' _ l r@SupportedTerm)
  where
    LeOrdTerm = leOrdTerm

pattern AndBitsTerm ::
  forall t.
  () =>
  (SupportedPrim t, PEvalBitwiseTerm t) =>
  Term t -> Term t -> Term t
pattern AndBitsTerm l r <- (AndBitsTerm' _ l r)
  where
    AndBitsTerm = andBitsTerm

pattern OrBitsTerm ::
  forall t.
  () =>
  (SupportedPrim t, PEvalBitwiseTerm t) =>
  Term t -> Term t -> Term t
pattern OrBitsTerm l r <- (OrBitsTerm' _ l r)
  where
    OrBitsTerm = orBitsTerm

pattern XorBitsTerm ::
  forall t.
  () =>
  (SupportedPrim t, PEvalBitwiseTerm t) =>
  Term t -> Term t -> Term t
pattern XorBitsTerm l r <- (XorBitsTerm' _ l r)
  where
    XorBitsTerm = xorBitsTerm

pattern ComplementBitsTerm ::
  forall t.
  () =>
  (SupportedPrim t, PEvalBitwiseTerm t) =>
  Term t -> Term t
pattern ComplementBitsTerm t <- (ComplementBitsTerm' _ t)
  where
    ComplementBitsTerm = complementBitsTerm

pattern ShiftLeftTerm ::
  forall t.
  () =>
  (SupportedPrim t, PEvalShiftTerm t) =>
  Term t -> Term t -> Term t
pattern ShiftLeftTerm l r <- (ShiftLeftTerm' _ l r)
  where
    ShiftLeftTerm = shiftLeftTerm

pattern ShiftRightTerm ::
  forall t.
  () =>
  (SupportedPrim t, PEvalShiftTerm t) =>
  Term t -> Term t -> Term t
pattern ShiftRightTerm l r <- (ShiftRightTerm' _ l r)
  where
    ShiftRightTerm = shiftRightTerm

pattern RotateLeftTerm ::
  forall t.
  () =>
  (SupportedPrim t, PEvalRotateTerm t) =>
  Term t -> Term t -> Term t
pattern RotateLeftTerm l r <- (RotateLeftTerm' _ l r)
  where
    RotateLeftTerm = rotateLeftTerm

pattern RotateRightTerm ::
  forall t.
  () =>
  (SupportedPrim t, PEvalRotateTerm t) =>
  Term t -> Term t -> Term t
pattern RotateRightTerm l r <- (RotateRightTerm' _ l r)
  where
    RotateRightTerm = rotateRightTerm

pattern BitCastTerm ::
  forall b.
  () =>
  forall a.
  (SupportedPrim a, SupportedPrim b, PEvalBitCastTerm a b) =>
  Term a -> Term b
pattern BitCastTerm t <- (BitCastTerm' _ t@SupportedTerm)
  where
    BitCastTerm = bitCastTerm

pattern BitCastOrTerm ::
  forall b.
  () =>
  forall a.
  (SupportedPrim a, SupportedPrim b, PEvalBitCastOrTerm a b) =>
  Term b -> Term a -> Term b
pattern BitCastOrTerm t1 t2 <- (BitCastOrTerm' _ t1 t2@SupportedTerm)
  where
    BitCastOrTerm = bitCastOrTerm

pattern BVConcatTerm ::
  forall ret.
  () =>
  forall bv l r.
  ( PEvalBVTerm bv,
    KnownNat l,
    KnownNat r,
    KnownNat (l + r),
    1 <= l,
    1 <= r,
    1 <= l + r,
    SupportedPrim (bv l),
    SupportedPrim (bv r),
    SupportedPrim (bv (l + r)),
    ret ~ bv (l + r)
  ) =>
  Term (bv l) -> Term (bv r) -> Term ret
pattern BVConcatTerm l r <- (BVConcatTerm' _ l@SupportedTerm r@SupportedTerm)
  where
    BVConcatTerm = bvConcatTerm

pattern BVSelectTerm ::
  forall ret.
  () =>
  forall bv w n ix.
  ( PEvalBVTerm bv,
    KnownNat n,
    KnownNat ix,
    KnownNat w,
    1 <= n,
    1 <= w,
    ix + w <= n,
    SupportedPrim (bv n),
    SupportedPrim (bv w),
    ret ~ bv w
  ) =>
  Proxy ix -> Proxy w -> Term (bv n) -> Term ret
pattern BVSelectTerm ix w t <- (BVSelectTerm' _ ix w t@SupportedTerm)
  where
    BVSelectTerm = bvSelectTerm

pattern BVExtendTerm ::
  forall ret.
  () =>
  forall bv l r.
  ( PEvalBVTerm bv,
    KnownNat l,
    KnownNat r,
    1 <= l,
    1 <= r,
    l <= r,
    SupportedPrim (bv l),
    SupportedPrim (bv r),
    ret ~ bv r
  ) =>
  Bool -> Proxy r -> Term (bv l) -> Term ret
pattern BVExtendTerm signed p t <- (BVExtendTerm' _ signed p t@SupportedTerm)
  where
    BVExtendTerm = bvExtendTerm

pattern ApplyTerm ::
  forall b.
  () =>
  forall f a.
  (PEvalApplyTerm f a b, SupportedPrim f, SupportedPrim a, SupportedPrim b) =>
  Term f -> Term a -> Term b
pattern ApplyTerm f x <- (ApplyTerm' _ f@SupportedTerm x@SupportedTerm)
  where
    ApplyTerm = applyTerm

pattern DivIntegralTerm ::
  forall t.
  () =>
  (SupportedPrim t, PEvalDivModIntegralTerm t) =>
  Term t -> Term t -> Term t
pattern DivIntegralTerm l r <- (DivIntegralTerm' _ l r)
  where
    DivIntegralTerm = divIntegralTerm

pattern ModIntegralTerm ::
  forall t.
  () =>
  (SupportedPrim t, PEvalDivModIntegralTerm t) =>
  Term t -> Term t -> Term t
pattern ModIntegralTerm l r <- (ModIntegralTerm' _ l r)
  where
    ModIntegralTerm = modIntegralTerm

pattern QuotIntegralTerm ::
  forall t.
  () =>
  (SupportedPrim t, PEvalDivModIntegralTerm t) =>
  Term t -> Term t -> Term t
pattern QuotIntegralTerm l r <- (QuotIntegralTerm' _ l r)
  where
    QuotIntegralTerm = quotIntegralTerm

pattern RemIntegralTerm ::
  forall t.
  () =>
  (SupportedPrim t, PEvalDivModIntegralTerm t) =>
  Term t -> Term t -> Term t
pattern RemIntegralTerm l r <- (RemIntegralTerm' _ l r)
  where
    RemIntegralTerm = remIntegralTerm

pattern FPTraitTerm ::
  forall r.
  () =>
  forall eb sb.
  (r ~ Bool, ValidFP eb sb, SupportedPrim (FP eb sb)) =>
  FPTrait -> Term (FP eb sb) -> Term r
pattern FPTraitTerm trait t <- (FPTraitTerm' _ trait t)
  where
    FPTraitTerm = fpTraitTerm

pattern FdivTerm ::
  forall t.
  () =>
  (SupportedPrim t, PEvalFractionalTerm t) =>
  Term t -> Term t -> Term t
pattern FdivTerm l r <- (FdivTerm' _ l r)
  where
    FdivTerm = fdivTerm

pattern RecipTerm ::
  forall t.
  () =>
  (SupportedPrim t, PEvalFractionalTerm t) =>
  Term t -> Term t
pattern RecipTerm t <- (RecipTerm' _ t)
  where
    RecipTerm = recipTerm

pattern FloatingUnaryTerm ::
  forall t.
  () =>
  (SupportedPrim t, PEvalFloatingTerm t) =>
  FloatingUnaryOp -> Term t -> Term t
pattern FloatingUnaryTerm op t <- (FloatingUnaryTerm' _ op t)
  where
    FloatingUnaryTerm = floatingUnaryTerm

pattern PowerTerm ::
  forall t.
  () =>
  (SupportedPrim t, PEvalFloatingTerm t) =>
  Term t -> Term t -> Term t
pattern PowerTerm l r <- (PowerTerm' _ l r)
  where
    PowerTerm = powerTerm

pattern FPUnaryTerm ::
  forall ret.
  () =>
  forall eb sb.
  (ret ~ FP eb sb, ValidFP eb sb, SupportedPrim (FP eb sb)) =>
  FPUnaryOp -> Term (FP eb sb) -> Term ret
pattern FPUnaryTerm op t <- (FPUnaryTerm' _ op t)
  where
    FPUnaryTerm = fpUnaryTerm

pattern FPBinaryTerm ::
  forall ret.
  () =>
  forall eb sb.
  (ret ~ FP eb sb, ValidFP eb sb, SupportedPrim (FP eb sb)) =>
  FPBinaryOp -> Term (FP eb sb) -> Term (FP eb sb) -> Term ret
pattern FPBinaryTerm op l r <- (FPBinaryTerm' _ op l r)
  where
    FPBinaryTerm = fpBinaryTerm

pattern FPRoundingUnaryTerm ::
  forall ret.
  () =>
  forall eb sb.
  (ret ~ FP eb sb, ValidFP eb sb, SupportedPrim (FP eb sb)) =>
  FPRoundingUnaryOp -> Term FPRoundingMode -> Term (FP eb sb) -> Term ret
pattern FPRoundingUnaryTerm op rm t <- (FPRoundingUnaryTerm' _ op rm t)
  where
    FPRoundingUnaryTerm = fpRoundingUnaryTerm

pattern FPRoundingBinaryTerm ::
  forall ret.
  () =>
  forall eb sb.
  (ret ~ FP eb sb, ValidFP eb sb, SupportedPrim (FP eb sb)) =>
  FPRoundingBinaryOp -> Term FPRoundingMode -> Term (FP eb sb) -> Term (FP eb sb) -> Term ret
pattern FPRoundingBinaryTerm op rm l r <- (FPRoundingBinaryTerm' _ op rm l r)
  where
    FPRoundingBinaryTerm = fpRoundingBinaryTerm

pattern FPFMATerm ::
  forall ret.
  () =>
  forall eb sb.
  (ret ~ FP eb sb, ValidFP eb sb, SupportedPrim (FP eb sb)) =>
  Term FPRoundingMode -> Term (FP eb sb) -> Term (FP eb sb) -> Term (FP eb sb) -> Term ret
pattern FPFMATerm rm t1 t2 t3 <- (FPFMATerm' _ rm t1 t2 t3)
  where
    FPFMATerm = fpFMATerm

pattern FromIntegralTerm ::
  forall b.
  () =>
  forall a.
  (PEvalFromIntegralTerm a b, SupportedPrim a, SupportedPrim b) =>
  Term a -> Term b
pattern FromIntegralTerm t <- (FromIntegralTerm' _ t@SupportedTerm)
  where
    FromIntegralTerm = fromIntegralTerm

pattern FromFPOrTerm ::
  forall a.
  () =>
  forall eb sb.
  ( PEvalIEEEFPConvertibleTerm a,
    ValidFP eb sb,
    SupportedPrim a
  ) =>
  Term a -> Term FPRoundingMode -> Term (FP eb sb) -> Term a
pattern FromFPOrTerm t1 rm t2 <- (FromFPOrTerm' _ t1 rm t2)
  where
    FromFPOrTerm = fromFPOrTerm

pattern ToFPTerm ::
  forall ret.
  () =>
  forall eb sb a.
  ( PEvalIEEEFPConvertibleTerm a,
    ValidFP eb sb,
    SupportedPrim (FP eb sb),
    SupportedPrim a,
    ret ~ FP eb sb
  ) =>
  Term FPRoundingMode -> Term a -> Proxy eb -> Proxy sb -> Term ret
pattern ToFPTerm rm t eb sb <- (ToFPTerm' _ rm t@SupportedTerm eb sb)
  where
    ToFPTerm rm t _ _ = toFPTerm rm t

#if MIN_VERSION_base(4, 16, 4)
{-# COMPLETE
  ConTerm,
  SymTerm,
  ForallTerm,
  ExistsTerm,
  NotTerm,
  OrTerm,
  AndTerm,
  EqTerm,
  DistinctTerm,
  ITETerm,
  AddNumTerm,
  NegNumTerm,
  MulNumTerm,
  AbsNumTerm,
  SignumNumTerm,
  LtOrdTerm,
  LeOrdTerm,
  AndBitsTerm,
  OrBitsTerm,
  XorBitsTerm,
  ComplementBitsTerm,
  ShiftLeftTerm,
  ShiftRightTerm,
  RotateLeftTerm,
  RotateRightTerm,
  BitCastTerm,
  BitCastOrTerm,
  BVConcatTerm,
  BVSelectTerm,
  BVExtendTerm,
  ApplyTerm,
  DivIntegralTerm,
  ModIntegralTerm,
  QuotIntegralTerm,
  RemIntegralTerm,
  FPTraitTerm,
  FdivTerm,
  RecipTerm,
  FloatingUnaryTerm,
  PowerTerm,
  FPUnaryTerm,
  FPBinaryTerm,
  FPRoundingUnaryTerm,
  FPRoundingBinaryTerm,
  FPFMATerm,
  FromIntegralTerm,
  FromFPOrTerm,
  ToFPTerm
  #-}
#endif

termInfo :: Term t -> CachedInfo
termInfo (ConTerm' i _) = i
termInfo (SymTerm' i _) = i
termInfo (ForallTerm' i _ _) = i
termInfo (ExistsTerm' i _ _) = i
termInfo (NotTerm' i _) = i
termInfo (OrTerm' i _ _) = i
termInfo (AndTerm' i _ _) = i
termInfo (EqTerm' i _ _) = i
termInfo (DistinctTerm' i _) = i
termInfo (ITETerm' i _ _ _) = i
termInfo (AddNumTerm' i _ _) = i
termInfo (NegNumTerm' i _) = i
termInfo (MulNumTerm' i _ _) = i
termInfo (AbsNumTerm' i _) = i
termInfo (SignumNumTerm' i _) = i
termInfo (LtOrdTerm' i _ _) = i
termInfo (LeOrdTerm' i _ _) = i
termInfo (AndBitsTerm' i _ _) = i
termInfo (OrBitsTerm' i _ _) = i
termInfo (XorBitsTerm' i _ _) = i
termInfo (ComplementBitsTerm' i _) = i
termInfo (ShiftLeftTerm' i _ _) = i
termInfo (ShiftRightTerm' i _ _) = i
termInfo (RotateLeftTerm' i _ _) = i
termInfo (RotateRightTerm' i _ _) = i
termInfo (BitCastTerm' i _) = i
termInfo (BitCastOrTerm' i _ _) = i
termInfo (BVConcatTerm' i _ _) = i
termInfo (BVSelectTerm' i _ _ _) = i
termInfo (BVExtendTerm' i _ _ _) = i
termInfo (ApplyTerm' i _ _) = i
termInfo (DivIntegralTerm' i _ _) = i
termInfo (ModIntegralTerm' i _ _) = i
termInfo (QuotIntegralTerm' i _ _) = i
termInfo (RemIntegralTerm' i _ _) = i
termInfo (FPTraitTerm' i _ _) = i
termInfo (FdivTerm' i _ _) = i
termInfo (RecipTerm' i _) = i
termInfo (FloatingUnaryTerm' i _ _) = i
termInfo (PowerTerm' i _ _) = i
termInfo (FPUnaryTerm' i _ _) = i
termInfo (FPBinaryTerm' i _ _ _) = i
termInfo (FPRoundingUnaryTerm' i _ _ _) = i
termInfo (FPRoundingBinaryTerm' i _ _ _ _) = i
termInfo (FPFMATerm' i _ _ _ _) = i
termInfo (FromIntegralTerm' i _) = i
termInfo (FromFPOrTerm' i _ _ _) = i
termInfo (ToFPTerm' i _ _ _ _) = i

{-# INLINE termThreadId #-}
termThreadId :: Term t -> WeakThreadId
termThreadId = cachedThreadId . termInfo

{-# INLINE termDigest #-}
termDigest :: Term t -> Digest
termDigest = cachedDigest . termInfo

{-# INLINE termId #-}
termId :: Term t -> Id
termId = cachedId . termInfo

{-# INLINE termStableIdent #-}
termStableIdent :: Term t -> StableIdent
termStableIdent = cachedStableIdent . termInfo

-- | Pattern for term with dynamic typing.
pattern DynTerm :: forall a b. (SupportedPrim a) => Term a -> Term b
pattern DynTerm x <- ((\v@SupportedTerm -> cast v) -> Just x)

-- baseHash :: Term t -> Digest
-- baseHash t = case hashId t of
--   HashId h _ -> h
-- {-# INLINE baseHash #-}

data HashId = HashId {-# UNPACK #-} !Digest Id deriving (Show)

instance Eq HashId where
  HashId _ l == HashId _ r = l == r
  {-# INLINE (==) #-}

instance Hashable HashId where
  hashWithSalt s (HashId i _) = hashWithSalt s i
  {-# INLINE hashWithSalt #-}

eqHashId :: HashId -> HashId -> Bool
eqHashId = (==)
{-# INLINE eqHashId #-}

data TypeHashId = TypeHashId {-# UNPACK #-} !Fingerprint {-# UNPACK #-} !HashId
  deriving (Show)

instance Eq TypeHashId where
  TypeHashId l li == TypeHashId r ri = l == r && li == ri
  {-# INLINE (==) #-}

instance Hashable TypeHashId where
  hashWithSalt s (TypeHashId tp i) = s `hashWithSalt` tp `hashWithSalt` i
  {-# INLINE hashWithSalt #-}

{-# INLINE termHashId #-}
termHashId :: Term t -> HashId
termHashId t = HashId (termDigest t) (termId t)

typeFingerprint :: forall t. (SupportedPrim t) => Fingerprint
typeFingerprint = typeRepFingerprint $ SomeTypeRep $ primTypeRep @t
{-# INLINE typeFingerprint #-}

{-# INLINE termTypeHashId #-}
termTypeHashId :: forall t. Term t -> TypeHashId
termTypeHashId t@SupportedTerm = TypeHashId (typeFingerprint @t) (termHashId t)

-- {-# NOINLINE typeHashId #-}
introSupportedPrimConstraint0 :: forall t a. Term t -> ((SupportedPrim t) => a) -> a
introSupportedPrimConstraint0 ConTerm' {} x = x
introSupportedPrimConstraint0 (SymTerm' _ t) x = withSymbolSupported t x
introSupportedPrimConstraint0 ForallTerm' {} x = x
introSupportedPrimConstraint0 ExistsTerm' {} x = x
introSupportedPrimConstraint0 NotTerm' {} x = x
introSupportedPrimConstraint0 OrTerm' {} x = x
introSupportedPrimConstraint0 AndTerm' {} x = x
introSupportedPrimConstraint0 EqTerm' {} x = x
introSupportedPrimConstraint0 DistinctTerm' {} x = x
introSupportedPrimConstraint0 ITETerm' {} x = x
introSupportedPrimConstraint0 AddNumTerm' {} x = x
introSupportedPrimConstraint0 NegNumTerm' {} x = x
introSupportedPrimConstraint0 MulNumTerm' {} x = x
introSupportedPrimConstraint0 AbsNumTerm' {} x = x
introSupportedPrimConstraint0 SignumNumTerm' {} x = x
introSupportedPrimConstraint0 LtOrdTerm' {} x = x
introSupportedPrimConstraint0 LeOrdTerm' {} x = x
introSupportedPrimConstraint0 AndBitsTerm' {} x = x
introSupportedPrimConstraint0 OrBitsTerm' {} x = x
introSupportedPrimConstraint0 XorBitsTerm' {} x = x
introSupportedPrimConstraint0 ComplementBitsTerm' {} x = x
introSupportedPrimConstraint0 ShiftLeftTerm' {} x = x
introSupportedPrimConstraint0 RotateLeftTerm' {} x = x
introSupportedPrimConstraint0 ShiftRightTerm' {} x = x
introSupportedPrimConstraint0 RotateRightTerm' {} x = x
introSupportedPrimConstraint0 BitCastTerm' {} x = x
introSupportedPrimConstraint0 BitCastOrTerm' {} x = x
introSupportedPrimConstraint0 BVConcatTerm' {} x = x
introSupportedPrimConstraint0 BVSelectTerm' {} x = x
introSupportedPrimConstraint0 BVExtendTerm' {} x = x
introSupportedPrimConstraint0 ApplyTerm' {} x = x
introSupportedPrimConstraint0 DivIntegralTerm' {} x = x
introSupportedPrimConstraint0 ModIntegralTerm' {} x = x
introSupportedPrimConstraint0 QuotIntegralTerm' {} x = x
introSupportedPrimConstraint0 RemIntegralTerm' {} x = x
introSupportedPrimConstraint0 FPTraitTerm' {} x = x
introSupportedPrimConstraint0 FdivTerm' {} x = x
introSupportedPrimConstraint0 RecipTerm' {} x = x
introSupportedPrimConstraint0 FloatingUnaryTerm' {} x = x
introSupportedPrimConstraint0 PowerTerm' {} x = x
introSupportedPrimConstraint0 FPUnaryTerm' {} x = x
introSupportedPrimConstraint0 FPBinaryTerm' {} x = x
introSupportedPrimConstraint0 FPRoundingUnaryTerm' {} x = x
introSupportedPrimConstraint0 FPRoundingBinaryTerm' {} x = x
introSupportedPrimConstraint0 FPFMATerm' {} x = x
introSupportedPrimConstraint0 FromIntegralTerm' {} x = x
introSupportedPrimConstraint0 FromFPOrTerm' {} x = x
introSupportedPrimConstraint0 ToFPTerm' {} x = x

-- | Introduce the 'SupportedPrim' constraint from a term.
introSupportedPrimConstraint ::
  forall t a. Term t -> ((SupportedPrim t, Typeable t) => a) -> a
introSupportedPrimConstraint t a = introSupportedPrimConstraint0 t a
{-# INLINE introSupportedPrimConstraint #-}

-- | Pretty-print a term.
pformatTerm :: forall t. Term t -> String
pformatTerm (ConTerm t) = pformatCon t
pformatTerm (SymTerm sym) = showUntyped sym
pformatTerm (ForallTerm sym arg) = "(forall " ++ show sym ++ " " ++ pformatTerm arg ++ ")"
pformatTerm (ExistsTerm sym arg) = "(exists " ++ show sym ++ " " ++ pformatTerm arg ++ ")"
pformatTerm (NotTerm arg) = "(! " ++ pformatTerm arg ++ ")"
pformatTerm (OrTerm arg1 arg2) = "(|| " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (AndTerm arg1 arg2) = "(&& " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (EqTerm arg1 arg2) = "(= " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (DistinctTerm args) = "(distinct " ++ unwords (map pformatTerm $ toList args) ++ ")"
pformatTerm (ITETerm cond arg1 arg2) = "(ite " ++ pformatTerm cond ++ " " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (AddNumTerm arg1 arg2) = "(+ " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (NegNumTerm arg) = "(- " ++ pformatTerm arg ++ ")"
pformatTerm (MulNumTerm arg1 arg2) = "(* " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (AbsNumTerm arg) = "(abs " ++ pformatTerm arg ++ ")"
pformatTerm (SignumNumTerm arg) = "(signum " ++ pformatTerm arg ++ ")"
pformatTerm (LtOrdTerm arg1 arg2) = "(< " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (LeOrdTerm arg1 arg2) = "(<= " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (AndBitsTerm arg1 arg2) = "(& " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (OrBitsTerm arg1 arg2) = "(| " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (XorBitsTerm arg1 arg2) = "(^ " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (ComplementBitsTerm arg) = "(~ " ++ pformatTerm arg ++ ")"
pformatTerm (ShiftLeftTerm arg n) = "(shl " ++ pformatTerm arg ++ " " ++ pformatTerm n ++ ")"
pformatTerm (ShiftRightTerm arg n) = "(shr " ++ pformatTerm arg ++ " " ++ pformatTerm n ++ ")"
pformatTerm (RotateLeftTerm arg n) = "(rotl " ++ pformatTerm arg ++ " " ++ pformatTerm n ++ ")"
pformatTerm (RotateRightTerm arg n) = "(rotr " ++ pformatTerm arg ++ " " ++ pformatTerm n ++ ")"
pformatTerm (BitCastTerm arg) = "(bitcast " ++ pformatTerm arg ++ ")"
pformatTerm (BitCastOrTerm d arg) = "(bitcast_or " ++ pformatTerm d ++ " " ++ pformatTerm arg ++ ")"
pformatTerm (BVConcatTerm arg1 arg2) = "(bvconcat " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (BVSelectTerm (_ :: Proxy ix) (_ :: Proxy w) arg) =
  "(bvselect " ++ show (typeRep @ix) ++ " " ++ show (typeRep @w) ++ " " ++ pformatTerm arg ++ ")"
pformatTerm (BVExtendTerm signed (_ :: Proxy n) arg) =
  (if signed then "(bvsext " else "(bvzext ") ++ show (typeRep @n) ++ " " ++ pformatTerm arg ++ ")"
pformatTerm (ApplyTerm func arg) = "(apply " ++ pformatTerm func ++ " " ++ pformatTerm arg ++ ")"
pformatTerm (DivIntegralTerm arg1 arg2) = "(div " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (ModIntegralTerm arg1 arg2) = "(mod " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (QuotIntegralTerm arg1 arg2) = "(quot " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (RemIntegralTerm arg1 arg2) = "(rem " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (FPTraitTerm trait arg) = "(" ++ show trait ++ " " ++ pformatTerm arg ++ ")"
pformatTerm (FdivTerm arg1 arg2) = "(fdiv " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (RecipTerm arg) = "(recip " ++ pformatTerm arg ++ ")"
pformatTerm (FloatingUnaryTerm op arg) = "(" ++ show op ++ " " ++ pformatTerm arg ++ ")"
pformatTerm (PowerTerm arg1 arg2) = "(** " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (FPUnaryTerm op arg) = "(" ++ show op ++ " " ++ pformatTerm arg ++ ")"
pformatTerm (FPBinaryTerm op arg1 arg2) = "(" ++ show op ++ " " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (FPRoundingUnaryTerm op mode arg) = "(" ++ show op ++ " " ++ pformatTerm mode ++ " " ++ pformatTerm arg ++ ")"
pformatTerm (FPRoundingBinaryTerm op mode arg1 arg2) =
  "(" ++ show op ++ " " ++ pformatTerm mode ++ " " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ ")"
pformatTerm (FPFMATerm mode arg1 arg2 arg3) =
  "(fp.fma " ++ pformatTerm mode ++ " " ++ pformatTerm arg1 ++ " " ++ pformatTerm arg2 ++ " " ++ pformatTerm arg3 ++ ")"
pformatTerm (FromIntegralTerm arg) = "(from_integral " ++ pformatTerm arg ++ ")"
pformatTerm (FromFPOrTerm d r arg) = "(from_fp_or " ++ pformatTerm d ++ " " ++ pformatTerm r ++ " " ++ pformatTerm arg ++ ")"
pformatTerm (ToFPTerm r arg _ _) = "(to_fp " ++ pformatTerm r ++ " " ++ pformatTerm arg ++ ")"

-- {-# INLINE pformatTerm #-}

instance NFData (Term a) where
  rnf i = rnf (termInfo i)
  {-# INLINE rnf #-}

#if MIN_VERSION_base(4,15,0)
type CODE x = forall qq. Quote qq => Code qq (x)
#else
type CODE x = TExpQ x
#endif

instance Lift (Term t) where
  liftTyped (ConTerm v) = [||conTerm v||]
  liftTyped (SymTerm t) = [||symTerm t||]
  liftTyped (ForallTerm t1 t2) = [||forallTerm t1 t2||]
  liftTyped (ExistsTerm t1 t2) = [||existsTerm t1 t2||]
  liftTyped (NotTerm t) = [||notTerm t||]
  liftTyped (OrTerm t1 t2) = [||orTerm t1 t2||]
  liftTyped (AndTerm t1 t2) = [||andTerm t1 t2||]
  liftTyped (EqTerm t1 t2) = [||eqTerm t1 t2||]
  liftTyped (DistinctTerm t) = [||distinctTerm t||]
  liftTyped (ITETerm t1 t2 t3) = [||iteTerm t1 t2 t3||]
  liftTyped (AddNumTerm t1 t2) = [||addNumTerm t1 t2||]
  liftTyped (NegNumTerm t) = [||negNumTerm t||]
  liftTyped (MulNumTerm t1 t2) = [||mulNumTerm t1 t2||]
  liftTyped (AbsNumTerm t) = [||absNumTerm t||]
  liftTyped (SignumNumTerm t) = [||signumNumTerm t||]
  liftTyped (LtOrdTerm t1 t2) = [||ltOrdTerm t1 t2||]
  liftTyped (LeOrdTerm t1 t2) = [||leOrdTerm t1 t2||]
  liftTyped (AndBitsTerm t1 t2) = [||andBitsTerm t1 t2||]
  liftTyped (OrBitsTerm t1 t2) = [||orBitsTerm t1 t2||]
  liftTyped (XorBitsTerm t1 t2) = [||xorBitsTerm t1 t2||]
  liftTyped (ComplementBitsTerm t) = [||complementBitsTerm t||]
  liftTyped (ShiftLeftTerm t1 t2) = [||shiftLeftTerm t1 t2||]
  liftTyped (ShiftRightTerm t1 t2) = [||shiftRightTerm t1 t2||]
  liftTyped (RotateLeftTerm t1 t2) = [||rotateLeftTerm t1 t2||]
  liftTyped (RotateRightTerm t1 t2) = [||rotateRightTerm t1 t2||]
  liftTyped (BitCastTerm t) = [||bitCastTerm t||]
  liftTyped (BitCastOrTerm t1 t2) = [||bitCastOrTerm t1 t2||]
  liftTyped (BVConcatTerm t1 t2) = [||bvConcatTerm t1 t2||]
  liftTyped (BVSelectTerm (_ :: p ix) (_ :: q w) t3) =
    let pix = [||Proxy||] :: CODE (Proxy ix)
        pw = [||Proxy||] :: CODE (Proxy w)
     in [||bvSelectTerm $$pix $$pw t3||]
  liftTyped (BVExtendTerm b (_ :: p r) t2) =
    let pr = [||Proxy||] :: CODE (Proxy r)
     in [||bvExtendTerm b $$pr t2||]
  liftTyped (ApplyTerm t1 t2) = [||applyTerm t1 t2||]
  liftTyped (DivIntegralTerm t1 t2) = [||divIntegralTerm t1 t2||]
  liftTyped (ModIntegralTerm t1 t2) = [||modIntegralTerm t1 t2||]
  liftTyped (QuotIntegralTerm t1 t2) = [||quotIntegralTerm t1 t2||]
  liftTyped (RemIntegralTerm t1 t2) = [||remIntegralTerm t1 t2||]
  liftTyped (FPTraitTerm t1 t2) = [||fpTraitTerm t1 t2||]
  liftTyped (FdivTerm t1 t2) = [||fdivTerm t1 t2||]
  liftTyped (RecipTerm t) = [||recipTerm t||]
  liftTyped (FloatingUnaryTerm t1 t2) = [||floatingUnaryTerm t1 t2||]
  liftTyped (PowerTerm t1 t2) = [||powerTerm t1 t2||]
  liftTyped (FPUnaryTerm t1 t2) = [||fpUnaryTerm t1 t2||]
  liftTyped (FPBinaryTerm t1 t2 t3) = [||fpBinaryTerm t1 t2 t3||]
  liftTyped (FPRoundingUnaryTerm t1 t2 t3) =
    [||fpRoundingUnaryTerm t1 t2 t3||]
  liftTyped (FPRoundingBinaryTerm t1 t2 t3 t4) =
    [||fpRoundingBinaryTerm t1 t2 t3 t4||]
  liftTyped (FPFMATerm t1 t2 t3 t4) = [||fpFMATerm t1 t2 t3 t4||]
  liftTyped (FromIntegralTerm t) = [||fromIntegralTerm t||]
  liftTyped (FromFPOrTerm t1 t2 t3) = [||fromFPOrTerm t1 t2 t3||]
  liftTyped (ToFPTerm t1 t2 _ _) =
    [||toFPTerm t1 t2||]

instance Show (Term ty) where
  show t@(ConTerm v) =
    "ConTerm{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", v="
      ++ pformatCon v
      ++ "}"
  show t@(SymTerm name@TypedSymbol {}) =
    "SymTerm{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", name="
      ++ show name
      ++ ", type="
      ++ show (primTypeRep @ty)
      ++ "}"
  show t@(ForallTerm sym arg) =
    "Forall{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", sym="
      ++ show sym
      ++ ", arg="
      ++ show arg
      ++ "}"
  show t@(ExistsTerm sym arg) =
    "Exists{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", sym="
      ++ show sym
      ++ ", arg="
      ++ show arg
      ++ "}"
  show t@(NotTerm arg) =
    "Not{tid=" ++ show (termThreadId t) ++ ", id=" ++ show (termId t) ++ ", arg=" ++ show arg ++ "}"
  show t@(OrTerm arg1 arg2) =
    "Or{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show t@(AndTerm arg1 arg2) =
    "And{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show t@(EqTerm arg1 arg2) =
    "Eqv{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show t@(DistinctTerm args) =
    "Distinct{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", args="
      ++ show args
      ++ "}"
  show t@(ITETerm cond l r) =
    "ITE{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", cond="
      ++ show cond
      ++ ", then="
      ++ show l
      ++ ", else="
      ++ show r
      ++ "}"
  show t@(AddNumTerm arg1 arg2) =
    "AddNum{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show t@(NegNumTerm arg) =
    "NegNum{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", arg="
      ++ show arg
      ++ "}"
  show t@(MulNumTerm arg1 arg2) =
    "MulNum{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show t@(AbsNumTerm arg) =
    "AbsNum{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", arg="
      ++ show arg
      ++ "}"
  show t@(SignumNumTerm arg) =
    "SignumNum{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", arg="
      ++ show arg
      ++ "}"
  show t@(LtOrdTerm arg1 arg2) =
    "LTNum{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show t@(LeOrdTerm arg1 arg2) =
    "LENum{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show t@(AndBitsTerm arg1 arg2) =
    "AndBits{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show t@(OrBitsTerm arg1 arg2) =
    "OrBits{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show t@(XorBitsTerm arg1 arg2) =
    "XorBits{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show t@(ComplementBitsTerm arg) =
    "ComplementBits{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", arg="
      ++ show arg
      ++ "}"
  show t@(ShiftLeftTerm arg n) =
    "ShiftLeft{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", arg="
      ++ show arg
      ++ ", n="
      ++ show n
      ++ "}"
  show t@(ShiftRightTerm arg n) =
    "ShiftRight{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", arg="
      ++ show arg
      ++ ", n="
      ++ show n
      ++ "}"
  show t@(RotateLeftTerm arg n) =
    "RotateLeft{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", arg="
      ++ show arg
      ++ ", n="
      ++ show n
      ++ "}"
  show t@(RotateRightTerm arg n) =
    "RotateRight{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", arg="
      ++ show arg
      ++ ", n="
      ++ show n
      ++ "}"
  show t@(BitCastTerm arg) =
    "BitCast{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", arg="
      ++ show arg
      ++ "}"
  show t@(BitCastOrTerm arg d) =
    "BitCastOr{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", default="
      ++ show d
      ++ ", arg="
      ++ show arg
      ++ "}"
  show t@(BVConcatTerm arg1 arg2) =
    "BVConcat{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show t@(BVSelectTerm arg ix w) =
    "BVSelect{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", ix="
      ++ show ix
      ++ ", w="
      ++ show w
      ++ ", arg="
      ++ show arg
      ++ "}"
  show t@(BVExtendTerm signed n arg) =
    "BVExtend{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", signed="
      ++ show signed
      ++ ", n="
      ++ show n
      ++ ", arg="
      ++ show arg
      ++ "}"
  show t@(ApplyTerm func arg) =
    "Apply{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", f="
      ++ show func
      ++ ", arg="
      ++ show arg
      ++ "}"
  show t@(DivIntegralTerm arg1 arg2) =
    "DivIntegral{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show t@(ModIntegralTerm arg1 arg2) =
    "ModIntegral{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show t@(QuotIntegralTerm arg1 arg2) =
    "QuotIntegral{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show t@(RemIntegralTerm arg1 arg2) =
    "RemIntegral{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show t@(FPTraitTerm trait arg) =
    "FPTrait{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", trait="
      ++ show trait
      ++ ", arg="
      ++ show arg
      ++ "}"
  show t@(FdivTerm arg1 arg2) =
    "Fdiv{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show t@(RecipTerm arg) =
    "Recip{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", arg="
      ++ show arg
      ++ "}"
  show t@(FloatingUnaryTerm op arg) =
    "FloatingUnary{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", op="
      ++ show op
      ++ ", arg="
      ++ show arg
      ++ "}"
  show t@(PowerTerm arg1 arg2) =
    "Power{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show t@(FPUnaryTerm op arg) =
    "FPUnary{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", op="
      ++ show op
      ++ ", arg="
      ++ show arg
      ++ "}"
  show t@(FPBinaryTerm op arg1 arg2) =
    "FPBinary{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", op="
      ++ show op
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show t@(FPRoundingUnaryTerm op mode arg) =
    "FPRoundingUnary{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", op="
      ++ show op
      ++ ", mode="
      ++ show mode
      ++ ", arg="
      ++ show arg
      ++ "}"
  show t@(FPRoundingBinaryTerm op mode arg1 arg2) =
    "FPRoundingBinary{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", op="
      ++ show op
      ++ ", mode="
      ++ show mode
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show t@(FPFMATerm mode arg1 arg2 arg3) =
    "FPFMA{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", mode="
      ++ show mode
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ ", arg3="
      ++ show arg3
      ++ "}"
  show t@(FromIntegralTerm arg) =
    "FromIntegral{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", arg="
      ++ show arg
      ++ "}"
  show t@(FromFPOrTerm arg d mode) =
    "FromFPTerm{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", default="
      ++ show d
      ++ ", mode="
      ++ show mode
      ++ ", arg="
      ++ show arg
      ++ "}"
  show t@(ToFPTerm arg mode _ _) =
    "ToFPTerm{tid="
      ++ show (termThreadId t)
      ++ ", id="
      ++ show (termId t)
      ++ ", mode="
      ++ show mode
      ++ ", arg="
      ++ show arg
      ++ "}"

-- {-# INLINE show #-}

-- | Pretty-print a term, possibly eliding parts of it.
prettyPrintTerm :: Term t -> Doc ann
prettyPrintTerm v@SupportedTerm =
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
    formatted = pformatTerm v
    len = length formatted

instance (SupportedPrim t) => Eq (Term t) where
  a == b =
    if threadId a == threadId b
      then termId a == termId b
      else unsafePerformIO $ do
        tid <- myWeakThreadId
        a' <- toCurThreadImpl tid a
        b' <- toCurThreadImpl tid b
        return $ a' == b'

instance (SupportedPrim t) => Hashable (Term t) where
  hashWithSalt s t = hashWithSalt s $ termHashId t
  {-# INLINE hashWithSalt #-}

-- | Term without identity (before internalizing).
data UTerm t where
  UConTerm :: (SupportedPrim t) => !t -> UTerm t
  USymTerm :: !(TypedSymbol 'AnyKind t) -> UTerm t
  UForallTerm ::
    !(TypedSymbol 'ConstantKind t) ->
    !(Term Bool) ->
    UTerm Bool
  UExistsTerm ::
    !(TypedSymbol 'ConstantKind t) ->
    !(Term Bool) ->
    UTerm Bool
  UNotTerm :: !(Term Bool) -> UTerm Bool
  UOrTerm :: !(Term Bool) -> !(Term Bool) -> UTerm Bool
  UAndTerm :: !(Term Bool) -> !(Term Bool) -> UTerm Bool
  UEqTerm :: !(Term t) -> !(Term t) -> UTerm Bool
  UDistinctTerm :: !(NonEmpty (Term t)) -> UTerm Bool
  UITETerm ::
    (SupportedPrim t) =>
    !(Term Bool) ->
    !(Term t) ->
    !(Term t) ->
    UTerm t
  UAddNumTerm :: (SupportedPrim t, PEvalNumTerm t) => !(Term t) -> !(Term t) -> UTerm t
  UNegNumTerm :: (SupportedPrim t, PEvalNumTerm t) => !(Term t) -> UTerm t
  UMulNumTerm :: (SupportedPrim t, PEvalNumTerm t) => !(Term t) -> !(Term t) -> UTerm t
  UAbsNumTerm :: (SupportedPrim t, PEvalNumTerm t) => !(Term t) -> UTerm t
  USignumNumTerm :: (SupportedPrim t, PEvalNumTerm t) => !(Term t) -> UTerm t
  ULtOrdTerm :: (SupportedPrim t, PEvalOrdTerm t) => !(Term t) -> !(Term t) -> UTerm Bool
  ULeOrdTerm :: (SupportedPrim t, PEvalOrdTerm t) => !(Term t) -> !(Term t) -> UTerm Bool
  UAndBitsTerm :: (SupportedPrim t, PEvalBitwiseTerm t) => !(Term t) -> !(Term t) -> UTerm t
  UOrBitsTerm :: (SupportedPrim t, PEvalBitwiseTerm t) => !(Term t) -> !(Term t) -> UTerm t
  UXorBitsTerm :: (SupportedPrim t, PEvalBitwiseTerm t) => !(Term t) -> !(Term t) -> UTerm t
  UComplementBitsTerm :: (SupportedPrim t, PEvalBitwiseTerm t) => !(Term t) -> UTerm t
  UShiftLeftTerm ::
    (SupportedPrim t, PEvalShiftTerm t) => !(Term t) -> !(Term t) -> UTerm t
  UShiftRightTerm ::
    (SupportedPrim t, PEvalShiftTerm t) => !(Term t) -> !(Term t) -> UTerm t
  URotateLeftTerm ::
    (SupportedPrim t, PEvalRotateTerm t) => !(Term t) -> !(Term t) -> UTerm t
  URotateRightTerm ::
    (SupportedPrim t, PEvalRotateTerm t) => !(Term t) -> !(Term t) -> UTerm t
  UBitCastTerm ::
    (SupportedPrim b, PEvalBitCastTerm a b) =>
    !(Term a) ->
    UTerm b
  UBitCastOrTerm ::
    (SupportedPrim b, PEvalBitCastOrTerm a b) =>
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
      1 <= l + r,
      SupportedPrim (bv (l + r))
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
      ix + w <= n,
      SupportedPrim (bv w)
    ) =>
    !(Proxy ix) ->
    !(Proxy w) ->
    !(Term (bv n)) ->
    UTerm (bv w)
  UBVExtendTerm ::
    ( PEvalBVTerm bv,
      KnownNat l,
      KnownNat r,
      1 <= l,
      1 <= r,
      l <= r,
      SupportedPrim (bv r)
    ) =>
    !Bool ->
    !(Proxy r) ->
    !(Term (bv l)) ->
    UTerm (bv r)
  UApplyTerm ::
    (PEvalApplyTerm f a b, SupportedPrim b) =>
    Term f ->
    Term a ->
    UTerm b
  UDivIntegralTerm ::
    (SupportedPrim t, PEvalDivModIntegralTerm t) =>
    !(Term t) ->
    !(Term t) ->
    UTerm t
  UModIntegralTerm ::
    (SupportedPrim t, PEvalDivModIntegralTerm t) =>
    !(Term t) ->
    !(Term t) ->
    UTerm t
  UQuotIntegralTerm ::
    (SupportedPrim t, PEvalDivModIntegralTerm t) =>
    !(Term t) ->
    !(Term t) ->
    UTerm t
  URemIntegralTerm ::
    (SupportedPrim t, PEvalDivModIntegralTerm t) =>
    !(Term t) ->
    !(Term t) ->
    UTerm t
  UFPTraitTerm ::
    (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
    !FPTrait ->
    !(Term (FP eb sb)) ->
    UTerm Bool
  UFdivTerm ::
    (SupportedPrim t, PEvalFractionalTerm t) =>
    !(Term t) ->
    !(Term t) ->
    UTerm t
  URecipTerm :: (SupportedPrim t, PEvalFractionalTerm t) => !(Term t) -> UTerm t
  UFloatingUnaryTerm ::
    (SupportedPrim t, PEvalFloatingTerm t) =>
    !FloatingUnaryOp ->
    !(Term t) ->
    UTerm t
  UPowerTerm ::
    (SupportedPrim t, PEvalFloatingTerm t) => !(Term t) -> !(Term t) -> UTerm t
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
    (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
    !FPRoundingUnaryOp ->
    !(Term FPRoundingMode) ->
    !(Term (FP eb sb)) ->
    UTerm (FP eb sb)
  UFPRoundingBinaryTerm ::
    (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
    !FPRoundingBinaryOp ->
    !(Term FPRoundingMode) ->
    !(Term (FP eb sb)) ->
    !(Term (FP eb sb)) ->
    UTerm (FP eb sb)
  UFPFMATerm ::
    (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
    !(Term FPRoundingMode) ->
    !(Term (FP eb sb)) ->
    !(Term (FP eb sb)) ->
    !(Term (FP eb sb)) ->
    UTerm (FP eb sb)
  UFromIntegralTerm ::
    (PEvalFromIntegralTerm a b, SupportedPrim b) =>
    !(Term a) ->
    UTerm b
  UFromFPOrTerm ::
    ( PEvalIEEEFPConvertibleTerm a,
      SupportedPrim a,
      ValidFP eb sb
    ) =>
    Term a ->
    !(Term FPRoundingMode) ->
    !(Term (FP eb sb)) ->
    UTerm a
  UToFPTerm ::
    ( PEvalIEEEFPConvertibleTerm a,
      ValidFP eb sb,
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
  case eqTypeRep (primTypeRep @a) (primTypeRep @b) of
    Just HRefl -> taga == tagb
    Nothing -> False
{-# INLINE eqHeteroSymbol #-}

preHashConDescription :: (SupportedPrim t) => t -> Digest
preHashConDescription = fromIntegral . hashConWithSalt 0
{-# INLINE preHashConDescription #-}

preHashSymDescription :: TypedSymbol 'AnyKind t -> Digest
preHashSymDescription = fromIntegral . hashWithSalt 1
{-# INLINE preHashSymDescription #-}

preHashForallDescription ::
  TypedSymbol 'ConstantKind t -> HashId -> Digest
preHashForallDescription sym h =
  fromIntegral
    ( 2
        `hashWithSalt` sym
        `hashWithSalt` h
    )
{-# INLINE preHashForallDescription #-}

preHashExistsDescription ::
  TypedSymbol 'ConstantKind t -> HashId -> Digest
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

preHashLtOrdDescription :: Fingerprint -> HashId -> HashId -> Digest
preHashLtOrdDescription tp h1 h2 =
  fromIntegral (18 `hashWithSalt` tp `hashWithSalt` h1 `hashWithSalt` h2)
{-# INLINE preHashLtOrdDescription #-}

preHashLeOrdDescription :: Fingerprint -> HashId -> HashId -> Digest
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

instance Interned (Term t) where
  type Uninterned (Term t) = UTerm t
  data Description (Term t) where
    DConTerm ::
      (t -> t -> Bool) -> {-# UNPACK #-} !Digest -> t -> Description (Term t)
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
      {-# UNPACK #-} !Fingerprint ->
      {-# UNPACK #-} !HashId ->
      {-# UNPACK #-} !HashId ->
      Description (Term Bool)
    DLeOrdTerm ::
      {-# UNPACK #-} !Digest ->
      {-# UNPACK #-} !Fingerprint ->
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

  describe (UConTerm v) = DConTerm sameCon (preHashConDescription v) v
  describe ((USymTerm name) :: UTerm t) =
    DSymTerm @t (preHashSymDescription name) name
  describe (UForallTerm (sym :: TypedSymbol 'ConstantKind arg) arg) =
    let argHashId = termHashId arg
     in DForallTerm (preHashForallDescription sym argHashId) sym argHashId
  describe (UExistsTerm (sym :: TypedSymbol 'ConstantKind arg) arg) =
    let argHashId = termHashId arg
     in DExistsTerm (preHashExistsDescription sym argHashId) sym argHashId
  describe (UNotTerm arg) =
    let argHashId = termHashId arg
     in DNotTerm (preHashNotDescription argHashId) argHashId
  describe (UOrTerm arg1 arg2) =
    let arg1HashId = termHashId arg1
        arg2HashId = termHashId arg2
     in DOrTerm
          (preHashOrDescription arg1HashId arg2HashId)
          arg1HashId
          arg2HashId
  describe (UAndTerm arg1 arg2) =
    let arg1HashId = termHashId arg1
        arg2HashId = termHashId arg2
     in DAndTerm
          (preHashAndDescription arg1HashId arg2HashId)
          arg1HashId
          arg2HashId
  describe (UEqTerm (arg1@SupportedTerm :: Term arg) arg2) = do
    let fingerprint = typeFingerprint @arg
        arg1HashId = termHashId arg1
        arg2HashId = termHashId arg2
     in DEqTerm
          (preHashEqDescription fingerprint arg1HashId arg2HashId)
          fingerprint
          arg1HashId
          arg2HashId
  describe (UDistinctTerm args@((SupportedTerm :: Term arg) :| _)) =
    let fingerprint = typeFingerprint @arg
        argsHashId = termHashId <$> args
     in DDistinctTerm
          (preHashDistinctDescription fingerprint argsHashId)
          fingerprint
          argsHashId
  describe (UITETerm cond (l :: Term arg) r) =
    let condHashId = termHashId cond
        lHashId = termHashId l
        rHashId = termHashId r
     in DITETerm
          (preHashITEDescription condHashId lHashId rHashId)
          condHashId
          lHashId
          rHashId
  describe (UAddNumTerm arg1 arg2) =
    let arg1HashId = termHashId arg1
        arg2HashId = termHashId arg2
     in DAddNumTerm
          (preHashAddNumDescription arg1HashId arg2HashId)
          arg1HashId
          arg2HashId
  describe (UNegNumTerm arg) =
    let argHashId = termHashId arg
     in DNegNumTerm (preHashNegNumDescription argHashId) argHashId
  describe (UMulNumTerm arg1 arg2) =
    let arg1HashId = termHashId arg1
        arg2HashId = termHashId arg2
     in DMulNumTerm
          (preHashMulNumDescription arg1HashId arg2HashId)
          arg1HashId
          arg2HashId
  describe (UAbsNumTerm arg) =
    let argHashId = termHashId arg
     in DAbsNumTerm (preHashAbsNumDescription argHashId) argHashId
  describe (USignumNumTerm arg) =
    let argHashId = termHashId arg
     in DSignumNumTerm (preHashSignumNumDescription argHashId) argHashId
  describe (ULtOrdTerm (arg1 :: Term arg) arg2) =
    let tr = typeFingerprint @arg
        arg1HashId = termHashId arg1
        arg2HashId = termHashId arg2
     in DLtOrdTerm
          (preHashLtOrdDescription tr arg1HashId arg2HashId)
          tr
          arg1HashId
          arg2HashId
  describe (ULeOrdTerm (arg1 :: Term arg) arg2) =
    let tr = typeFingerprint @arg
        arg1HashId = termHashId arg1
        arg2HashId = termHashId arg2
     in DLeOrdTerm
          (preHashLeOrdDescription tr arg1HashId arg2HashId)
          tr
          arg1HashId
          arg2HashId
  describe (UAndBitsTerm arg1 arg2) =
    let arg1HashId = termHashId arg1
        arg2HashId = termHashId arg2
     in DAndBitsTerm
          (preHashAndBitsDescription arg1HashId arg2HashId)
          arg1HashId
          arg2HashId
  describe (UOrBitsTerm arg1 arg2) =
    let arg1HashId = termHashId arg1
        arg2HashId = termHashId arg2
     in DOrBitsTerm
          (preHashOrBitsDescription arg1HashId arg2HashId)
          arg1HashId
          arg2HashId
  describe (UXorBitsTerm arg1 arg2) =
    let arg1HashId = termHashId arg1
        arg2HashId = termHashId arg2
     in DXorBitsTerm
          (preHashXorBitsDescription arg1HashId arg2HashId)
          arg1HashId
          arg2HashId
  describe (UComplementBitsTerm arg) =
    let argHashId = termHashId arg
     in DComplementBitsTerm
          (preHashComplementBitsDescription argHashId)
          argHashId
  describe (UShiftLeftTerm arg n) =
    let argHashId = termHashId arg
        nHashId = termHashId n
     in DShiftLeftTerm
          (preHashShiftLeftDescription argHashId nHashId)
          argHashId
          nHashId
  describe (UShiftRightTerm arg n) =
    let argHashId = termHashId arg
        nHashId = termHashId n
     in DShiftRightTerm
          (preHashShiftRightDescription argHashId nHashId)
          argHashId
          nHashId
  describe (URotateLeftTerm arg n) =
    let argHashId = termHashId arg
        nHashId = termHashId n
     in DRotateLeftTerm
          (preHashRotateLeftDescription argHashId nHashId)
          argHashId
          nHashId
  describe (URotateRightTerm arg n) =
    let argHashId = termHashId arg
        nHashId = termHashId n
     in DRotateRightTerm
          (preHashRotateRightDescription argHashId nHashId)
          argHashId
          nHashId
  describe (UBitCastTerm (arg :: Term a)) =
    let argHashId = termTypeHashId arg
     in DBitCastTerm (preHashBitCastDescription argHashId) argHashId
  describe (UBitCastOrTerm d (arg :: Term a)) =
    let dHashId = termHashId d
        argHashId = termTypeHashId arg
     in DBitCastOrTerm
          (preHashBitCastOrDescription dHashId argHashId)
          dHashId
          argHashId
  describe (UBVConcatTerm (arg1 :: Term bv1) (arg2 :: Term bv2)) =
    let arg1HashId = termTypeHashId arg1
        arg2HashId = termTypeHashId arg2
     in DBVConcatTerm
          (preHashBVConcatDescription arg1HashId arg2HashId)
          arg1HashId
          arg2HashId
  describe (UBVSelectTerm (ix :: Proxy ix) _ (arg :: Term arg)) =
    let ixFingerprint = typeRepFingerprint $ someTypeRep ix
        argHashId = termTypeHashId arg
     in DBVSelectTerm
          (preHashBVSelectDescription ixFingerprint argHashId)
          ixFingerprint
          argHashId
  describe (UBVExtendTerm signed (n :: Proxy n) (arg :: Term arg)) =
    let argHashId = termTypeHashId arg
     in DBVExtendTerm
          (preHashBVExtendDescription signed argHashId)
          signed
          n
          argHashId
  describe (UApplyTerm (f :: Term f) (arg :: Term a)) =
    let fHashId = termTypeHashId f
        argHashId = termTypeHashId arg
     in DApplyTerm
          (preHashApplyDescription fHashId argHashId)
          fHashId
          argHashId
  describe (UDivIntegralTerm arg1 arg2) =
    let arg1HashId = termHashId arg1
        arg2HashId = termHashId arg2
     in DDivIntegralTerm
          (preHashDivIntegralDescription arg1HashId arg2HashId)
          arg1HashId
          arg2HashId
  describe (UModIntegralTerm arg1 arg2) =
    let arg1HashId = termHashId arg1
        arg2HashId = termHashId arg2
     in DModIntegralTerm
          (preHashModIntegralDescription arg1HashId arg2HashId)
          arg1HashId
          arg2HashId
  describe (UQuotIntegralTerm arg1 arg2) =
    let arg1HashId = termHashId arg1
        arg2HashId = termHashId arg2
     in DQuotIntegralTerm
          (preHashQuotIntegralDescription arg1HashId arg2HashId)
          arg1HashId
          arg2HashId
  describe (URemIntegralTerm arg1 arg2) =
    let arg1HashId = termHashId arg1
        arg2HashId = termHashId arg2
     in DRemIntegralTerm
          (preHashRemIntegralDescription arg1HashId arg2HashId)
          arg1HashId
          arg2HashId
  describe (UFPTraitTerm trait (arg :: Term arg)) =
    let argHashId = termTypeHashId arg
     in DFPTraitTerm
          (preHashFPTraitDescription trait argHashId)
          trait
          argHashId
  describe (UFdivTerm arg1 arg2) =
    let arg1HashId = termHashId arg1
        arg2HashId = termHashId arg2
     in DFdivTerm
          (preHashFdivDescription arg1HashId arg2HashId)
          arg1HashId
          arg2HashId
  describe (URecipTerm arg) =
    let argHashId = termHashId arg
     in DRecipTerm (preHashRecipDescription argHashId) argHashId
  describe (UFloatingUnaryTerm op arg) =
    let argHashId = termHashId arg
     in DFloatingUnaryTerm
          (preHashFloatingUnaryDescription op argHashId)
          op
          argHashId
  describe (UPowerTerm arg1 arg2) =
    let arg1HashId = termHashId arg1
        arg2HashId = termHashId arg2
     in DPowerTerm
          (preHashPowerDescription arg1HashId arg2HashId)
          arg1HashId
          arg2HashId
  describe (UFPUnaryTerm op arg) =
    let argHashId = termHashId arg
     in DFPUnaryTerm
          (preHashFPUnaryDescription op argHashId)
          op
          argHashId
  describe (UFPBinaryTerm op arg1 arg2) =
    let arg1HashId = termHashId arg1
        arg2HashId = termHashId arg2
     in DFPBinaryTerm
          (preHashFPBinaryDescription op arg1HashId arg2HashId)
          op
          arg1HashId
          arg2HashId
  describe (UFPRoundingUnaryTerm op mode arg) =
    let modeHashId = termHashId mode
        argHashId = termHashId arg
     in DFPRoundingUnaryTerm
          (preHashFPRoundingUnaryDescription op modeHashId argHashId)
          op
          modeHashId
          argHashId
  describe (UFPRoundingBinaryTerm op mode arg1 arg2) =
    let modeHashId = termHashId mode
        arg1HashId = termHashId arg1
        arg2HashId = termHashId arg2
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
    let modeHashId = termHashId mode
        arg1HashId = termHashId arg1
        arg2HashId = termHashId arg2
        arg3HashId = termHashId arg3
     in DFPFMATerm
          (preHashFPFMADescription modeHashId arg1HashId arg2HashId arg3HashId)
          modeHashId
          arg1HashId
          arg2HashId
          arg3HashId
  describe (UFromIntegralTerm (arg :: Term a)) =
    let argHashId = termTypeHashId arg
     in DFromIntegralTerm (preHashFromIntegralDescription argHashId) argHashId
  describe (UFromFPOrTerm d mode (arg :: Term a)) =
    let dHashId = termHashId d
        modeHashId = termHashId mode
        argHashId = termTypeHashId arg
     in DFromFPOrTerm
          (preHashFromFPOrDescription dHashId modeHashId argHashId)
          dHashId
          modeHashId
          argHashId
  describe (UToFPTerm mode (arg :: Term a) _ _) =
    let modeHashId = termHashId mode
        argHashId = termTypeHashId arg
     in DToFPTerm
          (preHashToFPTermDescription modeHashId argHashId)
          modeHashId
          argHashId

  -- {-# INLINE describe #-}

  identify info = go
    where
      go (UConTerm v) = goPhantomCon info getPhantomDict v
      go (USymTerm v) = SymTerm' info v
      go (UForallTerm sym arg) = ForallTerm' info sym arg
      go (UExistsTerm sym arg) = ExistsTerm' info sym arg
      go (UNotTerm arg) = NotTerm' info arg
      go (UOrTerm arg1 arg2) = OrTerm' info arg1 arg2
      go (UAndTerm arg1 arg2) = AndTerm' info arg1 arg2
      go (UEqTerm arg1 arg2) = EqTerm' info arg1 arg2
      go (UDistinctTerm args) = DistinctTerm' info args
      -- ITE is propagated
      go (UITETerm cond l r) = ITETerm' info cond l r
      go (UAddNumTerm arg1 arg2) = AddNumTerm' info arg1 arg2
      go (UNegNumTerm arg) = NegNumTerm' info arg
      go (UMulNumTerm arg1 arg2) = MulNumTerm' info arg1 arg2
      go (UAbsNumTerm arg) = AbsNumTerm' info arg
      go (USignumNumTerm arg) = SignumNumTerm' info arg
      go (ULtOrdTerm arg1 arg2) = LtOrdTerm' info arg1 arg2
      go (ULeOrdTerm arg1 arg2) = LeOrdTerm' info arg1 arg2
      go (UAndBitsTerm arg1 arg2) = AndBitsTerm' info arg1 arg2
      go (UOrBitsTerm arg1 arg2) = OrBitsTerm' info arg1 arg2
      go (UXorBitsTerm arg1 arg2) = XorBitsTerm' info arg1 arg2
      go (UComplementBitsTerm arg) = ComplementBitsTerm' info arg
      go (UShiftLeftTerm arg n) = ShiftLeftTerm' info arg n
      go (UShiftRightTerm arg n) = ShiftRightTerm' info arg n
      go (URotateLeftTerm arg n) = RotateLeftTerm' info arg n
      go (URotateRightTerm arg n) = RotateRightTerm' info arg n
      go (UBitCastTerm arg) = goPhantomBitCast info getPhantomDict arg
      go (UBitCastOrTerm d arg) = BitCastOrTerm' info d arg
      go (UBVConcatTerm arg1 arg2) =
        goPhantomBVConcat info getPhantomDict arg1 arg2
      go (UBVSelectTerm ix w arg) =
        goPhantomBVSelect info getPhantomDict ix w arg
      go (UBVExtendTerm signed n arg) =
        goPhantomBVExtend info getPhantomDict signed n arg
      go (UApplyTerm f arg) = goPhantomApply info getPhantomDict f arg
      go (UDivIntegralTerm arg1 arg2) = DivIntegralTerm' info arg1 arg2
      go (UModIntegralTerm arg1 arg2) = ModIntegralTerm' info arg1 arg2
      go (UQuotIntegralTerm arg1 arg2) = QuotIntegralTerm' info arg1 arg2
      go (URemIntegralTerm arg1 arg2) = RemIntegralTerm' info arg1 arg2
      go (UFPTraitTerm trait arg) =
        goPhantomFPTrait info getPhantomDict trait arg
      go (UFdivTerm arg1 arg2) = FdivTerm' info arg1 arg2
      go (URecipTerm arg) = RecipTerm' info arg
      go (UFloatingUnaryTerm op arg) = FloatingUnaryTerm' info op arg
      go (UPowerTerm arg1 arg2) = PowerTerm' info arg1 arg2
      go (UFPUnaryTerm op arg) = goPhantomFPUnary info getPhantomDict op arg
      go (UFPBinaryTerm op arg1 arg2) =
        goPhantomFPBinary info getPhantomDict op arg1 arg2
      go (UFPRoundingUnaryTerm op mode arg) =
        goPhantomFPRoundingUnary info getPhantomDict op mode arg
      go (UFPRoundingBinaryTerm op mode arg1 arg2) =
        goPhantomFPRoundingBinary info getPhantomDict op mode arg1 arg2
      go (UFPFMATerm mode arg1 arg2 arg3) =
        goPhantomFPFMA info getPhantomDict mode arg1 arg2 arg3
      go (UFromIntegralTerm arg) =
        goPhantomFromIntegral info getPhantomDict arg
      go (UFromFPOrTerm d mode arg) = FromFPOrTerm' info d mode arg
      go (UToFPTerm mode (arg :: Term a) _ _) =
        goPhantomToFP info getPhantomDict mode arg
      {-# INLINE go #-}

  -- {-# INLINE identify #-}
  threadId = termThreadId
  {-# INLINE threadId #-}

  descriptionDigest (DConTerm _ h _) = h
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

-- {-# INLINE descriptionDigest #-}
{-# NOINLINE goPhantomCon #-}
goPhantomCon ::
  CachedInfo ->
  PhantomDict t ->
  t ->
  Term t
goPhantomCon info PhantomDict v = ConTerm' info v

{-# NOINLINE goPhantomBitCast #-}
goPhantomBitCast ::
  (PEvalBitCastTerm a t) =>
  CachedInfo ->
  PhantomDict t ->
  Term a ->
  Term t
goPhantomBitCast info PhantomDict arg = BitCastTerm' info arg

{-# NOINLINE goPhantomBVConcat #-}
goPhantomBVConcat ::
  ( PEvalBVTerm bv,
    KnownNat l,
    KnownNat r,
    KnownNat (l + r),
    1 <= l,
    1 <= r,
    1 <= l + r
  ) =>
  CachedInfo ->
  PhantomDict (bv (l + r)) ->
  Term (bv l) ->
  Term (bv r) ->
  Term (bv (l + r))
goPhantomBVConcat info PhantomDict arg1 arg2 =
  BVConcatTerm' info arg1 arg2

{-# NOINLINE goPhantomBVSelect #-}
goPhantomBVSelect ::
  ( PEvalBVTerm bv,
    KnownNat n,
    KnownNat ix,
    KnownNat w,
    1 <= n,
    1 <= w,
    ix + w <= n
  ) =>
  CachedInfo ->
  PhantomDict (bv w) ->
  Proxy ix ->
  Proxy w ->
  Term (bv n) ->
  Term (bv w)
goPhantomBVSelect info PhantomDict ix w arg =
  BVSelectTerm' info ix w arg

{-# NOINLINE goPhantomBVExtend #-}
goPhantomBVExtend ::
  ( PEvalBVTerm bv,
    KnownNat l,
    KnownNat r,
    1 <= l,
    1 <= r,
    l <= r
  ) =>
  CachedInfo ->
  PhantomDict (bv r) ->
  Bool ->
  Proxy r ->
  Term (bv l) ->
  Term (bv r)
goPhantomBVExtend info PhantomDict signed n arg =
  BVExtendTerm' info signed n arg

{-# NOINLINE goPhantomApply #-}
goPhantomApply ::
  (PEvalApplyTerm f a t) =>
  CachedInfo ->
  PhantomDict t ->
  Term f ->
  Term a ->
  Term t
goPhantomApply info PhantomDict f arg = ApplyTerm' info f arg

{-# NOINLINE goPhantomFPTrait #-}
goPhantomFPTrait ::
  (ValidFP eb sb) =>
  CachedInfo ->
  PhantomDict (FP eb sb) ->
  FPTrait ->
  Term (FP eb sb) ->
  Term Bool
goPhantomFPTrait info PhantomDict trait arg = FPTraitTerm' info trait arg

{-# NOINLINE goPhantomFPUnary #-}
goPhantomFPUnary ::
  (ValidFP eb sb) =>
  CachedInfo ->
  PhantomDict (FP eb sb) ->
  FPUnaryOp ->
  Term (FP eb sb) ->
  Term (FP eb sb)
goPhantomFPUnary info PhantomDict op arg = FPUnaryTerm' info op arg

{-# NOINLINE goPhantomFPBinary #-}
goPhantomFPBinary ::
  (ValidFP eb sb) =>
  CachedInfo ->
  PhantomDict (FP eb sb) ->
  FPBinaryOp ->
  Term (FP eb sb) ->
  Term (FP eb sb) ->
  Term (FP eb sb)
goPhantomFPBinary info PhantomDict op arg1 arg2 =
  FPBinaryTerm' info op arg1 arg2

{-# NOINLINE goPhantomFPRoundingUnary #-}
goPhantomFPRoundingUnary ::
  (ValidFP eb sb) =>
  CachedInfo ->
  PhantomDict (FP eb sb) ->
  FPRoundingUnaryOp ->
  Term FPRoundingMode ->
  Term (FP eb sb) ->
  Term (FP eb sb)
goPhantomFPRoundingUnary info PhantomDict op mode arg =
  FPRoundingUnaryTerm' info op mode arg

{-# NOINLINE goPhantomFPRoundingBinary #-}
goPhantomFPRoundingBinary ::
  (ValidFP eb sb) =>
  CachedInfo ->
  PhantomDict (FP eb sb) ->
  FPRoundingBinaryOp ->
  Term FPRoundingMode ->
  Term (FP eb sb) ->
  Term (FP eb sb) ->
  Term (FP eb sb)
goPhantomFPRoundingBinary info PhantomDict op mode arg1 arg2 =
  FPRoundingBinaryTerm' info op mode arg1 arg2

{-# NOINLINE goPhantomFPFMA #-}
goPhantomFPFMA ::
  (ValidFP eb sb) =>
  CachedInfo ->
  PhantomDict (FP eb sb) ->
  Term FPRoundingMode ->
  Term (FP eb sb) ->
  Term (FP eb sb) ->
  Term (FP eb sb) ->
  Term (FP eb sb)
goPhantomFPFMA info PhantomDict mode arg1 arg2 arg3 =
  FPFMATerm' info mode arg1 arg2 arg3

{-# NOINLINE goPhantomFromIntegral #-}
goPhantomFromIntegral ::
  (PEvalFromIntegralTerm a b) =>
  CachedInfo ->
  PhantomDict b ->
  Term a ->
  Term b
goPhantomFromIntegral info PhantomDict arg = FromIntegralTerm' info arg

{-# NOINLINE goPhantomToFP #-}
goPhantomToFP ::
  forall a eb sb.
  (ValidFP eb sb, PEvalIEEEFPConvertibleTerm a) =>
  CachedInfo ->
  PhantomDict (FP eb sb) ->
  Term FPRoundingMode ->
  Term a ->
  Term (FP eb sb)
goPhantomToFP info PhantomDict mode arg =
  ToFPTerm' info mode arg (Proxy @eb) (Proxy @sb)

instance Eq (Description (Term t)) where
  DConTerm eqFunc _ l == DConTerm _ _ r =
    eqFunc l r
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
  DLtOrdTerm _ lrep li1 li2 == DLtOrdTerm _ rrep ri1 ri2 = lrep == rrep && eqHashId li1 ri1 && eqHashId li2 ri2
  DLeOrdTerm _ lrep li1 li2 == DLeOrdTerm _ rrep ri1 ri2 = lrep == rrep && eqHashId li1 ri1 && eqHashId li2 ri2
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

-- {-# INLINE (==) #-}

instance Hashable (Description (Term t)) where
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
fullReconstructTerm (ConTerm i) = curThreadConTerm i
fullReconstructTerm (SymTerm sym) = curThreadSymTerm sym
fullReconstructTerm (ForallTerm sym arg) =
  fullReconstructTerm1 (curThreadForallTerm sym) arg
fullReconstructTerm (ExistsTerm sym arg) =
  fullReconstructTerm1 (curThreadExistsTerm sym) arg
fullReconstructTerm (NotTerm arg) =
  fullReconstructTerm1 curThreadNotTerm arg
fullReconstructTerm (OrTerm arg1 arg2) =
  fullReconstructTerm2 curThreadOrTerm arg1 arg2
fullReconstructTerm (AndTerm arg1 arg2) =
  fullReconstructTerm2 curThreadAndTerm arg1 arg2
fullReconstructTerm (EqTerm arg1 arg2) =
  fullReconstructTerm2 curThreadEqTerm arg1 arg2
fullReconstructTerm (DistinctTerm args) =
  traverse fullReconstructTerm args >>= curThreadDistinctTerm
fullReconstructTerm (ITETerm cond arg1 arg2) =
  fullReconstructTerm3 curThreadIteTerm cond arg1 arg2
fullReconstructTerm (AddNumTerm arg1 arg2) =
  fullReconstructTerm2 curThreadAddNumTerm arg1 arg2
fullReconstructTerm (NegNumTerm arg) =
  fullReconstructTerm1 curThreadNegNumTerm arg
fullReconstructTerm (MulNumTerm arg1 arg2) =
  fullReconstructTerm2 curThreadMulNumTerm arg1 arg2
fullReconstructTerm (AbsNumTerm arg) =
  fullReconstructTerm1 curThreadAbsNumTerm arg
fullReconstructTerm (SignumNumTerm arg) =
  fullReconstructTerm1 curThreadSignumNumTerm arg
fullReconstructTerm (LtOrdTerm arg1 arg2) =
  fullReconstructTerm2 curThreadLtOrdTerm arg1 arg2
fullReconstructTerm (LeOrdTerm arg1 arg2) =
  fullReconstructTerm2 curThreadLeOrdTerm arg1 arg2
fullReconstructTerm (AndBitsTerm arg1 arg2) =
  fullReconstructTerm2 curThreadAndBitsTerm arg1 arg2
fullReconstructTerm (OrBitsTerm arg1 arg2) =
  fullReconstructTerm2 curThreadOrBitsTerm arg1 arg2
fullReconstructTerm (XorBitsTerm arg1 arg2) =
  fullReconstructTerm2 curThreadXorBitsTerm arg1 arg2
fullReconstructTerm (ComplementBitsTerm arg) =
  fullReconstructTerm1 curThreadComplementBitsTerm arg
fullReconstructTerm (ShiftLeftTerm arg n) =
  fullReconstructTerm1 (curThreadShiftLeftTerm arg) n
fullReconstructTerm (ShiftRightTerm arg n) =
  fullReconstructTerm1 (curThreadShiftRightTerm arg) n
fullReconstructTerm (RotateLeftTerm arg n) =
  fullReconstructTerm1 (curThreadRotateLeftTerm arg) n
fullReconstructTerm (RotateRightTerm arg n) =
  fullReconstructTerm1 (curThreadRotateRightTerm arg) n
fullReconstructTerm (BitCastTerm v) =
  fullReconstructTerm1 curThreadBitCastTerm v
fullReconstructTerm (BitCastOrTerm d v) =
  fullReconstructTerm2 curThreadBitCastOrTerm d v
fullReconstructTerm (BVConcatTerm arg1 arg2) =
  fullReconstructTerm2 curThreadBVConcatTerm arg1 arg2
fullReconstructTerm (BVSelectTerm (_ :: Proxy ix) (_ :: Proxy w) arg) =
  fullReconstructTerm1 (curThreadBVSelectTerm (Proxy @ix) (Proxy @w)) arg
fullReconstructTerm (BVExtendTerm signed p arg) =
  fullReconstructTerm1 (curThreadBVExtendTerm signed p) arg
fullReconstructTerm (ApplyTerm f arg) =
  fullReconstructTerm2 curThreadApplyTerm f arg
fullReconstructTerm (DivIntegralTerm arg1 arg2) =
  fullReconstructTerm2 curThreadDivIntegralTerm arg1 arg2
fullReconstructTerm (ModIntegralTerm arg1 arg2) =
  fullReconstructTerm2 curThreadModIntegralTerm arg1 arg2
fullReconstructTerm (QuotIntegralTerm arg1 arg2) =
  fullReconstructTerm2 curThreadQuotIntegralTerm arg1 arg2
fullReconstructTerm (RemIntegralTerm arg1 arg2) =
  fullReconstructTerm2 curThreadRemIntegralTerm arg1 arg2
fullReconstructTerm (FPTraitTerm trait arg) =
  fullReconstructTerm1 (curThreadFpTraitTerm trait) arg
fullReconstructTerm (FdivTerm arg1 arg2) =
  fullReconstructTerm2 curThreadFdivTerm arg1 arg2
fullReconstructTerm (RecipTerm arg) =
  fullReconstructTerm1 curThreadRecipTerm arg
fullReconstructTerm (FloatingUnaryTerm op arg) =
  fullReconstructTerm1 (curThreadFloatingUnaryTerm op) arg
fullReconstructTerm (PowerTerm arg1 arg2) =
  fullReconstructTerm2 curThreadPowerTerm arg1 arg2
fullReconstructTerm (FPUnaryTerm op arg) =
  fullReconstructTerm1 (curThreadFpUnaryTerm op) arg
fullReconstructTerm (FPBinaryTerm op arg1 arg2) =
  fullReconstructTerm2 (curThreadFpBinaryTerm op) arg1 arg2
fullReconstructTerm (FPRoundingUnaryTerm op mode arg) =
  fullReconstructTerm2 (curThreadFpRoundingUnaryTerm op) mode arg
fullReconstructTerm (FPRoundingBinaryTerm op mode arg1 arg2) =
  fullReconstructTerm3 (curThreadFpRoundingBinaryTerm op) mode arg1 arg2
fullReconstructTerm (FPFMATerm mode arg1 arg2 arg3) = do
  rmode <- fullReconstructTerm mode
  rarg1 <- fullReconstructTerm arg1
  rarg2 <- fullReconstructTerm arg2
  rarg3 <- fullReconstructTerm arg3
  curThreadFpFMATerm rmode rarg1 rarg2 rarg3
fullReconstructTerm (FromIntegralTerm arg) =
  fullReconstructTerm1 curThreadFromIntegralTerm arg
fullReconstructTerm (FromFPOrTerm d r arg) =
  fullReconstructTerm3 curThreadFromFPOrTerm d r arg
fullReconstructTerm (ToFPTerm r arg _ _) =
  fullReconstructTerm2 curThreadToFPTerm r arg

toCurThreadImpl :: forall t. WeakThreadId -> Term t -> IO (Term t)
toCurThreadImpl tid t | termThreadId t == tid = return t
toCurThreadImpl _ t = fullReconstructTerm t
{-# INLINE toCurThreadImpl #-}

-- | Convert a term to the current thread.
toCurThread :: forall t. Term t -> IO (Term t)
toCurThread t = do
  tid <- myWeakThreadId
  toCurThreadImpl tid t
{-# INLINE toCurThread #-}

-- | Construct and internalizing a 'ConTerm'.
curThreadConTerm :: forall t. (SupportedPrim t) => t -> IO (Term t)
curThreadConTerm t = intern $ UConTerm t
{-# INLINE curThreadConTerm #-}

-- | Construct and internalizing a 'SymTerm'.
curThreadSymTerm :: forall knd t. TypedSymbol knd t -> IO (Term t)
curThreadSymTerm (TypedSymbol s) = intern $ USymTerm $ TypedSymbol s
{-# INLINE curThreadSymTerm #-}

-- | Construct and internalizing a 'ForallTerm'.
curThreadForallTerm ::
  TypedSymbol 'ConstantKind t ->
  Term Bool ->
  IO (Term Bool)
curThreadForallTerm sym arg = intern $ UForallTerm sym arg
{-# INLINE curThreadForallTerm #-}

-- | Construct and internalizing a 'ExistsTerm'.
curThreadExistsTerm ::
  TypedSymbol 'ConstantKind t ->
  Term Bool ->
  IO (Term Bool)
curThreadExistsTerm sym arg = intern $ UExistsTerm sym arg
{-# INLINE curThreadExistsTerm #-}

-- | Construct and internalizing a 'SymTerm' with an identifier, using simple
-- symbols.
curThreadSsymTerm :: (SupportedPrim t) => Identifier -> IO (Term t)
curThreadSsymTerm ident =
  curThreadSymTerm @AnyKind $ TypedSymbol $ SimpleSymbol ident
{-# INLINE curThreadSsymTerm #-}

-- | Construct and internalizing a 'SymTerm' with an identifier and an index,
-- using indexed symbols.
curThreadIsymTerm :: (SupportedPrim t) => Identifier -> Int -> IO (Term t)
curThreadIsymTerm str idx =
  curThreadSymTerm @AnyKind $ TypedSymbol $ IndexedSymbol str idx
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
curThreadEqTerm :: Term a -> Term a -> IO (Term Bool)
curThreadEqTerm l r = intern $ UEqTerm l r
{-# INLINE curThreadEqTerm #-}

-- | Construct and internalizing a 'DistinctTerm'.
curThreadDistinctTerm :: NonEmpty (Term a) -> IO (Term Bool)
curThreadDistinctTerm args = intern $ UDistinctTerm args
{-# INLINE curThreadDistinctTerm #-}

-- | Construct and internalizing a 'ITETerm'.
curThreadIteTerm :: Term Bool -> Term a -> Term a -> IO (Term a)
curThreadIteTerm c l@SupportedTerm r = intern $ UITETerm c l r
{-# INLINE curThreadIteTerm #-}

-- | Construct and internalizing a 'AddNumTerm'.
curThreadAddNumTerm :: (PEvalNumTerm a) => Term a -> Term a -> IO (Term a)
curThreadAddNumTerm l@SupportedTerm r = intern $ UAddNumTerm l r
{-# INLINE curThreadAddNumTerm #-}

-- | Construct and internalizing a 'NegNumTerm'.
curThreadNegNumTerm :: (PEvalNumTerm a) => Term a -> IO (Term a)
curThreadNegNumTerm l@SupportedTerm = intern $ UNegNumTerm l
{-# INLINE curThreadNegNumTerm #-}

-- | Construct and internalizing a 'MulNumTerm'.
curThreadMulNumTerm :: (PEvalNumTerm a) => Term a -> Term a -> IO (Term a)
curThreadMulNumTerm l@SupportedTerm r = intern $ UMulNumTerm l r
{-# INLINE curThreadMulNumTerm #-}

-- | Construct and internalizing a 'AbsNumTerm'.
curThreadAbsNumTerm :: (PEvalNumTerm a) => Term a -> IO (Term a)
curThreadAbsNumTerm l@SupportedTerm = intern $ UAbsNumTerm l
{-# INLINE curThreadAbsNumTerm #-}

-- | Construct and internalizing a 'SignumNumTerm'.
curThreadSignumNumTerm :: (PEvalNumTerm a) => Term a -> IO (Term a)
curThreadSignumNumTerm l@SupportedTerm = intern $ USignumNumTerm l
{-# INLINE curThreadSignumNumTerm #-}

-- | Construct and internalizing a 'LtOrdTerm'.
curThreadLtOrdTerm :: (PEvalOrdTerm a) => Term a -> Term a -> IO (Term Bool)
curThreadLtOrdTerm l@SupportedTerm r = intern $ ULtOrdTerm l r
{-# INLINE curThreadLtOrdTerm #-}

-- | Construct and internalizing a 'LeOrdTerm'.
curThreadLeOrdTerm :: (PEvalOrdTerm a) => Term a -> Term a -> IO (Term Bool)
curThreadLeOrdTerm l@SupportedTerm r = intern $ ULeOrdTerm l r
{-# INLINE curThreadLeOrdTerm #-}

-- | Construct and internalizing a 'AndBitsTerm'.
curThreadAndBitsTerm :: (PEvalBitwiseTerm a) => Term a -> Term a -> IO (Term a)
curThreadAndBitsTerm l@SupportedTerm r = intern $ UAndBitsTerm l r
{-# INLINE curThreadAndBitsTerm #-}

-- | Construct and internalizing a 'OrBitsTerm'.
curThreadOrBitsTerm :: (PEvalBitwiseTerm a) => Term a -> Term a -> IO (Term a)
curThreadOrBitsTerm l@SupportedTerm r = intern $ UOrBitsTerm l r
{-# INLINE curThreadOrBitsTerm #-}

-- | Construct and internalizing a 'XorBitsTerm'.
curThreadXorBitsTerm :: (PEvalBitwiseTerm a) => Term a -> Term a -> IO (Term a)
curThreadXorBitsTerm l@SupportedTerm r = intern $ UXorBitsTerm l r
{-# INLINE curThreadXorBitsTerm #-}

-- | Construct and internalizing a 'ComplementBitsTerm'.
curThreadComplementBitsTerm :: (PEvalBitwiseTerm a) => Term a -> IO (Term a)
curThreadComplementBitsTerm l@SupportedTerm = intern $ UComplementBitsTerm l
{-# INLINE curThreadComplementBitsTerm #-}

-- | Construct and internalizing a 'ShiftLeftTerm'.
curThreadShiftLeftTerm :: (PEvalShiftTerm a) => Term a -> Term a -> IO (Term a)
curThreadShiftLeftTerm t@SupportedTerm n = intern $ UShiftLeftTerm t n
{-# INLINE curThreadShiftLeftTerm #-}

-- | Construct and internalizing a 'ShiftRightTerm'.
curThreadShiftRightTerm :: (PEvalShiftTerm a) => Term a -> Term a -> IO (Term a)
curThreadShiftRightTerm t@SupportedTerm n = intern $ UShiftRightTerm t n
{-# INLINE curThreadShiftRightTerm #-}

-- | Construct and internalizing a 'RotateLeftTerm'.
curThreadRotateLeftTerm ::
  (PEvalRotateTerm a) => Term a -> Term a -> IO (Term a)
curThreadRotateLeftTerm t@SupportedTerm n = intern $ URotateLeftTerm t n
{-# INLINE curThreadRotateLeftTerm #-}

-- | Construct and internalizing a 'RotateRightTerm'.
curThreadRotateRightTerm ::
  (PEvalRotateTerm a) => Term a -> Term a -> IO (Term a)
curThreadRotateRightTerm t@SupportedTerm n = intern $ URotateRightTerm t n
{-# INLINE curThreadRotateRightTerm #-}

-- | Construct and internalizing a 'BitCastTerm'.
curThreadBitCastTerm ::
  forall a b.
  (SupportedPrim b, PEvalBitCastTerm a b) =>
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
curThreadBitCastOrTerm d@SupportedTerm a = intern $ UBitCastOrTerm d a
{-# INLINE curThreadBitCastOrTerm #-}

-- | Construct and internalizing a 'BVConcatTerm'.
curThreadBVConcatTerm ::
  forall bv l r.
  ( PEvalBVTerm bv,
    KnownNat l,
    KnownNat r,
    KnownNat (l + r),
    1 <= l,
    1 <= r,
    1 <= l + r,
    SupportedPrim (bv (l + r))
  ) =>
  Term (bv l) ->
  Term (bv r) ->
  IO (Term (bv (l + r)))
curThreadBVConcatTerm l r = intern $ UBVConcatTerm l r
{-# INLINE curThreadBVConcatTerm #-}

-- | Construct and internalizing a 'BVSelectTerm'.
curThreadBVSelectTerm ::
  forall bv n ix w p q.
  ( PEvalBVTerm bv,
    KnownNat n,
    KnownNat ix,
    KnownNat w,
    1 <= n,
    1 <= w,
    ix + w <= n,
    SupportedPrim (bv w)
  ) =>
  p ix ->
  q w ->
  Term (bv n) ->
  IO (Term (bv w))
curThreadBVSelectTerm _ _ v = intern $ UBVSelectTerm (Proxy @ix) (Proxy @w) v
{-# INLINE curThreadBVSelectTerm #-}

-- | Construct and internalizing a 'BVExtendTerm'.
curThreadBVExtendTerm ::
  forall bv l r proxy.
  ( PEvalBVTerm bv,
    KnownNat l,
    KnownNat r,
    1 <= l,
    1 <= r,
    l <= r,
    SupportedPrim (bv r)
  ) =>
  Bool ->
  proxy r ->
  Term (bv l) ->
  IO (Term (bv r))
curThreadBVExtendTerm signed _ v = intern $ UBVExtendTerm signed (Proxy @r) v
{-# INLINE curThreadBVExtendTerm #-}

-- | Construct and internalizing a 'BVExtendTerm' with sign extension.
curThreadBvsignExtendTerm ::
  forall bv l r proxy.
  ( PEvalBVTerm bv,
    KnownNat l,
    KnownNat r,
    1 <= l,
    1 <= r,
    l <= r,
    SupportedPrim (bv r)
  ) =>
  proxy r ->
  Term (bv l) ->
  IO (Term (bv r))
curThreadBvsignExtendTerm _ v = intern $ UBVExtendTerm True (Proxy @r) v
{-# INLINE curThreadBvsignExtendTerm #-}

-- | Construct and internalizing a 'BVExtendTerm' with zero extension.
curThreadBvzeroExtendTerm ::
  forall bv l r proxy.
  ( PEvalBVTerm bv,
    KnownNat l,
    KnownNat r,
    1 <= l,
    1 <= r,
    l <= r,
    SupportedPrim (bv r)
  ) =>
  proxy r ->
  Term (bv l) ->
  IO (Term (bv r))
curThreadBvzeroExtendTerm _ v = intern $ UBVExtendTerm False (Proxy @r) v
{-# INLINE curThreadBvzeroExtendTerm #-}

-- | Construct and internalizing a 'ApplyTerm'.
curThreadApplyTerm ::
  forall f a b.
  (PEvalApplyTerm f a b, SupportedPrim b) =>
  Term f ->
  Term a ->
  IO (Term b)
curThreadApplyTerm f a = intern $ UApplyTerm f a
{-# INLINE curThreadApplyTerm #-}

-- | Construct and internalizing a 'DivIntegralTerm'.
curThreadDivIntegralTerm ::
  (PEvalDivModIntegralTerm a) => Term a -> Term a -> IO (Term a)
curThreadDivIntegralTerm l@SupportedTerm r = intern $ UDivIntegralTerm l r
{-# INLINE curThreadDivIntegralTerm #-}

-- | Construct and internalizing a 'ModIntegralTerm'.
curThreadModIntegralTerm ::
  (PEvalDivModIntegralTerm a) => Term a -> Term a -> IO (Term a)
curThreadModIntegralTerm l@SupportedTerm r = intern $ UModIntegralTerm l r
{-# INLINE curThreadModIntegralTerm #-}

-- | Construct and internalizing a 'QuotIntegralTerm'.
curThreadQuotIntegralTerm ::
  (PEvalDivModIntegralTerm a) => Term a -> Term a -> IO (Term a)
curThreadQuotIntegralTerm l@SupportedTerm r = intern $ UQuotIntegralTerm l r
{-# INLINE curThreadQuotIntegralTerm #-}

-- | Construct and internalizing a 'RemIntegralTerm'.
curThreadRemIntegralTerm ::
  (PEvalDivModIntegralTerm a) => Term a -> Term a -> IO (Term a)
curThreadRemIntegralTerm l@SupportedTerm r = intern $ URemIntegralTerm l r
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
curThreadFdivTerm l@SupportedTerm r = intern $ UFdivTerm l r
{-# INLINE curThreadFdivTerm #-}

-- | Construct and internalizing a 'RecipTerm'.
curThreadRecipTerm :: (PEvalFractionalTerm a) => Term a -> IO (Term a)
curThreadRecipTerm l@SupportedTerm = intern $ URecipTerm l
{-# INLINE curThreadRecipTerm #-}

-- | Construct and internalizing a 'FloatingUnaryTerm'.
curThreadFloatingUnaryTerm ::
  (PEvalFloatingTerm a) => FloatingUnaryOp -> Term a -> IO (Term a)
curThreadFloatingUnaryTerm op a@SupportedTerm = intern $ UFloatingUnaryTerm op a
{-# INLINE curThreadFloatingUnaryTerm #-}

-- | Construct and internalizing a 'PowerTerm'.
curThreadPowerTerm :: (PEvalFloatingTerm a) => Term a -> Term a -> IO (Term a)
curThreadPowerTerm l@SupportedTerm r = intern $ UPowerTerm l r
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
  (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
  FPRoundingUnaryOp ->
  Term FPRoundingMode ->
  Term (FP eb sb) ->
  IO (Term (FP eb sb))
curThreadFpRoundingUnaryTerm op mode v = intern $ UFPRoundingUnaryTerm op mode v
{-# INLINE curThreadFpRoundingUnaryTerm #-}

-- | Construct and internalizing a 'FPRoundingBinaryTerm'.
curThreadFpRoundingBinaryTerm ::
  (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
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
  (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
  Term FPRoundingMode ->
  Term (FP eb sb) ->
  Term (FP eb sb) ->
  Term (FP eb sb) ->
  IO (Term (FP eb sb))
curThreadFpFMATerm mode l r s = intern $ UFPFMATerm mode l r s
{-# INLINE curThreadFpFMATerm #-}

-- | Construct and internalizing a 'FromIntegralTerm'.
curThreadFromIntegralTerm ::
  forall a b.
  (PEvalFromIntegralTerm a b, SupportedPrim b) =>
  Term a ->
  IO (Term b)
curThreadFromIntegralTerm = intern . UFromIntegralTerm
{-# INLINE curThreadFromIntegralTerm #-}

-- | Construct and internalizing a 'FromFPOrTerm'.
curThreadFromFPOrTerm ::
  forall a eb sb.
  ( PEvalIEEEFPConvertibleTerm a,
    ValidFP eb sb
  ) =>
  Term a ->
  Term FPRoundingMode ->
  Term (FP eb sb) ->
  IO (Term a)
curThreadFromFPOrTerm d@SupportedTerm r f = intern $ UFromFPOrTerm d r f
{-# INLINE curThreadFromFPOrTerm #-}

-- | Construct and internalizing a 'ToFPTerm'.
curThreadToFPTerm ::
  forall a eb sb.
  ( PEvalIEEEFPConvertibleTerm a,
    ValidFP eb sb,
    SupportedPrim (FP eb sb)
  ) =>
  Term FPRoundingMode ->
  Term a ->
  IO (Term (FP eb sb))
curThreadToFPTerm r f = intern $ UToFPTerm r f (Proxy @eb) (Proxy @sb)
{-# INLINE curThreadToFPTerm #-}

inCurThread1 ::
  forall a b.
  (Term a -> IO (Term b)) ->
  Term a ->
  IO (Term b)
inCurThread1 f t = do
  tid <- myWeakThreadId
  toCurThreadImpl tid t >>= f
{-# INLINE inCurThread1 #-}

inCurThread2 ::
  forall a b c.
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
  (Term a -> IO (Term b)) ->
  Term a ->
  Term b
unsafeInCurThread1 f = unsafePerformIO . inCurThread1 f
{-# NOINLINE unsafeInCurThread1 #-}

unsafeInCurThread2 ::
  forall a b c.
  (Term a -> Term b -> IO (Term c)) ->
  Term a ->
  Term b ->
  Term c
unsafeInCurThread2 f a b = unsafePerformIO $ inCurThread2 f a b
{-# NOINLINE unsafeInCurThread2 #-}

unsafeInCurThread3 ::
  forall a b c d.
  (Term a -> Term b -> Term c -> IO (Term d)) ->
  Term a ->
  Term b ->
  Term c ->
  Term d
unsafeInCurThread3 f a b c = unsafePerformIO $ inCurThread3 f a b c
{-# NOINLINE unsafeInCurThread3 #-}

-- | Construct and internalizing a 'ConTerm'.
conTerm :: (SupportedPrim t) => t -> Term t
conTerm = unsafePerformIO . curThreadConTerm
{-# NOINLINE conTerm #-}

-- | Construct and internalizing a 'SymTerm'.
symTerm :: TypedSymbol knd t -> Term t
symTerm = unsafePerformIO . curThreadSymTerm
{-# NOINLINE symTerm #-}

-- | Construct and internalizing a 'ForallTerm'.
forallTerm ::
  TypedSymbol 'ConstantKind t ->
  Term Bool ->
  Term Bool
forallTerm sym@TypedSymbol {} = unsafeInCurThread1 (curThreadForallTerm sym)
{-# NOINLINE forallTerm #-}

-- | Construct and internalizing a 'ExistsTerm'.
existsTerm ::
  TypedSymbol 'ConstantKind t ->
  Term Bool ->
  Term Bool
existsTerm sym@TypedSymbol {} = unsafeInCurThread1 (curThreadExistsTerm sym)
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
eqTerm :: Term a -> Term a -> Term Bool
eqTerm = unsafeInCurThread2 curThreadEqTerm
{-# NOINLINE eqTerm #-}

-- | Construct and internalizing a 'DistinctTerm'.
distinctTerm :: NonEmpty (Term a) -> Term Bool
distinctTerm args =
  unsafePerformIO $ do
    tid <- myWeakThreadId
    traverse (toCurThreadImpl tid) args >>= curThreadDistinctTerm
{-# NOINLINE distinctTerm #-}

-- | Construct and internalizing a 'ITETerm'.
iteTerm :: Term Bool -> Term a -> Term a -> Term a
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
  (PEvalBitCastTerm a b, SupportedPrim b) =>
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
bvConcatTerm ::
  forall bv l r.
  ( PEvalBVTerm bv,
    KnownNat l,
    KnownNat r,
    KnownNat (l + r),
    1 <= l,
    1 <= r,
    1 <= l + r,
    SupportedPrim (bv (l + r))
  ) =>
  Term (bv l) ->
  Term (bv r) ->
  Term (bv (l + r))
bvConcatTerm = unsafeInCurThread2 curThreadBVConcatTerm
{-# NOINLINE bvConcatTerm #-}

-- | Construct and internalizing a 'BVSelectTerm'.
bvSelectTerm ::
  forall bv n ix w p q.
  ( PEvalBVTerm bv,
    KnownNat n,
    KnownNat ix,
    KnownNat w,
    1 <= n,
    1 <= w,
    ix + w <= n,
    SupportedPrim (bv w)
  ) =>
  p ix ->
  q w ->
  Term (bv n) ->
  Term (bv w)
bvSelectTerm ix w = unsafeInCurThread1 (curThreadBVSelectTerm ix w)
{-# NOINLINE bvSelectTerm #-}

-- | Construct and internalizing a 'BVExtendTerm'.
bvExtendTerm ::
  forall bv l r proxy.
  ( PEvalBVTerm bv,
    KnownNat l,
    KnownNat r,
    1 <= l,
    1 <= r,
    l <= r,
    SupportedPrim (bv r)
  ) =>
  Bool ->
  proxy r ->
  Term (bv l) ->
  Term (bv r)
bvExtendTerm signed r = unsafeInCurThread1 (curThreadBVExtendTerm signed r)
{-# NOINLINE bvExtendTerm #-}

-- | Construct and internalizing a 'BVExtendTerm' with sign extension.
bvsignExtendTerm ::
  forall bv l r proxy.
  ( PEvalBVTerm bv,
    KnownNat l,
    KnownNat r,
    1 <= l,
    1 <= r,
    l <= r,
    SupportedPrim (bv r)
  ) =>
  proxy r ->
  Term (bv l) ->
  Term (bv r)
bvsignExtendTerm r = unsafeInCurThread1 (curThreadBvsignExtendTerm r)
{-# NOINLINE bvsignExtendTerm #-}

-- | Construct and internalizing a 'BVExtendTerm' with zero extension.
bvzeroExtendTerm ::
  forall bv l r proxy.
  ( PEvalBVTerm bv,
    KnownNat l,
    KnownNat r,
    1 <= l,
    1 <= r,
    l <= r,
    SupportedPrim (bv r)
  ) =>
  proxy r ->
  Term (bv l) ->
  Term (bv r)
bvzeroExtendTerm r = unsafeInCurThread1 (curThreadBvzeroExtendTerm r)
{-# NOINLINE bvzeroExtendTerm #-}

-- | Construct and internalizing a 'ApplyTerm'.
applyTerm ::
  (PEvalApplyTerm f a b, SupportedPrim b) => Term f -> Term a -> Term b
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
  (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
  FPRoundingUnaryOp ->
  Term FPRoundingMode ->
  Term (FP eb sb) ->
  Term (FP eb sb)
fpRoundingUnaryTerm op = unsafeInCurThread2 (curThreadFpRoundingUnaryTerm op)
{-# NOINLINE fpRoundingUnaryTerm #-}

-- | Construct and internalizing a 'FPRoundingBinaryTerm'.
fpRoundingBinaryTerm ::
  (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
  FPRoundingBinaryOp ->
  Term FPRoundingMode ->
  Term (FP eb sb) ->
  Term (FP eb sb) ->
  Term (FP eb sb)
fpRoundingBinaryTerm op = unsafeInCurThread3 (curThreadFpRoundingBinaryTerm op)
{-# NOINLINE fpRoundingBinaryTerm #-}

-- | Construct and internalizing a 'FPFMATerm'.
fpFMATerm ::
  (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
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
fromIntegralTerm ::
  (PEvalFromIntegralTerm a b, SupportedPrim b) => Term a -> Term b
fromIntegralTerm = unsafeInCurThread1 curThreadFromIntegralTerm
{-# NOINLINE fromIntegralTerm #-}

-- | Construct and internalizing a 'FromFPOrTerm'.
fromFPOrTerm ::
  ( PEvalIEEEFPConvertibleTerm a,
    ValidFP eb sb
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

-- | Construct and internalizing 'True' term.
trueTerm :: Term Bool
trueTerm = conTerm True
{-# NOINLINE trueTerm #-}

-- | Construct and internalizing 'False' term.
falseTerm :: Term Bool
falseTerm = conTerm False
{-# NOINLINE falseTerm #-}

boolConTermView :: forall a. Term a -> Maybe Bool
boolConTermView (ConTerm b) = cast b
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
boolTermView t@SupportedTerm = cast t
{-# INLINE boolTermView #-}

-- | Pattern matcher for 'Bool' terms.
pattern BoolTerm :: Term Bool -> Term a
pattern BoolTerm b <- (boolTermView -> Just b)

-- | Partial evaluation for not terms.
pevalNotTerm :: Term Bool -> Term Bool
pevalNotTerm (NotTerm tm) = tm
pevalNotTerm (ConTerm a) = if a then falseTerm else trueTerm
pevalNotTerm (OrTerm (NotTerm n1) n2) = pevalAndTerm n1 (pevalNotTerm n2)
pevalNotTerm (OrTerm (DistinctTerm (n1 :| [n2])) n3) =
  pevalAndTerm (pevalEqTerm n1 n2) (pevalNotTerm n3)
pevalNotTerm (OrTerm n1 (NotTerm n2)) = pevalAndTerm (pevalNotTerm n1) n2
pevalNotTerm (OrTerm n1 (DistinctTerm (n2 :| [n3]))) =
  pevalAndTerm (pevalNotTerm n1) (pevalEqTerm n2 n3)
pevalNotTerm (AndTerm (NotTerm n1) n2) = pevalOrTerm n1 (pevalNotTerm n2)
pevalNotTerm (AndTerm (DistinctTerm (n1 :| [n2])) n3) =
  pevalOrTerm (pevalEqTerm n1 n2) (pevalNotTerm n3)
pevalNotTerm (AndTerm n1 (NotTerm n2)) = pevalOrTerm (pevalNotTerm n1) n2
pevalNotTerm (AndTerm n1 (DistinctTerm (n2 :| [n3]))) =
  pevalOrTerm (pevalNotTerm n1) (pevalEqTerm n2 n3)
pevalNotTerm (EqTerm a b) = distinctTerm $ a :| [b]
pevalNotTerm (DistinctTerm (a :| [b])) = eqTerm a b
pevalNotTerm tm = notTerm tm
{-# INLINEABLE pevalNotTerm #-}

orEqFirst :: Term Bool -> Term Bool -> Bool
orEqFirst _ (ConTerm False) = True
orEqFirst
  (DistinctTerm ((e1 :: Term a) :| [ec1@ConTerm {} :: Term b]))
  (EqTerm (DynTerm (e2 :: Term a)) (DynTerm (ec2@ConTerm {} :: Term b)))
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
orEqTrue (ConTerm True) ~_ = True
orEqTrue _ (ConTerm True) = True
-- orEqTrue (NotTerm _ e1) (NotTerm _ e2) = andEqFalse e1 e2
orEqTrue
  (DistinctTerm ((e1 :: Term a) :| [ec1@ConTerm {} :: Term b]))
  (DistinctTerm ((DynTerm (e2 :: Term a)) :| [DynTerm (ec2@ConTerm {} :: Term b)]))
    | e1 == e2 && ec1 /= ec2 = True
-- orEqTrue
--   (NotTerm _ (EqTerm _ (e1 :: Term a) (ec1@(ConTerm _ _ _ _) :: Term b)))
--   (NotTerm _ (EqTerm _ (Dyn (e2 :: Term a)) (Dyn (ec2@(ConTerm _ _ _ _) :: Term b))))
--     | e1 == e2 && ec1 /= ec2 = True
orEqTrue (NotTerm l) r | l == r = True
orEqTrue l (NotTerm r) | l == r = True
orEqTrue _ _ = False
{-# INLINE orEqTrue #-}

andEqFirst :: Term Bool -> Term Bool -> Bool
andEqFirst _ (ConTerm True) = True
-- andEqFirst x (NotTerm _ y) = andEqFalse x y
andEqFirst
  (EqTerm (e1 :: Term a) (ec1@ConTerm {} :: Term b))
  (DistinctTerm ((DynTerm (e2 :: Term a)) :| [DynTerm (ec2@ConTerm {} :: Term b)]))
    | e1 == e2 && ec1 /= ec2 = True
-- andEqFirst
--   (EqTerm _ (e1 :: Term a) (ec1@(ConTerm _ _ _ _) :: Term b))
--   (NotTerm _ (EqTerm _ (Dyn (e2 :: Term a)) (Dyn (ec2@(ConTerm _ _ _ _) :: Term b))))
--     | e1 == e2 && ec1 /= ec2 = True
andEqFirst x y
  | x == y = True
  | otherwise = False
{-# INLINE andEqFirst #-}

andEqFalse :: Term Bool -> Term Bool -> Bool
andEqFalse (ConTerm False) ~_ = True
andEqFalse _ (ConTerm False) = True
-- andEqFalse (NotTerm _ e1) (NotTerm _ e2) = orEqTrue e1 e2
andEqFalse
  (EqTerm (e1 :: Term a) (ec1@ConTerm {} :: Term b))
  (EqTerm (DynTerm (e2 :: Term a)) (DynTerm (ec2@ConTerm {} :: Term b)))
    | e1 == e2 && ec1 /= ec2 = True
andEqFalse (NotTerm x) y | x == y = True
andEqFalse x (NotTerm y) | x == y = True
andEqFalse _ _ = False
{-# INLINE andEqFalse #-}

-- | Partial evaluation for or terms.
pevalOrTerm :: Term Bool -> Term Bool -> Term Bool
pevalOrTerm l ~r
  | orEqTrue l r = trueTerm
  | orEqFirst l r = l
  | orEqFirst r l = r
pevalOrTerm l r@(OrTerm r1 r2)
  | orEqTrue l r1 = trueTerm
  | orEqTrue l r2 = trueTerm
  | orEqFirst r1 l = r
  | orEqFirst r2 l = r
  | orEqFirst l r1 = pevalOrTerm l r2
  | orEqFirst l r2 = pevalOrTerm l r1
pevalOrTerm l@(OrTerm l1 l2) r
  | orEqTrue l1 r = trueTerm
  | orEqTrue l2 r = trueTerm
  | orEqFirst l1 r = l
  | orEqFirst l2 r = l
  | orEqFirst r l1 = pevalOrTerm l2 r
  | orEqFirst r l2 = pevalOrTerm l1 r
pevalOrTerm l (AndTerm r1 r2)
  | orEqFirst l r1 = l
  | orEqFirst l r2 = l
  | orEqTrue l r1 = pevalOrTerm l r2
  | orEqTrue l r2 = pevalOrTerm l r1
pevalOrTerm (AndTerm l1 l2) r
  | orEqFirst r l1 = r
  | orEqFirst r l2 = r
  | orEqTrue l1 r = pevalOrTerm l2 r
  | orEqTrue l2 r = pevalOrTerm l1 r
pevalOrTerm
  (AndTerm nl1@(NotTerm l1) l2)
  (EqTerm (DynTerm (e1 :: Term Bool)) (DynTerm (e2 :: Term Bool)))
    | l1 == e1 && l2 == e2 = pevalOrTerm nl1 l2
pevalOrTerm (NotTerm nl) (NotTerm nr) =
  pevalNotTerm $ pevalAndTerm nl nr
pevalOrTerm l r = orTerm l r
{-# INLINEABLE pevalOrTerm #-}

-- | Partial evaluation for and terms.
pevalAndTerm :: Term Bool -> Term Bool -> Term Bool
pevalAndTerm l ~r
  | andEqFalse l r = falseTerm
  | andEqFirst l r = l
  | andEqFirst r l = r
pevalAndTerm l r@(AndTerm r1 r2)
  | andEqFalse l r1 = falseTerm
  | andEqFalse l r2 = falseTerm
  | andEqFirst r1 l = r
  | andEqFirst r2 l = r
  | andEqFirst l r1 = pevalAndTerm l r2
  | andEqFirst l r2 = pevalAndTerm l r1
pevalAndTerm l@(AndTerm l1 l2) r
  | andEqFalse l1 r = falseTerm
  | andEqFalse l2 r = falseTerm
  | andEqFirst l1 r = l
  | andEqFirst l2 r = l
  | andEqFirst r l1 = pevalAndTerm l2 r
  | andEqFirst r l2 = pevalAndTerm l1 r
pevalAndTerm l (OrTerm r1 r2)
  | andEqFirst l r1 = l
  | andEqFirst l r2 = l
  | andEqFalse l r1 = pevalAndTerm l r2
  | andEqFalse l r2 = pevalAndTerm l r1
pevalAndTerm (OrTerm l1 l2) r
  | andEqFirst r l1 = r
  | andEqFirst r l2 = r
  | andEqFalse l1 r = pevalAndTerm l2 r
  | andEqFalse l2 r = pevalAndTerm l1 r
pevalAndTerm
  (OrTerm l1 nl2@(NotTerm l2))
  (NotTerm (EqTerm (DynTerm (e1 :: Term Bool)) (DynTerm (e2 :: Term Bool))))
    | l1 == e1 && l2 == e2 = pevalAndTerm l1 nl2
pevalAndTerm (NotTerm nl) (NotTerm nr) = pevalNotTerm $ pevalOrTerm nl nr
pevalAndTerm l r = andTerm l r
{-# INLINEABLE pevalAndTerm #-}

-- | Partial evaluation for imply terms.
pevalImplyTerm :: Term Bool -> Term Bool -> Term Bool
pevalImplyTerm l = pevalOrTerm (pevalNotTerm l)

-- | Partial evaluation for xor terms.
pevalXorTerm :: Term Bool -> Term Bool -> Term Bool
pevalXorTerm l r = pevalOrTerm (pevalAndTerm (pevalNotTerm l) r) (pevalAndTerm l (pevalNotTerm r))

pevalImpliesTerm :: Term Bool -> Term Bool -> Bool
pevalImpliesTerm (ConTerm False) _ = True
pevalImpliesTerm _ (ConTerm True) = True
pevalImpliesTerm
  (EqTerm (e1 :: Term a) (ec1@ConTerm {} :: Term b))
  (DistinctTerm ((DynTerm (e2 :: Term a)) :| [(DynTerm (ec2@ConTerm {} :: Term b))]))
    | e1 == e2 && ec1 /= ec2 = True
-- pevalImpliesTerm
--   (EqTerm _ (e1 :: Term a) (ec1@(ConTerm _ _ _ _) :: Term b))
--   (NotTerm _ (EqTerm _ (Dyn (e2 :: Term a)) (Dyn (ec2@(ConTerm _ _ _ _) :: Term b))))
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
      AndTerm nt1 nt2 -> ra
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
      OrTerm nt1 nt2 -> ra
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
pevalInferImplies cond (NotTerm nt1) _ falseRes
  | cond == nt1 = Just falseRes
  | otherwise = Nothing
-- \| otherwise = case (cond, nt1) of
--     ( EqTerm _ (e1 :: Term a) (ec1@(ConTerm _ _ _ _) :: Term b),
--       EqTerm _ (Dyn (e2 :: Term a)) (Dyn (ec2@(ConTerm _ _ _ _) :: Term b))
--       )
--         | e1 == e2 && ec1 /= ec2 -> Just trueRes
--     _ -> Nothing
pevalInferImplies
  (EqTerm (e1 :: Term a) (ec1@ConTerm {} :: Term b))
  (DistinctTerm ((DynTerm (e2 :: Term a)) :| [DynTerm (ec2@ConTerm {} :: Term b)]))
  trueRes
  _
    | e1 == e2 && ec1 /= ec2 = Just trueRes
pevalInferImplies
  (EqTerm (e1 :: Term a) (ec1@ConTerm {} :: Term b))
  (EqTerm (DynTerm (e2 :: Term a)) (DynTerm (ec2@ConTerm {} :: Term b)))
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
pevalITEBoolLeft cond (AndTerm t1 t2) ifFalse =
  msum
    [ pevalITEBoolLeftAnd cond t1 t2 ifFalse,
      case ifFalse of
        AndTerm f1 f2 -> pevalITEBoolBothAnd cond t1 t2 f1 f2
        _ -> Nothing
    ]
pevalITEBoolLeft cond (OrTerm t1 t2) ifFalse =
  msum
    [ pevalITEBoolLeftOr cond t1 t2 ifFalse,
      case ifFalse of
        OrTerm f1 f2 -> pevalITEBoolBothOr cond t1 t2 f1 f2
        _ -> Nothing
    ]
pevalITEBoolLeft cond (NotTerm nIfTrue) ifFalse =
  msum
    [ pevalITEBoolLeftNot cond nIfTrue ifFalse,
      case ifFalse of
        NotTerm nIfFalse ->
          pevalITEBoolBothNot cond nIfTrue nIfFalse
        _ -> Nothing
    ]
pevalITEBoolLeft _ _ _ = Nothing

pevalITEBoolNoLeft :: Term Bool -> Term Bool -> Term Bool -> Maybe (Term Bool)
pevalITEBoolNoLeft cond ifTrue (AndTerm f1 f2) = pevalITEBoolRightAnd cond ifTrue f1 f2
pevalITEBoolNoLeft cond ifTrue (OrTerm f1 f2) = pevalITEBoolRightOr cond ifTrue f1 f2
pevalITEBoolNoLeft cond ifTrue (NotTerm nIfFalse) = pevalITEBoolRightNot cond ifTrue nIfFalse
pevalITEBoolNoLeft _ _ _ = Nothing

-- | Basic partial evaluation for ITE terms.
pevalITEBasic :: (SupportedPrim a) => Term Bool -> Term a -> Term a -> Maybe (Term a)
pevalITEBasic (ConTerm True) ~ifTrue ~_ = Just ifTrue
pevalITEBasic (ConTerm False) ~_ ~ifFalse = Just ifFalse
pevalITEBasic (NotTerm ncond) ifTrue ifFalse = Just $ pevalITETerm ncond ifFalse ifTrue
pevalITEBasic _ ifTrue ifFalse | ifTrue == ifFalse = Just ifTrue
pevalITEBasic (ITETerm cc ct cf) (ITETerm tc tt tf) (ITETerm fc ft ff) -- later
  | cc == tc && cc == fc = Just $ pevalITETerm cc (pevalITETerm ct tt ft) (pevalITETerm cf tf ff)
pevalITEBasic cond (ITETerm tc tt tf) ifFalse -- later
  | cond == tc = Just $ pevalITETerm cond tt ifFalse
  | tt == ifFalse = Just $ pevalITETerm (pevalOrTerm (pevalNotTerm cond) tc) tt tf
  | tf == ifFalse = Just $ pevalITETerm (pevalAndTerm cond tc) tt tf
pevalITEBasic cond ifTrue (ITETerm fc ft ff) -- later
  | ifTrue == ft = Just $ pevalITETerm (pevalOrTerm cond fc) ifTrue ff
  | ifTrue == ff = Just $ pevalITETerm (pevalOrTerm cond (pevalNotTerm fc)) ifTrue ft
  | pevalImpliesTerm fc cond = Just $ pevalITETerm cond ifTrue ff
pevalITEBasic _ _ _ = Nothing

pevalITEBoolBasic :: Term Bool -> Term Bool -> Term Bool -> Maybe (Term Bool)
pevalITEBoolBasic cond ifTrue ifFalse
  | cond == ifTrue = Just $ pevalOrTerm cond ifFalse
  | cond == ifFalse = Just $ pevalAndTerm cond ifTrue
pevalITEBoolBasic cond (ConTerm v) ifFalse
  | v = Just $ pevalOrTerm cond ifFalse
  | otherwise = Just $ pevalAndTerm (pevalNotTerm cond) ifFalse
pevalITEBoolBasic cond ifTrue (ConTerm v)
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
pevalDefaultEqTerm (NotTerm lv) r
  | lv == r = falseTerm
pevalDefaultEqTerm l (NotTerm rv)
  | l == rv = falseTerm
pevalDefaultEqTerm (AddNumTerm (ConTerm c) v) (ConTerm c2) =
  pevalDefaultEqTerm v (conTerm $ c2 - c)
pevalDefaultEqTerm l (ITETerm c t f)
  | l == t = pevalOrTerm c (pevalDefaultEqTerm l f)
  | l == f = pevalOrTerm (pevalNotTerm c) (pevalDefaultEqTerm l t)
pevalDefaultEqTerm (ITETerm c t f) r
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
  funcDummyConstraint _ = SBV.sTrue

instance NonFuncSBVRep Bool where
  type NonFuncSBVBaseType Bool = Bool

instance SupportedNonFuncPrim Bool where
  conNonFuncSBVTerm = conSBVTerm
  symNonFuncSBVTerm = symSBVTerm @Bool
  withNonFuncPrim r = r

data PhantomDict a where
  PhantomDict :: (SupportedPrim a) => PhantomDict a

data PhantomNonFuncDict a where
  PhantomNonFuncDict ::
    (SupportedNonFuncPrim a) => PhantomNonFuncDict a

{-# NOINLINE phantomDictCache #-}
phantomDictCache :: IORef (HM.HashMap SomeTypeRep (PhantomDict Any))
phantomDictCache = unsafePerformIO $ newIORef HM.empty

-- TODO
{-# NOINLINE getPhantomDict #-}
getPhantomDict :: forall a. (SupportedPrim a) => PhantomDict a
getPhantomDict = unsafePerformIO $ do
  cache <- readIORef phantomDictCache
  let !tr = SomeTypeRep $ primTypeRep @a
  case HM.lookup tr cache of
    Just p -> return $ unsafeCoerce p
    Nothing -> do
      let r = PhantomDict :: PhantomDict a
      atomicModifyIORefCAS_ phantomDictCache $ HM.insert tr $ unsafeCoerce r
      return r

{-# NOINLINE phantomNonFuncDictCache #-}
phantomNonFuncDictCache ::
  IORef (HM.HashMap SomeTypeRep (PhantomNonFuncDict Any))
phantomNonFuncDictCache = unsafePerformIO $ newIORef HM.empty

-- TODO
{-# NOINLINE getPhantomNonFuncDict #-}
getPhantomNonFuncDict ::
  forall a. (SupportedNonFuncPrim a) => PhantomNonFuncDict a
getPhantomNonFuncDict = unsafePerformIO $ do
  cache <- readIORef phantomNonFuncDictCache
  let !tr = SomeTypeRep $ primTypeRep @a
  case HM.lookup tr cache of
    Just p -> return $ unsafeCoerce p
    Nothing -> do
      let r = PhantomNonFuncDict :: PhantomNonFuncDict a
      atomicModifyIORefCAS_ phantomNonFuncDictCache $
        HM.insert tr $
          unsafeCoerce r
      return r
