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
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

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
    UnaryOp (..),
    BinaryOp (..),
    TernaryOp (..),
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
    PEvalBVSignConversionTerm (..),
    PEvalBVTerm (..),
    PEvalFractionalTerm (..),
    PEvalFloatingTerm (..),

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
    identityWithTypeRep,
    introSupportedPrimConstraint,
    pformat,

    -- * Interning
    UTerm (..),
    prettyPrintTerm,
    forallTerm,
    existsTerm,
    constructUnary,
    constructBinary,
    constructTernary,
    conTerm,
    symTerm,
    ssymTerm,
    isymTerm,
    notTerm,
    orTerm,
    andTerm,
    eqTerm,
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
    toSignedTerm,
    toUnsignedTerm,
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
    --
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
import Data.Array ((!))
import Data.Bits (Bits)
import Data.Function (on)
import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable (hash, hashWithSalt))
import Data.IORef (atomicModifyIORef')
import Data.Interned
  ( Cache,
    Id,
    Interned (Description, Uninterned, cache, cacheWidth, describe, identify),
  )
import Data.Interned.Internal
  ( Cache (getCache),
    CacheState (CacheState),
  )
import Data.Kind (Constraint, Type)
import Data.Maybe (fromMaybe)
import qualified Data.SBV as SBV
import qualified Data.SBV.Dynamic as SBVD
import qualified Data.SBV.Trans as SBVT
import qualified Data.SBV.Trans.Control as SBVTC
import Data.String (IsString (fromString))
import Data.Typeable (Proxy (Proxy), cast)
import GHC.Exts (sortWith)
import GHC.Generics (Generic)
import GHC.IO (unsafeDupablePerformIO)
import GHC.Stack (HasCallStack)
import GHC.TypeNats (KnownNat, Nat, type (+), type (<=))
import Grisette.Internal.Core.Data.Class.BitVector
  ( SizedBV,
  )
import Grisette.Internal.Core.Data.Class.SignConversion (SignConversion)
import Grisette.Internal.Core.Data.Class.SymRotate (SymRotate)
import Grisette.Internal.Core.Data.Class.SymShift (SymShift)
import Grisette.Internal.Core.Data.Symbol
  ( Identifier,
    Symbol (IndexedSymbol, SimpleSymbol),
  )
import Grisette.Internal.SymPrim.FP (FP, FPRoundingMode, ValidFP)
import Grisette.Internal.SymPrim.Prim.Internal.Caches
  ( typeMemoizedCache,
  )
import Grisette.Internal.SymPrim.Prim.Internal.IsZero (KnownIsZero)
import Grisette.Internal.SymPrim.Prim.Internal.Utils
  ( eqHeteroRep,
    eqTypeRepBool,
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

-- SBV Translation
class (Monad m) => SBVFreshMonad m where
  sbvFresh :: (SBV.SymVal a) => String -> m (SBV.SBV a)

instance (MonadIO m) => SBVFreshMonad (SBVT.SymbolicT m) where
  sbvFresh = SBVT.free

instance (MonadIO m) => SBVFreshMonad (SBVTC.QueryT m) where
  sbvFresh = SBVTC.freshVar

instance (SBVFreshMonad m) => SBVFreshMonad (ReaderT r m) where
  sbvFresh = lift . sbvFresh

instance (SBVFreshMonad m, Monoid w) => SBVFreshMonad (WriterT w m) where
  sbvFresh = lift . sbvFresh

instance (SBVFreshMonad m, Monoid w) => SBVFreshMonad (RWST r w s m) where
  sbvFresh = lift . sbvFresh

instance (SBVFreshMonad m) => SBVFreshMonad (StateT s m) where
  sbvFresh = lift . sbvFresh

translateTypeError :: (HasCallStack) => Maybe String -> TypeRep a -> b
translateTypeError Nothing ta =
  error $
    "Don't know how to translate the type " ++ show ta ++ " to SMT"
translateTypeError (Just reason) ta =
  error $
    "Don't know how to translate the type " ++ show ta ++ " to SMT: " <> reason

class (SupportedPrim a, Ord a) => NonFuncSBVRep a where
  type NonFuncSBVBaseType (n :: Nat) a

type NonFuncPrimConstraint n a =
  ( SBV.SymVal (NonFuncSBVBaseType n a),
    SBV.EqSymbolic (SBVType n a),
    SBV.Mergeable (SBVType n a),
    SBV.SMTDefinable (SBVType n a),
    SBV.Mergeable (SBVType n a),
    SBVType n a ~ SBV.SBV (NonFuncSBVBaseType n a),
    PrimConstraint n a
  )

class (NonFuncSBVRep a) => SupportedNonFuncPrim a where
  conNonFuncSBVTerm ::
    (KnownIsZero n) =>
    proxy n ->
    a ->
    SBV.SBV (NonFuncSBVBaseType n a)
  symNonFuncSBVTerm ::
    (SBVFreshMonad m, KnownIsZero n) =>
    proxy n ->
    String ->
    m (SBV.SBV (NonFuncSBVBaseType n a))
  withNonFuncPrim ::
    (KnownIsZero n) => proxy n -> ((NonFuncPrimConstraint n a) => r) -> r

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

class SBVRep t where
  type SBVType (n :: Nat) t

class SupportedPrimConstraint t where
  type PrimConstraint (n :: Nat) t :: Constraint
  type PrimConstraint _ _ = ()

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
  termCache :: Cache (Term t)
  termCache = typeMemoizedCache
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
  conSBVTerm :: (KnownIsZero n) => proxy n -> t -> SBVType n t
  symSBVName :: TypedSymbol 'AnyKind t -> Int -> String
  symSBVTerm ::
    (SBVFreshMonad m, KnownIsZero n) =>
    proxy n ->
    String ->
    m (SBVType n t)
  default withPrim ::
    ( PrimConstraint n t,
      SBV.SMTDefinable (SBVType n t),
      SBV.Mergeable (SBVType n t),
      Typeable (SBVType n t),
      KnownIsZero n
    ) =>
    p n ->
    ( ( PrimConstraint n t,
        SBV.SMTDefinable (SBVType n t),
        SBV.Mergeable (SBVType n t),
        Typeable (SBVType n t)
      ) =>
      a
    ) ->
    a
  withPrim ::
    (KnownIsZero n) =>
    p n ->
    ( ( PrimConstraint n t,
        SBV.SMTDefinable (SBVType n t),
        SBV.Mergeable (SBVType n t),
        Typeable (SBVType n t)
      ) =>
      a
    ) ->
    a
  withPrim _ i = i
  sbvIte ::
    (KnownIsZero n) =>
    proxy n ->
    SBV.SBV Bool ->
    SBVType n t ->
    SBVType n t ->
    SBVType n t
  sbvIte p = withPrim @t p SBV.ite
  sbvEq ::
    (KnownIsZero n) =>
    proxy n ->
    SBVType n t ->
    SBVType n t ->
    SBV.SBV Bool
  default sbvEq ::
    (KnownIsZero n, SBVT.EqSymbolic (SBVType n t)) =>
    proxy n ->
    SBVType n t ->
    SBVType n t ->
    SBV.SBV Bool
  sbvEq _ = (SBV..==)
  parseSMTModelResult :: Int -> ([([SBVD.CV], SBVD.CV)], SBVD.CV) -> t
  castTypedSymbol ::
    (IsSymbolKind knd') => TypedSymbol knd t -> Maybe (TypedSymbol knd' t)
  isFuncType :: Bool
  funcDummyConstraint ::
    (KnownIsZero n) => p n -> SBVType n t -> SBV.SBV Bool

castSomeTypedSymbol ::
  (IsSymbolKind knd') =>
  SomeTypedSymbol knd ->
  Maybe (SomeTypedSymbol knd')
castSomeTypedSymbol (SomeTypedSymbol ty s@TypedSymbol {}) = do
  SomeTypedSymbol ty <$> castTypedSymbol s

parseSMTModelResultError ::
  (HasCallStack) => TypeRep a -> ([([SBVD.CV], SBVD.CV)], SBVD.CV) -> a
parseSMTModelResultError ty cv =
  error $
    "BUG: cannot parse SBV model value \""
      <> show cv
      <> "\" to Grisette model value with the type "
      <> show ty

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

-- Partial Evaluation for the terms
class
  (SupportedPrim f, SupportedPrim a, SupportedPrim b) =>
  PEvalApplyTerm f a b
    | f -> a b
  where
  pevalApplyTerm :: Term f -> Term a -> Term b
  sbvApplyTerm ::
    (KnownIsZero n) => proxy n -> SBVType n f -> SBVType n a -> SBVType n b

class (SupportedNonFuncPrim t, Bits t) => PEvalBitwiseTerm t where
  pevalAndBitsTerm :: Term t -> Term t -> Term t
  pevalOrBitsTerm :: Term t -> Term t -> Term t
  pevalXorBitsTerm :: Term t -> Term t -> Term t
  pevalComplementBitsTerm :: Term t -> Term t
  withSbvBitwiseTermConstraint ::
    (KnownIsZero n) =>
    proxy n ->
    (((Bits (SBVType n t)) => r)) ->
    r
  sbvAndBitsTerm ::
    (KnownIsZero n) => proxy n -> SBVType n t -> SBVType n t -> SBVType n t
  sbvAndBitsTerm p = withSbvBitwiseTermConstraint @t p (SBV..&.)
  sbvOrBitsTerm ::
    (KnownIsZero n) => proxy n -> SBVType n t -> SBVType n t -> SBVType n t
  sbvOrBitsTerm p = withSbvBitwiseTermConstraint @t p (SBV..|.)
  sbvXorBitsTerm ::
    (KnownIsZero n) => proxy n -> SBVType n t -> SBVType n t -> SBVType n t
  sbvXorBitsTerm p = withSbvBitwiseTermConstraint @t p SBV.xor
  sbvComplementBitsTerm ::
    (KnownIsZero n) => proxy n -> SBVType n t -> SBVType n t
  sbvComplementBitsTerm p = withSbvBitwiseTermConstraint @t p SBV.complement

class (SupportedNonFuncPrim t, SymShift t) => PEvalShiftTerm t where
  pevalShiftLeftTerm :: Term t -> Term t -> Term t
  pevalShiftRightTerm :: Term t -> Term t -> Term t
  withSbvShiftTermConstraint ::
    (KnownIsZero n) =>
    proxy n ->
    (((SBV.SIntegral (NonFuncSBVBaseType n t)) => r)) ->
    r
  sbvShiftLeftTerm ::
    forall proxy n.
    (KnownIsZero n) =>
    proxy n ->
    SBVType n t ->
    SBVType n t ->
    SBVType n t
  sbvShiftLeftTerm p l r =
    withNonFuncPrim @t p $
      withSbvShiftTermConstraint @t p $
        SBV.sShiftLeft l r
  sbvShiftRightTerm ::
    forall proxy n.
    (KnownIsZero n) =>
    proxy n ->
    SBVType n t ->
    SBVType n t ->
    SBVType n t
  sbvShiftRightTerm p l r =
    withNonFuncPrim @t p $
      withSbvShiftTermConstraint @t p $
        SBV.sShiftRight l r

class (SupportedNonFuncPrim t, SymRotate t) => PEvalRotateTerm t where
  pevalRotateLeftTerm :: Term t -> Term t -> Term t
  pevalRotateRightTerm :: Term t -> Term t -> Term t
  withSbvRotateTermConstraint ::
    (KnownIsZero n) =>
    proxy n ->
    (((SBV.SIntegral (NonFuncSBVBaseType n t)) => r)) ->
    r
  sbvRotateLeftTerm ::
    forall proxy n.
    (KnownIsZero n) =>
    proxy n ->
    SBVType n t ->
    SBVType n t ->
    SBVType n t
  sbvRotateLeftTerm p l r =
    withNonFuncPrim @t p $
      withSbvRotateTermConstraint @t p $
        SBV.sRotateLeft l r
  sbvRotateRightTerm ::
    forall proxy n.
    (KnownIsZero n) =>
    proxy n ->
    SBVType n t ->
    SBVType n t ->
    SBVType n t
  sbvRotateRightTerm p l r =
    withNonFuncPrim @t p $
      withSbvRotateTermConstraint @t p $
        SBV.sRotateRight l r

class (SupportedNonFuncPrim t, Num t) => PEvalNumTerm t where
  pevalAddNumTerm :: Term t -> Term t -> Term t
  pevalNegNumTerm :: Term t -> Term t
  pevalMulNumTerm :: Term t -> Term t -> Term t
  pevalAbsNumTerm :: Term t -> Term t
  pevalSignumNumTerm :: Term t -> Term t
  withSbvNumTermConstraint ::
    (KnownIsZero n) =>
    proxy n ->
    (((Num (SBVType n t)) => r)) ->
    r
  sbvAddNumTerm ::
    forall proxy n.
    (KnownIsZero n) =>
    proxy n ->
    SBVType n t ->
    SBVType n t ->
    SBVType n t
  sbvAddNumTerm p l r = withSbvNumTermConstraint @t p $ l + r
  sbvNegNumTerm ::
    forall proxy n.
    (KnownIsZero n) =>
    proxy n ->
    SBVType n t ->
    SBVType n t
  sbvNegNumTerm p l = withSbvNumTermConstraint @t p $ -l
  sbvMulNumTerm ::
    forall proxy n.
    (KnownIsZero n) =>
    proxy n ->
    SBVType n t ->
    SBVType n t ->
    SBVType n t
  sbvMulNumTerm p l r = withSbvNumTermConstraint @t p $ l * r
  sbvAbsNumTerm ::
    forall proxy n.
    (KnownIsZero n) =>
    proxy n ->
    SBVType n t ->
    SBVType n t
  sbvAbsNumTerm p l = withSbvNumTermConstraint @t p $ abs l
  sbvSignumNumTerm ::
    forall proxy n.
    (KnownIsZero n) =>
    proxy n ->
    SBVType n t ->
    SBVType n t
  sbvSignumNumTerm p l = withSbvNumTermConstraint @t p $ signum l

pevalSubNumTerm :: (PEvalNumTerm a) => Term a -> Term a -> Term a
pevalSubNumTerm l r = pevalAddNumTerm l (pevalNegNumTerm r)

class (SupportedNonFuncPrim t, Ord t) => PEvalOrdTerm t where
  pevalLtOrdTerm :: Term t -> Term t -> Term Bool
  pevalLeOrdTerm :: Term t -> Term t -> Term Bool
  withSbvOrdTermConstraint ::
    (KnownIsZero n) =>
    proxy n ->
    (((SBV.OrdSymbolic (SBVType n t)) => r)) ->
    r
  sbvLtOrdTerm ::
    (KnownIsZero n) =>
    proxy n ->
    SBVType n t ->
    SBVType n t ->
    SBV.SBV Bool
  sbvLtOrdTerm p l r = withSbvOrdTermConstraint @t p $ l SBV..< r
  sbvLeOrdTerm ::
    (KnownIsZero n) =>
    proxy n ->
    SBVType n t ->
    SBVType n t ->
    SBV.SBV Bool
  sbvLeOrdTerm p l r = withSbvOrdTermConstraint @t p $ l SBV..<= r

pevalGtOrdTerm :: (PEvalOrdTerm a) => Term a -> Term a -> Term Bool
pevalGtOrdTerm = flip pevalLtOrdTerm

pevalGeOrdTerm :: (PEvalOrdTerm a) => Term a -> Term a -> Term Bool
pevalGeOrdTerm = flip pevalLeOrdTerm

class (SupportedNonFuncPrim t, Integral t) => PEvalDivModIntegralTerm t where
  pevalDivIntegralTerm :: Term t -> Term t -> Term t
  pevalModIntegralTerm :: Term t -> Term t -> Term t
  pevalQuotIntegralTerm :: Term t -> Term t -> Term t
  pevalRemIntegralTerm :: Term t -> Term t -> Term t
  withSbvDivModIntegralTermConstraint ::
    (KnownIsZero n) =>
    proxy n ->
    (((SBV.SDivisible (SBVType n t)) => r)) ->
    r
  sbvDivIntegralTerm ::
    forall proxy n.
    (KnownIsZero n) =>
    proxy n ->
    SBVType n t ->
    SBVType n t ->
    SBVType n t
  sbvDivIntegralTerm p l r =
    withSbvDivModIntegralTermConstraint @t p $ l `SBV.sDiv` r
  sbvModIntegralTerm ::
    forall proxy n.
    (KnownIsZero n) =>
    proxy n ->
    SBVType n t ->
    SBVType n t ->
    SBVType n t
  sbvModIntegralTerm p l r =
    withSbvDivModIntegralTermConstraint @t p $ l `SBV.sMod` r
  sbvQuotIntegralTerm ::
    forall proxy n.
    (KnownIsZero n) =>
    proxy n ->
    SBVType n t ->
    SBVType n t ->
    SBVType n t
  sbvQuotIntegralTerm p l r =
    withSbvDivModIntegralTermConstraint @t p $ l `SBV.sQuot` r
  sbvRemIntegralTerm ::
    forall proxy n.
    (KnownIsZero n) =>
    proxy n ->
    SBVType n t ->
    SBVType n t ->
    SBVType n t
  sbvRemIntegralTerm p l r =
    withSbvDivModIntegralTermConstraint @t p $ l `SBV.sRem` r

class
  ( PEvalBVTerm s,
    PEvalBVTerm u,
    forall n. (KnownNat n, 1 <= n) => SupportedNonFuncPrim (u n),
    forall n. (KnownNat n, 1 <= n) => SupportedNonFuncPrim (s n),
    forall n. (KnownNat n, 1 <= n) => SignConversion (u n) (s n)
  ) =>
  PEvalBVSignConversionTerm u s
    | u -> s,
      s -> u
  where
  pevalBVToSignedTerm :: (KnownNat n, 1 <= n) => Term (u n) -> Term (s n)
  pevalBVToUnsignedTerm :: (KnownNat n, 1 <= n) => Term (s n) -> Term (u n)
  withSbvSignConversionTermConstraint ::
    forall n integerBitwidth p q r.
    (KnownIsZero integerBitwidth, KnownNat n, 1 <= n) =>
    p n ->
    q integerBitwidth ->
    ( ( ( Integral (NonFuncSBVBaseType integerBitwidth (u n)),
          Integral (NonFuncSBVBaseType integerBitwidth (s n))
        ) =>
        r
      )
    ) ->
    r
  sbvToSigned ::
    forall n integerBitwidth o p q.
    (KnownIsZero integerBitwidth, KnownNat n, 1 <= n) =>
    o u ->
    p n ->
    q integerBitwidth ->
    SBVType integerBitwidth (u n) ->
    SBVType integerBitwidth (s n)
  sbvToSigned _ _ qint u =
    withNonFuncPrim @(u n) qint $
      withNonFuncPrim @(s n) qint $
        withSbvSignConversionTermConstraint @u @s (Proxy @n) qint $
          SBV.sFromIntegral u
  sbvToUnsigned ::
    forall n integerBitwidth o p q.
    (KnownIsZero integerBitwidth, KnownNat n, 1 <= n) =>
    o s ->
    p n ->
    q integerBitwidth ->
    SBVType integerBitwidth (s n) ->
    SBVType integerBitwidth (u n)
  sbvToUnsigned _ _ qint u =
    withNonFuncPrim @(u n) qint $
      withNonFuncPrim @(s n) qint $
        withSbvSignConversionTermConstraint @u @s (Proxy @n) qint $
          SBV.sFromIntegral u

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
    (KnownIsZero n, KnownNat l, KnownNat r, 1 <= l, 1 <= r) =>
    p0 n ->
    p1 l ->
    p2 r ->
    SBVType n (bv l) ->
    SBVType n (bv r) ->
    SBVType n (bv (l + r))
  sbvBVExtendTerm ::
    (KnownIsZero n, KnownNat l, KnownNat r, 1 <= l, 1 <= r, l <= r) =>
    p0 n ->
    p1 l ->
    p2 r ->
    Bool ->
    SBVType n (bv l) ->
    SBVType n (bv r)
  sbvBVSelectTerm ::
    ( KnownIsZero int,
      KnownNat ix,
      KnownNat w,
      KnownNat n,
      1 <= n,
      1 <= w,
      ix + w <= n
    ) =>
    p0 int ->
    p1 ix ->
    p2 w ->
    p3 n ->
    SBVType int (bv n) ->
    SBVType int (bv w)

class (SupportedNonFuncPrim t, Fractional t) => PEvalFractionalTerm t where
  pevalFdivTerm :: Term t -> Term t -> Term t
  pevalRecipTerm :: Term t -> Term t
  withSbvFractionalTermConstraint ::
    (KnownIsZero n) =>
    proxy n ->
    (((Fractional (SBVType n t)) => r)) ->
    r
  sbvFdivTerm ::
    (KnownIsZero n) =>
    proxy n ->
    SBVType n t ->
    SBVType n t ->
    SBVType n t
  sbvFdivTerm p l r = withSbvFractionalTermConstraint @t p $ l / r
  sbvRecipTerm ::
    (KnownIsZero n) =>
    proxy n ->
    SBVType n t ->
    SBVType n t
  sbvRecipTerm p l = withSbvFractionalTermConstraint @t p $ recip l

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

class (SupportedNonFuncPrim t) => PEvalFloatingTerm t where
  pevalFloatingUnaryTerm :: FloatingUnaryOp -> Term t -> Term t
  pevalPowerTerm :: Term t -> Term t -> Term t
  withSbvFloatingTermConstraint ::
    (KnownIsZero n) =>
    proxy n ->
    (((Floating (SBVType n t)) => r)) ->
    r
  sbvPowerTerm ::
    (KnownIsZero n) =>
    proxy n ->
    SBVType n t ->
    SBVType n t ->
    SBVType n t
  sbvPowerTerm p = withSbvFloatingTermConstraint @t p (**)
  sbvFloatingUnaryTerm ::
    (KnownIsZero n) =>
    proxy n ->
    FloatingUnaryOp ->
    SBVType n t ->
    SBVType n t
  sbvFloatingUnaryTerm p op l =
    withSbvFloatingTermConstraint @t p $
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

class
  (SupportedPrim arg, SupportedPrim t, Lift tag, NFData tag, Show tag, Typeable tag, Eq tag, Hashable tag) =>
  UnaryOp tag arg t
    | tag arg -> t
  where
  pevalUnary :: (Typeable tag, Typeable t) => tag -> Term arg -> Term t
  pformatUnary :: tag -> Term arg -> String

class
  ( SupportedPrim arg1,
    SupportedPrim arg2,
    SupportedPrim t,
    Lift tag,
    NFData tag,
    Show tag,
    Typeable tag,
    Eq tag,
    Hashable tag
  ) =>
  BinaryOp tag arg1 arg2 t
    | tag arg1 arg2 -> t
  where
  pevalBinary :: (Typeable tag, Typeable t) => tag -> Term arg1 -> Term arg2 -> Term t
  pformatBinary :: tag -> Term arg1 -> Term arg2 -> String

class
  ( SupportedPrim arg1,
    SupportedPrim arg2,
    SupportedPrim arg3,
    SupportedPrim t,
    Lift tag,
    NFData tag,
    Show tag,
    Typeable tag,
    Eq tag,
    Hashable tag
  ) =>
  TernaryOp tag arg1 arg2 arg3 t
    | tag arg1 arg2 arg3 -> t
  where
  pevalTernary :: (Typeable tag, Typeable t) => tag -> Term arg1 -> Term arg2 -> Term arg3 -> Term t
  pformatTernary :: tag -> Term arg1 -> Term arg2 -> Term arg3 -> String

-- Typed Symbols

data SymbolKind = ConstantKind | AnyKind

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

type TypedConstantSymbol = TypedSymbol 'ConstantKind

type TypedAnySymbol = TypedSymbol 'AnyKind

instance Eq (TypedSymbol knd t) where
  TypedSymbol x == TypedSymbol y = x == y

instance Ord (TypedSymbol knd t) where
  TypedSymbol x <= TypedSymbol y = x <= y

instance Lift (TypedSymbol knd t) where
  liftTyped (TypedSymbol x) = [||TypedSymbol x||]

instance Show (TypedSymbol knd t) where
  show (TypedSymbol symbol) = show symbol ++ " :: " ++ show (typeRep @t)

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

withSymbolSupported :: TypedSymbol knd t -> ((SupportedPrim t) => a) -> a
withSymbolSupported (TypedSymbol _) a = a

withSymbolKind :: TypedSymbol knd t -> ((IsSymbolKind knd) => a) -> a
withSymbolKind (TypedSymbol _) a = a

-- | A non-index symbol. Type information are checked at runtime.
data SomeTypedSymbol knd where
  SomeTypedSymbol ::
    forall knd t.
    TypeRep t ->
    TypedSymbol knd t ->
    SomeTypedSymbol knd

type SomeTypedConstantSymbol = SomeTypedSymbol 'ConstantKind

type SomeTypedAnySymbol = SomeTypedSymbol 'AnyKind

instance NFData (SomeTypedSymbol knd) where
  rnf (SomeTypedSymbol p s) = rnf (SomeTypeRep p) `seq` rnf s

instance Eq (SomeTypedSymbol knd) where
  (SomeTypedSymbol t1 s1) == (SomeTypedSymbol t2 s2) = case eqTypeRep t1 t2 of
    Just HRefl -> s1 == s2
    _ -> False

instance Ord (SomeTypedSymbol knd) where
  (SomeTypedSymbol t1 s1) <= (SomeTypedSymbol t2 s2) =
    SomeTypeRep t1 < SomeTypeRep t2
      || ( case eqTypeRep t1 t2 of
             Just HRefl -> s1 <= s2
             _ -> False
         )

instance Hashable (SomeTypedSymbol knd) where
  hashWithSalt s (SomeTypedSymbol t1 s1) = s `hashWithSalt` s1 `hashWithSalt` t1

instance Show (SomeTypedSymbol knd) where
  show (SomeTypedSymbol _ s) = show s

someTypedSymbol :: forall knd t. TypedSymbol knd t -> SomeTypedSymbol knd
someTypedSymbol s@(TypedSymbol _) = SomeTypedSymbol (typeRep @t) s

-- Terms

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

data FPUnaryOp = FPAbs | FPNeg
  deriving (Eq, Ord, Generic, Hashable, Lift, NFData)

instance Show FPUnaryOp where
  show FPAbs = "fp.abs"
  show FPNeg = "fp.neg"

data FPBinaryOp = FPRem | FPMin | FPMax
  deriving (Eq, Ord, Generic, Hashable, Lift, NFData)

instance Show FPBinaryOp where
  show FPRem = "fp.rem"
  show FPMin = "fp.min"
  show FPMax = "fp.max"

data FPRoundingUnaryOp = FPSqrt | FPRoundToIntegral
  deriving (Eq, Ord, Generic, Hashable, Lift, NFData)

instance Show FPRoundingUnaryOp where
  show FPSqrt = "fp.sqrt"
  show FPRoundToIntegral = "fp.roundToIntegral"

data FPRoundingBinaryOp = FPAdd | FPSub | FPMul | FPDiv
  deriving (Eq, Ord, Generic, Hashable, Lift, NFData)

instance Show FPRoundingBinaryOp where
  show FPAdd = "fp.add"
  show FPSub = "fp.sub"
  show FPMul = "fp.mul"
  show FPDiv = "fp.div"

data Term t where
  ConTerm :: (SupportedPrim t) => {-# UNPACK #-} !Id -> !t -> Term t
  SymTerm :: (SupportedPrim t) => {-# UNPACK #-} !Id -> !(TypedSymbol 'AnyKind t) -> Term t
  ForallTerm :: (SupportedNonFuncPrim t) => {-# UNPACK #-} !Id -> !(TypedSymbol 'ConstantKind t) -> !(Term Bool) -> Term Bool
  ExistsTerm :: (SupportedNonFuncPrim t) => {-# UNPACK #-} !Id -> !(TypedSymbol 'ConstantKind t) -> !(Term Bool) -> Term Bool
  UnaryTerm ::
    (UnaryOp tag arg t) =>
    {-# UNPACK #-} !Id ->
    !tag ->
    !(Term arg) ->
    Term t
  BinaryTerm ::
    (BinaryOp tag arg1 arg2 t) =>
    {-# UNPACK #-} !Id ->
    !tag ->
    !(Term arg1) ->
    !(Term arg2) ->
    Term t
  TernaryTerm ::
    (TernaryOp tag arg1 arg2 arg3 t) =>
    {-# UNPACK #-} !Id ->
    !tag ->
    !(Term arg1) ->
    !(Term arg2) ->
    !(Term arg3) ->
    Term t
  NotTerm :: {-# UNPACK #-} !Id -> !(Term Bool) -> Term Bool
  OrTerm :: {-# UNPACK #-} !Id -> !(Term Bool) -> !(Term Bool) -> Term Bool
  AndTerm :: {-# UNPACK #-} !Id -> !(Term Bool) -> !(Term Bool) -> Term Bool
  EqTerm ::
    (SupportedNonFuncPrim t) =>
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    !(Term t) ->
    Term Bool
  ITETerm ::
    (SupportedPrim t) =>
    {-# UNPACK #-} !Id ->
    !(Term Bool) ->
    !(Term t) ->
    !(Term t) ->
    Term t
  AddNumTerm ::
    (PEvalNumTerm t) =>
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    !(Term t) ->
    Term t
  NegNumTerm ::
    (PEvalNumTerm t) =>
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    Term t
  MulNumTerm ::
    (PEvalNumTerm t) =>
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    !(Term t) ->
    Term t
  AbsNumTerm ::
    (PEvalNumTerm t) => {-# UNPACK #-} !Id -> !(Term t) -> Term t
  SignumNumTerm :: (PEvalNumTerm t) => {-# UNPACK #-} !Id -> !(Term t) -> Term t
  LtOrdTerm ::
    (PEvalOrdTerm t) =>
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    !(Term t) ->
    Term Bool
  LeOrdTerm ::
    (PEvalOrdTerm t) =>
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    !(Term t) ->
    Term Bool
  AndBitsTerm ::
    (PEvalBitwiseTerm t) =>
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    !(Term t) ->
    Term t
  OrBitsTerm ::
    (PEvalBitwiseTerm t) =>
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    !(Term t) ->
    Term t
  XorBitsTerm ::
    (PEvalBitwiseTerm t) =>
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    !(Term t) ->
    Term t
  ComplementBitsTerm ::
    (PEvalBitwiseTerm t) =>
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    Term t
  ShiftLeftTerm ::
    (PEvalShiftTerm t) => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term t
  ShiftRightTerm ::
    (PEvalShiftTerm t) => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term t
  RotateLeftTerm ::
    (PEvalRotateTerm t) =>
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    !(Term t) ->
    Term t
  RotateRightTerm ::
    (PEvalRotateTerm t) =>
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    !(Term t) ->
    Term t
  ToSignedTerm ::
    (PEvalBVSignConversionTerm u s, KnownNat n, 1 <= n) =>
    {-# UNPACK #-} !Id ->
    !(Term (u n)) ->
    Term (s n)
  ToUnsignedTerm ::
    (PEvalBVSignConversionTerm u s, KnownNat n, 1 <= n) =>
    {-# UNPACK #-} !Id ->
    !(Term (s n)) ->
    Term (u n)
  BVConcatTerm ::
    ( PEvalBVTerm bv,
      KnownNat l,
      KnownNat r,
      KnownNat (l + r),
      1 <= l,
      1 <= r,
      1 <= l + r
    ) =>
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
    {-# UNPACK #-} !Id ->
    !(TypeRep ix) ->
    !(TypeRep w) ->
    !(Term (bv n)) ->
    Term (bv w)
  BVExtendTerm ::
    (PEvalBVTerm bv, KnownNat l, KnownNat r, 1 <= l, 1 <= r, l <= r) =>
    {-# UNPACK #-} !Id ->
    !Bool ->
    !(TypeRep r) ->
    !(Term (bv l)) ->
    Term (bv r)
  ApplyTerm ::
    ( SupportedPrim a,
      SupportedPrim b,
      SupportedPrim f,
      PEvalApplyTerm f a b
    ) =>
    {-# UNPACK #-} !Id ->
    !(Term f) ->
    !(Term a) ->
    Term b
  DivIntegralTerm ::
    (PEvalDivModIntegralTerm t) =>
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    !(Term t) ->
    Term t
  ModIntegralTerm ::
    (PEvalDivModIntegralTerm t) =>
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    !(Term t) ->
    Term t
  QuotIntegralTerm ::
    (PEvalDivModIntegralTerm t) =>
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    !(Term t) ->
    Term t
  RemIntegralTerm ::
    (PEvalDivModIntegralTerm t) =>
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    !(Term t) ->
    Term t
  FPTraitTerm ::
    (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
    {-# UNPACK #-} !Id ->
    !FPTrait ->
    !(Term (FP eb sb)) ->
    Term Bool
  FdivTerm ::
    (PEvalFractionalTerm t) =>
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    !(Term t) ->
    Term t
  RecipTerm ::
    (PEvalFractionalTerm t) =>
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    Term t
  FloatingUnaryTerm ::
    (PEvalFloatingTerm t) =>
    {-# UNPACK #-} !Id ->
    !FloatingUnaryOp ->
    !(Term t) ->
    Term t
  PowerTerm ::
    (PEvalFloatingTerm t) =>
    {-# UNPACK #-} !Id ->
    !(Term t) ->
    !(Term t) ->
    Term t
  FPUnaryTerm ::
    (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
    {-# UNPACK #-} !Id ->
    !FPUnaryOp ->
    !(Term (FP eb sb)) ->
    Term (FP eb sb)
  FPBinaryTerm ::
    (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
    {-# UNPACK #-} !Id ->
    !FPBinaryOp ->
    !(Term (FP eb sb)) ->
    !(Term (FP eb sb)) ->
    Term (FP eb sb)
  FPRoundingUnaryTerm ::
    (ValidFP eb sb, SupportedPrim (FP eb sb), SupportedPrim FPRoundingMode) =>
    {-# UNPACK #-} !Id ->
    !FPRoundingUnaryOp ->
    !(Term FPRoundingMode) ->
    !(Term (FP eb sb)) ->
    Term (FP eb sb)
  FPRoundingBinaryTerm ::
    (ValidFP eb sb, SupportedPrim (FP eb sb), SupportedPrim FPRoundingMode) =>
    {-# UNPACK #-} !Id ->
    !FPRoundingBinaryOp ->
    !(Term FPRoundingMode) ->
    !(Term (FP eb sb)) ->
    !(Term (FP eb sb)) ->
    Term (FP eb sb)
  FPFMATerm ::
    (ValidFP eb sb, SupportedPrim (FP eb sb), SupportedPrim FPRoundingMode) =>
    {-# UNPACK #-} !Id ->
    !(Term FPRoundingMode) ->
    !(Term (FP eb sb)) ->
    !(Term (FP eb sb)) ->
    !(Term (FP eb sb)) ->
    Term (FP eb sb)

identity :: Term t -> Id
identity = snd . identityWithTypeRep
{-# INLINE identity #-}

identityWithTypeRep :: forall t. Term t -> (SomeTypeRep, Id)
identityWithTypeRep (ConTerm i _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (SymTerm i _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (ForallTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (ExistsTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (UnaryTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (BinaryTerm i _ _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (TernaryTerm i _ _ _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (NotTerm i _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (OrTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (AndTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (EqTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (ITETerm i _ _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (AddNumTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (NegNumTerm i _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (MulNumTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (AbsNumTerm i _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (SignumNumTerm i _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (LtOrdTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (LeOrdTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (AndBitsTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (OrBitsTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (XorBitsTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (ComplementBitsTerm i _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (ShiftLeftTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (ShiftRightTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (RotateLeftTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (RotateRightTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (ToSignedTerm i _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (ToUnsignedTerm i _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (BVConcatTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (BVSelectTerm i _ _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (BVExtendTerm i _ _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (ApplyTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (DivIntegralTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (ModIntegralTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (QuotIntegralTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (RemIntegralTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (FPTraitTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (FdivTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (RecipTerm i _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (FloatingUnaryTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (PowerTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (FPUnaryTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (FPBinaryTerm i _ _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (FPRoundingUnaryTerm i _ _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (FPRoundingBinaryTerm i _ _ _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (FPFMATerm i _ _ _ _) = (someTypeRep (Proxy @t), i)
{-# INLINE identityWithTypeRep #-}

introSupportedPrimConstraint :: forall t a. Term t -> ((SupportedPrim t) => a) -> a
introSupportedPrimConstraint ConTerm {} x = x
introSupportedPrimConstraint SymTerm {} x = x
introSupportedPrimConstraint ForallTerm {} x = x
introSupportedPrimConstraint ExistsTerm {} x = x
introSupportedPrimConstraint UnaryTerm {} x = x
introSupportedPrimConstraint BinaryTerm {} x = x
introSupportedPrimConstraint TernaryTerm {} x = x
introSupportedPrimConstraint NotTerm {} x = x
introSupportedPrimConstraint OrTerm {} x = x
introSupportedPrimConstraint AndTerm {} x = x
introSupportedPrimConstraint EqTerm {} x = x
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
introSupportedPrimConstraint ToSignedTerm {} x = x
introSupportedPrimConstraint ToUnsignedTerm {} x = x
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
{-# INLINE introSupportedPrimConstraint #-}

pformat :: forall t. (SupportedPrim t) => Term t -> String
pformat (ConTerm _ t) = pformatCon t
pformat (SymTerm _ sym) = pformatSym sym
pformat (ForallTerm _ sym arg) = "(forall " ++ show sym ++ " " ++ pformat arg ++ ")"
pformat (ExistsTerm _ sym arg) = "(exists " ++ show sym ++ " " ++ pformat arg ++ ")"
pformat (UnaryTerm _ tag arg1) = pformatUnary tag arg1
pformat (BinaryTerm _ tag arg1 arg2) = pformatBinary tag arg1 arg2
pformat (TernaryTerm _ tag arg1 arg2 arg3) = pformatTernary tag arg1 arg2 arg3
pformat (NotTerm _ arg) = "(! " ++ pformat arg ++ ")"
pformat (OrTerm _ arg1 arg2) = "(|| " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (AndTerm _ arg1 arg2) = "(&& " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (EqTerm _ arg1 arg2) = "(= " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (ITETerm _ cond arg1 arg2) = "(ite " ++ pformat cond ++ " " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (AddNumTerm _ arg1 arg2) = "(+ " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (NegNumTerm _ arg) = "(- " ++ pformat arg ++ ")"
pformat (MulNumTerm _ arg1 arg2) = "(* " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (AbsNumTerm _ arg) = "(abs " ++ pformat arg ++ ")"
pformat (SignumNumTerm _ arg) = "(signum " ++ pformat arg ++ ")"
pformat (LtOrdTerm _ arg1 arg2) = "(< " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (LeOrdTerm _ arg1 arg2) = "(<= " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (AndBitsTerm _ arg1 arg2) = "(& " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (OrBitsTerm _ arg1 arg2) = "(| " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (XorBitsTerm _ arg1 arg2) = "(^ " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (ComplementBitsTerm _ arg) = "(~ " ++ pformat arg ++ ")"
pformat (ShiftLeftTerm _ arg n) = "(shl " ++ pformat arg ++ " " ++ pformat n ++ ")"
pformat (ShiftRightTerm _ arg n) = "(shr " ++ pformat arg ++ " " ++ pformat n ++ ")"
pformat (RotateLeftTerm _ arg n) = "(rotl " ++ pformat arg ++ " " ++ pformat n ++ ")"
pformat (RotateRightTerm _ arg n) = "(rotr " ++ pformat arg ++ " " ++ pformat n ++ ")"
pformat (ToSignedTerm _ arg) = "(u2s " ++ pformat arg ++ " " ++ ")"
pformat (ToUnsignedTerm _ arg) = "(s2u " ++ pformat arg ++ " " ++ ")"
pformat (BVConcatTerm _ arg1 arg2) = "(bvconcat " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (BVSelectTerm _ ix w arg) = "(bvselect " ++ show ix ++ " " ++ show w ++ " " ++ pformat arg ++ ")"
pformat (BVExtendTerm _ signed n arg) =
  (if signed then "(bvsext " else "(bvzext ") ++ show n ++ " " ++ pformat arg ++ ")"
pformat (ApplyTerm _ func arg) = "(apply " ++ pformat func ++ " " ++ pformat arg ++ ")"
pformat (DivIntegralTerm _ arg1 arg2) = "(div " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (ModIntegralTerm _ arg1 arg2) = "(mod " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (QuotIntegralTerm _ arg1 arg2) = "(quot " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (RemIntegralTerm _ arg1 arg2) = "(rem " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (FPTraitTerm _ trait arg) = "(" ++ show trait ++ " " ++ pformat arg ++ ")"
pformat (FdivTerm _ arg1 arg2) = "(fdiv " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (RecipTerm _ arg) = "(recip " ++ pformat arg ++ ")"
pformat (FloatingUnaryTerm _ op arg) = "(" ++ show op ++ " " ++ pformat arg ++ ")"
pformat (PowerTerm _ arg1 arg2) = "(** " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (FPUnaryTerm _ op arg) = "(" ++ show op ++ " " ++ pformat arg ++ ")"
pformat (FPBinaryTerm _ op arg1 arg2) = "(" ++ show op ++ " " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (FPRoundingUnaryTerm _ op mode arg) = "(" ++ show op ++ " " ++ pformat mode ++ " " ++ pformat arg ++ ")"
pformat (FPRoundingBinaryTerm _ op mode arg1 arg2) =
  "(" ++ show op ++ " " ++ pformat mode ++ " " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (FPFMATerm _ mode arg1 arg2 arg3) =
  "(fp.fma " ++ pformat mode ++ " " ++ pformat arg1 ++ " " ++ pformat arg2 ++ " " ++ pformat arg3 ++ ")"
{-# INLINE pformat #-}

instance NFData (Term a) where
  rnf i = identity i `seq` ()

instance Lift (Term t) where
  liftTyped (ConTerm _ i) = [||conTerm i||]
  liftTyped (SymTerm _ sym) = [||symTerm (unTypedSymbol sym)||]
  liftTyped (ForallTerm _ sym arg) = [||forallTerm sym arg||]
  liftTyped (ExistsTerm _ sym arg) = [||existsTerm sym arg||]
  liftTyped (UnaryTerm _ tag arg) = [||constructUnary tag arg||]
  liftTyped (BinaryTerm _ tag arg1 arg2) = [||constructBinary tag arg1 arg2||]
  liftTyped (TernaryTerm _ tag arg1 arg2 arg3) = [||constructTernary tag arg1 arg2 arg3||]
  liftTyped (NotTerm _ arg) = [||notTerm arg||]
  liftTyped (OrTerm _ arg1 arg2) = [||orTerm arg1 arg2||]
  liftTyped (AndTerm _ arg1 arg2) = [||andTerm arg1 arg2||]
  liftTyped (EqTerm _ arg1 arg2) = [||eqTerm arg1 arg2||]
  liftTyped (ITETerm _ cond arg1 arg2) = [||iteTerm cond arg1 arg2||]
  liftTyped (AddNumTerm _ arg1 arg2) = [||addNumTerm arg1 arg2||]
  liftTyped (NegNumTerm _ arg) = [||negNumTerm arg||]
  liftTyped (MulNumTerm _ arg1 arg2) = [||mulNumTerm arg1 arg2||]
  liftTyped (AbsNumTerm _ arg) = [||absNumTerm arg||]
  liftTyped (SignumNumTerm _ arg) = [||signumNumTerm arg||]
  liftTyped (LtOrdTerm _ arg1 arg2) = [||ltOrdTerm arg1 arg2||]
  liftTyped (LeOrdTerm _ arg1 arg2) = [||leOrdTerm arg1 arg2||]
  liftTyped (AndBitsTerm _ arg1 arg2) = [||andBitsTerm arg1 arg2||]
  liftTyped (OrBitsTerm _ arg1 arg2) = [||orBitsTerm arg1 arg2||]
  liftTyped (XorBitsTerm _ arg1 arg2) = [||xorBitsTerm arg1 arg2||]
  liftTyped (ComplementBitsTerm _ arg) = [||complementBitsTerm arg||]
  liftTyped (ShiftLeftTerm _ arg n) = [||shiftLeftTerm arg n||]
  liftTyped (ShiftRightTerm _ arg n) = [||shiftRightTerm arg n||]
  liftTyped (RotateLeftTerm _ arg n) = [||rotateLeftTerm arg n||]
  liftTyped (RotateRightTerm _ arg n) = [||rotateRightTerm arg n||]
  liftTyped (ToSignedTerm _ v) = [||toSignedTerm v||]
  liftTyped (ToUnsignedTerm _ v) = [||toUnsignedTerm v||]
  liftTyped (BVConcatTerm _ arg1 arg2) = [||bvconcatTerm arg1 arg2||]
  liftTyped (BVSelectTerm _ (_ :: TypeRep ix) (_ :: TypeRep w) arg) = [||bvselectTerm (Proxy @ix) (Proxy @w) arg||]
  liftTyped (BVExtendTerm _ signed (_ :: TypeRep n) arg) = [||bvextendTerm signed (Proxy @n) arg||]
  liftTyped (ApplyTerm _ f arg) = [||applyTerm f arg||]
  liftTyped (DivIntegralTerm _ arg1 arg2) = [||divIntegralTerm arg1 arg2||]
  liftTyped (ModIntegralTerm _ arg1 arg2) = [||modIntegralTerm arg1 arg2||]
  liftTyped (QuotIntegralTerm _ arg1 arg2) = [||quotIntegralTerm arg1 arg2||]
  liftTyped (RemIntegralTerm _ arg1 arg2) = [||remIntegralTerm arg1 arg2||]
  liftTyped (FPTraitTerm _ trait arg) = [||fpTraitTerm trait arg||]
  liftTyped (FdivTerm _ arg1 arg2) = [||fdivTerm arg1 arg2||]
  liftTyped (RecipTerm _ arg) = [||recipTerm arg||]
  liftTyped (FloatingUnaryTerm _ op arg) = [||floatingUnaryTerm op arg||]
  liftTyped (PowerTerm _ arg1 arg2) = [||powerTerm arg1 arg2||]
  liftTyped (FPUnaryTerm _ op arg) = [||fpUnaryTerm op arg||]
  liftTyped (FPBinaryTerm _ op arg1 arg2) = [||fpBinaryTerm op arg1 arg2||]
  liftTyped (FPRoundingUnaryTerm _ op mode arg) = [||fpRoundingUnaryTerm op mode arg||]
  liftTyped (FPRoundingBinaryTerm _ op mode arg1 arg2) = [||fpRoundingBinaryTerm op mode arg1 arg2||]
  liftTyped (FPFMATerm _ mode arg1 arg2 arg3) = [||fpFMATerm mode arg1 arg2 arg3||]

instance Show (Term ty) where
  show (ConTerm i v) = "ConTerm{id=" ++ show i ++ ", v=" ++ show v ++ "}"
  show (SymTerm i name) =
    "SymTerm{id="
      ++ show i
      ++ ", name="
      ++ show name
      ++ ", type="
      ++ show (typeRep @ty)
      ++ "}"
  show (ForallTerm i sym arg) = "Forall{id=" ++ show i ++ ", sym=" ++ show sym ++ ", arg=" ++ show arg ++ "}"
  show (ExistsTerm i sym arg) = "Exists{id=" ++ show i ++ ", sym=" ++ show sym ++ ", arg=" ++ show arg ++ "}"
  show (UnaryTerm i tag arg) = "Unary{id=" ++ show i ++ ", tag=" ++ show tag ++ ", arg=" ++ show arg ++ "}"
  show (BinaryTerm i tag arg1 arg2) =
    "Binary{id="
      ++ show i
      ++ ", tag="
      ++ show tag
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ "}"
  show (TernaryTerm i tag arg1 arg2 arg3) =
    "Ternary{id="
      ++ show i
      ++ ", tag="
      ++ show tag
      ++ ", arg1="
      ++ show arg1
      ++ ", arg2="
      ++ show arg2
      ++ ", arg3="
      ++ show arg3
      ++ "}"
  show (NotTerm i arg) = "Not{id=" ++ show i ++ ", arg=" ++ show arg ++ "}"
  show (OrTerm i arg1 arg2) = "Or{id=" ++ show i ++ ", arg1=" ++ show arg1 ++ ", arg2=" ++ show arg2 ++ "}"
  show (AndTerm i arg1 arg2) = "And{id=" ++ show i ++ ", arg1=" ++ show arg1 ++ ", arg2=" ++ show arg2 ++ "}"
  show (EqTerm i arg1 arg2) = "Eqv{id=" ++ show i ++ ", arg1=" ++ show arg1 ++ ", arg2=" ++ show arg2 ++ "}"
  show (ITETerm i cond l r) =
    "ITE{id="
      ++ show i
      ++ ", cond="
      ++ show cond
      ++ ", then="
      ++ show l
      ++ ", else="
      ++ show r
      ++ "}"
  show (AddNumTerm i arg1 arg2) = "AddNum{id=" ++ show i ++ ", arg1=" ++ show arg1 ++ ", arg2=" ++ show arg2 ++ "}"
  show (NegNumTerm i arg) = "NegNum{id=" ++ show i ++ ", arg=" ++ show arg ++ "}"
  show (MulNumTerm i arg1 arg2) = "MulNum{id=" ++ show i ++ ", arg1=" ++ show arg1 ++ ", arg2=" ++ show arg2 ++ "}"
  show (AbsNumTerm i arg) = "AbsNum{id=" ++ show i ++ ", arg=" ++ show arg ++ "}"
  show (SignumNumTerm i arg) = "SignumNum{id=" ++ show i ++ ", arg=" ++ show arg ++ "}"
  show (LtOrdTerm i arg1 arg2) = "LTNum{id=" ++ show i ++ ", arg1=" ++ show arg1 ++ ", arg2=" ++ show arg2 ++ "}"
  show (LeOrdTerm i arg1 arg2) = "LENum{id=" ++ show i ++ ", arg1=" ++ show arg1 ++ ", arg2=" ++ show arg2 ++ "}"
  show (AndBitsTerm i arg1 arg2) = "AndBits{id=" ++ show i ++ ", arg1=" ++ show arg1 ++ ", arg2=" ++ show arg2 ++ "}"
  show (OrBitsTerm i arg1 arg2) = "OrBits{id=" ++ show i ++ ", arg1=" ++ show arg1 ++ ", arg2=" ++ show arg2 ++ "}"
  show (XorBitsTerm i arg1 arg2) = "XorBits{id=" ++ show i ++ ", arg1=" ++ show arg1 ++ ", arg2=" ++ show arg2 ++ "}"
  show (ComplementBitsTerm i arg) = "ComplementBits{id=" ++ show i ++ ", arg=" ++ show arg ++ "}"
  show (ShiftLeftTerm i arg n) = "ShiftLeft{id=" ++ show i ++ ", arg=" ++ show arg ++ ", n=" ++ show n ++ "}"
  show (ShiftRightTerm i arg n) = "ShiftRight{id=" ++ show i ++ ", arg=" ++ show arg ++ ", n=" ++ show n ++ "}"
  show (RotateLeftTerm i arg n) = "RotateLeft{id=" ++ show i ++ ", arg=" ++ show arg ++ ", n=" ++ show n ++ "}"
  show (RotateRightTerm i arg n) = "RotateRight{id=" ++ show i ++ ", arg=" ++ show arg ++ ", n=" ++ show n ++ "}"
  show (ToSignedTerm i arg) = "ToSigned{id=" ++ show i ++ ", arg=" ++ show arg ++ "}"
  show (ToUnsignedTerm i arg) = "ToUnsigned{id=" ++ show i ++ ", arg=" ++ show arg ++ "}"
  show (BVConcatTerm i arg1 arg2) = "BVConcat{id=" ++ show i ++ ", arg1=" ++ show arg1 ++ ", arg2=" ++ show arg2 ++ "}"
  show (BVSelectTerm i ix w arg) =
    "BVSelect{id=" ++ show i ++ ", ix=" ++ show ix ++ ", w=" ++ show w ++ ", arg=" ++ show arg ++ "}"
  show (BVExtendTerm i signed n arg) =
    "BVExtend{id=" ++ show i ++ ", signed=" ++ show signed ++ ", n=" ++ show n ++ ", arg=" ++ show arg ++ "}"
  show (ApplyTerm i f arg) =
    "Apply{id=" ++ show i ++ ", f=" ++ show f ++ ", arg=" ++ show arg ++ "}"
  show (DivIntegralTerm i arg1 arg2) =
    "DivIntegral{id=" ++ show i ++ ", arg1=" ++ show arg1 ++ ", arg2=" ++ show arg2 ++ "}"
  show (ModIntegralTerm i arg1 arg2) =
    "ModIntegral{id=" ++ show i ++ ", arg1=" ++ show arg1 ++ ", arg2=" ++ show arg2 ++ "}"
  show (QuotIntegralTerm i arg1 arg2) =
    "QuotIntegral{id=" ++ show i ++ ", arg1=" ++ show arg1 ++ ", arg2=" ++ show arg2 ++ "}"
  show (RemIntegralTerm i arg1 arg2) =
    "RemIntegral{id=" ++ show i ++ ", arg1=" ++ show arg1 ++ ", arg2=" ++ show arg2 ++ "}"
  show (FPTraitTerm i trait arg) =
    "FPTrait{id=" ++ show i ++ ", trait=" ++ show trait ++ ", arg=" ++ show arg ++ "}"
  show (FdivTerm i arg1 arg2) = "Fdiv{id=" ++ show i ++ ", arg1=" ++ show arg1 ++ ", arg2=" ++ show arg2 ++ "}"
  show (RecipTerm i arg) = "Recip{id=" ++ show i ++ ", arg=" ++ show arg ++ "}"
  show (FloatingUnaryTerm i op arg) = "FloatingUnary{id=" ++ show i ++ ", op=" ++ show op ++ ", arg=" ++ show arg ++ "}"
  show (PowerTerm i arg1 arg2) = "Power{id=" ++ show i ++ ", arg1=" ++ show arg1 ++ ", arg2=" ++ show arg2 ++ "}"
  show (FPUnaryTerm i op arg) = "FPUnary{id=" ++ show i ++ ", op=" ++ show op ++ ", arg=" ++ show arg ++ "}"
  show (FPBinaryTerm i op arg1 arg2) =
    "FPBinary{id=" ++ show i ++ ", op=" ++ show op ++ ", arg1=" ++ show arg1 ++ ", arg2=" ++ show arg2 ++ "}"
  show (FPRoundingUnaryTerm i op mode arg) =
    "FPRoundingUnary{id=" ++ show i ++ ", op=" ++ show op ++ ", mode=" ++ show mode ++ ", arg=" ++ show arg ++ "}"
  show (FPRoundingBinaryTerm i op mode arg1 arg2) =
    "FPRoundingBinary{id="
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
  show (FPFMATerm i mode arg1 arg2 arg3) =
    "FPFMA{id="
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
  {-# INLINE show #-}

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
    formatted = introSupportedPrimConstraint v $ pformat v
    len = length formatted

instance (SupportedPrim t) => Eq (Term t) where
  (==) = (==) `on` identity

instance (SupportedPrim t) => Hashable (Term t) where
  hashWithSalt s t = hashWithSalt s $ identity t

-- Interning

data UTerm t where
  UConTerm :: (SupportedPrim t) => !t -> UTerm t
  USymTerm :: (SupportedPrim t) => !(TypedSymbol 'AnyKind t) -> UTerm t
  UForallTerm :: (SupportedNonFuncPrim t) => !(TypedSymbol 'ConstantKind t) -> !(Term Bool) -> UTerm Bool
  UExistsTerm :: (SupportedNonFuncPrim t) => !(TypedSymbol 'ConstantKind t) -> !(Term Bool) -> UTerm Bool
  UUnaryTerm :: (UnaryOp tag arg t) => !tag -> !(Term arg) -> UTerm t
  UBinaryTerm ::
    (BinaryOp tag arg1 arg2 t) =>
    !tag ->
    !(Term arg1) ->
    !(Term arg2) ->
    UTerm t
  UTernaryTerm ::
    (TernaryOp tag arg1 arg2 arg3 t) =>
    !tag ->
    !(Term arg1) ->
    !(Term arg2) ->
    !(Term arg3) ->
    UTerm t
  UNotTerm :: !(Term Bool) -> UTerm Bool
  UOrTerm :: !(Term Bool) -> !(Term Bool) -> UTerm Bool
  UAndTerm :: !(Term Bool) -> !(Term Bool) -> UTerm Bool
  UEqTerm :: (SupportedNonFuncPrim t) => !(Term t) -> !(Term t) -> UTerm Bool
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
  UToSignedTerm ::
    (PEvalBVSignConversionTerm u s, KnownNat n, 1 <= n) =>
    !(Term (u n)) ->
    UTerm (s n)
  UToUnsignedTerm ::
    (PEvalBVSignConversionTerm u s, KnownNat n, 1 <= n) =>
    !(Term (s n)) ->
    UTerm (u n)
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
    !(TypeRep r) ->
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

eqTypedId :: (TypeRep a, Id) -> (TypeRep b, Id) -> Bool
eqTypedId (a, i1) (b, i2) = i1 == i2 && eqTypeRepBool a b
{-# INLINE eqTypedId #-}

eqHeteroTag :: (Eq a) => (TypeRep a, a) -> (TypeRep b, b) -> Bool
eqHeteroTag (tpa, taga) (tpb, tagb) = eqHeteroRep tpa tpb taga tagb
{-# INLINE eqHeteroTag #-}

eqHeteroSymbol :: forall ta a tb b. TypedSymbol ta a -> TypedSymbol tb b -> Bool
eqHeteroSymbol (TypedSymbol taga) (TypedSymbol tagb) =
  case eqTypeRep (typeRep @a) (typeRep @b) of
    Just HRefl -> taga == tagb
    Nothing -> False
{-# INLINE eqHeteroSymbol #-}

eqHeteroSymbol0 :: (TypeRep a, TypedSymbol ta a) -> (TypeRep b, TypedSymbol tb b) -> Bool
eqHeteroSymbol0 (tpa, taga) (tpb, tagb) = case eqTypeRep tpb tpa of
  Just HRefl -> unTypedSymbol taga == unTypedSymbol tagb
  Nothing -> False
{-# INLINE eqHeteroSymbol0 #-}

instance (SupportedPrim t) => Interned (Term t) where
  type Uninterned (Term t) = UTerm t
  data Description (Term t) where
    DConTerm :: t -> Description (Term t)
    DSymTerm :: TypedSymbol 'AnyKind t -> Description (Term t)
    DForallTerm :: {-# UNPACK #-} !(TypeRep t, TypedSymbol 'ConstantKind t) -> {-# UNPACK #-} !Id -> Description (Term Bool)
    DExistsTerm :: {-# UNPACK #-} !(TypeRep t, TypedSymbol 'ConstantKind t) -> {-# UNPACK #-} !Id -> Description (Term Bool)
    DUnaryTerm ::
      (Eq tag, Hashable tag) =>
      {-# UNPACK #-} !(TypeRep tag, tag) ->
      {-# UNPACK #-} !(TypeRep arg, Id) ->
      Description (Term t)
    DBinaryTerm ::
      (Eq tag, Hashable tag) =>
      {-# UNPACK #-} !(TypeRep tag, tag) ->
      {-# UNPACK #-} !(TypeRep arg1, Id) ->
      {-# UNPACK #-} !(TypeRep arg2, Id) ->
      Description (Term t)
    DTernaryTerm ::
      (Eq tag, Hashable tag) =>
      {-# UNPACK #-} !(TypeRep tag, tag) ->
      {-# UNPACK #-} !(TypeRep arg1, Id) ->
      {-# UNPACK #-} !(TypeRep arg2, Id) ->
      {-# UNPACK #-} !(TypeRep arg3, Id) ->
      Description (Term t)
    DNotTerm :: {-# UNPACK #-} !Id -> Description (Term Bool)
    DOrTerm :: {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term Bool)
    DAndTerm :: {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term Bool)
    DEqTerm :: TypeRep args -> {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term Bool)
    DITETerm :: {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term t)
    DAddNumTerm :: {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term t)
    DNegNumTerm :: {-# UNPACK #-} !Id -> Description (Term t)
    DMulNumTerm :: {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term t)
    DAbsNumTerm :: {-# UNPACK #-} !Id -> Description (Term t)
    DSignumNumTerm :: {-# UNPACK #-} !Id -> Description (Term t)
    DLtOrdTerm :: TypeRep args -> {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term Bool)
    DLeOrdTerm :: TypeRep args -> {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term Bool)
    DAndBitsTerm :: {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term t)
    DOrBitsTerm :: {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term t)
    DXorBitsTerm :: {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term t)
    DComplementBitsTerm :: {-# UNPACK #-} !Id -> Description (Term t)
    DShiftLeftTerm :: {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term t)
    DShiftRightTerm :: {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term t)
    DRotateLeftTerm :: {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term t)
    DRotateRightTerm :: {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term t)
    DBVConcatTerm :: TypeRep bv1 -> TypeRep bv2 -> {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term t)
    DToSignedTerm ::
      !(TypeRep u, Id) ->
      Description (Term s)
    DToUnsignedTerm ::
      !(TypeRep s, Id) ->
      Description (Term u)
    DBVSelectTerm ::
      forall bv (n :: Nat) (w :: Nat) (ix :: Nat).
      !(TypeRep ix) ->
      !(TypeRep (bv n), Id) ->
      Description (Term (bv w))
    DBVExtendTerm ::
      forall bv (l :: Nat) (r :: Nat).
      !Bool ->
      !(TypeRep r) ->
      {-# UNPACK #-} !(TypeRep (bv l), Id) ->
      Description (Term (bv r))
    DApplyTerm ::
      ( PEvalApplyTerm f a b
      ) =>
      {-# UNPACK #-} !(TypeRep f, Id) ->
      {-# UNPACK #-} !(TypeRep a, Id) ->
      Description (Term b)
    DDivIntegralTerm :: {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term a)
    DModIntegralTerm :: {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term a)
    DQuotIntegralTerm :: {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term a)
    DRemIntegralTerm :: {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term a)
    DFPTraitTerm :: FPTrait -> {-# UNPACK #-} !Id -> Description (Term Bool)
    DFdivTerm :: {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term a)
    DRecipTerm :: {-# UNPACK #-} !Id -> Description (Term a)
    DFloatingUnaryTerm :: FloatingUnaryOp -> {-# UNPACK #-} !Id -> Description (Term a)
    DPowerTerm :: {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term a)
    DFPUnaryTerm :: FPUnaryOp -> {-# UNPACK #-} !Id -> Description (Term (FP eb sb))
    DFPBinaryTerm :: FPBinaryOp -> {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term (FP eb sb))
    DFPRoundingUnaryTerm :: FPRoundingUnaryOp -> {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term (FP eb sb))
    DFPRoundingBinaryTerm :: FPRoundingBinaryOp -> {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term (FP eb sb))
    DFPFMATerm :: {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term (FP eb sb))

  describe (UConTerm v) = DConTerm v
  describe ((USymTerm name) :: UTerm t) = DSymTerm @t name
  describe (UForallTerm (sym :: TypedSymbol 'ConstantKind arg) arg) =
    DForallTerm (typeRep :: TypeRep arg, sym) (identity arg)
  describe (UExistsTerm (sym :: TypedSymbol 'ConstantKind arg) arg) =
    DExistsTerm (typeRep :: TypeRep arg, sym) (identity arg)
  describe ((UUnaryTerm (tag :: tagt) (tm :: Term arg)) :: UTerm t) =
    DUnaryTerm (typeRep, tag) (typeRep :: TypeRep arg, identity tm)
  describe ((UBinaryTerm (tag :: tagt) (tm1 :: Term arg1) (tm2 :: Term arg2)) :: UTerm t) =
    DBinaryTerm @tagt @arg1 @arg2 @t (typeRep, tag) (typeRep, identity tm1) (typeRep, identity tm2)
  describe ((UTernaryTerm (tag :: tagt) (tm1 :: Term arg1) (tm2 :: Term arg2) (tm3 :: Term arg3)) :: UTerm t) =
    DTernaryTerm @tagt @arg1 @arg2 @arg3 @t
      (typeRep, tag)
      (typeRep, identity tm1)
      (typeRep, identity tm2)
      (typeRep, identity tm3)
  describe (UNotTerm arg) = DNotTerm (identity arg)
  describe (UOrTerm arg1 arg2) = DOrTerm (identity arg1) (identity arg2)
  describe (UAndTerm arg1 arg2) = DAndTerm (identity arg1) (identity arg2)
  describe (UEqTerm (arg1 :: Term arg) arg2) = DEqTerm (typeRep :: TypeRep arg) (identity arg1) (identity arg2)
  describe (UITETerm cond (l :: Term arg) r) = DITETerm (identity cond) (identity l) (identity r)
  describe (UAddNumTerm arg1 arg2) = DAddNumTerm (identity arg1) (identity arg2)
  describe (UNegNumTerm arg) = DNegNumTerm (identity arg)
  describe (UMulNumTerm arg1 arg2) = DMulNumTerm (identity arg1) (identity arg2)
  describe (UAbsNumTerm arg) = DAbsNumTerm (identity arg)
  describe (USignumNumTerm arg) = DSignumNumTerm (identity arg)
  describe (ULtOrdTerm (arg1 :: arg) arg2) = DLtOrdTerm (typeRep :: TypeRep arg) (identity arg1) (identity arg2)
  describe (ULeOrdTerm (arg1 :: arg) arg2) = DLeOrdTerm (typeRep :: TypeRep arg) (identity arg1) (identity arg2)
  describe (UAndBitsTerm arg1 arg2) = DAndBitsTerm (identity arg1) (identity arg2)
  describe (UOrBitsTerm arg1 arg2) = DOrBitsTerm (identity arg1) (identity arg2)
  describe (UXorBitsTerm arg1 arg2) = DXorBitsTerm (identity arg1) (identity arg2)
  describe (UComplementBitsTerm arg) = DComplementBitsTerm (identity arg)
  describe (UShiftLeftTerm arg n) = DShiftLeftTerm (identity arg) (identity n)
  describe (UShiftRightTerm arg n) = DShiftRightTerm (identity arg) (identity n)
  describe (URotateLeftTerm arg n) = DRotateLeftTerm (identity arg) (identity n)
  describe (URotateRightTerm arg n) = DRotateRightTerm (identity arg) (identity n)
  describe (UToSignedTerm (arg :: Term bv)) = DToSignedTerm (typeRep :: TypeRep bv, identity arg)
  describe (UToUnsignedTerm (arg :: Term bv)) = DToSignedTerm (typeRep :: TypeRep bv, identity arg)
  describe (UBVConcatTerm (arg1 :: bv1) (arg2 :: bv2)) =
    DBVConcatTerm (typeRep :: TypeRep bv1) (typeRep :: TypeRep bv2) (identity arg1) (identity arg2)
  describe (UBVSelectTerm (ix :: TypeRep ix) _ (arg :: Term arg)) =
    DBVSelectTerm ix (typeRep :: TypeRep arg, identity arg)
  describe (UBVExtendTerm signed (n :: TypeRep n) (arg :: Term arg)) =
    DBVExtendTerm signed n (typeRep :: TypeRep arg, identity arg)
  describe (UApplyTerm (f :: Term f) (arg :: Term a)) =
    DApplyTerm (typeRep :: TypeRep f, identity f) (typeRep :: TypeRep a, identity arg)
  describe (UDivIntegralTerm arg1 arg2) = DDivIntegralTerm (identity arg1) (identity arg2)
  describe (UModIntegralTerm arg1 arg2) = DModIntegralTerm (identity arg1) (identity arg2)
  describe (UQuotIntegralTerm arg1 arg2) = DRemIntegralTerm (identity arg1) (identity arg2)
  describe (URemIntegralTerm arg1 arg2) = DQuotIntegralTerm (identity arg1) (identity arg2)
  describe (UFPTraitTerm trait arg) = DFPTraitTerm trait (identity arg)
  describe (UFdivTerm arg1 arg2) = DFdivTerm (identity arg1) (identity arg2)
  describe (URecipTerm arg) = DRecipTerm (identity arg)
  describe (UFloatingUnaryTerm op arg) = DFloatingUnaryTerm op (identity arg)
  describe (UPowerTerm arg1 arg2) = DPowerTerm (identity arg1) (identity arg2)
  describe (UFPUnaryTerm op arg) = DFPUnaryTerm op (identity arg)
  describe (UFPBinaryTerm op arg1 arg2) = DFPBinaryTerm op (identity arg1) (identity arg2)
  describe (UFPRoundingUnaryTerm op mode arg) = DFPRoundingUnaryTerm op (identity mode) (identity arg)
  describe (UFPRoundingBinaryTerm op mode arg1 arg2) = DFPRoundingBinaryTerm op (identity mode) (identity arg1) (identity arg2)
  describe (UFPFMATerm mode arg1 arg2 arg3) = DFPFMATerm (identity mode) (identity arg1) (identity arg2) (identity arg3)

  identify i = go
    where
      go (UConTerm v) = ConTerm i v
      go (USymTerm v) = SymTerm i v
      go (UForallTerm sym arg) = ForallTerm i sym arg
      go (UExistsTerm sym arg) = ExistsTerm i sym arg
      go (UUnaryTerm tag tm) = UnaryTerm i tag tm
      go (UBinaryTerm tag tm1 tm2) = BinaryTerm i tag tm1 tm2
      go (UTernaryTerm tag tm1 tm2 tm3) = TernaryTerm i tag tm1 tm2 tm3
      go (UNotTerm arg) = NotTerm i arg
      go (UOrTerm arg1 arg2) = OrTerm i arg1 arg2
      go (UAndTerm arg1 arg2) = AndTerm i arg1 arg2
      go (UEqTerm arg1 arg2) = EqTerm i arg1 arg2
      go (UITETerm cond l r) = ITETerm i cond l r
      go (UAddNumTerm arg1 arg2) = AddNumTerm i arg1 arg2
      go (UNegNumTerm arg) = NegNumTerm i arg
      go (UMulNumTerm arg1 arg2) = MulNumTerm i arg1 arg2
      go (UAbsNumTerm arg) = AbsNumTerm i arg
      go (USignumNumTerm arg) = SignumNumTerm i arg
      go (ULtOrdTerm arg1 arg2) = LtOrdTerm i arg1 arg2
      go (ULeOrdTerm arg1 arg2) = LeOrdTerm i arg1 arg2
      go (UAndBitsTerm arg1 arg2) = AndBitsTerm i arg1 arg2
      go (UOrBitsTerm arg1 arg2) = OrBitsTerm i arg1 arg2
      go (UXorBitsTerm arg1 arg2) = XorBitsTerm i arg1 arg2
      go (UComplementBitsTerm arg) = ComplementBitsTerm i arg
      go (UShiftLeftTerm arg n) = ShiftLeftTerm i arg n
      go (UShiftRightTerm arg n) = ShiftRightTerm i arg n
      go (URotateLeftTerm arg n) = RotateLeftTerm i arg n
      go (URotateRightTerm arg n) = RotateRightTerm i arg n
      go (UToSignedTerm arg) = ToSignedTerm i arg
      go (UToUnsignedTerm arg) = ToUnsignedTerm i arg
      go (UBVConcatTerm arg1 arg2) = BVConcatTerm i arg1 arg2
      go (UBVSelectTerm ix w arg) = BVSelectTerm i ix w arg
      go (UBVExtendTerm signed n arg) = BVExtendTerm i signed n arg
      go (UApplyTerm f arg) = ApplyTerm i f arg
      go (UDivIntegralTerm arg1 arg2) = DivIntegralTerm i arg1 arg2
      go (UModIntegralTerm arg1 arg2) = ModIntegralTerm i arg1 arg2
      go (UQuotIntegralTerm arg1 arg2) = QuotIntegralTerm i arg1 arg2
      go (URemIntegralTerm arg1 arg2) = RemIntegralTerm i arg1 arg2
      go (UFPTraitTerm trait arg) = FPTraitTerm i trait arg
      go (UFdivTerm arg1 arg2) = FdivTerm i arg1 arg2
      go (URecipTerm arg) = RecipTerm i arg
      go (UFloatingUnaryTerm op arg) = FloatingUnaryTerm i op arg
      go (UPowerTerm arg1 arg2) = PowerTerm i arg1 arg2
      go (UFPUnaryTerm op arg) = FPUnaryTerm i op arg
      go (UFPBinaryTerm op arg1 arg2) = FPBinaryTerm i op arg1 arg2
      go (UFPRoundingUnaryTerm op mode arg) = FPRoundingUnaryTerm i op mode arg
      go (UFPRoundingBinaryTerm op mode arg1 arg2) = FPRoundingBinaryTerm i op mode arg1 arg2
      go (UFPFMATerm mode arg1 arg2 arg3) = FPFMATerm i mode arg1 arg2 arg3
  cache = termCache

instance (SupportedPrim t) => Eq (Description (Term t)) where
  DConTerm (l :: tyl) == DConTerm (r :: tyr) = cast @tyl @tyr l == Just r
  DSymTerm ls == DSymTerm rs = ls == rs
  DForallTerm ls li == DForallTerm rs ri = eqHeteroSymbol0 ls rs && li == ri
  DExistsTerm ls li == DExistsTerm rs ri = eqHeteroSymbol0 ls rs && li == ri
  DUnaryTerm (tagl :: tagl) li == DUnaryTerm (tagr :: tagr) ri = eqHeteroTag tagl tagr && eqTypedId li ri
  DBinaryTerm (tagl :: tagl) li1 li2 == DBinaryTerm (tagr :: tagr) ri1 ri2 =
    eqHeteroTag tagl tagr && eqTypedId li1 ri1 && eqTypedId li2 ri2
  DTernaryTerm (tagl :: tagl) li1 li2 li3 == DTernaryTerm (tagr :: tagr) ri1 ri2 ri3 =
    eqHeteroTag tagl tagr && eqTypedId li1 ri1 && eqTypedId li2 ri2 && eqTypedId li3 ri3
  DNotTerm li == DNotTerm ri = li == ri
  DOrTerm li1 li2 == DOrTerm ri1 ri2 = li1 == ri1 && li2 == ri2
  DAndTerm li1 li2 == DAndTerm ri1 ri2 = li1 == ri1 && li2 == ri2
  DEqTerm lrep li1 li2 == DEqTerm rrep ri1 ri2 = eqTypeRepBool lrep rrep && li1 == ri1 && li2 == ri2
  DITETerm lc li1 li2 == DITETerm rc ri1 ri2 = lc == rc && li1 == ri1 && li2 == ri2
  DAddNumTerm li1 li2 == DAddNumTerm ri1 ri2 = li1 == ri1 && li2 == ri2
  DNegNumTerm li == DNegNumTerm ri = li == ri
  DMulNumTerm li1 li2 == DMulNumTerm ri1 ri2 = li1 == ri1 && li2 == ri2
  DAbsNumTerm li == DAbsNumTerm ri = li == ri
  DSignumNumTerm li == DSignumNumTerm ri = li == ri
  DLtOrdTerm lrep li1 li2 == DLtOrdTerm rrep ri1 ri2 = eqTypeRepBool lrep rrep && li1 == ri1 && li2 == ri2
  DLeOrdTerm lrep li1 li2 == DLeOrdTerm rrep ri1 ri2 = eqTypeRepBool lrep rrep && li1 == ri1 && li2 == ri2
  DAndBitsTerm li1 li2 == DAndBitsTerm ri1 ri2 = li1 == ri1 && li2 == ri2
  DOrBitsTerm li1 li2 == DOrBitsTerm ri1 ri2 = li1 == ri1 && li2 == ri2
  DXorBitsTerm li1 li2 == DXorBitsTerm ri1 ri2 = li1 == ri1 && li2 == ri2
  DComplementBitsTerm li == DComplementBitsTerm ri = li == ri
  DShiftLeftTerm li ln == DShiftLeftTerm ri rn = li == ri && ln == rn
  DShiftRightTerm li ln == DShiftRightTerm ri rn = li == ri && ln == rn
  DRotateLeftTerm li ln == DRotateLeftTerm ri rn = li == ri && ln == rn
  DRotateRightTerm li ln == DRotateRightTerm ri rn = li == ri && ln == rn
  DToSignedTerm li == DToSignedTerm ri = eqTypedId li ri
  DToUnsignedTerm li == DToUnsignedTerm ri = eqTypedId li ri
  DBVConcatTerm lrep1 lrep2 li1 li2 == DBVConcatTerm rrep1 rrep2 ri1 ri2 =
    eqTypeRepBool lrep1 rrep1 && eqTypeRepBool lrep2 rrep2 && li1 == ri1 && li2 == ri2
  DBVSelectTerm lix li == DBVSelectTerm rix ri =
    eqTypeRepBool lix rix && eqTypedId li ri
  DBVExtendTerm lIsSigned ln li == DBVExtendTerm rIsSigned rn ri =
    lIsSigned == rIsSigned
      && eqTypeRepBool ln rn
      && eqTypedId li ri
  DApplyTerm lf li == DApplyTerm rf ri = eqTypedId lf rf && eqTypedId li ri
  DDivIntegralTerm li1 li2 == DDivIntegralTerm ri1 ri2 = li1 == ri1 && li2 == ri2
  DModIntegralTerm li1 li2 == DModIntegralTerm ri1 ri2 = li1 == ri1 && li2 == ri2
  DQuotIntegralTerm li1 li2 == DQuotIntegralTerm ri1 ri2 = li1 == ri1 && li2 == ri2
  DRemIntegralTerm li1 li2 == DRemIntegralTerm ri1 ri2 = li1 == ri1 && li2 == ri2
  DFPTraitTerm lt li == DFPTraitTerm rt ri = lt == rt && li == ri
  DFdivTerm li1 li2 == DFdivTerm ri1 ri2 = li1 == ri1 && li2 == ri2
  DRecipTerm li == DRecipTerm ri = li == ri
  DFloatingUnaryTerm lop li == DFloatingUnaryTerm rop ri = lop == rop && li == ri
  DPowerTerm li1 li2 == DPowerTerm ri1 ri2 = li1 == ri1 && li2 == ri2
  DFPUnaryTerm lop li == DFPUnaryTerm rop ri = lop == rop && li == ri
  DFPBinaryTerm lop li1 li2 == DFPBinaryTerm rop ri1 ri2 = lop == rop && li1 == ri1 && li2 == ri2
  DFPRoundingUnaryTerm lop lmode li == DFPRoundingUnaryTerm rop rmode ri =
    lop == rop && lmode == rmode && li == ri
  DFPRoundingBinaryTerm lop lmode li1 li2 == DFPRoundingBinaryTerm rop rmode ri1 ri2 =
    lop == rop && lmode == rmode && li1 == ri1 && li2 == ri2
  DFPFMATerm lmode li1 li2 li3 == DFPFMATerm rmode ri1 ri2 ri3 =
    lmode == rmode && li1 == ri1 && li2 == ri2 && li3 == ri3
  _ == _ = False

instance (SupportedPrim t) => Hashable (Description (Term t)) where
  hashWithSalt s (DConTerm c) = s `hashWithSalt` (0 :: Int) `hashWithSalt` c
  hashWithSalt s (DSymTerm name) = s `hashWithSalt` (1 :: Int) `hashWithSalt` name
  hashWithSalt s (DForallTerm sym id) = s `hashWithSalt` (48 :: Int) `hashWithSalt` sym `hashWithSalt` id
  hashWithSalt s (DExistsTerm sym id) = s `hashWithSalt` (49 :: Int) `hashWithSalt` sym `hashWithSalt` id
  hashWithSalt s (DUnaryTerm tag id1) = s `hashWithSalt` (2 :: Int) `hashWithSalt` tag `hashWithSalt` id1
  hashWithSalt s (DBinaryTerm tag id1 id2) =
    s `hashWithSalt` (3 :: Int) `hashWithSalt` tag `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DTernaryTerm tag id1 id2 id3) =
    s `hashWithSalt` (4 :: Int) `hashWithSalt` tag `hashWithSalt` id1 `hashWithSalt` id2 `hashWithSalt` id3
  hashWithSalt s (DNotTerm id1) = s `hashWithSalt` (5 :: Int) `hashWithSalt` id1
  hashWithSalt s (DOrTerm id1 id2) = s `hashWithSalt` (6 :: Int) `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DAndTerm id1 id2) = s `hashWithSalt` (7 :: Int) `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DEqTerm rep id1 id2) =
    s
      `hashWithSalt` (8 :: Int)
      `hashWithSalt` rep
      `hashWithSalt` id1
      `hashWithSalt` id2
  hashWithSalt s (DITETerm idc id1 id2) =
    s
      `hashWithSalt` (9 :: Int)
      `hashWithSalt` idc
      `hashWithSalt` id1
      `hashWithSalt` id2
  hashWithSalt s (DAddNumTerm id1 id2) = s `hashWithSalt` (10 :: Int) `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DNegNumTerm id1) = s `hashWithSalt` (11 :: Int) `hashWithSalt` id1
  hashWithSalt s (DMulNumTerm id1 id2) = s `hashWithSalt` (12 :: Int) `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DAbsNumTerm id1) = s `hashWithSalt` (13 :: Int) `hashWithSalt` id1
  hashWithSalt s (DSignumNumTerm id1) = s `hashWithSalt` (14 :: Int) `hashWithSalt` id1
  hashWithSalt s (DLtOrdTerm rep id1 id2) =
    s `hashWithSalt` (15 :: Int) `hashWithSalt` rep `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DLeOrdTerm rep id1 id2) =
    s `hashWithSalt` (16 :: Int) `hashWithSalt` rep `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DAndBitsTerm id1 id2) = s `hashWithSalt` (17 :: Int) `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DOrBitsTerm id1 id2) = s `hashWithSalt` (18 :: Int) `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DXorBitsTerm id1 id2) = s `hashWithSalt` (19 :: Int) `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DComplementBitsTerm id1) = s `hashWithSalt` (20 :: Int) `hashWithSalt` id1
  hashWithSalt s (DShiftLeftTerm id1 idn) = s `hashWithSalt` (38 :: Int) `hashWithSalt` id1 `hashWithSalt` idn
  hashWithSalt s (DShiftRightTerm id1 idn) = s `hashWithSalt` (39 :: Int) `hashWithSalt` id1 `hashWithSalt` idn
  hashWithSalt s (DRotateLeftTerm id1 idn) = s `hashWithSalt` (40 :: Int) `hashWithSalt` id1 `hashWithSalt` idn
  hashWithSalt s (DRotateRightTerm id1 idn) = s `hashWithSalt` (41 :: Int) `hashWithSalt` id1 `hashWithSalt` idn
  hashWithSalt s (DToSignedTerm id) = s `hashWithSalt` (23 :: Int) `hashWithSalt` id
  hashWithSalt s (DToUnsignedTerm id) = s `hashWithSalt` (24 :: Int) `hashWithSalt` id
  hashWithSalt s (DBVConcatTerm rep1 rep2 id1 id2) =
    s `hashWithSalt` (25 :: Int) `hashWithSalt` rep1 `hashWithSalt` rep2 `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DBVSelectTerm ix id1) = s `hashWithSalt` (26 :: Int) `hashWithSalt` ix `hashWithSalt` id1
  hashWithSalt s (DBVExtendTerm signed n id1) =
    s
      `hashWithSalt` (27 :: Int)
      `hashWithSalt` signed
      `hashWithSalt` n
      `hashWithSalt` id1
  hashWithSalt s (DDivIntegralTerm id1 id2) = s `hashWithSalt` (30 :: Int) `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DModIntegralTerm id1 id2) = s `hashWithSalt` (31 :: Int) `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DQuotIntegralTerm id1 id2) = s `hashWithSalt` (32 :: Int) `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DRemIntegralTerm id1 id2) = s `hashWithSalt` (33 :: Int) `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DApplyTerm id1 id2) = s `hashWithSalt` (38 :: Int) `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DFPTraitTerm trait id1) = s `hashWithSalt` (39 :: Int) `hashWithSalt` trait `hashWithSalt` id1
  hashWithSalt s (DFdivTerm id1 id2) = s `hashWithSalt` (40 :: Int) `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DRecipTerm id1) = s `hashWithSalt` (41 :: Int) `hashWithSalt` id1
  hashWithSalt s (DFloatingUnaryTerm op id1) = s `hashWithSalt` (42 :: Int) `hashWithSalt` op `hashWithSalt` id1
  hashWithSalt s (DPowerTerm id1 id2) = s `hashWithSalt` (48 :: Int) `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DFPUnaryTerm op id1) = s `hashWithSalt` (43 :: Int) `hashWithSalt` op `hashWithSalt` id1
  hashWithSalt s (DFPBinaryTerm op id1 id2) = s `hashWithSalt` (44 :: Int) `hashWithSalt` op `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DFPRoundingUnaryTerm op mode id1) =
    s `hashWithSalt` (45 :: Int) `hashWithSalt` op `hashWithSalt` mode `hashWithSalt` id1
  hashWithSalt s (DFPRoundingBinaryTerm op mode id1 id2) =
    s `hashWithSalt` (46 :: Int) `hashWithSalt` op `hashWithSalt` mode `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DFPFMATerm mode id1 id2 id3) =
    s `hashWithSalt` (47 :: Int) `hashWithSalt` mode `hashWithSalt` id1 `hashWithSalt` id2 `hashWithSalt` id3

internTerm :: forall t. (SupportedPrim t) => Uninterned (Term t) -> Term t
internTerm !bt = unsafeDupablePerformIO $ atomicModifyIORef' slot go
  where
    slot = getCache cache ! r
    !dt = describe bt
    !hdt = hash dt
    !wid = cacheWidth dt
    r = hdt `mod` wid
    go (CacheState i m) = case M.lookup dt m of
      Nothing -> let t = identify (wid * i + r) bt in (CacheState (i + 1) (M.insert dt t m), t)
      Just t -> (CacheState i m, t)

constructUnary ::
  forall tag arg t.
  (SupportedPrim t, UnaryOp tag arg t, Typeable tag, Typeable t, Show tag) =>
  tag ->
  Term arg ->
  Term t
constructUnary tag tm = let x = internTerm $ UUnaryTerm tag tm in x
{-# INLINE constructUnary #-}

constructBinary ::
  forall tag arg1 arg2 t.
  (SupportedPrim t, BinaryOp tag arg1 arg2 t, Typeable tag, Typeable t, Show tag) =>
  tag ->
  Term arg1 ->
  Term arg2 ->
  Term t
constructBinary tag tm1 tm2 = internTerm $ UBinaryTerm tag tm1 tm2
{-# INLINE constructBinary #-}

constructTernary ::
  forall tag arg1 arg2 arg3 t.
  (SupportedPrim t, TernaryOp tag arg1 arg2 arg3 t, Typeable tag, Typeable t, Show tag) =>
  tag ->
  Term arg1 ->
  Term arg2 ->
  Term arg3 ->
  Term t
constructTernary tag tm1 tm2 tm3 = internTerm $ UTernaryTerm tag tm1 tm2 tm3
{-# INLINE constructTernary #-}

conTerm :: (SupportedPrim t, Typeable t, Hashable t, Eq t, Show t) => t -> Term t
conTerm t = internTerm $ UConTerm t
{-# INLINE conTerm #-}

symTerm :: forall t. (SupportedPrim t, Typeable t) => Symbol -> Term t
symTerm t = internTerm $ USymTerm $ TypedSymbol t
{-# INLINE symTerm #-}

forallTerm :: (SupportedNonFuncPrim t, Typeable t) => TypedSymbol 'ConstantKind t -> Term Bool -> Term Bool
forallTerm sym arg = internTerm $ UForallTerm sym arg
{-# INLINE forallTerm #-}

existsTerm :: (SupportedNonFuncPrim t, Typeable t) => TypedSymbol 'ConstantKind t -> Term Bool -> Term Bool
existsTerm sym arg = internTerm $ UExistsTerm sym arg
{-# INLINE existsTerm #-}

ssymTerm :: (SupportedPrim t, Typeable t) => Identifier -> Term t
ssymTerm = symTerm . SimpleSymbol
{-# INLINE ssymTerm #-}

isymTerm :: (SupportedPrim t, Typeable t) => Identifier -> Int -> Term t
isymTerm str idx = symTerm $ IndexedSymbol str idx
{-# INLINE isymTerm #-}

notTerm :: Term Bool -> Term Bool
notTerm = internTerm . UNotTerm
{-# INLINE notTerm #-}

orTerm :: Term Bool -> Term Bool -> Term Bool
orTerm l r = internTerm $ UOrTerm l r
{-# INLINE orTerm #-}

andTerm :: Term Bool -> Term Bool -> Term Bool
andTerm l r = internTerm $ UAndTerm l r
{-# INLINE andTerm #-}

eqTerm :: (SupportedNonFuncPrim a) => Term a -> Term a -> Term Bool
eqTerm l r = internTerm $ UEqTerm l r
{-# INLINE eqTerm #-}

iteTerm :: (SupportedPrim a) => Term Bool -> Term a -> Term a -> Term a
iteTerm c l r = internTerm $ UITETerm c l r
{-# INLINE iteTerm #-}

addNumTerm :: (PEvalNumTerm a) => Term a -> Term a -> Term a
addNumTerm l r = internTerm $ UAddNumTerm l r
{-# INLINE addNumTerm #-}

negNumTerm :: (PEvalNumTerm a) => Term a -> Term a
negNumTerm = internTerm . UNegNumTerm
{-# INLINE negNumTerm #-}

mulNumTerm :: (PEvalNumTerm a) => Term a -> Term a -> Term a
mulNumTerm l r = internTerm $ UMulNumTerm l r
{-# INLINE mulNumTerm #-}

absNumTerm :: (PEvalNumTerm a) => Term a -> Term a
absNumTerm = internTerm . UAbsNumTerm
{-# INLINE absNumTerm #-}

signumNumTerm :: (PEvalNumTerm a) => Term a -> Term a
signumNumTerm = internTerm . USignumNumTerm
{-# INLINE signumNumTerm #-}

ltOrdTerm :: (PEvalOrdTerm a) => Term a -> Term a -> Term Bool
ltOrdTerm l r = internTerm $ ULtOrdTerm l r
{-# INLINE ltOrdTerm #-}

leOrdTerm :: (PEvalOrdTerm a) => Term a -> Term a -> Term Bool
leOrdTerm l r = internTerm $ ULeOrdTerm l r
{-# INLINE leOrdTerm #-}

andBitsTerm :: (PEvalBitwiseTerm a) => Term a -> Term a -> Term a
andBitsTerm l r = internTerm $ UAndBitsTerm l r
{-# INLINE andBitsTerm #-}

orBitsTerm :: (PEvalBitwiseTerm a) => Term a -> Term a -> Term a
orBitsTerm l r = internTerm $ UOrBitsTerm l r
{-# INLINE orBitsTerm #-}

xorBitsTerm :: (PEvalBitwiseTerm a) => Term a -> Term a -> Term a
xorBitsTerm l r = internTerm $ UXorBitsTerm l r
{-# INLINE xorBitsTerm #-}

complementBitsTerm :: (PEvalBitwiseTerm a) => Term a -> Term a
complementBitsTerm = internTerm . UComplementBitsTerm
{-# INLINE complementBitsTerm #-}

shiftLeftTerm :: (PEvalShiftTerm a) => Term a -> Term a -> Term a
shiftLeftTerm t n = internTerm $ UShiftLeftTerm t n
{-# INLINE shiftLeftTerm #-}

shiftRightTerm :: (PEvalShiftTerm a) => Term a -> Term a -> Term a
shiftRightTerm t n = internTerm $ UShiftRightTerm t n
{-# INLINE shiftRightTerm #-}

rotateLeftTerm :: (PEvalRotateTerm a) => Term a -> Term a -> Term a
rotateLeftTerm t n = internTerm $ URotateLeftTerm t n
{-# INLINE rotateLeftTerm #-}

rotateRightTerm :: (PEvalRotateTerm a) => Term a -> Term a -> Term a
rotateRightTerm t n = internTerm $ URotateRightTerm t n
{-# INLINE rotateRightTerm #-}

toSignedTerm ::
  (PEvalBVSignConversionTerm u s, KnownNat n, 1 <= n) =>
  Term (u n) ->
  Term (s n)
toSignedTerm = internTerm . UToSignedTerm

toUnsignedTerm ::
  (PEvalBVSignConversionTerm u s, KnownNat n, 1 <= n) =>
  Term (s n) ->
  Term (u n)
toUnsignedTerm = internTerm . UToUnsignedTerm

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
bvconcatTerm l r = internTerm $ UBVConcatTerm l r
{-# INLINE bvconcatTerm #-}

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
bvselectTerm _ _ v = internTerm $ UBVSelectTerm (typeRep @ix) (typeRep @w) v
{-# INLINE bvselectTerm #-}

bvextendTerm ::
  forall bv l r proxy.
  (PEvalBVTerm bv, KnownNat l, KnownNat r, 1 <= l, 1 <= r, l <= r) =>
  Bool ->
  proxy r ->
  Term (bv l) ->
  Term (bv r)
bvextendTerm signed _ v = internTerm $ UBVExtendTerm signed (typeRep @r) v
{-# INLINE bvextendTerm #-}

bvsignExtendTerm ::
  forall bv l r proxy.
  (PEvalBVTerm bv, KnownNat l, KnownNat r, 1 <= l, 1 <= r, l <= r) =>
  proxy r ->
  Term (bv l) ->
  Term (bv r)
bvsignExtendTerm _ v = internTerm $ UBVExtendTerm True (typeRep @r) v
{-# INLINE bvsignExtendTerm #-}

bvzeroExtendTerm ::
  forall bv l r proxy.
  (PEvalBVTerm bv, KnownNat l, KnownNat r, 1 <= l, 1 <= r, l <= r) =>
  proxy r ->
  Term (bv l) ->
  Term (bv r)
bvzeroExtendTerm _ v = internTerm $ UBVExtendTerm False (typeRep @r) v
{-# INLINE bvzeroExtendTerm #-}

applyTerm ::
  (SupportedPrim a, SupportedPrim b, SupportedPrim f, PEvalApplyTerm f a b) =>
  Term f ->
  Term a ->
  Term b
applyTerm f a = internTerm $ UApplyTerm f a
{-# INLINE applyTerm #-}

divIntegralTerm :: (PEvalDivModIntegralTerm a) => Term a -> Term a -> Term a
divIntegralTerm l r = internTerm $ UDivIntegralTerm l r
{-# INLINE divIntegralTerm #-}

modIntegralTerm :: (PEvalDivModIntegralTerm a) => Term a -> Term a -> Term a
modIntegralTerm l r = internTerm $ UModIntegralTerm l r
{-# INLINE modIntegralTerm #-}

quotIntegralTerm :: (PEvalDivModIntegralTerm a) => Term a -> Term a -> Term a
quotIntegralTerm l r = internTerm $ UQuotIntegralTerm l r
{-# INLINE quotIntegralTerm #-}

remIntegralTerm :: (PEvalDivModIntegralTerm a) => Term a -> Term a -> Term a
remIntegralTerm l r = internTerm $ URemIntegralTerm l r
{-# INLINE remIntegralTerm #-}

fpTraitTerm ::
  (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
  FPTrait ->
  Term (FP eb sb) ->
  Term Bool
fpTraitTerm trait v = internTerm $ UFPTraitTerm trait v

fdivTerm :: (PEvalFractionalTerm a) => Term a -> Term a -> Term a
fdivTerm l r = internTerm $ UFdivTerm l r
{-# INLINE fdivTerm #-}

recipTerm :: (PEvalFractionalTerm a) => Term a -> Term a
recipTerm = internTerm . URecipTerm
{-# INLINE recipTerm #-}

floatingUnaryTerm :: (PEvalFloatingTerm a) => FloatingUnaryOp -> Term a -> Term a
floatingUnaryTerm op = internTerm . UFloatingUnaryTerm op
{-# INLINE floatingUnaryTerm #-}

powerTerm :: (PEvalFloatingTerm a) => Term a -> Term a -> Term a
powerTerm l r = internTerm $ UPowerTerm l r
{-# INLINE powerTerm #-}

fpUnaryTerm ::
  (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
  FPUnaryOp ->
  Term (FP eb sb) ->
  Term (FP eb sb)
fpUnaryTerm op v = internTerm $ UFPUnaryTerm op v

fpBinaryTerm ::
  (ValidFP eb sb, SupportedPrim (FP eb sb)) =>
  FPBinaryOp ->
  Term (FP eb sb) ->
  Term (FP eb sb) ->
  Term (FP eb sb)
fpBinaryTerm op l r = internTerm $ UFPBinaryTerm op l r

fpRoundingUnaryTerm ::
  (ValidFP eb sb, SupportedPrim (FP eb sb), SupportedPrim FPRoundingMode) =>
  FPRoundingUnaryOp ->
  Term FPRoundingMode ->
  Term (FP eb sb) ->
  Term (FP eb sb)
fpRoundingUnaryTerm op mode v = internTerm $ UFPRoundingUnaryTerm op mode v

fpRoundingBinaryTerm ::
  (ValidFP eb sb, SupportedPrim (FP eb sb), SupportedPrim FPRoundingMode) =>
  FPRoundingBinaryOp ->
  Term FPRoundingMode ->
  Term (FP eb sb) ->
  Term (FP eb sb) ->
  Term (FP eb sb)
fpRoundingBinaryTerm op mode l r = internTerm $ UFPRoundingBinaryTerm op mode l r

fpFMATerm ::
  (ValidFP eb sb, SupportedPrim (FP eb sb), SupportedPrim FPRoundingMode) =>
  Term FPRoundingMode ->
  Term (FP eb sb) ->
  Term (FP eb sb) ->
  Term (FP eb sb) ->
  Term (FP eb sb)
fpFMATerm mode l r s = internTerm $ UFPFMATerm mode l r s

-- Support for boolean type
defaultValueForBool :: Bool
defaultValueForBool = False

defaultValueForBoolDyn :: ModelValue
defaultValueForBoolDyn = toModelValue defaultValueForBool

trueTerm :: Term Bool
trueTerm = conTerm True
{-# INLINE trueTerm #-}

falseTerm :: Term Bool
falseTerm = conTerm False
{-# INLINE falseTerm #-}

boolConTermView :: forall a. Term a -> Maybe Bool
boolConTermView (ConTerm _ b) = cast b
boolConTermView _ = Nothing
{-# INLINE boolConTermView #-}

pattern BoolConTerm :: Bool -> Term a
pattern BoolConTerm b <- (boolConTermView -> Just b)

pattern TrueTerm :: Term a
pattern TrueTerm <- BoolConTerm True

pattern FalseTerm :: Term a
pattern FalseTerm <- BoolConTerm False

boolTermView :: forall a. Term a -> Maybe (Term Bool)
boolTermView t = introSupportedPrimConstraint t $ cast t
{-# INLINE boolTermView #-}

pattern BoolTerm :: Term Bool -> Term a
pattern BoolTerm b <- (boolTermView -> Just b)

-- Not
pevalNotTerm :: Term Bool -> Term Bool
pevalNotTerm (NotTerm _ tm) = tm
pevalNotTerm (ConTerm _ a) = if a then falseTerm else trueTerm
pevalNotTerm (OrTerm _ (NotTerm _ n1) n2) = pevalAndTerm n1 (pevalNotTerm n2)
pevalNotTerm (OrTerm _ n1 (NotTerm _ n2)) = pevalAndTerm (pevalNotTerm n1) n2
pevalNotTerm (AndTerm _ (NotTerm _ n1) n2) = pevalOrTerm n1 (pevalNotTerm n2)
pevalNotTerm (AndTerm _ n1 (NotTerm _ n2)) = pevalOrTerm (pevalNotTerm n1) n2
pevalNotTerm tm = notTerm tm
{-# INLINEABLE pevalNotTerm #-}

orEqFirst :: Term Bool -> Term Bool -> Bool
orEqFirst _ (ConTerm _ False) = True
orEqFirst
  (NotTerm _ (EqTerm _ (e1 :: Term a) (ec1@(ConTerm _ _) :: Term b)))
  (EqTerm _ (Dyn (e2 :: Term a)) (Dyn (ec2@(ConTerm _ _) :: Term b)))
    | e1 == e2 && ec1 /= ec2 = True
orEqFirst x y
  | x == y = True
  | otherwise = False
{-# INLINE orEqFirst #-}

orEqTrue :: Term Bool -> Term Bool -> Bool
orEqTrue (ConTerm _ True) _ = True
orEqTrue _ (ConTerm _ True) = True
-- orEqTrue (NotTerm _ e1) (NotTerm _ e2) = andEqFalse e1 e2
orEqTrue
  (NotTerm _ (EqTerm _ (e1 :: Term a) (ec1@(ConTerm _ _) :: Term b)))
  (NotTerm _ (EqTerm _ (Dyn (e2 :: Term a)) (Dyn (ec2@(ConTerm _ _) :: Term b))))
    | e1 == e2 && ec1 /= ec2 = True
orEqTrue (NotTerm _ l) r | l == r = True
orEqTrue l (NotTerm _ r) | l == r = True
orEqTrue _ _ = False
{-# INLINE orEqTrue #-}

andEqFirst :: Term Bool -> Term Bool -> Bool
andEqFirst _ (ConTerm _ True) = True
-- andEqFirst x (NotTerm _ y) = andEqFalse x y
andEqFirst
  (EqTerm _ (e1 :: Term a) (ec1@(ConTerm _ _) :: Term b))
  (NotTerm _ (EqTerm _ (Dyn (e2 :: Term a)) (Dyn (ec2@(ConTerm _ _) :: Term b))))
    | e1 == e2 && ec1 /= ec2 = True
andEqFirst x y
  | x == y = True
  | otherwise = False
{-# INLINE andEqFirst #-}

andEqFalse :: Term Bool -> Term Bool -> Bool
andEqFalse (ConTerm _ False) _ = True
andEqFalse _ (ConTerm _ False) = True
-- andEqFalse (NotTerm _ e1) (NotTerm _ e2) = orEqTrue e1 e2
andEqFalse
  (EqTerm _ (e1 :: Term a) (ec1@(ConTerm _ _) :: Term b))
  (EqTerm _ (Dyn (e2 :: Term a)) (Dyn (ec2@(ConTerm _ _) :: Term b)))
    | e1 == e2 && ec1 /= ec2 = True
andEqFalse (NotTerm _ x) y | x == y = True
andEqFalse x (NotTerm _ y) | x == y = True
andEqFalse _ _ = False
{-# INLINE andEqFalse #-}

-- Or
pevalOrTerm :: Term Bool -> Term Bool -> Term Bool
pevalOrTerm l r
  | orEqTrue l r = trueTerm
  | orEqFirst l r = l
  | orEqFirst r l = r
pevalOrTerm l r@(OrTerm _ r1 r2)
  | orEqTrue l r1 = trueTerm
  | orEqTrue l r2 = trueTerm
  | orEqFirst r1 l = r
  | orEqFirst r2 l = r
  | orEqFirst l r1 = pevalOrTerm l r2
  | orEqFirst l r2 = pevalOrTerm l r1
pevalOrTerm l@(OrTerm _ l1 l2) r
  | orEqTrue l1 r = trueTerm
  | orEqTrue l2 r = trueTerm
  | orEqFirst l1 r = l
  | orEqFirst l2 r = l
  | orEqFirst r l1 = pevalOrTerm l2 r
  | orEqFirst r l2 = pevalOrTerm l1 r
pevalOrTerm l (AndTerm _ r1 r2)
  | orEqFirst l r1 = l
  | orEqFirst l r2 = l
  | orEqTrue l r1 = pevalOrTerm l r2
  | orEqTrue l r2 = pevalOrTerm l r1
pevalOrTerm (AndTerm _ l1 l2) r
  | orEqFirst r l1 = r
  | orEqFirst r l2 = r
  | orEqTrue l1 r = pevalOrTerm l2 r
  | orEqTrue l2 r = pevalOrTerm l1 r
pevalOrTerm
  (AndTerm _ nl1@(NotTerm _ l1) l2)
  (EqTerm _ (Dyn (e1 :: Term Bool)) (Dyn (e2 :: Term Bool)))
    | l1 == e1 && l2 == e2 = pevalOrTerm nl1 l2
pevalOrTerm (NotTerm _ nl) (NotTerm _ nr) = pevalNotTerm $ pevalAndTerm nl nr
pevalOrTerm l r = orTerm l r
{-# INLINEABLE pevalOrTerm #-}

pevalAndTerm :: Term Bool -> Term Bool -> Term Bool
pevalAndTerm l r
  | andEqFalse l r = falseTerm
  | andEqFirst l r = l
  | andEqFirst r l = r
pevalAndTerm l r@(AndTerm _ r1 r2)
  | andEqFalse l r1 = falseTerm
  | andEqFalse l r2 = falseTerm
  | andEqFirst r1 l = r
  | andEqFirst r2 l = r
  | andEqFirst l r1 = pevalAndTerm l r2
  | andEqFirst l r2 = pevalAndTerm l r1
pevalAndTerm l@(AndTerm _ l1 l2) r
  | andEqFalse l1 r = falseTerm
  | andEqFalse l2 r = falseTerm
  | andEqFirst l1 r = l
  | andEqFirst l2 r = l
  | andEqFirst r l1 = pevalAndTerm l2 r
  | andEqFirst r l2 = pevalAndTerm l1 r
pevalAndTerm l (OrTerm _ r1 r2)
  | andEqFirst l r1 = l
  | andEqFirst l r2 = l
  | andEqFalse l r1 = pevalAndTerm l r2
  | andEqFalse l r2 = pevalAndTerm l r1
pevalAndTerm (OrTerm _ l1 l2) r
  | andEqFirst r l1 = r
  | andEqFirst r l2 = r
  | andEqFalse l1 r = pevalAndTerm l2 r
  | andEqFalse l2 r = pevalAndTerm l1 r
pevalAndTerm
  (OrTerm _ l1 nl2@(NotTerm _ l2))
  (NotTerm _ (EqTerm _ (Dyn (e1 :: Term Bool)) (Dyn (e2 :: Term Bool))))
    | l1 == e1 && l2 == e2 = pevalAndTerm l1 nl2
pevalAndTerm (NotTerm _ nl) (NotTerm _ nr) = pevalNotTerm $ pevalOrTerm nl nr
pevalAndTerm l r = andTerm l r
{-# INLINEABLE pevalAndTerm #-}

pevalImplyTerm :: Term Bool -> Term Bool -> Term Bool
pevalImplyTerm l = pevalOrTerm (pevalNotTerm l)

pevalXorTerm :: Term Bool -> Term Bool -> Term Bool
pevalXorTerm l r = pevalOrTerm (pevalAndTerm (pevalNotTerm l) r) (pevalAndTerm l (pevalNotTerm r))

pevalImpliesTerm :: Term Bool -> Term Bool -> Bool
pevalImpliesTerm (ConTerm _ False) _ = True
pevalImpliesTerm _ (ConTerm _ True) = True
pevalImpliesTerm
  (EqTerm _ (e1 :: Term a) (ec1@(ConTerm _ _) :: Term b))
  (NotTerm _ (EqTerm _ (Dyn (e2 :: Term a)) (Dyn (ec2@(ConTerm _ _) :: Term b))))
    | e1 == e2 && ec1 /= ec2 = True
pevalImpliesTerm a b
  | a == b = True
  | otherwise = False
{-# INLINE pevalImpliesTerm #-}

pevalITEBoolLeftNot :: Term Bool -> Term Bool -> Term Bool -> Maybe (Term Bool)
pevalITEBoolLeftNot cond nIfTrue ifFalse
  -- need test
  | cond == nIfTrue = Just $ pevalAndTerm (pevalNotTerm cond) ifFalse
  | otherwise = case nIfTrue of
      AndTerm _ nt1 nt2 -> ra
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
      OrTerm _ nt1 nt2 -> ra
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
pevalInferImplies cond (NotTerm _ nt1) trueRes falseRes
  | cond == nt1 = Just falseRes
  | otherwise = case (cond, nt1) of
      ( EqTerm _ (e1 :: Term a) (ec1@(ConTerm _ _) :: Term b),
        EqTerm _ (Dyn (e2 :: Term a)) (Dyn (ec2@(ConTerm _ _) :: Term b))
        )
          | e1 == e2 && ec1 /= ec2 -> Just trueRes
      _ -> Nothing
pevalInferImplies
  (EqTerm _ (e1 :: Term a) (ec1@(ConTerm _ _) :: Term b))
  (EqTerm _ (Dyn (e2 :: Term a)) (Dyn (ec2@(ConTerm _ _) :: Term b)))
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
pevalITEBoolLeft cond (AndTerm _ t1 t2) ifFalse =
  msum
    [ pevalITEBoolLeftAnd cond t1 t2 ifFalse,
      case ifFalse of
        AndTerm _ f1 f2 -> pevalITEBoolBothAnd cond t1 t2 f1 f2
        _ -> Nothing
    ]
pevalITEBoolLeft cond (OrTerm _ t1 t2) ifFalse =
  msum
    [ pevalITEBoolLeftOr cond t1 t2 ifFalse,
      case ifFalse of
        OrTerm _ f1 f2 -> pevalITEBoolBothOr cond t1 t2 f1 f2
        _ -> Nothing
    ]
pevalITEBoolLeft cond (NotTerm _ nIfTrue) ifFalse =
  msum
    [ pevalITEBoolLeftNot cond nIfTrue ifFalse,
      case ifFalse of
        NotTerm _ nIfFalse ->
          pevalITEBoolBothNot cond nIfTrue nIfFalse
        _ -> Nothing
    ]
pevalITEBoolLeft _ _ _ = Nothing

pevalITEBoolNoLeft :: Term Bool -> Term Bool -> Term Bool -> Maybe (Term Bool)
pevalITEBoolNoLeft cond ifTrue (AndTerm _ f1 f2) = pevalITEBoolRightAnd cond ifTrue f1 f2
pevalITEBoolNoLeft cond ifTrue (OrTerm _ f1 f2) = pevalITEBoolRightOr cond ifTrue f1 f2
pevalITEBoolNoLeft cond ifTrue (NotTerm _ nIfFalse) = pevalITEBoolRightNot cond ifTrue nIfFalse
pevalITEBoolNoLeft _ _ _ = Nothing

pevalITEBasic :: (SupportedPrim a) => Term Bool -> Term a -> Term a -> Maybe (Term a)
pevalITEBasic (ConTerm _ True) ifTrue _ = Just ifTrue
pevalITEBasic (ConTerm _ False) _ ifFalse = Just ifFalse
pevalITEBasic (NotTerm _ ncond) ifTrue ifFalse = Just $ pevalITETerm ncond ifFalse ifTrue
pevalITEBasic _ ifTrue ifFalse | ifTrue == ifFalse = Just ifTrue
pevalITEBasic (ITETerm _ cc ct cf) (ITETerm _ tc tt tf) (ITETerm _ fc ft ff) -- later
  | cc == tc && cc == fc = Just $ pevalITETerm cc (pevalITETerm ct tt ft) (pevalITETerm cf tf ff)
pevalITEBasic cond (ITETerm _ tc tt tf) ifFalse -- later
  | cond == tc = Just $ pevalITETerm cond tt ifFalse
  | tt == ifFalse = Just $ pevalITETerm (pevalOrTerm (pevalNotTerm cond) tc) tt tf
  | tf == ifFalse = Just $ pevalITETerm (pevalAndTerm cond tc) tt tf
pevalITEBasic cond ifTrue (ITETerm _ fc ft ff) -- later
  | ifTrue == ft = Just $ pevalITETerm (pevalOrTerm cond fc) ifTrue ff
  | ifTrue == ff = Just $ pevalITETerm (pevalOrTerm cond (pevalNotTerm fc)) ifTrue ft
  | pevalImpliesTerm fc cond = Just $ pevalITETerm cond ifTrue ff
pevalITEBasic _ _ _ = Nothing

pevalITEBoolBasic :: Term Bool -> Term Bool -> Term Bool -> Maybe (Term Bool)
pevalITEBoolBasic cond ifTrue ifFalse
  | cond == ifTrue = Just $ pevalOrTerm cond ifFalse
  | cond == ifFalse = Just $ pevalAndTerm cond ifTrue
pevalITEBoolBasic cond (ConTerm _ v) ifFalse
  | v = Just $ pevalOrTerm cond ifFalse
  | otherwise = Just $ pevalAndTerm (pevalNotTerm cond) ifFalse
pevalITEBoolBasic cond ifTrue (ConTerm _ v)
  | v = Just $ pevalOrTerm (pevalNotTerm cond) ifTrue
  | otherwise = Just $ pevalAndTerm cond ifTrue
pevalITEBoolBasic _ _ _ = Nothing

pevalITEBool :: Term Bool -> Term Bool -> Term Bool -> Maybe (Term Bool)
pevalITEBool cond ifTrue ifFalse =
  msum
    [ pevalITEBasic cond ifTrue ifFalse,
      pevalITEBoolBasic cond ifTrue ifFalse,
      pevalITEBoolLeft cond ifTrue ifFalse,
      pevalITEBoolNoLeft cond ifTrue ifFalse
    ]

pevalITEBasicTerm :: (SupportedPrim a) => Term Bool -> Term a -> Term a -> Term a
pevalITEBasicTerm cond ifTrue ifFalse =
  fromMaybe (iteTerm cond ifTrue ifFalse) $
    pevalITEBasic cond ifTrue ifFalse

pevalDefaultEqTerm :: (SupportedNonFuncPrim a) => Term a -> Term a -> Term Bool
pevalDefaultEqTerm l@ConTerm {} r@ConTerm {} = conTerm $ l == r
pevalDefaultEqTerm l@ConTerm {} r = pevalDefaultEqTerm r l
pevalDefaultEqTerm l (BoolConTerm rv) =
  if rv
    then unsafeCoerce l
    else pevalNotTerm (unsafeCoerce l)
pevalDefaultEqTerm (NotTerm _ lv) r
  | lv == r = falseTerm
pevalDefaultEqTerm l (NotTerm _ rv)
  | l == rv = falseTerm
pevalDefaultEqTerm (AddNumTerm _ (ConTerm _ c) v) (ConTerm _ c2) =
  pevalDefaultEqTerm v (conTerm $ c2 - c)
pevalDefaultEqTerm l (ITETerm _ c t f)
  | l == t = pevalOrTerm c (pevalDefaultEqTerm l f)
  | l == f = pevalOrTerm (pevalNotTerm c) (pevalDefaultEqTerm l t)
pevalDefaultEqTerm (ITETerm _ c t f) r
  | t == r = pevalOrTerm c (pevalDefaultEqTerm f r)
  | f == r = pevalOrTerm (pevalNotTerm c) (pevalDefaultEqTerm t r)
pevalDefaultEqTerm l r
  | l == r = trueTerm
  | otherwise = eqTerm l r
{-# INLINEABLE pevalDefaultEqTerm #-}

instance SBVRep Bool where
  type SBVType _ Bool = SBV.SBV Bool

instance SupportedPrimConstraint Bool

instance SupportedPrim Bool where
  pformatCon True = "true"
  pformatCon False = "false"
  defaultValue = defaultValueForBool
  defaultValueDynamic _ = defaultValueForBoolDyn
  pevalITETerm cond ifTrue ifFalse =
    fromMaybe (iteTerm cond ifTrue ifFalse) $
      pevalITEBool cond ifTrue ifFalse
  pevalEqTerm = pevalDefaultEqTerm
  conSBVTerm _ n = if n then SBV.sTrue else SBV.sFalse
  symSBVName symbol _ = show symbol
  symSBVTerm _ = sbvFresh
  withPrim _ r = r
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
  funcDummyConstraint _ _ = SBV.sTrue

instance NonFuncSBVRep Bool where
  type NonFuncSBVBaseType _ Bool = Bool

instance SupportedNonFuncPrim Bool where
  conNonFuncSBVTerm = conSBVTerm
  symNonFuncSBVTerm = symSBVTerm @Bool
  withNonFuncPrim _ r = r
