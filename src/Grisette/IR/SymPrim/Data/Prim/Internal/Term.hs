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
-- Module      :   Grisette.IR.SymPrim.Data.Prim.Internal.Term
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.Prim.Internal.Term
  ( -- * Supported primitive types
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

    -- * Typed symbols
    TypedSymbol (..),
    SomeTypedSymbol (..),
    showUntyped,
    withSymbolSupported,
    someTypedSymbol,

    -- * Terms
    Term (..),
    identity,
    identityWithTypeRep,
    introSupportedPrimConstraint,
    pformat,

    -- * Interning
    UTerm (..),
    prettyPrintTerm,
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
    eqvTerm,
    iteTerm,
    addNumTerm,
    uminusNumTerm,
    timesNumTerm,
    absNumTerm,
    signumNumTerm,
    ltNumTerm,
    leNumTerm,
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
    divBoundedIntegralTerm,
    modBoundedIntegralTerm,
    quotBoundedIntegralTerm,
    remBoundedIntegralTerm,

    -- * Support for boolean type
    trueTerm,
    falseTerm,
    pattern BoolConTerm,
    pattern TrueTerm,
    pattern FalseTerm,
    pattern BoolTerm,
    pevalNotTerm,
    pevalEqvTerm,
    pevalNotEqvTerm,
    pevalOrTerm,
    pevalAndTerm,
    pevalImplyTerm,
    pevalXorTerm,
    pevalITEBasic,
    pevalITEBasicTerm,
  )
where

import Control.DeepSeq (NFData (rnf))
import Data.Array ((!))
import Data.Bits (FiniteBits)
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
import Data.Kind (Constraint)
import Data.String (IsString (fromString))
import Data.Typeable (Proxy (Proxy), cast)
import GHC.IO (unsafeDupablePerformIO)
import GHC.TypeNats (KnownNat, Nat, type (+), type (<=))
import Grisette.Core.Data.Class.BitVector
  ( SizedBV,
  )
import Grisette.Core.Data.Class.SignConversion (SignConversion)
import Grisette.Core.Data.Class.SymRotate (SymRotate)
import Grisette.Core.Data.Class.SymShift (SymShift)
import Grisette.Core.Data.Symbol
  ( Identifier,
    Symbol (IndexedSymbol, SimpleSymbol),
  )
import Grisette.IR.SymPrim.Data.Prim.Internal.Caches
  ( typeMemoizedCache,
  )
import Grisette.IR.SymPrim.Data.Prim.ModelValue
  ( ModelValue,
    toModelValue,
  )
import Grisette.IR.SymPrim.Data.Prim.Utils
  ( eqHeteroRep,
    eqTypeRepBool,
    pattern Dyn,
  )
import Language.Haskell.TH.Syntax (Lift (lift, liftTyped))
import Language.Haskell.TH.Syntax.Compat (unTypeSplice)
import Type.Reflection
  ( SomeTypeRep (SomeTypeRep),
    TypeRep,
    Typeable,
    eqTypeRep,
    someTypeRep,
    typeRep,
    type (:~~:) (HRefl),
  )

#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter
  ( column,
    pageWidth,
    Doc,
    PageWidth(Unbounded, AvailablePerLine),
    Pretty(pretty),
  )
import Data.Maybe (fromMaybe)
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad (msum)
#else
import Data.Text.Prettyprint.Doc
  ( column,
    pageWidth,
    Doc,
    PageWidth(Unbounded, AvailablePerLine),
    Pretty(pretty),
  )
#endif

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim

-- | Indicates that a type is supported and can be represented as a symbolic
-- term.
class (Lift t, Typeable t, Hashable t, Eq t, Show t, NFData t) => SupportedPrim t where
  type PrimConstraint t :: Constraint
  type PrimConstraint _ = ()
  default withPrim :: (PrimConstraint t) => proxy t -> ((PrimConstraint t) => a) -> a
  withPrim :: proxy t -> ((PrimConstraint t) => a) -> a
  withPrim _ i = i
  termCache :: Cache (Term t)
  termCache = typeMemoizedCache
  pformatCon :: t -> String
  default pformatCon :: (Show t) => t -> String
  pformatCon = show
  pformatSym :: TypedSymbol t -> String
  pformatSym = showUntyped
  defaultValue :: t
  defaultValueDynamic :: proxy t -> ModelValue
  defaultValueDynamic _ = toModelValue (defaultValue @t)
  pevalITETerm :: Term Bool -> Term t -> Term t -> Term t

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

class (SupportedPrim t) => PEvalBitwiseTerm t where
  pevalAndBitsTerm :: Term t -> Term t -> Term t
  pevalOrBitsTerm :: Term t -> Term t -> Term t
  pevalXorBitsTerm :: Term t -> Term t -> Term t
  pevalComplementBitsTerm :: Term t -> Term t

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

-- | A typed symbol is a symbol that is associated with a type. Note that the
-- same symbol bodies with different types are considered different symbols
-- and can coexist in a term.
--
-- Simple symbols can be created with the 'OverloadedStrings' extension:
--
-- >>> :set -XOverloadedStrings
-- >>> "a" :: TypedSymbol Bool
-- a :: Bool
data TypedSymbol t where
  TypedSymbol :: (SupportedPrim t) => {unTypedSymbol :: Symbol} -> TypedSymbol t

instance Eq (TypedSymbol t) where
  TypedSymbol x == TypedSymbol y = x == y

instance Ord (TypedSymbol t) where
  TypedSymbol x <= TypedSymbol y = x <= y

instance Lift (TypedSymbol t) where
  liftTyped (TypedSymbol x) = [||TypedSymbol x||]

instance Show (TypedSymbol t) where
  show (TypedSymbol symbol) = show symbol ++ " :: " ++ show (typeRep @t)

showUntyped :: TypedSymbol t -> String
showUntyped (TypedSymbol symbol) = show symbol

instance Hashable (TypedSymbol t) where
  s `hashWithSalt` TypedSymbol x = s `hashWithSalt` x

instance NFData (TypedSymbol t) where
  rnf (TypedSymbol str) = rnf str

instance (SupportedPrim t) => IsString (TypedSymbol t) where
  fromString = TypedSymbol . fromString

withSymbolSupported :: TypedSymbol t -> ((SupportedPrim t) => a) -> a
withSymbolSupported (TypedSymbol _) a = a

data SomeTypedSymbol where
  SomeTypedSymbol :: forall t. TypeRep t -> TypedSymbol t -> SomeTypedSymbol

instance NFData SomeTypedSymbol where
  rnf (SomeTypedSymbol p s) = rnf (SomeTypeRep p) `seq` rnf s

instance Eq SomeTypedSymbol where
  (SomeTypedSymbol t1 s1) == (SomeTypedSymbol t2 s2) = case eqTypeRep t1 t2 of
    Just HRefl -> s1 == s2
    _ -> False

instance Ord SomeTypedSymbol where
  (SomeTypedSymbol t1 s1) <= (SomeTypedSymbol t2 s2) =
    SomeTypeRep t1 < SomeTypeRep t2
      || ( case eqTypeRep t1 t2 of
             Just HRefl -> s1 <= s2
             _ -> False
         )

instance Hashable SomeTypedSymbol where
  hashWithSalt s (SomeTypedSymbol t1 s1) = s `hashWithSalt` s1 `hashWithSalt` t1

instance Show SomeTypedSymbol where
  show (SomeTypedSymbol _ s) = show s

someTypedSymbol :: forall t. TypedSymbol t -> SomeTypedSymbol
someTypedSymbol s@(TypedSymbol _) = SomeTypedSymbol (typeRep @t) s

-- Terms

data Term t where
  ConTerm :: (SupportedPrim t) => {-# UNPACK #-} !Id -> !t -> Term t
  SymTerm :: (SupportedPrim t) => {-# UNPACK #-} !Id -> !(TypedSymbol t) -> Term t
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
  EqvTerm :: (SupportedPrim t) => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term Bool
  ITETerm ::
    (SupportedPrim t) =>
    {-# UNPACK #-} !Id ->
    !(Term Bool) ->
    !(Term t) ->
    !(Term t) ->
    Term t
  AddNumTerm :: (SupportedPrim t, Num t) => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term t
  UMinusNumTerm :: (SupportedPrim t, Num t) => {-# UNPACK #-} !Id -> !(Term t) -> Term t
  TimesNumTerm :: (SupportedPrim t, Num t) => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term t
  AbsNumTerm :: (SupportedPrim t, Num t) => {-# UNPACK #-} !Id -> !(Term t) -> Term t
  SignumNumTerm :: (SupportedPrim t, Num t) => {-# UNPACK #-} !Id -> !(Term t) -> Term t
  LTNumTerm :: (SupportedPrim t, Num t, Ord t) => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term Bool
  LENumTerm :: (SupportedPrim t, Num t, Ord t) => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term Bool
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
  ShiftLeftTerm :: (SupportedPrim t, Integral t, FiniteBits t, SymShift t) => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term t
  ShiftRightTerm :: (SupportedPrim t, Integral t, FiniteBits t, SymShift t) => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term t
  RotateLeftTerm :: (SupportedPrim t, Integral t, FiniteBits t, SymRotate t) => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term t
  RotateRightTerm :: (SupportedPrim t, Integral t, FiniteBits t, SymRotate t) => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term t
  ToSignedTerm ::
    ( forall n. (KnownNat n, 1 <= n) => SupportedPrim (u n),
      forall n. (KnownNat n, 1 <= n) => SupportedPrim (s n),
      forall n. (KnownNat n, 1 <= n) => SignConversion (u n) (s n),
      SignConversion (u 1) (s 1),
      Typeable u,
      Typeable s,
      KnownNat n,
      1 <= n,
      SizedBV u,
      SizedBV s
    ) =>
    {-# UNPACK #-} !Id ->
    !(Term (u n)) ->
    Term (s n)
  ToUnsignedTerm ::
    ( forall n. (KnownNat n, 1 <= n) => SupportedPrim (u n),
      forall n. (KnownNat n, 1 <= n) => SupportedPrim (s n),
      forall n. (KnownNat n, 1 <= n) => SignConversion (u n) (s n),
      SignConversion (u 1) (s 1),
      Typeable u,
      Typeable s,
      KnownNat n,
      1 <= n,
      SizedBV u,
      SizedBV s
    ) =>
    {-# UNPACK #-} !Id ->
    !(Term (s n)) ->
    Term (u n)
  BVConcatTerm ::
    ( forall n. (KnownNat n, 1 <= n) => SupportedPrim (bv n),
      Typeable bv,
      KnownNat a,
      KnownNat b,
      KnownNat (a + b),
      1 <= a,
      1 <= b,
      1 <= a + b,
      SizedBV bv
    ) =>
    {-# UNPACK #-} !Id ->
    !(Term (bv a)) ->
    !(Term (bv b)) ->
    Term (bv (a + b))
  BVSelectTerm ::
    ( forall n. (KnownNat n, 1 <= n) => SupportedPrim (bv n),
      Typeable bv,
      KnownNat n,
      KnownNat ix,
      KnownNat w,
      1 <= n,
      1 <= w,
      ix + w <= n,
      SizedBV bv
    ) =>
    {-# UNPACK #-} !Id ->
    !(TypeRep ix) ->
    !(TypeRep w) ->
    !(Term (bv n)) ->
    Term (bv w)
  BVExtendTerm ::
    ( forall n. (KnownNat n, 1 <= n) => SupportedPrim (bv n),
      Typeable bv,
      KnownNat l,
      KnownNat r,
      1 <= l,
      1 <= r,
      l <= r,
      SizedBV bv
    ) =>
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
  DivIntegralTerm :: (SupportedPrim t, Integral t) => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term t
  ModIntegralTerm :: (SupportedPrim t, Integral t) => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term t
  QuotIntegralTerm :: (SupportedPrim t, Integral t) => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term t
  RemIntegralTerm :: (SupportedPrim t, Integral t) => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term t
  DivBoundedIntegralTerm :: (SupportedPrim t, Bounded t, Integral t) => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term t
  ModBoundedIntegralTerm :: (SupportedPrim t, Bounded t, Integral t) => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term t
  QuotBoundedIntegralTerm :: (SupportedPrim t, Bounded t, Integral t) => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term t
  RemBoundedIntegralTerm :: (SupportedPrim t, Bounded t, Integral t) => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term t

identity :: Term t -> Id
identity = snd . identityWithTypeRep
{-# INLINE identity #-}

identityWithTypeRep :: forall t. Term t -> (SomeTypeRep, Id)
identityWithTypeRep (ConTerm i _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (SymTerm i _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (UnaryTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (BinaryTerm i _ _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (TernaryTerm i _ _ _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (NotTerm i _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (OrTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (AndTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (EqvTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (ITETerm i _ _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (AddNumTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (UMinusNumTerm i _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (TimesNumTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (AbsNumTerm i _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (SignumNumTerm i _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (LTNumTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (LENumTerm i _ _) = (someTypeRep (Proxy @t), i)
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
identityWithTypeRep (DivBoundedIntegralTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (ModBoundedIntegralTerm i _ _) = (someTypeRep (Proxy @t), i)
identityWithTypeRep (QuotBoundedIntegralTerm i _ _) =
  (someTypeRep (Proxy @t), i)
identityWithTypeRep (RemBoundedIntegralTerm i _ _) = (someTypeRep (Proxy @t), i)
{-# INLINE identityWithTypeRep #-}

introSupportedPrimConstraint :: forall t a. Term t -> ((SupportedPrim t) => a) -> a
introSupportedPrimConstraint ConTerm {} x = x
introSupportedPrimConstraint SymTerm {} x = x
introSupportedPrimConstraint UnaryTerm {} x = x
introSupportedPrimConstraint BinaryTerm {} x = x
introSupportedPrimConstraint TernaryTerm {} x = x
introSupportedPrimConstraint NotTerm {} x = x
introSupportedPrimConstraint OrTerm {} x = x
introSupportedPrimConstraint AndTerm {} x = x
introSupportedPrimConstraint EqvTerm {} x = x
introSupportedPrimConstraint ITETerm {} x = x
introSupportedPrimConstraint AddNumTerm {} x = x
introSupportedPrimConstraint UMinusNumTerm {} x = x
introSupportedPrimConstraint TimesNumTerm {} x = x
introSupportedPrimConstraint AbsNumTerm {} x = x
introSupportedPrimConstraint SignumNumTerm {} x = x
introSupportedPrimConstraint LTNumTerm {} x = x
introSupportedPrimConstraint LENumTerm {} x = x
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
introSupportedPrimConstraint DivBoundedIntegralTerm {} x = x
introSupportedPrimConstraint ModBoundedIntegralTerm {} x = x
introSupportedPrimConstraint QuotBoundedIntegralTerm {} x = x
introSupportedPrimConstraint RemBoundedIntegralTerm {} x = x
{-# INLINE introSupportedPrimConstraint #-}

pformat :: forall t. (SupportedPrim t) => Term t -> String
pformat (ConTerm _ t) = pformatCon t
pformat (SymTerm _ sym) = pformatSym sym
pformat (UnaryTerm _ tag arg1) = pformatUnary tag arg1
pformat (BinaryTerm _ tag arg1 arg2) = pformatBinary tag arg1 arg2
pformat (TernaryTerm _ tag arg1 arg2 arg3) = pformatTernary tag arg1 arg2 arg3
pformat (NotTerm _ arg) = "(! " ++ pformat arg ++ ")"
pformat (OrTerm _ arg1 arg2) = "(|| " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (AndTerm _ arg1 arg2) = "(&& " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (EqvTerm _ arg1 arg2) = "(= " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (ITETerm _ cond arg1 arg2) = "(ite " ++ pformat cond ++ " " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (AddNumTerm _ arg1 arg2) = "(+ " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (UMinusNumTerm _ arg) = "(- " ++ pformat arg ++ ")"
pformat (TimesNumTerm _ arg1 arg2) = "(* " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (AbsNumTerm _ arg) = "(abs " ++ pformat arg ++ ")"
pformat (SignumNumTerm _ arg) = "(signum " ++ pformat arg ++ ")"
pformat (LTNumTerm _ arg1 arg2) = "(< " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (LENumTerm _ arg1 arg2) = "(<= " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
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
pformat (DivBoundedIntegralTerm _ arg1 arg2) = "(div " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (ModBoundedIntegralTerm _ arg1 arg2) = "(mod " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (QuotBoundedIntegralTerm _ arg1 arg2) = "(quot " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
pformat (RemBoundedIntegralTerm _ arg1 arg2) = "(rem " ++ pformat arg1 ++ " " ++ pformat arg2 ++ ")"
{-# INLINE pformat #-}

instance NFData (Term a) where
  rnf i = identity i `seq` ()

instance Lift (Term t) where
  lift = unTypeSplice . liftTyped
  liftTyped (ConTerm _ i) = [||conTerm i||]
  liftTyped (SymTerm _ sym) = [||symTerm (unTypedSymbol sym)||]
  liftTyped (UnaryTerm _ tag arg) = [||constructUnary tag arg||]
  liftTyped (BinaryTerm _ tag arg1 arg2) = [||constructBinary tag arg1 arg2||]
  liftTyped (TernaryTerm _ tag arg1 arg2 arg3) = [||constructTernary tag arg1 arg2 arg3||]
  liftTyped (NotTerm _ arg) = [||notTerm arg||]
  liftTyped (OrTerm _ arg1 arg2) = [||orTerm arg1 arg2||]
  liftTyped (AndTerm _ arg1 arg2) = [||andTerm arg1 arg2||]
  liftTyped (EqvTerm _ arg1 arg2) = [||eqvTerm arg1 arg2||]
  liftTyped (ITETerm _ cond arg1 arg2) = [||iteTerm cond arg1 arg2||]
  liftTyped (AddNumTerm _ arg1 arg2) = [||addNumTerm arg1 arg2||]
  liftTyped (UMinusNumTerm _ arg) = [||uminusNumTerm arg||]
  liftTyped (TimesNumTerm _ arg1 arg2) = [||timesNumTerm arg1 arg2||]
  liftTyped (AbsNumTerm _ arg) = [||absNumTerm arg||]
  liftTyped (SignumNumTerm _ arg) = [||signumNumTerm arg||]
  liftTyped (LTNumTerm _ arg1 arg2) = [||ltNumTerm arg1 arg2||]
  liftTyped (LENumTerm _ arg1 arg2) = [||leNumTerm arg1 arg2||]
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
  liftTyped (DivBoundedIntegralTerm _ arg1 arg2) = [||divBoundedIntegralTerm arg1 arg2||]
  liftTyped (ModBoundedIntegralTerm _ arg1 arg2) = [||modBoundedIntegralTerm arg1 arg2||]
  liftTyped (QuotBoundedIntegralTerm _ arg1 arg2) = [||quotBoundedIntegralTerm arg1 arg2||]
  liftTyped (RemBoundedIntegralTerm _ arg1 arg2) = [||remBoundedIntegralTerm arg1 arg2||]

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
  show (EqvTerm i arg1 arg2) = "Eqv{id=" ++ show i ++ ", arg1=" ++ show arg1 ++ ", arg2=" ++ show arg2 ++ "}"
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
  show (UMinusNumTerm i arg) = "UMinusNum{id=" ++ show i ++ ", arg=" ++ show arg ++ "}"
  show (TimesNumTerm i arg1 arg2) = "TimesNum{id=" ++ show i ++ ", arg1=" ++ show arg1 ++ ", arg2=" ++ show arg2 ++ "}"
  show (AbsNumTerm i arg) = "AbsNum{id=" ++ show i ++ ", arg=" ++ show arg ++ "}"
  show (SignumNumTerm i arg) = "SignumNum{id=" ++ show i ++ ", arg=" ++ show arg ++ "}"
  show (LTNumTerm i arg1 arg2) = "LTNum{id=" ++ show i ++ ", arg1=" ++ show arg1 ++ ", arg2=" ++ show arg2 ++ "}"
  show (LENumTerm i arg1 arg2) = "LENum{id=" ++ show i ++ ", arg1=" ++ show arg1 ++ ", arg2=" ++ show arg2 ++ "}"
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
  show (DivBoundedIntegralTerm i arg1 arg2) =
    "DivBoundedIntegral{id=" ++ show i ++ ", arg1=" ++ show arg1 ++ ", arg2=" ++ show arg2 ++ "}"
  show (ModBoundedIntegralTerm i arg1 arg2) =
    "ModBoundedIntegral{id=" ++ show i ++ ", arg1=" ++ show arg1 ++ ", arg2=" ++ show arg2 ++ "}"
  show (QuotBoundedIntegralTerm i arg1 arg2) =
    "QuotBoundedIntegral{id=" ++ show i ++ ", arg1=" ++ show arg1 ++ ", arg2=" ++ show arg2 ++ "}"
  show (RemBoundedIntegralTerm i arg1 arg2) =
    "RemBoundedIntegral{id=" ++ show i ++ ", arg1=" ++ show arg1 ++ ", arg2=" ++ show arg2 ++ "}"

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
  USymTerm :: (SupportedPrim t) => !(TypedSymbol t) -> UTerm t
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
  UEqvTerm :: (SupportedPrim t) => !(Term t) -> !(Term t) -> UTerm Bool
  UITETerm ::
    (SupportedPrim t) =>
    !(Term Bool) ->
    !(Term t) ->
    !(Term t) ->
    UTerm t
  UAddNumTerm :: (SupportedPrim t, Num t) => !(Term t) -> !(Term t) -> UTerm t
  UUMinusNumTerm :: (SupportedPrim t, Num t) => !(Term t) -> UTerm t
  UTimesNumTerm :: (SupportedPrim t, Num t) => !(Term t) -> !(Term t) -> UTerm t
  UAbsNumTerm :: (SupportedPrim t, Num t) => !(Term t) -> UTerm t
  USignumNumTerm :: (SupportedPrim t, Num t) => !(Term t) -> UTerm t
  ULTNumTerm :: (SupportedPrim t, Num t, Ord t) => !(Term t) -> !(Term t) -> UTerm Bool
  ULENumTerm :: (SupportedPrim t, Num t, Ord t) => !(Term t) -> !(Term t) -> UTerm Bool
  UAndBitsTerm :: (PEvalBitwiseTerm t) => !(Term t) -> !(Term t) -> UTerm t
  UOrBitsTerm :: (PEvalBitwiseTerm t) => !(Term t) -> !(Term t) -> UTerm t
  UXorBitsTerm :: (PEvalBitwiseTerm t) => !(Term t) -> !(Term t) -> UTerm t
  UComplementBitsTerm :: (PEvalBitwiseTerm t) => !(Term t) -> UTerm t
  UShiftLeftTerm :: (SupportedPrim t, Integral t, FiniteBits t, SymShift t) => !(Term t) -> !(Term t) -> UTerm t
  UShiftRightTerm :: (SupportedPrim t, Integral t, FiniteBits t, SymShift t) => !(Term t) -> !(Term t) -> UTerm t
  URotateLeftTerm :: (SupportedPrim t, Integral t, FiniteBits t, SymRotate t) => !(Term t) -> !(Term t) -> UTerm t
  URotateRightTerm :: (SupportedPrim t, Integral t, FiniteBits t, SymRotate t) => !(Term t) -> !(Term t) -> UTerm t
  UToSignedTerm ::
    ( forall n. (KnownNat n, 1 <= n) => SupportedPrim (u n),
      forall n. (KnownNat n, 1 <= n) => SupportedPrim (s n),
      forall n. (KnownNat n, 1 <= n) => SignConversion (u n) (s n),
      SignConversion (u 1) (s 1),
      Typeable u,
      Typeable s,
      KnownNat n,
      1 <= n,
      SizedBV u,
      SizedBV s
    ) =>
    !(Term (u n)) ->
    UTerm (s n)
  UToUnsignedTerm ::
    ( forall n. (KnownNat n, 1 <= n) => SupportedPrim (u n),
      forall n. (KnownNat n, 1 <= n) => SupportedPrim (s n),
      forall n. (KnownNat n, 1 <= n) => SignConversion (u n) (s n),
      SignConversion (u 1) (s 1),
      Typeable u,
      Typeable s,
      KnownNat n,
      1 <= n,
      SizedBV u,
      SizedBV s
    ) =>
    !(Term (s n)) ->
    UTerm (u n)
  UBVConcatTerm ::
    ( forall n. (KnownNat n, 1 <= n) => SupportedPrim (bv n),
      Typeable bv,
      KnownNat a,
      KnownNat b,
      KnownNat (a + b),
      1 <= a,
      1 <= b,
      1 <= a + b,
      SizedBV bv
    ) =>
    !(Term (bv a)) ->
    !(Term (bv b)) ->
    UTerm (bv (a + b))
  UBVSelectTerm ::
    ( forall n. (KnownNat n, 1 <= n) => SupportedPrim (bv n),
      Typeable bv,
      KnownNat n,
      KnownNat ix,
      KnownNat w,
      1 <= n,
      1 <= w,
      ix + w <= n,
      SizedBV bv
    ) =>
    !(TypeRep ix) ->
    !(TypeRep w) ->
    !(Term (bv n)) ->
    UTerm (bv w)
  UBVExtendTerm ::
    ( forall n. (KnownNat n, 1 <= n) => SupportedPrim (bv n),
      Typeable bv,
      KnownNat l,
      KnownNat r,
      1 <= l,
      1 <= r,
      l <= r,
      SizedBV bv
    ) =>
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
  UDivIntegralTerm :: (SupportedPrim t, Integral t) => !(Term t) -> !(Term t) -> UTerm t
  UModIntegralTerm :: (SupportedPrim t, Integral t) => !(Term t) -> !(Term t) -> UTerm t
  UQuotIntegralTerm :: (SupportedPrim t, Integral t) => !(Term t) -> !(Term t) -> UTerm t
  URemIntegralTerm :: (SupportedPrim t, Integral t) => !(Term t) -> !(Term t) -> UTerm t
  UDivBoundedIntegralTerm :: (SupportedPrim t, Bounded t, Integral t) => !(Term t) -> !(Term t) -> UTerm t
  UModBoundedIntegralTerm :: (SupportedPrim t, Bounded t, Integral t) => !(Term t) -> !(Term t) -> UTerm t
  UQuotBoundedIntegralTerm :: (SupportedPrim t, Bounded t, Integral t) => !(Term t) -> !(Term t) -> UTerm t
  URemBoundedIntegralTerm :: (SupportedPrim t, Bounded t, Integral t) => !(Term t) -> !(Term t) -> UTerm t

eqTypedId :: (TypeRep a, Id) -> (TypeRep b, Id) -> Bool
eqTypedId (a, i1) (b, i2) = i1 == i2 && eqTypeRepBool a b
{-# INLINE eqTypedId #-}

eqHeteroTag :: (Eq a) => (TypeRep a, a) -> (TypeRep b, b) -> Bool
eqHeteroTag (tpa, taga) (tpb, tagb) = eqHeteroRep tpa tpb taga tagb
{-# INLINE eqHeteroTag #-}

instance (SupportedPrim t) => Interned (Term t) where
  type Uninterned (Term t) = UTerm t
  data Description (Term t) where
    DConTerm :: t -> Description (Term t)
    DSymTerm :: TypedSymbol t -> Description (Term t)
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
    DEqvTerm :: TypeRep args -> {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term Bool)
    DITETerm :: {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term t)
    DAddNumTerm :: {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term t)
    DUMinusNumTerm :: {-# UNPACK #-} !Id -> Description (Term t)
    DTimesNumTerm :: {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term t)
    DAbsNumTerm :: {-# UNPACK #-} !Id -> Description (Term t)
    DSignumNumTerm :: {-# UNPACK #-} !Id -> Description (Term t)
    DLTNumTerm :: TypeRep args -> {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term Bool)
    DLENumTerm :: TypeRep args -> {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term Bool)
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
    DDivBoundedIntegralTerm :: {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term a)
    DModBoundedIntegralTerm :: {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term a)
    DQuotBoundedIntegralTerm :: {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term a)
    DRemBoundedIntegralTerm :: {-# UNPACK #-} !Id -> {-# UNPACK #-} !Id -> Description (Term a)

  describe (UConTerm v) = DConTerm v
  describe ((USymTerm name) :: UTerm t) = DSymTerm @t name
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
  describe (UEqvTerm (arg1 :: Term arg) arg2) = DEqvTerm (typeRep :: TypeRep arg) (identity arg1) (identity arg2)
  describe (UITETerm cond (l :: Term arg) r) = DITETerm (identity cond) (identity l) (identity r)
  describe (UAddNumTerm arg1 arg2) = DAddNumTerm (identity arg1) (identity arg2)
  describe (UUMinusNumTerm arg) = DUMinusNumTerm (identity arg)
  describe (UTimesNumTerm arg1 arg2) = DTimesNumTerm (identity arg1) (identity arg2)
  describe (UAbsNumTerm arg) = DAbsNumTerm (identity arg)
  describe (USignumNumTerm arg) = DSignumNumTerm (identity arg)
  describe (ULTNumTerm (arg1 :: arg) arg2) = DLTNumTerm (typeRep :: TypeRep arg) (identity arg1) (identity arg2)
  describe (ULENumTerm (arg1 :: arg) arg2) = DLENumTerm (typeRep :: TypeRep arg) (identity arg1) (identity arg2)
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
  describe (UDivBoundedIntegralTerm arg1 arg2) = DDivBoundedIntegralTerm (identity arg1) (identity arg2)
  describe (UModBoundedIntegralTerm arg1 arg2) = DModBoundedIntegralTerm (identity arg1) (identity arg2)
  describe (UQuotBoundedIntegralTerm arg1 arg2) = DRemBoundedIntegralTerm (identity arg1) (identity arg2)
  describe (URemBoundedIntegralTerm arg1 arg2) = DQuotBoundedIntegralTerm (identity arg1) (identity arg2)

  identify i = go
    where
      go (UConTerm v) = ConTerm i v
      go (USymTerm v) = SymTerm i v
      go (UUnaryTerm tag tm) = UnaryTerm i tag tm
      go (UBinaryTerm tag tm1 tm2) = BinaryTerm i tag tm1 tm2
      go (UTernaryTerm tag tm1 tm2 tm3) = TernaryTerm i tag tm1 tm2 tm3
      go (UNotTerm arg) = NotTerm i arg
      go (UOrTerm arg1 arg2) = OrTerm i arg1 arg2
      go (UAndTerm arg1 arg2) = AndTerm i arg1 arg2
      go (UEqvTerm arg1 arg2) = EqvTerm i arg1 arg2
      go (UITETerm cond l r) = ITETerm i cond l r
      go (UAddNumTerm arg1 arg2) = AddNumTerm i arg1 arg2
      go (UUMinusNumTerm arg) = UMinusNumTerm i arg
      go (UTimesNumTerm arg1 arg2) = TimesNumTerm i arg1 arg2
      go (UAbsNumTerm arg) = AbsNumTerm i arg
      go (USignumNumTerm arg) = SignumNumTerm i arg
      go (ULTNumTerm arg1 arg2) = LTNumTerm i arg1 arg2
      go (ULENumTerm arg1 arg2) = LENumTerm i arg1 arg2
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
      go (UDivBoundedIntegralTerm arg1 arg2) = DivBoundedIntegralTerm i arg1 arg2
      go (UModBoundedIntegralTerm arg1 arg2) = ModBoundedIntegralTerm i arg1 arg2
      go (UQuotBoundedIntegralTerm arg1 arg2) = QuotBoundedIntegralTerm i arg1 arg2
      go (URemBoundedIntegralTerm arg1 arg2) = RemBoundedIntegralTerm i arg1 arg2
  cache = termCache

instance (SupportedPrim t) => Eq (Description (Term t)) where
  DConTerm (l :: tyl) == DConTerm (r :: tyr) = cast @tyl @tyr l == Just r
  DSymTerm ls == DSymTerm rs = ls == rs
  DUnaryTerm (tagl :: tagl) li == DUnaryTerm (tagr :: tagr) ri = eqHeteroTag tagl tagr && eqTypedId li ri
  DBinaryTerm (tagl :: tagl) li1 li2 == DBinaryTerm (tagr :: tagr) ri1 ri2 =
    eqHeteroTag tagl tagr && eqTypedId li1 ri1 && eqTypedId li2 ri2
  DTernaryTerm (tagl :: tagl) li1 li2 li3 == DTernaryTerm (tagr :: tagr) ri1 ri2 ri3 =
    eqHeteroTag tagl tagr && eqTypedId li1 ri1 && eqTypedId li2 ri2 && eqTypedId li3 ri3
  DNotTerm li == DNotTerm ri = li == ri
  DOrTerm li1 li2 == DOrTerm ri1 ri2 = li1 == ri1 && li2 == ri2
  DAndTerm li1 li2 == DAndTerm ri1 ri2 = li1 == ri1 && li2 == ri2
  DEqvTerm lrep li1 li2 == DEqvTerm rrep ri1 ri2 = eqTypeRepBool lrep rrep && li1 == ri1 && li2 == ri2
  DITETerm lc li1 li2 == DITETerm rc ri1 ri2 = lc == rc && li1 == ri1 && li2 == ri2
  DAddNumTerm li1 li2 == DAddNumTerm ri1 ri2 = li1 == ri1 && li2 == ri2
  DUMinusNumTerm li == DUMinusNumTerm ri = li == ri
  DTimesNumTerm li1 li2 == DTimesNumTerm ri1 ri2 = li1 == ri1 && li2 == ri2
  DAbsNumTerm li == DAbsNumTerm ri = li == ri
  DSignumNumTerm li == DSignumNumTerm ri = li == ri
  DLTNumTerm lrep li1 li2 == DLTNumTerm rrep ri1 ri2 = eqTypeRepBool lrep rrep && li1 == ri1 && li2 == ri2
  DLENumTerm lrep li1 li2 == DLENumTerm rrep ri1 ri2 = eqTypeRepBool lrep rrep && li1 == ri1 && li2 == ri2
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
  DDivBoundedIntegralTerm li1 li2 == DDivBoundedIntegralTerm ri1 ri2 = li1 == ri1 && li2 == ri2
  DModBoundedIntegralTerm li1 li2 == DModBoundedIntegralTerm ri1 ri2 = li1 == ri1 && li2 == ri2
  DQuotBoundedIntegralTerm li1 li2 == DQuotBoundedIntegralTerm ri1 ri2 = li1 == ri1 && li2 == ri2
  DRemBoundedIntegralTerm li1 li2 == DRemBoundedIntegralTerm ri1 ri2 = li1 == ri1 && li2 == ri2
  _ == _ = False

instance (SupportedPrim t) => Hashable (Description (Term t)) where
  hashWithSalt s (DConTerm c) = s `hashWithSalt` (0 :: Int) `hashWithSalt` c
  hashWithSalt s (DSymTerm name) = s `hashWithSalt` (1 :: Int) `hashWithSalt` name
  hashWithSalt s (DUnaryTerm tag id1) = s `hashWithSalt` (2 :: Int) `hashWithSalt` tag `hashWithSalt` id1
  hashWithSalt s (DBinaryTerm tag id1 id2) =
    s `hashWithSalt` (3 :: Int) `hashWithSalt` tag `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DTernaryTerm tag id1 id2 id3) =
    s `hashWithSalt` (4 :: Int) `hashWithSalt` tag `hashWithSalt` id1 `hashWithSalt` id2 `hashWithSalt` id3
  hashWithSalt s (DNotTerm id1) = s `hashWithSalt` (5 :: Int) `hashWithSalt` id1
  hashWithSalt s (DOrTerm id1 id2) = s `hashWithSalt` (6 :: Int) `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DAndTerm id1 id2) = s `hashWithSalt` (7 :: Int) `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DEqvTerm rep id1 id2) =
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
  hashWithSalt s (DUMinusNumTerm id1) = s `hashWithSalt` (11 :: Int) `hashWithSalt` id1
  hashWithSalt s (DTimesNumTerm id1 id2) = s `hashWithSalt` (12 :: Int) `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DAbsNumTerm id1) = s `hashWithSalt` (13 :: Int) `hashWithSalt` id1
  hashWithSalt s (DSignumNumTerm id1) = s `hashWithSalt` (14 :: Int) `hashWithSalt` id1
  hashWithSalt s (DLTNumTerm rep id1 id2) =
    s `hashWithSalt` (15 :: Int) `hashWithSalt` rep `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DLENumTerm rep id1 id2) =
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
  hashWithSalt s (DDivBoundedIntegralTerm id1 id2) = s `hashWithSalt` (34 :: Int) `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DModBoundedIntegralTerm id1 id2) = s `hashWithSalt` (35 :: Int) `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DQuotBoundedIntegralTerm id1 id2) = s `hashWithSalt` (36 :: Int) `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DRemBoundedIntegralTerm id1 id2) = s `hashWithSalt` (37 :: Int) `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DApplyTerm id1 id2) = s `hashWithSalt` (38 :: Int) `hashWithSalt` id1 `hashWithSalt` id2

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

eqvTerm :: (SupportedPrim a) => Term a -> Term a -> Term Bool
eqvTerm l r = internTerm $ UEqvTerm l r
{-# INLINE eqvTerm #-}

iteTerm :: (SupportedPrim a) => Term Bool -> Term a -> Term a -> Term a
iteTerm c l r = internTerm $ UITETerm c l r
{-# INLINE iteTerm #-}

addNumTerm :: (SupportedPrim a, Num a) => Term a -> Term a -> Term a
addNumTerm l r = internTerm $ UAddNumTerm l r
{-# INLINE addNumTerm #-}

uminusNumTerm :: (SupportedPrim a, Num a) => Term a -> Term a
uminusNumTerm = internTerm . UUMinusNumTerm
{-# INLINE uminusNumTerm #-}

timesNumTerm :: (SupportedPrim a, Num a) => Term a -> Term a -> Term a
timesNumTerm l r = internTerm $ UTimesNumTerm l r
{-# INLINE timesNumTerm #-}

absNumTerm :: (SupportedPrim a, Num a) => Term a -> Term a
absNumTerm = internTerm . UAbsNumTerm
{-# INLINE absNumTerm #-}

signumNumTerm :: (SupportedPrim a, Num a) => Term a -> Term a
signumNumTerm = internTerm . USignumNumTerm
{-# INLINE signumNumTerm #-}

ltNumTerm :: (SupportedPrim a, Num a, Ord a) => Term a -> Term a -> Term Bool
ltNumTerm l r = internTerm $ ULTNumTerm l r
{-# INLINE ltNumTerm #-}

leNumTerm :: (SupportedPrim a, Num a, Ord a) => Term a -> Term a -> Term Bool
leNumTerm l r = internTerm $ ULENumTerm l r
{-# INLINE leNumTerm #-}

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

shiftLeftTerm :: (SupportedPrim a, Integral a, FiniteBits a, SymShift a) => Term a -> Term a -> Term a
shiftLeftTerm t n = internTerm $ UShiftLeftTerm t n
{-# INLINE shiftLeftTerm #-}

shiftRightTerm :: (SupportedPrim a, Integral a, FiniteBits a, SymShift a) => Term a -> Term a -> Term a
shiftRightTerm t n = internTerm $ UShiftRightTerm t n
{-# INLINE shiftRightTerm #-}

rotateLeftTerm :: (SupportedPrim a, Integral a, FiniteBits a, SymRotate a) => Term a -> Term a -> Term a
rotateLeftTerm t n = internTerm $ URotateLeftTerm t n
{-# INLINE rotateLeftTerm #-}

rotateRightTerm :: (SupportedPrim a, Integral a, FiniteBits a, SymRotate a) => Term a -> Term a -> Term a
rotateRightTerm t n = internTerm $ URotateRightTerm t n
{-# INLINE rotateRightTerm #-}

toSignedTerm ::
  ( forall n. (KnownNat n, 1 <= n) => SupportedPrim (u n),
    forall n. (KnownNat n, 1 <= n) => SupportedPrim (s n),
    forall n. (KnownNat n, 1 <= n) => SignConversion (u n) (s n),
    SignConversion (u 1) (s 1),
    Typeable u,
    Typeable s,
    KnownNat n,
    1 <= n,
    SizedBV u,
    SizedBV s
  ) =>
  Term (u n) ->
  Term (s n)
toSignedTerm = internTerm . UToSignedTerm

toUnsignedTerm ::
  ( forall n. (KnownNat n, 1 <= n) => SupportedPrim (u n),
    forall n. (KnownNat n, 1 <= n) => SupportedPrim (s n),
    forall n. (KnownNat n, 1 <= n) => SignConversion (u n) (s n),
    SignConversion (u 1) (s 1),
    Typeable u,
    Typeable s,
    KnownNat n,
    1 <= n,
    SizedBV u,
    SizedBV s
  ) =>
  Term (s n) ->
  Term (u n)
toUnsignedTerm = internTerm . UToUnsignedTerm

bvconcatTerm ::
  ( forall n. (KnownNat n, 1 <= n) => SupportedPrim (bv n),
    Typeable bv,
    KnownNat a,
    KnownNat b,
    KnownNat (a + b),
    1 <= a,
    1 <= b,
    1 <= a + b,
    SizedBV bv
  ) =>
  Term (bv a) ->
  Term (bv b) ->
  Term (bv (a + b))
bvconcatTerm l r = internTerm $ UBVConcatTerm l r
{-# INLINE bvconcatTerm #-}

bvselectTerm ::
  forall bv n ix w p q.
  ( forall n. (KnownNat n, 1 <= n) => SupportedPrim (bv n),
    Typeable bv,
    KnownNat n,
    KnownNat ix,
    KnownNat w,
    1 <= n,
    1 <= w,
    ix + w <= n,
    SizedBV bv
  ) =>
  p ix ->
  q w ->
  Term (bv n) ->
  Term (bv w)
bvselectTerm _ _ v = internTerm $ UBVSelectTerm (typeRep @ix) (typeRep @w) v
{-# INLINE bvselectTerm #-}

bvextendTerm ::
  forall bv l r proxy.
  ( forall n. (KnownNat n, 1 <= n) => SupportedPrim (bv n),
    Typeable bv,
    KnownNat l,
    KnownNat r,
    1 <= l,
    1 <= r,
    l <= r,
    SizedBV bv
  ) =>
  Bool ->
  proxy r ->
  Term (bv l) ->
  Term (bv r)
bvextendTerm signed _ v = internTerm $ UBVExtendTerm signed (typeRep @r) v
{-# INLINE bvextendTerm #-}

bvsignExtendTerm ::
  forall bv l r proxy.
  ( forall n. (KnownNat n, 1 <= n) => SupportedPrim (bv n),
    Typeable bv,
    KnownNat l,
    KnownNat r,
    1 <= l,
    1 <= r,
    l <= r,
    SizedBV bv
  ) =>
  proxy r ->
  Term (bv l) ->
  Term (bv r)
bvsignExtendTerm _ v = internTerm $ UBVExtendTerm True (typeRep @r) v
{-# INLINE bvsignExtendTerm #-}

bvzeroExtendTerm ::
  forall bv l r proxy.
  ( forall n. (KnownNat n, 1 <= n) => SupportedPrim (bv n),
    Typeable bv,
    KnownNat l,
    KnownNat r,
    1 <= l,
    1 <= r,
    l <= r,
    SizedBV bv
  ) =>
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

divIntegralTerm :: (SupportedPrim a, Integral a) => Term a -> Term a -> Term a
divIntegralTerm l r = internTerm $ UDivIntegralTerm l r
{-# INLINE divIntegralTerm #-}

modIntegralTerm :: (SupportedPrim a, Integral a) => Term a -> Term a -> Term a
modIntegralTerm l r = internTerm $ UModIntegralTerm l r
{-# INLINE modIntegralTerm #-}

quotIntegralTerm :: (SupportedPrim a, Integral a) => Term a -> Term a -> Term a
quotIntegralTerm l r = internTerm $ UQuotIntegralTerm l r
{-# INLINE quotIntegralTerm #-}

remIntegralTerm :: (SupportedPrim a, Integral a) => Term a -> Term a -> Term a
remIntegralTerm l r = internTerm $ URemIntegralTerm l r
{-# INLINE remIntegralTerm #-}

divBoundedIntegralTerm :: (SupportedPrim a, Bounded a, Integral a) => Term a -> Term a -> Term a
divBoundedIntegralTerm l r = internTerm $ UDivBoundedIntegralTerm l r
{-# INLINE divBoundedIntegralTerm #-}

modBoundedIntegralTerm :: (SupportedPrim a, Bounded a, Integral a) => Term a -> Term a -> Term a
modBoundedIntegralTerm l r = internTerm $ UModBoundedIntegralTerm l r
{-# INLINE modBoundedIntegralTerm #-}

quotBoundedIntegralTerm :: (SupportedPrim a, Bounded a, Integral a) => Term a -> Term a -> Term a
quotBoundedIntegralTerm l r = internTerm $ UQuotBoundedIntegralTerm l r
{-# INLINE quotBoundedIntegralTerm #-}

remBoundedIntegralTerm :: (SupportedPrim a, Bounded a, Integral a) => Term a -> Term a -> Term a
remBoundedIntegralTerm l r = internTerm $ URemBoundedIntegralTerm l r
{-# INLINE remBoundedIntegralTerm #-}

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

-- Eqv
pevalEqvTerm :: forall a. (SupportedPrim a) => Term a -> Term a -> Term Bool
pevalEqvTerm l@ConTerm {} r@ConTerm {} = conTerm $ l == r
pevalEqvTerm l@ConTerm {} r = pevalEqvTerm r l
pevalEqvTerm l (BoolConTerm rv) = if rv then unsafeCoerce l else pevalNotTerm $ unsafeCoerce l
pevalEqvTerm (NotTerm _ lv) r
  | lv == unsafeCoerce r = falseTerm
pevalEqvTerm l (NotTerm _ rv)
  | unsafeCoerce l == rv = falseTerm
{-
pevalBinary _ (ConTerm l) (ConTerm r) =
  if l == r then trueTerm else falseTerm
  -}
pevalEqvTerm
  ( AddNumTerm
      _
      (ConTerm _ c :: Term a)
      (Dyn (v :: Term a))
    )
  (Dyn (ConTerm _ c2 :: Term a)) =
    pevalEqvTerm v (conTerm $ c2 - c)
pevalEqvTerm
  (Dyn (ConTerm _ c2 :: Term a))
  ( AddNumTerm
      _
      (Dyn (ConTerm _ c :: Term a))
      (Dyn (v :: Term a))
    ) =
    pevalEqvTerm v (conTerm $ c2 - c)
pevalEqvTerm l (ITETerm _ c t f)
  | l == t = pevalOrTerm c (pevalEqvTerm l f)
  | l == f = pevalOrTerm (pevalNotTerm c) (pevalEqvTerm l t)
pevalEqvTerm (ITETerm _ c t f) r
  | t == r = pevalOrTerm c (pevalEqvTerm f r)
  | f == r = pevalOrTerm (pevalNotTerm c) (pevalEqvTerm t r)
pevalEqvTerm l r
  | l == r = trueTerm
  | otherwise = eqvTerm l r
{-# INLINEABLE pevalEqvTerm #-}

pevalNotEqvTerm :: (SupportedPrim a) => Term a -> Term a -> Term Bool
pevalNotEqvTerm l r = pevalNotTerm $ pevalEqvTerm l r
{-# INLINE pevalNotEqvTerm #-}

orEqFirst :: Term Bool -> Term Bool -> Bool
orEqFirst _ (ConTerm _ False) = True
orEqFirst
  (NotTerm _ (EqvTerm _ (e1 :: Term a) (ec1@(ConTerm _ _) :: Term b)))
  (EqvTerm _ (Dyn (e2 :: Term a)) (Dyn (ec2@(ConTerm _ _) :: Term b)))
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
  (NotTerm _ (EqvTerm _ (e1 :: Term a) (ec1@(ConTerm _ _) :: Term b)))
  (NotTerm _ (EqvTerm _ (Dyn (e2 :: Term a)) (Dyn (ec2@(ConTerm _ _) :: Term b))))
    | e1 == e2 && ec1 /= ec2 = True
orEqTrue (NotTerm _ l) r | l == r = True
orEqTrue l (NotTerm _ r) | l == r = True
orEqTrue _ _ = False
{-# INLINE orEqTrue #-}

andEqFirst :: Term Bool -> Term Bool -> Bool
andEqFirst _ (ConTerm _ True) = True
-- andEqFirst x (NotTerm _ y) = andEqFalse x y
andEqFirst
  (EqvTerm _ (e1 :: Term a) (ec1@(ConTerm _ _) :: Term b))
  (NotTerm _ (EqvTerm _ (Dyn (e2 :: Term a)) (Dyn (ec2@(ConTerm _ _) :: Term b))))
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
  (EqvTerm _ (e1 :: Term a) (ec1@(ConTerm _ _) :: Term b))
  (EqvTerm _ (Dyn (e2 :: Term a)) (Dyn (ec2@(ConTerm _ _) :: Term b)))
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
  (EqvTerm _ (e1 :: Term a) (ec1@(ConTerm _ _) :: Term b))
  (NotTerm _ (EqvTerm _ (Dyn (e2 :: Term a)) (Dyn (ec2@(ConTerm _ _) :: Term b))))
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
      ( EqvTerm _ (e1 :: Term a) (ec1@(ConTerm _ _) :: Term b),
        EqvTerm _ (Dyn (e2 :: Term a)) (Dyn (ec2@(ConTerm _ _) :: Term b))
        )
          | e1 == e2 && ec1 /= ec2 -> Just trueRes
      _ -> Nothing
pevalInferImplies
  (EqvTerm _ (e1 :: Term a) (ec1@(ConTerm _ _) :: Term b))
  (EqvTerm _ (Dyn (e2 :: Term a)) (Dyn (ec2@(ConTerm _ _) :: Term b)))
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

instance SupportedPrim Bool where
  pformatCon True = "true"
  pformatCon False = "false"
  defaultValue = defaultValueForBool
  defaultValueDynamic _ = defaultValueForBoolDyn
  pevalITETerm cond ifTrue ifFalse =
    fromMaybe (iteTerm cond ifTrue ifFalse) $
      pevalITEBool cond ifTrue ifFalse
