{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
  ( SupportedPrim (..),
    SymRep (..),
    ConRep (..),
    LinkedRep (..),
    UnaryOp (..),
    BinaryOp (..),
    TernaryOp (..),
    TypedSymbol (..),
    SomeTypedSymbol (..),
    Term (..),
    UTerm (..),
    type (-->) (..),
    buildGeneralFun,
  )
where

import Control.DeepSeq (NFData)
import Data.Bits (Bits, FiniteBits)
import Data.Hashable (Hashable)
import Data.Interned (Cache, Id)
import Data.Kind (Constraint)
import qualified Data.Text as T
import GHC.TypeNats (KnownNat, type (+), type (<=))
import Grisette.Core.Data.Class.BitVector
  ( SizedBV,
  )
import Grisette.Core.Data.Class.SignConversion (SignConversion)
import Grisette.Core.Data.Class.SymRotate (SymRotate)
import Grisette.Core.Data.Class.SymShift (SymShift)
import Grisette.IR.SymPrim.Data.Prim.ModelValue
  ( ModelValue,
    toModelValue,
  )
import Grisette.IR.SymPrim.Data.TabularFun
  ( type (=->),
  )
import Language.Haskell.TH.Syntax (Lift)
import Type.Reflection (TypeRep, Typeable)

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
  pformatSym _ = showUntyped
  defaultValue :: t
  defaultValueDynamic :: proxy t -> ModelValue
  defaultValueDynamic _ = toModelValue (defaultValue @t)

class ConRep sym where
  type ConType sym

class (SupportedPrim con) => SymRep con where
  type SymType con

class
  (ConRep sym, SymRep con, sym ~ SymType con, con ~ ConType sym) =>
  LinkedRep con sym
    | con -> sym,
      sym -> con
  where
  underlyingTerm :: sym -> Term con
  wrapTerm :: Term con -> sym

class
  (SupportedPrim arg, SupportedPrim t, Lift tag, NFData tag, Show tag, Typeable tag, Eq tag, Hashable tag) =>
  UnaryOp tag arg t
    | tag arg -> t
  where
  partialEvalUnary :: (Typeable tag, Typeable t) => tag -> Term arg -> Term t
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
  partialEvalBinary :: (Typeable tag, Typeable t) => tag -> Term arg1 -> Term arg2 -> Term t
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
  partialEvalTernary :: (Typeable tag, Typeable t) => tag -> Term arg1 -> Term arg2 -> Term arg3 -> Term t
  pformatTernary :: tag -> Term arg1 -> Term arg2 -> Term arg3 -> String

data TypedSymbol t where
  SimpleSymbol :: (SupportedPrim t) => T.Text -> TypedSymbol t
  IndexedSymbol :: (SupportedPrim t) => T.Text -> Int -> TypedSymbol t
  WithInfo ::
    forall t a.
    ( SupportedPrim t,
      Typeable a,
      Ord a,
      Lift a,
      NFData a,
      Show a,
      Hashable a
    ) =>
    TypedSymbol t ->
    a ->
    TypedSymbol t

data SomeTypedSymbol where
  SomeTypedSymbol :: forall t. TypeRep t -> TypedSymbol t -> SomeTypedSymbol

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
  ITETerm :: (SupportedPrim t) => {-# UNPACK #-} !Id -> !(Term Bool) -> !(Term t) -> !(Term t) -> Term t
  AddNumTerm :: (SupportedPrim t, Num t) => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term t
  UMinusNumTerm :: (SupportedPrim t, Num t) => {-# UNPACK #-} !Id -> !(Term t) -> Term t
  TimesNumTerm :: (SupportedPrim t, Num t) => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term t
  AbsNumTerm :: (SupportedPrim t, Num t) => {-# UNPACK #-} !Id -> !(Term t) -> Term t
  SignumNumTerm :: (SupportedPrim t, Num t) => {-# UNPACK #-} !Id -> !(Term t) -> Term t
  LTNumTerm :: (SupportedPrim t, Num t, Ord t) => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term Bool
  LENumTerm :: (SupportedPrim t, Num t, Ord t) => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term Bool
  AndBitsTerm :: (SupportedPrim t, Bits t) => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term t
  OrBitsTerm :: (SupportedPrim t, Bits t) => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term t
  XorBitsTerm :: (SupportedPrim t, Bits t) => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term t
  ComplementBitsTerm :: (SupportedPrim t, Bits t) => {-# UNPACK #-} !Id -> !(Term t) -> Term t
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
      1 <= (a + b),
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
  TabularFunApplyTerm ::
    ( SupportedPrim a,
      SupportedPrim b
    ) =>
    {-# UNPACK #-} !Id ->
    Term (a =-> b) ->
    Term a ->
    Term b
  GeneralFunApplyTerm ::
    ( SupportedPrim a,
      SupportedPrim b
    ) =>
    {-# UNPACK #-} !Id ->
    Term (a --> b) ->
    Term a ->
    Term b
  DivIntegralTerm :: (SupportedPrim t, Integral t) => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term t
  ModIntegralTerm :: (SupportedPrim t, Integral t) => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term t
  QuotIntegralTerm :: (SupportedPrim t, Integral t) => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term t
  RemIntegralTerm :: (SupportedPrim t, Integral t) => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term t
  DivBoundedIntegralTerm :: (SupportedPrim t, Bounded t, Integral t) => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term t
  ModBoundedIntegralTerm :: (SupportedPrim t, Bounded t, Integral t) => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term t
  QuotBoundedIntegralTerm :: (SupportedPrim t, Bounded t, Integral t) => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term t
  RemBoundedIntegralTerm :: (SupportedPrim t, Bounded t, Integral t) => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term t

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
  UITETerm :: (SupportedPrim t) => !(Term Bool) -> !(Term t) -> !(Term t) -> UTerm t
  UAddNumTerm :: (SupportedPrim t, Num t) => !(Term t) -> !(Term t) -> UTerm t
  UUMinusNumTerm :: (SupportedPrim t, Num t) => !(Term t) -> UTerm t
  UTimesNumTerm :: (SupportedPrim t, Num t) => !(Term t) -> !(Term t) -> UTerm t
  UAbsNumTerm :: (SupportedPrim t, Num t) => !(Term t) -> UTerm t
  USignumNumTerm :: (SupportedPrim t, Num t) => !(Term t) -> UTerm t
  ULTNumTerm :: (SupportedPrim t, Num t, Ord t) => !(Term t) -> !(Term t) -> UTerm Bool
  ULENumTerm :: (SupportedPrim t, Num t, Ord t) => !(Term t) -> !(Term t) -> UTerm Bool
  UAndBitsTerm :: (SupportedPrim t, Bits t) => !(Term t) -> !(Term t) -> UTerm t
  UOrBitsTerm :: (SupportedPrim t, Bits t) => !(Term t) -> !(Term t) -> UTerm t
  UXorBitsTerm :: (SupportedPrim t, Bits t) => !(Term t) -> !(Term t) -> UTerm t
  UComplementBitsTerm :: (SupportedPrim t, Bits t) => !(Term t) -> UTerm t
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
  UTabularFunApplyTerm ::
    ( SupportedPrim a,
      SupportedPrim b
    ) =>
    Term (a =-> b) ->
    Term a ->
    UTerm b
  UGeneralFunApplyTerm ::
    ( SupportedPrim a,
      SupportedPrim b
    ) =>
    Term (a --> b) ->
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

data (-->) a b where
  GeneralFun :: (SupportedPrim a, SupportedPrim b) => TypedSymbol a -> Term b -> a --> b

infixr 0 -->

buildGeneralFun :: (SupportedPrim a, SupportedPrim b) => TypedSymbol a -> Term b -> a --> b
