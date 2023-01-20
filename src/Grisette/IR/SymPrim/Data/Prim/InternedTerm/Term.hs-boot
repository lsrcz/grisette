{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
  ( SupportedPrim (..),
    UnaryOp (..),
    BinaryOp (..),
    TernaryOp (..),
    TypedSymbol (..),
    SomeTypedSymbol (..),
    Term (..),
    UTerm (..),
    type (-->) (..),
  )
where

import Control.DeepSeq
import Data.Bits
import Data.Hashable
import Data.Interned
import Data.Kind
import GHC.TypeNats
import Grisette.Core.Data.Class.BitVector
import Grisette.Core.Data.Class.Function
import Grisette.IR.SymPrim.Data.Prim.ModelValue
import {-# SOURCE #-} Grisette.IR.SymPrim.Data.TabularFun
import Language.Haskell.TH.Syntax
import Type.Reflection

class (Lift t, Typeable t, Hashable t, Eq t, Show t, NFData t) => SupportedPrim t where
  type PrimConstraint t :: Constraint
  type PrimConstraint t = ()
  default withPrim :: PrimConstraint t => proxy t -> (PrimConstraint t => a) -> a
  withPrim :: proxy t -> (PrimConstraint t => a) -> a
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
  SimpleSymbol :: SupportedPrim t => String -> TypedSymbol t
  IndexedSymbol :: SupportedPrim t => String -> Int -> TypedSymbol t
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
  EqvTerm :: SupportedPrim t => {-# UNPACK #-} !Id -> !(Term t) -> !(Term t) -> Term Bool
  ITETerm :: SupportedPrim t => {-# UNPACK #-} !Id -> !(Term Bool) -> !(Term t) -> !(Term t) -> Term t
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
  ShiftBitsTerm :: (SupportedPrim t, Bits t) => {-# UNPACK #-} !Id -> !(Term t) -> {-# UNPACK #-} !Int -> Term t
  RotateBitsTerm :: (SupportedPrim t, Bits t) => {-# UNPACK #-} !Id -> !(Term t) -> {-# UNPACK #-} !Int -> Term t
  BVConcatTerm ::
    ( SupportedPrim (bv a),
      SupportedPrim (bv b),
      SupportedPrim (bv c),
      KnownNat a,
      KnownNat b,
      KnownNat c,
      BVConcat (bv a) (bv b) (bv c)
    ) =>
    {-# UNPACK #-} !Id ->
    !(Term (bv a)) ->
    !(Term (bv b)) ->
    Term (bv c)
  BVSelectTerm ::
    ( SupportedPrim (bv a),
      SupportedPrim (bv w),
      KnownNat a,
      KnownNat w,
      KnownNat ix,
      BVSelect (bv a) ix w (bv w)
    ) =>
    {-# UNPACK #-} !Id ->
    !(TypeRep ix) ->
    !(TypeRep w) ->
    !(Term (bv a)) ->
    Term (bv w)
  BVExtendTerm ::
    ( SupportedPrim (bv a),
      SupportedPrim (bv b),
      KnownNat a,
      KnownNat b,
      KnownNat n,
      BVExtend (bv a) n (bv b)
    ) =>
    {-# UNPACK #-} !Id ->
    !Bool ->
    !(TypeRep n) ->
    !(Term (bv a)) ->
    Term (bv b)
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
  DivIntegerTerm :: !Id -> Term Integer -> Term Integer -> Term Integer
  ModIntegerTerm :: !Id -> Term Integer -> Term Integer -> Term Integer

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
  UEqvTerm :: SupportedPrim t => !(Term t) -> !(Term t) -> UTerm Bool
  UITETerm :: SupportedPrim t => !(Term Bool) -> !(Term t) -> !(Term t) -> UTerm t
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
  UShiftBitsTerm :: (SupportedPrim t, Bits t) => !(Term t) -> {-# UNPACK #-} !Int -> UTerm t
  URotateBitsTerm :: (SupportedPrim t, Bits t) => !(Term t) -> {-# UNPACK #-} !Int -> UTerm t
  UBVConcatTerm ::
    ( SupportedPrim (bv a),
      SupportedPrim (bv b),
      SupportedPrim (bv c),
      KnownNat a,
      KnownNat b,
      KnownNat c,
      BVConcat (bv a) (bv b) (bv c)
    ) =>
    !(Term (bv a)) ->
    !(Term (bv b)) ->
    UTerm (bv c)
  UBVSelectTerm ::
    ( SupportedPrim (bv a),
      SupportedPrim (bv w),
      KnownNat a,
      KnownNat ix,
      KnownNat w,
      BVSelect (bv a) ix w (bv w)
    ) =>
    !(TypeRep ix) ->
    !(TypeRep w) ->
    !(Term (bv a)) ->
    UTerm (bv w)
  UBVExtendTerm ::
    ( SupportedPrim (bv a),
      SupportedPrim (bv b),
      KnownNat a,
      KnownNat b,
      KnownNat n,
      BVExtend (bv a) n (bv b)
    ) =>
    !Bool ->
    !(TypeRep n) ->
    !(Term (bv a)) ->
    UTerm (bv b)
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
  UDivIntegerTerm :: Term Integer -> Term Integer -> UTerm Integer
  UModIntegerTerm :: Term Integer -> Term Integer -> UTerm Integer

data (-->) a b where
  GeneralFun :: (SupportedPrim a, SupportedPrim b) => TypedSymbol a -> Term b -> a --> b

infixr 0 -->
