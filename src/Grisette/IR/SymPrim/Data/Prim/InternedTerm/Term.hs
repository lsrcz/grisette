{-# LANGUAGE AllowAmbiguousTypes #-}
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

-- |
-- Module      :   Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
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
    showUntyped,
    withSymbolSupported,
    someTypedSymbol,
    Term (..),
    UTerm (..),
    type (-->) (..),
    buildGeneralFun,
    prettyPrintTerm,
  )
where

import Control.DeepSeq (NFData (rnf))
import Data.Bits (Bits, FiniteBits)
import Data.Function (on)
import Data.Hashable (Hashable (hashWithSalt))
import Data.Interned
  ( Cache,
    Id,
    Interned (Description, Uninterned, cache, describe, identify),
  )
import Data.Kind (Constraint)
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Data.Typeable (Proxy (Proxy), cast)
import GHC.Generics (Generic)
import GHC.TypeNats (KnownNat, Nat, type (+), type (<=))
import Grisette.Core.Data.BV (IntN, WordN)
import Grisette.Core.Data.Class.BitVector
  ( SizedBV,
  )
import Grisette.Core.Data.Class.Function (Function ((#)))
import Grisette.Core.Data.Class.SignConversion (SignConversion)
import Grisette.Core.Data.Class.SymRotate (SymRotate)
import Grisette.Core.Data.Class.SymShift (SymShift)
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Caches
  ( typeMemoizedCache,
  )
import {-# SOURCE #-} Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
  ( absNumTerm,
    addNumTerm,
    andBitsTerm,
    andTerm,
    bvconcatTerm,
    bvextendTerm,
    bvselectTerm,
    complementBitsTerm,
    conTerm,
    constructBinary,
    constructTernary,
    constructUnary,
    divBoundedIntegralTerm,
    divIntegralTerm,
    eqvTerm,
    generalFunApplyTerm,
    iteTerm,
    leNumTerm,
    ltNumTerm,
    modBoundedIntegralTerm,
    modIntegralTerm,
    notTerm,
    orBitsTerm,
    orTerm,
    quotBoundedIntegralTerm,
    quotIntegralTerm,
    remBoundedIntegralTerm,
    remIntegralTerm,
    rotateLeftTerm,
    rotateRightTerm,
    shiftLeftTerm,
    shiftRightTerm,
    signumNumTerm,
    symTerm,
    tabularFunApplyTerm,
    timesNumTerm,
    toSignedTerm,
    toUnsignedTerm,
    uminusNumTerm,
    xorBitsTerm,
  )
import {-# SOURCE #-} Grisette.IR.SymPrim.Data.Prim.InternedTerm.TermSubstitution
  ( substTerm,
  )
import {-# SOURCE #-} Grisette.IR.SymPrim.Data.Prim.InternedTerm.TermUtils
  ( identity,
    introSupportedPrimConstraint,
    pformat,
  )
import Grisette.IR.SymPrim.Data.Prim.ModelValue
  ( ModelValue,
    toModelValue,
  )
import Grisette.IR.SymPrim.Data.Prim.Utils
  ( eqHeteroRep,
    eqTypeRepBool,
  )
import Grisette.IR.SymPrim.Data.TabularFun
  ( type (=->) (TabularFun),
  )
import Language.Haskell.TH.Syntax (Lift (lift, liftTyped))
import Language.Haskell.TH.Syntax.Compat (unTypeSplice)
import Type.Reflection
  ( SomeTypeRep (SomeTypeRep),
    TypeRep,
    Typeable,
    eqTypeRep,
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

-- deriving (Eq, Ord, Generic, Lift, NFData)

instance Eq (TypedSymbol t) where
  SimpleSymbol x == SimpleSymbol y = x == y
  IndexedSymbol x i == IndexedSymbol y j = i == j && x == y
  WithInfo s1 (i1 :: a) == WithInfo s2 (i2 :: b) = case eqTypeRep (typeRep @a) (typeRep @b) of
    Just HRefl -> i1 == i2 && s1 == s2
    _ -> False
  _ == _ = False

instance Ord (TypedSymbol t) where
  SimpleSymbol x <= SimpleSymbol y = x <= y
  IndexedSymbol x i <= IndexedSymbol y j = i < j || (i == j && x <= y)
  WithInfo s1 (i1 :: a) <= WithInfo s2 (i2 :: b) = case eqTypeRep (typeRep @a) (typeRep @b) of
    Just HRefl -> s1 < s2 || (s1 == s2 && i1 <= i2)
    _ -> False
  _ <= _ = False

instance Lift (TypedSymbol t) where
  liftTyped (SimpleSymbol x) = [||SimpleSymbol x||]
  liftTyped (IndexedSymbol x i) = [||IndexedSymbol x i||]
  liftTyped (WithInfo s1 i1) = [||WithInfo s1 i1||]

instance Show (TypedSymbol t) where
  show (SimpleSymbol str) = T.unpack str ++ " :: " ++ show (typeRep @t)
  show (IndexedSymbol str i) = T.unpack str ++ "@" ++ show i ++ " :: " ++ show (typeRep @t)
  show (WithInfo s info) = showUntyped s ++ ":" ++ show info ++ " :: " ++ show (typeRep @t)

showUntyped :: TypedSymbol t -> String
showUntyped (SimpleSymbol str) = T.unpack str
showUntyped (IndexedSymbol str i) = T.unpack str ++ "@" ++ show i
showUntyped (WithInfo s info) = showUntyped s ++ ":" ++ show info

instance Hashable (TypedSymbol t) where
  s `hashWithSalt` SimpleSymbol x = s `hashWithSalt` x
  s `hashWithSalt` IndexedSymbol x i = s `hashWithSalt` x `hashWithSalt` i
  s `hashWithSalt` WithInfo sym info = s `hashWithSalt` sym `hashWithSalt` info

instance NFData (TypedSymbol t) where
  rnf (SimpleSymbol str) = rnf str
  rnf (IndexedSymbol str i) = rnf str `seq` rnf i
  rnf (WithInfo s info) = rnf s `seq` rnf info

instance (SupportedPrim t) => IsString (TypedSymbol t) where
  fromString = SimpleSymbol . T.pack

withSymbolSupported :: TypedSymbol t -> ((SupportedPrim t) => a) -> a
withSymbolSupported (SimpleSymbol _) a = a
withSymbolSupported (IndexedSymbol _ _) a = a
withSymbolSupported (WithInfo _ _) a = a

{-
data TypedSymbol t where
  TypedSymbol :: (SupportedPrim t) => Symbol -> TypedSymbol t

typedSymbol :: forall proxy t. (SupportedPrim t) => proxy t -> Symbol -> TypedSymbol t
typedSymbol _ = TypedSymbol

instance NFData (TypedSymbol t) where
  rnf (TypedSymbol s) = rnf s

instance Eq (TypedSymbol t) where
  (TypedSymbol s1) == (TypedSymbol s2) = s1 == s2

instance Ord (TypedSymbol t) where
  (TypedSymbol s1) <= (TypedSymbol s2) = s1 <= s2

instance Hashable (TypedSymbol t) where
  hashWithSalt s (TypedSymbol s1) = s `hashWithSalt` s1

instance Show (TypedSymbol t) where
  show (TypedSymbol s) = show s ++ " :: " ++ show (typeRep @t)

instance Lift (TypedSymbol t) where
  liftTyped (TypedSymbol s) = [||TypedSymbol s||]
  -}

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
someTypedSymbol s@(SimpleSymbol _) = SomeTypedSymbol (typeRep @t) s
someTypedSymbol s@(IndexedSymbol _ _) = SomeTypedSymbol (typeRep @t) s
someTypedSymbol s@(WithInfo _ _) = SomeTypedSymbol (typeRep @t) s

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

instance NFData (Term a) where
  rnf i = identity i `seq` ()

instance Lift (Term t) where
  lift = unTypeSplice . liftTyped
  liftTyped (ConTerm _ i) = [||conTerm i||]
  liftTyped (SymTerm _ sym) = [||symTerm sym||]
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
  liftTyped (TabularFunApplyTerm _ func arg) = [||tabularFunApplyTerm func arg||]
  liftTyped (GeneralFunApplyTerm _ func arg) = [||generalFunApplyTerm func arg||]
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
  show (TabularFunApplyTerm i func arg) =
    "TabularFunApply{id=" ++ show i ++ ", func=" ++ show func ++ ", arg=" ++ show arg ++ "}"
  show (GeneralFunApplyTerm i func arg) =
    "GeneralFunApply{id=" ++ show i ++ ", func=" ++ show func ++ ", arg=" ++ show arg ++ "}"
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
    DTabularFunApplyTerm ::
      {-# UNPACK #-} !(TypeRep (a =-> b), Id) ->
      {-# UNPACK #-} !(TypeRep a, Id) ->
      Description (Term b)
    DGeneralFunApplyTerm ::
      {-# UNPACK #-} !(TypeRep (a --> b), Id) ->
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
  describe (UTabularFunApplyTerm (func :: Term f) (arg :: Term a)) =
    DTabularFunApplyTerm (typeRep :: TypeRep f, identity func) (typeRep :: TypeRep a, identity arg)
  describe (UGeneralFunApplyTerm (func :: Term f) (arg :: Term a)) =
    DGeneralFunApplyTerm (typeRep :: TypeRep f, identity func) (typeRep :: TypeRep a, identity arg)
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
      go (UTabularFunApplyTerm func arg) = TabularFunApplyTerm i func arg
      go (UGeneralFunApplyTerm func arg) = GeneralFunApplyTerm i func arg
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
  DTabularFunApplyTerm lf li == DTabularFunApplyTerm rf ri = eqTypedId lf rf && eqTypedId li ri
  DGeneralFunApplyTerm lf li == DGeneralFunApplyTerm rf ri = eqTypedId lf rf && eqTypedId li ri
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
  hashWithSalt s (DTabularFunApplyTerm id1 id2) = s `hashWithSalt` (28 :: Int) `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DGeneralFunApplyTerm id1 id2) = s `hashWithSalt` (29 :: Int) `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DDivIntegralTerm id1 id2) = s `hashWithSalt` (30 :: Int) `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DModIntegralTerm id1 id2) = s `hashWithSalt` (31 :: Int) `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DQuotIntegralTerm id1 id2) = s `hashWithSalt` (32 :: Int) `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DRemIntegralTerm id1 id2) = s `hashWithSalt` (33 :: Int) `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DDivBoundedIntegralTerm id1 id2) = s `hashWithSalt` (34 :: Int) `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DModBoundedIntegralTerm id1 id2) = s `hashWithSalt` (35 :: Int) `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DQuotBoundedIntegralTerm id1 id2) = s `hashWithSalt` (36 :: Int) `hashWithSalt` id1 `hashWithSalt` id2
  hashWithSalt s (DRemBoundedIntegralTerm id1 id2) = s `hashWithSalt` (37 :: Int) `hashWithSalt` id1 `hashWithSalt` id2

-- Basic Bool
defaultValueForBool :: Bool
defaultValueForBool = False

defaultValueForBoolDyn :: ModelValue
defaultValueForBoolDyn = toModelValue defaultValueForBool

instance SupportedPrim Bool where
  pformatCon True = "true"
  pformatCon False = "false"
  defaultValue = defaultValueForBool
  defaultValueDynamic _ = defaultValueForBoolDyn

defaultValueForInteger :: Integer
defaultValueForInteger = 0

defaultValueForIntegerDyn :: ModelValue
defaultValueForIntegerDyn = toModelValue defaultValueForInteger

-- Basic Integer
instance SupportedPrim Integer where
  pformatCon = show
  defaultValue = defaultValueForInteger
  defaultValueDynamic _ = defaultValueForIntegerDyn

-- Signed BV
instance (KnownNat w, 1 <= w) => SupportedPrim (IntN w) where
  type PrimConstraint (IntN w) = (KnownNat w, 1 <= w)
  pformatCon = show
  defaultValue = 0

-- Unsigned BV
instance (KnownNat w, 1 <= w) => SupportedPrim (WordN w) where
  type PrimConstraint (WordN w) = (KnownNat w, 1 <= w)
  pformatCon = show
  defaultValue = 0

-- | General symbolic function type. Use the '#' operator to apply the function.
-- Note that this function should be applied to symbolic values only. It is by
-- itself already a symbolic value, but can be considered partially concrete
-- as the function body is specified. Use 'Grisette.IR.SymPrim.Data.SymPrim.-~>' for uninterpreted general
-- symbolic functions.
--
-- The result would be partially evaluated.
--
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeOperators
-- >>> let f = ("x" :: TypedSymbol Integer) --> ("x" + 1 + "y" :: SymInteger) :: Integer --> Integer
-- >>> f # 1    -- 1 has the type SymInteger
-- (+ 2 y)
-- >>> f # "a"  -- "a" has the type SymInteger
-- (+ 1 (+ a y))
data (-->) a b where
  GeneralFun :: (SupportedPrim a, SupportedPrim b) => TypedSymbol a -> Term b -> a --> b

instance (LinkedRep a sa, LinkedRep b sb) => Function (a --> b) sa sb where
  (GeneralFun s t) # x = wrapTerm $ substTerm s (underlyingTerm x) t

{-
pattern GeneralFun :: () => (SupportedPrim a, SupportedPrim b) => TypedSymbol a -> Term b -> a --> b
pattern GeneralFun arg v <- GeneralFun arg v

{-# COMPLETE GeneralFun #-}
-}

infixr 0 -->

buildGeneralFun :: () => (SupportedPrim a, SupportedPrim b) => TypedSymbol a -> Term b -> a --> b
buildGeneralFun arg v = GeneralFun newarg (substTerm arg (symTerm newarg) v)
  where
    newarg = WithInfo arg ARG

data ARG = ARG
  deriving (Eq, Ord, Lift, Show, Generic)

instance NFData ARG where
  rnf ARG = ()

instance Hashable ARG where
  hashWithSalt s ARG = s `hashWithSalt` (0 :: Int)

instance Eq (a --> b) where
  GeneralFun sym1 tm1 == GeneralFun sym2 tm2 = sym1 == sym2 && tm1 == tm2

instance Show (a --> b) where
  show (GeneralFun sym tm) = "\\(" ++ show sym ++ ") -> " ++ pformat tm

instance Lift (a --> b) where
  liftTyped (GeneralFun sym tm) = [||GeneralFun sym tm||]

instance Hashable (a --> b) where
  s `hashWithSalt` (GeneralFun sym tm) = s `hashWithSalt` sym `hashWithSalt` tm

instance NFData (a --> b) where
  rnf (GeneralFun sym tm) = rnf sym `seq` rnf tm

instance (SupportedPrim a, SupportedPrim b) => SupportedPrim (a --> b) where
  type PrimConstraint (a --> b) = (SupportedPrim a, SupportedPrim b)
  defaultValue = buildGeneralFun (SimpleSymbol "a") (conTerm defaultValue)

instance
  (SupportedPrim a, SupportedPrim b) =>
  SupportedPrim (a =-> b)
  where
  type PrimConstraint (a =-> b) = (SupportedPrim a, SupportedPrim b)
  defaultValue = TabularFun [] (defaultValue @b)
