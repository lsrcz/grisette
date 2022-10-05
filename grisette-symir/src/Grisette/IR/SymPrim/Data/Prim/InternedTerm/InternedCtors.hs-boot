{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
  ( constructUnary,
    constructBinary,
    constructTernary,
    concTerm,
    symbTerm,
    ssymbTerm,
    isymbTerm,
    sinfosymbTerm,
    iinfosymbTerm,
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
    shiftBitsTerm,
    rotateBitsTerm,
    bvconcatTerm,
    bvselectTerm,
    bvextendTerm,
    bvsignExtendTerm,
    bvzeroExtendTerm,
    tabularFuncApplyTerm,
    generalFuncApplyTerm,
    divIntegerTerm,
    modIntegerTerm,
  )
where

import Control.DeepSeq
import Data.Bits
import Data.Hashable
import Data.Typeable
import GHC.TypeNats
import Grisette.Core.Data.Class.BitVector
import {-# SOURCE #-} Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import {-# SOURCE #-} Grisette.IR.SymPrim.Data.TabularFunc
import Language.Haskell.TH.Syntax

constructUnary ::
  forall tag arg t.
  (SupportedPrim t, UnaryOp tag arg t, Typeable tag, Typeable t, Show tag) =>
  tag ->
  Term arg ->
  Term t
constructBinary ::
  forall tag arg1 arg2 t.
  (SupportedPrim t, BinaryOp tag arg1 arg2 t, Typeable tag, Typeable t, Show tag) =>
  tag ->
  Term arg1 ->
  Term arg2 ->
  Term t
constructTernary ::
  forall tag arg1 arg2 arg3 t.
  (SupportedPrim t, TernaryOp tag arg1 arg2 arg3 t, Typeable tag, Typeable t, Show tag) =>
  tag ->
  Term arg1 ->
  Term arg2 ->
  Term arg3 ->
  Term t
concTerm :: (SupportedPrim t, Typeable t, Hashable t, Eq t, Show t) => t -> Term t
symbTerm :: forall t. (SupportedPrim t, Typeable t) => Symbol -> Term t
ssymbTerm :: (SupportedPrim t, Typeable t) => String -> Term t
isymbTerm :: (SupportedPrim t, Typeable t) => String -> Int -> Term t
sinfosymbTerm ::
  (SupportedPrim t, Typeable t, Typeable a, Ord a, Lift a, NFData a, Show a, Hashable a) =>
  String ->
  a ->
  Term t
iinfosymbTerm ::
  (SupportedPrim t, Typeable t, Typeable a, Ord a, Lift a, NFData a, Show a, Hashable a) =>
  String ->
  Int ->
  a ->
  Term t
notTerm :: Term Bool -> Term Bool
orTerm :: Term Bool -> Term Bool -> Term Bool
andTerm :: Term Bool -> Term Bool -> Term Bool
eqvTerm :: SupportedPrim a => Term a -> Term a -> Term Bool
iteTerm :: SupportedPrim a => Term Bool -> Term a -> Term a -> Term a
addNumTerm :: (SupportedPrim a, Num a) => Term a -> Term a -> Term a
uminusNumTerm :: (SupportedPrim a, Num a) => Term a -> Term a
timesNumTerm :: (SupportedPrim a, Num a) => Term a -> Term a -> Term a
absNumTerm :: (SupportedPrim a, Num a) => Term a -> Term a
signumNumTerm :: (SupportedPrim a, Num a) => Term a -> Term a
ltNumTerm :: (SupportedPrim a, Num a, Ord a) => Term a -> Term a -> Term Bool
leNumTerm :: (SupportedPrim a, Num a, Ord a) => Term a -> Term a -> Term Bool
andBitsTerm :: (SupportedPrim a, Bits a) => Term a -> Term a -> Term a
orBitsTerm :: (SupportedPrim a, Bits a) => Term a -> Term a -> Term a
xorBitsTerm :: (SupportedPrim a, Bits a) => Term a -> Term a -> Term a
complementBitsTerm :: (SupportedPrim a, Bits a) => Term a -> Term a
shiftBitsTerm :: (SupportedPrim a, Bits a) => Term a -> Int -> Term a
rotateBitsTerm :: (SupportedPrim a, Bits a) => Term a -> Int -> Term a
bvconcatTerm ::
  ( SupportedPrim (bv a),
    SupportedPrim (bv b),
    SupportedPrim (bv c),
    KnownNat a,
    KnownNat b,
    KnownNat c,
    BVConcat (bv a) (bv b) (bv c)
  ) =>
  Term (bv a) ->
  Term (bv b) ->
  Term (bv c)
bvselectTerm ::
  forall bv a ix w proxy.
  ( SupportedPrim (bv a),
    SupportedPrim (bv w),
    KnownNat a,
    KnownNat w,
    KnownNat ix,
    BVSelect (bv a) ix w (bv w)
  ) =>
  proxy ix ->
  proxy w ->
  Term (bv a) ->
  Term (bv w)
bvextendTerm ::
  forall bv a n w proxy.
  ( SupportedPrim (bv a),
    SupportedPrim (bv w),
    KnownNat a,
    KnownNat n,
    KnownNat w,
    BVExtend (bv a) n (bv w)
  ) =>
  Bool ->
  proxy n ->
  Term (bv a) ->
  Term (bv w)
bvsignExtendTerm ::
  forall bv a n w proxy.
  ( SupportedPrim (bv a),
    SupportedPrim (bv w),
    KnownNat a,
    KnownNat n,
    KnownNat w,
    BVExtend (bv a) n (bv w)
  ) =>
  proxy n ->
  Term (bv a) ->
  Term (bv w)
bvzeroExtendTerm ::
  forall bv a n w proxy.
  ( SupportedPrim (bv a),
    SupportedPrim (bv w),
    KnownNat a,
    KnownNat n,
    KnownNat w,
    BVExtend (bv a) n (bv w)
  ) =>
  proxy n ->
  Term (bv a) ->
  Term (bv w)
tabularFuncApplyTerm :: (SupportedPrim a, SupportedPrim b) => Term (a =-> b) -> Term a -> Term b
generalFuncApplyTerm :: (SupportedPrim a, SupportedPrim b) => Term (a --> b) -> Term a -> Term b
divIntegerTerm :: Term Integer -> Term Integer -> Term Integer
modIntegerTerm :: Term Integer -> Term Integer -> Term Integer
