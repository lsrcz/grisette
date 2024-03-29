{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
  ( constructUnary,
    constructBinary,
    constructTernary,
    conTerm,
    symTerm,
    ssymTerm,
    isymTerm,
    sinfosymTerm,
    iinfosymTerm,
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
    tabularFunApplyTerm,
    generalFunApplyTerm,
    divIntegralTerm,
    modIntegralTerm,
    quotIntegralTerm,
    remIntegralTerm,
    divBoundedIntegralTerm,
    modBoundedIntegralTerm,
    quotBoundedIntegralTerm,
    remBoundedIntegralTerm,
  )
where

import Control.DeepSeq (NFData)
import Data.Array ((!))
import Data.Bits (Bits, FiniteBits)
import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable (hash))
import Data.IORef (atomicModifyIORef')
import Data.Interned
  ( Interned (Uninterned, cache, cacheWidth, describe, identify),
  )
import Data.Interned.Internal
  ( Cache (getCache),
    CacheState (CacheState),
  )
import qualified Data.Text as T
import GHC.IO (unsafeDupablePerformIO)
import GHC.TypeNats (KnownNat, type (+), type (<=))
import Grisette.Core.Data.Class.BitVector
  ( SizedBV,
  )
import Grisette.Core.Data.Class.SignConversion (SignConversion)
import Grisette.Core.Data.Class.SymRotate (SymRotate)
import Grisette.Core.Data.Class.SymShift (SymShift)
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
  ( BinaryOp,
    SupportedPrim,
    Term,
    TernaryOp,
    TypedSymbol (IndexedSymbol, SimpleSymbol, WithInfo),
    UTerm
      ( UAbsNumTerm,
        UAddNumTerm,
        UAndBitsTerm,
        UAndTerm,
        UBVConcatTerm,
        UBVExtendTerm,
        UBVSelectTerm,
        UBinaryTerm,
        UComplementBitsTerm,
        UConTerm,
        UDivBoundedIntegralTerm,
        UDivIntegralTerm,
        UEqvTerm,
        UGeneralFunApplyTerm,
        UITETerm,
        ULENumTerm,
        ULTNumTerm,
        UModBoundedIntegralTerm,
        UModIntegralTerm,
        UNotTerm,
        UOrBitsTerm,
        UOrTerm,
        UQuotBoundedIntegralTerm,
        UQuotIntegralTerm,
        URemBoundedIntegralTerm,
        URemIntegralTerm,
        URotateLeftTerm,
        URotateRightTerm,
        UShiftLeftTerm,
        UShiftRightTerm,
        USignumNumTerm,
        USymTerm,
        UTabularFunApplyTerm,
        UTernaryTerm,
        UTimesNumTerm,
        UToSignedTerm,
        UToUnsignedTerm,
        UUMinusNumTerm,
        UUnaryTerm,
        UXorBitsTerm
      ),
    UnaryOp,
    type (-->),
  )
import Grisette.IR.SymPrim.Data.TabularFun
  ( type (=->),
  )
import Language.Haskell.TH.Syntax (Lift)
import Type.Reflection (Typeable, typeRep)

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

symTerm :: forall t. (SupportedPrim t, Typeable t) => TypedSymbol t -> Term t
symTerm t = internTerm $ USymTerm t
{-# INLINE symTerm #-}

ssymTerm :: (SupportedPrim t, Typeable t) => T.Text -> Term t
ssymTerm = symTerm . SimpleSymbol
{-# INLINE ssymTerm #-}

isymTerm :: (SupportedPrim t, Typeable t) => T.Text -> Int -> Term t
isymTerm str idx = symTerm $ IndexedSymbol str idx
{-# INLINE isymTerm #-}

sinfosymTerm ::
  (SupportedPrim t, Typeable t, Typeable a, Ord a, Lift a, NFData a, Show a, Hashable a) =>
  T.Text ->
  a ->
  Term t
sinfosymTerm s info = symTerm $ WithInfo (SimpleSymbol s) info
{-# INLINE sinfosymTerm #-}

iinfosymTerm ::
  (SupportedPrim t, Typeable t, Typeable a, Ord a, Lift a, NFData a, Show a, Hashable a) =>
  T.Text ->
  Int ->
  a ->
  Term t
iinfosymTerm str idx info = symTerm $ WithInfo (IndexedSymbol str idx) info
{-# INLINE iinfosymTerm #-}

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

andBitsTerm :: (SupportedPrim a, Bits a) => Term a -> Term a -> Term a
andBitsTerm l r = internTerm $ UAndBitsTerm l r
{-# INLINE andBitsTerm #-}

orBitsTerm :: (SupportedPrim a, Bits a) => Term a -> Term a -> Term a
orBitsTerm l r = internTerm $ UOrBitsTerm l r
{-# INLINE orBitsTerm #-}

xorBitsTerm :: (SupportedPrim a, Bits a) => Term a -> Term a -> Term a
xorBitsTerm l r = internTerm $ UXorBitsTerm l r
{-# INLINE xorBitsTerm #-}

complementBitsTerm :: (SupportedPrim a, Bits a) => Term a -> Term a
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

tabularFunApplyTerm :: (SupportedPrim a, SupportedPrim b) => Term (a =-> b) -> Term a -> Term b
tabularFunApplyTerm f a = internTerm $ UTabularFunApplyTerm f a
{-# INLINE tabularFunApplyTerm #-}

generalFunApplyTerm :: (SupportedPrim a, SupportedPrim b) => Term (a --> b) -> Term a -> Term b
generalFunApplyTerm f a = internTerm $ UGeneralFunApplyTerm f a
{-# INLINE generalFunApplyTerm #-}

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
