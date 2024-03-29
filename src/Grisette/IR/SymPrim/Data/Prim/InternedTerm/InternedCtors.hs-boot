{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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
import Data.Bits (Bits, FiniteBits)
import Data.Hashable (Hashable)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import GHC.TypeNats (KnownNat, type (+), type (<=))
import Grisette.Core.Data.Class.BitVector
  ( SizedBV,
  )
import Grisette.Core.Data.Class.SignConversion (SignConversion)
import Grisette.Core.Data.Class.SymRotate (SymRotate)
import Grisette.Core.Data.Class.SymShift (SymShift)
import {-# SOURCE #-} Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
  ( BinaryOp,
    SupportedPrim,
    Term,
    TernaryOp,
    TypedSymbol,
    UnaryOp,
    type (-->),
  )
import Grisette.IR.SymPrim.Data.TabularFun
  ( type (=->),
  )
import Language.Haskell.TH.Syntax (Lift)

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
conTerm :: (SupportedPrim t, Typeable t, Hashable t, Eq t, Show t) => t -> Term t
symTerm :: (SupportedPrim t, Typeable t) => TypedSymbol t -> Term t
ssymTerm :: (SupportedPrim t, Typeable t) => T.Text -> Term t
isymTerm :: (SupportedPrim t, Typeable t) => T.Text -> Int -> Term t
sinfosymTerm ::
  (SupportedPrim t, Typeable t, Typeable a, Ord a, Lift a, NFData a, Show a, Hashable a) =>
  T.Text ->
  a ->
  Term t
iinfosymTerm ::
  (SupportedPrim t, Typeable t, Typeable a, Ord a, Lift a, NFData a, Show a, Hashable a) =>
  T.Text ->
  Int ->
  a ->
  Term t
notTerm :: Term Bool -> Term Bool
orTerm :: Term Bool -> Term Bool -> Term Bool
andTerm :: Term Bool -> Term Bool -> Term Bool
eqvTerm :: (SupportedPrim a) => Term a -> Term a -> Term Bool
iteTerm :: (SupportedPrim a) => Term Bool -> Term a -> Term a -> Term a
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
shiftLeftTerm :: (SupportedPrim a, Integral a, FiniteBits a, SymShift a) => Term a -> Term a -> Term a
shiftRightTerm :: (SupportedPrim a, Integral a, FiniteBits a, SymShift a) => Term a -> Term a -> Term a
rotateLeftTerm :: (SupportedPrim a, Integral a, FiniteBits a, SymRotate a) => Term a -> Term a -> Term a
rotateRightTerm :: (SupportedPrim a, Integral a, FiniteBits a, SymRotate a) => Term a -> Term a -> Term a
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
tabularFunApplyTerm :: (SupportedPrim a, SupportedPrim b) => Term (a =-> b) -> Term a -> Term b
generalFunApplyTerm :: (SupportedPrim a, SupportedPrim b) => Term (a --> b) -> Term a -> Term b
divIntegralTerm :: (SupportedPrim a, Integral a) => Term a -> Term a -> Term a
modIntegralTerm :: (SupportedPrim a, Integral a) => Term a -> Term a -> Term a
quotIntegralTerm :: (SupportedPrim a, Integral a) => Term a -> Term a -> Term a
remIntegralTerm :: (SupportedPrim a, Integral a) => Term a -> Term a -> Term a
divBoundedIntegralTerm :: (SupportedPrim a, Bounded a, Integral a) => Term a -> Term a -> Term a
modBoundedIntegralTerm :: (SupportedPrim a, Bounded a, Integral a) => Term a -> Term a -> Term a
quotBoundedIntegralTerm :: (SupportedPrim a, Bounded a, Integral a) => Term a -> Term a -> Term a
remBoundedIntegralTerm :: (SupportedPrim a, Bounded a, Integral a) => Term a -> Term a -> Term a
