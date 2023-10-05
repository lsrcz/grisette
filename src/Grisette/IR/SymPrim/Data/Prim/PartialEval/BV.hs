{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.Prim.PartialEval.BV
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.Prim.PartialEval.BV
  ( pevalToSignedTerm,
    pevalToUnsignedTerm,
    pevalBVConcatTerm,
    pevalBVSelectTerm,
    pevalBVExtendTerm,
    pevalBVZeroExtendTerm,
    pevalBVSignExtendTerm,
  )
where

import Data.Typeable (Typeable)
import GHC.TypeNats (KnownNat, type (+), type (<=))
import Grisette.Core.Data.Class.BitVector
  ( SizedBV (sizedBVConcat, sizedBVSelect, sizedBVSext, sizedBVZext),
  )
import Grisette.Core.Data.Class.SignConversion (SignConversion (toSigned, toUnsigned))
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
  ( bvconcatTerm,
    bvextendTerm,
    bvselectTerm,
    conTerm,
    toSignedTerm,
    toUnsignedTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
  ( SupportedPrim,
    Term (ConTerm, ToSignedTerm, ToUnsignedTerm),
  )
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.TermUtils
  ( castTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Unfold
  ( binaryUnfoldOnce,
    unaryUnfoldOnce,
  )

-- ToSigned
pevalToSignedTerm ::
  ( SupportedPrim u,
    SupportedPrim s,
    SignConversion u s
  ) =>
  Term u ->
  Term s
pevalToSignedTerm = unaryUnfoldOnce doPevalToSignedTerm toSignedTerm

doPevalToSignedTerm ::
  ( SupportedPrim u,
    SupportedPrim s,
    SignConversion u s
  ) =>
  Term u ->
  Maybe (Term s)
doPevalToSignedTerm (ConTerm _ b) = Just $ conTerm $ toSigned b
doPevalToSignedTerm (ToUnsignedTerm _ b) = Just b >>= castTerm
doPevalToSignedTerm _ = Nothing

-- ToUnsigned
pevalToUnsignedTerm ::
  ( SupportedPrim u,
    SupportedPrim s,
    SignConversion u s
  ) =>
  Term s ->
  Term u
pevalToUnsignedTerm = unaryUnfoldOnce doPevalToUnsignedTerm toUnsignedTerm

doPevalToUnsignedTerm ::
  ( SupportedPrim u,
    SupportedPrim s,
    SignConversion u s
  ) =>
  Term s ->
  Maybe (Term u)
doPevalToUnsignedTerm (ConTerm _ b) = Just $ conTerm $ toUnsigned b
doPevalToUnsignedTerm (ToSignedTerm _ b) = Just b >>= castTerm
doPevalToUnsignedTerm _ = Nothing

-- select
pevalBVSelectTerm ::
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
pevalBVSelectTerm ix w = unaryUnfoldOnce (doPevalBVSelectTerm ix w) (bvselectTerm ix w)

doPevalBVSelectTerm ::
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
  Maybe (Term (bv w))
doPevalBVSelectTerm ix w (ConTerm _ b) = Just $ conTerm $ sizedBVSelect ix w b
doPevalBVSelectTerm _ _ _ = Nothing

-- ext
pevalBVZeroExtendTerm ::
  forall proxy l r bv.
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
pevalBVZeroExtendTerm = pevalBVExtendTerm False

pevalBVSignExtendTerm ::
  forall proxy l r bv.
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
pevalBVSignExtendTerm = pevalBVExtendTerm True

pevalBVExtendTerm ::
  forall proxy l r bv.
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
pevalBVExtendTerm signed p = unaryUnfoldOnce (doPevalBVExtendTerm signed p) (bvextendTerm signed p)

doPevalBVExtendTerm ::
  forall proxy l r bv.
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
  Maybe (Term (bv r))
doPevalBVExtendTerm signed p (ConTerm _ b) = Just $ conTerm $ if signed then sizedBVSext p b else sizedBVZext p b
doPevalBVExtendTerm _ _ _ = Nothing

pevalBVConcatTerm ::
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
pevalBVConcatTerm = binaryUnfoldOnce doPevalBVConcatTerm bvconcatTerm

doPevalBVConcatTerm ::
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
  Term (bv a) ->
  Term (bv b) ->
  Maybe (Term (bv (a + b)))
doPevalBVConcatTerm (ConTerm _ v) (ConTerm _ v') = Just $ conTerm $ sizedBVConcat v v'
doPevalBVConcatTerm _ _ = Nothing
