{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
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
  ( pevalBVConcatTerm,
    pevalBVSelectTerm,
    pevalBVExtendTerm,
    pevalBVZeroExtendTerm,
    pevalBVSignExtendTerm,
  )
where

import GHC.TypeNats
import Grisette.Core.Data.Class.BitVector
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Unfold

-- select
pevalBVSelectTerm ::
  forall bv n ix w proxy.
  ( SupportedPrim (bv n),
    SupportedPrim (bv w),
    KnownNat n,
    KnownNat ix,
    KnownNat w,
    1 <= n,
    1 <= w,
    ix + w <= n,
    SizedBV bv
  ) =>
  proxy ix ->
  proxy w ->
  Term (bv n) ->
  Term (bv w)
pevalBVSelectTerm ix w = unaryUnfoldOnce (doPevalBVSelectTerm ix w) (bvselectTerm ix w)

doPevalBVSelectTerm ::
  forall bv n ix w proxy.
  ( SupportedPrim (bv n),
    SupportedPrim (bv w),
    KnownNat n,
    KnownNat ix,
    KnownNat w,
    1 <= n,
    1 <= w,
    ix + w <= n,
    SizedBV bv
  ) =>
  proxy ix ->
  proxy w ->
  Term (bv n) ->
  Maybe (Term (bv w))
doPevalBVSelectTerm ix w (ConTerm _ b) = Just $ conTerm $ selectSizedBV ix w b
doPevalBVSelectTerm _ _ _ = Nothing

-- ext
pevalBVZeroExtendTerm ::
  forall proxy l r bv.
  ( KnownNat l,
    KnownNat r,
    1 <= l,
    l <= r,
    SupportedPrim (bv l),
    SupportedPrim (bv r),
    SizedBV bv
  ) =>
  proxy r ->
  Term (bv l) ->
  Term (bv r)
pevalBVZeroExtendTerm = pevalBVExtendTerm False

pevalBVSignExtendTerm ::
  forall proxy l r bv.
  ( KnownNat l,
    KnownNat r,
    1 <= l,
    l <= r,
    SupportedPrim (bv l),
    SupportedPrim (bv r),
    SizedBV bv
  ) =>
  proxy r ->
  Term (bv l) ->
  Term (bv r)
pevalBVSignExtendTerm = pevalBVExtendTerm True

pevalBVExtendTerm ::
  forall proxy l r bv.
  ( KnownNat l,
    KnownNat r,
    1 <= l,
    l <= r,
    SupportedPrim (bv l),
    SupportedPrim (bv r),
    SizedBV bv
  ) =>
  Bool ->
  proxy r ->
  Term (bv l) ->
  Term (bv r)
pevalBVExtendTerm signed p = unaryUnfoldOnce (doPevalBVExtendTerm signed p) (bvextendTerm signed p)

doPevalBVExtendTerm ::
  forall proxy l r bv.
  ( KnownNat l,
    KnownNat r,
    1 <= l,
    l <= r,
    SupportedPrim (bv l),
    SupportedPrim (bv r),
    SizedBV bv
  ) =>
  Bool ->
  proxy r ->
  Term (bv l) ->
  Maybe (Term (bv r))
doPevalBVExtendTerm signed p (ConTerm _ b) = Just $ conTerm $ if signed then sextSizedBV p b else zextSizedBV p b
doPevalBVExtendTerm _ _ _ = Nothing

pevalBVConcatTerm ::
  ( SupportedPrim (bv a),
    SupportedPrim (bv b),
    SupportedPrim (bv (a + b)),
    KnownNat a,
    KnownNat b,
    1 <= a,
    1 <= b,
    SizedBV bv
  ) =>
  Term (bv a) ->
  Term (bv b) ->
  Term (bv (a + b))
pevalBVConcatTerm = binaryUnfoldOnce doPevalBVConcatTerm bvconcatTerm

doPevalBVConcatTerm ::
  ( SupportedPrim (bv a),
    SupportedPrim (bv b),
    SupportedPrim (bv (a + b)),
    KnownNat a,
    KnownNat b,
    1 <= a,
    1 <= b,
    SizedBV bv
  ) =>
  Term (bv a) ->
  Term (bv b) ->
  Maybe (Term (bv (a + b)))
doPevalBVConcatTerm (ConTerm _ v) (ConTerm _ v') = Just $ conTerm $ concatSizedBV v v'
doPevalBVConcatTerm _ _ = Nothing
