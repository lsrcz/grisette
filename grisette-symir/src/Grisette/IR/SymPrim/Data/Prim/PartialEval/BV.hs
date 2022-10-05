{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

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
pevalBVSelectTerm ix w = unaryUnfoldOnce (doPevalBVSelectTerm ix w) (bvselectTerm ix w)

doPevalBVSelectTerm ::
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
  Maybe (Term (bv w))
doPevalBVSelectTerm ix w (ConcTerm _ b) = Just $ concTerm $ bvselect ix w b
doPevalBVSelectTerm _ _ _ = Nothing

-- ext
pevalBVZeroExtendTerm ::
  forall proxy a n b bv.
  ( KnownNat a,
    KnownNat b,
    KnownNat n,
    BVExtend (bv a) n (bv b),
    SupportedPrim (bv a),
    SupportedPrim (bv b)
  ) =>
  proxy n ->
  Term (bv a) ->
  Term (bv b)
pevalBVZeroExtendTerm = pevalBVExtendTerm False

pevalBVSignExtendTerm ::
  forall proxy a n b bv.
  ( KnownNat a,
    KnownNat b,
    KnownNat n,
    BVExtend (bv a) n (bv b),
    SupportedPrim (bv a),
    SupportedPrim (bv b)
  ) =>
  proxy n ->
  Term (bv a) ->
  Term (bv b)
pevalBVSignExtendTerm = pevalBVExtendTerm True

pevalBVExtendTerm ::
  forall proxy a n b bv.
  ( KnownNat a,
    KnownNat b,
    KnownNat n,
    BVExtend (bv a) n (bv b),
    SupportedPrim (bv a),
    SupportedPrim (bv b)
  ) =>
  Bool ->
  proxy n ->
  Term (bv a) ->
  Term (bv b)
pevalBVExtendTerm signed p = unaryUnfoldOnce (doPevalBVExtendTerm signed p) (bvextendTerm signed p)

doPevalBVExtendTerm ::
  forall proxy a n b bv.
  ( KnownNat a,
    KnownNat b,
    KnownNat n,
    BVExtend (bv a) n (bv b),
    SupportedPrim (bv a),
    SupportedPrim (bv b)
  ) =>
  Bool ->
  proxy n ->
  Term (bv a) ->
  Maybe (Term (bv b))
doPevalBVExtendTerm signed p (ConcTerm _ b) = Just $ concTerm $ if signed then bvsignExtend p b else bvzeroExtend p b
doPevalBVExtendTerm _ _ _ = Nothing

pevalBVConcatTerm ::
  ( SupportedPrim (s w),
    SupportedPrim (s w'),
    SupportedPrim (s w''),
    KnownNat w,
    KnownNat w',
    KnownNat w'',
    BVConcat (s w) (s w') (s w'')
  ) =>
  Term (s w) ->
  Term (s w') ->
  Term (s w'')
pevalBVConcatTerm = binaryUnfoldOnce doPevalBVConcatTerm bvconcatTerm

doPevalBVConcatTerm ::
  ( SupportedPrim (s w),
    SupportedPrim (s w'),
    SupportedPrim (s w''),
    KnownNat w,
    KnownNat w',
    KnownNat w'',
    BVConcat (s w) (s w') (s w'')
  ) =>
  Term (s w) ->
  Term (s w') ->
  Maybe (Term (s w''))
doPevalBVConcatTerm (ConcTerm _ v) (ConcTerm _ v') = Just $ concTerm $ bvconcat v v'
doPevalBVConcatTerm _ _ = Nothing
