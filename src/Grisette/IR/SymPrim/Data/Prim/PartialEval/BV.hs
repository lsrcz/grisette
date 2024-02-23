{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
import Grisette.Core.Data.Class.SignConversion
  ( SignConversion (toSigned, toUnsigned),
  )
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
    Term (BVSelectTerm, ConTerm, ToSignedTerm, ToUnsignedTerm),
  )
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.TermUtils
  ( castTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Unfold
  ( binaryUnfoldOnce,
    unaryUnfoldOnce,
  )
import Grisette.Utils.Parameterized
  ( LeqProof (LeqProof),
    NatRepr,
    addNat,
    natRepr,
    unsafeLeqProof,
    withKnownNat,
  )

-- ToSigned
pevalToSignedTerm ::
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
pevalToSignedTerm = unaryUnfoldOnce doPevalToSignedTerm toSignedTerm

doPevalToSignedTerm ::
  ( forall n. (KnownNat n, 1 <= n) => SupportedPrim (u n),
    forall n. (KnownNat n, 1 <= n) => SupportedPrim (s n),
    forall n. (KnownNat n, 1 <= n) => SignConversion (u n) (s n),
    Typeable u,
    Typeable s,
    KnownNat n,
    1 <= n,
    SizedBV u,
    SizedBV s
  ) =>
  Term (u n) ->
  Maybe (Term (s n))
doPevalToSignedTerm (ConTerm _ b) = Just $ conTerm $ toSigned b
doPevalToSignedTerm (ToUnsignedTerm _ b) = Just b >>= castTerm
doPevalToSignedTerm _ = Nothing

-- ToUnsigned
pevalToUnsignedTerm ::
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
pevalToUnsignedTerm = unaryUnfoldOnce doPevalToUnsignedTerm toUnsignedTerm

doPevalToUnsignedTerm ::
  ( forall n. (KnownNat n, 1 <= n) => SupportedPrim (u n),
    forall n. (KnownNat n, 1 <= n) => SupportedPrim (s n),
    Typeable u,
    Typeable s,
    KnownNat n,
    1 <= n,
    SignConversion (u n) (s n),
    SizedBV u,
    SizedBV s
  ) =>
  Term (s n) ->
  Maybe (Term (u n))
doPevalToUnsignedTerm (ConTerm _ b) = Just $ conTerm $ toUnsigned b
doPevalToUnsignedTerm (ToSignedTerm _ b) = Just b >>= castTerm
doPevalToUnsignedTerm _ = Nothing

data BVSelectWitness n ix w where
  BVSelectWitness ::
    (KnownNat n, 1 <= n, KnownNat w, 1 <= w, KnownNat ix, ix + w <= n) =>
    BVSelectWitness n ix w

unsafeBVSelectWitness ::
  forall n ix w. NatRepr n -> NatRepr ix -> NatRepr w -> BVSelectWitness n ix w
unsafeBVSelectWitness n ix w =
  withKnownNat n $
    withKnownNat ix $
      withKnownNat w $
        case ( unsafeLeqProof @1 @n,
               unsafeLeqProof @1 @w,
               unsafeLeqProof @(ix + w) @n
             ) of
          (LeqProof, LeqProof, LeqProof) -> BVSelectWitness

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
pevalBVSelectTerm ix w =
  unaryUnfoldOnce (doPevalBVSelectTerm ix w) (bvselectTerm ix w)

unsafePevalBVSelectTerm ::
  forall bv n ix w.
  ( forall n. (KnownNat n, 1 <= n) => SupportedPrim (bv n),
    Typeable bv,
    SizedBV bv
  ) =>
  NatRepr n ->
  NatRepr ix ->
  NatRepr w ->
  Term (bv n) ->
  Term (bv w)
unsafePevalBVSelectTerm n ix w term =
  case unsafeBVSelectWitness n ix w of
    BVSelectWitness -> pevalBVSelectTerm ix w term

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
doPevalBVSelectTerm ix w (ToSignedTerm _ b) =
  Just $ pevalToSignedTerm $ pevalBVSelectTerm ix w b
doPevalBVSelectTerm ix w (ToUnsignedTerm _ b) =
  Just $ pevalToUnsignedTerm $ pevalBVSelectTerm ix w b
doPevalBVSelectTerm
  _
  _
  (BVSelectTerm _ (_ :: proxy ix1) _ (b :: Term (bv n1))) =
    Just $
      unsafePevalBVSelectTerm
        (natRepr @n1)
        (addNat (natRepr @ix) (natRepr @ix1))
        (natRepr @w)
        b
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
