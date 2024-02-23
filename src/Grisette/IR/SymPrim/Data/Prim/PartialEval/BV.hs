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

import Data.Maybe (isJust)
import Data.Typeable (Proxy (Proxy), Typeable, (:~:) (Refl))
import GHC.TypeNats (KnownNat, natVal, sameNat, type (+), type (<=))
import Grisette.Core.Data.Class.BitVector
  ( SizedBV
      ( sizedBVConcat,
        sizedBVFromIntegral,
        sizedBVSelect,
        sizedBVSext,
        sizedBVZext
      ),
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
    Term
      ( BVConcatTerm,
        BVExtendTerm,
        BVSelectTerm,
        ConTerm,
        ToSignedTerm,
        ToUnsignedTerm
      ),
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
    SomeNatRepr (SomeNatRepr),
    SomePositiveNatRepr (SomePositiveNatRepr),
    addNat,
    mkNatRepr,
    mkPositiveNatRepr,
    natRepr,
    unsafeAxiom,
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
doPevalToSignedTerm (BVConcatTerm _ b1 b2) =
  Just $ pevalBVConcatTerm (pevalToSignedTerm b1) (pevalToSignedTerm b2)
doPevalToSignedTerm (BVExtendTerm _ signed pr b) =
  Just $ pevalBVExtendTerm signed pr $ pevalToSignedTerm b
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
    forall n. (KnownNat n, 1 <= n) => SignConversion (u n) (s n),
    Typeable u,
    Typeable s,
    KnownNat n,
    1 <= n,
    SizedBV u,
    SizedBV s
  ) =>
  Term (s n) ->
  Maybe (Term (u n))
doPevalToUnsignedTerm (ConTerm _ b) = Just $ conTerm $ toUnsigned b
doPevalToUnsignedTerm (ToSignedTerm _ b) = Just b >>= castTerm
doPevalToUnsignedTerm (BVConcatTerm _ b1 b2) =
  Just $ pevalBVConcatTerm (pevalToUnsignedTerm b1) (pevalToUnsignedTerm b2)
doPevalToUnsignedTerm (BVExtendTerm _ signed pr b) =
  Just $ pevalBVExtendTerm signed pr $ pevalToUnsignedTerm b
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
  withKnownNat n $
    withKnownNat ix $
      withKnownNat w $
        case ( unsafeLeqProof @1 @n,
               unsafeLeqProof @1 @w,
               unsafeLeqProof @(ix + w) @n
             ) of
          (LeqProof, LeqProof, LeqProof) -> pevalBVSelectTerm ix w term

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
doPevalBVSelectTerm _ _ rhs
  | isJust (sameNat (Proxy @ix) (Proxy @0))
      && isJust (sameNat (Proxy @w) (Proxy @n)) =
      Just rhs >>= castTerm
doPevalBVSelectTerm ix w (ConTerm _ b) = Just $ conTerm $ sizedBVSelect ix w b
doPevalBVSelectTerm ix w (ToSignedTerm _ b) =
  Just $ pevalToSignedTerm $ pevalBVSelectTerm ix w b
doPevalBVSelectTerm ix w (ToUnsignedTerm _ b) =
  Just $ pevalToUnsignedTerm $ pevalBVSelectTerm ix w b
doPevalBVSelectTerm
  pix
  pw
  (BVConcatTerm _ (b1 :: Term (bv n1)) (b2 :: Term (bv n2)))
    | ix + w <= n2 = Just $ unsafePevalBVSelectTerm n2Repr ixRepr wRepr b2
    | ix >= n2 =
        case mkNatRepr (ix - n2) of
          SomeNatRepr ixpn2Repr ->
            Just $ unsafePevalBVSelectTerm n1Repr ixpn2Repr wRepr b1
    | otherwise =
        case (mkNatRepr (w + ix - n2), mkNatRepr (n2 - ix)) of
          (SomeNatRepr wixpn2Repr, SomeNatRepr n2pixRepr) ->
            let b1Part =
                  unsafePevalBVSelectTerm n1Repr (natRepr @0) wixpn2Repr b1
                b2Part = unsafePevalBVSelectTerm n2Repr ixRepr n2pixRepr b2
             in Just $
                  unsafePevalBVConcatTerm
                    wixpn2Repr
                    n2pixRepr
                    wRepr
                    b1Part
                    b2Part
    where
      ixRepr = natRepr @ix
      wRepr = natRepr @w
      n1Repr = natRepr @n1
      n2Repr = natRepr @n2
      ix = natVal @ix pix
      w = natVal @w pw
      n2 = natVal @n2 (Proxy @n2)
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
doPevalBVSelectTerm
  pix
  pw
  (BVExtendTerm _ signed _ (b :: Term (bv n1)))
    | ix + w <= n1 = Just $ unsafePevalBVSelectTerm n1Repr ixRepr wRepr b
    | ix < n1 =
        case mkNatRepr (n1 - ix) of
          SomeNatRepr n1pixRepr ->
            let bPart = unsafePevalBVSelectTerm n1Repr ixRepr n1pixRepr b
             in Just $ unsafePevalBVExtendTerm n1pixRepr wRepr signed bPart
    | otherwise = Nothing
    where
      ixRepr = natRepr @ix
      wRepr = natRepr @w
      n1Repr = natRepr @n1
      ix = natVal @ix pix
      w = natVal @w pw
      n1 = natVal @n1 (Proxy @n1)
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
pevalBVExtendTerm signed p =
  unaryUnfoldOnce (doPevalBVExtendTerm signed p) (bvextendTerm signed p)

unsafePevalBVExtendTerm ::
  forall bv l r.
  ( forall n. (KnownNat n, 1 <= n) => SupportedPrim (bv n),
    Typeable bv,
    SizedBV bv
  ) =>
  NatRepr l ->
  NatRepr r ->
  Bool ->
  Term (bv l) ->
  Term (bv r)
unsafePevalBVExtendTerm lRepr rRepr signed v =
  case (unsafeLeqProof @1 @l, unsafeLeqProof @1 @r, unsafeLeqProof @l @r) of
    (LeqProof, LeqProof, LeqProof) ->
      withKnownNat lRepr $
        withKnownNat rRepr $
          pevalBVExtendTerm signed (Proxy @r) v

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
doPevalBVExtendTerm signed p (ConTerm _ b) =
  Just $ conTerm $ if signed then sizedBVSext p b else sizedBVZext p b
doPevalBVExtendTerm _ _ b
  | isJust $ sameNat (Proxy @l) (Proxy @r) =
      Just b >>= castTerm
doPevalBVExtendTerm False pr b =
  case (mkPositiveNatRepr $ r - l) of
    SomePositiveNatRepr (rplRepr :: NatRepr lpr) ->
      Just $
        unsafePevalBVConcatTerm
          rplRepr
          lRepr
          rRepr
          (conTerm $ sizedBVFromIntegral 0)
          b
  where
    lRepr = natRepr @l
    rRepr = natRepr @r
    l = natVal @l (Proxy @l)
    r = natVal @r pr
doPevalBVExtendTerm True p (BVExtendTerm _ True _ (b :: Term (bv l1))) =
  case unsafeLeqProof @l1 @r of
    LeqProof -> Just $ pevalBVExtendTerm True p b
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

unsafeBVConcatTerm ::
  forall bv n1 n2 r.
  ( forall n. (KnownNat n, 1 <= n) => SupportedPrim (bv n),
    Typeable bv,
    SizedBV bv
  ) =>
  NatRepr n1 ->
  NatRepr n2 ->
  NatRepr r ->
  Term (bv n1) ->
  Term (bv n2) ->
  Term (bv r)
unsafeBVConcatTerm n1Repr n2Repr rRepr lhs rhs =
  case ( unsafeAxiom :: (n1 + n2) :~: r,
         unsafeLeqProof @1 @r,
         unsafeLeqProof @1 @n1,
         unsafeLeqProof @1 @n2
       ) of
    (Refl, LeqProof, LeqProof, LeqProof) ->
      withKnownNat n1Repr $
        withKnownNat n2Repr $
          withKnownNat rRepr $
            bvconcatTerm lhs rhs

unsafePevalBVConcatTerm ::
  forall bv n1 n2 r.
  ( forall n. (KnownNat n, 1 <= n) => SupportedPrim (bv n),
    Typeable bv,
    SizedBV bv
  ) =>
  NatRepr n1 ->
  NatRepr n2 ->
  NatRepr r ->
  Term (bv n1) ->
  Term (bv n2) ->
  Term (bv r)
unsafePevalBVConcatTerm n1Repr n2Repr rRepr lhs rhs =
  case ( unsafeAxiom :: (n1 + n2) :~: r,
         unsafeLeqProof @1 @r,
         unsafeLeqProof @1 @n1,
         unsafeLeqProof @1 @n2
       ) of
    (Refl, LeqProof, LeqProof, LeqProof) ->
      withKnownNat n1Repr $
        withKnownNat n2Repr $
          withKnownNat rRepr $
            pevalBVConcatTerm lhs rhs

doPevalBVConcatTerm ::
  forall bv l r.
  ( forall n. (KnownNat n, 1 <= n) => SupportedPrim (bv n),
    Typeable bv,
    KnownNat l,
    KnownNat r,
    KnownNat (l + r),
    1 <= l,
    1 <= r,
    1 <= (l + r),
    SizedBV bv
  ) =>
  Term (bv l) ->
  Term (bv r) ->
  Maybe (Term (bv (l + r)))
-- 1. [c1 c2] -> c1c2
doPevalBVConcatTerm (ConTerm _ v) (ConTerm _ v') =
  Just $ conTerm $ sizedBVConcat v v'
-- 2. [c1 (c2 ?)] -> (c1c2 ?)
doPevalBVConcatTerm
  (ConTerm _ vl)
  (BVConcatTerm _ (ConTerm _ (vrl :: bv rl)) (rr :: Term (bv rr))) =
    case unsafeLeqProof @1 @(l + rl) of
      LeqProof ->
        Just $
          withKnownNat lRlRepr $
            unsafeBVConcatTerm
              lRlRepr
              (natRepr @rr)
              (addNat (natRepr @l) (natRepr @r))
              (conTerm $ sizedBVConcat vl vrl)
              rr
    where
      lRlRepr = addNat (natRepr @l) (natRepr @rl)
-- 3. [c1 (s c2)] -> (c1 (s c2))
doPevalBVConcatTerm (ConTerm {}) (BVConcatTerm _ _ ConTerm {}) = Nothing
-- 4. [(c s) ?) -> (c [s ?])
doPevalBVConcatTerm
  (BVConcatTerm _ (ll@ConTerm {} :: Term (bv ll)) (lr :: Term (bv lr)))
  r =
    Just $ unsafeBVConcatTerm llRepr lrRRepr lRRepr ll rhs
    where
      llRepr = natRepr @ll
      lrRepr = natRepr @lr
      lRepr = natRepr @l
      rRepr = natRepr @r
      lrRRepr = addNat lrRepr rRepr
      lRRepr = addNat lRepr rRepr
      rhs :: Term (bv (lr + r))
      rhs = unsafePevalBVConcatTerm lrRepr rRepr lrRRepr lr r
-- 5. [? (c1 (s2 c2))] -> (([? c1] s2) c2)
doPevalBVConcatTerm
  l
  ( BVConcatTerm
      _
      (rl@ConTerm {} :: Term (bv rl))
      (BVConcatTerm _ (rrl :: Term (bv rrl)) (rrr@ConTerm {} :: Term (bv rrr)))
    ) =
    Just $ unsafeBVConcatTerm lRlRrlRepr rrrRepr lRRepr lRlRrl rrr
    where
      lRepr = natRepr @l
      rlRepr = natRepr @rl
      rrlRepr = natRepr @rrl
      rrrRepr = natRepr @rrr
      lRlRepr = addNat lRepr rlRepr
      rRepr = natRepr @r
      lRRepr = addNat lRepr rRepr
      lRl = unsafePevalBVConcatTerm lRepr rlRepr lRlRepr l rl
      lRlRrlRepr = addNat lRlRepr rrlRepr
      lRlRrl = unsafeBVConcatTerm lRlRepr rrlRepr lRlRrlRepr lRl rrl
-- 6. [(s1 c1) c2] -> (s1 c1c2)
doPevalBVConcatTerm
  (BVConcatTerm _ (ll :: Term (bv ll)) ((ConTerm _ vlr) :: Term (bv lr)))
  (ConTerm _ vr) =
    Just $ unsafeBVConcatTerm llRepr lrRRepr lRRepr ll rhs
    where
      llRepr = natRepr @ll
      lrRepr = natRepr @lr
      lRepr = natRepr @l
      rRepr = natRepr @r
      lrRRepr = addNat lrRepr rRepr
      lRRepr = addNat lRepr rRepr
      rhs :: Term (bv (lr + r))
      rhs = case unsafeLeqProof @1 @(lr + r) of
        LeqProof ->
          withKnownNat lrRRepr $ conTerm $ sizedBVConcat vlr vr
-- 7. [(s1 c1) (c2 s2)] -> (s1 (c1c2 s2))
doPevalBVConcatTerm
  (BVConcatTerm _ (ll :: Term (bv ll)) ((ConTerm _ vlr) :: Term (bv lr)))
  (BVConcatTerm _ ((ConTerm _ vrl) :: Term (bv rl)) (rr :: Term (bv rr))) =
    Just $ unsafeBVConcatTerm llRepr lrRlRrRepr lRRepr ll lrRlRR
    where
      lRepr = natRepr @l
      rRepr = natRepr @r
      llRepr = natRepr @ll
      lrRepr = natRepr @lr
      rlRepr = natRepr @rl
      rrRepr = natRepr @rr
      lRRepr = addNat lRepr rRepr
      lrRlRepr :: NatRepr (lr + rl)
      lrRlRepr = addNat lrRepr rlRepr
      lrRlRrRepr :: NatRepr ((lr + rl) + rr)
      lrRlRrRepr = addNat lrRlRepr rrRepr
      lrRl :: Term (bv (lr + rl))
      lrRl = case unsafeLeqProof @1 @(lr + rl) of
        LeqProof -> withKnownNat lrRlRepr $ conTerm $ sizedBVConcat vlr vrl
      lrRlRR :: Term (bv ((lr + rl) + rr))
      lrRlRR = unsafeBVConcatTerm lrRlRepr rrRepr lrRlRrRepr lrRl rr
-- 8. [?notc (s2 c)] -> ((s1 s2) c)
doPevalBVConcatTerm
  l
  (BVConcatTerm _ (rl :: Term (bv rl)) (rr@ConTerm {} :: Term (bv rr))) =
    Just $
      unsafeBVConcatTerm
        lRlRepr
        (natRepr @rr)
        (addNat (natRepr @l) (natRepr @r))
        lhs
        rr
    where
      lRepr = natRepr @l
      rlRepr = natRepr @rl
      lRlRepr = addNat lRepr rlRepr
      lhs :: Term (bv (l + rl))
      lhs = unsafeBVConcatTerm lRepr rlRepr lRlRepr l rl
doPevalBVConcatTerm _ _ = Nothing
