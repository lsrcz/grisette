{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Prim.Internal.Instances.BVPEval
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.Prim.Internal.Instances.BVPEval () where

import Data.Kind (Type)
import Data.Maybe (isJust)
import Data.Proxy (Proxy (Proxy))
import qualified Data.SBV as SBV
import Data.Typeable (Typeable, type (:~:) (Refl))
import GHC.TypeNats (KnownNat, Nat, natVal, sameNat, type (+), type (-), type (<=))
import Grisette.Internal.Core.Data.Class.BitVector
  ( SizedBV
      ( sizedBVConcat,
        sizedBVFromIntegral,
        sizedBVSelect,
        sizedBVSext,
        sizedBVZext
      ),
  )
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalBitCastTerm (doPevalBitCast)
import Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalBitwiseTerm ()
import Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalNumTerm ()
import Grisette.Internal.SymPrim.Prim.Internal.Instances.SupportedPrim
  ( bvIsNonZeroFromGEq1,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( PEvalBVTerm
      ( pevalBVConcatTerm,
        pevalBVExtendTerm,
        pevalBVSelectTerm,
        sbvBVConcatTerm,
        sbvBVExtendTerm,
        sbvBVSelectTerm
      ),
    PEvalBitCastTerm (pevalBitCastTerm, sbvBitCast),
    PEvalBitwiseTerm,
    PEvalNumTerm,
    SupportedPrim,
    Term,
    bitCastTerm,
    bvConcatTerm,
    bvExtendTerm,
    bvSelectTerm,
    conTerm,
    pattern AddNumTerm,
    pattern AndBitsTerm,
    pattern BVConcatTerm,
    pattern BVExtendTerm,
    pattern BVSelectTerm,
    pattern BitCastTerm,
    pattern ConTerm,
    pattern DynTerm,
    pattern MulNumTerm,
    pattern OrBitsTerm,
    pattern XorBitsTerm,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Unfold
  ( binaryUnfoldOnce,
    unaryUnfoldOnce,
  )
import Grisette.Internal.SymPrim.Prim.TermUtils (castTerm)
import Grisette.Internal.Utils.Parameterized
  ( LeqProof (LeqProof),
    NatRepr,
    SomeNatRepr (SomeNatRepr),
    SomePositiveNatRepr (SomePositiveNatRepr),
    addNat,
    mkNatRepr,
    mkPositiveNatRepr,
    natRepr,
    unsafeAxiom,
    unsafeKnownProof,
    unsafeLeqProof,
    withKnownNat,
    withKnownProof,
  )

pevalDefaultBVSelectTerm ::
  forall (bv2 :: Nat -> Type) bv n ix w p q.
  ( KnownNat n,
    KnownNat ix,
    KnownNat w,
    1 <= n,
    1 <= w,
    ix + w <= n,
    PEvalBVTerm bv,
    forall x. (KnownNat x, 1 <= x) => PEvalBitCastTerm (bv2 x) (bv x),
    PEvalBVTerm bv2,
    Typeable bv,
    SupportedPrim (bv w),
    SupportedPrim (bv2 n),
    forall m. (KnownNat m, 1 <= m) => PEvalNumTerm (bv m),
    forall m. (KnownNat m, 1 <= m) => PEvalBitwiseTerm (bv m)
  ) =>
  p ix ->
  q w ->
  Term (bv n) ->
  Term (bv w)
pevalDefaultBVSelectTerm ix w =
  unaryUnfoldOnce (doPevalDefaultBVSelectTerm @bv2 ix w) (bvSelectTerm ix w)

unsafePevalBVSelectTerm ::
  forall bv n ix w.
  (PEvalBVTerm bv) =>
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

doPevalDefaultBVSelectTerm ::
  forall (bv2 :: Nat -> Type) bv n ix w p q.
  ( KnownNat n,
    KnownNat ix,
    KnownNat w,
    forall x. (KnownNat x, 1 <= x) => PEvalBitCastTerm (bv2 x) (bv x),
    1 <= n,
    1 <= w,
    ix + w <= n,
    PEvalBVTerm bv,
    PEvalBVTerm bv2,
    Typeable bv,
    SupportedPrim (bv w),
    SupportedPrim (bv2 n),
    forall m. (KnownNat m, 1 <= m) => PEvalNumTerm (bv m),
    forall m. (KnownNat m, 1 <= m) => PEvalBitwiseTerm (bv m)
  ) =>
  p ix ->
  q w ->
  Term (bv n) ->
  Maybe (Term (bv w))
doPevalDefaultBVSelectTerm _ _ rhs
  | isJust (sameNat (Proxy @ix) (Proxy @0))
      && isJust (sameNat (Proxy @w) (Proxy @n)) =
      Just rhs >>= castTerm
doPevalDefaultBVSelectTerm ix w (ConTerm b) =
  Just $ conTerm $ sizedBVSelect ix w b
doPevalDefaultBVSelectTerm ix w (BitCastTerm (DynTerm (b :: Term (bv2 n)))) =
  Just $ pevalBitCastTerm $ pevalBVSelectTerm ix w b
doPevalDefaultBVSelectTerm ix w (AddNumTerm t1 t2)
  | natVal @ix ix == 0 =
      Just $
        AddNumTerm
          (pevalDefaultBVSelectTerm @bv2 @bv ix w t1)
          (pevalDefaultBVSelectTerm @bv2 @bv ix w t2)
doPevalDefaultBVSelectTerm ix w (MulNumTerm t1 t2)
  | natVal @ix ix == 0 =
      Just $
        MulNumTerm
          (pevalDefaultBVSelectTerm @bv2 @bv ix w t1)
          (pevalDefaultBVSelectTerm @bv2 @bv ix w t2)
doPevalDefaultBVSelectTerm ix w (AndBitsTerm t1 t2) =
  Just $
    AndBitsTerm
      (pevalDefaultBVSelectTerm @bv2 @bv ix w t1)
      (pevalDefaultBVSelectTerm @bv2 @bv ix w t2)
doPevalDefaultBVSelectTerm ix w (OrBitsTerm t1 t2) =
  Just $
    OrBitsTerm
      (pevalDefaultBVSelectTerm @bv2 @bv ix w t1)
      (pevalDefaultBVSelectTerm @bv2 @bv ix w t2)
doPevalDefaultBVSelectTerm ix w (XorBitsTerm t1 t2) =
  Just $
    XorBitsTerm
      (pevalDefaultBVSelectTerm @bv2 @bv ix w t1)
      (pevalDefaultBVSelectTerm @bv2 @bv ix w t2)
doPevalDefaultBVSelectTerm
  pix
  pw
  (BVConcatTerm (b1 :: Term (bv n1)) (b2 :: Term (bv n2)))
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
doPevalDefaultBVSelectTerm
  _
  _
  (BVSelectTerm (_ :: proxy ix1) _ (b :: Term (bv n1))) =
    Just $
      unsafePevalBVSelectTerm
        (natRepr @n1)
        (addNat (natRepr @ix) (natRepr @ix1))
        (natRepr @w)
        b
doPevalDefaultBVSelectTerm
  pix
  pw
  (BVExtendTerm signed _ (b :: Term (bv n1)))
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
doPevalDefaultBVSelectTerm _ _ _ = Nothing

pevalDefaultBVExtendTerm ::
  forall proxy l r bv.
  ( PEvalBVTerm bv,
    KnownNat l,
    KnownNat r,
    1 <= l,
    1 <= r,
    l <= r,
    Typeable bv,
    forall n. (KnownNat n, 1 <= n) => SupportedPrim (bv n)
  ) =>
  Bool ->
  proxy r ->
  Term (bv l) ->
  Term (bv r)
pevalDefaultBVExtendTerm signed p =
  unaryUnfoldOnce (doPevalDefaultBVExtendTerm signed p) (bvExtendTerm signed p)

unsafePevalBVExtendTerm ::
  forall bv l r.
  (PEvalBVTerm bv) =>
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

doPevalDefaultBVExtendTerm ::
  forall proxy l r bv.
  ( PEvalBVTerm bv,
    KnownNat l,
    KnownNat r,
    1 <= l,
    1 <= r,
    l <= r,
    Typeable bv,
    forall n. (KnownNat n, 1 <= n) => SupportedPrim (bv n)
  ) =>
  Bool ->
  proxy r ->
  Term (bv l) ->
  Maybe (Term (bv r))
doPevalDefaultBVExtendTerm signed p (ConTerm b) =
  Just $ conTerm $ if signed then sizedBVSext p b else sizedBVZext p b
doPevalDefaultBVExtendTerm _ _ b
  | isJust $ sameNat (Proxy @l) (Proxy @r) =
      Just b >>= castTerm
doPevalDefaultBVExtendTerm False pr b =
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
doPevalDefaultBVExtendTerm True p (BVExtendTerm True _ (b :: Term (bv l1))) =
  case unsafeLeqProof @l1 @r of
    LeqProof -> Just $ pevalBVExtendTerm True p b
doPevalDefaultBVExtendTerm _ _ _ = Nothing

pevalDefaultBVConcatTerm ::
  forall bv a b.
  ( KnownNat a,
    KnownNat b,
    1 <= a,
    1 <= b,
    PEvalBVTerm bv,
    forall n. (KnownNat n, 1 <= n) => SupportedPrim (bv n)
  ) =>
  Term (bv a) ->
  Term (bv b) ->
  Term (bv (a + b))
pevalDefaultBVConcatTerm =
  withKnownNat (addNat (natRepr @a) (natRepr @b)) $
    case (unsafeLeqProof @1 @(a + b)) of
      LeqProof ->
        binaryUnfoldOnce doPevalDefaultBVConcatTerm bvConcatTerm

unsafeBVConcatTerm ::
  forall bv n1 n2 r.
  (PEvalBVTerm bv, forall n. (KnownNat n, 1 <= n) => SupportedPrim (bv n)) =>
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
            bvConcatTerm lhs rhs

unsafePevalBVConcatTerm ::
  forall bv n1 n2 r.
  (PEvalBVTerm bv) =>
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

doPevalDefaultBVConcatTerm ::
  forall bv l r.
  ( KnownNat l,
    KnownNat r,
    KnownNat (l + r),
    1 <= l,
    1 <= r,
    1 <= (l + r),
    PEvalBVTerm bv,
    forall n. (KnownNat n, 1 <= n) => SupportedPrim (bv n)
  ) =>
  Term (bv l) ->
  Term (bv r) ->
  Maybe (Term (bv (l + r)))
-- 1. [c1 c2] -> c1c2
doPevalDefaultBVConcatTerm (ConTerm v) (ConTerm v') =
  withKnownNat (addNat (natRepr @l) (natRepr @r)) $
    Just $
      conTerm $
        sizedBVConcat v v'
-- 2. [c1 (c2 ?)] -> (c1c2 ?)
doPevalDefaultBVConcatTerm
  (ConTerm vl)
  (BVConcatTerm (ConTerm (vrl :: bv rl)) (rr :: Term (bv rr))) =
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
doPevalDefaultBVConcatTerm (ConTerm {}) (BVConcatTerm _ ConTerm {}) = Nothing
-- 4. [(c s) ?) -> (c [s ?])
doPevalDefaultBVConcatTerm
  (BVConcatTerm (ll@ConTerm {} :: Term (bv ll)) (lr :: Term (bv lr)))
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
doPevalDefaultBVConcatTerm
  l
  ( BVConcatTerm
      (rl@ConTerm {} :: Term (bv rl))
      (BVConcatTerm (rrl :: Term (bv rrl)) (rrr@ConTerm {} :: Term (bv rrr)))
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
doPevalDefaultBVConcatTerm
  (BVConcatTerm (ll :: Term (bv ll)) ((ConTerm vlr) :: Term (bv lr)))
  (ConTerm vr) =
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
doPevalDefaultBVConcatTerm
  (BVConcatTerm (ll :: Term (bv ll)) ((ConTerm vlr) :: Term (bv lr)))
  (BVConcatTerm ((ConTerm vrl) :: Term (bv rl)) (rr :: Term (bv rr))) =
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
doPevalDefaultBVConcatTerm
  l
  (BVConcatTerm (rl :: Term (bv rl)) (rr@ConTerm {} :: Term (bv rr))) =
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
doPevalDefaultBVConcatTerm
  (BVSelectTerm ix0 (_ :: p w0) (bv0 :: Term (bv n)))
  (BVSelectTerm (ix1 :: p ix1) (w1 :: p w1) (DynTerm (bv1 :: Term (bv n))))
    | ix1v + w1v == ix0v && bv0 == bv1 =
        Just $ unsafePevalBVSelectTerm nRepr ix1Repr (addNat w0Repr w1Repr) bv0
    where
      nRepr = natRepr @n
      w1v = natVal w1
      ix0v = natVal ix0
      ix1v = natVal ix1
      ix1Repr = natRepr @ix1
      w0Repr = natRepr @w0
      w1Repr = natRepr @w1
doPevalDefaultBVConcatTerm _ _ = Nothing

instance PEvalBVTerm WordN where
  pevalBVSelectTerm = pevalDefaultBVSelectTerm @IntN
  pevalBVConcatTerm = pevalDefaultBVConcatTerm
  pevalBVExtendTerm = pevalDefaultBVExtendTerm
  sbvBVConcatTerm pl pr l r =
    bvIsNonZeroFromGEq1 pl $
      bvIsNonZeroFromGEq1 pr $
        l SBV.# r
  sbvBVSelectTerm (pix :: p0 ix) (pw :: p1 w) (pn :: p2 n) bv =
    bvIsNonZeroFromGEq1 (Proxy @n) $
      bvIsNonZeroFromGEq1 (Proxy @w) $
        sbvDefaultBVSelectTerm pix pw pn bv
  sbvBVExtendTerm (_ :: p0 l) (_ :: p1 r) signed bv =
    withKnownProof
      (unsafeKnownProof @(r - l) (natVal (Proxy @r) - natVal (Proxy @l)))
      $ case (unsafeLeqProof @(l + 1) @r, unsafeLeqProof @1 @(r - l)) of
        (LeqProof, LeqProof) ->
          bvIsNonZeroFromGEq1 (Proxy @r) $
            bvIsNonZeroFromGEq1 (Proxy @l) $
              bvIsNonZeroFromGEq1 (Proxy @(r - l)) $
                if signed then SBV.signExtend bv else SBV.zeroExtend bv

instance PEvalBVTerm IntN where
  pevalBVSelectTerm = pevalDefaultBVSelectTerm @WordN
  pevalBVConcatTerm = pevalDefaultBVConcatTerm
  pevalBVExtendTerm = pevalDefaultBVExtendTerm
  sbvBVConcatTerm (pl :: p l) (pr :: q r) l r =
    bvIsNonZeroFromGEq1 pl $
      bvIsNonZeroFromGEq1 pr $
        withKnownNat (addNat (natRepr @l) (natRepr @r)) $
          case unsafeLeqProof @1 @(l + r) of
            LeqProof ->
              bvIsNonZeroFromGEq1 (Proxy @(l + r)) $
                sbvBitCast @(WordN (l + r)) @(IntN (l + r)) $
                  (sbvBitCast @(IntN l) @(WordN l) l)
                    SBV.# (sbvBitCast @(IntN r) @(WordN r) r)
  sbvBVSelectTerm (pix :: p0 ix) (pw :: p1 w) (pn :: p2 n) bv =
    bvIsNonZeroFromGEq1 (Proxy @n) $
      bvIsNonZeroFromGEq1 (Proxy @w) $
        sbvDefaultBVSelectTerm pix pw pn bv
  sbvBVExtendTerm (_ :: p0 l) (_ :: p1 r) signed bv =
    withKnownProof
      (unsafeKnownProof @(r - l) (natVal (Proxy @r) - natVal (Proxy @l)))
      $ case (unsafeLeqProof @(l + 1) @r, unsafeLeqProof @1 @(r - l)) of
        (LeqProof, LeqProof) ->
          bvIsNonZeroFromGEq1 (Proxy @r) $
            bvIsNonZeroFromGEq1 (Proxy @l) $
              bvIsNonZeroFromGEq1 (Proxy @(r - l)) $
                if signed
                  then SBV.signExtend bv
                  else
                    SBV.sFromIntegral
                      ( SBV.zeroExtend
                          (SBV.sFromIntegral bv :: SBV.SBV (SBV.WordN l)) ::
                          SBV.SBV (SBV.WordN r)
                      )

sbvDefaultBVSelectTerm ::
  ( KnownNat ix,
    KnownNat w,
    KnownNat n,
    1 <= n,
    1 <= w,
    (ix + w) <= n,
    SBV.SymVal (bv n)
  ) =>
  p1 ix ->
  p2 w ->
  p3 n ->
  SBV.SBV (bv n) ->
  SBV.SBV (bv w)
sbvDefaultBVSelectTerm (_ :: p0 ix) (_ :: p1 w) (_ :: p2 n) bv =
  withKnownProof
    ( unsafeKnownProof @(w + ix - 1)
        (natVal (Proxy @w) + natVal (Proxy @ix) - 1)
    )
    $ case ( unsafeAxiom @(w + ix - 1 - ix + 1) @w,
             unsafeLeqProof @(((w + ix) - 1) + 1) @n,
             unsafeLeqProof @ix @(w + ix - 1)
           ) of
      (Refl, LeqProof, LeqProof) ->
        bvIsNonZeroFromGEq1 (Proxy @n) $
          bvIsNonZeroFromGEq1 (Proxy @w) $
            SBV.bvExtract (Proxy @(w + ix - 1)) (Proxy @ix) bv

instance (KnownNat n, 1 <= n) => PEvalBitCastTerm (WordN n) (IntN n) where
  pevalBitCastTerm = unaryUnfoldOnce doPevalBitCastBV bitCastTerm
    where
      doPevalBitCastBV :: Term (WordN n) -> Maybe (Term (IntN n))
      doPevalBitCastBV
        (BVConcatTerm (l :: Term (WordN l)) (r :: Term (WordN r))) =
          Just $
            pevalBVConcatTerm
              (pevalBitCastTerm @(WordN l) @(IntN l) l)
              (pevalBitCastTerm @(WordN r) @(IntN r) r)
      doPevalBitCastBV (BVExtendTerm signed pr (b :: Term (WordN l))) =
        Just $
          pevalBVExtendTerm signed pr $
            pevalBitCastTerm @(WordN l) @(IntN l) b
      doPevalBitCastBV v = doPevalBitCast v
  sbvBitCast = bvIsNonZeroFromGEq1 (Proxy @n) SBV.sFromIntegral

instance (KnownNat n, 1 <= n) => PEvalBitCastTerm (IntN n) (WordN n) where
  pevalBitCastTerm = unaryUnfoldOnce doPevalBitCastBV bitCastTerm
    where
      doPevalBitCastBV :: Term (IntN n) -> Maybe (Term (WordN n))
      doPevalBitCastBV
        (BVConcatTerm (l :: Term (IntN l)) (r :: Term (IntN r))) =
          Just $
            pevalBVConcatTerm
              (pevalBitCastTerm @(IntN l) @(WordN l) l)
              (pevalBitCastTerm @(IntN r) @(WordN r) r)
      doPevalBitCastBV (BVExtendTerm signed pr (b :: Term (IntN l))) =
        Just $
          pevalBVExtendTerm signed pr $
            pevalBitCastTerm @(IntN l) @(WordN l) b
      doPevalBitCastBV v = doPevalBitCast v
  sbvBitCast = bvIsNonZeroFromGEq1 (Proxy @n) SBV.sFromIntegral
