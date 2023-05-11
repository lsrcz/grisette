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
  ( pevalBVToSignedTerm,
    pevalBVToUnsignedTerm,
    pevalBVConcatTerm,
    pevalBVSelectTerm,
    pevalBVExtendTerm,
    pevalBVZeroExtendTerm,
    pevalBVSignExtendTerm,
  )
where

import Data.Typeable
import Debug.Trace
import GHC.TypeNats
import Grisette.Core.Data.Class.BitVector
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.TermUtils
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bits
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bool
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Num
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Unfold
import Grisette.Utils
import qualified Type.Reflection as R

-- toSigned
pevalBVToSignedTerm ::
  ( forall n. (KnownNat n, 1 <= n) => SupportedPrim (sbv n),
    forall n. (KnownNat n, 1 <= n) => SupportedPrim (ubv n),
    forall n. (KnownNat n, 1 <= n) => BVSignPair (sbv n) (ubv n),
    Typeable sbv,
    Typeable ubv,
    SizedBV sbv,
    SizedBV ubv,
    KnownNat n,
    1 <= n
  ) =>
  Term (ubv n) ->
  Term (sbv n)
pevalBVToSignedTerm = unaryUnfoldOnce doPevalBVToSignedTerm bvToSignedTerm

doPevalBVToSignedTerm ::
  ( forall n. (KnownNat n, 1 <= n) => SupportedPrim (sbv n),
    forall n. (KnownNat n, 1 <= n) => SupportedPrim (ubv n),
    forall n. (KnownNat n, 1 <= n) => BVSignPair (sbv n) (ubv n),
    Typeable sbv,
    Typeable ubv,
    SizedBV sbv,
    SizedBV ubv,
    KnownNat n,
    1 <= n
  ) =>
  Term (ubv n) ->
  Maybe (Term (sbv n))
doPevalBVToSignedTerm (ConTerm _ b) = Just $ conTerm $ toSigned b
doPevalBVToSignedTerm (BVToUnsignedTerm _ b) = Just b >>= castTerm
doPevalBVToSignedTerm _ = Nothing

-- toUnsigned
pevalBVToUnsignedTerm ::
  ( forall n. (KnownNat n, 1 <= n) => SupportedPrim (sbv n),
    forall n. (KnownNat n, 1 <= n) => SupportedPrim (ubv n),
    forall n. (KnownNat n, 1 <= n) => BVSignPair (sbv n) (ubv n),
    Typeable sbv,
    Typeable ubv,
    SizedBV sbv,
    SizedBV ubv,
    KnownNat n,
    1 <= n
  ) =>
  Term (sbv n) ->
  Term (ubv n)
pevalBVToUnsignedTerm = unaryUnfoldOnce doPevalBVToUnsignedTerm bvToUnsignedTerm

doPevalBVToUnsignedTerm ::
  ( forall n. (KnownNat n, 1 <= n) => SupportedPrim (sbv n),
    forall n. (KnownNat n, 1 <= n) => SupportedPrim (ubv n),
    forall n. (KnownNat n, 1 <= n) => BVSignPair (sbv n) (ubv n),
    Typeable sbv,
    Typeable ubv,
    SizedBV sbv,
    SizedBV ubv,
    KnownNat n,
    1 <= n
  ) =>
  Term (sbv n) ->
  Maybe (Term (ubv n))
doPevalBVToUnsignedTerm (ConTerm _ b) = Just $ conTerm $ toUnsigned b
doPevalBVToUnsignedTerm (BVToSignedTerm _ b) = Just b >>= castTerm
doPevalBVToUnsignedTerm _ = Nothing

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
doPevalBVSelectTerm ix w b
  | ixv == 0 && wv == bv =
      case unsafeAxiom @w @n of
        Refl -> Just b
  where
    ixv = natVal ix
    wv = natVal w
    bv = natVal (Proxy @n)
doPevalBVSelectTerm ix w (BVExtendTerm _ signed r (b :: Term (bv b)))
  | ixv + wv <= bv = case unsafeLeqProof @(ix + w) @b of
      LeqProof -> Just $ pevalBVSelectTerm ix w b
  | ixv >= bv && not signed = Just $ conTerm $ integerToSizedBV (Proxy @w) 0
  | ixv >= bv && signed =
      let t = pevalTestBitTerm b (fromIntegral $ bv - 1)
       in Just $ pevalITETerm t (conTerm $ integerToSizedBV (Proxy @w) $ -1) (conTerm $ integerToSizedBV (Proxy @w) 0)
  | ixv + wv < nv =
      case ( unsafeKnownProof @(ix + w) (ixv + wv),
             unsafeLeqProof @(ix + w) @(ix + w),
             unsafeLeqProof @b @(ix + w),
             unsafeLeqProof @1 @(ix + w)
           ) of
        (KnownProof, LeqProof, LeqProof, LeqProof) ->
          Just $ pevalBVSelectTerm ix w $ pevalBVExtendTerm signed (Proxy @(ix + w)) b
  where
    ixv = natVal ix
    wv = natVal w
    nv = natVal (Proxy @n)
    bv = natVal (Proxy @b)
doPevalBVSelectTerm ix w (BVSelectTerm _ (ix' :: R.TypeRep ix') (w' :: R.TypeRep w') (b :: Term (bv b))) =
  case (unsafeKnownProof @(ix + ix') (ixv + ixv'), unsafeLeqProof @((ix + ix') + w) @b) of
    (KnownProof, LeqProof) -> Just $ pevalBVSelectTerm (Proxy @(ix + ix')) w b
  where
    ixv = natVal ix
    ixv' = natVal ix'
doPevalBVSelectTerm ix w (BVConcatTerm _ (l :: Term (bv l)) (r :: Term (bv r)))
  | ixv + wv <= rv = case unsafeLeqProof @(ix + w) @r of
      LeqProof -> Just $ pevalBVSelectTerm ix w r
  | ixv >= rv = case (unsafeKnownProof @(ix - r) (ixv - rv), unsafeLeqProof @((ix - r) + w) @l) of
      (KnownProof, LeqProof) -> Just $ pevalBVSelectTerm (Proxy @(ix - r)) w l
  where
    ixv = natVal ix
    wv = natVal w
    rv = natVal (Proxy @r)
doPevalBVSelectTerm ix w (BVToSignedTerm _ l) =
  Just $ pevalBVToSignedTerm $ pevalBVSelectTerm ix w l
doPevalBVSelectTerm ix w (BVToUnsignedTerm _ l) =
  Just $ pevalBVToUnsignedTerm $ pevalBVSelectTerm ix w l
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
