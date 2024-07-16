{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalBitCastTerm
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalBitCastTerm
  ( doPevalBitCast,
  )
where

import qualified Data.SBV as SBV
import GHC.TypeLits (KnownNat, type (+), type (<=))
import Grisette.Internal.Core.Data.Class.BitCast (BitCast (bitCast))
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP (FP, ValidFP, withValidFPProofs)
import Grisette.Internal.SymPrim.Prim.Internal.Instances.SupportedPrim ()
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( PEvalBitCastTerm (pevalBitCastTerm, sbvBitCast),
    SupportedNonFuncPrim,
    Term (BitCastTerm, ConTerm),
    bitCastTerm,
    conTerm,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Unfold (unaryUnfoldOnce)
import Grisette.Internal.SymPrim.Prim.Internal.Utils (pattern Dyn)

doPevalBitCastSameType ::
  forall x b. (SupportedNonFuncPrim b) => Term x -> Maybe (Term b)
doPevalBitCastSameType (BitCastTerm _ (Dyn (b :: Term b))) = Just b
doPevalBitCastSameType (BitCastTerm _ x) = doPevalBitCastSameType x
doPevalBitCastSameType _ = Nothing

doPevalBitCast :: (PEvalBitCastTerm a b) => Term a -> Maybe (Term b)
doPevalBitCast (ConTerm _ v) = Just $ conTerm $ bitCast v
doPevalBitCast t = doPevalBitCastSameType t

pevalBitCastGeneral ::
  forall a b.
  (PEvalBitCastTerm a b) =>
  Term a ->
  Term b
pevalBitCastGeneral = unaryUnfoldOnce doPevalBitCast bitCastTerm

instance PEvalBitCastTerm Bool (IntN 1) where
  pevalBitCastTerm = pevalBitCastGeneral
  sbvBitCast _ x = SBV.ite x (SBV.literal 1) (SBV.literal 0)

instance PEvalBitCastTerm Bool (WordN 1) where
  pevalBitCastTerm = pevalBitCastGeneral
  sbvBitCast _ x = SBV.ite x (SBV.literal 1) (SBV.literal 0)

instance PEvalBitCastTerm (IntN 1) Bool where
  pevalBitCastTerm = pevalBitCastGeneral
  sbvBitCast _ x = SBV.sTestBit x 0

instance PEvalBitCastTerm (WordN 1) Bool where
  pevalBitCastTerm = pevalBitCastGeneral
  sbvBitCast _ x = SBV.sTestBit x 0

instance
  (n ~ eb + sb, ValidFP eb sb, KnownNat n, 1 <= n) =>
  PEvalBitCastTerm (WordN n) (FP eb sb)
  where
  pevalBitCastTerm = pevalBitCastGeneral
  sbvBitCast _ = withValidFPProofs @eb @sb $ SBV.sWordAsSFloatingPoint

instance
  (n ~ eb + sb, ValidFP eb sb, KnownNat n, 1 <= n) =>
  PEvalBitCastTerm (IntN n) (FP eb sb)
  where
  pevalBitCastTerm = pevalBitCastGeneral
  sbvBitCast _ =
    withValidFPProofs @eb @sb $ SBV.sWordAsSFloatingPoint . SBV.sFromIntegral

instance
  (n ~ eb + sb, ValidFP eb sb, KnownNat n, 1 <= n) =>
  PEvalBitCastTerm (FP eb sb) (WordN n)
  where
  pevalBitCastTerm = pevalBitCastGeneral
  sbvBitCast _ v = withValidFPProofs @eb @sb $ SBV.sFloatingPointAsSWord v

instance
  (n ~ eb + sb, ValidFP eb sb, KnownNat n, 1 <= n) =>
  PEvalBitCastTerm (FP eb sb) (IntN n)
  where
  pevalBitCastTerm = pevalBitCastGeneral
  sbvBitCast _ =
    withValidFPProofs @eb @sb $ SBV.sFromIntegral . SBV.sFloatingPointAsSWord
