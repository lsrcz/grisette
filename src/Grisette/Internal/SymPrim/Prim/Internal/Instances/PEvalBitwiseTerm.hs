{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalBitwiseTerm
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalBitwiseTerm () where

import Data.Bits (Bits (complement, xor, zeroBits, (.&.), (.|.)))
import GHC.TypeLits (KnownNat, type (<=))
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.Prim.Internal.Instances.SupportedPrim ()
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( PEvalBitwiseTerm
      ( pevalAndBitsTerm,
        pevalComplementBitsTerm,
        pevalOrBitsTerm,
        pevalXorBitsTerm,
        withSbvBitwiseTermConstraint
      ),
    SupportedPrim (withPrim),
    Term (ComplementBitsTerm, ConTerm),
    andBitsTerm,
    complementBitsTerm,
    conTerm,
    orBitsTerm,
    xorBitsTerm,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Unfold
  ( binaryUnfoldOnce,
    unaryUnfoldOnce,
  )

pevalDefaultAndBitsTerm ::
  (Bits a, SupportedPrim a, PEvalBitwiseTerm a) => Term a -> Term a -> Term a
pevalDefaultAndBitsTerm = binaryUnfoldOnce doPevalAndBitsTerm andBitsTerm
  where
    doPevalAndBitsTerm (ConTerm _ _ a) (ConTerm _ _ b) =
      Just $ conTerm (a .&. b)
    doPevalAndBitsTerm (ConTerm _ _ a) b
      | a == zeroBits = Just $ conTerm zeroBits
      | a == complement zeroBits = Just b
    doPevalAndBitsTerm a (ConTerm _ _ b)
      | b == zeroBits = Just $ conTerm zeroBits
      | b == complement zeroBits = Just a
    doPevalAndBitsTerm a b | a == b = Just a
    doPevalAndBitsTerm _ _ = Nothing

pevalDefaultOrBitsTerm ::
  (Bits a, SupportedPrim a, PEvalBitwiseTerm a) => Term a -> Term a -> Term a
pevalDefaultOrBitsTerm = binaryUnfoldOnce doPevalOrBitsTerm orBitsTerm
  where
    doPevalOrBitsTerm (ConTerm _ _ a) (ConTerm _ _ b) = Just $ conTerm (a .|. b)
    doPevalOrBitsTerm (ConTerm _ _ a) b
      | a == zeroBits = Just b
      | a == complement zeroBits = Just $ conTerm $ complement zeroBits
    doPevalOrBitsTerm a (ConTerm _ _ b)
      | b == zeroBits = Just a
      | b == complement zeroBits = Just $ conTerm $ complement zeroBits
    doPevalOrBitsTerm a b | a == b = Just a
    doPevalOrBitsTerm _ _ = Nothing

pevalDefaultXorBitsTerm ::
  (PEvalBitwiseTerm a, SupportedPrim a) => Term a -> Term a -> Term a
pevalDefaultXorBitsTerm = binaryUnfoldOnce doPevalXorBitsTerm xorBitsTerm
  where
    doPevalXorBitsTerm (ConTerm _ _ a) (ConTerm _ _ b) =
      Just $ conTerm (a `xor` b)
    doPevalXorBitsTerm (ConTerm _ _ a) b
      | a == zeroBits = Just b
      | a == complement zeroBits = Just $ pevalComplementBitsTerm b
    doPevalXorBitsTerm a (ConTerm _ _ b)
      | b == zeroBits = Just a
      | b == complement zeroBits = Just $ pevalComplementBitsTerm a
    doPevalXorBitsTerm a b | a == b = Just $ conTerm zeroBits
    doPevalXorBitsTerm (ComplementBitsTerm _ _ i) (ComplementBitsTerm _ _ j) =
      Just $ pevalXorBitsTerm i j
    doPevalXorBitsTerm (ComplementBitsTerm _ _ i) j =
      Just $ pevalComplementBitsTerm $ pevalXorBitsTerm i j
    doPevalXorBitsTerm i (ComplementBitsTerm _ _ j) =
      Just $ pevalComplementBitsTerm $ pevalXorBitsTerm i j
    doPevalXorBitsTerm _ _ = Nothing

pevalDefaultComplementBitsTerm ::
  (Bits a, SupportedPrim a, PEvalBitwiseTerm a) => Term a -> Term a
pevalDefaultComplementBitsTerm =
  unaryUnfoldOnce doPevalComplementBitsTerm complementBitsTerm
  where
    doPevalComplementBitsTerm (ConTerm _ _ a) = Just $ conTerm $ complement a
    doPevalComplementBitsTerm (ComplementBitsTerm _ _ a) = Just a
    doPevalComplementBitsTerm _ = Nothing

instance (KnownNat n, 1 <= n) => PEvalBitwiseTerm (WordN n) where
  pevalAndBitsTerm = pevalDefaultAndBitsTerm
  pevalOrBitsTerm = pevalDefaultOrBitsTerm
  pevalXorBitsTerm = pevalDefaultXorBitsTerm
  pevalComplementBitsTerm = pevalDefaultComplementBitsTerm
  withSbvBitwiseTermConstraint r = withPrim @(WordN n) r

instance (KnownNat n, 1 <= n) => PEvalBitwiseTerm (IntN n) where
  pevalAndBitsTerm = pevalDefaultAndBitsTerm
  pevalOrBitsTerm = pevalDefaultOrBitsTerm
  pevalXorBitsTerm = pevalDefaultXorBitsTerm
  pevalComplementBitsTerm = pevalDefaultComplementBitsTerm
  withSbvBitwiseTermConstraint r = withPrim @(IntN n) r
