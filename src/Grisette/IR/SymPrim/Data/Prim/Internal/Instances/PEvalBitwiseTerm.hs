{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.Prim.Internal.Instances.PEvalBitwiseTerm
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.Prim.Internal.Instances.PEvalBitwiseTerm () where

import Data.Bits (Bits (complement, xor, zeroBits, (.&.), (.|.)))
import Grisette.IR.SymPrim.Data.Prim.Internal.Term
  ( PEvalBitwiseTerm
      ( pevalAndBitsTerm,
        pevalComplementBitsTerm,
        pevalOrBitsTerm,
        pevalXorBitsTerm
      ),
    SupportedPrim,
    Term (ComplementBitsTerm, ConTerm),
    andBitsTerm,
    complementBitsTerm,
    conTerm,
    orBitsTerm,
    xorBitsTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.Internal.Unfold
  ( binaryUnfoldOnce,
    unaryUnfoldOnce,
  )

instance (Bits a, SupportedPrim a) => PEvalBitwiseTerm a where
  pevalAndBitsTerm = binaryUnfoldOnce doPevalAndBitsTerm andBitsTerm
    where
      doPevalAndBitsTerm (ConTerm _ a) (ConTerm _ b) = Just $ conTerm (a .&. b)
      doPevalAndBitsTerm (ConTerm _ a) b
        | a == zeroBits = Just $ conTerm zeroBits
        | a == complement zeroBits = Just b
      doPevalAndBitsTerm a (ConTerm _ b)
        | b == zeroBits = Just $ conTerm zeroBits
        | b == complement zeroBits = Just a
      doPevalAndBitsTerm a b | a == b = Just a
      doPevalAndBitsTerm _ _ = Nothing
  pevalOrBitsTerm = binaryUnfoldOnce doPevalOrBitsTerm orBitsTerm
    where
      doPevalOrBitsTerm (ConTerm _ a) (ConTerm _ b) = Just $ conTerm (a .|. b)
      doPevalOrBitsTerm (ConTerm _ a) b
        | a == zeroBits = Just b
        | a == complement zeroBits = Just $ conTerm $ complement zeroBits
      doPevalOrBitsTerm a (ConTerm _ b)
        | b == zeroBits = Just a
        | b == complement zeroBits = Just $ conTerm $ complement zeroBits
      doPevalOrBitsTerm a b | a == b = Just a
      doPevalOrBitsTerm _ _ = Nothing
  pevalXorBitsTerm = binaryUnfoldOnce doPevalXorBitsTerm xorBitsTerm
    where
      doPevalXorBitsTerm (ConTerm _ a) (ConTerm _ b) =
        Just $ conTerm (a `xor` b)
      doPevalXorBitsTerm (ConTerm _ a) b
        | a == zeroBits = Just b
        | a == complement zeroBits = Just $ pevalComplementBitsTerm b
      doPevalXorBitsTerm a (ConTerm _ b)
        | b == zeroBits = Just a
        | b == complement zeroBits = Just $ pevalComplementBitsTerm a
      doPevalXorBitsTerm a b | a == b = Just $ conTerm zeroBits
      doPevalXorBitsTerm (ComplementBitsTerm _ i) (ComplementBitsTerm _ j) =
        Just $ pevalXorBitsTerm i j
      doPevalXorBitsTerm (ComplementBitsTerm _ i) j =
        Just $ pevalComplementBitsTerm $ pevalXorBitsTerm i j
      doPevalXorBitsTerm i (ComplementBitsTerm _ j) =
        Just $ pevalComplementBitsTerm $ pevalXorBitsTerm i j
      doPevalXorBitsTerm _ _ = Nothing
  pevalComplementBitsTerm =
    unaryUnfoldOnce doPevalComplementBitsTerm complementBitsTerm
    where
      doPevalComplementBitsTerm (ConTerm _ a) = Just $ conTerm $ complement a
      doPevalComplementBitsTerm (ComplementBitsTerm _ a) = Just a
      doPevalComplementBitsTerm _ = Nothing
