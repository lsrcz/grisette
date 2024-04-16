{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.IR.SymPrim.Data.Prim.Internal.Instances.PEvalOrdTerm
  ( pevalGeneralLtOrdTerm,
    pevalGeneralLeOrdTerm,
  )
where

import Control.Monad (msum)
import GHC.TypeNats (KnownNat, type (<=))
import Grisette.Core.Data.BV (IntN, WordN)
import Grisette.IR.SymPrim.Data.Prim.Internal.Instances.PEvalNumTerm ()
import Grisette.IR.SymPrim.Data.Prim.Internal.Term
  ( PEvalNumTerm (pevalNegNumTerm),
    PEvalOrdTerm (pevalLeOrdTerm, pevalLtOrdTerm),
    Term (AddNumTerm, ConTerm),
    conTerm,
    ltOrdTerm,
    pevalSubNumTerm, leOrdTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Unfold (binaryUnfoldOnce)

-- Lt
pevalGeneralLtOrdTerm :: (PEvalOrdTerm a) => Term a -> Term a -> Term Bool
pevalGeneralLtOrdTerm = binaryUnfoldOnce doPevalGeneralLtOrdTerm ltOrdTerm

doPevalGeneralLtOrdTerm ::
  (PEvalOrdTerm a) => Term a -> Term a -> Maybe (Term Bool)
doPevalGeneralLtOrdTerm (ConTerm _ a) (ConTerm _ b) = Just $ conTerm $ a < b
doPevalGeneralLtOrdTerm _ _ = Nothing

-- Le
pevalGeneralLeOrdTerm :: (PEvalOrdTerm a) => Term a -> Term a -> Term Bool
pevalGeneralLeOrdTerm = binaryUnfoldOnce doPevalGeneralLeOrdTerm leOrdTerm

doPevalGeneralLeOrdTerm ::
  (PEvalOrdTerm a) => Term a -> Term a -> Maybe (Term Bool)
doPevalGeneralLeOrdTerm (ConTerm _ a) (ConTerm _ b) = Just $ conTerm $ a <= b
doPevalGeneralLeOrdTerm _ _ = Nothing

instance PEvalOrdTerm Integer where
  pevalLtOrdTerm = binaryUnfoldOnce doPevalLtOrdTerm ltOrdTerm
    where
      doPevalLtOrdTerm l r =
        msum
          [ doPevalGeneralLtOrdTerm l r,
            case (l, r) of
              (ConTerm _ l, AddNumTerm _ (ConTerm _ j) k) ->
                Just $ pevalLtOrdTerm (conTerm $ l - j) k
              (AddNumTerm _ (ConTerm _ i) j, ConTerm _ k) ->
                Just $ pevalLtOrdTerm j (conTerm $ k - i)
              ((AddNumTerm _ (ConTerm _ j) k), l) ->
                Just $
                  pevalLtOrdTerm
                    (conTerm j)
                    (pevalSubNumTerm l k)
              (j, (AddNumTerm _ (ConTerm _ k) l)) ->
                Just $ pevalLtOrdTerm (conTerm $ -k) (pevalSubNumTerm l j)
              (l, ConTerm _ r) ->
                Just $ pevalLtOrdTerm (conTerm $ -r) (pevalNegNumTerm l)
              _ -> Nothing
          ]
  pevalLeOrdTerm = binaryUnfoldOnce doPevalLeOrdTerm leOrdTerm
    where
      doPevalLeOrdTerm l r =
        msum
          [ doPevalGeneralLeOrdTerm l r,
            case (l, r) of
              (ConTerm _ l, AddNumTerm _ (ConTerm _ j) k) ->
                Just $ pevalLeOrdTerm (conTerm $ l - j) k
              (AddNumTerm _ (ConTerm _ i) j, ConTerm _ k) ->
                Just $ pevalLeOrdTerm j (conTerm $ k - i)
              (AddNumTerm _ (ConTerm _ j) k, l) ->
                Just $ pevalLeOrdTerm (conTerm j) (pevalSubNumTerm l k)
              (j, AddNumTerm _ (ConTerm _ k) l) ->
                Just $ pevalLeOrdTerm (conTerm $ -k) (pevalSubNumTerm l j)
              (l, ConTerm _ r) ->
                Just $ pevalLeOrdTerm (conTerm $ -r) (pevalNegNumTerm l)
              _ -> Nothing
          ]

instance (KnownNat n, 1 <= n) => PEvalOrdTerm (WordN n) where
  pevalLtOrdTerm = pevalGeneralLtOrdTerm
  pevalLeOrdTerm = pevalGeneralLeOrdTerm

instance (KnownNat n, 1 <= n) => PEvalOrdTerm (IntN n) where
  pevalLtOrdTerm = pevalGeneralLtOrdTerm
  pevalLeOrdTerm = pevalGeneralLeOrdTerm
