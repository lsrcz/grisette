{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.Internal.TH.DeriveTypeParamHandler
  ( DeriveTypeParamHandler (..),
    NatShouldBePositive (..),
    IsFPBits (..),
    PrimaryConstraint (..),
    SomeDeriveTypeParamHandler (..),
  )
where

import GHC.TypeLits (KnownNat, Nat, type (<=))
import Grisette.Internal.SymPrim.FP (ValidFP)
import Grisette.Internal.TH.Util
  ( allSameKind,
    classParamKinds,
    concatPreds,
    getTypeWithMaybeSubst,
  )
import Language.Haskell.TH (Kind, Name, Pred, Q, Type (ConT), appT, conT)
import Language.Haskell.TH.Datatype.TyVarBndr (TyVarBndrUnit, tvKind)

class DeriveTypeParamHandler handler where
  handleTypeParams ::
    Int ->
    handler ->
    [([(TyVarBndrUnit, Maybe Type)], Maybe [Pred])] ->
    Q [([(TyVarBndrUnit, Maybe Type)], Maybe [Pred])]
  handleBody :: handler -> [[Type]] -> Q [Pred]

data NatShouldBePositive = NatShouldBePositive

instance DeriveTypeParamHandler NatShouldBePositive where
  handleTypeParams _ _ = mapM (uncurry handle)
    where
      handle ::
        [(TyVarBndrUnit, Maybe Type)] ->
        Maybe [Pred] ->
        Q ([(TyVarBndrUnit, Maybe Type)], Maybe [Pred])
      handle [] preds = return ([], preds)
      handle tys _
        | not (allSameKind (map fst tys)) =
            fail "NatShouldBePositive: All type parameters must be aligned "
      handle (ty : tys) Nothing
        | tvKind (fst ty) == ConT ''Nat = do
            let (t : ts) = map (uncurry getTypeWithMaybeSubst) $ ty : tys
            knownPred <- [t|KnownNat $t|]
            geq1Pred <- [t|1 <= $t|]
            eqPreds <- mapM (\t' -> [t|$t ~ $t'|]) ts
            return (ty : tys, Just $ knownPred : geq1Pred : eqPreds)
      handle tys preds = return (tys, preds)
  handleBody _ _ = return []

data IsFPBits = IsFPBits {ebIdx :: Int, sbIdx :: Int}

instance DeriveTypeParamHandler IsFPBits where
  handleTypeParams _ (IsFPBits ebIdx sbIdx) tys
    | ebIdx >= length tys =
        fail "IsFPBits: ebIdx out of bounds"
    | sbIdx >= length tys =
        fail "IsFPBits: sbIdx out of bounds"
    | otherwise = do
        let eb = tys !! ebIdx
        let ebts = map (uncurry getTypeWithMaybeSubst) (fst eb)
        let sb = tys !! sbIdx
        let sbts = map (uncurry getTypeWithMaybeSubst) (fst sb)
        case (ebts, sbts) of
          _
            | length ebts /= length sbts ->
                fail $
                  "IsFPBits: eb and sb must have the same number of type "
                    <> "parameters. This might happen because of a bug in "
                    <> "Grisette"
            | not (allSameKind (fst <$> fst eb)) ->
                fail "IsFPBits: All type parameters must be aligned"
            | not (allSameKind (fst <$> fst sb)) ->
                fail "IsFPBits: All type parameters must be aligned"
          ([], []) -> return tys
          ((et : ets), (st : sts)) -> do
            validFloat <- [t|ValidFP $et $st|]
            eqebPreds <- mapM (\et' -> [t|$et ~ $et'|]) ets
            eqsbPreds <- mapM (\st' -> [t|$st ~ $st'|]) sts
            return $
              zipWith
                ( \i (ts, preds) ->
                    if i == ebIdx
                      then
                        ( ts,
                          concatPreds
                            (Just $ validFloat : eqebPreds ++ eqsbPreds)
                            preds
                        )
                      else
                        if i == sbIdx
                          then (ts, concatPreds (Just []) preds)
                          else (ts, preds)
                )
                [0 ..]
                tys
          _ -> fail "IsFPBits: This should never happen"
  handleBody _ _ = return []

data PrimaryConstraint = PrimaryConstraint
  { className :: Name,
    ignoreIfAlreadyHandled :: Bool
  }

instance DeriveTypeParamHandler PrimaryConstraint where
  handleTypeParams
    _
    (PrimaryConstraint className ignoreIfAlreadyHandled)
    tys = do
      kinds <- classParamKinds className
      mapM (uncurry (handle kinds)) tys
      where
        handle ::
          [Kind] ->
          [(TyVarBndrUnit, Maybe Type)] ->
          Maybe [Pred] ->
          Q ([(TyVarBndrUnit, Maybe Type)], Maybe [Pred])
        handle _ [] preds = return ([], preds)
        handle _ tys _
          | not (allSameKind (map fst tys)) =
              fail "PrimaryConstraint: All type parameters must be aligned"
        handle _ tys (Just preds)
          | ignoreIfAlreadyHandled =
              return (tys, Just preds)
        handle kinds tys preds
          | (tvKind . fst <$> tys) == kinds = do
              ts <- mapM (uncurry getTypeWithMaybeSubst) tys
              cls <- foldl appT (conT className) $ return <$> ts
              return (tys, concatPreds (Just [cls]) preds)
        handle _ tys preds = return (tys, preds)
  handleBody (PrimaryConstraint _ _) _ = return []

data SomeDeriveTypeParamHandler where
  SomeDeriveTypeParamHandler ::
    (DeriveTypeParamHandler handler) =>
    handler ->
    SomeDeriveTypeParamHandler

instance DeriveTypeParamHandler SomeDeriveTypeParamHandler where
  handleTypeParams n (SomeDeriveTypeParamHandler h) = handleTypeParams n h
  handleBody (SomeDeriveTypeParamHandler h) = handleBody h
