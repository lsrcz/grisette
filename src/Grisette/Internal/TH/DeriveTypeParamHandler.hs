{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :   Grisette.Internal.TH.DeriveTypeParamHandler
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
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

-- | A derive type param handler handles type parameters and provides
-- constraints or instantiations for them.
--
-- The first argument is the number of types that are zipped together. For
-- most classes, this is 1, but for some classes, like 'Grisette.ToCon', this is
-- 2.
--
-- The second argument is the handler itself.
--
-- The third argument is a list of type parameters and their constraints. Each
-- entry in the list corresponds to a type parameter of the datatype. The
-- first element in the pair is a list of zipped type parameters with possibly
-- concrete types. For example, if we are deriving 'Grisette.ToCon' for
-- `Either`, the argument will be:
--
-- > [([(e0, Nothing), (e1, Nothing)], Nothing),
-- >  ([(a0, Nothing), (a1, Nothing)], Nothing)]
--
-- We can see that the type parameters for the concrete and symbolic `Either`
-- types are zipped together: the first element of the list are for the error
-- types, and the second element of the list are for the value types.
--
-- The handler may concretize some types, or add constraints based on the type
-- parameters.
class DeriveTypeParamHandler handler where
  handleTypeParams ::
    Int ->
    handler ->
    [([(TyVarBndrUnit, Maybe Type)], Maybe [Pred])] ->
    Q [([(TyVarBndrUnit, Maybe Type)], Maybe [Pred])]
  handleBody :: handler -> [[Type]] -> Q [Pred]

-- | Ensures that type parameters with the kind 'Nat' are known and positive.
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

-- | Ensures that the type parameters are valid for floating point operations.
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

-- | Adds a primary constraint to the type parameters. It applies the class
-- to each type parameter that are zipped into a list, with the desired kinds.
--
-- For example, if we are deriving 'Grisette.ToCon' for `Either`, and the input
-- to this handler is as follows:
--
-- > [([(e0, Nothing), (e1, Nothing)], Nothing),
-- >  ([(a0, Nothing), (a1, Nothing)], Nothing)]
--
-- Then this will generate constraints for the type parameters of `Either`:
--
-- > [([(e0, Nothing), (e1, Nothing)], Just [ToCon e0 e1]),
-- >  ([(a0, Nothing), (a1, Nothing)], Just [ToCon a0 a1])]
--
-- Type parameters that are already handled by other handlers can be ignored.
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

-- | A type that can handle type parameters.
data SomeDeriveTypeParamHandler where
  SomeDeriveTypeParamHandler ::
    (DeriveTypeParamHandler handler) =>
    handler ->
    SomeDeriveTypeParamHandler

instance DeriveTypeParamHandler SomeDeriveTypeParamHandler where
  handleTypeParams n (SomeDeriveTypeParamHandler h) = handleTypeParams n h
  handleBody (SomeDeriveTypeParamHandler h) = handleBody h
