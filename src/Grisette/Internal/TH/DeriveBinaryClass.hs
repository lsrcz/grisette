{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.Internal.TH.DeriveBinaryClass
  ( deriveSimpleConversion,
    deriveSimpleConversions,
    deriveSimpleConversion1,
    deriveSimpleConversion1s,
  )
where

import Control.Monad (foldM, when)
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import GHC.TypeNats (KnownNat, Nat, type (<=))
import Grisette.Internal.TH.Derivation
  ( NatShouldBePositive,
    PrimaryConstraint (PrimaryConstraint),
    Strategy,
    getClassName,
    getStrategy,
  )
import Grisette.Internal.TH.Util
  ( binaryClassParamKind,
    dropNTypeParam,
    getTypeWithMaybeSubst,
    reifyDatatypeWithFreshNames,
    substDataType,
  )
import Language.Haskell.TH
  ( Dec,
    Kind,
    Name,
    Pred,
    Q,
    Type (ConT),
    appT,
    conT,
    standaloneDerivWithStrategyD,
  )
import Language.Haskell.TH.Datatype
  ( ConstructorInfo (constructorFields),
    DatatypeInfo (datatypeCons, datatypeVars),
    datatypeType,
  )
import Language.Haskell.TH.Datatype.TyVarBndr
  ( TyVarBndrUnit,
    tvKind,
    tvName,
  )

class DeriveBinaryTypeParamHandler handler where
  handleTypeParam ::
    handler ->
    [(TyVarBndrUnit, TyVarBndrUnit, Maybe [Pred], Maybe Type, Maybe Type)] ->
    Q [(TyVarBndrUnit, TyVarBndrUnit, Maybe [Pred], Maybe Type, Maybe Type)]
  handleBody :: handler -> [Type] -> [Type] -> Q [Pred]

instance DeriveBinaryTypeParamHandler NatShouldBePositive where
  handleTypeParam _ =
    mapM
      ( \(tv0, tv1, preds, substTy0, substTy1) -> do
          (newPreds, newSubstTy0, newSubstTy1) <-
            handle tv0 tv1 preds substTy0 substTy1
          return (tv0, tv1, newPreds, newSubstTy0, newSubstTy1)
      )
    where
      handle ::
        TyVarBndrUnit ->
        TyVarBndrUnit ->
        Maybe [Pred] ->
        Maybe Type ->
        Maybe Type ->
        Q (Maybe [Pred], Maybe Type, Maybe Type)
      handle tv0 tv1 _ _ _
        | tv0 /= tv1 =
            fail "NatShouldBePositive: type parameters not aligned"
      handle tv0 tv1 Nothing substTy0 substTy1
        | tvKind tv0 == ConT ''Nat = do
            let t0 = getTypeWithMaybeSubst tv0 substTy0
            let t1 = getTypeWithMaybeSubst tv1 substTy1
            knownPred <- [t|KnownNat $t0|]
            geq1Pred <- [t|1 <= $t0|]
            tyeqPred <- [t|$t0 ~ $t1|]
            return (Just [knownPred, geq1Pred, tyeqPred], substTy0, substTy1)
      handle _ _ preds ty0 ty1 = return (preds, ty0, ty1)
  handleBody _ _ _ = return []

instance DeriveBinaryTypeParamHandler PrimaryConstraint where
  handleTypeParam
    (PrimaryConstraint className ignoreIfAlreadyHandled)
    tys = do
      kind <- binaryClassParamKind className
      mapM
        ( \(tv0, tv1, preds, substTy0, substTy1) -> do
            (newPreds, newSubstTy0, newSubstTy1) <-
              handle kind tv0 tv1 preds substTy0 substTy1
            return (tv0, tv1, newPreds, newSubstTy0, newSubstTy1)
        )
        tys
      where
        handle ::
          Kind ->
          TyVarBndrUnit ->
          TyVarBndrUnit ->
          Maybe [Pred] ->
          Maybe Type ->
          Maybe Type ->
          Q (Maybe [Pred], Maybe Type, Maybe Type)
        handle _ _ _ (Just preds) substTy0 substTy1
          | ignoreIfAlreadyHandled =
              return (Just preds, substTy0, substTy1)
        handle kind tv0 tv1 preds substTy0 substTy1
          | tvKind tv0 == kind = do
              let t0 = getTypeWithMaybeSubst tv0 substTy0
              let t1 = getTypeWithMaybeSubst tv1 substTy1
              cls <- [t|$(conT className) $t0 $t1|]
              case preds of
                Nothing -> return (Just [cls], substTy0, substTy1)
                Just ps -> return (Just $ cls : ps, substTy0, substTy1)
        handle _ _ _ preds substTy0 substTy1 =
          return (preds, substTy0, substTy1)
  handleBody (PrimaryConstraint _ _) _ _ = return []

data SomeBinaryDeriveTypeParamHandler where
  SomeBinaryDeriveTypeParamHandler ::
    (DeriveBinaryTypeParamHandler h) => h -> SomeBinaryDeriveTypeParamHandler

instance DeriveBinaryTypeParamHandler SomeBinaryDeriveTypeParamHandler where
  handleTypeParam (SomeBinaryDeriveTypeParamHandler h) = handleTypeParam h
  handleBody (SomeBinaryDeriveTypeParamHandler h) = handleBody h

class DeriveBinaryStrategyHandler strategy where
  binaryInstanceDeclaration ::
    strategy ->
    [TyVarBndrUnit] ->
    [TyVarBndrUnit] ->
    [Pred] ->
    Type ->
    Type ->
    Q [Dec]

standaloneDeriveBinaryByStrategy ::
  Strategy -> [Pred] -> Type -> Type -> Q [Dec]
standaloneDeriveBinaryByStrategy strategy preds ty0 ty1 = do
  s <- getStrategy strategy ty1
  (: [])
    <$> standaloneDerivWithStrategyD
      (Just s)
      (return preds)
      (appT (appT (conT $ getClassName strategy) $ return ty0) $ return ty1)

instance DeriveBinaryStrategyHandler Strategy where
  binaryInstanceDeclaration strategy _ _ =
    standaloneDeriveBinaryByStrategy strategy

deriveBinaryWithHandlers ::
  (DeriveBinaryStrategyHandler strategy) =>
  [SomeBinaryDeriveTypeParamHandler] ->
  strategy ->
  Bool ->
  Int ->
  Name ->
  Q [Dec]
deriveBinaryWithHandlers
  handlers
  strategy
  ignoreBodyConstraints
  numDroppedTailTypes
  name =
    do
      when (numDroppedTailTypes < 0) $
        fail "deriveBinaryHandlers: numDroppedTailTypes must be non-negative"
      when (numDroppedTailTypes > 0 && not ignoreBodyConstraints) $
        fail $
          "deriveBinaryHandlers: ignoreBodyConstraints must be True if "
            <> "numDroppedTailTypes > 0"
      dfrom <- reifyDatatypeWithFreshNames name
      dto <- reifyDatatypeWithFreshNames name
      let tyVarsFrom =
            reverse $ drop numDroppedTailTypes $ reverse $ datatypeVars dfrom
      let tyVarsTo =
            reverse $ drop numDroppedTailTypes $ reverse $ datatypeVars dto
      tyVarsWithConstraints <-
        foldM
          (flip handleTypeParam)
          ( zipWith
              (\tfrom tto -> (tfrom, tto, Nothing, Nothing, Nothing))
              tyVarsFrom
              tyVarsTo
          )
          handlers
      let trd5 (_, _, c, _, _) = c
      let allTyVarsConstraints =
            concatMap (fromMaybe [] . trd5) tyVarsWithConstraints
      allConstraints <-
        ( if ignoreBodyConstraints
            then return allTyVarsConstraints
            else do
              let consFrom = datatypeCons dfrom
              let allFieldsFrom = nubOrd $ concatMap constructorFields consFrom
              let consTo = datatypeCons dto
              let allFieldsTo = nubOrd $ concatMap constructorFields consTo
              bodyConstraints <-
                concat
                  <$> mapM
                    (\handler -> handleBody handler allFieldsFrom allFieldsTo)
                    handlers
              return $ allTyVarsConstraints ++ bodyConstraints
          )
      let substMapFrom =
            M.fromList $
              mapMaybe
                ( \(tv0, _, _, t0, _) -> do
                    substTy <- t0
                    return (tvName tv0, substTy)
                )
                tyVarsWithConstraints
      let substMapTo =
            M.fromList $
              mapMaybe
                ( \(_, tv1, _, _, t1) -> do
                    substTy <- t1
                    return (tvName tv1, substTy)
                )
                tyVarsWithConstraints
      tyFrom <-
        dropNTypeParam numDroppedTailTypes $
          datatypeType $
            substDataType dfrom substMapFrom
      tyTo <-
        dropNTypeParam numDroppedTailTypes $
          datatypeType $
            substDataType dto substMapTo
      let fst5 (a, _, _, _, _) = a
      let snd5 (_, b, _, _, _) = b
      binaryInstanceDeclaration
        strategy
        (fst5 <$> tyVarsWithConstraints)
        (snd5 <$> tyVarsWithConstraints)
        allConstraints
        tyFrom
        tyTo

deriveSimpleConversion :: Strategy -> Name -> Name -> Q [Dec]
deriveSimpleConversion strategy cls =
  deriveBinaryWithHandlers
    [SomeBinaryDeriveTypeParamHandler $ PrimaryConstraint cls False]
    strategy
    True
    0

deriveSimpleConversions :: Strategy -> Name -> [Name] -> Q [Dec]
deriveSimpleConversions strategy cls =
  fmap concat . traverse (deriveSimpleConversion strategy cls)

deriveSimpleConversion1 :: Strategy -> Name -> Name -> Name -> Q [Dec]
deriveSimpleConversion1 strategy cls cls1 =
  deriveBinaryWithHandlers
    [ SomeBinaryDeriveTypeParamHandler $ PrimaryConstraint cls False,
      SomeBinaryDeriveTypeParamHandler $ PrimaryConstraint cls1 False
    ]
    strategy
    True
    1

deriveSimpleConversion1s :: Strategy -> Name -> Name -> [Name] -> Q [Dec]
deriveSimpleConversion1s strategy cls cls1 =
  fmap concat . traverse (deriveSimpleConversion1 strategy cls cls1)
