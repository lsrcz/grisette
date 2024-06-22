{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.Internal.TH.Derivation
  ( NatShouldBePositive (..),
    DeriveTypeParamHandler (..),
    IsFPBits (..),
    StarShouldBeConstrained (..),
    SomeDeriveTypeParamHandler (..),
    Strategy (..),
    deriveWithHandlers,
    simpleBuiltinInstanceHandlers,
    deriveSimpleBuiltin,
    deriveSimpleBuiltins,
    deriveSimpleBuiltin1,
    deriveSimpleBuiltin1s,
    deriveFunctorArgBuiltin,
    deriveFunctorArgBuiltins,
  )
where

import Control.Monad (foldM, when)
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import GHC.TypeNats (KnownNat, Nat, type (<=))
import Generics.Deriving (Default, Default1)
import Grisette.Internal.SymPrim.FP (ValidFP)
import Grisette.Internal.TH.Util (substDataType)
import Language.Haskell.TH
  ( Dec,
    DerivStrategy
      ( AnyclassStrategy,
        NewtypeStrategy,
        StockStrategy,
        ViaStrategy
      ),
    Name,
    Pred,
    Q,
    Type (AppT, ArrowT, ConT, StarT),
    appT,
    conT,
    standaloneDerivWithStrategyD,
    varT,
  )
import Language.Haskell.TH.Datatype
  ( ConstructorInfo (constructorFields),
    DatatypeInfo (datatypeCons, datatypeVars),
    datatypeType,
    reifyDatatype,
  )
import Language.Haskell.TH.Datatype.TyVarBndr
  ( TyVarBndrUnit,
    tvKind,
    tvName,
  )
import Language.Haskell.TH.Ppr (pprint)

class DeriveTypeParamHandler handler where
  handleTypeParam ::
    handler ->
    [(TyVarBndrUnit, Maybe [Pred], Maybe Type)] ->
    Q [(TyVarBndrUnit, Maybe [Pred], Maybe Type)]
  handleBody :: handler -> [Type] -> Q [Pred]

data NatShouldBePositive = NatShouldBePositive

getTypeWithMaybeSubst :: TyVarBndrUnit -> Maybe Type -> Q Type
getTypeWithMaybeSubst tv Nothing = varT $ tvName tv
getTypeWithMaybeSubst _ (Just t) = return t

instance DeriveTypeParamHandler NatShouldBePositive where
  handleTypeParam _ =
    mapM
      ( \(tv, preds, substTy) -> do
          (newPreds, newSubstTy) <- handle tv preds substTy
          return (tv, newPreds, newSubstTy)
      )
    where
      handle ::
        TyVarBndrUnit ->
        Maybe [Pred] ->
        Maybe Type ->
        Q (Maybe [Pred], Maybe Type)
      handle tv Nothing substTy
        | tvKind tv == ConT ''Nat = do
            let t = getTypeWithMaybeSubst tv substTy
            knownPred <- [t|KnownNat $t|]
            geq1Pred <- [t|1 <= $t|]
            return (Just [knownPred, geq1Pred], Nothing)
      handle _ preds ty = return (preds, ty)
  handleBody _ _ = return []

data IsFPBits = IsFPBits {ebIdx :: Int, sbIdx :: Int}

instance DeriveTypeParamHandler IsFPBits where
  handleTypeParam (IsFPBits ebIdx sbIdx) tys
    | ebIdx >= length tys =
        fail "IsValidFloat: ebIdx out of bounds"
    | sbIdx >= length tys =
        fail "IsValidFloat: sbIdx out of bounds"
    | otherwise = do
        let fst3 (a, _, _) = a
        let thd3 (_, _, c) = c
        let eb = tys !! ebIdx
        let ebt = getTypeWithMaybeSubst (fst3 eb) (thd3 eb)
        let sb = tys !! sbIdx
        let sbt = getTypeWithMaybeSubst (fst3 sb) (thd3 sb)
        validFloat <- [t|ValidFP $ebt $sbt|]
        return $
          zipWith
            ( curry
                ( \(i, (t, param, substTy)) ->
                    case param of
                      Nothing ->
                        if i == ebIdx
                          then (t, Just [validFloat], substTy)
                          else
                            if i == sbIdx
                              then (t, Just [], substTy)
                              else (t, Nothing, substTy)
                      Just params ->
                        if i == ebIdx
                          then (t, Just $ validFloat : params, substTy)
                          else (t, Just params, substTy)
                )
            )
            [0 ..]
            tys
  handleBody _ _ = return []

data StarShouldBeConstrained = StarShouldBeConstrained Name Bool

instance DeriveTypeParamHandler StarShouldBeConstrained where
  handleTypeParam (StarShouldBeConstrained className ignoreIfAlreadyHandled) =
    mapM
      ( \(tv, preds, substTy) -> do
          (newPreds, newSubstTy) <- handle tv preds substTy
          return (tv, newPreds, newSubstTy)
      )
    where
      handle ::
        TyVarBndrUnit ->
        Maybe [Pred] ->
        Maybe Type ->
        Q (Maybe [Pred], Maybe Type)
      handle _ (Just preds) substTy
        | ignoreIfAlreadyHandled =
            return (Just preds, substTy)
      handle tv preds substTy
        | tvKind tv == StarT = do
            let t = getTypeWithMaybeSubst tv substTy
            cls <- [t|$(conT className) $t|]
            case preds of
              Nothing -> return (Just [cls], substTy)
              Just ps -> return (Just $ cls : ps, substTy)
      handle _ preds substTy = return (preds, substTy)
  handleBody (StarShouldBeConstrained _ _) _ = return []

data StarToStarShouldBeConstrained = StarToStarShouldBeConstrained Name Bool

instance DeriveTypeParamHandler StarToStarShouldBeConstrained where
  handleTypeParam
    (StarToStarShouldBeConstrained className ignoreIfAlreadyHandled) =
      mapM
        ( \(tv, preds, substTy) -> do
            (newPreds, newSubstTy) <- handle tv preds substTy
            return (tv, newPreds, newSubstTy)
        )
      where
        handle ::
          TyVarBndrUnit ->
          Maybe [Pred] ->
          Maybe Type ->
          Q (Maybe [Pred], Maybe Type)
        handle _ (Just preds) substTy
          | ignoreIfAlreadyHandled =
              return (Just preds, substTy)
        handle tv preds substTy
          | tvKind tv == AppT (AppT ArrowT StarT) StarT = do
              let t = getTypeWithMaybeSubst tv substTy
              cls <- [t|$(conT className) $t|]
              case preds of
                Nothing -> return (Just [cls], substTy)
                Just ps -> return (Just $ cls : ps, substTy)
        handle _ preds substTy = return (preds, substTy)
  handleBody (StarToStarShouldBeConstrained _ _) _ = return []

data SomeDeriveTypeParamHandler where
  SomeDeriveTypeParamHandler ::
    (DeriveTypeParamHandler h) => h -> SomeDeriveTypeParamHandler

instance DeriveTypeParamHandler SomeDeriveTypeParamHandler where
  handleTypeParam (SomeDeriveTypeParamHandler h) = handleTypeParam h
  handleBody (SomeDeriveTypeParamHandler h) = handleBody h

data Strategy = Stock | WithNewtype | ViaDefault | ViaDefault1 | Anyclass
  deriving (Eq)

dropLastTypeParam :: Type -> Q Type
dropLastTypeParam (AppT c _) = return c
dropLastTypeParam v =
  fail $
    "dropLastTypeParam: have no type parameters: "
      <> pprint v
      <> " / "
      <> show v

dropNTypeParam :: Int -> Type -> Q Type
dropNTypeParam 0 t = return t
dropNTypeParam n t = dropLastTypeParam t >>= dropNTypeParam (n - 1)

deriveWithHandlers ::
  [SomeDeriveTypeParamHandler] ->
  Strategy ->
  Bool ->
  Int ->
  Name ->
  Name ->
  Q [Dec]
deriveWithHandlers
  handlers
  strategy
  ignoreBodyConstraints
  numDroppedTailTypes
  cls
  name =
    do
      when (numDroppedTailTypes < 0) $
        fail "deriveWithHandlers: numDroppedTailTypes must be non-negative"
      when (numDroppedTailTypes > 0 && not ignoreBodyConstraints) $
        fail $
          "deriveWithHandlers: ignoreBodyConstraints must be True if "
            <> "numDroppedTailTypes > 0"
      d <- reifyDatatype name
      let tyVars = reverse $ drop numDroppedTailTypes $ reverse $ datatypeVars d
      tyVarsWithConstraints <-
        foldM
          (flip handleTypeParam)
          ((,Nothing,Nothing) <$> tyVars)
          handlers
      let snd3 (_, b, _) = b
      let allTyVarsConstraints =
            concatMap (fromMaybe [] . snd3) tyVarsWithConstraints
      allConstraints <-
        ( if ignoreBodyConstraints
            then return allTyVarsConstraints
            else do
              let cons = datatypeCons d
              let allFields = nubOrd $ concatMap constructorFields cons
              bodyConstraints <- concat <$> mapM (`handleBody` allFields) handlers
              return $ allTyVarsConstraints ++ bodyConstraints
          )
      let substMap =
            M.fromList $
              mapMaybe
                ( \(tv, _, t) -> do
                    substTy <- t
                    return (tvName tv, substTy)
                )
                tyVarsWithConstraints
      ty <-
        dropNTypeParam numDroppedTailTypes $
          datatypeType $
            substDataType d substMap
      deriveStrategy <- getStrategy ty
      deriv <-
        standaloneDerivWithStrategyD
          (Just deriveStrategy)
          (return allConstraints)
          (appT (conT cls) $ return ty)
      return [deriv]
    where
      getStrategy ty =
        case strategy of
          Stock -> return StockStrategy
          WithNewtype -> return NewtypeStrategy
          ViaDefault ->
            ViaStrategy
              <$> [t|Default $(return ty)|]
          ViaDefault1 ->
            ViaStrategy
              <$> [t|Default1 $(return ty)|]
          Anyclass -> return AnyclassStrategy

simpleBuiltinInstanceHandlers :: Name -> [SomeDeriveTypeParamHandler]
simpleBuiltinInstanceHandlers cls =
  [SomeDeriveTypeParamHandler $ StarShouldBeConstrained cls False]

simple1BuiltinInstanceHandlers :: Name -> Name -> [SomeDeriveTypeParamHandler]
simple1BuiltinInstanceHandlers cls cls1 =
  [ SomeDeriveTypeParamHandler $ StarShouldBeConstrained cls False,
    SomeDeriveTypeParamHandler $ StarToStarShouldBeConstrained cls1 False
  ]

functorArgBuiltinInstanceHandlers ::
  Name -> Name -> [SomeDeriveTypeParamHandler]
functorArgBuiltinInstanceHandlers cls cls1 =
  [ SomeDeriveTypeParamHandler $ StarShouldBeConstrained cls False,
    SomeDeriveTypeParamHandler $ StarToStarShouldBeConstrained cls1 False
  ]

deriveSimpleBuiltin :: Strategy -> Name -> Name -> Q [Dec]
deriveSimpleBuiltin strategy cls =
  deriveWithHandlers
    (simpleBuiltinInstanceHandlers cls)
    strategy
    True
    0
    cls

deriveSimpleBuiltins :: Strategy -> Name -> [Name] -> Q [Dec]
deriveSimpleBuiltins strategy cls =
  fmap concat . traverse (deriveSimpleBuiltin strategy cls)

deriveSimpleBuiltin1 :: Strategy -> Name -> Name -> Name -> Q [Dec]
deriveSimpleBuiltin1 strategy cls cls1 =
  deriveWithHandlers
    (simple1BuiltinInstanceHandlers cls cls1)
    strategy
    True
    1
    cls1

deriveSimpleBuiltin1s :: Strategy -> Name -> Name -> [Name] -> Q [Dec]
deriveSimpleBuiltin1s strategy cls cls1 =
  fmap concat . traverse (deriveSimpleBuiltin1 strategy cls cls1)

deriveFunctorArgBuiltin :: Strategy -> Name -> Name -> Name -> Q [Dec]
deriveFunctorArgBuiltin strategy cls cls1 =
  deriveWithHandlers
    (functorArgBuiltinInstanceHandlers cls cls1)
    strategy
    True
    0
    cls

deriveFunctorArgBuiltins :: Strategy -> Name -> Name -> [Name] -> Q [Dec]
deriveFunctorArgBuiltins strategy cls cls1 =
  fmap concat . traverse (deriveFunctorArgBuiltin strategy cls cls1)
