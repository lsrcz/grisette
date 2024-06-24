{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.Internal.TH.Derivation
  ( DeriveTypeParamHandler (..),
    NatShouldBePositive (..),
    IsFPBits (..),
    PrimaryConstraint (..),
    SomeDeriveTypeParamHandler (..),
    DeriveStrategyHandler (..),
    Strategy (..),
    getStrategy,
    getClassName,
    deriveWithHandlers,
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
import Grisette.Internal.TH.Util
  ( dropNTypeParam,
    getTypeWithMaybeSubst,
    singleParamClassParamKind,
    substDataType,
  )
import Language.Haskell.TH
  ( Dec,
    DerivStrategy
      ( AnyclassStrategy,
        NewtypeStrategy,
        StockStrategy,
        ViaStrategy
      ),
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
    reifyDatatype,
  )
import Language.Haskell.TH.Datatype.TyVarBndr
  ( TyVarBndrUnit,
    tvKind,
    tvName,
  )

class DeriveTypeParamHandler handler where
  handleTypeParam ::
    handler ->
    [(TyVarBndrUnit, Maybe [Pred], Maybe Type)] ->
    Q [(TyVarBndrUnit, Maybe [Pred], Maybe Type)]
  handleBody :: handler -> [Type] -> Q [Pred]

data NatShouldBePositive = NatShouldBePositive

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
            return (Just [knownPred, geq1Pred], substTy)
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

data PrimaryConstraint = PrimaryConstraint Name Bool

instance DeriveTypeParamHandler PrimaryConstraint where
  handleTypeParam
    (PrimaryConstraint className ignoreIfAlreadyHandled)
    tys = do
      kind <- singleParamClassParamKind className
      mapM
        ( \(tv, preds, substTy) -> do
            (newPreds, newSubstTy) <- handle kind tv preds substTy
            return (tv, newPreds, newSubstTy)
        )
        tys
      where
        handle ::
          Kind ->
          TyVarBndrUnit ->
          Maybe [Pred] ->
          Maybe Type ->
          Q (Maybe [Pred], Maybe Type)
        handle _ _ (Just preds) substTy
          | ignoreIfAlreadyHandled =
              return (Just preds, substTy)
        handle kind tv preds substTy
          | tvKind tv == kind = do
              let t = getTypeWithMaybeSubst tv substTy
              cls <- [t|$(conT className) $t|]
              case preds of
                Nothing -> return (Just [cls], substTy)
                Just ps -> return (Just $ cls : ps, substTy)
        handle _ _ preds substTy = return (preds, substTy)
  handleBody (PrimaryConstraint _ _) _ = return []

data SomeDeriveTypeParamHandler where
  SomeDeriveTypeParamHandler ::
    (DeriveTypeParamHandler h) => h -> SomeDeriveTypeParamHandler

instance DeriveTypeParamHandler SomeDeriveTypeParamHandler where
  handleTypeParam (SomeDeriveTypeParamHandler h) = handleTypeParam h
  handleBody (SomeDeriveTypeParamHandler h) = handleBody h

data Strategy
  = Stock Name
  | WithNewtype Name
  | ViaDefault Name
  | ViaDefault1 Name
  | Anyclass Name
  deriving (Eq)

class DeriveStrategyHandler strategy where
  instanceDeclaration ::
    strategy -> [TyVarBndrUnit] -> [Pred] -> Type -> Q [Dec]

getStrategy :: Strategy -> Type -> Q DerivStrategy
getStrategy strategy ty =
  case strategy of
    Stock _ -> return StockStrategy
    WithNewtype _ -> return NewtypeStrategy
    ViaDefault _ ->
      ViaStrategy
        <$> [t|Default $(return ty)|]
    ViaDefault1 _ ->
      ViaStrategy
        <$> [t|Default1 $(return ty)|]
    Anyclass _ -> return AnyclassStrategy

getClassName :: Strategy -> Name
getClassName strategy =
  case strategy of
    Stock className -> className
    WithNewtype className -> className
    ViaDefault className -> className
    ViaDefault1 className -> className
    Anyclass className -> className

standaloneDeriveByStrategy :: Strategy -> [Pred] -> Type -> Q [Dec]
standaloneDeriveByStrategy strategy preds ty = do
  s <- getStrategy strategy ty
  (: [])
    <$> standaloneDerivWithStrategyD
      (Just s)
      (return preds)
      (appT (conT $ getClassName strategy) $ return ty)

instance DeriveStrategyHandler Strategy where
  instanceDeclaration strategy _ = standaloneDeriveByStrategy strategy

deriveWithHandlers ::
  (DeriveStrategyHandler strategy) =>
  [SomeDeriveTypeParamHandler] ->
  strategy ->
  Bool ->
  Int ->
  Name ->
  Q [Dec]
deriveWithHandlers
  handlers
  strategy
  ignoreBodyConstraints
  numDroppedTailTypes
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
      let fst3 (a, _, _) = a
      instanceDeclaration
        strategy
        (fst3 <$> tyVarsWithConstraints)
        allConstraints
        ty

deriveSimpleBuiltin :: Strategy -> Name -> Name -> Q [Dec]
deriveSimpleBuiltin strategy cls =
  deriveWithHandlers
    [SomeDeriveTypeParamHandler $ PrimaryConstraint cls False]
    strategy
    True
    0

deriveSimpleBuiltins :: Strategy -> Name -> [Name] -> Q [Dec]
deriveSimpleBuiltins strategy cls =
  fmap concat . traverse (deriveSimpleBuiltin strategy cls)

deriveSimpleBuiltin1 :: Strategy -> Name -> Name -> Name -> Q [Dec]
deriveSimpleBuiltin1 strategy cls cls1 =
  deriveWithHandlers
    [ SomeDeriveTypeParamHandler $ PrimaryConstraint cls False,
      SomeDeriveTypeParamHandler $ PrimaryConstraint cls1 False
    ]
    strategy
    True
    1

deriveSimpleBuiltin1s :: Strategy -> Name -> Name -> [Name] -> Q [Dec]
deriveSimpleBuiltin1s strategy cls cls1 =
  fmap concat . traverse (deriveSimpleBuiltin1 strategy cls cls1)

deriveFunctorArgBuiltin :: Strategy -> Name -> Name -> Name -> Q [Dec]
deriveFunctorArgBuiltin strategy cls cls1 =
  deriveWithHandlers
    [ SomeDeriveTypeParamHandler $ PrimaryConstraint cls False,
      SomeDeriveTypeParamHandler $ PrimaryConstraint cls1 False
    ]
    strategy
    True
    0

deriveFunctorArgBuiltins :: Strategy -> Name -> Name -> [Name] -> Q [Dec]
deriveFunctorArgBuiltins strategy cls cls1 =
  fmap concat . traverse (deriveFunctorArgBuiltin strategy cls cls1)
