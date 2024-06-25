{-# LANGUAGE TemplateHaskell #-}

module Grisette.Internal.TH.DeriveInstanceProvider
  ( DeriveInstanceProvider (..),
    Strategy (..),
  )
where

import Generics.Deriving (Default, Default1)
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
    Type,
    appT,
    conT,
    standaloneDerivWithStrategyD,
  )
import Language.Haskell.TH.Datatype.TyVarBndr (TyVarBndrUnit)

class DeriveInstanceProvider provider where
  instanceDeclaration ::
    provider -> [[(TyVarBndrUnit, Maybe Type)]] -> [Pred] -> [Type] -> Q [Dec]

data Strategy
  = Stock {strategyClassName :: Name}
  | WithNewtype {strategyClassName :: Name}
  | ViaDefault {strategyClassName :: Name}
  | ViaDefault1 {strategyClassName :: Name}
  | Anyclass {strategyClassName :: Name}
  deriving (Eq)

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

instance DeriveInstanceProvider Strategy where
  instanceDeclaration strategy _ preds tys = do
    s <- getStrategy strategy (last tys)
    (: [])
      <$> standaloneDerivWithStrategyD
        (Just s)
        (return preds)
        (foldl appT (conT $ strategyClassName strategy) $ return <$> tys)
