{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.Internal.TH.DeriveUnifiedInterface
  ( TypeableMode (..),
    PrimaryUnifiedConstraint (..),
    UnifiedInstance (..),
    deriveUnifiedInterfaceExtra,
    deriveUnifiedInterface,
    deriveUnifiedInterfaces,
    deriveUnifiedInterface1Extra,
    deriveUnifiedInterface1,
    deriveUnifiedInterface1s,
    deriveFunctorArgUnifiedInterfaceExtra,
    deriveFunctorArgUnifiedInterface,
    deriveFunctorArgUnifiedInterfaces,
  )
where

import Control.Monad (unless)
import Data.Typeable (Typeable)
import Grisette.Internal.TH.DeriveInstanceProvider
  ( DeriveInstanceProvider (instanceDeclaration),
  )
import Grisette.Internal.TH.DeriveTypeParamHandler
  ( DeriveTypeParamHandler (handleBody, handleTypeParams),
    NatShouldBePositive (NatShouldBePositive),
    SomeDeriveTypeParamHandler (SomeDeriveTypeParamHandler),
  )
import Grisette.Internal.TH.DeriveWithHandlers (deriveWithHandlers)
import Grisette.Internal.TH.Util
  ( allSameKind,
    classParamKinds,
    concatPreds,
    getTypeWithMaybeSubst,
    tvIsMode,
    tvIsStar,
    tvIsStarToStar,
  )
import Grisette.Unified.Internal.EvaluationMode (EvaluationMode)
import Grisette.Unified.Internal.Util (withMode)
import Language.Haskell.TH
  ( Dec,
    Exp,
    Inline (Inline),
    Kind,
    Name,
    Phases (AllPhases),
    Pred,
    Q,
    RuleMatch (FunLike),
    Type (ConT),
    appT,
    conT,
    instanceD,
    lam1E,
    newName,
    normalB,
    pragInlD,
    valD,
    varE,
    varP,
  )
import Language.Haskell.TH.Datatype.TyVarBndr (TyVarBndrUnit, kindedTV, tvKind)

data TypeableMode = TypeableMode

instance DeriveTypeParamHandler TypeableMode where
  handleTypeParams n _ tys = do
    unless (n == 1) $
      fail $
        "TypeableMode: unified type class should have exactly one type "
          <> "parameter"
    let numModeParam = length $ (filter (tvIsMode . fst . head)) $ fst <$> tys
    newTys <-
      if numModeParam == 0
        then do
          nm <- newName "mode"
          return $
            ( [(kindedTV nm (ConT ''EvaluationMode), Nothing)],
              Nothing
            )
              : tys
        else
          if numModeParam == 1
            then return tys
            else fail "TypeableMode: multiple mode type variables found"
    mapM (uncurry handleMode) newTys
    where
      handleMode ::
        [(TyVarBndrUnit, Maybe Type)] ->
        Maybe [Pred] ->
        Q ([(TyVarBndrUnit, Maybe Type)], Maybe [Pred])
      handleMode [(tv, substTy)] preds | tvIsMode tv = do
        typeable <- [t|Typeable $(getTypeWithMaybeSubst tv substTy)|]
        return ([(tv, substTy)], concatPreds (Just [typeable]) preds)
      handleMode tys preds = return (tys, preds)
  handleBody _ _ = return []

data PrimaryUnifiedConstraint = PrimaryUnifiedConstraint Name Bool

instance DeriveTypeParamHandler PrimaryUnifiedConstraint where
  handleTypeParams
    n
    (PrimaryUnifiedConstraint className ignoreIfAlreadyHandled)
    tys = do
      unless (n == 1) $
        fail $
          "TypeableMode: unified type class should have exactly one type "
            <> "parameter"
      kinds <- classParamKinds className
      let modes = filter (tvIsMode . fst . head) $ fst <$> tys
      case modes of
        [] -> fail "PrimaryUnifiedConstraint: No mode type variable found"
        [[md]] -> do
          mdTy <- uncurry getTypeWithMaybeSubst md
          mapM (uncurry $ handle kinds mdTy) tys
        [_] ->
          fail "PrimaryUnifiedConstraint: multiple mode type variables found"
        _ ->
          fail "PrimaryUnifiedConstraint: multiple mode type variables found"
      where
        handle ::
          [Kind] ->
          Type ->
          [(TyVarBndrUnit, Maybe Type)] ->
          Maybe [Pred] ->
          Q ([(TyVarBndrUnit, Maybe Type)], Maybe [Pred])
        handle _ _ [] preds = return ([], preds)
        handle _ _ tys (Just preds)
          | ignoreIfAlreadyHandled =
              return (tys, Just preds)
        handle _ _ tys _
          | not (allSameKind (map fst tys)) =
              fail
                "PrimaryUnifiedConstraint: All type parameters must be aligned"
        handle kinds modety tys preds
          | ConT ''EvaluationMode : (tvKind . fst <$> tys) == kinds = do
              ts <- mapM (uncurry getTypeWithMaybeSubst) tys
              cls <-
                foldl appT (appT (conT className) (return modety)) $
                  return <$> ts
              return (tys, concatPreds (Just [cls]) preds)
        handle _ _ tys preds = return (tys, preds)
  handleBody (PrimaryUnifiedConstraint _ _) _ = return []

data UnifiedInstance = UnifiedInstance
  { _cls :: Name,
    _clsWithFunc :: Name,
    _withFunc :: Name,
    _withFunc1 :: Maybe Name
  }

instance DeriveInstanceProvider UnifiedInstance where
  instanceDeclaration
    (UnifiedInstance cls clsWithFunc withFunc maybeWithFunc1)
    tys'
    ctx
    ty' = do
      unless (all ((== 1) . length) tys') $
        fail "UnifiedInstance: only support classes with one type parameter"
      unless (length ty' == 1) $
        fail "UnifiedInstance: only support classes with one type parameter"
      let tys = head <$> tys'
      let modes =
            map (uncurry getTypeWithMaybeSubst) $ filter (tvIsMode . fst) tys
      let stars =
            map (uncurry getTypeWithMaybeSubst) $ filter (tvIsStar . fst) tys
      let starToStars =
            map (uncurry getTypeWithMaybeSubst) $
              filter (tvIsStarToStar . fst) tys
      case modes of
        [] -> fail "UnifiedInstance: no mode type variables found"
        [md] -> do
          sequence
            [ instanceD
                (return ctx)
                [t|$(conT cls) $md $(return $ head ty')|]
                [ body md clsWithFunc withFunc stars maybeWithFunc1 starToStars,
                  pragInlD clsWithFunc Inline FunLike AllPhases
                ]
            ]
        _ -> fail "UnifiedInstance: multiple mode type variables found"
      where
        applyWithFunc :: Name -> Q Type -> Q Type -> Q Exp -> Q Exp
        applyWithFunc withFunc mode var exp =
          [|$(varE withFunc) @($mode) @($var) $exp|]
        body ::
          Q Type -> Name -> Name -> [Q Type] -> Maybe Name -> [Q Type] -> Q Dec
        body mode clsWithFunc withFunc starVars maybeWithFunc1 starToStarVars =
          do
            var <- newName "r"
            let arg = varP var
            let branch = foldr (applyWithFunc withFunc mode) (varE var) starVars
            let withModeFunc = 'withMode
            case (maybeWithFunc1, starToStarVars) of
              (_, []) -> do
                let exp =
                      lam1E
                        arg
                        [|$(varE withModeFunc) @($mode) $branch $branch|]
                valD (varP clsWithFunc) (normalB exp) []
              (Just withFunc1, _) -> do
                let branchWithFunc1 =
                      foldr (applyWithFunc withFunc1 mode) branch starToStarVars
                let exp =
                      lam1E
                        arg
                        [|
                          $(varE withModeFunc)
                            @($mode)
                            $branchWithFunc1
                            $branchWithFunc1
                          |]
                valD (varP clsWithFunc) (normalB exp) []
              (Nothing, _) ->
                fail $
                  "UnifiedInstance: withFunc1 is not provided, type have "
                    <> "functor type parameters"

deriveUnifiedInterfaceExtra ::
  [SomeDeriveTypeParamHandler] ->
  Name ->
  Name ->
  Name ->
  Q [Dec]
deriveUnifiedInterfaceExtra extraHandlers cls withFunc name =
  deriveWithHandlers
    ( extraHandlers
        <> [ SomeDeriveTypeParamHandler TypeableMode,
             SomeDeriveTypeParamHandler NatShouldBePositive,
             SomeDeriveTypeParamHandler $ PrimaryUnifiedConstraint cls False
           ]
    )
    (UnifiedInstance cls withFunc withFunc Nothing)
    True
    0
    [name]

deriveUnifiedInterface :: Name -> Name -> Name -> Q [Dec]
deriveUnifiedInterface = deriveUnifiedInterfaceExtra []

deriveUnifiedInterfaces :: Name -> Name -> [Name] -> Q [Dec]
deriveUnifiedInterfaces cls withFunc =
  fmap concat . mapM (deriveUnifiedInterface cls withFunc)

deriveUnifiedInterface1Extra ::
  [SomeDeriveTypeParamHandler] ->
  Name ->
  Name ->
  Name ->
  Name ->
  Name ->
  Q [Dec]
deriveUnifiedInterface1Extra extraHandlers cls withFunc cls1 withFunc1 name =
  deriveWithHandlers
    ( extraHandlers
        <> [ SomeDeriveTypeParamHandler TypeableMode,
             SomeDeriveTypeParamHandler NatShouldBePositive,
             SomeDeriveTypeParamHandler $ PrimaryUnifiedConstraint cls False,
             SomeDeriveTypeParamHandler $ PrimaryUnifiedConstraint cls1 False
           ]
    )
    (UnifiedInstance cls1 withFunc1 withFunc (Just withFunc1))
    True
    1
    [name]

deriveUnifiedInterface1 ::
  Name -> Name -> Name -> Name -> Name -> Q [Dec]
deriveUnifiedInterface1 = deriveUnifiedInterface1Extra []

deriveUnifiedInterface1s ::
  Name -> Name -> Name -> Name -> [Name] -> Q [Dec]
deriveUnifiedInterface1s cls withFunc cls1 withFunc1 =
  fmap concat . mapM (deriveUnifiedInterface1 cls withFunc cls1 withFunc1)

deriveFunctorArgUnifiedInterfaceExtra ::
  [SomeDeriveTypeParamHandler] -> Name -> Name -> Name -> Name -> Name -> Q [Dec]
deriveFunctorArgUnifiedInterfaceExtra
  extraHandlers
  cls
  withFunc
  cls1
  withFunc1
  name =
    deriveWithHandlers
      ( extraHandlers
          <> [ SomeDeriveTypeParamHandler TypeableMode,
               SomeDeriveTypeParamHandler NatShouldBePositive,
               SomeDeriveTypeParamHandler $ PrimaryUnifiedConstraint cls False,
               SomeDeriveTypeParamHandler $ PrimaryUnifiedConstraint cls1 False
             ]
      )
      (UnifiedInstance cls withFunc withFunc (Just withFunc1))
      True
      0
      [name]

deriveFunctorArgUnifiedInterface ::
  Name -> Name -> Name -> Name -> Name -> Q [Dec]
deriveFunctorArgUnifiedInterface = deriveFunctorArgUnifiedInterfaceExtra []

deriveFunctorArgUnifiedInterfaces ::
  Name -> Name -> Name -> Name -> [Name] -> Q [Dec]
deriveFunctorArgUnifiedInterfaces cls withFunc cls1 withFunc1 =
  fmap concat
    . mapM (deriveFunctorArgUnifiedInterface cls withFunc cls1 withFunc1)
