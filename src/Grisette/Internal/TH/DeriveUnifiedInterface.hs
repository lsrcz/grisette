{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Internal.TH.DeriveUnifiedInterface
  ( TypeableMode (..),
    UnifiedInstance (..),
    deriveUnifiedInterface,
    deriveUnifiedInterfaces,
    deriveUnifiedInterface1,
    deriveUnifiedInterface1s,
    deriveFunctorArgUnifiedInterface,
    deriveFunctorArgUnifiedInterfaces,
  )
where

import Control.Monad (when)
import Data.Typeable (Typeable)
import Grisette.Internal.Core.TH.Util (tvIsMode, tvIsStar, tvIsStarToStar)
import Grisette.Internal.TH.Derivation
  ( DeriveStrategyHandler (instanceDeclaration),
    DeriveTypeParamHandler (handleBody, handleTypeParam),
    NatShouldBePositive (NatShouldBePositive),
    SomeDeriveTypeParamHandler (SomeDeriveTypeParamHandler),
    deriveWithHandlers,
    getTypeWithMaybeSubst,
  )
import Grisette.Unified.Internal.EvaluationMode (EvaluationMode)
import Language.Haskell.TH
  ( Dec (ClassD),
    Exp,
    Info (ClassI),
    Inline (Inline),
    Pred,
    Q,
    Type (ConT),
    conT,
    instanceD,
    lam1E,
    newName,
    normalB,
    pragInlD,
    reify,
    valD,
    varE,
    varP,
    varT,
  )
import Language.Haskell.TH.Datatype.TyVarBndr
  ( TyVarBndrUnit,
    kindedTV,
    tvKind,
    tvName,
  )
import Language.Haskell.TH.Syntax
  ( Kind,
    Name,
    Phases (AllPhases),
    RuleMatch (FunLike),
  )

data TypeableMode = TypeableMode

instance DeriveTypeParamHandler TypeableMode where
  handleTypeParam _ tys = do
    let fst3 (a, _, _) = a
    let modes = filter tvIsMode $ fst3 <$> tys
    newTys <- case modes of
      [] -> do
        nm <- newName "mode"
        return $ (kindedTV nm (ConT ''EvaluationMode), Nothing, Nothing) : tys
      [_] -> return tys
      _ ->
        fail "TypeableMode: multiple mode type variables found"
    mapM
      ( \(tv, preds, substTy) -> do
          newPreds <- handleMode tv preds substTy
          return (tv, newPreds, substTy)
      )
      newTys
    where
      handleMode ::
        TyVarBndrUnit ->
        Maybe [Pred] ->
        Maybe Type ->
        Q (Maybe [Pred])
      handleMode tv preds substTy | tvIsMode tv = do
        typeable <- [t|Typeable $(getTypeWithMaybeSubst tv substTy)|]
        case preds of
          Nothing -> return (Just [typeable])
          Just ps -> return (Just (typeable : ps))
      handleMode _ preds _ = return preds
  handleBody _ _ = return []

data UnifiedInstance = UnifiedInstance
  { _cls :: Name,
    _clsWithFunc :: Name,
    _withFunc :: Name,
    _withFunc1 :: Maybe Name
  }

instance DeriveStrategyHandler UnifiedInstance where
  instanceDeclaration
    (UnifiedInstance cls clsWithFunc withFunc maybeWithFunc1)
    tys
    ctx
    ty = do
      let modes = (varT . tvName) <$> filter tvIsMode tys
      let stars = (varT . tvName) <$> filter tvIsStar tys
      let starToStars = (varT . tvName) <$> filter tvIsStarToStar tys
      case modes of
        [] -> fail "UnifiedInstance: no mode type variables found"
        [md] -> do
          sequence
            [ instanceD
                (return ctx)
                [t|$(conT cls) $md $(return ty)|]
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
            case (maybeWithFunc1, starToStarVars) of
              (_, []) -> do
                let exp = lam1E arg [|withMode @($mode) $branch $branch|]
                valD (varP clsWithFunc) (normalB exp) []
              (Just withFunc1, _) -> do
                let branchWithFunc1 =
                      foldr (applyWithFunc withFunc1 mode) branch starToStarVars
                let exp =
                      lam1E
                        arg
                        [|withMode @($mode) $branchWithFunc1 $branchWithFunc1|]
                valD (varP clsWithFunc) (normalB exp) []
              (Nothing, _) ->
                fail $
                  "UnifiedInstance: withFunc1 is not provided, type have "
                    <> "functor type parameters"

data PrimaryUnifiedConstraint = PrimaryUnifiedConstraint Name Bool

unifiedClassParamKind :: Name -> Q Kind
unifiedClassParamKind className = do
  cls <- reify className
  case cls of
    ClassI (ClassD _ _ bndrs _ _) _ ->
      case bndrs of
        [mode, x] -> do
          when (tvKind mode /= ConT ''EvaluationMode) $
            fail $
              "unifiedClassParamKind: first type parameter must be "
                <> "EvaluationMode"
          return $ tvKind x
        _ ->
          fail $
            "unifiedClassParamKind: only support classes with two type "
              <> "parameters, but "
              <> show className
              <> " has "
              <> show (length bndrs)
    _ ->
      fail $
        "unifiedClassParamKind:" <> show className <> " is not a class"

instance DeriveTypeParamHandler PrimaryUnifiedConstraint where
  handleTypeParam
    (PrimaryUnifiedConstraint className ignoreIfAlreadyHandled)
    tys = do
      kind <- unifiedClassParamKind className
      let fst3 (a, _, _) = a
      let modes = filter (tvIsMode . fst3) tys
      case modes of
        [] -> fail "PrimaryUnifiedConstraint: no mode type variables found"
        [(md, _, mdSubstTy)] -> do
          mdTy <- getTypeWithMaybeSubst md mdSubstTy
          mapM
            ( \(tv, preds, substTy) -> do
                (newPreds, newSubstTy) <- handle kind mdTy tv preds substTy
                return (tv, newPreds, newSubstTy)
            )
            tys
        _ ->
          fail "PrimaryUnifiedConstraint: multiple mode type variables found"
      where
        handle ::
          Kind ->
          Type ->
          TyVarBndrUnit ->
          Maybe [Pred] ->
          Maybe Type ->
          Q (Maybe [Pred], Maybe Type)
        handle _ _ _ (Just preds) substTy
          | ignoreIfAlreadyHandled =
              return (Just preds, substTy)
        handle kind mdty tv preds substTy
          | tvKind tv == kind = do
              let t = getTypeWithMaybeSubst tv substTy
              cls <- [t|$(conT className) $(return mdty) $t|]
              case preds of
                Nothing -> return (Just [cls], substTy)
                Just ps -> return (Just $ cls : ps, substTy)
        handle _ _ _ preds substTy = return (preds, substTy)
  handleBody (PrimaryUnifiedConstraint _ _) _ = return []

deriveUnifiedInterface :: Name -> Name -> Name -> Q [Dec]
deriveUnifiedInterface cls withFunc =
  deriveWithHandlers
    [ SomeDeriveTypeParamHandler TypeableMode,
      SomeDeriveTypeParamHandler NatShouldBePositive,
      SomeDeriveTypeParamHandler $ PrimaryUnifiedConstraint cls True
    ]
    (UnifiedInstance cls withFunc withFunc Nothing)
    True
    0

deriveUnifiedInterfaces :: Name -> Name -> [Name] -> Q [Dec]
deriveUnifiedInterfaces cls withFunc =
  fmap concat . mapM (deriveUnifiedInterface cls withFunc)

deriveUnifiedInterface1 ::
  Name -> Name -> Name -> Name -> Name -> Q [Dec]
deriveUnifiedInterface1 cls withFunc cls1 withFunc1 =
  deriveWithHandlers
    [ SomeDeriveTypeParamHandler TypeableMode,
      SomeDeriveTypeParamHandler NatShouldBePositive,
      SomeDeriveTypeParamHandler $ PrimaryUnifiedConstraint cls True,
      SomeDeriveTypeParamHandler $ PrimaryUnifiedConstraint cls1 True
    ]
    (UnifiedInstance cls1 withFunc1 withFunc (Just withFunc1))
    True
    1

deriveUnifiedInterface1s ::
  Name -> Name -> Name -> Name -> [Name] -> Q [Dec]
deriveUnifiedInterface1s cls withFunc cls1 withFunc1 =
  fmap concat . mapM (deriveUnifiedInterface1 cls withFunc cls1 withFunc1)

deriveFunctorArgUnifiedInterface ::
  Name -> Name -> Name -> Name -> Name -> Q [Dec]
deriveFunctorArgUnifiedInterface cls withFunc cls1 withFunc1 =
  deriveWithHandlers
    [ SomeDeriveTypeParamHandler TypeableMode,
      SomeDeriveTypeParamHandler NatShouldBePositive,
      SomeDeriveTypeParamHandler $ PrimaryUnifiedConstraint cls True,
      SomeDeriveTypeParamHandler $ PrimaryUnifiedConstraint cls1 True
    ]
    (UnifiedInstance cls withFunc withFunc (Just withFunc1))
    True
    0

deriveFunctorArgUnifiedInterfaces ::
  Name -> Name -> Name -> Name -> [Name] -> Q [Dec]
deriveFunctorArgUnifiedInterfaces cls withFunc cls1 withFunc1 =
  fmap concat
    . mapM (deriveFunctorArgUnifiedInterface cls withFunc cls1 withFunc1)
