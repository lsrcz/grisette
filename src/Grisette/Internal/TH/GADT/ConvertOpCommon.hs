{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Grisette.Internal.TH.GADT.ConvertOpCommon
  ( genConvertOpClass,
    ConvertOpClassConfig (..),
    ConvertOpFieldConfig (..),
    defaultFieldFunExp,
  )
where

import Control.Monad (replicateM, zipWithM)
import qualified Data.Map as M
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as S
import Data.Traversable (for)
import Grisette.Internal.Core.Control.Monad.Union
  ( Union,
    toUnionSym,
    unionToCon,
  )
import Grisette.Internal.TH.GADT.Common
  ( CheckArgsResult (argVars, constructors, keptVars),
    DeriveConfig
      ( evalModeConfig,
        needExtraMergeableUnderEvalMode,
        needExtraMergeableWithConcretizedEvalMode
      ),
    EvalModeConfig (EvalModeConstraints, EvalModeSpecified),
    checkArgs,
    evalModeSpecializeList,
    extraConstraint,
    freshenCheckArgsResult,
    isVarUsedInFields,
    specializeResult,
  )
import Grisette.Internal.TH.Util (allUsedNames)
import Grisette.Unified.Internal.EvalModeTag (EvalModeTag (C, S))
import Language.Haskell.TH
  ( Body (NormalB),
    Clause (Clause),
    Dec (FunD, InstanceD),
    Exp (VarE),
    Kind,
    Name,
    Overlap (Incoherent),
    Pat (VarP, WildP),
    Pred,
    Q,
    Quote (newName),
    Type (AppT, ArrowT, ConT, StarT, VarT),
    clause,
    conP,
    funD,
    nameBase,
    normalB,
    varE,
    varP,
  )
import Language.Haskell.TH.Datatype
  ( ConstructorInfo (constructorFields, constructorName),
    TypeSubstitution (freeVariables),
    resolveTypeSynonyms,
  )

type FieldFunExp = M.Map Name Name -> Type -> Q Exp

defaultFieldFunExp :: [Name] -> FieldFunExp
defaultFieldFunExp binaryOpFunNames argToFunPat = go
  where
    go ty = do
      let allArgNames = M.keysSet argToFunPat
      let typeHasNoArg ty =
            S.fromList (freeVariables [ty])
              `S.intersection` allArgNames
              == S.empty
      let fun0 = varE $ head binaryOpFunNames
          fun1 b = [|$(varE $ binaryOpFunNames !! 1) $(go b)|]
          fun2 b c = [|$(varE $ binaryOpFunNames !! 2) $(go b) $(go c)|]
          fun3 b c d =
            [|$(varE $ binaryOpFunNames !! 3) $(go b) $(go c) $(go d)|]
      case ty of
        AppT (AppT (AppT (VarT _) b) c) d -> fun3 b c d
        AppT (AppT (VarT _) b) c -> fun2 b c
        AppT (VarT _) b -> fun1 b
        _ | typeHasNoArg ty -> fun0
        AppT a b | typeHasNoArg a -> fun1 b
        AppT (AppT a b) c | typeHasNoArg a -> fun2 b c
        AppT (AppT (AppT a b) c) d | typeHasNoArg a -> fun3 b c d
        VarT nm -> case M.lookup nm argToFunPat of
          Just pname -> varE pname
          _ -> fail $ "defaultFieldFunExp: unsupported type: " <> show ty
        _ -> fail $ "defaultFieldFunExp: unsupported type: " <> show ty

funPatAndExps ::
  FieldFunExp ->
  [(Type, Kind)] ->
  [Type] ->
  Q ([Pat], [Exp])
funPatAndExps fieldFunExpGen argTypes fields = do
  let usedArgs = S.fromList $ freeVariables fields
  args <-
    traverse
      ( \(ty, _) ->
          case ty of
            VarT nm ->
              if S.member nm usedArgs
                then do
                  pname <- newName "p"
                  return (nm, Just pname)
                else return ('undefined, Nothing)
            _ -> return ('undefined, Nothing)
      )
      argTypes
  let argToFunPat =
        M.fromList $ mapMaybe (\(ty, mpat) -> fmap (ty,) mpat) args
  let funPats = fmap (maybe WildP VarP . snd) args
  defaultFieldFunExps <- traverse (fieldFunExpGen argToFunPat) fields
  return (funPats, defaultFieldFunExps)

genConvertOpFieldClause ::
  ConvertOpFieldConfig ->
  [(Type, Kind)] ->
  ConstructorInfo ->
  Q Clause
genConvertOpFieldClause ConvertOpFieldConfig {..} argTypes conInfo = do
  fields <- mapM resolveTypeSynonyms $ constructorFields conInfo
  (funPats, defaultFieldFunExps) <- funPatAndExps fieldFunExp argTypes fields
  fieldsPatNames <- replicateM (length fields) $ newName "field"
  fieldPats <- conP (constructorName conInfo) (fmap varP fieldsPatNames)
  let fieldPatExps = fmap VarE fieldsPatNames
  fieldResExps <- zipWithM fieldResFun fieldPatExps defaultFieldFunExps
  resExp <- fieldCombineFun (constructorName conInfo) fieldResExps
  let resUsedNames = allUsedNames resExp
  let transformPat (VarP nm) =
        if S.member nm resUsedNames then VarP nm else WildP
      transformPat p = p
  return $
    Clause
      (fmap transformPat $ funPats ++ [fieldPats])
      (NormalB resExp)
      []

genConvertOpFun ::
  ConvertOpClassConfig ->
  Int ->
  [(Type, Kind)] ->
  [ConstructorInfo] ->
  Q Dec
genConvertOpFun (ConvertOpClassConfig {..}) n argTypes constructors = do
  clauses <-
    traverse
      ( genConvertOpFieldClause
          convertOpFieldConfigs
          argTypes
      )
      constructors
  let instanceFunName = convertOpFunNames !! n
  return $ FunD instanceFunName clauses

data ConvertOpFieldConfig = ConvertOpFieldConfig
  { fieldResFun :: Exp -> Exp -> Q Exp,
    fieldCombineFun :: Name -> [Exp] -> Q Exp,
    fieldFunExp :: FieldFunExp
  }

data ConvertOpClassConfig = ConvertOpClassConfig
  { convertOpFieldConfigs :: ConvertOpFieldConfig,
    convertOpTarget :: EvalModeTag,
    convertOpInstanceNames :: [Name],
    convertOpFunNames :: [Name]
  }

convertCtxForVar :: [Type] -> Type -> Type -> Kind -> Q (Maybe Pred)
convertCtxForVar instanceExps lty rty knd = case knd of
  StarT ->
    Just
      <$> [t|$(return $ head instanceExps) $(return lty) $(return rty)|]
  AppT (AppT ArrowT StarT) StarT ->
    Just
      <$> [t|$(return $ instanceExps !! 1) $(return lty) $(return rty)|]
  AppT (AppT (AppT ArrowT StarT) StarT) StarT ->
    Just
      <$> [t|$(return $ instanceExps !! 2) $(return lty) $(return rty)|]
  AppT (AppT (AppT StarT StarT) StarT) _ ->
    fail $ "Unsupported kind: " <> show knd
  _ -> return Nothing

genConvertOpClass ::
  DeriveConfig -> ConvertOpClassConfig -> Int -> Name -> Q [Dec]
genConvertOpClass deriveConfig (ConvertOpClassConfig {..}) n typName = do
  oldLhsResult <-
    freshenCheckArgsResult True
      =<< checkArgs
        (nameBase $ head convertOpInstanceNames)
        (length convertOpInstanceNames - 1)
        typName
        False
        n
  oldRhsResult <- freshenCheckArgsResult False oldLhsResult
  -- let baseSpecializeList = evalModeSpecializeList deriveConfig
  let convertOpSource = case convertOpTarget of
        S -> C
        C -> S
  let constructSpecializeList ::
        [(Int, EvalModeConfig)] ->
        [([(Int, EvalModeConfig)], [(Int, EvalModeConfig)])]
      constructSpecializeList [] = [([], [])]
      constructSpecializeList ((n, cfg) : xs) =
        case cfg of
          EvalModeConstraints _ -> do
            (l, r) <-
              [ ([(n, cfg)], [(n, EvalModeSpecified convertOpTarget)]),
                ([(n, EvalModeSpecified convertOpSource)], [(n, cfg)]),
                ( [(n, EvalModeSpecified convertOpSource)],
                  [(n, EvalModeSpecified convertOpTarget)]
                )
                ]
            (ol, or) <- constructSpecializeList xs
            return (l ++ ol, r ++ or)
          EvalModeSpecified _ -> do
            (ol, or) <- constructSpecializeList xs
            return ((n, cfg) : ol, (n, cfg) : or)
  fmap concat
    . for
      ( constructSpecializeList
          (evalModeConfig deriveConfig)
      )
    $ \(l, r) -> do
      let lConfig = deriveConfig {evalModeConfig = l}
      let rConfig =
            mempty
              { evalModeConfig = r,
                needExtraMergeableUnderEvalMode =
                  needExtraMergeableUnderEvalMode deriveConfig
              }
      lResult <- specializeResult (evalModeSpecializeList lConfig) oldLhsResult
      rResult <- specializeResult (evalModeSpecializeList rConfig) oldRhsResult

      let instanceName = convertOpInstanceNames !! n
      let lKeptVars = keptVars lResult
      let rKeptVars = keptVars rResult
      let lConstructors = constructors lResult
      let rConstructors = constructors rResult
      let lKeptType = foldl AppT (ConT typName) $ fmap fst lKeptVars
      let rKeptType = foldl AppT (ConT typName) $ fmap fst rKeptVars
      lExtraPreds <-
        extraConstraint lConfig typName instanceName [] lKeptVars lConstructors
      rExtraPreds <-
        extraConstraint rConfig typName instanceName [] rKeptVars rConstructors
      let instanceType = AppT (AppT (ConT instanceName) lKeptType) rKeptType
      let isTypeUsedInFields (VarT nm) = isVarUsedInFields lResult nm
          isTypeUsedInFields _ = False
      ctxs <-
        traverse
          ( \((lty, knd), (rty, _)) ->
              convertCtxForVar (ConT <$> convertOpInstanceNames) lty rty knd
          )
          $ filter (isTypeUsedInFields . fst . fst)
          $ zip lKeptVars rKeptVars
      instanceFun <-
        genConvertOpFun
          (ConvertOpClassConfig {..})
          n
          (argVars lResult)
          (constructors lResult)

      let instanceUnionType =
            case convertOpTarget of
              S ->
                AppT
                  (AppT (ConT instanceName) lKeptType)
                  (AppT (ConT ''Union) rKeptType)
              C ->
                AppT
                  (AppT (ConT instanceName) (AppT (ConT ''Union) lKeptType))
                  rKeptType
      instanceUnionFun <-
        case convertOpTarget of
          S ->
            funD
              (head convertOpFunNames)
              [clause [] (normalB [|toUnionSym|]) []]
          C ->
            funD
              (head convertOpFunNames)
              [clause [] (normalB [|unionToCon|]) []]
      rUnionExtraPreds <-
        extraConstraint
          rConfig
            { needExtraMergeableWithConcretizedEvalMode =
                convertOpTarget == S,
              needExtraMergeableUnderEvalMode = True
            }
          typName
          instanceName
          []
          rKeptVars
          rConstructors
      return $
        InstanceD
          (Just Incoherent)
          (lExtraPreds ++ rExtraPreds ++ catMaybes ctxs)
          instanceType
          [instanceFun]
          : ( [ InstanceD
                  (Just Incoherent)
                  (lExtraPreds ++ rUnionExtraPreds ++ catMaybes ctxs)
                  instanceUnionType
                  [instanceUnionFun]
                | n == 0
              ]
            )
