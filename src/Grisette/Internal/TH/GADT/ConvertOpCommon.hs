{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Internal.TH.GADT.ConvertOpCommon
  ( genConvertOpClass,
    ConvertOpClassConfig (..),
    defaultFieldFunExp,
  )
where

import Control.Monad (foldM, replicateM, zipWithM)
import qualified Data.Map as M
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as S
import Grisette.Internal.Core.Control.Monad.Union (Union)
import Grisette.Internal.Core.Data.Class.PlainUnion (unionToCon)
import Grisette.Internal.Core.Data.Class.TryMerge (toUnionSym)
import Grisette.Internal.TH.GADT.Common
  ( CheckArgsResult (argVars, constructors, keptVars),
    DeriveConfig
      ( DeriveConfig,
        bitSizePositions,
        evalModeConfig,
        fpBitSizePositions,
        needExtraMergeableUnderEvalMode,
        needExtraMergeableWithConcretizedEvalMode
      ),
    EvalModeConfig (EvalModeConstraints, EvalModeSpecified),
    checkArgs,
    extraBitSizeConstraint,
    extraEvalModeConstraint,
    extraExtraMergeableConstraint,
    extraFpBitSizeConstraint,
    freshenCheckArgsResult,
    isVarUsedInFields,
  )
import Grisette.Internal.TH.Util (allUsedNames)
import Grisette.Internal.Unified.EvalModeTag (EvalModeTag (C, S))
import Grisette.Internal.Unified.Util
  ( EvalModeConvertible (withModeConvertible'),
  )
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

tagPair ::
  DeriveConfig ->
  EvalModeTag ->
  [(Type, Kind)] ->
  [(Type, Kind)] ->
  [(Type, Type)]
tagPair deriveConfig convertOpTarget lhsKeptVars rhsKeptVars =
  let conKeptVars =
        if convertOpTarget == S then lhsKeptVars else rhsKeptVars
      symKeptVars =
        if convertOpTarget == S then rhsKeptVars else lhsKeptVars
   in mapMaybe
        ( \case
            (n, EvalModeConstraints _)
              | n < length conKeptVars && n >= 0 ->
                  Just (fst $ conKeptVars !! n, fst $ symKeptVars !! n)
            _ -> Nothing
        )
        (evalModeConfig deriveConfig)

caseSplitTagPairs ::
  DeriveConfig ->
  EvalModeTag ->
  [(Type, Kind)] ->
  [(Type, Kind)] ->
  Exp ->
  Q Exp
caseSplitTagPairs deriveConfig convertOpTarget lhsKeptVars rhsKeptVars exp = do
  let tags = tagPair deriveConfig convertOpTarget lhsKeptVars rhsKeptVars
  foldM
    ( \exp (lty, rty) ->
        [|
          withModeConvertible'
            @($(return lty))
            @($(return rty))
            $(return exp)
            $(return exp)
            $(return exp)
          |]
    )
    exp
    tags

genConvertOpFieldClause ::
  DeriveConfig ->
  ConvertOpClassConfig ->
  [(Type, Kind)] ->
  [(Type, Kind)] ->
  [(Type, Kind)] ->
  [(Type, Kind)] ->
  ConstructorInfo ->
  Q Clause
genConvertOpFieldClause
  deriveConfig@DeriveConfig {..}
  ConvertOpClassConfig {..}
  lhsKeptTypes
  rhsKeptTypes
  lhsArgTypes
  _rhsArgTypes
  lhsConInfo = do
    fields <- mapM resolveTypeSynonyms $ constructorFields lhsConInfo
    (funPats, defaultFieldFunExps) <- funPatAndExps convertFieldFunExp lhsArgTypes fields
    fieldsPatNames <- replicateM (length fields) $ newName "field"
    fieldPats <- conP (constructorName lhsConInfo) (fmap varP fieldsPatNames)
    let fieldPatExps = fmap VarE fieldsPatNames
    fieldResExps <- zipWithM convertFieldResFun fieldPatExps defaultFieldFunExps
    resExp <- convertFieldCombineFun (constructorName lhsConInfo) fieldResExps
    let resUsedNames = allUsedNames resExp
    let transformPat (VarP nm) =
          if S.member nm resUsedNames then VarP nm else WildP
        transformPat p = p
    resExpWithTags <-
      caseSplitTagPairs
        deriveConfig
        convertOpTarget
        lhsKeptTypes
        rhsKeptTypes
        resExp
    -- let conKeptVars =
    --       if convertOpTarget == S then lhsKeptTypes else rhsKeptTypes
    -- let symKeptVars =
    --       if convertOpTarget == S then rhsKeptTypes else lhsKeptTypes
    -- let tags =
    --       mapMaybe
    --         ( \case
    --             (n, EvalModeConstraints _)
    --               | n < length conKeptVars && n >= 0 ->
    --                   Just (fst $ conKeptVars !! n, fst $ symKeptVars !! n)
    --             _ -> Nothing
    --         )
    --         evalModeConfig
    -- resExpWithTags <-
    --   foldM
    --     ( \exp (lty, rty) ->
    --         [|
    --           withModeConvertible'
    --             @($(return lty))
    --             @($(return rty))
    --             $(return exp)
    --             $(return exp)
    --             $(return exp)
    --           |]
    --     )
    --     resExp
    --     tags
    return $
      Clause
        (fmap transformPat $ funPats ++ [fieldPats])
        (NormalB resExpWithTags)
        []

genConvertOpFun ::
  DeriveConfig ->
  ConvertOpClassConfig ->
  Int ->
  [(Type, Kind)] ->
  [(Type, Kind)] ->
  [(Type, Kind)] ->
  [(Type, Kind)] ->
  [ConstructorInfo] ->
  Q Dec
genConvertOpFun
  deriveConfig
  convertOpClassConfig
  n
  lhsKeptTypes
  rhsKeptTypes
  lhsArgTypes
  rhsArgTypes
  lhsConstructors = do
    clauses <-
      traverse
        ( genConvertOpFieldClause
            deriveConfig
            convertOpClassConfig
            lhsKeptTypes
            rhsKeptTypes
            lhsArgTypes
            rhsArgTypes
        )
        lhsConstructors
    let instanceFunName = (convertOpFunNames convertOpClassConfig) !! n
    return $ FunD instanceFunName clauses

data ConvertOpClassConfig = ConvertOpClassConfig
  { convertOpTarget :: EvalModeTag,
    convertOpInstanceNames :: [Name],
    convertOpFunNames :: [Name],
    convertFieldResFun :: Exp -> Exp -> Q Exp,
    convertFieldCombineFun :: Name -> [Exp] -> Q Exp,
    convertFieldFunExp :: FieldFunExp
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

-- | Generate extra constraints for a GADT.
extraConstraintConvert ::
  DeriveConfig ->
  EvalModeTag ->
  Name ->
  Name ->
  [(Type, Kind)] ->
  [(Type, Kind)] ->
  [ConstructorInfo] ->
  Q [Pred]
extraConstraintConvert
  DeriveConfig {..}
  convertOpTarget
  tyName
  instanceName
  lhsKeptArgs
  rhsKeptArgs
  rhsConstructors = do
    let conKeptVars = if convertOpTarget == S then lhsKeptArgs else rhsKeptArgs
    let symKeptVars = if convertOpTarget == S then rhsKeptArgs else lhsKeptArgs

    rhsEvalModePreds <-
      if convertOpTarget == S && needExtraMergeableWithConcretizedEvalMode
        then
          traverse
            (extraEvalModeConstraint tyName instanceName rhsKeptArgs)
            evalModeConfig
        else return []
    extraArgEvalModePreds <-
      traverse
        ( \case
            (n, EvalModeConstraints _)
              | n < length lhsKeptArgs && n >= 0 ->
                  (: [])
                    <$> [t|
                      EvalModeConvertible
                        $(return $ fst $ conKeptVars !! n)
                        $(return $ fst $ symKeptVars !! n)
                      |]
            _ -> return []
        )
        evalModeConfig
    bitSizePreds <-
      traverse
        (extraBitSizeConstraint tyName instanceName lhsKeptArgs)
        bitSizePositions
    fpBitSizePreds <-
      traverse
        (extraFpBitSizeConstraint tyName instanceName lhsKeptArgs)
        fpBitSizePositions
    extraMergeablePreds <-
      if convertOpTarget == S
        && ( any
               ( \case
                   (_, EvalModeConstraints _) -> True
                   (_, EvalModeSpecified _) -> False
               )
               evalModeConfig
               || needExtraMergeableWithConcretizedEvalMode
           )
        then
          extraExtraMergeableConstraint rhsConstructors rhsKeptArgs
        else return []
    return $
      concat
        ( rhsEvalModePreds
            ++ extraArgEvalModePreds
            ++ bitSizePreds
            ++ fpBitSizePreds
            ++ [extraMergeablePreds]
        )

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
  -- let convertOpSource = case convertOpTarget of
  --       S -> C
  --       C -> S
  let lResult = oldLhsResult
  let rResult = oldRhsResult
  -- let lConfig = deriveConfig {evalModeConfig = l}
  -- let rConfig = deriveConfig {evalModeConfig = r}
  -- lResult <- specializeResult (evalModeSpecializeList lConfig) oldLhsResult
  -- rResult <- specializeResult (evalModeSpecializeList rConfig) oldRhsResult

  let instanceName = convertOpInstanceNames !! n
  let lKeptVars = keptVars lResult
  let rKeptVars = keptVars rResult
  let lConstructors = constructors lResult
  let rConstructors = constructors rResult
  let lKeptType = foldl AppT (ConT typName) $ fmap fst lKeptVars
  let rKeptType = foldl AppT (ConT typName) $ fmap fst rKeptVars
  extraPreds <-
    extraConstraintConvert
      deriveConfig
      convertOpTarget
      typName
      instanceName
      lKeptVars
      rKeptVars
      rConstructors
  unionExtraPreds <-
    extraConstraintConvert
      deriveConfig {needExtraMergeableWithConcretizedEvalMode = True}
      convertOpTarget
      typName
      instanceName
      lKeptVars
      rKeptVars
      rConstructors

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
      deriveConfig
      (ConvertOpClassConfig {..})
      n
      (keptVars lResult)
      (keptVars rResult)
      (argVars lResult)
      (argVars rResult)
      lConstructors

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
  instanceUnionFun <- do
    resExp <-
      if convertOpTarget == S
        then varE 'toUnionSym
        else varE 'unionToCon
    funD (head convertOpFunNames) [clause [] (normalB $ return resExp) []]

  return $
    InstanceD
      (Just Incoherent)
      (extraPreds ++ catMaybes ctxs)
      instanceType
      [instanceFun]
      : ( [ InstanceD
              (Just Incoherent)
              (unionExtraPreds ++ catMaybes ctxs)
              instanceUnionType
              [instanceUnionFun]
            | n == 0
          ]
        )
