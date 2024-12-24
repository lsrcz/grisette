{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :   Grisette.Internal.TH.GADT.UnaryOpCommon
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.GADT.UnaryOpCommon
  ( UnaryOpClassConfig (..),
    UnaryOpFieldConfig (..),
    UnaryOpDeserializeConfig (..),
    UnaryOpUnifiedConfig (..),
    UnaryOpConfig (..),
    FieldFunExp,
    defaultFieldResFun,
    defaultFieldFunExp,
    genUnaryOpClass,
    defaultUnaryOpInstanceTypeFromConfig,
    defaultUnaryOpUnifiedFun,
  )
where

import Control.Monad (replicateM, zipWithM)
import Data.Bytes.Serial (Serial (deserialize))
import qualified Data.List as List
import qualified Data.Map as M
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as S
import Grisette.Internal.TH.GADT.Common
  ( CheckArgsResult
      ( CheckArgsResult,
        argVars,
        constructors,
        keptVars
      ),
    DeriveConfig (evalModeConfig),
    checkArgs,
    ctxForVar,
    evalModeSpecializeList,
    extraConstraint,
    freshenCheckArgsResult,
    isVarUsedInFields,
    specializeResult,
  )
import Grisette.Internal.TH.Util (allUsedNames)
import Grisette.Unified.Internal.Util (withMode)
import Language.Haskell.TH
  ( Body (NormalB),
    Clause (Clause),
    Dec (FunD, InstanceD),
    Exp (VarE),
    Kind,
    Lit (IntegerL),
    Match (Match),
    Name,
    Pat (LitP, VarP, WildP),
    Q,
    Type (AppT, ArrowT, ConT, StarT, VarT),
    appE,
    bindS,
    caseE,
    clause,
    conE,
    conP,
    conT,
    doE,
    funD,
    match,
    mkName,
    nameBase,
    newName,
    noBindS,
    normalB,
    sigP,
    varE,
    varP,
    wildP,
  )
import Language.Haskell.TH.Datatype
  ( ConstructorInfo (constructorFields, constructorName, constructorVariant),
    ConstructorVariant,
    TypeSubstitution (freeVariables),
    resolveTypeSynonyms,
  )

-- | Type of field function expression generator.
type FieldFunExp = M.Map Name Name -> M.Map Name [Name] -> Type -> Q Exp

-- | Default field function expression generator.
defaultFieldFunExp :: [Name] -> FieldFunExp
defaultFieldFunExp unaryOpFunNames argToFunPat _ = go
  where
    go ty = do
      let allArgNames = M.keysSet argToFunPat
      let typeHasNoArg ty =
            S.fromList (freeVariables [ty])
              `S.intersection` allArgNames
              == S.empty
      let fun0 = varE $ head unaryOpFunNames
          fun1 b = [|$(varE $ unaryOpFunNames !! 1) $(go b)|]
          fun2 b c = [|$(varE $ unaryOpFunNames !! 2) $(go b) $(go c)|]
          fun3 b c d =
            [|$(varE $ unaryOpFunNames !! 3) $(go b) $(go c) $(go d)|]
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

data UnaryOpFieldConfig = UnaryOpFieldConfig
  { extraPatNames :: [String],
    extraLiftedPatNames :: Int -> [String],
    fieldResFun ::
      ConstructorVariant ->
      Name ->
      [Exp] ->
      Int ->
      Exp ->
      Exp ->
      Q (Exp, [Bool]),
    fieldCombineFun ::
      Int ->
      ConstructorVariant ->
      Name ->
      [Exp] ->
      [Exp] ->
      Q (Exp, [Bool]),
    fieldFunExp :: FieldFunExp
  }

newtype UnaryOpUnifiedConfig = UnaryOpUnifiedConfig
  {unifiedFun :: Type -> (Type, Kind) -> Q (Maybe Exp)}

newtype UnaryOpDeserializeConfig = UnaryOpDeserializeConfig
  {fieldDeserializeFun :: FieldFunExp}

defaultUnaryOpUnifiedFun :: [Name] -> Type -> (Type, Kind) -> Q (Maybe Exp)
defaultUnaryOpUnifiedFun funNames modeTy (ty, kind) =
  case kind of
    StarT ->
      Just
        <$> [|
          $(varE $ head funNames) @($(return modeTy))
            @($(return ty))
          |]
    AppT (AppT ArrowT StarT) StarT ->
      Just
        <$> [|
          $(varE $ funNames !! 1) @($(return modeTy))
            @($(return ty))
          |]
    AppT (AppT (AppT ArrowT StarT) StarT) StarT ->
      Just
        <$> [|
          $(varE $ funNames !! 2) @($(return modeTy))
            @($(return ty))
          |]
    _ -> return Nothing

-- | Configuration for a unary function field expression generation on a GADT.
data UnaryOpConfig
  = UnaryOpField {_fieldConfig :: UnaryOpFieldConfig, funNames :: [Name]}
  | UnaryOpUnified {_unifiedConfig :: UnaryOpUnifiedConfig, funNames :: [Name]}
  | UnaryOpDeserialize
      { _deserializeConfig :: UnaryOpDeserializeConfig,
        funNames :: [Name]
      }

-- | Default field result function.
defaultFieldResFun ::
  ConstructorVariant -> Name -> [Exp] -> Int -> Exp -> Exp -> Q (Exp, [Bool])
defaultFieldResFun _ _ extraPatExps _ fieldPatExp defaultFieldFunExp = do
  res <-
    appE
      ( foldl
          (\exp name -> appE exp (return name))
          (return defaultFieldFunExp)
          extraPatExps
      )
      (return fieldPatExp)
  return (res, (True <$ extraPatExps))

funPatAndExps ::
  FieldFunExp ->
  (Int -> [String]) ->
  [(Type, Kind)] ->
  [Type] ->
  Q ([Pat], [[Pat]], [Exp])
funPatAndExps fieldFunExpGen extraLiftedPatNames argTypes fields = do
  let usedArgs = S.fromList $ freeVariables fields
  let liftedNames = extraLiftedPatNames (length argTypes)
  args <-
    traverse
      ( \(ty, _) -> do
          case ty of
            VarT nm ->
              if S.member nm usedArgs
                then do
                  pname <- newName "p"
                  epname <- traverse newName liftedNames
                  return (nm, Just (pname, epname))
                else return ('undefined, Nothing)
            _ -> return ('undefined, Nothing)
      )
      argTypes
  let argToFunPat =
        M.fromList $ mapMaybe (\(nm, mpat) -> fmap ((nm,) . fst) mpat) args
  let argToLiftedPat =
        M.fromList $ mapMaybe (\(nm, mpat) -> fmap ((nm,) . snd) mpat) args
  let funPats = fmap (maybe WildP (VarP . fst) . snd) args
  let extraLiftedPats =
        fmap
          ( maybe
              (replicate (length liftedNames) WildP)
              (fmap VarP . snd)
              . snd
          )
          args
  defaultFieldFunExps <-
    traverse
      (fieldFunExpGen argToFunPat argToLiftedPat)
      fields
  return (funPats, extraLiftedPats, defaultFieldFunExps)

-- | Generate a clause for a unary function on a GADT.
genUnaryOpFieldClause ::
  UnaryOpFieldConfig ->
  [(Type, Kind)] ->
  Int ->
  ConstructorInfo ->
  Q Clause
genUnaryOpFieldClause
  (UnaryOpFieldConfig {..})
  argTypes
  conIdx
  conInfo = do
    fields <- mapM resolveTypeSynonyms $ constructorFields conInfo
    (funPats, funLiftedPats, defaultFieldFunExps) <-
      funPatAndExps fieldFunExp extraLiftedPatNames argTypes fields
    extraPatNames <- traverse newName extraPatNames
    let extraPatExps = fmap VarE extraPatNames
    fieldsPatNames <- replicateM (length fields) $ newName "field"
    let extraPats = fmap VarP extraPatNames
    fieldPats <- conP (constructorName conInfo) (fmap varP fieldsPatNames)
    let fieldPatExps = fmap VarE fieldsPatNames

    fieldResExpsAndArgsUsed <-
      sequence $
        zipWith3
          ( fieldResFun
              (constructorVariant conInfo)
              (constructorName conInfo)
              extraPatExps
          )
          [0 ..]
          fieldPatExps
          defaultFieldFunExps
    let fieldResExps = fst <$> fieldResExpsAndArgsUsed
    let extraArgsUsedByFields = snd <$> fieldResExpsAndArgsUsed

    (resExp, extraArgsUsedByResult) <-
      fieldCombineFun
        conIdx
        (constructorVariant conInfo)
        (constructorName conInfo)
        (VarE <$> extraPatNames)
        fieldResExps
    let resUsedNames = allUsedNames resExp
    let extraArgsUsed =
          fmap or $
            List.transpose $
              extraArgsUsedByResult : extraArgsUsedByFields
    let extraArgsPats =
          zipWith
            (\pat used -> if used then pat else WildP)
            extraPats
            extraArgsUsed
    let transformPat (VarP nm) =
          if S.member nm resUsedNames then VarP nm else WildP
        transformPat p = p
    return $
      Clause
        ( fmap transformPat $
            concat (zipWith (:) funPats funLiftedPats)
              ++ extraArgsPats
              ++ [fieldPats]
        )
        (NormalB resExp)
        []

-- | Configuration for a unary operation type class generation on a GADT.
data UnaryOpClassConfig = UnaryOpClassConfig
  { unaryOpConfigs :: [UnaryOpConfig],
    unaryOpInstanceNames :: [Name],
    unaryOpExtraVars :: DeriveConfig -> Q [(Type, Kind)],
    unaryOpInstanceTypeFromConfig ::
      DeriveConfig ->
      [(Type, Kind)] ->
      [(Type, Kind)] ->
      Name ->
      Q Type,
    unaryOpAllowExistential :: Bool
  }

-- | Default unary operation instance type generator.
defaultUnaryOpInstanceTypeFromConfig ::
  DeriveConfig -> [(Type, Kind)] -> [(Type, Kind)] -> Name -> Q Type
defaultUnaryOpInstanceTypeFromConfig _ _ _ = conT

genUnaryOpFun ::
  DeriveConfig ->
  UnaryOpConfig ->
  Int ->
  [(Type, Kind)] ->
  [(Type, Kind)] ->
  [(Type, Kind)] ->
  (Name -> Bool) ->
  [ConstructorInfo] ->
  Q Dec
genUnaryOpFun _ (UnaryOpField config funNames) n _ _ argTypes _ constructors = do
  clauses <-
    zipWithM
      ( genUnaryOpFieldClause
          config
          argTypes
      )
      [0 ..]
      constructors
  let instanceFunName = funNames !! n
  return $ FunD instanceFunName clauses
genUnaryOpFun
  deriveConfig
  (UnaryOpUnified (UnaryOpUnifiedConfig {..}) funNames)
  n
  extraVars
  keptTypes
  _
  isVarUsedInFields
  _ = do
    modeTy <- case evalModeConfig deriveConfig of
      [] -> return $ fst $ head extraVars
      [(i, _)] -> return $ fst $ keptTypes !! i
      _ -> fail "Unified classes does not support multiple evaluation modes"
    let isTypeUsedInFields (VarT nm) = isVarUsedInFields nm
        isTypeUsedInFields _ = False
    exprs <-
      traverse (unifiedFun modeTy) $
        filter (isTypeUsedInFields . fst) keptTypes
    rVar <- newName "r"
    let rf =
          foldl
            ( \exp nextFun -> case nextFun of
                Nothing -> exp
                Just fun -> appE (return fun) exp
            )
            (return $ VarE rVar)
            exprs
    let instanceFunName = funNames !! n
    funD
      instanceFunName
      [ clause
          [varP rVar]
          ( normalB
              [|
                withMode @($(return modeTy)) $(rf) $(rf)
                |]
          )
          []
      ]
genUnaryOpFun
  _
  (UnaryOpDeserialize UnaryOpDeserializeConfig {..} funNames)
  n
  _
  _
  argTypes
  _
  constructors = do
    allFields <-
      mapM resolveTypeSynonyms $
        concatMap constructorFields constructors
    let usedArgs = S.fromList $ freeVariables allFields
    args <-
      traverse
        ( \(ty, _) -> do
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
          M.fromList $ mapMaybe (\(nm, mpat) -> fmap (nm,) mpat) args
    let funPats = fmap (maybe WildP VarP . snd) args
    let genAuxFunMatch conIdx conInfo = do
          fields <- mapM resolveTypeSynonyms $ constructorFields conInfo
          defaultFieldFunExps <-
            traverse
              (fieldDeserializeFun argToFunPat M.empty)
              fields
          let conName = constructorName conInfo
          exp <-
            foldl
              (\exp fieldFun -> [|$exp <*> $(return fieldFun)|])
              [|return $(conE conName)|]
              defaultFieldFunExps
          return $ Match (LitP (IntegerL conIdx)) (NormalB exp) []
    auxMatches <- zipWithM genAuxFunMatch [0 ..] constructors
    auxFallbackMatch <- match wildP (normalB [|undefined|]) []
    let instanceFunName = funNames !! n
    -- let auxFunName = mkName "go"
    let selName = mkName "sel"
    exp <-
      doE
        [ bindS
            (sigP (varP selName) (conT ''Int))
            [|deserialize|],
          noBindS $
            caseE (varE selName) $
              return <$> auxMatches ++ [auxFallbackMatch]
        ]
    return $
      FunD
        instanceFunName
        [ Clause
            funPats
            (NormalB exp)
            []
        ]

-- | Generate a unary operation type class instance for a GADT.
genUnaryOpClass ::
  DeriveConfig ->
  UnaryOpClassConfig ->
  Int ->
  Name ->
  Q [Dec]
genUnaryOpClass deriveConfig (UnaryOpClassConfig {..}) n typName = do
  result@CheckArgsResult {..} <-
    specializeResult (evalModeSpecializeList deriveConfig)
      =<< freshenCheckArgsResult True
      =<< checkArgs
        (nameBase $ head unaryOpInstanceNames)
        (length unaryOpInstanceNames - 1)
        typName
        unaryOpAllowExistential
        n
  extraVars <- unaryOpExtraVars deriveConfig
  instanceTypes <-
    traverse
      (unaryOpInstanceTypeFromConfig deriveConfig extraVars keptVars)
      unaryOpInstanceNames
  let isTypeUsedInFields (VarT nm) = isVarUsedInFields result nm
      isTypeUsedInFields _ = False
  ctxs <-
    traverse (uncurry $ ctxForVar instanceTypes) $
      filter (isTypeUsedInFields . fst) keptVars
  let keptType = foldl AppT (ConT typName) $ fmap fst keptVars
  instanceFuns <-
    traverse
      ( \config ->
          genUnaryOpFun
            deriveConfig
            config
            n
            extraVars
            keptVars
            argVars
            (isVarUsedInFields result)
            constructors
      )
      unaryOpConfigs
  let instanceName = unaryOpInstanceNames !! n
  let instanceType = AppT (instanceTypes !! n) keptType
  extraPreds <-
    extraConstraint
      deriveConfig
      typName
      instanceName
      extraVars
      keptVars
      constructors
  return
    [ InstanceD
        Nothing
        (extraPreds ++ catMaybes ctxs)
        instanceType
        instanceFuns
    ]
