{-# LANGUAGE GADTs #-}
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
    UnaryOpConfig (..),
    UnaryOpFunConfig (..),
    FieldFunExp,
    defaultFieldResFun,
    defaultFieldFunExp,
    genUnaryOpClass,
    defaultUnaryOpInstanceTypeFromConfig,
  )
where

import Control.Monad (replicateM, zipWithM)
import qualified Data.List as List
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import qualified Data.Set as S
import Grisette.Internal.TH.GADT.Common
  ( CheckArgsResult
      ( CheckArgsResult,
        argVars,
        constructors,
        keptVars
      ),
    DeriveConfig,
    checkArgs,
    ctxForVar,
    evalModeSpecializeList,
    extraConstraint,
    freshenCheckArgsResult,
    isVarUsedInFields,
    specializeResult,
  )
import Grisette.Internal.TH.Util (allUsedNames)
import Language.Haskell.TH
  ( Body (NormalB),
    Clause (Clause),
    Dec (FunD, InstanceD),
    Exp (VarE),
    Kind,
    Name,
    Pat (VarP, WildP),
    Q,
    Type (AppT, ConT, VarT),
    appE,
    clause,
    conP,
    conT,
    funD,
    nameBase,
    newName,
    normalB,
    varE,
    varP,
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

-- | Configuration for a unary function field expression generation on a GADT.
data UnaryOpConfig where
  UnaryOpConfig ::
    (UnaryOpFunConfig config) => config -> [Name] -> UnaryOpConfig

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
  Int ->
  ConstructorInfo ->
  Q Clause
genUnaryOpFieldClause
  (UnaryOpFieldConfig {..})
  argTypes
  totalConNumber
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
        totalConNumber
        conIdx
        (constructorVariant conInfo)
        (constructorName conInfo)
        extraPatExps
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
    unaryOpContextNames :: Maybe [Name],
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

-- | Configuration for the derivation rules for a unary operation that can be
-- derived by transforming each field and then combining the results.
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
      -- \| Total number of constructors
      Int ->
      -- \| Constructor index
      Int ->
      -- \| Constructor variant
      ConstructorVariant ->
      -- \| Constructor name
      Name ->
      -- \| Extra pattern expressions
      [Exp] ->
      -- \| Field result expressions
      [Exp] ->
      Q (Exp, [Bool]),
    fieldFunExp :: FieldFunExp
  }

-- | Configuration for the derivation rules for a unary operation.
class UnaryOpFunConfig config where
  genUnaryOpFun ::
    -- | Derive configuration
    DeriveConfig ->
    -- | Configuration
    config ->
    -- | Function names
    [Name] ->
    -- | Number of functor arguments to the class
    Int ->
    -- | Extra variables
    [(Type, Kind)] ->
    -- | Kept variables
    [(Type, Kind)] ->
    -- | Argument variables
    [(Type, Kind)] ->
    -- | Whether the variable is used in fields
    (Name -> Bool) ->
    -- | Constructor infos
    [ConstructorInfo] ->
    Q Dec

instance UnaryOpFunConfig UnaryOpFieldConfig where
  genUnaryOpFun _ _ funNames n _ _ _ _ [] =
    funD (funNames !! n) [clause [] (normalB [|error "impossible"|]) []]
  genUnaryOpFun _ config funNames n _ _ argTypes _ constructors = do
    clauses <-
      zipWithM
        ( genUnaryOpFieldClause
            config
            argTypes
            (length constructors)
        )
        [0 ..]
        constructors
    let instanceFunName = funNames !! n
    return $ FunD instanceFunName clauses

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

  let isTypeUsedInFields (VarT nm) = isVarUsedInFields result nm
      isTypeUsedInFields _ = False
  contextInstanceTypes <-
    traverse
      (unaryOpInstanceTypeFromConfig deriveConfig extraVars keptVars)
      (fromMaybe unaryOpInstanceNames unaryOpContextNames)
  ctxs <-
    traverse (uncurry $ ctxForVar contextInstanceTypes) $
      filter (isTypeUsedInFields . fst) keptVars
  let keptType = foldl AppT (ConT typName) $ fmap fst keptVars
  instanceFuns <-
    traverse
      ( \(UnaryOpConfig config funNames) ->
          genUnaryOpFun
            deriveConfig
            config
            funNames
            n
            extraVars
            keptVars
            argVars
            (isVarUsedInFields result)
            constructors
      )
      unaryOpConfigs
  let instanceName = unaryOpInstanceNames !! n
  instanceTypes <-
    traverse
      (unaryOpInstanceTypeFromConfig deriveConfig extraVars keptVars)
      unaryOpInstanceNames
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
        ( extraPreds
            ++ if null constructors
              then []
              else catMaybes ctxs
        )
        instanceType
        instanceFuns
    ]
