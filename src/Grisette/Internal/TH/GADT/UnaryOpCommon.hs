{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

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
    FieldFunExp,
    defaultFieldResFun,
    defaultFieldFunExp,
    genUnaryOpClause,
    genUnaryOpClass,
  )
where

import Control.Monad (replicateM, zipWithM)
import qualified Data.List as List
import qualified Data.Map as M
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as S
import Grisette.Internal.TH.GADT.Common
  ( CheckArgsResult
      ( CheckArgsResult,
        argNewNames,
        constructors,
        isVarUsedInFields,
        keptNewNames,
        keptNewVars
      ),
    checkArgs,
    ctxForVar,
  )
import Grisette.Internal.TH.Util (allUsedNames)
import Language.Haskell.TH
  ( Body (NormalB),
    Clause (Clause),
    Dec (FunD, InstanceD),
    Exp
      ( VarE
      ),
    Name,
    Pat (VarP, WildP),
    Q,
    Type (AppT, ConT, VarT),
    appE,
    conP,
    nameBase,
    newName,
    varE,
    varP,
  )
import Language.Haskell.TH.Datatype
  ( ConstructorInfo (constructorFields, constructorName, constructorVariant),
    ConstructorVariant,
    TypeSubstitution (freeVariables),
    resolveTypeSynonyms,
    tvName,
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
    fieldFunExp :: FieldFunExp,
    fieldFunNames :: [Name]
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
  [Name] ->
  [Type] ->
  Q ([Pat], [[Pat]], [Exp])
funPatAndExps fieldFunExpGen extraLiftedPatNames argTypes fields = do
  let usedArgs = S.fromList $ freeVariables fields
  let liftedNames = extraLiftedPatNames (length argTypes)
  args <-
    traverse
      ( \nm ->
          if S.member nm usedArgs
            then do
              pname <- newName "p"
              epname <- traverse newName liftedNames
              return (nm, Just (pname, epname))
            else return (nm, Nothing)
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
genUnaryOpClause ::
  UnaryOpFieldConfig ->
  [Name] ->
  Int ->
  ConstructorInfo ->
  Q Clause
genUnaryOpClause
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
  { unaryOpFieldConfigs :: [UnaryOpFieldConfig],
    unaryOpInstanceNames :: [Name]
  }

genUnaryOpFun ::
  UnaryOpFieldConfig ->
  Int ->
  [Name] ->
  [ConstructorInfo] ->
  Q Dec
genUnaryOpFun
  config
  n
  argNewNames
  constructors = do
    clauses <-
      zipWithM (genUnaryOpClause config argNewNames) [0 ..] constructors
    let instanceFunName = (fieldFunNames config) !! n
    return $ FunD instanceFunName clauses

-- | Generate a unary operation type class instance for a GADT.
genUnaryOpClass ::
  UnaryOpClassConfig ->
  Int ->
  Name ->
  Q [Dec]
genUnaryOpClass (UnaryOpClassConfig {..}) n typName = do
  CheckArgsResult {..} <-
    checkArgs
      (nameBase $ head unaryOpInstanceNames)
      (length unaryOpInstanceNames - 1)
      typName
      True
      n
  ctxs <-
    traverse (ctxForVar unaryOpInstanceNames) $
      filter (isVarUsedInFields . tvName) keptNewVars
  let keptType = foldl AppT (ConT typName) $ fmap VarT keptNewNames
  instanceFuns <-
    traverse
      ( \config ->
          genUnaryOpFun
            config
            n
            argNewNames
            constructors
      )
      unaryOpFieldConfigs
  let instanceType = AppT (ConT $ unaryOpInstanceNames !! n) keptType
  return
    [ InstanceD
        Nothing
        (catMaybes ctxs)
        instanceType
        instanceFuns
    ]
