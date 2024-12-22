{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      :   Grisette.Internal.TH.GADT.BinaryOpCommon
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.GADT.BinaryOpCommon
  ( BinaryOpClassConfig (..),
    BinaryOpFieldConfig (..),
    FieldFunExp,
    defaultFieldFunExp,
    genBinaryOpClause,
    genBinaryOpClass,
  )
where

import Control.Monad (replicateM, zipWithM)
import qualified Data.Map as M
import Data.Maybe (catMaybes, mapMaybe)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Set as S
import Grisette.Internal.TH.GADT.Common
  ( CheckArgsResult
      ( argNewNames,
        constructors,
        isVarUsedInFields,
        keptNewNames,
        keptNewVars
      ),
    DeriveConfig,
    checkArgs,
    ctxForVar,
    extraConstraint,
  )
import Language.Haskell.TH
  ( Clause,
    Dec (FunD, InstanceD),
    Exp (VarE),
    Name,
    Pat (VarP, WildP),
    Q,
    Type (AppT, ConT, VarT),
    clause,
    conP,
    nameBase,
    newName,
    normalB,
    recP,
    sigP,
    varE,
    varP,
    varT,
    wildP,
  )
import Language.Haskell.TH.Datatype
  ( ConstructorInfo (constructorFields, constructorName, constructorVars),
    TypeSubstitution (freeVariables),
    resolveTypeSynonyms,
    tvName,
  )
import Type.Reflection
  ( TypeRep,
    eqTypeRep,
    someTypeRep,
    typeRep,
    type (:~~:) (HRefl),
  )

-- | Type of field function expression generator.
type FieldFunExp = M.Map Name Name -> Type -> Q Exp

-- | Default field function expression generator.
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
  [Name] ->
  [Type] ->
  Q ([Pat], [Exp])
funPatAndExps fieldFunExpGen argTypes fields = do
  let usedArgs = S.fromList $ freeVariables fields
  args <-
    traverse
      ( \nm ->
          if S.member nm usedArgs
            then do
              pname <- newName "p"
              return (nm, Just pname)
            else return (nm, Nothing)
      )
      argTypes
  let argToFunPat =
        M.fromList $ mapMaybe (\(nm, mpat) -> fmap (nm,) mpat) args
  let funPats = fmap (maybe WildP VarP . snd) args
  defaultFieldFunExps <- traverse (fieldFunExpGen argToFunPat) fields
  return (funPats, defaultFieldFunExps)

-- | Configuration for a binary operation field generation on a GADT.
data BinaryOpFieldConfig = BinaryOpFieldConfig
  { fieldResFun :: (Exp, Exp) -> Exp -> Q Exp,
    fieldCombineFun :: [Exp] -> Q Exp,
    fieldDifferentExistentialFun :: Exp -> Q Exp,
    fieldLMatchResult :: Q Exp,
    fieldRMatchResult :: Q Exp,
    fieldFunExp :: FieldFunExp,
    fieldFunNames :: [Name]
  }

-- | Generate a clause for a binary operation on a GADT.
genBinaryOpClause ::
  BinaryOpFieldConfig ->
  [Name] ->
  [Name] ->
  Bool ->
  ConstructorInfo ->
  ConstructorInfo ->
  Q [Clause]
genBinaryOpClause
  (BinaryOpFieldConfig {..})
  lhsArgNewNames
  _rhsArgNewNames
  isLast
  lhsConstructors
  rhsConstructors =
    do
      lhsFields <- mapM resolveTypeSynonyms $ constructorFields lhsConstructors
      rhsFields <- mapM resolveTypeSynonyms $ constructorFields rhsConstructors
      (funPats, defaultFieldFunExps) <-
        funPatAndExps fieldFunExp lhsArgNewNames lhsFields
      lhsFieldsPatNames <- replicateM (length lhsFields) $ newName "lhsField"
      rhsFieldsPatNames <- replicateM (length rhsFields) $ newName "rhsField"
      let lhsFieldPats =
            conP
              (constructorName lhsConstructors)
              ( zipWith
                  (\nm field -> sigP (varP nm) (return field))
                  lhsFieldsPatNames
                  lhsFields
              )
      let rhsFieldPats =
            conP
              (constructorName rhsConstructors)
              ( zipWith
                  (\nm field -> sigP (varP nm) (return field))
                  rhsFieldsPatNames
                  rhsFields
              )
      let singleMatchPat =
            if null lhsFields
              then conP (constructorName lhsConstructors) []
              else recP (constructorName rhsConstructors) []
      let lhsFieldPatExps = fmap VarE lhsFieldsPatNames
      let rhsFieldPatExps = fmap VarE rhsFieldsPatNames

      fieldResExps <-
        zipWithM
          fieldResFun
          (zip lhsFieldPatExps rhsFieldPatExps)
          defaultFieldFunExps
      let resExp = fieldCombineFun fieldResExps

      let eqt l r =
            [|
              eqTypeRep
                (typeRep :: TypeRep $(varT $ tvName l))
                (typeRep :: TypeRep $(varT $ tvName r))
              |]
      let eqx trueCont l r = do
            cmp <-
              [|
                compare
                  (someTypeRep (Proxy :: Proxy $(varT $ tvName l)))
                  (someTypeRep (Proxy :: Proxy $(varT $ tvName r)))
                |]
            [|
              case $(eqt l r) of
                Just HRefl -> $(trueCont)
                _ ->
                  $(fieldDifferentExistentialFun cmp)
              |]
      let construct [] = resExp
          construct ((l, r) : xs) = [|$(eqx (construct xs) l r)|]
      bothMatched <-
        clause
          ((return <$> funPats) ++ [lhsFieldPats, rhsFieldPats])
          ( normalB
              [|
                $( construct $
                     zip
                       (constructorVars lhsConstructors)
                       (constructorVars rhsConstructors)
                 )
                |]
          )
          []
      lhsMatched <-
        clause
          ((wildP <$ funPats) ++ [singleMatchPat, wildP])
          (normalB [|$(fieldLMatchResult)|])
          []
      rhsMatched <-
        clause
          ((wildP <$ funPats) ++ [wildP, singleMatchPat])
          (normalB [|$(fieldRMatchResult)|])
          []
      if isLast
        then return [bothMatched]
        else return [bothMatched, lhsMatched, rhsMatched]

-- | Configuration for a binary operation type class generation on a GADT.
data BinaryOpClassConfig = BinaryOpClassConfig
  { binaryOpFieldConfigs :: [BinaryOpFieldConfig],
    binaryOpInstanceNames :: [Name]
  }

-- | Generate a function for a binary operation on a GADT.
genBinaryOpFun ::
  BinaryOpFieldConfig ->
  Int ->
  [Name] ->
  [Name] ->
  [ConstructorInfo] ->
  [ConstructorInfo] ->
  Q Dec
genBinaryOpFun
  config
  n
  lhsArgNewNames
  rhsArgNewNames
  lhsConstructors
  rhsConstructors = do
    clauses <-
      zipWithM
        (genBinaryOpClause config lhsArgNewNames rhsArgNewNames False)
        (init lhsConstructors)
        (init rhsConstructors)
    lastClause <-
      genBinaryOpClause
        config
        lhsArgNewNames
        rhsArgNewNames
        True
        (last lhsConstructors)
        (last rhsConstructors)
    let instanceFunName = (fieldFunNames config) !! n
    return $ FunD instanceFunName (concat clauses ++ lastClause)

-- | Generate a type class instance for a binary operation on a GADT.
genBinaryOpClass ::
  DeriveConfig -> BinaryOpClassConfig -> Int -> Name -> Q [Dec]
genBinaryOpClass deriveConfig (BinaryOpClassConfig {..}) n typName = do
  lhsResult <-
    checkArgs
      (nameBase $ head binaryOpInstanceNames)
      (length binaryOpInstanceNames - 1)
      typName
      (n == 0)
      n
  rhsResult <-
    checkArgs
      (nameBase $ head binaryOpInstanceNames)
      (length binaryOpInstanceNames - 1)
      typName
      (n == 0)
      n
  let keptNewVars' = keptNewVars lhsResult
  let keptNewNames' = keptNewNames lhsResult
  let isVarUsedInFields' = isVarUsedInFields lhsResult
  ctxs <-
    traverse (ctxForVar $ fmap ConT binaryOpInstanceNames) $
      filter (isVarUsedInFields' . tvName) keptNewVars'
  let keptType = foldl AppT (ConT typName) $ fmap VarT keptNewNames'
  instanceFuns <-
    traverse
      ( \config ->
          genBinaryOpFun
            config
            n
            (argNewNames lhsResult)
            (argNewNames rhsResult)
            (constructors lhsResult)
            (constructors rhsResult)
      )
      binaryOpFieldConfigs
  let instanceName = binaryOpInstanceNames !! n
  let instanceType = AppT (ConT instanceName) keptType
  extraPreds <- extraConstraint deriveConfig typName instanceName [] keptNewVars'
  return
    [ InstanceD
        Nothing
        (extraPreds ++ catMaybes ctxs)
        instanceType
        instanceFuns
    ]
