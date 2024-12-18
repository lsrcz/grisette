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
    defaultFieldResFun,
    genUnaryOpClause,
    genUnaryOpClass,
  )
where

import Control.Monad (replicateM, zipWithM)
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
  )
import Grisette.Internal.TH.Util (occName)
import Language.Haskell.TH
  ( Body (NormalB),
    Clause (Clause),
    Dec (FunD, InstanceD),
    Exp (ConE, VarE),
    Name,
    Pat (VarP, WildP),
    Pred,
    Q,
    Type (AppT, ArrowT, ConT, StarT, VarT),
    appE,
    conP,
    conT,
    newName,
    varE,
    varP,
    varT,
  )
import Language.Haskell.TH.Datatype
  ( ConstructorInfo (constructorFields, constructorName),
    TypeSubstitution (freeVariables),
    tvName,
  )
import Language.Haskell.TH.Datatype.TyVarBndr (TyVarBndr_, tvKind)

fieldFunExp :: [Name] -> Type -> M.Map Name Name -> Type -> Q Exp
fieldFunExp unaryOpFunNames keptType argToFunPat ty = do
  let allArgNames = M.keysSet argToFunPat
  let typeHasNoArg ty =
        S.fromList (freeVariables [ty]) `S.intersection` allArgNames == S.empty
  let fun0 = varE $ head unaryOpFunNames
  let fun1 b =
        [|
          $(varE $ unaryOpFunNames !! 1)
            $(fieldFunExp unaryOpFunNames keptType argToFunPat b)
          |]
  let fun2 b c =
        [|
          $(varE $ unaryOpFunNames !! 2)
            $(fieldFunExp unaryOpFunNames keptType argToFunPat b)
            $(fieldFunExp unaryOpFunNames keptType argToFunPat c)
          |]
  let fun3 b c d =
        [|
          $(varE $ unaryOpFunNames !! 3)
            $(fieldFunExp unaryOpFunNames keptType argToFunPat b)
            $(fieldFunExp unaryOpFunNames keptType argToFunPat c)
            $(fieldFunExp unaryOpFunNames keptType argToFunPat d)
          |]
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
      _ -> fail $ "fieldFunExp: unsupported type: " <> show ty
    _ -> fail $ "fieldFunExp: unsupported type: " <> show ty

funPatAndExps ::
  (M.Map Name Name -> Type -> Q Exp) -> [Name] -> [Type] -> Q ([Pat], [Exp])
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
  let argToFunPat = M.fromList $ mapMaybe (\(nm, mpat) -> fmap (nm,) mpat) args
  let funPats = fmap (maybe WildP VarP . snd) args
  fieldFunExps <- traverse (fieldFunExpGen argToFunPat) fields
  return (funPats, fieldFunExps)

-- | Configuration for a unary function field expression generation on a GADT.
data UnaryOpFieldConfig = UnaryOpFieldConfig
  { extraPatNames :: [String],
    fieldResFun :: [Exp] -> [Exp] -> [Exp] -> Q [Exp],
    fieldCombineFun :: Exp -> [Exp] -> [Exp] -> Q Exp
  }

-- | Default field result function.
defaultFieldResFun :: [Exp] -> [Exp] -> [Exp] -> Q [Exp]
defaultFieldResFun extraPatExps fieldFunExps fieldPatExps =
  zipWithM
    ( \field fun ->
        appE
          ( foldl
              (\exp name -> appE exp (return name))
              (return fun)
              extraPatExps
          )
          (return field)
    )
    fieldPatExps
    fieldFunExps

-- | Generate a clause for a unary function on a GADT.
genUnaryOpClause ::
  [Name] ->
  UnaryOpFieldConfig ->
  Type ->
  [Name] ->
  ConstructorInfo ->
  Q Clause
genUnaryOpClause
  unaryOpFunNames
  (UnaryOpFieldConfig {..})
  keptType
  argTypes
  conInfo = do
    let fields = constructorFields conInfo
    (funPats, fieldFunExps) <-
      funPatAndExps (fieldFunExp unaryOpFunNames keptType) argTypes fields
    extraPatNames <- traverse newName extraPatNames
    let extraPatExps = fmap VarE extraPatNames
    fieldsPatNames <- replicateM (length fields) $ newName "field"
    let extraPats = fmap VarP extraPatNames
    fieldPats <- conP (constructorName conInfo) (fmap varP fieldsPatNames)
    let fieldPatExps = fmap VarE fieldsPatNames

    fieldResExps <- fieldResFun extraPatExps fieldFunExps fieldPatExps

    resExp <-
      fieldCombineFun
        (ConE (constructorName conInfo))
        (VarE <$> extraPatNames)
        fieldResExps
    return $ Clause (funPats ++ extraPats ++ [fieldPats]) (NormalB resExp) []

-- | Configuration for a unary operation type class generation on a GADT.
data UnaryOpClassConfig = UnaryOpClassConfig
  { unaryOpFieldConfig :: UnaryOpFieldConfig,
    unaryOpInstanceNames :: [Name],
    unaryOpFunNames :: [Name]
  }

-- | Generate a unary operation type class instance for a GADT.
genUnaryOpClass ::
  UnaryOpClassConfig ->
  Int ->
  Name ->
  Q [Dec]
genUnaryOpClass (UnaryOpClassConfig {..}) n typName = do
  CheckArgsResult {..} <-
    checkArgs
      (occName $ head unaryOpInstanceNames)
      (length unaryOpInstanceNames - 1)
      typName
      n
  let ctxForVar :: TyVarBndr_ flag -> Q (Maybe Pred)
      ctxForVar var = case tvKind var of
        StarT ->
          Just <$> [t|$(conT $ head unaryOpInstanceNames) $(varT $ tvName var)|]
        AppT (AppT ArrowT StarT) StarT ->
          Just <$> [t|$(conT $ unaryOpInstanceNames !! 1) $(varT $ tvName var)|]
        AppT (AppT (AppT ArrowT StarT) StarT) StarT ->
          Just <$> [t|$(conT $ unaryOpInstanceNames !! 2) $(varT $ tvName var)|]
        AppT (AppT (AppT (AppT ArrowT StarT) StarT) StarT) StarT ->
          Just <$> [t|$(conT $ unaryOpInstanceNames !! 3) $(varT $ tvName var)|]
        AppT (AppT (AppT (AppT ArrowT StarT) StarT) StarT) _ ->
          fail $ "Unsupported kind: " <> show (tvKind var)
        _ -> return Nothing
  ctxs <- traverse ctxForVar $ filter (isVarUsedInFields . tvName) keptNewVars
  let keptType = foldl AppT (ConT typName) $ fmap VarT keptNewNames
  clauses <-
    traverse
      (genUnaryOpClause unaryOpFunNames unaryOpFieldConfig keptType argNewNames)
      constructors
  let instanceType = AppT (ConT $ unaryOpInstanceNames !! n) keptType
  let instanceFunName = unaryOpFunNames !! n
  let instanceFun = FunD instanceFunName clauses
  return
    [ InstanceD
        Nothing
        (catMaybes ctxs)
        instanceType
        [instanceFun]
    ]
