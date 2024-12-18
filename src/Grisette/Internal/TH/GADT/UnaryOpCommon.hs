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
    genUnaryOpClause,
    genUnaryOpClass',
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
    Exp (ConE),
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

fieldFunExp :: [Name] -> M.Map Name Name -> Type -> Q Exp
fieldFunExp unaryOpFunNames argToFunPat ty = do
  let notContains =
        M.null $
          M.restrictKeys argToFunPat (S.fromList $ freeVariables [ty])
  let allArgNames = M.keysSet argToFunPat
  let typeHasNoArg ty =
        S.fromList (freeVariables [ty]) `S.intersection` allArgNames == S.empty
  if notContains
    then varE $ head unaryOpFunNames
    else case ty of
      _ | typeHasNoArg ty -> [|$(varE $ head unaryOpFunNames)|]
      AppT a b | typeHasNoArg a -> do
        [|
          $(varE $ unaryOpFunNames !! 1)
            $(fieldFunExp unaryOpFunNames argToFunPat b)
          |]
      AppT (AppT a b) c
        | typeHasNoArg a ->
            [|
              $(varE $ unaryOpFunNames !! 2)
                $(fieldFunExp unaryOpFunNames argToFunPat b)
                $(fieldFunExp unaryOpFunNames argToFunPat c)
              |]
      AppT (AppT (AppT a b) c) d
        | typeHasNoArg a ->
            [|
              $(varE $ unaryOpFunNames !! 3)
                $(fieldFunExp unaryOpFunNames argToFunPat b)
                $(fieldFunExp unaryOpFunNames argToFunPat c)
                $(fieldFunExp unaryOpFunNames argToFunPat d)
              |]
      VarT nm -> do
        case M.lookup nm argToFunPat of
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
    fieldCombineFun :: Exp -> [Exp] -> Q Exp
  }

-- | Generate a clause for a unary function on a GADT.
genUnaryOpClause ::
  [Name] ->
  UnaryOpFieldConfig ->
  [Name] ->
  ConstructorInfo ->
  Q Clause
genUnaryOpClause
  unaryOpFunNames
  (UnaryOpFieldConfig {..})
  argTypes
  conInfo = do
    let fields = constructorFields conInfo
    (funPats, fieldFunExps) <-
      funPatAndExps (fieldFunExp unaryOpFunNames) argTypes fields
    extraPatNames <- traverse newName extraPatNames
    fieldsPatNames <- replicateM (length fields) $ newName "field"
    let extraPats = fmap VarP extraPatNames
    fieldPats <- conP (constructorName conInfo) (fmap varP fieldsPatNames)

    fieldResExps <-
      zipWithM
        ( \nm fun ->
            appE
              ( foldl
                  (\exp name -> appE exp (varE name))
                  (return fun)
                  extraPatNames
              )
              (varE nm)
        )
        fieldsPatNames
        fieldFunExps

    resExp <- fieldCombineFun (ConE (constructorName conInfo)) fieldResExps
    return $ Clause (funPats ++ extraPats ++ [fieldPats]) (NormalB resExp) []

-- | Configuration for a unary operation type class generation on a GADT.
data UnaryOpClassConfig = UnaryOpClassConfig
  { unaryOpFieldConfig :: UnaryOpFieldConfig,
    unaryOpInstanceNames :: [Name],
    unaryOpFunNames :: [Name]
  }

genUnaryOpClass' ::
  ( [Name] ->
    UnaryOpFieldConfig ->
    [Name] ->
    ConstructorInfo ->
    Q Clause
  ) ->
  UnaryOpClassConfig ->
  Int ->
  Name ->
  Q [Dec]
genUnaryOpClass' genUnaryOpClauseFun (UnaryOpClassConfig {..}) n typName = do
  CheckArgsResult {..} <-
    checkArgs
      (occName $ head unaryOpInstanceNames)
      (length unaryOpInstanceNames - 1)
      typName
      n
  let ctxForVar :: TyVarBndr_ flag -> Q (Maybe Pred)
      ctxForVar var = case tvKind var of
        StarT -> Just <$> [t|$(conT $ head unaryOpInstanceNames) $(varT $ tvName var)|]
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
  clauses <-
    traverse
      (genUnaryOpClauseFun unaryOpFunNames unaryOpFieldConfig argNewNames)
      constructors
  let instanceType =
        AppT (ConT $ unaryOpInstanceNames !! n) $
          foldl AppT (ConT typName) $
            fmap VarT keptNewNames
  let instanceFunName = unaryOpFunNames !! n
  let instanceFun = FunD instanceFunName clauses
  return
    [ InstanceD
        Nothing
        (catMaybes ctxs)
        instanceType
        [instanceFun]
    ]

-- | Generate a unary operation type class instance for a GADT.
genUnaryOpClass ::
  UnaryOpClassConfig ->
  Int ->
  Name ->
  Q [Dec]
genUnaryOpClass = genUnaryOpClass' genUnaryOpClause
