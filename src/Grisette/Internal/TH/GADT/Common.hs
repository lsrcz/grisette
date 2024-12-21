{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :   Grisette.Internal.TH.GADT.Common
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.GADT.Common
  ( CheckArgsResult (..),
    checkArgs,
    ctxForVar,
  )
where

import Control.Monad (unless, when)
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Haskell.TH
  ( Name,
    Pred,
    Q,
    Type (AppT, ArrowT, StarT, VarT),
    conT,
    nameBase,
    newName,
    varT,
  )
import Language.Haskell.TH.Datatype
  ( ConstructorInfo (constructorFields, constructorName, constructorVars),
    DatatypeInfo (datatypeCons, datatypeVars),
    TypeSubstitution (applySubstitution, freeVariables),
    reifyDatatype,
    tvName,
  )
import Language.Haskell.TH.Datatype.TyVarBndr (TyVarBndr_, mapTVName, tvKind)

-- | Result of 'checkArgs' for a GADT.
data CheckArgsResult = CheckArgsResult
  { constructors :: [ConstructorInfo],
    keptNewNames :: [Name],
    keptNewVars :: [TyVarBndr_ ()],
    argNewNames :: [Name],
    argNewVars :: [TyVarBndr_ ()],
    isVarUsedInFields :: Name -> Bool
  }

freshenConstructorInfo :: ConstructorInfo -> Q ConstructorInfo
freshenConstructorInfo conInfo = do
  let vars = constructorVars conInfo
  newNames <- traverse (newName . nameBase . tvName) vars
  let newVars = zipWith (mapTVName . const) newNames vars
  let substMap = M.fromList $ zip (tvName <$> vars) $ VarT <$> newNames
  return $ applySubstitution substMap conInfo {constructorVars = newVars}

-- | Check if the number of type parameters is valid for a GADT, and return
-- new names for the type variables, split into kept and arg parts.
checkArgs ::
  String ->
  Int ->
  Name ->
  Bool ->
  Int ->
  Q CheckArgsResult
checkArgs clsName maxArgNum typName allowExistential n = do
  when (n < 0) $
    fail $
      unlines
        [ "Cannot derive "
            ++ clsName
            ++ " instance with negative type parameters",
          "\tRequested: " ++ show n,
          "\tHint: Use a non-negative number of type parameters"
        ]
  when (n > maxArgNum) $
    fail $
      unlines
        [ "Cannot derive "
            <> clsName
            <> " instance with more than "
            <> show maxArgNum
            <> " type parameters",
          "\tRequested: " <> show n
        ]
  d <- reifyDatatype typName
  let dvars = datatypeVars d
  when (length dvars < n) $
    fail $
      unlines
        [ "Cannot derive "
            <> clsName
            <> show n
            <> " instance for the type "
            <> show typName,
          "\tReason: The type "
            <> show typName
            <> " has only "
            <> show (length dvars)
            <> " type variables."
        ]
  let keptVars = take (length dvars - n) dvars
  keptNewNames <- traverse (newName . nameBase . tvName) keptVars
  let keptNewVars =
        zipWith (mapTVName . const) keptNewNames keptVars
  let argVars = drop (length dvars - n) dvars
  argNewNames <- traverse (newName . nameBase . tvName) argVars
  let argNewVars =
        zipWith (mapTVName . const) argNewNames argVars
  let substMap =
        M.fromList $
          zip
            (tvName <$> dvars)
            (VarT <$> keptNewNames ++ argNewNames)
  constructors <-
    mapM freshenConstructorInfo $
      applySubstitution substMap $
        datatypeCons d
  unless allowExistential $
    mapM_
      ( \c ->
          when (constructorVars c /= []) $
            fail $
              unlines
                [ "Cannot derive "
                    <> clsName
                    <> show n
                    <> " instance for the type "
                    <> show typName,
                  "\tReason: The constructor "
                    <> nameBase (constructorName c)
                    <> " has existential variables"
                ]
      )
      constructors
  mapM_
    ( \c -> do
        let fields = constructorFields c
        let existentialVars = tvName <$> constructorVars c
        let fieldReferencedVars = freeVariables fields
        let notReferencedVars =
              S.fromList existentialVars S.\\ S.fromList fieldReferencedVars
        unless (null notReferencedVars) $
          fail $
            unlines
              [ "Cannot derive "
                  <> clsName
                  <> show n
                  <> " instance for the type "
                  <> show typName,
                "Reason: Ambiguous existential variable in the constructor: "
                  <> nameBase (constructorName c)
                  <> ", this is not supported. Please consider binding the "
                  <> "existential variable to a field. You can use Proxy type to "
                  <> "do this."
              ]
    )
    constructors
  let allFields = concatMap constructorFields constructors
  let allFieldsFreeVars = S.fromList $ freeVariables allFields
  let isVarUsedInFields var = S.member var allFieldsFreeVars
  return $ CheckArgsResult {..}

-- | Generate a context for a variable in a GADT.
ctxForVar :: [Name] -> TyVarBndr_ flag -> Q (Maybe Pred)
ctxForVar instanceNames var = case tvKind var of
  StarT ->
    Just
      <$> [t|$(conT $ head instanceNames) $(varT $ tvName var)|]
  AppT (AppT ArrowT StarT) StarT ->
    Just
      <$> [t|$(conT $ instanceNames !! 1) $(varT $ tvName var)|]
  AppT (AppT (AppT ArrowT StarT) StarT) StarT ->
    Just
      <$> [t|$(conT $ instanceNames !! 2) $(varT $ tvName var)|]
  AppT (AppT (AppT (AppT ArrowT StarT) StarT) StarT) StarT ->
    Just
      <$> [t|$(conT $ instanceNames !! 3) $(varT $ tvName var)|]
  AppT (AppT (AppT (AppT ArrowT StarT) StarT) StarT) _ ->
    fail $ "Unsupported kind: " <> show (tvKind var)
  _ -> return Nothing
