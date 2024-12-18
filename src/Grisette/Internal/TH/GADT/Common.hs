{-# LANGUAGE RecordWildCards #-}

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
  )
where

import Control.Monad (when)
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Haskell.TH
  ( Name,
    Q,
    Type (VarT),
    newName, nameBase,
  )
import Language.Haskell.TH.Datatype
  ( ConstructorInfo (constructorFields),
    DatatypeInfo (datatypeCons, datatypeVars),
    TypeSubstitution (applySubstitution, freeVariables),
    reifyDatatype,
    tvName,
  )
import Language.Haskell.TH.Datatype.TyVarBndr (TyVarBndr_, mapTVName)

-- | Result of 'checkArgs' for a GADT.
data CheckArgsResult = CheckArgsResult
  { constructors :: [ConstructorInfo],
    keptNewNames :: [Name],
    keptNewVars :: [TyVarBndr_ ()],
    argNewNames :: [Name],
    argNewVars :: [TyVarBndr_ ()],
    isVarUsedInFields :: Name -> Bool
  }

-- | Check if the number of type parameters is valid for a GADT, and return
-- new names for the type variables, split into kept and arg parts.
checkArgs ::
  String ->
  Int ->
  Name ->
  Int ->
  Q CheckArgsResult
checkArgs clsName maxArgNum typName n = do
  when (n < 0) $
    fail $
      unlines
        [ "Cannot derive "
            ++ clsName
            ++ " instance with negative type parameters",
          "Requested: " ++ show n,
          "Hint: Use a non-negative number of type parameters"
        ]
  when (n > maxArgNum) $
    fail $
      "Requesting "
        <> clsName
        <> " instance with more than "
        <> show maxArgNum
        <> " type parameters"
  d <- reifyDatatype typName
  let dvars = datatypeVars d
  when (length dvars < n) $
    fail $
      "Requesting Mergeable"
        <> show n
        <> " instance, while the type "
        <> show typName
        <> " has only "
        <> show (length dvars)
        <> " type variables."
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
  let constructors = applySubstitution substMap $ datatypeCons d
  let allFields = concatMap constructorFields constructors
  let allFieldsFreeVars = S.fromList $ freeVariables allFields
  let isVarUsedInFields var = S.member var allFieldsFreeVars
  return $ CheckArgsResult {..}
