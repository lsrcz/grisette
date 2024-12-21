{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

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
    extraConstraint,
    ExtraConstraint (..),
  )
where

import Control.Monad (unless, when)
import Data.Foldable (traverse_)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import GHC.TypeLits (KnownNat, Nat, type (<=))
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.SymPrim.FP (ValidFP)
import Grisette.Unified.Internal.EvalModeTag (EvalModeTag (C, S))
import Language.Haskell.TH
  ( Name,
    Pred,
    Q,
    Type (AppT, ArrowT, ConT, StarT, VarT),
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

-- | Extra constraints for a GADT.
data ExtraConstraint = ExtraConstraint
  { evalModeConstraint :: [(Int, Name)],
    evalModeSpecificConstraint :: [(Int, EvalModeTag)],
    bitSizeConstraint :: [Int],
    fpBitSizeConstraint :: [(Int, Int)],
    needExtraMergeable :: Bool
  }

instance Semigroup ExtraConstraint where
  (<>) = (<>)

instance Monoid ExtraConstraint where
  mempty = ExtraConstraint [] [] [] [] False
  mappend = (<>)

checkValidExtraConstraintPosition ::
  Name -> Name -> [TyVarBndr_ ()] -> Int -> Q ()
checkValidExtraConstraintPosition tyName instanceName args n = do
  when (n >= length args) $
    fail $
      "Cannot introduce extra constraint for the "
        <> show n
        <> "th argument of "
        <> show tyName
        <> " when deriving the "
        <> show instanceName
        <> " instance because there are only "
        <> show (length args)
        <> " arguments."

checkAllValidExtraConstraintPosition ::
  ExtraConstraint -> Name -> Name -> [TyVarBndr_ ()] -> Q ()
checkAllValidExtraConstraintPosition
  ExtraConstraint {..}
  tyName
  instanceName
  args = do
    traverse_
      (checkValidExtraConstraintPosition tyName instanceName args . fst)
      evalModeConstraint
    traverse_
      (checkValidExtraConstraintPosition tyName instanceName args . fst)
      evalModeSpecificConstraint
    traverse_
      (checkValidExtraConstraintPosition tyName instanceName args)
      bitSizeConstraint
    traverse_
      (checkValidExtraConstraintPosition tyName instanceName args . fst)
      fpBitSizeConstraint
    traverse_
      (checkValidExtraConstraintPosition tyName instanceName args . snd)
      fpBitSizeConstraint

extraEvalModeConstraint ::
  Name -> Name -> [TyVarBndr_ ()] -> (Int, Name) -> Q [Pred]
extraEvalModeConstraint tyName instanceName args (n, nm) = do
  let arg = args !! n
  let argKind = tvKind arg
  when (argKind /= ConT ''EvalModeTag) $
    fail $
      "Cannot introduce EvalMode constraint for the "
        <> show n
        <> "th argument of "
        <> show tyName
        <> " when deriving the "
        <> show instanceName
        <> " instance because it is not an EvalModeTag."
  pred <- [t|$(conT nm) $(varT $ tvName arg)|]
  return [pred]

extraEvalModeSpecificConstraint ::
  Name -> Name -> [TyVarBndr_ ()] -> (Int, EvalModeTag) -> Q [Pred]
extraEvalModeSpecificConstraint tyName instanceName args (n, tag) = do
  let arg = args !! n
  let argKind = tvKind arg
  when (argKind /= ConT ''EvalModeTag) $
    fail $
      "Cannot introduce EvalMode constraint for the "
        <> show n
        <> "th argument of "
        <> show tyName
        <> " when deriving the "
        <> show instanceName
        <> " instance because it is not an EvalModeTag."
  pred <-
    case tag of
      S -> [t|$(varT $ tvName arg) ~ 'S|]
      C -> [t|$(varT $ tvName arg) ~ 'C|]
  return [pred]

extraBitSizeConstraint :: Name -> Name -> [TyVarBndr_ ()] -> Int -> Q [Pred]
extraBitSizeConstraint tyName instanceName args n = do
  let arg = args !! n
  let argKind = tvKind arg
  when (argKind /= ConT ''Nat) $
    fail $
      "Cannot introduce BitSize constraint for the "
        <> show n
        <> "th argument of "
        <> show tyName
        <> " when deriving the "
        <> show instanceName
        <> " instance because it is not a Nat."
  predKnown <- [t|KnownNat $(varT $ tvName arg)|]
  predPositive <- [t|1 <= $(varT $ tvName arg)|]
  return [predKnown, predPositive]

extraFpBitSizeConstraint :: Name -> Name -> [TyVarBndr_ ()] -> (Int, Int) -> Q [Pred]
extraFpBitSizeConstraint tyName instanceName args (eb, sb) = do
  let argEb = args !! eb
  let argEbKind = tvKind argEb
  let argSb = args !! sb
  let argSbKind = tvKind argSb
  when (argEbKind /= ConT ''Nat || argSbKind /= ConT ''Nat) $
    fail $
      "Cannot introduce ValidFP constraint for the "
        <> show eb
        <> "th and "
        <> show sb
        <> "th arguments of "
        <> show tyName
        <> " when deriving the "
        <> show instanceName
        <> " instance because they are not Nats."
  pred <- [t|ValidFP $(varT $ tvName argEb) $(varT $ tvName argSb)|]
  return [pred]

extraExtraMergeableConstraint :: [TyVarBndr_ ()] -> Q [Pred]
extraExtraMergeableConstraint args = do
  catMaybes <$> traverse (ctxForVar [''Mergeable]) args

-- | Generate extra constraints for a GADT.
extraConstraint ::
  ExtraConstraint -> Name -> Name -> [TyVarBndr_ ()] -> Q [Pred]
extraConstraint extra@ExtraConstraint {..} tyName instanceName args = do
  checkAllValidExtraConstraintPosition
    extra
    tyName
    instanceName
    args
  evalModePreds <-
    traverse
      (extraEvalModeConstraint tyName instanceName args)
      evalModeConstraint
  evalModeSpecificPreds <-
    traverse
      (extraEvalModeSpecificConstraint tyName instanceName args)
      evalModeSpecificConstraint
  bitSizePreds <-
    traverse (extraBitSizeConstraint tyName instanceName args) bitSizeConstraint
  fpBitSizePreds <-
    traverse
      (extraFpBitSizeConstraint tyName instanceName args)
      fpBitSizeConstraint
  extraMergeablePreds <-
    if needExtraMergeable
      then extraExtraMergeableConstraint args
      else return []
  return $
    extraMergeablePreds
      ++ concat
        ( evalModePreds
            ++ evalModeSpecificPreds
            ++ bitSizePreds
            ++ fpBitSizePreds
        )
