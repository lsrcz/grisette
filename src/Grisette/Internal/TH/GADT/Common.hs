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
    EvalModeConfig (..),
    DeriveConfig (..),
    extraConstraint,
    specializeResult,
    evalModeSpecializeList,
  )
where

import Control.Monad (foldM, unless, when)
import Data.Foldable (traverse_)
import qualified Data.Map as M
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as S
import GHC.TypeLits (KnownNat, Nat, type (<=))
import Grisette.Internal.Core.Data.Class.Mergeable
  ( Mergeable,
    Mergeable1,
    Mergeable2,
  )
import Grisette.Internal.SymPrim.FP (ValidFP)
import Grisette.Unified.Internal.EvalModeTag (EvalModeTag (C, S))
import Grisette.Unified.Internal.Util (DecideEvalMode)
import Language.Haskell.TH
  ( Kind,
    Name,
    Pred,
    Q,
    Type (AppT, ArrowT, ConT, PromotedT, StarT, VarT),
    conT,
    nameBase,
    newName,
  )
import Language.Haskell.TH.Datatype
  ( ConstructorInfo (constructorFields, constructorName, constructorVars),
    DatatypeInfo (datatypeCons, datatypeVars),
    TypeSubstitution (applySubstitution, freeVariables),
    reifyDatatype,
    tvName,
  )
import Language.Haskell.TH.Datatype.TyVarBndr (mapTVName, tvKind)

-- | Result of 'checkArgs' for a GADT.
data CheckArgsResult = CheckArgsResult
  { constructors :: [ConstructorInfo],
    keptNewVars :: [(Type, Kind)],
    argNewVars :: [(Type, Kind)],
    isVarUsedInFields :: Name -> Bool
  }

specializeResult :: [(Int, EvalModeTag)] -> CheckArgsResult -> Q CheckArgsResult
specializeResult evalModeConfigs result = do
  let modeToName C = 'C
      modeToName S = 'S
  map <-
    foldM
      ( \lst (n, tag) -> do
          let (_, knd) = lst !! n
          return $
            take n lst
              ++ [(PromotedT $ modeToName tag, knd)]
              ++ drop (n + 1) lst
      )
      (keptNewVars result)
      evalModeConfigs
  return $ result {keptNewVars = map}

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
        zipWith (\nm tv -> (VarT nm, tvKind tv)) keptNewNames keptVars
  let argVars = drop (length dvars - n) dvars
  argNewNames <- traverse (newName . nameBase . tvName) argVars
  let argNewVars =
        zipWith (\nm tv -> (VarT nm, tvKind tv)) argNewNames argVars
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
ctxForVar :: [Type] -> Type -> Kind -> Q (Maybe Pred)
ctxForVar instanceExps ty knd = case knd of
  StarT ->
    Just
      <$> [t|$(return $ head instanceExps) $(return ty)|]
  AppT (AppT ArrowT StarT) StarT ->
    Just
      <$> [t|$(return $ instanceExps !! 1) $(return ty)|]
  AppT (AppT (AppT ArrowT StarT) StarT) StarT ->
    Just
      <$> [t|$(return $ instanceExps !! 2) $(return ty)|]
  AppT (AppT (AppT (AppT ArrowT StarT) StarT) StarT) StarT ->
    Just
      <$> [t|$(return $ instanceExps !! 3) $(return ty)|]
  AppT (AppT (AppT (AppT ArrowT StarT) StarT) StarT) _ ->
    fail $ "Unsupported kind: " <> show knd
  _ -> return Nothing

data EvalModeConfig
  = EvalModeConstraints [Name]
  | EvalModeSpecified EvalModeTag

-- | Extra constraints for a GADT.
data DeriveConfig = DeriveConfig
  { evalModeConfig :: [(Int, EvalModeConfig)],
    bitSizePositions :: [Int],
    fpBitSizePositions :: [(Int, Int)],
    needExtraMergeable :: Bool
  }

evalModeSpecializeList :: DeriveConfig -> [(Int, EvalModeTag)]
evalModeSpecializeList DeriveConfig {..} =
  mapMaybe
    ( \(n, cfg) ->
        case cfg of
          EvalModeConstraints _ -> Nothing
          EvalModeSpecified tag -> Just (n, tag)
    )
    evalModeConfig

instance Semigroup DeriveConfig where
  (<>) = (<>)

instance Monoid DeriveConfig where
  mempty = DeriveConfig [] [] [] False
  mappend = (<>)

checkValidExtraConstraintPosition ::
  Name -> Name -> [(Type, Kind)] -> Int -> Q ()
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
  DeriveConfig -> Name -> Name -> [(Type, Kind)] -> Q ()
checkAllValidExtraConstraintPosition
  DeriveConfig {..}
  tyName
  instanceName
  args = do
    traverse_
      (checkValidExtraConstraintPosition tyName instanceName args . fst)
      evalModeConfig
    traverse_
      (checkValidExtraConstraintPosition tyName instanceName args)
      bitSizePositions
    traverse_
      (checkValidExtraConstraintPosition tyName instanceName args . fst)
      fpBitSizePositions
    traverse_
      (checkValidExtraConstraintPosition tyName instanceName args . snd)
      fpBitSizePositions

extraEvalModeConstraint ::
  Name -> Name -> [(Type, Kind)] -> (Int, EvalModeConfig) -> Q [Pred]
extraEvalModeConstraint
  tyName
  instanceName
  args
  (n, EvalModeConstraints names) = do
    let (arg, argKind) = args !! n
    when (argKind /= ConT ''EvalModeTag) $
      fail $
        "Cannot introduce EvalMode constraint for the "
          <> show n
          <> "th argument of "
          <> show tyName
          <> " when deriving the "
          <> show instanceName
          <> " instance because it is not an EvalModeTag."
    traverse (\nm -> [t|$(conT nm) $(return arg)|]) names
extraEvalModeConstraint _ _ _ (_, EvalModeSpecified _) = return []

extraBitSizeConstraint :: Name -> Name -> [(Type, Kind)] -> Int -> Q [Pred]
extraBitSizeConstraint tyName instanceName args n = do
  let (arg, argKind) = args !! n
  when (argKind /= ConT ''Nat) $
    fail $
      "Cannot introduce BitSize constraint for the "
        <> show n
        <> "th argument of "
        <> show tyName
        <> " when deriving the "
        <> show instanceName
        <> " instance because it is not a Nat."
  predKnown <- [t|KnownNat $(return arg)|]
  predPositive <- [t|1 <= $(return arg)|]
  return [predKnown, predPositive]

extraFpBitSizeConstraint :: Name -> Name -> [(Type, Kind)] -> (Int, Int) -> Q [Pred]
extraFpBitSizeConstraint tyName instanceName args (eb, sb) = do
  let (argEb, argEbKind) = args !! eb
  let (argSb, argSbKind) = args !! sb
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
  pred <- [t|ValidFP $(return argEb) $(return argSb)|]
  return [pred]

extraExtraMergeableConstraint :: [(Type, Kind)] -> Q [Pred]
extraExtraMergeableConstraint args = do
  catMaybes
    <$> traverse
      ( uncurry $
          ctxForVar
            [ ConT ''Mergeable,
              ConT ''Mergeable1,
              ConT ''Mergeable2
            ]
      )
      args

-- | Generate extra constraints for a GADT.
extraConstraint ::
  DeriveConfig -> Name -> Name -> [(Type, Kind)] -> [(Type, Kind)] -> Q [Pred]
extraConstraint
  deriveConfig@DeriveConfig {..}
  tyName
  instanceName
  extraArgs
  keptArgs = do
    checkAllValidExtraConstraintPosition
      deriveConfig
      tyName
      instanceName
      keptArgs
    evalModePreds <-
      traverse
        (extraEvalModeConstraint tyName instanceName keptArgs)
        evalModeConfig
    extraArgEvalModePreds <-
      if null evalModeConfig
        then
          traverse
            ( \(arg, kind) ->
                if kind == ConT ''EvalModeTag
                  then (: []) <$> [t|DecideEvalMode $(return arg)|]
                  else return []
            )
            extraArgs
        else return []
    bitSizePreds <-
      traverse
        (extraBitSizeConstraint tyName instanceName keptArgs)
        bitSizePositions
    fpBitSizePreds <-
      traverse
        (extraFpBitSizeConstraint tyName instanceName keptArgs)
        fpBitSizePositions
    extraMergeablePreds <-
      if needExtraMergeable
        then extraExtraMergeableConstraint keptArgs
        else return []
    return $
      extraMergeablePreds
        ++ concat
          ( extraArgEvalModePreds
              ++ evalModePreds
              ++ bitSizePreds
              ++ fpBitSizePreds
          )
