{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    extraEvalModeConstraint,
    extraBitSizeConstraint,
    extraFpBitSizeConstraint,
    extraExtraMergeableConstraint,
    extraConstraint,
    specializeResult,
    evalModeSpecializeList,
    isVarUsedInFields,
    freshenCheckArgsResult,
  )
where

import Control.Monad (foldM, unless, when)
import Data.Bifunctor (first)
import qualified Data.Map as M
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as S
import GHC.TypeLits (KnownNat, Nat, type (<=))
import Grisette.Internal.Internal.Decl.Core.Data.Class.Mergeable
  ( Mergeable,
    Mergeable1,
    Mergeable2,
  )
import Grisette.Internal.SymPrim.FP (ValidFP)
import Grisette.Internal.Unified.EvalModeTag (EvalModeTag (C, S))
import Grisette.Internal.Unified.Util (DecideEvalMode)
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
    keptVars :: [(Type, Kind)],
    argVars :: [(Type, Kind)]
  }

-- | Specialize the evaluation mode tags for the t'CheckArgsResult'.
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
      (keptVars result)
      evalModeConfigs
  return $ result {keptVars = map}

freshenConstructorInfo :: ConstructorInfo -> Q ConstructorInfo
freshenConstructorInfo conInfo = do
  let vars = constructorVars conInfo
  newNames <- traverse (newName . nameBase . tvName) vars
  let newVars = zipWith (mapTVName . const) newNames vars
  let substMap = M.fromList $ zip (tvName <$> vars) $ VarT <$> newNames
  return $ applySubstitution substMap conInfo {constructorVars = newVars}

-- | Freshen the type variables in the t'CheckArgsResult'.
freshenCheckArgsResult :: Bool -> CheckArgsResult -> Q CheckArgsResult
freshenCheckArgsResult freshenNats result = do
  let genNewName :: (Type, Kind) -> Q (Maybe Name)
      genNewName (VarT _, knd) =
        if not freshenNats && knd == ConT ''Nat
          then return Nothing
          else Just <$> newName "a"
      genNewName _ = return Nothing
  keptNewNames <- traverse genNewName (keptVars result)
  argNewNames <- traverse genNewName (argVars result)

  let substMap =
        M.fromList
          $ mapMaybe
            ( \(newName, oldVar) ->
                case (newName, oldVar) of
                  (Just newName, (VarT oldName, _)) ->
                    Just (oldName, VarT newName)
                  _ -> Nothing
            )
          $ zip
            (keptNewNames ++ argNewNames)
            (keptVars result ++ argVars result)
  constructors <-
    mapM freshenConstructorInfo $
      applySubstitution substMap $
        constructors result
  let newKeptVars = first (applySubstitution substMap) <$> (keptVars result)
  let newArgVars = first (applySubstitution substMap) <$> (argVars result)
  return $
    result
      { constructors = constructors,
        keptVars = newKeptVars,
        argVars = newArgVars
      }

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
  let keptVars =
        (\bndr -> (VarT $ tvName bndr, tvKind bndr))
          <$> take (length dvars - n) dvars
  let argVars =
        (\bndr -> (VarT $ tvName bndr, tvKind bndr))
          <$> drop (length dvars - n) dvars
  let constructors = datatypeCons d
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
  return $ CheckArgsResult {..}

isVarUsedInConstructorFields :: [ConstructorInfo] -> Name -> Bool
isVarUsedInConstructorFields constructors var =
  let allFields = concatMap constructorFields constructors
      allFieldsFreeVars = S.fromList $ freeVariables allFields
   in S.member var allFieldsFreeVars

-- | Check if a variable is used in the fields of a constructor.
isVarUsedInFields :: CheckArgsResult -> Name -> Bool
isVarUsedInFields CheckArgsResult {..} =
  isVarUsedInConstructorFields constructors

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

-- | Configuration for constraints for evaluation modes tag.
--
-- * 'EvalModeConstraints' specifies a list of constraints for the tag, for
--   example, we may use 'Grisette.Unified.EvalModeBase' and
--   'Grisette.Unified.EvalModeBV' to specify that the evaluation mode must
--   support both base (boolean and data types) and bit vectors. This should be
--   used when the data type uses bit vectors.
--
-- * 'EvalModeSpecified' specifies a that an evaluation mode tag should be
--   specialized to a specific tag for all the instances.
data EvalModeConfig
  = EvalModeConstraints [Name]
  | EvalModeSpecified EvalModeTag

-- | Configuration for deriving instances for a GADT.
data DeriveConfig = DeriveConfig
  { evalModeConfig :: [(Int, EvalModeConfig)],
    bitSizePositions :: [Int],
    fpBitSizePositions :: [(Int, Int)],
    needExtraMergeableUnderEvalMode :: Bool,
    needExtraMergeableWithConcretizedEvalMode :: Bool,
    useNoStrategy :: Bool
  }

-- | Get all the evaluation modes to specialize in the t'DeriveConfig'.
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
  mempty = DeriveConfig [] [] [] False False False
  mappend = (<>)

-- | Generate extra constraints for evaluation modes.
extraEvalModeConstraint ::
  Name -> Name -> [(Type, Kind)] -> (Int, EvalModeConfig) -> Q [Pred]
extraEvalModeConstraint
  tyName
  instanceName
  args
  (n, EvalModeConstraints names)
    | n >= length args = return []
    | otherwise = do
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

-- | Generate extra constraints for bit vectors.
extraBitSizeConstraint :: Name -> Name -> [(Type, Kind)] -> Int -> Q [Pred]
extraBitSizeConstraint tyName instanceName args n
  | n >= length args = return []
  | otherwise = do
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

-- | Generate extra constraints for floating point exponents and significands.
extraFpBitSizeConstraint ::
  Name -> Name -> [(Type, Kind)] -> (Int, Int) -> Q [Pred]
extraFpBitSizeConstraint tyName instanceName args (eb, sb)
  | eb >= length args || sb >= length args = return []
  | otherwise = do
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

-- | Generate extra constraints for 'Mergeable' instances.
extraExtraMergeableConstraint :: [ConstructorInfo] -> [(Type, Kind)] -> Q [Pred]
extraExtraMergeableConstraint constructors args = do
  let isTypeUsedInFields' (VarT nm) =
        isVarUsedInConstructorFields constructors nm
      isTypeUsedInFields' _ = False
  catMaybes
    <$> traverse
      ( \(arg, knd) ->
          if isTypeUsedInFields' arg
            then
              ctxForVar
                [ ConT ''Mergeable,
                  ConT ''Mergeable1,
                  ConT ''Mergeable2
                ]
                arg
                knd
            else return Nothing
      )
      args

-- | Generate extra constraints for a GADT.
extraConstraint ::
  DeriveConfig ->
  Name ->
  Name ->
  [(Type, Kind)] ->
  [(Type, Kind)] ->
  [ConstructorInfo] ->
  Q [Pred]
extraConstraint
  DeriveConfig {..}
  tyName
  instanceName
  extraArgs
  keptArgs
  constructors = do
    -- checkAllValidExtraConstraintPosition
    --   deriveConfig
    --   tyName
    --   instanceName
    --   keptArgs
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
      if needExtraMergeableUnderEvalMode
        && ( any
               ( \case
                   (_, EvalModeConstraints _) -> True
                   (_, EvalModeSpecified _) -> False
               )
               evalModeConfig
               || needExtraMergeableWithConcretizedEvalMode
           )
        then extraExtraMergeableConstraint constructors keptArgs
        else return []
    return $
      extraMergeablePreds
        ++ concat
          ( extraArgEvalModePreds
              ++ evalModePreds
              ++ bitSizePreds
              ++ fpBitSizePreds
          )
