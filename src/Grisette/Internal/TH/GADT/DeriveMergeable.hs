{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :   Grisette.Internal.TH.GADT.DeriveMergeable
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.GADT.DeriveMergeable
  ( deriveGADTMergeable,
    deriveGADTMergeable1,
    deriveGADTMergeable2,
    deriveGADTMergeable3,
    genMergeableAndGetMergingInfoResult,
    genMergeable,
    genMergeable',
  )
where

import Control.Monad (foldM, replicateM, zipWithM)
import qualified Data.Map as M
import Data.Maybe (catMaybes, isJust, mapMaybe)
import qualified Data.Set as S
import Grisette.Internal.Core.Data.Class.Mergeable
  ( Mergeable (rootStrategy),
    Mergeable1 (liftRootStrategy),
    Mergeable2 (liftRootStrategy2),
    Mergeable3 (liftRootStrategy3),
    MergingStrategy (SimpleStrategy, SortedStrategy),
    product2Strategy,
    wrapStrategy,
  )
import Grisette.Internal.TH.GADT.Common
  ( CheckArgsResult
      ( CheckArgsResult,
        argNewVars,
        constructors,
        isVarUsedInFields,
        keptNewVars
      ),
    DeriveConfig,
    checkArgs,
    evalModeSpecializeList,
    extraConstraint,
    specializeResult,
  )
import Language.Haskell.TH
  ( Bang (Bang),
    Body (NormalB),
    Clause (Clause),
    Con (ForallC, GadtC),
    Dec (DataD, FunD, InstanceD, SigD),
    Exp (AppE, ConE, VarE),
    Kind,
    Name,
    Pat (SigP, VarP, WildP),
    Pred,
    Q,
    SourceStrictness (NoSourceStrictness),
    SourceUnpackedness (NoSourceUnpackedness),
    Type (AppT, ArrowT, ConT, ForallT, StarT, VarT),
    appE,
    conE,
    lamE,
    lookupTypeName,
    mkName,
    nameBase,
    newName,
    normalB,
    tupP,
    varE,
    varP,
    varT,
    wildP,
  )
import Language.Haskell.TH.Datatype
  ( ConstructorInfo
      ( constructorContext,
        constructorFields,
        constructorName,
        constructorVars
      ),
    DatatypeInfo (datatypeCons, datatypeName, datatypeVars),
    TypeSubstitution (applySubstitution, freeVariables),
    reifyDatatype,
    tvName,
  )
import Language.Haskell.TH.Datatype.TyVarBndr
  ( TyVarBndrUnit,
    kindedTV,
    plainTVFlag,
    specifiedSpec,
  )
import Language.Haskell.TH.Lib (clause, conP, litE, stringL)
import Type.Reflection (SomeTypeRep (SomeTypeRep), TypeRep, typeRep)
import Unsafe.Coerce (unsafeCoerce)

genMergingInfoCon ::
  [TyVarBndrUnit] ->
  Name ->
  Bool ->
  ConstructorInfo ->
  Q (Con, Name, [Clause], [Clause], [Clause])
genMergingInfoCon dataTypeVars tyName isLast con = do
  let conName = nameBase $ constructorName con
  let newConName = mkName $ conName <> "MergingInfo"
  let oriVars = dataTypeVars ++ constructorVars con
  newDataTypeVars <- traverse (newName . nameBase . tvName) dataTypeVars
  newConstructorVars <-
    traverse (newName . nameBase . tvName) $ constructorVars con
  let newNames = newDataTypeVars ++ newConstructorVars
  -- newNames <- traverse (newName . nameBase . tvName) oriVars
  let newVars = fmap VarT newNames
  let substMap = M.fromList $ zip (tvName <$> oriVars) newVars
  let fields =
        zip [0 ..] $
          applySubstitution substMap $
            constructorFields con
  let tyFields =
        AppT (ConT ''TypeRep)
          <$> applySubstitution
            substMap
            ((VarT . tvName) <$> constructorVars con)
  let strategyFields = fmap (AppT (ConT ''MergingStrategy) . snd) fields
  tyFieldNamesL <- traverse (const $ newName "p") tyFields
  tyFieldNamesR <- traverse (const $ newName "p") tyFields
  let tyFieldPatsL = fmap varP tyFieldNamesL
  let tyFieldPatsR = fmap varP tyFieldNamesR
  let tyFieldVarsL = fmap varE tyFieldNamesL
  let tyFieldVarsR = fmap varE tyFieldNamesR
  let strategyFieldPats = replicate (length strategyFields) wildP
  let patsL = tyFieldPatsL ++ strategyFieldPats
  let patsR = tyFieldPatsR ++ strategyFieldPats
  let allWildcards = fmap (const wildP) $ tyFieldPatsL ++ strategyFieldPats
  let eqCont l r cont =
        [|
          SomeTypeRep $l == SomeTypeRep $r
            && $cont
          |]
  let eqExp =
        foldl (\cont (l, r) -> eqCont l r cont) (conE 'True) $
          zip tyFieldVarsL tyFieldVarsR
  eqClause <-
    clause
      [conP newConName patsL, conP newConName patsR]
      (normalB eqExp)
      []
  let cmpCont l r cont =
        [|
          case SomeTypeRep $l `compare` SomeTypeRep $r of
            EQ -> $cont
            x -> x
          |]
  let cmpExp =
        foldl (\cont (l, r) -> cmpCont l r cont) (conE 'EQ) $
          zip tyFieldVarsL tyFieldVarsR
  cmpClause0 <-
    clause
      [conP newConName patsL, conP newConName patsR]
      (normalB cmpExp)
      []
  cmpClause1 <-
    clause
      [conP newConName allWildcards, wildP]
      (normalB $ conE 'LT)
      []
  cmpClause2 <-
    clause
      [wildP, conP newConName allWildcards]
      (normalB $ conE 'GT)
      []
  let cmpClauses =
        if isLast
          then [cmpClause0]
          else [cmpClause0, cmpClause1, cmpClause2]
  let showCont t cont =
        [|$cont <> " " <> show $t|]
  let showExp = foldl (flip showCont) (litE $ stringL conName) tyFieldVarsL
  showClause <-
    clause
      [conP newConName patsL]
      (normalB showExp)
      []
  let ctx = applySubstitution substMap $ constructorContext con
  let ctxAndGadtUsedVars =
        S.fromList (freeVariables ctx)
          <> S.fromList (freeVariables tyFields)
          <> S.fromList (freeVariables strategyFields)
  let isCtxAndGadtUsedVar nm = S.member nm ctxAndGadtUsedVars
  return
    ( ForallC
        ( (`plainTVFlag` specifiedSpec)
            <$> filter isCtxAndGadtUsedVar newDataTypeVars ++ newConstructorVars
        )
        ctx
        $ GadtC
          [newConName]
          ( (Bang NoSourceUnpackedness NoSourceStrictness,)
              <$> tyFields ++ strategyFields
          )
          (ConT tyName),
      newConName,
      [eqClause],
      cmpClauses,
      [showClause]
    )

data MergingInfoResult = MergingInfoResult
  { _infoName :: Name,
    _conInfoNames :: [Name]
  }

genMergingInfo :: Name -> Q (MergingInfoResult, [Dec])
genMergingInfo typName = do
  d <- reifyDatatype typName
  let originalName = nameBase $ datatypeName d
  let newName = originalName <> "MergingInfo"
  found <- lookupTypeName newName
  let constructors = datatypeCons d
  let name = mkName newName
  r <-
    if null constructors
      then return []
      else do
        cons0 <-
          traverse (genMergingInfoCon (datatypeVars d) name False) $
            init constructors
        consLast <-
          genMergingInfoCon (datatypeVars d) name True $
            last constructors
        return $ cons0 ++ [consLast]
  let cons = fmap (\(a, _, _, _, _) -> a) r
  let eqClauses =
        concatMap (\(_, _, a, _, _) -> a) r
          ++ [ Clause [WildP, WildP] (NormalB $ ConE 'False) []
               | length constructors > 1
             ]
  let cmpClauses = concatMap (\(_, _, _, a, _) -> a) r
  let showClauses = concatMap (\(_, _, _, _, a) -> a) r
  return
    ( MergingInfoResult
        name
        (fmap (\(_, a, _, _, _) -> a) r),
      if isJust found
        then []
        else
          [ DataD [] name [] Nothing cons [],
            InstanceD
              Nothing
              []
              (ConT ''Eq `AppT` ConT name)
              [FunD '(==) eqClauses],
            InstanceD
              Nothing
              []
              (ConT ''Ord `AppT` ConT name)
              [FunD 'compare cmpClauses],
            InstanceD
              Nothing
              []
              (ConT ''Show `AppT` ConT name)
              [FunD 'show showClauses]
          ]
    )

-- | Generate 'Mergeable' instance and merging information for a GADT.
genMergeableAndGetMergingInfoResult ::
  DeriveConfig -> Name -> Int -> Q (MergingInfoResult, [Dec])
genMergeableAndGetMergingInfoResult deriveConfig typName n = do
  (infoResult, infoDec) <- genMergingInfo typName
  (_, decs) <- genMergeable' deriveConfig infoResult typName n
  return (infoResult, infoDec ++ decs)

-- | Generate 'Mergeable' instance for a GADT.
genMergeable :: DeriveConfig -> Name -> Int -> Q [Dec]
genMergeable deriveConfig typName n = do
  (infoResult, infoDec) <- genMergingInfo typName
  (_, decs) <- genMergeable' deriveConfig infoResult typName n
  return $ infoDec ++ decs

genMergeFunClause' :: Name -> ConstructorInfo -> Q Clause
genMergeFunClause' conInfoName con = do
  let numExistential = length $ constructorVars con
  let numFields = length $ constructorFields con
  let argWildCards = replicate numExistential wildP :: [Q Pat]
  case numFields of
    0 -> do
      clause
        [conP conInfoName argWildCards]
        (normalB [|SimpleStrategy $ \_ t _ -> t|])
        []
    1 -> do
      pname <- newName "s"
      upname <- newName "a"
      let unwrapPat = conP (constructorName con) [varP upname]
      let unwrapFun = lamE [unwrapPat] $ appE (varE 'unsafeCoerce) (varE upname)
      clause
        [conP conInfoName $ argWildCards ++ [varP pname]]
        ( normalB
            [|
              wrapStrategy
                $(varE pname)
                (unsafeCoerce . $(conE $ constructorName con))
                $unwrapFun
              |]
        )
        []
    _ -> do
      -- fail $ show (argWildCards, conInfoName)
      pnames <- replicateM numFields $ newName "s"
      upnames <- replicateM numFields $ newName "a"
      let wrapPat1 [] = error "Should not happen"
          wrapPat1 [x] = varP x
          wrapPat1 (x : xs) = tupP [varP x, wrapPat1 xs]
      let wrapped = foldl AppE (ConE $ constructorName con) $ fmap VarE upnames
      let wrapFun =
            lamE
              [varP $ head upnames, wrapPat1 $ tail upnames]
              [|unsafeCoerce ($(return wrapped))|]
      let unwrapPat = conP (constructorName con) $ fmap varP upnames
      let unwrapExp1 [] = error "Should not happen"
          unwrapExp1 [_] = error "Should not happen"
          unwrapExp1 [x, y] =
            [|(unsafeCoerce $(varE x), unsafeCoerce $(varE y))|]
          unwrapExp1 (x : xs) = [|(unsafeCoerce $(varE x), $(unwrapExp1 xs))|]
      let unwrapFun = lamE [unwrapPat] (unwrapExp1 upnames)
      let strategy1 [] = error "Should not happen"
          strategy1 [x] = varE x
          strategy1 (x : xs) =
            [|
              product2Strategy
                ((,))
                (\(x, y) -> (x, y))
                $(varE x)
                $(strategy1 xs)
              |]
      clause
        ([conP conInfoName $ argWildCards ++ fmap varP pnames])
        ( normalB
            [|
              product2Strategy
                $wrapFun
                $unwrapFun
                $(varE $ head pnames)
                $(strategy1 $ tail pnames)
              |]
        )
        []

constructVarPats :: ConstructorInfo -> Q Pat
constructVarPats conInfo = do
  let fields = constructorFields conInfo
      capture n = return $ SigP WildP $ fields !! n
  conP (constructorName conInfo) $ capture <$> [0 .. length fields - 1]

genMergingInfoFunClause' ::
  [(Type, Kind)] -> Name -> ConstructorInfo -> Q Clause
genMergingInfoFunClause' argTypes conInfoName con = do
  let conVars = constructorVars con
  capturedVarTyReps <-
    traverse (\bndr -> [|typeRep @($(varT $ tvName bndr))|]) conVars
  varPat <- constructVarPats con
  let infoExpWithTypeReps = foldl AppE (ConE conInfoName) capturedVarTyReps

  let fields = constructorFields con
  let usedArgs = S.fromList $ freeVariables fields

  strategyNames <-
    traverse
      ( \(ty, _) ->
          case ty of
            VarT nm ->
              if S.member nm usedArgs
                then do
                  pname <- newName "p"
                  return (nm, Just pname)
                else return ('undefined, Nothing)
            _ -> return ('undefined, Nothing)
      )
      argTypes
  let argToStrategyPat =
        mapMaybe (\(nm, mpat) -> fmap (nm,) mpat) strategyNames
  let strategyPats = fmap (maybe WildP VarP . snd) strategyNames

  let argNameSet =
        S.fromList $
          mapMaybe
            ( \(ty, _) -> case ty of
                VarT nm -> Just nm
                _ -> Nothing
            )
            argTypes
  let containsArg :: Type -> Bool
      containsArg ty =
        S.intersection argNameSet (S.fromList (freeVariables [ty])) /= S.empty
  let typeHasNoArg = not . containsArg

  let fieldStrategyExp ty =
        if not (containsArg ty)
          then [|rootStrategy :: MergingStrategy $(return ty)|]
          else case ty of
            _
              | typeHasNoArg ty ->
                  [|rootStrategy :: MergingStrategy $(return ty)|]
            AppT a b
              | typeHasNoArg a ->
                  [|
                    liftRootStrategy
                      $(fieldStrategyExp b) ::
                      MergingStrategy $(return ty)
                    |]
            AppT (AppT a b) c
              | typeHasNoArg a ->
                  [|
                    liftRootStrategy2
                      $(fieldStrategyExp b)
                      $(fieldStrategyExp c) ::
                      MergingStrategy $(return ty)
                    |]
            AppT (AppT (AppT a b) c) d
              | typeHasNoArg a ->
                  [|
                    liftRootStrategy3
                      $(fieldStrategyExp b)
                      $(fieldStrategyExp c)
                      $(fieldStrategyExp d) ::
                      MergingStrategy $(return ty)
                    |]
            VarT nm -> do
              case lookup nm argToStrategyPat of
                Just pname -> varE pname
                _ -> fail "BUG: fieldStrategyExp"
            _ -> fail $ "fieldStrategyExp: unsupported type: " <> show ty
  fieldStrategyExps <- traverse fieldStrategyExp fields
  let infoExp = foldl AppE infoExpWithTypeReps fieldStrategyExps
  -- fail $ show infoExp
  return $ Clause (strategyPats ++ [varPat]) (NormalB infoExp) []

-- | Generate 'Mergeable' instance for a GADT, using a given merging info
-- result.
genMergeable' ::
  DeriveConfig -> MergingInfoResult -> Name -> Int -> Q (Name, [Dec])
genMergeable' deriveConfig (MergingInfoResult infoName conInfoNames) typName n = do
  CheckArgsResult {..} <-
    specializeResult (evalModeSpecializeList deriveConfig)
      =<< checkArgs "Mergeable" 3 typName True n

  d <- reifyDatatype typName
  let ctxForVar :: (Type, Kind) -> Q (Maybe Pred)
      ctxForVar (ty, kind) = case kind of
        StarT -> Just <$> [t|Mergeable $(return ty)|]
        AppT (AppT ArrowT StarT) StarT ->
          Just <$> [t|Mergeable1 $(return ty)|]
        AppT (AppT (AppT ArrowT StarT) StarT) StarT ->
          Just <$> [t|Mergeable2 $(return ty)|]
        AppT (AppT (AppT (AppT ArrowT StarT) StarT) StarT) StarT ->
          Just <$> [t|Mergeable3 $(return ty)|]
        AppT (AppT (AppT (AppT ArrowT StarT) StarT) StarT) _ ->
          fail $ "Unsupported kind: " <> show kind
        _ -> return Nothing
  let isTypeUsedInFields (VarT nm) = isVarUsedInFields nm
      isTypeUsedInFields _ = False
  mergeableContexts <-
    traverse ctxForVar $ filter (isTypeUsedInFields . fst) keptNewVars

  let instanceName =
        case n of
          0 -> ''Mergeable
          1 -> ''Mergeable1
          2 -> ''Mergeable2
          3 -> ''Mergeable3
          _ -> error "Unsupported n"

  let instanceHead = ConT instanceName
  extraPreds <- extraConstraint deriveConfig typName instanceName [] keptNewVars

  let targetType =
        foldl
          (\ty (var, _) -> AppT ty var)
          (ConT typName)
          (keptNewVars ++ argNewVars)
  let infoType = ConT infoName
  let mergingInfoFunFinalType = AppT (AppT ArrowT targetType) infoType

  let mergingInfoFunTypeWithoutCtx =
        foldr
          (((AppT . AppT ArrowT) . AppT (ConT ''MergingStrategy)) . fst)
          mergingInfoFunFinalType
          argNewVars

  let mergingInfoFunType =
        ForallT
          ( mapMaybe
              ( \(ty, knd) -> case ty of
                  VarT nm -> Just $ kindedTV nm knd
                  _ -> Nothing
              )
              $ keptNewVars ++ argNewVars
          )
          (extraPreds ++ catMaybes mergeableContexts)
          mergingInfoFunTypeWithoutCtx
  let mergingInfoFunName =
        mkName $
          "mergingInfo"
            <> (if n /= 0 then show n else "")
            <> nameBase (datatypeName d)
  let mergingInfoFunSigD = SigD mergingInfoFunName mergingInfoFunType
  clauses <-
    traverse (uncurry (genMergingInfoFunClause' argNewVars)) $
      zip conInfoNames constructors
  let mergingInfoFunDec = FunD mergingInfoFunName clauses

  let mergeFunType =
        AppT (AppT ArrowT infoType) (AppT (ConT ''MergingStrategy) targetType)
  let mergeFunName =
        mkName $
          "merge"
            <> (if n /= 0 then show n else "")
            <> nameBase (datatypeName d)
  let mergeFunSigD = SigD mergeFunName mergeFunType
  mergeFunClauses <- zipWithM genMergeFunClause' conInfoNames constructors
  let mergeFunDec = FunD mergeFunName mergeFunClauses

  let instanceType =
        AppT
          instanceHead
          (foldl AppT (ConT typName) $ fmap fst keptNewVars)

  let mergeInstanceFunName = case n of
        0 -> 'rootStrategy
        1 -> 'liftRootStrategy
        2 -> 'liftRootStrategy2
        3 -> 'liftRootStrategy3
        _ -> error "Unsupported n"
  mergeInstanceFunPatNames <- replicateM n $ newName "rootStrategy"
  let mergeInstanceFunPats = VarP <$> mergeInstanceFunPatNames

  mergeInstanceFunBody <-
    [|
      SortedStrategy
        $( foldM
             (\exp name -> appE (return exp) $ varE name)
             (VarE mergingInfoFunName)
             mergeInstanceFunPatNames
         )
        $(varE mergeFunName)
      |]

  let mergeInstanceFunClause =
        Clause mergeInstanceFunPats (NormalB mergeInstanceFunBody) []

  return
    ( mergingInfoFunName,
      [ mergingInfoFunSigD,
        mergingInfoFunDec,
        mergeFunSigD,
        mergeFunDec,
        InstanceD
          Nothing
          (extraPreds ++ catMaybes mergeableContexts)
          instanceType
          [FunD mergeInstanceFunName [mergeInstanceFunClause]]
      ]
    )

-- | Derive 'Mergeable' instance for GADT.
deriveGADTMergeable :: DeriveConfig -> Name -> Q [Dec]
deriveGADTMergeable deriveConfig nm = genMergeable deriveConfig nm 0

-- | Derive 'Mergeable1' instance for GADT.
deriveGADTMergeable1 :: DeriveConfig -> Name -> Q [Dec]
deriveGADTMergeable1 deriveConfig nm = genMergeable deriveConfig nm 1

-- | Derive 'Mergeable2' instance for GADT.
deriveGADTMergeable2 :: DeriveConfig -> Name -> Q [Dec]
deriveGADTMergeable2 deriveConfig nm = genMergeable deriveConfig nm 2

-- | Derive 'Mergeable3' instance for GADT.
deriveGADTMergeable3 :: DeriveConfig -> Name -> Q [Dec]
deriveGADTMergeable3 deriveConfig nm = genMergeable deriveConfig nm 3
