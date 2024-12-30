{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
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
    genMergeableNoExistential,
    genMergeableNoStrategy,
    genMergeableList,
  )
where

import Control.Monad (foldM, replicateM, zipWithM)
import qualified Data.Map as M
import Data.Maybe (catMaybes, isJust, mapMaybe)
import qualified Data.Set as S
import Data.Word (Word16, Word32, Word64, Word8)
import Grisette.Internal.Internal.Decl.Core.Data.Class.Mergeable
  ( Mergeable (rootStrategy),
    Mergeable1 (liftRootStrategy),
    Mergeable2 (liftRootStrategy2),
    Mergeable3 (liftRootStrategy3),
    MergingStrategy (NoStrategy, SimpleStrategy, SortedStrategy),
    product2Strategy,
    wrapStrategy,
  )
import Grisette.Internal.TH.GADT.Common
  ( CheckArgsResult
      ( CheckArgsResult,
        argVars,
        constructors,
        keptVars
      ),
    DeriveConfig (useNoStrategy, unconstrainedPositions),
    checkArgs,
    evalModeSpecializeList,
    extraConstraint,
    isVarUsedInFields,
    specializeResult,
  )
import Grisette.Internal.TH.GADT.UnaryOpCommon
  ( FieldFunExp,
    UnaryOpClassConfig
      ( UnaryOpClassConfig,
        unaryOpAllowExistential,
        unaryOpConfigs,
        unaryOpContextNames,
        unaryOpExtraVars,
        unaryOpInstanceNames,
        unaryOpInstanceTypeFromConfig
      ),
    UnaryOpConfig (UnaryOpConfig),
    UnaryOpFunConfig (genUnaryOpFun),
    defaultUnaryOpInstanceTypeFromConfig,
    genUnaryOpClass,
  )
import Grisette.Internal.TH.Util (dataTypeHasExistential, integerE, mangleName)
import Language.Haskell.TH
  ( Bang (Bang),
    Body (NormalB),
    Clause (Clause),
    Con (ForallC, GadtC),
    Dec (DataD, FunD, InstanceD, PragmaD, SigD),
    Exp (AppE, ConE, VarE),
    Inline (Inline),
    Kind,
    Name,
    Pat (SigP, VarP, WildP),
    Phases (AllPhases),
    Pragma (InlineP),
    Pred,
    Q,
    RuleMatch (FunLike),
    SourceStrictness (NoSourceStrictness),
    SourceUnpackedness (NoSourceUnpackedness),
    Type (AppT, ArrowT, ConT, ForallT, StarT, VarT),
    appE,
    caseE,
    conE,
    conT,
    integerL,
    lamE,
    litP,
    lookupTypeName,
    mkName,
    nameBase,
    newName,
    normalB,
    recP,
    sigP,
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
    resolveTypeSynonyms,
    tvName,
  )
import Language.Haskell.TH.Datatype.TyVarBndr
  ( TyVarBndrUnit,
    kindedTVSpecified,
    plainTVFlag,
    specifiedSpec,
  )
import Language.Haskell.TH.Lib (clause, conP, litE, match, stringL)
import Type.Reflection (SomeTypeRep (SomeTypeRep), TypeRep, typeRep)
import Unsafe.Coerce (unsafeCoerce)

genMergingInfoCon ::
  [TyVarBndrUnit] ->
  Name ->
  Bool ->
  ConstructorInfo ->
  Q (Con, Name, [Clause], [Clause], [Clause])
genMergingInfoCon dataTypeVars tyName isLast con = do
  let conName = mangleName $ constructorName con
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
  let originalName = mangleName $ datatypeName d
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

constructMergingStrategyExp :: ConstructorInfo -> [Exp] -> Q Exp
constructMergingStrategyExp _ [] = [|SimpleStrategy $ \_ t _ -> t|]
constructMergingStrategyExp conInfo [x] = do
  upname <- newName "a"
  let unwrapPat = conP (constructorName conInfo) [varP upname]
  let unwrapFun = lamE [unwrapPat] $ appE (varE 'unsafeCoerce) (varE upname)
  [|
    wrapStrategy
      $(return x)
      (unsafeCoerce . $(conE $ constructorName conInfo))
      $unwrapFun
    |]
constructMergingStrategyExp conInfo (x : xs) = do
  upnames <- replicateM (length xs + 1) $ newName "a"
  let wrapPat1 [] = error "Should not happen"
      wrapPat1 [x] = varP x
      wrapPat1 (x : xs) = tupP [varP x, wrapPat1 xs]
  let wrapped = foldl AppE (ConE $ constructorName conInfo) $ fmap VarE upnames
  let wrapFun =
        lamE
          [varP $ head upnames, wrapPat1 $ tail upnames]
          [|unsafeCoerce ($(return wrapped))|]
  let unwrapPat = conP (constructorName conInfo) $ fmap varP upnames
  let unwrapExp1 [] = error "Should not happen"
      unwrapExp1 [_] = error "Should not happen"
      unwrapExp1 [x, y] =
        [|(unsafeCoerce $(varE x), unsafeCoerce $(varE y))|]
      unwrapExp1 (x : xs) = [|(unsafeCoerce $(varE x), $(unwrapExp1 xs))|]
  let unwrapFun = lamE [unwrapPat] (unwrapExp1 upnames)
  let strategy1 [] = error "Should not happen"
      strategy1 [x] = return x
      strategy1 (x : xs) =
        [|
          product2Strategy
            ((,))
            (\(x, y) -> (x, y))
            $(return x)
            $(strategy1 xs)
          |]
  [|
    product2Strategy
      $wrapFun
      $unwrapFun
      $(return x)
      $(strategy1 xs)
    |]

genMergeFunClause' :: Name -> ConstructorInfo -> Q Clause
genMergeFunClause' conInfoName con = do
  let numExistential = length $ constructorVars con
  let numFields = length $ constructorFields con
  let argWildCards = replicate numExistential wildP :: [Q Pat]

  pnames <- replicateM numFields $ newName "s"
  clause
    ([conP conInfoName $ argWildCards ++ fmap varP pnames])
    (normalB (constructMergingStrategyExp con (map VarE pnames)))
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

mergeableFieldFunExp :: [Name] -> FieldFunExp
mergeableFieldFunExp unaryOpFunNames argToFunPat _ = go
  where
    go ty = do
      let allArgNames = M.keysSet argToFunPat
      let typeHasNoArg ty =
            S.fromList (freeVariables [ty])
              `S.intersection` allArgNames
              == S.empty
      let fun0a a = [|$(varE $ head unaryOpFunNames) @($(return a))|]
          fun1a a b = [|$(varE $ unaryOpFunNames !! 1) @($(return a)) $(go b)|]
          fun2a a b c =
            [|
              $(varE $ unaryOpFunNames !! 2)
                @($(return a))
                $(go b)
                $(go c)
              |]
          fun3a a b c d =
            [|
              $(varE $ unaryOpFunNames !! 3)
                @($(return a))
                $(go b)
                $(go c)
                $(go d)
              |]

      case ty of
        AppT (AppT (AppT a@(VarT _) b) c) d -> fun3a a b c d
        AppT (AppT a@(VarT _) b) c -> fun2a a b c
        AppT a@(VarT _) b -> fun1a a b
        _ | typeHasNoArg ty -> fun0a ty
        AppT a b | typeHasNoArg a -> fun1a a b
        AppT (AppT a b) c | typeHasNoArg a -> fun2a a b c
        AppT (AppT (AppT a b) c) d | typeHasNoArg a -> fun3a a b c d
        VarT nm -> case M.lookup nm argToFunPat of
          Just pname -> varE pname
          _ -> fail $ "defaultFieldFunExp: unsupported type: " <> show ty
        _ -> fail $ "defaultFieldFunExp: unsupported type: " <> show ty

mergeableInstanceNames :: [Name]
mergeableInstanceNames =
  [ ''Mergeable,
    ''Mergeable1,
    ''Mergeable2,
    ''Mergeable3
  ]

getMergeableInstanceName :: Int -> Name
getMergeableInstanceName n = mergeableInstanceNames !! n

rootStrategyFunNames :: [Name]
rootStrategyFunNames =
  [ 'rootStrategy,
    'liftRootStrategy,
    'liftRootStrategy2,
    'liftRootStrategy3
  ]

getMergeableFunName :: Int -> Name
getMergeableFunName n = rootStrategyFunNames !! n

mergeableNoExistentialConfig :: UnaryOpClassConfig
mergeableNoExistentialConfig =
  UnaryOpClassConfig
    { unaryOpConfigs =
        [ UnaryOpConfig
            MergeableNoExistentialConfig
              { mergeableNoExistentialFun =
                  mergeableFieldFunExp rootStrategyFunNames
              }
            rootStrategyFunNames
        ],
      unaryOpInstanceNames =
        [''Mergeable, ''Mergeable1, ''Mergeable2, ''Mergeable3],
      unaryOpExtraVars = const $ return [],
      unaryOpInstanceTypeFromConfig = defaultUnaryOpInstanceTypeFromConfig,
      unaryOpAllowExistential = False,
      unaryOpContextNames = Nothing
    }

newtype MergeableNoExistentialConfig = MergeableNoExistentialConfig
  { mergeableNoExistentialFun :: FieldFunExp
  }

instance UnaryOpFunConfig MergeableNoExistentialConfig where
  genUnaryOpFun
    _
    MergeableNoExistentialConfig {..}
    funNames
    n
    _
    _
    argTypes
    _
    constructors = do
      allFields <-
        mapM resolveTypeSynonyms $
          concatMap constructorFields constructors
      let usedArgs = S.fromList $ freeVariables allFields
      args <-
        traverse
          ( \(ty, _) -> do
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
      let argToFunPat =
            M.fromList $ mapMaybe (\(nm, mpat) -> fmap (nm,) mpat) args
      let funPats = fmap (maybe WildP VarP . snd) args
      let genAuxFunExp conInfo = do
            fields <- mapM resolveTypeSynonyms $ constructorFields conInfo
            defaultFieldFunExps <-
              traverse
                (mergeableNoExistentialFun argToFunPat M.empty)
                fields
            constructMergingStrategyExp conInfo defaultFieldFunExps
      auxExps <- mapM genAuxFunExp constructors
      funExp <- case auxExps of
        [] -> [|NoStrategy|]
        [singleExp] -> return singleExp
        _ -> do
          p <- newName "p"
          let numConstructors = length constructors
          let getIdx i =
                if numConstructors <= 2
                  then if i == 0 then [|False|] else [|True|]
                  else integerE i
          let getIdxPat i =
                if numConstructors <= 2
                  then conP (if i == 0 then 'False else 'True) []
                  else do
                    let w8Bound = fromIntegral (maxBound @Word8)
                    let w16Bound = fromIntegral (maxBound @Word16)
                    let w32Bound = fromIntegral (maxBound @Word32)
                    let w64Bound = fromIntegral (maxBound @Word64)
                    sigP
                      (litP (integerL i))
                      ( conT $
                          if
                            | numConstructors <= w8Bound + 1 -> ''Word8
                            | numConstructors <= w16Bound + 1 -> ''Word16
                            | numConstructors <= w32Bound + 1 -> ''Word32
                            | numConstructors <= w64Bound + 1 -> ''Word64
                            | otherwise -> ''Integer
                      )
          let idxFun =
                lamE [varP p] $
                  caseE
                    (varE p)
                    ( zipWith
                        ( \conIdx conInfo -> do
                            match
                              (recP (constructorName conInfo) [])
                              (normalB (getIdx conIdx))
                              []
                        )
                        [0 ..]
                        constructors
                    )
          let auxFun =
                lamE [varP p] $
                  caseE
                    (varE p)
                    ( zipWith
                        ( \conIdx exp -> do
                            match
                              (getIdxPat conIdx)
                              (normalB (return exp))
                              []
                        )
                        [0 ..]
                        auxExps
                        ++ [match wildP (normalB [|undefined|]) []]
                    )
          [|
            SortedStrategy $idxFun $auxFun
            |]
      let instanceFunName = funNames !! n
      return $
        FunD
          instanceFunName
          [ Clause
              funPats
              (NormalB funExp)
              []
          ]

-- | Generate 'Mergeable' instance for a GADT, using a given merging info
-- result.
genMergeable' ::
  DeriveConfig -> MergingInfoResult -> Name -> Int -> Q (Name, [Dec])
genMergeable' deriveConfig (MergingInfoResult infoName conInfoNames) typName n = do
  result@CheckArgsResult {..} <-
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
  let isTypeUsedInFields (VarT nm) = isVarUsedInFields result nm
      isTypeUsedInFields _ = False
  mergeableContexts <-
    traverse ctxForVar $
      filter (isTypeUsedInFields . fst) $
        fmap snd $
          filter (not . (`elem` unconstrainedPositions deriveConfig) . fst) $
            zip [0 ..] keptVars

  let instanceName = getMergeableInstanceName n
  let instanceHead = ConT instanceName
  extraPreds <-
    extraConstraint
      deriveConfig
      typName
      instanceName
      []
      keptVars
      constructors

  let targetType =
        foldl
          (\ty (var, _) -> AppT ty var)
          (ConT typName)
          (keptVars ++ argVars)
  let infoType = ConT infoName
  let mergingInfoFunFinalType = AppT (AppT ArrowT targetType) infoType

  let mergingInfoFunTypeWithoutCtx =
        foldr
          (((AppT . AppT ArrowT) . AppT (ConT ''MergingStrategy)) . fst)
          mergingInfoFunFinalType
          argVars

  let mergingInfoFunType =
        ForallT
          ( mapMaybe
              ( \(ty, knd) -> case ty of
                  VarT nm -> Just $ kindedTVSpecified nm knd
                  _ -> Nothing
              )
              $ keptVars ++ argVars
          )
          (extraPreds ++ catMaybes mergeableContexts)
          mergingInfoFunTypeWithoutCtx
  let mangledName = mangleName (datatypeName d)
  let mergingInfoFunName =
        mkName $
          "mergingInfo"
            <> (if n /= 0 then show n else "")
            <> mangledName
  let mergingInfoFunSigD = SigD mergingInfoFunName mergingInfoFunType
  clauses <-
    traverse (uncurry (genMergingInfoFunClause' argVars)) $
      zip conInfoNames constructors
  let mergingInfoFunDec = FunD mergingInfoFunName clauses

  let mergeFunType =
        AppT (AppT ArrowT infoType) (AppT (ConT ''MergingStrategy) targetType)
  let mergeFunName =
        mkName $
          "merge"
            <> (if n /= 0 then show n else "")
            <> mangledName
  let mergeFunSigD = SigD mergeFunName mergeFunType
  mergeFunClauses <- zipWithM genMergeFunClause' conInfoNames constructors
  let mergeFunDec = FunD mergeFunName mergeFunClauses

  let instanceType =
        AppT
          instanceHead
          (foldl AppT (ConT typName) $ fmap fst keptVars)

  let mergeInstanceFunName = getMergeableFunName n
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
      [ PragmaD (InlineP mergingInfoFunName Inline FunLike AllPhases),
        mergingInfoFunSigD,
        mergingInfoFunDec,
        PragmaD (InlineP mergeFunName Inline FunLike AllPhases),
        mergeFunSigD,
        mergeFunDec,
        InstanceD
          Nothing
          (extraPreds ++ catMaybes mergeableContexts)
          instanceType
          [FunD mergeInstanceFunName [mergeInstanceFunClause]]
      ]
    )

-- | Generate 'Mergeable' instance for a GADT without existential variables.
genMergeableNoExistential :: DeriveConfig -> Name -> Int -> Q [Dec]
genMergeableNoExistential deriveConfig typName n = do
  genUnaryOpClass deriveConfig mergeableNoExistentialConfig n typName

-- | Generate 'Mergeable' instance for a GADT, using 'NoStrategy'.
genMergeableNoStrategy :: DeriveConfig -> Name -> Int -> Q [Dec]
genMergeableNoStrategy deriveConfig typName n = do
  CheckArgsResult {..} <-
    specializeResult (evalModeSpecializeList deriveConfig)
      =<< checkArgs "Mergeable" 3 typName True n
  let instanceName = getMergeableInstanceName n
  let instanceHead = ConT instanceName
  let instanceType =
        AppT
          instanceHead
          (foldl AppT (ConT typName) $ fmap fst keptVars)
  let mergeInstanceFunName = getMergeableFunName n

  let mergeInstanceFunClause =
        Clause (replicate n WildP) (NormalB (ConE 'NoStrategy)) []
  return
    [ InstanceD
        Nothing
        []
        instanceType
        [FunD mergeInstanceFunName [mergeInstanceFunClause]]
    ]

-- | Generate 'Mergeable' instance for a GADT.
genMergeable :: DeriveConfig -> Name -> Int -> Q [Dec]
genMergeable deriveConfig typName n = do
  hasExistential <- dataTypeHasExistential typName
  if
    | useNoStrategy deriveConfig ->
        genMergeableNoStrategy deriveConfig typName n
    | hasExistential -> do
        (infoResult, infoDec) <- genMergingInfo typName
        (_, decs) <- genMergeable' deriveConfig infoResult typName n
        return $ infoDec ++ decs
    | otherwise -> genMergeableNoExistential deriveConfig typName n

-- | Generate multiple 'Mergeable' instances for a GADT.
genMergeableList :: DeriveConfig -> Name -> [Int] -> Q [Dec]
genMergeableList _ _ [] = return []
genMergeableList deriveConfig typName [n] = genMergeable deriveConfig typName n
genMergeableList deriveConfig typName l@(n : ns) = do
  hasExistential <- dataTypeHasExistential typName
  if
    | useNoStrategy deriveConfig ->
        concat <$> traverse (genMergeableNoStrategy deriveConfig typName) l
    | hasExistential -> do
        (info, dn) <-
          genMergeableAndGetMergingInfoResult
            deriveConfig
            typName
            n
        dns <-
          traverse (genMergeable' deriveConfig info typName) ns
        return $ dn ++ concatMap snd dns
    | otherwise ->
        concat <$> traverse (genMergeableNoExistential deriveConfig typName) l

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
