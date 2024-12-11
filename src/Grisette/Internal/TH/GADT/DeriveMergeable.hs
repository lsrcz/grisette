{-# LANGUAGE FlexibleContexts #-}
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
  )
where

import Control.Monad (foldM, replicateM, zipWithM)
import qualified Data.Map as M
import Data.Maybe (isJust, mapMaybe)
import Data.Proxy (Proxy (Proxy))
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
import Grisette.Internal.TH.GADT.Common (checkArgs)
import Grisette.Internal.TH.Util (occName)
import Language.Haskell.TH
  ( Bang (Bang),
    Body (NormalB),
    Clause (Clause),
    Con (ForallC, GadtC),
    Dec (DataD, FunD, InstanceD, SigD),
    Exp (AppE, ConE, VarE),
    Name,
    Pat (SigP, VarP, WildP),
    Q,
    SourceStrictness (NoSourceStrictness),
    SourceUnpackedness (NoSourceUnpackedness),
    Type (AppT, ArrowT, ConT, ForallT, VarT),
    appE,
    conE,
    conT,
    lamE,
    lookupTypeName,
    mkName,
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
    mapTVFlag,
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
  Q (Con, Name, S.Set Int, [Clause], [Clause], [Clause])
genMergingInfoCon dataTypeVars tyName isLast con = do
  let conName = occName $ constructorName con
  let newConName = mkName $ conName <> "MergingInfo"
  if null (constructorFields con) && null dataTypeVars
    then do
      eqClause <-
        clause
          [conP newConName [], conP newConName []]
          (normalB $ conE 'True)
          []
      cmpClause0 <-
        clause
          [conP newConName [], conP newConName []]
          (normalB $ conE 'EQ)
          []
      cmpClause1 <-
        clause
          [conP newConName [], wildP]
          (normalB $ conE 'LT)
          []
      cmpClause2 <-
        clause
          [wildP, conP newConName []]
          (normalB $ conE 'GT)
          []
      let cmpClauses =
            if isLast
              then [cmpClause0]
              else [cmpClause0, cmpClause1, cmpClause2]
      let nameLit = litE $ stringL conName
      let showExp = [|$nameLit <> " " <> show (Proxy @($(conT tyName)))|]
      showClause <-
        clause
          [conP newConName []]
          (normalB showExp)
          []
      return
        ( GadtC [newConName] [] (ConT tyName),
          newConName,
          S.fromList [],
          [eqClause],
          cmpClauses,
          [showClause]
        )
    else do
      let oriVars = dataTypeVars ++ constructorVars con
      newNames <- traverse (newName . occName . tvName) oriVars
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
          [wildP, conP newConName patsR]
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
      return
        ( ForallC
            ((`plainTVFlag` specifiedSpec) <$> newNames)
            (applySubstitution substMap $ constructorContext con)
            $ GadtC
              [newConName]
              ( (Bang NoSourceUnpackedness NoSourceStrictness,)
                  <$> tyFields ++ strategyFields
              )
              (ConT tyName),
          newConName,
          S.fromList [0 .. length tyFields - 1],
          -- S.fromList $ fst <$> dedupedFields,
          [eqClause],
          cmpClauses,
          [showClause]
        )

genMergingInfo :: Name -> Q (Name, [Dec], [Name], [S.Set Int])
genMergingInfo typName = do
  d <- reifyDatatype typName
  let originalName = occName $ datatypeName d
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
  let cons = fmap (\(a, _, _, _, _, _) -> a) r
  let eqClauses =
        concatMap (\(_, _, _, a, _, _) -> a) r
          ++ [ Clause [WildP, WildP] (NormalB $ ConE 'False) []
               | length constructors > 1
             ]
  let cmpClauses = concatMap (\(_, _, _, _, a, _) -> a) r
  let showClauses = concatMap (\(_, _, _, _, _, a) -> a) r
  return
    ( name,
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
          ],
      fmap (\(_, a, _, _, _, _) -> a) r,
      fmap (\(_, _, a, _, _, _) -> a) r
    )

genMergeable :: Name -> Int -> Q [Dec]
genMergeable typName n = do
  (infoName, info, conInfoNames, pos) <- genMergingInfo typName
  (_, decs) <- genMergeable' infoName conInfoNames pos typName n
  return $ info ++ decs

genMergeFunClause' :: Name -> ConstructorInfo -> Q Clause
genMergeFunClause' conInfoName con = do
  let numExistential = length $ constructorVars con
  let numFields = length $ constructorFields con
  let argWildCards = replicate numExistential wildP
  case numFields of
    0 -> do
      let pat = conP conInfoName []
      clause
        (argWildCards ++ [pat])
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

genMergingInfoFunClause' ::
  [Name] -> Name -> S.Set Int -> ConstructorInfo -> Q Clause
genMergingInfoFunClause' argTypes conInfoName pos oldCon = do
  let conName = constructorName oldCon
  let oldConVars = constructorVars oldCon
  newNames <- traverse (newName . occName . tvName) oldConVars
  let substMap = M.fromList $ zip (tvName <$> oldConVars) (VarT <$> newNames)
  let con = applySubstitution substMap oldCon
  let conVars = constructorVars con
  let fields = constructorFields con
  let capture n =
        if S.member n pos
          then do
            return (SigP WildP $ fields !! n)
          else return (WildP)
  capturedVarTyReps <-
    traverse (\bndr -> [|typeRep @($(varT $ tvName bndr))|]) conVars
  varPat <- conP conName $ capture <$> [0 .. length (constructorFields con) - 1]
  let infoExpWithTypeReps = foldl AppE (ConE conInfoName) capturedVarTyReps

  let fields = constructorFields con
  let usedArgs = S.fromList $ freeVariables fields

  strategyNames <-
    traverse
      ( \nm ->
          if S.member nm usedArgs
            then do
              pname <- newName "p"
              return (nm, Just pname)
            else return (nm, Nothing)
      )
      argTypes
  let argToStrategyPat =
        mapMaybe (\(nm, mpat) -> fmap (nm,) mpat) strategyNames
  let strategyPats = fmap (maybe WildP VarP . snd) strategyNames

  let argTypeSet = S.fromList argTypes
  let containsArg :: Type -> Bool
      containsArg ty =
        S.intersection argTypeSet (S.fromList (freeVariables [ty])) /= S.empty

  let fieldStrategyExp ty =
        if not (containsArg ty)
          then [|rootStrategy :: MergingStrategy $(return ty)|]
          else case ty of
            AppT (AppT (AppT (ConT _) a) b) c -> do
              [|
                liftRootStrategy3
                  $(fieldStrategyExp a)
                  $(fieldStrategyExp b)
                  $(fieldStrategyExp c) ::
                  MergingStrategy $(return ty)
                |]
            AppT (AppT (ConT _) a) b -> do
              [|
                liftRootStrategy2 $(fieldStrategyExp a) $(fieldStrategyExp b) ::
                  MergingStrategy $(return ty)
                |]
            AppT (ConT _) a -> do
              [|
                liftRootStrategy $(fieldStrategyExp a) ::
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

genMergeable' :: Name -> [Name] -> [S.Set Int] -> Name -> Int -> Q (Name, [Dec])
genMergeable' infoName conInfoNames pos typName n = do
  (constructors, keptNewNames, keptNewVars, argNewNames, argNewVars) <-
    checkArgs "Mergeable" 3 typName n

  d <- reifyDatatype typName
  let mergeableContexts =
        fmap (AppT (ConT ''Mergeable) . VarT . tvName) keptNewVars

  let targetType =
        foldl
          (\ty nm -> AppT ty (VarT nm))
          (ConT typName)
          (keptNewNames ++ argNewNames)
  let infoType = ConT infoName
  let mergingInfoFunFinalType = AppT (AppT ArrowT targetType) infoType

  let mergingInfoFunTypeWithoutCtx =
        foldr
          ((AppT . AppT ArrowT) . AppT (ConT ''MergingStrategy) . VarT)
          mergingInfoFunFinalType
          argNewNames

  let mergingInfoFunType =
        ForallT
          (mapTVFlag (const specifiedSpec) <$> keptNewVars ++ argNewVars)
          mergeableContexts
          mergingInfoFunTypeWithoutCtx
  let mergingInfoFunName =
        mkName $
          "mergingInfo"
            <> (if n /= 0 then show n else "")
            <> occName (datatypeName d)
  let mergingInfoFunSigD = SigD mergingInfoFunName mergingInfoFunType
  clauses <-
    traverse
      ( \(conInfoName, pos, con) ->
          genMergingInfoFunClause' (tvName <$> argNewVars) conInfoName pos con
      )
      $ zip3 conInfoNames pos constructors
  let mergingInfoFunDec = FunD mergingInfoFunName clauses

  let mergeFunType =
        AppT (AppT ArrowT infoType) (AppT (ConT ''MergingStrategy) targetType)
  let mergeFunName =
        mkName $
          "merge"
            <> (if n /= 0 then show n else "")
            <> occName (datatypeName d)
  let mergeFunSigD = SigD mergeFunName mergeFunType
  mergeFunClauses <- zipWithM genMergeFunClause' conInfoNames constructors
  let mergeFunDec = FunD mergeFunName mergeFunClauses

  let instanceHead = case n of
        0 -> ConT ''Mergeable
        1 -> ConT ''Mergeable1
        2 -> ConT ''Mergeable2
        3 -> ConT ''Mergeable3
        _ -> error "Unsupported n"

  let instanceType =
        AppT
          instanceHead
          (foldl AppT (ConT typName) $ fmap VarT keptNewNames)

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
          mergeableContexts
          instanceType
          [FunD mergeInstanceFunName [mergeInstanceFunClause]]
      ]
    )

-- | Derive 'Mergeable' instance for GADT.
deriveGADTMergeable :: Name -> Q [Dec]
deriveGADTMergeable nm = genMergeable nm 0

-- | Derive 'Mergeable1' instance for GADT.
deriveGADTMergeable1 :: Name -> Q [Dec]
deriveGADTMergeable1 nm = genMergeable nm 1

-- | Derive 'Mergeable2' instance for GADT.
deriveGADTMergeable2 :: Name -> Q [Dec]
deriveGADTMergeable2 nm = genMergeable nm 2

-- | Derive 'Mergeable3' instance for GADT.
deriveGADTMergeable3 :: Name -> Q [Dec]
deriveGADTMergeable3 nm = genMergeable nm 3
