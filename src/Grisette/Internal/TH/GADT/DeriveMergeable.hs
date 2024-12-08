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

import Control.Monad (foldM, replicateM, when, zipWithM)
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
import Grisette.Internal.TH.Util (occName)
import Language.Haskell.TH
  ( Bang (Bang),
    Body (NormalB),
    Clause (Clause),
    Con (ForallC, GadtC),
    Dec (DataD, FunD, InstanceD, SigD),
    Exp (AppE, ConE, LamE, LitE, VarE),
    Lit (StringL),
    Name,
    Pat (ConP, SigP, VarP, WildP),
    Q,
    Quote (newName),
    SourceStrictness (NoSourceStrictness),
    SourceUnpackedness (NoSourceUnpackedness),
    Type (AppT, ArrowT, ConT, ForallT, VarT),
    appE,
    conE,
    conT,
    lamE,
    lookupTypeName,
    mkName,
    normalB,
    tupP,
    varE,
    varP,
    varT,
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
    plainTVFlag,
    specifiedSpec,
  )
import Language.Haskell.TH.Lib (conP)
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
      let eqClause =
            Clause
              [ConP newConName [] [], ConP newConName [] []]
              (NormalB $ ConE 'True)
              []
      let cmpClause0 =
            Clause
              [ConP newConName [] [], ConP newConName [] []]
              (NormalB $ ConE 'EQ)
              []
      let cmpClause1 =
            Clause
              [ConP newConName [] [], WildP]
              (NormalB $ ConE 'LT)
              []
      let cmpClause2 =
            Clause
              [WildP, ConP newConName [] []]
              (NormalB $ ConE 'GT)
              []
      let cmpClauses =
            if isLast
              then [cmpClause0]
              else [cmpClause0, cmpClause1, cmpClause2]
      let nameLit = LitE $ StringL conName
      showExp <- [|$(return nameLit) <> " " <> show (Proxy @($(conT tyName)))|]
      let showClause =
            Clause
              [ConP newConName [] []]
              (NormalB showExp)
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
      let tyFieldPatsL = fmap VarP tyFieldNamesL
      let tyFieldPatsR = fmap VarP tyFieldNamesR
      let tyFieldVarsL = fmap VarE tyFieldNamesL
      let tyFieldVarsR = fmap VarE tyFieldNamesR
      let strategyFieldPats = replicate (length strategyFields) WildP
      let patsL = tyFieldPatsL ++ strategyFieldPats
      let patsR = tyFieldPatsR ++ strategyFieldPats
      let allWildcards = fmap (const WildP) $ tyFieldPatsL ++ strategyFieldPats
      let eqCont l r cont =
            [|
              SomeTypeRep $(return l) == SomeTypeRep $(return r)
                && $(return cont)
              |]
      eqExp <-
        foldM (\cont (l, r) -> eqCont l r cont) (ConE 'True) $
          zip tyFieldVarsL tyFieldVarsR
      let eqClause =
            Clause
              [ConP newConName [] patsL, ConP newConName [] patsR]
              (NormalB eqExp)
              []
      let cmpCont l r cont =
            [|
              case SomeTypeRep $(return l) `compare` SomeTypeRep $(return r) of
                EQ -> $(return cont)
                x -> x
              |]
      cmpExp <-
        foldM (\cont (l, r) -> cmpCont l r cont) (ConE 'EQ) $
          zip tyFieldVarsL tyFieldVarsR
      let cmpClause0 =
            Clause
              [ConP newConName [] patsL, ConP newConName [] patsR]
              (NormalB cmpExp)
              []
      let cmpClause1 =
            Clause
              [ConP newConName [] allWildcards, WildP]
              (NormalB $ ConE 'LT)
              []
      let cmpClause2 =
            Clause
              [WildP, ConP newConName [] allWildcards]
              (NormalB $ ConE 'GT)
              []
      let cmpClauses =
            if isLast
              then [cmpClause0]
              else [cmpClause0, cmpClause1, cmpClause2]
      let showCont t cont =
            [|$(return cont) <> " " <> show $(return t)|]
      showExp <- foldM (flip showCont) (LitE $ StringL conName) tyFieldVarsL
      let showClause =
            Clause
              [ConP newConName [] patsL]
              (NormalB showExp)
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
  let argWildCards = replicate numExistential WildP
  case numFields of
    0 -> do
      let pat = ConP conInfoName [] []
      body <- normalB [|SimpleStrategy $ \_ t _ -> t|]
      return $ Clause (argWildCards ++ [pat]) body []
    1 -> do
      pname <- newName "s"
      let pat = ConP conInfoName [] $ argWildCards ++ [VarP pname]
      upname <- newName "a"
      let unwrapPat = ConP (constructorName con) [] [VarP upname]
      let unwrapFun = LamE [unwrapPat] $ AppE (VarE 'unsafeCoerce) (VarE upname)
      body <-
        normalB
          [|
            wrapStrategy
              $(varE pname)
              (unsafeCoerce . $(conE $ constructorName con))
              $(return unwrapFun)
            |]
      return $ Clause [pat] body []
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
      body <-
        [|
          product2Strategy
            $wrapFun
            $unwrapFun
            $(varE $ head pnames)
            $(strategy1 $ tail pnames)
          |]
      return $
        Clause
          ([ConP conInfoName [] $ argWildCards ++ fmap VarP pnames])
          (NormalB body)
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
  capturedPats <- traverse capture [0 .. length (constructorFields con) - 1]
  capturedVarTyReps <-
    traverse (\bndr -> [|typeRep @($(varT $ tvName bndr))|]) conVars
  let varPat = ConP conName [] capturedPats
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
  when (n < 0) $
    fail $
      unlines
        [ "Cannot derive Mergeable instance with negative type parameters",
          "Requested: " ++ show n,
          "Hint: Use a non-negative number of type parameters"
        ]
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
  keptNewNames <- traverse (newName . occName . tvName) keptVars
  let keptNewVars = (`plainTVFlag` specifiedSpec) <$> keptNewNames
  let argVars = drop (length dvars - n) dvars
  argNewNames <- traverse (newName . occName . tvName) argVars
  let argNewVars = (`plainTVFlag` specifiedSpec) <$> argNewNames
  let substMap =
        M.fromList $
          zip
            (tvName <$> dvars)
            (VarT <$> keptNewNames ++ argNewNames)

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
          (keptNewVars ++ argNewVars)
          mergeableContexts
          mergingInfoFunTypeWithoutCtx
  let mergingInfoFunName =
        mkName $
          "mergingInfo"
            <> (if n /= 0 then show n else "")
            <> occName (datatypeName d)
  let mergingInfoFunSigD = SigD mergingInfoFunName mergingInfoFunType
  let constructors = datatypeCons d
  clauses <-
    traverse
      ( \(conInfoName, pos, con) ->
          genMergingInfoFunClause' (tvName <$> argNewVars) conInfoName pos con
      )
      $ zip3 conInfoNames pos
      $ applySubstitution substMap constructors
  let mergingInfoFunDec = FunD mergingInfoFunName clauses

  let mergeFunType =
        AppT (AppT ArrowT infoType) (AppT (ConT ''MergingStrategy) targetType)
  let mergeFunName =
        mkName $
          "merge"
            <> (if n /= 0 then show n else "")
            <> occName (datatypeName d)
  let mergeFunSigD = SigD mergeFunName mergeFunType
  mergeFunClauses <-
    zipWithM genMergeFunClause' conInfoNames $
      applySubstitution substMap constructors
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
          (instanceType)
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
