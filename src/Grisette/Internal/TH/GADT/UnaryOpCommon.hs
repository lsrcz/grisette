{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Grisette.Internal.TH.GADT.UnaryOpCommon
  ( UnaryOpClassConfig (..),
    UnaryOpFieldConfig (..),
    genUnaryOpClause,
    genUnaryOpClass,
    fieldExp,
  )
where

import Control.Monad (replicateM, zipWithM)
import qualified Data.Map as M
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as S
import Grisette.Internal.TH.GADT.Common (checkArgs)
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

fieldExp :: [Name] -> M.Map Name Name -> Type -> Q Exp
fieldExp unaryOpFunNames argToFunPat ty = do
  let notContains =
        M.null $
          M.restrictKeys argToFunPat (S.fromList $ freeVariables [ty])
  if notContains
    then varE $ head unaryOpFunNames
    else case ty of
      AppT (AppT (AppT (ConT _) a) b) c -> do
        [|
          $(varE $ unaryOpFunNames !! 3)
            $(fieldExp unaryOpFunNames argToFunPat a)
            $(fieldExp unaryOpFunNames argToFunPat b)
            $(fieldExp unaryOpFunNames argToFunPat c)
          |]
      AppT (AppT (ConT _) a) b -> do
        [|
          $(varE $ unaryOpFunNames !! 2)
            $(fieldExp unaryOpFunNames argToFunPat a)
            $(fieldExp unaryOpFunNames argToFunPat b)
          |]
      AppT (ConT _) a -> do
        [|
          $(varE $ unaryOpFunNames !! 1)
            $(fieldExp unaryOpFunNames argToFunPat a)
          |]
      VarT nm -> do
        case M.lookup nm argToFunPat of
          Just pname -> varE pname
          _ -> fail $ "fieldExp: unsupported type: " <> show ty
      _ -> fail $ "fieldExp: unsupported type: " <> show ty

patAndExps ::
  (M.Map Name Name -> Type -> Q Exp) -> [Name] -> [Type] -> Q ([Pat], [Exp])
patAndExps fieldFunExpGen argTypes fields = do
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
  fieldEvalSymFunExps <- traverse (fieldFunExpGen argToFunPat) fields
  return (funPats, fieldEvalSymFunExps)

data UnaryOpFieldConfig = UnaryOpFieldConfig
  { extraPatNames :: [String],
    fieldCombineFun :: Exp -> [Exp] -> Q Exp
  }

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
      patAndExps (fieldExp unaryOpFunNames) argTypes fields
    extraPatNames <- traverse newName extraPatNames
    fieldsPatNames <- replicateM (length fields) $ newName "field"
    let extraPats = fmap VarP extraPatNames
    fieldPats <- conP (constructorName conInfo) (fmap varP fieldsPatNames)

    fieldExps <-
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

    resExp <- fieldCombineFun (ConE (constructorName conInfo)) fieldExps
    return $ Clause (funPats ++ extraPats ++ [fieldPats]) (NormalB resExp) []

data UnaryOpClassConfig = UnaryOpClassConfig
  { unaryOpFieldConfig :: UnaryOpFieldConfig,
    unaryOpInstanceNames :: [Name],
    unaryOpFunNames :: [Name]
  }

genUnaryOpClass ::
  UnaryOpClassConfig ->
  Int ->
  Name ->
  Q [Dec]
genUnaryOpClass (UnaryOpClassConfig {..}) n typName = do
  (constructors, keptNewNames, keptNewVars, argNewNames, _) <-
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
  ctxs <- traverse ctxForVar keptNewVars
  clauses <-
    traverse
      (genUnaryOpClause unaryOpFunNames unaryOpFieldConfig argNewNames)
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
