{-# LANGUAGE TemplateHaskell #-}

module Grisette.Internal.TH.GADT.ShowPPrintCommon (showPrintFieldFunExp) where

import qualified Data.Map as M
import qualified Data.Set as S
import Grisette.Internal.TH.GADT.UnaryOpCommon (FieldFunExp)
import Language.Haskell.TH (Name, Type (AppT, VarT), varE)
import Language.Haskell.TH.Datatype (TypeSubstitution (freeVariables))

showPrintFieldFunExp :: [Name] -> [Name] -> FieldFunExp
showPrintFieldFunExp precNames listNames argToFunPat liftedExps = go
  where
    allArgNames = M.keysSet argToFunPat
    typeHasNoArg ty =
      S.fromList (freeVariables [ty])
        `S.intersection` allArgNames
        == S.empty
    goLst ty = do
      let fun0 = varE (head listNames)
          fun1 b = [|$(varE $ listNames !! 1) $(go b) $(goLst b)|]
          fun2 b c =
            [|$(varE $ listNames !! 2) $(go b) $(goLst b) $(go c) $(goLst c)|]
      case ty of
        AppT (AppT (VarT _) b) c -> fun2 b c
        AppT (VarT _) b -> fun1 b
        _ | typeHasNoArg ty -> fun0
        AppT a b | typeHasNoArg a -> fun1 b
        AppT (AppT a b) c | typeHasNoArg a -> fun2 b c
        VarT nm -> case M.lookup nm liftedExps of
          Just [p] -> varE p
          _ -> fail $ "defaultFieldFunExp: unsupported type: " <> show ty
        _ -> fail $ "defaultFieldFunExp: unsupported type: " <> show ty
    go ty = do
      let fun0 = varE (head precNames)
          fun1 b = [|$(varE $ precNames !! 1) $(go b) $(goLst b)|]
          fun2 b c =
            [|$(varE $ precNames !! 2) $(go b) $(goLst b) $(go c) $(goLst c)|]
      case ty of
        AppT (AppT (VarT _) b) c -> fun2 b c
        AppT (VarT _) b -> fun1 b
        _ | typeHasNoArg ty -> fun0
        AppT a b | typeHasNoArg a -> fun1 b
        AppT (AppT a b) c | typeHasNoArg a -> fun2 b c
        VarT nm -> case M.lookup nm argToFunPat of
          Just pname -> varE pname
          _ -> fail $ "defaultFieldFunExp: unsupported type: " <> show ty
        _ -> fail $ "defaultFieldFunExp: unsupported type: " <> show ty
