{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      :   Grisette.Internal.TH.GADT.DeriveShow
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.GADT.DeriveShow
  ( deriveGADTShow,
    deriveGADTShow1,
    deriveGADTShow2,
  )
where

import Data.Functor.Classes (Show1 (liftShowsPrec), Show2 (liftShowsPrec2))
import qualified Data.List as List
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import GHC.Show (appPrec, appPrec1)
import Grisette.Internal.TH.GADT.UnaryOpCommon
  ( FieldFunExp,
    UnaryOpClassConfig
      ( UnaryOpClassConfig,
        unaryOpFieldConfig,
        unaryOpFunNames,
        unaryOpInstanceNames
      ),
    UnaryOpFieldConfig
      ( UnaryOpFieldConfig,
        extraLiftedPatNames,
        extraPatNames,
        fieldCombineFun,
        fieldFunExp,
        fieldResFun
      ),
    defaultFieldFunExp,
    genUnaryOpClass,
  )
import Grisette.Internal.TH.Util (integerE, isNonUnitTuple)
import Language.Haskell.TH
  ( Dec,
    Fixity (Fixity),
    Name,
    Q,
    Type (AppT, VarT),
    defaultFixity,
    integerL,
    listE,
    litE,
    nameBase,
    stringE,
    varE,
  )
import Language.Haskell.TH.Datatype
  ( ConstructorVariant (InfixConstructor, NormalConstructor, RecordConstructor),
    TypeSubstitution (freeVariables),
    reifyFixityCompat,
  )

showFieldFunExp :: FieldFunExp
showFieldFunExp argToFunPat liftedExps = go
  where
    allArgNames = M.keysSet argToFunPat
    typeHasNoArg ty =
      S.fromList (freeVariables [ty])
        `S.intersection` allArgNames
        == S.empty
    goLst ty = do
      let fun0 = [|showList|]
          fun1 b = [|liftShowList $(go b) $(goLst b)|]
          fun2 b c = [|liftShowList $(go b) $(goLst b) $(go c) $(goLst c)|]
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
      let fun0 = [|showsPrec|]
          fun1 b = [|liftShowsPrec $(go b) $(goLst b)|]
          fun2 b c = [|liftShowsPrec2 $(go b) $(goLst b) $(go c) $(goLst c)|]
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

showConfig :: UnaryOpClassConfig
showConfig =
  UnaryOpClassConfig
    { unaryOpFieldConfig =
        UnaryOpFieldConfig
          { extraPatNames = ["prec"],
            extraLiftedPatNames = \i -> (["sl" | i /= 0]),
            fieldCombineFun =
              \variant conName [prec] exps -> do
                case (variant, exps) of
                  (NormalConstructor, []) -> do
                    r <- [|showString $(stringE $ nameBase conName)|]
                    return (r, [False])
                  (NormalConstructor, [exp]) -> do
                    r <-
                      [|
                        showParen
                          ($(return prec) > $(integerE appPrec))
                          ( showString $(stringE $ nameBase conName)
                              . showChar ' '
                              . $(return exp)
                          )
                        |]
                    return (r, [True])
                  (NormalConstructor, _) | isNonUnitTuple conName -> do
                    let commaSeped =
                          List.intersperse [|showChar ','|] $
                            return <$> exps
                    r <-
                      [|
                        showChar '('
                          . foldr1 (.) $(listE commaSeped)
                          . showChar ')'
                        |]
                    return (r, [False])
                  (NormalConstructor, _) -> do
                    let spaceSeped =
                          List.intersperse [|showChar ' '|] $
                            return <$> exps
                    r <-
                      [|
                        showParen
                          ($(return prec) > $(integerE appPrec))
                          ( showString $(stringE $ nameBase conName)
                              . showChar ' '
                              . (foldr1 (.) $(listE spaceSeped))
                          )
                        |]
                    return (r, [True])
                  (RecordConstructor _, _) -> do
                    let commaSpaceSeped =
                          List.intersperse [|showString ", "|] $
                            return <$> exps
                    r <-
                      [|
                        showString $(stringE $ nameBase conName)
                          . showString " {"
                          . foldr1 (.) $(listE commaSpaceSeped)
                          . showString "}"
                        |]
                    return (r, [False])
                  (InfixConstructor, [l, r]) -> do
                    fi <-
                      fromMaybe defaultFixity `fmap` reifyFixityCompat conName
                    let conPrec = case fi of Fixity prec _ -> prec
                    r <-
                      [|
                        showParen
                          ($(return prec) > $(integerE conPrec))
                          ( $(return l)
                              . showChar ' '
                              . showString $(stringE $ nameBase conName)
                              . showChar ' '
                              . $(return r)
                          )
                        |]
                    return (r, [True])
                  _ ->
                    fail "deriveGADTShow: unexpected constructor variant",
            fieldResFun = \variant conName _ pos fieldPat fieldFun -> do
              let makeShowField p =
                    [|
                      $(return fieldFun)
                        $(litE $ integerL $ fromIntegral p)
                        $(return fieldPat)
                      |]
              let attachUsedInfo = ((,[False]) <$>)
              case variant of
                NormalConstructor
                  | isNonUnitTuple conName ->
                      attachUsedInfo $ makeShowField 0
                NormalConstructor ->
                  attachUsedInfo $ makeShowField appPrec1
                RecordConstructor names ->
                  attachUsedInfo
                    [|
                      showString $(stringE $ nameBase (names !! pos) ++ " = ")
                        . $(makeShowField 0)
                      |]
                InfixConstructor -> do
                  fi <- fromMaybe defaultFixity `fmap` reifyFixityCompat conName
                  let conPrec = case fi of Fixity prec _ -> prec
                  attachUsedInfo $ makeShowField (conPrec + 1),
            fieldFunExp =
              \argToFunPat liftedExps ty ->
                if M.null argToFunPat
                  then defaultFieldFunExp ['showsPrec] argToFunPat liftedExps ty
                  else showFieldFunExp argToFunPat liftedExps ty
          },
      unaryOpInstanceNames = [''Show, ''Show1, ''Show2],
      unaryOpFunNames = ['showsPrec, 'liftShowsPrec, 'liftShowsPrec2]
    }

-- | Derive 'Show' instance for a GADT.
deriveGADTShow :: Name -> Q [Dec]
deriveGADTShow = genUnaryOpClass showConfig 0

-- | Derive 'Show1' instance for a GADT.
deriveGADTShow1 :: Name -> Q [Dec]
deriveGADTShow1 = genUnaryOpClass showConfig 1

-- | Derive 'Show2' instance for a GADT.
deriveGADTShow2 :: Name -> Q [Dec]
deriveGADTShow2 = genUnaryOpClass showConfig 2
