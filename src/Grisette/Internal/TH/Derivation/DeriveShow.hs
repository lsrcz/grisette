{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      :   Grisette.Internal.TH.Derivation.DeriveShow
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.Derivation.DeriveShow
  ( deriveShow,
    deriveShow1,
    deriveShow2,
  )
where

import Data.Functor.Classes
  ( Show1 (liftShowList, liftShowsPrec),
    Show2 (liftShowList2, liftShowsPrec2),
  )
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import GHC.Show (appPrec, appPrec1)
import Grisette.Internal.TH.Derivation.Common (DeriveConfig)
import Grisette.Internal.TH.Derivation.ShowPPrintCommon (showPrintFieldFunExp)
import Grisette.Internal.TH.Derivation.UnaryOpCommon
  ( UnaryOpClassConfig
      ( UnaryOpClassConfig,
        unaryOpAllowExistential,
        unaryOpConfigs,
        unaryOpContextNames,
        unaryOpExtraVars,
        unaryOpInstanceNames,
        unaryOpInstanceTypeFromConfig
      ),
    UnaryOpConfig (UnaryOpConfig),
    UnaryOpFieldConfig
      ( UnaryOpFieldConfig,
        extraLiftedPatNames,
        extraPatNames,
        fieldCombineFun,
        fieldFunExp,
        fieldResFun
      ),
    defaultUnaryOpInstanceTypeFromConfig,
    genUnaryOpClass,
  )
import Grisette.Internal.TH.Util (integerE, isNonUnitTuple)
import Language.Haskell.TH
  ( Dec,
    Fixity (Fixity),
    Name,
    Q,
    defaultFixity,
    integerL,
    listE,
    litE,
    nameBase,
    stringE,
  )
import Language.Haskell.TH.Datatype
  ( ConstructorVariant (InfixConstructor, NormalConstructor, RecordConstructor),
    reifyFixityCompat,
  )

showConfig :: UnaryOpClassConfig
showConfig =
  UnaryOpClassConfig
    { unaryOpConfigs =
        [ UnaryOpConfig
            UnaryOpFieldConfig
              { extraPatNames = ["prec"],
                extraLiftedPatNames = \i -> (["sl" | i /= 0]),
                fieldCombineFun =
                  \_ _ variant conName [prec] exps -> do
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
                        fail "deriveShow: unexpected constructor variant",
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
                      fi <-
                        fromMaybe defaultFixity `fmap` reifyFixityCompat conName
                      let conPrec = case fi of Fixity prec _ -> prec
                      attachUsedInfo $ makeShowField (conPrec + 1),
                fieldFunExp =
                  showPrintFieldFunExp
                    ['showsPrec, 'liftShowsPrec, 'liftShowsPrec2]
                    ['showList, 'liftShowList, 'liftShowList2]
              }
            ['showsPrec, 'liftShowsPrec, 'liftShowsPrec2]
        ],
      unaryOpInstanceNames = [''Show, ''Show1, ''Show2],
      unaryOpExtraVars = const $ return [],
      unaryOpInstanceTypeFromConfig = defaultUnaryOpInstanceTypeFromConfig,
      unaryOpAllowExistential = True,
      unaryOpContextNames = Nothing
    }

-- | Derive 'Show' instance for a data type.
deriveShow :: DeriveConfig -> Name -> Q [Dec]
deriveShow deriveConfig = genUnaryOpClass deriveConfig showConfig 0

-- | Derive 'Show1' instance for a data type.
deriveShow1 :: DeriveConfig -> Name -> Q [Dec]
deriveShow1 deriveConfig = genUnaryOpClass deriveConfig showConfig 1

-- | Derive 'Show2' instance for a data type.
deriveShow2 :: DeriveConfig -> Name -> Q [Dec]
deriveShow2 deriveConfig = genUnaryOpClass deriveConfig showConfig 2
