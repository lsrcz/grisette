{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      :   Grisette.Internal.TH.Derivation.DerivePPrint
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.Derivation.DerivePPrint
  ( derivePPrint,
    derivePPrint1,
    derivePPrint2,
  )
where

import Data.Maybe (fromMaybe)
import Data.String (IsString (fromString))
import GHC.Show (appPrec1)
import Grisette.Internal.Internal.Decl.Core.Data.Class.PPrint
  ( PPrint (pformatList, pformatPrec),
    PPrint1 (liftPFormatList, liftPFormatPrec),
    PPrint2 (liftPFormatList2, liftPFormatPrec2),
    align,
    condEnclose,
    flatAlt,
    group,
    groupedEnclose,
    nest,
    pformatWithConstructorNoAlign,
    vcat,
    vsep,
    (<+>),
  )
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
    Exp (ListE),
    Fixity (Fixity),
    Name,
    defaultFixity,
    listE,
    nameBase,
    stringE,
  )
import Language.Haskell.TH.Datatype
  ( ConstructorVariant (InfixConstructor, NormalConstructor, RecordConstructor),
    reifyFixityCompat,
  )
import Language.Haskell.TH.Syntax (Q)

pprintConfig :: UnaryOpClassConfig
pprintConfig =
  UnaryOpClassConfig
    { unaryOpConfigs =
        [ UnaryOpConfig
            UnaryOpFieldConfig
              { extraPatNames = ["prec"],
                extraLiftedPatNames = \i -> (["pl" | i /= 0]),
                fieldCombineFun = \_ _ variant conName [prec] exps -> do
                  let initExps =
                        ( \e ->
                            [|
                              $(return e)
                                <> (fromString ",")
                                <> flatAlt (fromString "") (fromString " ")
                              |]
                        )
                          <$> init exps
                      lastExp = [|$(return $ last exps)|]
                      commaSeped = initExps ++ [lastExp]
                  case (variant, exps) of
                    (NormalConstructor, []) -> do
                      r <- [|fromString $(stringE $ nameBase conName)|]
                      return (r, [False])
                    (NormalConstructor, [exp]) -> do
                      r <-
                        [|
                          pformatWithConstructorNoAlign
                            $(return prec)
                            (fromString $(stringE $ nameBase conName))
                            [$(return exp)]
                          |]
                      return (r, [True])
                    (NormalConstructor, _) | isNonUnitTuple conName -> do
                      r <-
                        [|
                          groupedEnclose (fromString "(") (fromString ")") $
                            vcat $
                              $(listE commaSeped)
                          |]
                      return (r, [False])
                    (NormalConstructor, _) -> do
                      r <-
                        [|
                          pformatWithConstructorNoAlign
                            $(return prec)
                            (fromString $(stringE $ nameBase conName))
                            [vsep $(return $ ListE exps)]
                          |]
                      return (r, [True])
                    (RecordConstructor _, _) -> do
                      r <-
                        [|
                          pformatWithConstructorNoAlign
                            $(return prec)
                            (fromString $(stringE $ nameBase conName))
                            [ groupedEnclose (fromString "{") (fromString "}") $
                                vcat $
                                  $(listE commaSeped)
                            ]
                          |]
                      return (r, [True])
                    (InfixConstructor, [l, r]) -> do
                      fi <-
                        fromMaybe defaultFixity `fmap` reifyFixityCompat conName
                      let conPrec = case fi of Fixity prec _ -> prec
                      r <-
                        [|
                          group
                            $ condEnclose
                              ($(return prec) > $(integerE conPrec))
                              (fromString "(")
                              (fromString ")")
                            $ nest 2
                            $ vsep
                              [ align $ $(return l),
                                fromString $(stringE $ nameBase conName)
                                  <+> $(return r)
                              ]
                          |]
                      return (r, [True])
                    _ ->
                      fail "derivePPrint: unexpected constructor variant",
                fieldResFun = \variant conName _ pos fieldPat fieldFun -> do
                  let makePPrintField p =
                        [|
                          $(return fieldFun)
                            $(integerE p)
                            $(return fieldPat)
                          |]
                  let attachUsedInfo = ((,[False]) <$>)
                  case variant of
                    NormalConstructor
                      | isNonUnitTuple conName ->
                          attachUsedInfo $ makePPrintField 0
                    NormalConstructor ->
                      attachUsedInfo $ makePPrintField appPrec1
                    RecordConstructor names ->
                      attachUsedInfo
                        [|
                          fromString $(stringE $ nameBase (names !! pos) ++ " = ")
                            <> $(makePPrintField 0)
                          |]
                    InfixConstructor -> do
                      fi <-
                        fromMaybe defaultFixity `fmap` reifyFixityCompat conName
                      let conPrec = case fi of Fixity prec _ -> prec
                      attachUsedInfo $ makePPrintField (conPrec + 1),
                fieldFunExp =
                  showPrintFieldFunExp
                    ['pformatPrec, 'liftPFormatPrec, 'liftPFormatPrec2]
                    ['pformatList, 'liftPFormatList, 'liftPFormatList2]
              }
            ['pformatPrec, 'liftPFormatPrec, 'liftPFormatPrec2]
        ],
      unaryOpExtraVars = const $ return [],
      unaryOpInstanceNames = [''PPrint, ''PPrint1, ''PPrint2],
      unaryOpInstanceTypeFromConfig = defaultUnaryOpInstanceTypeFromConfig,
      unaryOpAllowExistential = True,
      unaryOpContextNames = Nothing
    }

-- | Derive 'PPrint' instance for a data type.
derivePPrint :: DeriveConfig -> Name -> Q [Dec]
derivePPrint deriveConfig = genUnaryOpClass deriveConfig pprintConfig 0

-- | Derive 'PPrint1' instance for a data type.
derivePPrint1 :: DeriveConfig -> Name -> Q [Dec]
derivePPrint1 deriveConfig = genUnaryOpClass deriveConfig pprintConfig 1

-- | Derive 'PPrint2' instance for a data type.
derivePPrint2 :: DeriveConfig -> Name -> Q [Dec]
derivePPrint2 deriveConfig = genUnaryOpClass deriveConfig pprintConfig 2
