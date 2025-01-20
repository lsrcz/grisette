{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :   Grisette.Internal.TH.Derivation.DeriveNFData
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.Derivation.DeriveNFData
  ( deriveNFData,
    deriveNFData1,
    deriveNFData2,
  )
where

import Control.DeepSeq (NFData (rnf), NFData1 (liftRnf), NFData2 (liftRnf2))
import Grisette.Internal.TH.Derivation.Common (DeriveConfig)
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
    defaultFieldFunExp,
    defaultFieldResFun,
    defaultUnaryOpInstanceTypeFromConfig,
    genUnaryOpClass,
  )
import Language.Haskell.TH (Dec, Name)
import Language.Haskell.TH.Syntax (Q)

nfdataConfig :: UnaryOpClassConfig
nfdataConfig =
  UnaryOpClassConfig
    { unaryOpConfigs =
        [ UnaryOpConfig
            UnaryOpFieldConfig
              { extraPatNames = [],
                extraLiftedPatNames = const [],
                fieldCombineFun = \_ _ _ _ _ exps -> do
                  r <-
                    foldl
                      (\acc exp -> [|$acc `seq` $(return exp)|])
                      ([|()|])
                      exps
                  return (r, []),
                fieldResFun = defaultFieldResFun,
                fieldFunExp = defaultFieldFunExp ['rnf, 'liftRnf, 'liftRnf2]
              }
            ['rnf, 'liftRnf, 'liftRnf2]
        ],
      unaryOpInstanceNames = [''NFData, ''NFData1, ''NFData2],
      unaryOpExtraVars = const $ return [],
      unaryOpInstanceTypeFromConfig = defaultUnaryOpInstanceTypeFromConfig,
      unaryOpAllowExistential = True,
      unaryOpContextNames = Nothing
    }

-- | Derive 'NFData' instance for a data type.
deriveNFData :: DeriveConfig -> Name -> Q [Dec]
deriveNFData deriveConfig = genUnaryOpClass deriveConfig nfdataConfig 0

-- | Derive 'NFData1' instance for a data type.
deriveNFData1 :: DeriveConfig -> Name -> Q [Dec]
deriveNFData1 deriveConfig = genUnaryOpClass deriveConfig nfdataConfig 1

-- | Derive 'NFData2' instance for a data type.
deriveNFData2 :: DeriveConfig -> Name -> Q [Dec]
deriveNFData2 deriveConfig = genUnaryOpClass deriveConfig nfdataConfig 2
