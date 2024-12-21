{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :   Grisette.Internal.TH.GADT.DeriveNFData
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.GADT.DeriveNFData
  ( deriveGADTNFData,
    deriveGADTNFData1,
    deriveGADTNFData2,
  )
where

import Control.DeepSeq (NFData (rnf), NFData1 (liftRnf), NFData2 (liftRnf2))
import Grisette.Internal.TH.GADT.Common (ExtraConstraint)
import Grisette.Internal.TH.GADT.UnaryOpCommon
  ( UnaryOpClassConfig
      ( UnaryOpClassConfig,
        unaryOpFieldConfigs,
        unaryOpInstanceNames
      ),
    UnaryOpFieldConfig
      ( UnaryOpFieldConfig,
        extraLiftedPatNames,
        extraPatNames,
        fieldCombineFun,
        fieldFunExp,
        fieldFunNames,
        fieldResFun
      ),
    defaultFieldFunExp,
    defaultFieldResFun,
    genUnaryOpClass,
  )
import Language.Haskell.TH (Dec, Name)
import Language.Haskell.TH.Syntax (Q)

nfdataConfig :: UnaryOpClassConfig
nfdataConfig =
  UnaryOpClassConfig
    { unaryOpFieldConfigs =
        [ UnaryOpFieldConfig
            { extraPatNames = [],
              extraLiftedPatNames = const [],
              fieldCombineFun = \_ _ _ _ exps -> do
                r <-
                  foldl
                    (\acc exp -> [|$acc `seq` $(return exp)|])
                    ([|()|])
                    exps
                return (r, []),
              fieldResFun = defaultFieldResFun,
              fieldFunExp = defaultFieldFunExp ['rnf, 'liftRnf, 'liftRnf2],
              fieldFunNames = ['rnf, 'liftRnf, 'liftRnf2]
            }
        ],
      unaryOpInstanceNames = [''NFData, ''NFData1, ''NFData2]
    }

-- | Derive 'NFData' instance for a GADT.
deriveGADTNFData :: ExtraConstraint -> Name -> Q [Dec]
deriveGADTNFData extra = genUnaryOpClass extra nfdataConfig 0

-- | Derive 'NFData1' instance for a GADT.
deriveGADTNFData1 :: ExtraConstraint -> Name -> Q [Dec]
deriveGADTNFData1 extra = genUnaryOpClass extra nfdataConfig 1

-- | Derive 'NFData2' instance for a GADT.
deriveGADTNFData2 :: ExtraConstraint -> Name -> Q [Dec]
deriveGADTNFData2 extra = genUnaryOpClass extra nfdataConfig 2
