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
import Grisette.Internal.TH.GADT.UnaryOpCommon
  ( UnaryOpClassConfig
      ( UnaryOpClassConfig,
        unaryOpFieldConfigs,
        unaryOpInstanceNames, unaryOpAllowExistential
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

genNFData' :: Int -> Name -> Q [Dec]
genNFData' n typName = do
  genUnaryOpClass
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
        unaryOpInstanceNames =
          [''NFData, ''NFData1, ''NFData2],
        unaryOpAllowExistential = True
      }
    n
    typName

-- | Derive 'NFData' instance for a GADT.
deriveGADTNFData :: Name -> Q [Dec]
deriveGADTNFData = genNFData' 0

-- | Derive 'NFData1' instance for a GADT.
deriveGADTNFData1 :: Name -> Q [Dec]
deriveGADTNFData1 = genNFData' 1

-- | Derive 'NFData2' instance for a GADT.
deriveGADTNFData2 :: Name -> Q [Dec]
deriveGADTNFData2 = genNFData' 2
