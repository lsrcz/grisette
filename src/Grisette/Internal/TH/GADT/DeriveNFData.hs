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
        unaryOpFieldConfig,
        unaryOpFunNames,
        unaryOpInstanceNames
      ),
    UnaryOpFieldConfig
      ( UnaryOpFieldConfig,
        extraPatNames,
        fieldCombineFun,
        fieldResFun
      ),
    defaultFieldResFun,
    genUnaryOpClass,
  )
import Language.Haskell.TH (Dec, Name)
import Language.Haskell.TH.Syntax (Q)

genNFData' :: Int -> Name -> Q [Dec]
genNFData' n typName = do
  genUnaryOpClass
    UnaryOpClassConfig
      { unaryOpFieldConfig =
          UnaryOpFieldConfig
            { extraPatNames = [],
              fieldCombineFun = \_ _ exps ->
                foldl
                  (\acc exp -> [|$acc `seq` $(return exp)|])
                  ([|()|])
                  exps,
              fieldResFun = defaultFieldResFun
            },
        unaryOpInstanceNames =
          [''NFData, ''NFData1, ''NFData2],
        unaryOpFunNames =
          ['rnf, 'liftRnf, 'liftRnf2]
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
