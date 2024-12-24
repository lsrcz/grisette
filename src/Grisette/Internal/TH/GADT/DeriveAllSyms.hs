{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

-- |
-- Module      :   Grisette.Internal.TH.GADT.DeriveAllSyms
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.GADT.DeriveAllSyms
  ( deriveGADTAllSyms,
    deriveGADTAllSyms1,
    deriveGADTAllSyms2,
  )
where

import Grisette.Internal.SymPrim.AllSyms
  ( AllSyms (allSymsS),
    AllSyms1 (liftAllSymsS),
    AllSyms2 (liftAllSymsS2),
  )
import Grisette.Internal.TH.GADT.Common (DeriveConfig)
import Grisette.Internal.TH.GADT.UnaryOpCommon
  ( UnaryOpClassConfig
      ( UnaryOpClassConfig,
        unaryOpAllowExistential,
        unaryOpConfigs,
        unaryOpExtraVars,
        unaryOpInstanceNames,
        unaryOpInstanceTypeFromConfig
      ),
    UnaryOpConfig (UnaryOpField),
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
import Language.Haskell.TH (Dec, Exp (AppE, ListE, VarE), Name, Q)

allSymsConfig :: UnaryOpClassConfig
allSymsConfig =
  UnaryOpClassConfig
    { unaryOpConfigs =
        [ UnaryOpField
            UnaryOpFieldConfig
              { extraPatNames = [],
                extraLiftedPatNames = const [],
                fieldResFun = defaultFieldResFun,
                fieldCombineFun = \_ _ _ _ exp ->
                  return (AppE (VarE 'mconcat) $ ListE exp, False <$ exp),
                fieldFunExp =
                  defaultFieldFunExp
                    [ 'allSymsS,
                      'liftAllSymsS,
                      'liftAllSymsS2
                    ]
              }
            ['allSymsS, 'liftAllSymsS, 'liftAllSymsS2]
        ],
      unaryOpInstanceNames = [''AllSyms, ''AllSyms1, ''AllSyms2],
      unaryOpExtraVars = const $ return [],
      unaryOpInstanceTypeFromConfig = defaultUnaryOpInstanceTypeFromConfig,
      unaryOpAllowExistential = True
    }

-- | Derive 'AllSyms' instance for a GADT.
deriveGADTAllSyms :: DeriveConfig -> Name -> Q [Dec]
deriveGADTAllSyms deriveConfig = genUnaryOpClass deriveConfig allSymsConfig 0

-- | Derive 'AllSyms1' instance for a GADT.
deriveGADTAllSyms1 :: DeriveConfig -> Name -> Q [Dec]
deriveGADTAllSyms1 deriveConfig = genUnaryOpClass deriveConfig allSymsConfig 1

-- | Derive 'AllSyms2' instance for a GADT.
deriveGADTAllSyms2 :: DeriveConfig -> Name -> Q [Dec]
deriveGADTAllSyms2 deriveConfig = genUnaryOpClass deriveConfig allSymsConfig 2
