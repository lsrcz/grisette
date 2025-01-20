{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

-- |
-- Module      :   Grisette.Internal.TH.Derivation.DeriveAllSyms
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.Derivation.DeriveAllSyms
  ( deriveAllSyms,
    deriveAllSyms1,
    deriveAllSyms2,
  )
where

import Grisette.Internal.Internal.Decl.SymPrim.AllSyms
  ( AllSyms (allSymsS),
    AllSyms1 (liftAllSymsS),
    AllSyms2 (liftAllSymsS2),
  )
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
import Language.Haskell.TH (Dec, Exp (AppE, ListE, VarE), Name, Q)

allSymsConfig :: UnaryOpClassConfig
allSymsConfig =
  UnaryOpClassConfig
    { unaryOpConfigs =
        [ UnaryOpConfig
            UnaryOpFieldConfig
              { extraPatNames = [],
                extraLiftedPatNames = const [],
                fieldResFun = defaultFieldResFun,
                fieldCombineFun = \_ _ _ _ _ exp ->
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
      unaryOpAllowExistential = True,
      unaryOpContextNames = Nothing
    }

-- | Derive 'AllSyms' instance for a data type.
deriveAllSyms :: DeriveConfig -> Name -> Q [Dec]
deriveAllSyms deriveConfig = genUnaryOpClass deriveConfig allSymsConfig 0

-- | Derive 'AllSyms1' instance for a data type.
deriveAllSyms1 :: DeriveConfig -> Name -> Q [Dec]
deriveAllSyms1 deriveConfig = genUnaryOpClass deriveConfig allSymsConfig 1

-- | Derive 'AllSyms2' instance for a data type.
deriveAllSyms2 :: DeriveConfig -> Name -> Q [Dec]
deriveAllSyms2 deriveConfig = genUnaryOpClass deriveConfig allSymsConfig 2
