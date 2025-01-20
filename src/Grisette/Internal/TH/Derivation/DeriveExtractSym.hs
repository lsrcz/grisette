{-# LANGUAGE TemplateHaskell #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- |
-- Module      :   Grisette.Internal.TH.Derivation.DeriveExtractSym
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.Derivation.DeriveExtractSym
  ( deriveExtractSym,
    deriveExtractSym1,
    deriveExtractSym2,
  )
where

import Grisette.Internal.Internal.Decl.Core.Data.Class.ExtractSym
  ( ExtractSym (extractSymMaybe),
    ExtractSym1 (liftExtractSymMaybe),
    ExtractSym2 (liftExtractSymMaybe2),
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
import Language.Haskell.TH
  ( Dec,
    Exp (AppE, ListE, VarE),
    Name,
    Q,
  )

extractSymConfig :: UnaryOpClassConfig
extractSymConfig =
  UnaryOpClassConfig
    { unaryOpConfigs =
        [ UnaryOpConfig
            UnaryOpFieldConfig
              { extraPatNames = [],
                extraLiftedPatNames = const [],
                fieldResFun = defaultFieldResFun,
                fieldCombineFun = \_ _ _ _ _ exp ->
                  if null exp
                    then (,[]) <$> [|return mempty|]
                    else return (AppE (VarE 'mconcat) $ ListE exp, False <$ exp),
                fieldFunExp =
                  defaultFieldFunExp
                    [ 'extractSymMaybe,
                      'liftExtractSymMaybe,
                      'liftExtractSymMaybe2
                    ]
              }
            [ 'extractSymMaybe,
              'liftExtractSymMaybe,
              'liftExtractSymMaybe2
            ]
        ],
      unaryOpInstanceNames =
        [''ExtractSym, ''ExtractSym1, ''ExtractSym2],
      unaryOpExtraVars = const $ return [],
      unaryOpInstanceTypeFromConfig = defaultUnaryOpInstanceTypeFromConfig,
      unaryOpAllowExistential = True,
      unaryOpContextNames = Nothing
    }

-- | Derive 'ExtractSym' instance for a data type.
deriveExtractSym :: DeriveConfig -> Name -> Q [Dec]
deriveExtractSym deriveConfig = genUnaryOpClass deriveConfig extractSymConfig 0

-- | Derive 'ExtractSym1' instance for a data type.
deriveExtractSym1 :: DeriveConfig -> Name -> Q [Dec]
deriveExtractSym1 deriveConfig = genUnaryOpClass deriveConfig extractSymConfig 1

-- | Derive 'ExtractSym2' instance for a data type.
deriveExtractSym2 :: DeriveConfig -> Name -> Q [Dec]
deriveExtractSym2 deriveConfig = genUnaryOpClass deriveConfig extractSymConfig 2
