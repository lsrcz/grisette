{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

-- |
-- Module      :   Grisette.Internal.TH.Derivation.DeriveUnifiedSymEq
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.Derivation.DeriveUnifiedSymEq
  ( deriveUnifiedSymEq,
    deriveUnifiedSymEq1,
    deriveUnifiedSymEq2,
  )
where

import Grisette.Internal.Internal.Decl.Unified.Class.UnifiedSymEq
  ( UnifiedSymEq (withBaseSymEq),
    UnifiedSymEq1 (withBaseSymEq1),
    UnifiedSymEq2 (withBaseSymEq2),
  )
import Grisette.Internal.TH.Derivation.Common (DeriveConfig (evalModeConfig))
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
    genUnaryOpClass,
  )
import Grisette.Internal.TH.Derivation.UnifiedOpCommon
  ( UnaryOpUnifiedConfig (UnaryOpUnifiedConfig, unifiedFun),
    defaultUnaryOpUnifiedFun,
  )
import Grisette.Internal.Unified.EvalModeTag (EvalModeTag)
import Language.Haskell.TH
  ( Dec,
    Name,
    Q,
    Type (ConT, VarT),
    appT,
    conT,
    newName,
  )

unifiedSymEqConfig :: UnaryOpClassConfig
unifiedSymEqConfig =
  UnaryOpClassConfig
    { unaryOpConfigs =
        [ UnaryOpConfig
            UnaryOpUnifiedConfig
              { unifiedFun =
                  defaultUnaryOpUnifiedFun
                    ['withBaseSymEq, 'withBaseSymEq1, 'withBaseSymEq2]
              }
            ['withBaseSymEq, 'withBaseSymEq1, 'withBaseSymEq2]
        ],
      unaryOpInstanceNames = [''UnifiedSymEq, ''UnifiedSymEq1, ''UnifiedSymEq2],
      unaryOpExtraVars = \config -> do
        let modeConfigs = evalModeConfig config
        case modeConfigs of
          [] -> do
            nm <- newName "mode"
            return [(VarT nm, ConT ''EvalModeTag)]
          [_] -> return []
          _ -> fail "UnifiedSymEq does not support multiple evaluation modes",
      unaryOpInstanceTypeFromConfig =
        \config newModeVars keptNewVars con -> do
          let modeConfigs = evalModeConfig config
          modeVar <- case modeConfigs of
            [] -> return $ head newModeVars
            [(i, _)] -> do
              if i >= length keptNewVars
                then fail "UnifiedSymEq reference to a non-existent mode variable"
                else return $ keptNewVars !! i
            _ -> fail "UnifiedSymEq does not support multiple evaluation modes"
          appT (conT con) (return $ fst modeVar),
      unaryOpAllowExistential = True,
      unaryOpContextNames = Nothing
    }

-- | Derive 'UnifiedSymEq' instance for a data type.
deriveUnifiedSymEq :: DeriveConfig -> Name -> Q [Dec]
deriveUnifiedSymEq deriveConfig =
  genUnaryOpClass deriveConfig unifiedSymEqConfig 0

-- | Derive 'UnifiedSymEq1' instance for a data type.
deriveUnifiedSymEq1 :: DeriveConfig -> Name -> Q [Dec]
deriveUnifiedSymEq1 deriveConfig =
  genUnaryOpClass deriveConfig unifiedSymEqConfig 1

-- | Derive 'UnifiedSymEq2' instance for a data type.
deriveUnifiedSymEq2 :: DeriveConfig -> Name -> Q [Dec]
deriveUnifiedSymEq2 deriveConfig =
  genUnaryOpClass deriveConfig unifiedSymEqConfig 2
