{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module Grisette.Internal.TH.GADT.DeriveUnifiedSymEq
  ( deriveGADTUnifiedSymEq,
    deriveGADTUnifiedSymEq1,
    deriveGADTUnifiedSymEq2,
  )
where

import Grisette.Internal.TH.GADT.Common (DeriveConfig (evalModeConfig))
import Grisette.Internal.TH.GADT.UnaryOpCommon
  ( UnaryOpClassConfig
      ( UnaryOpClassConfig,
        unaryOpAllowExistential,
        unaryOpConfigs,
        unaryOpExtraVars,
        unaryOpInstanceNames,
        unaryOpInstanceTypeFromConfig
      ),
    UnaryOpConfig (UnaryOpUnified),
    UnaryOpUnifiedConfig (UnaryOpUnifiedConfig, unifiedFun),
    defaultUnaryOpUnifiedFun,
    genUnaryOpClass,
  )
import Grisette.Internal.Unified.Class.Internal.UnifiedSymEq
  ( UnifiedSymEq (withBaseSymEq),
    UnifiedSymEq1 (withBaseSymEq1),
    UnifiedSymEq2 (withBaseSymEq2),
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
        [ UnaryOpUnified
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
                then
                  fail "UnifiedSymEq reference to a non-existent mode variable"
                else return $ keptNewVars !! i
            _ -> fail "UnifiedSymEq does not support multiple evaluation modes"
          appT (conT con) (return $ fst modeVar),
      unaryOpAllowExistential = True
    }

-- | Derive 'UnifiedSymEq' instance for a GADT.
deriveGADTUnifiedSymEq :: DeriveConfig -> Name -> Q [Dec]
deriveGADTUnifiedSymEq deriveConfig =
  genUnaryOpClass deriveConfig unifiedSymEqConfig 0

-- | Derive 'UnifiedSymEq1' instance for a GADT.
deriveGADTUnifiedSymEq1 :: DeriveConfig -> Name -> Q [Dec]
deriveGADTUnifiedSymEq1 deriveConfig =
  genUnaryOpClass deriveConfig unifiedSymEqConfig 1

-- | Derive 'UnifiedSymEq2' instance for a GADT.
deriveGADTUnifiedSymEq2 :: DeriveConfig -> Name -> Q [Dec]
deriveGADTUnifiedSymEq2 deriveConfig =
  genUnaryOpClass deriveConfig unifiedSymEqConfig 2
