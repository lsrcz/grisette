{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module Grisette.Internal.TH.GADT.DeriveUnifiedSymOrd
  ( deriveGADTUnifiedSymOrd,
    deriveGADTUnifiedSymOrd1,
    deriveGADTUnifiedSymOrd2,
  )
where

import Grisette.Internal.Internal.Decl.Unified.Class.UnifiedSymOrd
  ( UnifiedSymOrd (withBaseSymOrd),
    UnifiedSymOrd1 (withBaseSymOrd1),
    UnifiedSymOrd2 (withBaseSymOrd2),
  )
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

unifiedSymOrdConfig :: UnaryOpClassConfig
unifiedSymOrdConfig =
  UnaryOpClassConfig
    { unaryOpConfigs =
        [ UnaryOpUnified
            UnaryOpUnifiedConfig
              { unifiedFun =
                  defaultUnaryOpUnifiedFun
                    ['withBaseSymOrd, 'withBaseSymOrd1, 'withBaseSymOrd2]
              }
            ['withBaseSymOrd, 'withBaseSymOrd1, 'withBaseSymOrd2]
        ],
      unaryOpInstanceNames = [''UnifiedSymOrd, ''UnifiedSymOrd1, ''UnifiedSymOrd2],
      unaryOpExtraVars = \config -> do
        let modeConfigs = evalModeConfig config
        case modeConfigs of
          [] -> do
            nm <- newName "mode"
            return [(VarT nm, ConT ''EvalModeTag)]
          [_] -> return []
          _ -> fail "UnifiedSymOrd does not support multiple evaluation modes",
      unaryOpInstanceTypeFromConfig =
        \config newModeVars keptNewVars con -> do
          let modeConfigs = evalModeConfig config
          modeVar <- case modeConfigs of
            [] -> return $ head newModeVars
            [(i, _)] -> do
              if i >= length keptNewVars
                then
                  fail "UnifiedSymOrd reference to a non-existent mode variable"
                else return $ keptNewVars !! i
            _ -> fail "UnifiedSymOrd does not support multiple evaluation modes"
          appT (conT con) (return $ fst modeVar),
      unaryOpAllowExistential = True
    }

-- | Derive 'UnifiedSymOrd' instance for a GADT.
deriveGADTUnifiedSymOrd :: DeriveConfig -> Name -> Q [Dec]
deriveGADTUnifiedSymOrd deriveConfig =
  genUnaryOpClass deriveConfig unifiedSymOrdConfig 0

-- | Derive 'UnifiedSymOrd1' instance for a GADT.
deriveGADTUnifiedSymOrd1 :: DeriveConfig -> Name -> Q [Dec]
deriveGADTUnifiedSymOrd1 deriveConfig =
  genUnaryOpClass deriveConfig unifiedSymOrdConfig 1

-- | Derive 'UnifiedSymOrd2' instance for a GADT.
deriveGADTUnifiedSymOrd2 :: DeriveConfig -> Name -> Q [Dec]
deriveGADTUnifiedSymOrd2 deriveConfig =
  genUnaryOpClass deriveConfig unifiedSymOrdConfig 2
