{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module Grisette.Internal.TH.GADT.DeriveUnifiedSimpleMergeable
  ( deriveGADTUnifiedSimpleMergeable,
    deriveGADTUnifiedSimpleMergeable1,
    deriveGADTUnifiedSimpleMergeable2,
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
import Grisette.Internal.Unified.Class.Internal.UnifiedSimpleMergeable
  ( UnifiedSimpleMergeable (withBaseSimpleMergeable),
    UnifiedSimpleMergeable1 (withBaseSimpleMergeable1),
    UnifiedSimpleMergeable2 (withBaseSimpleMergeable2),
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

unifiedSimpleMergeableConfig :: UnaryOpClassConfig
unifiedSimpleMergeableConfig =
  UnaryOpClassConfig
    { unaryOpConfigs =
        [ UnaryOpUnified
            UnaryOpUnifiedConfig
              { unifiedFun =
                  defaultUnaryOpUnifiedFun
                    [ 'withBaseSimpleMergeable,
                      'withBaseSimpleMergeable1,
                      'withBaseSimpleMergeable2
                    ]
              }
            [ 'withBaseSimpleMergeable,
              'withBaseSimpleMergeable1,
              'withBaseSimpleMergeable2
            ]
        ],
      unaryOpInstanceNames =
        [ ''UnifiedSimpleMergeable,
          ''UnifiedSimpleMergeable1,
          ''UnifiedSimpleMergeable2
        ],
      unaryOpExtraVars = \config -> do
        let modeConfigs = evalModeConfig config
        case modeConfigs of
          [] -> do
            nm <- newName "mode"
            return [(VarT nm, ConT ''EvalModeTag)]
          [_] -> return []
          _ -> fail "UnifiedSimpleMergeable does not support multiple evaluation modes",
      unaryOpInstanceTypeFromConfig =
        \config newModeVars keptNewVars con -> do
          let modeConfigs = evalModeConfig config
          modeVar <- case modeConfigs of
            [] -> return $ head newModeVars
            [(i, _)] -> do
              if i >= length keptNewVars
                then
                  fail "UnifiedSimpleMergeable reference to a non-existent mode variable"
                else return $ keptNewVars !! i
            _ -> fail "UnifiedSimpleMergeable does not support multiple evaluation modes"
          appT (conT con) (return $ fst modeVar),
      unaryOpAllowExistential = True
    }

-- | Derive 'UnifiedSimpleMergeable' instance for a GADT.
deriveGADTUnifiedSimpleMergeable :: DeriveConfig -> Name -> Q [Dec]
deriveGADTUnifiedSimpleMergeable deriveConfig =
  genUnaryOpClass deriveConfig unifiedSimpleMergeableConfig 0

-- | Derive 'UnifiedSimpleMergeable1' instance for a GADT.
deriveGADTUnifiedSimpleMergeable1 :: DeriveConfig -> Name -> Q [Dec]
deriveGADTUnifiedSimpleMergeable1 deriveConfig =
  genUnaryOpClass deriveConfig unifiedSimpleMergeableConfig 1

-- | Derive 'UnifiedSimpleMergeable2' instance for a GADT.
deriveGADTUnifiedSimpleMergeable2 :: DeriveConfig -> Name -> Q [Dec]
deriveGADTUnifiedSimpleMergeable2 deriveConfig =
  genUnaryOpClass deriveConfig unifiedSimpleMergeableConfig 2
