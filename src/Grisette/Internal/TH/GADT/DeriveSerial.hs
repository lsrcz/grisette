{-# LANGUAGE TemplateHaskell #-}

module Grisette.Internal.TH.GADT.DeriveSerial
  ( deriveGADTSerial,
    deriveGADTSerial1,
    deriveGADTSerial2,
  )
where

import Data.Bytes.Serial
  ( Serial (deserialize, serialize),
    Serial1 (deserializeWith, serializeWith),
    Serial2 (deserializeWith2, serializeWith2),
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
    UnaryOpConfig (UnaryOpDeserialize, UnaryOpField),
    UnaryOpDeserializeConfig (UnaryOpDeserializeConfig, fieldDeserializeFun),
    UnaryOpFieldConfig
      ( UnaryOpFieldConfig,
        extraLiftedPatNames,
        extraPatNames,
        fieldCombineFun,
        fieldFunExp,
        fieldResFun
      ),
    defaultFieldFunExp,
    defaultUnaryOpInstanceTypeFromConfig,
    genUnaryOpClass,
  )
import Grisette.Internal.TH.Util (integerE)
import Language.Haskell.TH (Dec, Name, Q)

serialConfig :: UnaryOpClassConfig
serialConfig =
  UnaryOpClassConfig
    { unaryOpConfigs =
        [ UnaryOpField
            UnaryOpFieldConfig
              { extraPatNames = [],
                extraLiftedPatNames = const [],
                fieldCombineFun = \conIdx _ _ [] exp -> do
                  r <-
                    foldl
                      (\r exp -> [|$r >> $(return exp)|])
                      ([|serialize ($(integerE conIdx) :: Int)|])
                      exp
                  return (r, [True]),
                fieldResFun = \_ _ _ _ fieldPat fieldFun -> do
                  r <- [|$(return fieldFun) $(return fieldPat)|]
                  return (r, [True]),
                fieldFunExp =
                  defaultFieldFunExp
                    ['serialize, 'serializeWith, 'serializeWith2]
              }
            ['serialize, 'serializeWith, 'serializeWith2],
          UnaryOpDeserialize
            UnaryOpDeserializeConfig
              { fieldDeserializeFun =
                  defaultFieldFunExp
                    ['deserialize, 'deserializeWith, 'deserializeWith2]
              }
            ['deserialize, 'deserializeWith, 'deserializeWith2]
        ],
      unaryOpInstanceNames = [''Serial, ''Serial1, ''Serial2],
      unaryOpExtraVars = const $ return [],
      unaryOpInstanceTypeFromConfig = defaultUnaryOpInstanceTypeFromConfig,
      unaryOpAllowExistential = False
    }

-- | Derive 'Serial' instance for a GADT.
deriveGADTSerial :: DeriveConfig -> Name -> Q [Dec]
deriveGADTSerial deriveConfig = genUnaryOpClass deriveConfig serialConfig 0

-- | Derive 'Serial1' instance for a GADT.
deriveGADTSerial1 :: DeriveConfig -> Name -> Q [Dec]
deriveGADTSerial1 deriveConfig = genUnaryOpClass deriveConfig serialConfig 1

-- | Derive 'Serial2' instance for a GADT.
deriveGADTSerial2 :: DeriveConfig -> Name -> Q [Dec]
deriveGADTSerial2 deriveConfig = genUnaryOpClass deriveConfig serialConfig 2
