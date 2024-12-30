{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      :   Grisette.Internal.TH.GADT.DeriveSerial
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
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
import Grisette.Internal.TH.GADT.SerializeCommon (serializeConfig)
import Grisette.Internal.TH.GADT.UnaryOpCommon
  ( UnaryOpClassConfig,
    genUnaryOpClass,
  )
import Language.Haskell.TH (Dec, Name, Q)

serialConfig :: UnaryOpClassConfig
serialConfig =
  serializeConfig
    [''Serial, ''Serial1, ''Serial2]
    ['serialize, 'serializeWith, 'serializeWith2]
    ['deserialize, 'deserializeWith, 'deserializeWith2]

-- | Derive 'Serial' instance for a GADT.
deriveGADTSerial :: DeriveConfig -> Name -> Q [Dec]
deriveGADTSerial deriveConfig = genUnaryOpClass deriveConfig serialConfig 0

-- | Derive 'Serial1' instance for a GADT.
deriveGADTSerial1 :: DeriveConfig -> Name -> Q [Dec]
deriveGADTSerial1 deriveConfig = genUnaryOpClass deriveConfig serialConfig 1

-- | Derive 'Serial2' instance for a GADT.
deriveGADTSerial2 :: DeriveConfig -> Name -> Q [Dec]
deriveGADTSerial2 deriveConfig = genUnaryOpClass deriveConfig serialConfig 2
