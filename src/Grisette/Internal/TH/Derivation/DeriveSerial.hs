{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

-- |
-- Module      :   Grisette.Internal.TH.Derivation.DeriveSerial
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.Derivation.DeriveSerial
  ( deriveSerial,
    deriveSerial1,
    deriveSerial2,
  )
where

import Data.Bytes.Serial
  ( Serial (deserialize, serialize),
    Serial1 (deserializeWith, serializeWith),
    Serial2 (deserializeWith2, serializeWith2),
  )
import Grisette.Internal.TH.Derivation.Common (DeriveConfig)
import Grisette.Internal.TH.Derivation.SerializeCommon (serializeConfig)
import Grisette.Internal.TH.Derivation.UnaryOpCommon
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

-- | Derive 'Serial' instance for a data type.
deriveSerial :: DeriveConfig -> Name -> Q [Dec]
deriveSerial deriveConfig = genUnaryOpClass deriveConfig serialConfig 0

-- | Derive 'Serial1' instance for a data type.
deriveSerial1 :: DeriveConfig -> Name -> Q [Dec]
deriveSerial1 deriveConfig = genUnaryOpClass deriveConfig serialConfig 1

-- | Derive 'Serial2' instance for a data type.
deriveSerial2 :: DeriveConfig -> Name -> Q [Dec]
deriveSerial2 deriveConfig = genUnaryOpClass deriveConfig serialConfig 2
