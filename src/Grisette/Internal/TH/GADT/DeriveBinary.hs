{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      :   Grisette.Internal.TH.GADT.DeriveBinary
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.GADT.DeriveBinary (deriveGADTBinary) where

import Data.Binary (Binary (get, put))
import Grisette.Internal.TH.GADT.Common
  ( DeriveConfig (useSerialForCerealAndBinary),
  )
import Grisette.Internal.TH.GADT.SerializeCommon
  ( serializeConfig,
    serializeWithSerialConfig,
  )
import Grisette.Internal.TH.GADT.UnaryOpCommon
  ( UnaryOpClassConfig,
    genUnaryOpClass,
  )
import Language.Haskell.TH (Dec, Name, Q)

binaryConfig :: UnaryOpClassConfig
binaryConfig = serializeConfig [''Binary] ['put] ['get]

binaryWithSerialConfig :: UnaryOpClassConfig
binaryWithSerialConfig =
  serializeWithSerialConfig [''Binary] ['put] ['get]

-- | Derive 'Binary' instance for a GADT.
deriveGADTBinary :: DeriveConfig -> Name -> Q [Dec]
deriveGADTBinary deriveConfig =
  genUnaryOpClass
    deriveConfig
    ( if useSerialForCerealAndBinary deriveConfig
        then binaryWithSerialConfig
        else binaryConfig
    )
    0
