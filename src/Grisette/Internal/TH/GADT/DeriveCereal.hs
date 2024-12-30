{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      :   Grisette.Internal.TH.GADT.DeriveCereal
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.GADT.DeriveCereal (deriveGADTCereal) where

import Data.Serialize (Serialize (get, put))
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

cerealConfig :: UnaryOpClassConfig
cerealConfig = serializeConfig [''Serialize] ['put] ['get]

cerealWithSerialConfig :: UnaryOpClassConfig
cerealWithSerialConfig =
  serializeWithSerialConfig [''Serialize] ['put] ['get]

-- | Derive 'Serialize' instance for a GADT.
deriveGADTCereal :: DeriveConfig -> Name -> Q [Dec]
deriveGADTCereal deriveConfig =
  genUnaryOpClass
    deriveConfig
    ( if useSerialForCerealAndBinary deriveConfig
        then cerealWithSerialConfig
        else cerealConfig
    )
    0
