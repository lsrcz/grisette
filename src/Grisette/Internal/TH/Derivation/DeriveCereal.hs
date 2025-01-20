{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

-- |
-- Module      :   Grisette.Internal.TH.Derivation.DeriveCereal
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.Derivation.DeriveCereal (deriveCereal) where

import Data.Serialize (Serialize (get, put))
import Grisette.Internal.TH.Derivation.Common
  ( DeriveConfig (useSerialForCerealAndBinary),
  )
import Grisette.Internal.TH.Derivation.SerializeCommon
  ( serializeConfig,
    serializeWithSerialConfig,
  )
import Grisette.Internal.TH.Derivation.UnaryOpCommon
  ( UnaryOpClassConfig,
    genUnaryOpClass,
  )
import Language.Haskell.TH (Dec, Name, Q)

cerealConfig :: UnaryOpClassConfig
cerealConfig = serializeConfig [''Serialize] ['put] ['get]

cerealWithSerialConfig :: UnaryOpClassConfig
cerealWithSerialConfig =
  serializeWithSerialConfig [''Serialize] ['put] ['get]

-- | Derive 'Serialize' instance for a data type.
deriveCereal :: DeriveConfig -> Name -> Q [Dec]
deriveCereal deriveConfig =
  genUnaryOpClass
    deriveConfig
    ( if useSerialForCerealAndBinary deriveConfig
        then cerealWithSerialConfig
        else cerealConfig
    )
    0
