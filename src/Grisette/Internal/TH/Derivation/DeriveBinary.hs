{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

-- |
-- Module      :   Grisette.Internal.TH.Derivation.DeriveBinary
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.Derivation.DeriveBinary (deriveBinary) where

import Data.Binary (Binary (get, put))
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

binaryConfig :: UnaryOpClassConfig
binaryConfig = serializeConfig [''Binary] ['put] ['get]

binaryWithSerialConfig :: UnaryOpClassConfig
binaryWithSerialConfig =
  serializeWithSerialConfig [''Binary] ['put] ['get]

-- | Derive 'Binary' instance for a data type.
deriveBinary :: DeriveConfig -> Name -> Q [Dec]
deriveBinary deriveConfig =
  genUnaryOpClass
    deriveConfig
    ( if useSerialForCerealAndBinary deriveConfig
        then binaryWithSerialConfig
        else binaryConfig
    )
    0
