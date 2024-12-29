{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :   Grisette.Internal.TH.GADT.DeriveToCon
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.GADT.DeriveToCon
  ( deriveGADTToCon,
    deriveGADTToCon1,
    deriveGADTToCon2,
  )
where

import Grisette.Internal.Internal.Decl.Core.Data.Class.ToCon
  ( ToCon (toCon),
    ToCon1 (liftToCon),
    ToCon2 (liftToCon2),
  )
import Grisette.Internal.TH.GADT.Common (DeriveConfig)
import Grisette.Internal.TH.GADT.ConvertOpCommon
  ( ConvertOpClassConfig
      ( ConvertOpClassConfig,
        convertFieldCombineFun,
        convertFieldFunExp,
        convertFieldResFun,
        convertOpFunNames,
        convertOpInstanceNames,
        convertOpTarget
      ),
    defaultFieldFunExp,
    genConvertOpClass,
  )
import Grisette.Internal.Unified.EvalModeTag (EvalModeTag (C))
import Language.Haskell.TH (Dec, Name, Q, conE)

toConClassConfig :: ConvertOpClassConfig
toConClassConfig =
  ConvertOpClassConfig
    { convertFieldResFun = \v f -> [|$(return f) $(return v)|],
      convertFieldCombineFun = \f args ->
        foldl
          (\acc arg -> [|$(acc) <*> $arg|])
          [|return $(conE f)|]
          $ fmap return args,
      convertFieldFunExp = defaultFieldFunExp ['toCon, 'liftToCon, 'liftToCon2],
      convertOpTarget = C,
      convertOpInstanceNames = [''ToCon, ''ToCon1, ''ToCon2],
      convertOpFunNames = ['toCon, 'liftToCon, 'liftToCon2]
    }

-- | Derive 'ToCon' instance for a GADT.
deriveGADTToCon :: DeriveConfig -> Name -> Q [Dec]
deriveGADTToCon deriveConfig = genConvertOpClass deriveConfig toConClassConfig 0

-- | Derive 'ToCon1' instance for a GADT.
deriveGADTToCon1 :: DeriveConfig -> Name -> Q [Dec]
deriveGADTToCon1 deriveConfig =
  genConvertOpClass deriveConfig toConClassConfig 1

-- | Derive 'ToCon2' instance for a GADT.
deriveGADTToCon2 :: DeriveConfig -> Name -> Q [Dec]
deriveGADTToCon2 deriveConfig =
  genConvertOpClass deriveConfig toConClassConfig 2
