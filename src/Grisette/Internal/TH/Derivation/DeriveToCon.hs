{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :   Grisette.Internal.TH.Derivation.DeriveToCon
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.Derivation.DeriveToCon
  ( deriveToCon,
    deriveToCon1,
    deriveToCon2,
  )
where

import Grisette.Internal.Internal.Decl.Core.Data.Class.ToCon
  ( ToCon (toCon),
    ToCon1 (liftToCon),
    ToCon2 (liftToCon2),
  )
import Grisette.Internal.TH.Derivation.Common (DeriveConfig)
import Grisette.Internal.TH.Derivation.ConvertOpCommon
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

-- | Derive 'ToCon' instance for a data type.
deriveToCon :: DeriveConfig -> Name -> Q [Dec]
deriveToCon deriveConfig = genConvertOpClass deriveConfig toConClassConfig 0

-- | Derive 'ToCon1' instance for a data type.
deriveToCon1 :: DeriveConfig -> Name -> Q [Dec]
deriveToCon1 deriveConfig =
  genConvertOpClass deriveConfig toConClassConfig 1

-- | Derive 'ToCon2' instance for a data type.
deriveToCon2 :: DeriveConfig -> Name -> Q [Dec]
deriveToCon2 deriveConfig =
  genConvertOpClass deriveConfig toConClassConfig 2
