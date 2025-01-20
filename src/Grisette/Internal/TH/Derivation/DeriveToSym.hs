{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

-- |
-- Module      :   Grisette.Internal.TH.Derivation.DeriveToSym
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.Derivation.DeriveToSym
  ( deriveToSym,
    deriveToSym1,
    deriveToSym2,
  )
where

import Grisette.Internal.Internal.Decl.Core.Data.Class.ToSym
  ( ToSym (toSym),
    ToSym1 (liftToSym),
    ToSym2 (liftToSym2),
  )
import Grisette.Internal.TH.Derivation.Common (DeriveConfig)
import Grisette.Internal.TH.Derivation.ConvertOpCommon
  ( ConvertOpClassConfig
      ( ConvertOpClassConfig,
        convertFieldCombineFun,
        convertFieldFunExp,
        convertFieldResFun,
        convertOpInstanceNames,
        convertOpTarget
      ),
    convertOpFunNames,
    defaultFieldFunExp,
    genConvertOpClass,
  )
import Grisette.Internal.Unified.EvalModeTag (EvalModeTag (S))
import Language.Haskell.TH (Dec, Name, Q, appE, conE)

toSymClassConfig :: ConvertOpClassConfig
toSymClassConfig =
  ConvertOpClassConfig
    { convertFieldResFun = \v f -> [|$(return f) $(return v)|],
      convertFieldCombineFun =
        \f args -> foldl appE (conE f) $ fmap return args,
      convertFieldFunExp = defaultFieldFunExp ['toSym, 'liftToSym, 'liftToSym2],
      convertOpTarget = S,
      convertOpInstanceNames = [''ToSym, ''ToSym1, ''ToSym2],
      convertOpFunNames = ['toSym, 'liftToSym, 'liftToSym2]
    }

-- | Derive 'ToSym' instance for a data type.
deriveToSym :: DeriveConfig -> Name -> Q [Dec]
deriveToSym deriveConfig = genConvertOpClass deriveConfig toSymClassConfig 0

-- | Derive 'ToSym1' instance for a data type.
deriveToSym1 :: DeriveConfig -> Name -> Q [Dec]
deriveToSym1 deriveConfig =
  genConvertOpClass deriveConfig toSymClassConfig 1

-- | Derive 'ToSym2' instance for a data type.
deriveToSym2 :: DeriveConfig -> Name -> Q [Dec]
deriveToSym2 deriveConfig =
  genConvertOpClass deriveConfig toSymClassConfig 2
