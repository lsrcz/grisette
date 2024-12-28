{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module Grisette.Internal.TH.GADT.DeriveToSym
  ( deriveGADTToSym,
    deriveGADTToSym1,
    deriveGADTToSym2,
  )
where

import Grisette.Internal.Internal.Decl.Core.Data.Class.ToSym
  ( ToSym (toSym),
    ToSym1 (liftToSym),
    ToSym2 (liftToSym2),
  )
import Grisette.Internal.TH.GADT.Common (DeriveConfig)
import Grisette.Internal.TH.GADT.ConvertOpCommon
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

-- | Derive 'ToSym' instance for a GADT.
deriveGADTToSym :: DeriveConfig -> Name -> Q [Dec]
deriveGADTToSym deriveConfig = genConvertOpClass deriveConfig toSymClassConfig 0

-- | Derive 'ToSym1' instance for a GADT.
deriveGADTToSym1 :: DeriveConfig -> Name -> Q [Dec]
deriveGADTToSym1 deriveConfig =
  genConvertOpClass deriveConfig toSymClassConfig 1

-- | Derive 'ToSym2' instance for a GADT.
deriveGADTToSym2 :: DeriveConfig -> Name -> Q [Dec]
deriveGADTToSym2 deriveConfig =
  genConvertOpClass deriveConfig toSymClassConfig 2
