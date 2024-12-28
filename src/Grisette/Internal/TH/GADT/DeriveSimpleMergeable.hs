{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Grisette.Internal.TH.GADT.DeriveSimpleMergeable
  ( deriveGADTSimpleMergeable,
    deriveGADTSimpleMergeable1,
    deriveGADTSimpleMergeable2,
  )
where

import Grisette.Internal.Internal.Decl.Core.Data.Class.SimpleMergeable
  ( SimpleMergeable (mrgIte),
    SimpleMergeable1 (liftMrgIte),
    SimpleMergeable2 (liftMrgIte2),
  )
import Grisette.Internal.TH.GADT.BinaryOpCommon
  ( BinaryOpClassConfig
      ( BinaryOpClassConfig,
        binaryOpAllowSumType,
        binaryOpFieldConfigs,
        binaryOpInstanceNames
      ),
    BinaryOpFieldConfig
      ( BinaryOpFieldConfig,
        extraPatNames,
        fieldCombineFun,
        fieldDifferentExistentialFun,
        fieldFunExp,
        fieldFunNames,
        fieldLMatchResult,
        fieldRMatchResult,
        fieldResFun
      ),
    binaryOpAllowExistential,
    defaultFieldFunExp,
    genBinaryOpClass,
  )
import Grisette.Internal.TH.GADT.Common (DeriveConfig)
import Language.Haskell.TH (Dec, Exp (AppE, ConE), Name, Q)

simpleMergeableConfig :: BinaryOpClassConfig
simpleMergeableConfig =
  BinaryOpClassConfig
    { binaryOpFieldConfigs =
        [ BinaryOpFieldConfig
            { extraPatNames = ["c"],
              fieldResFun = \[c] (lhs, rhs) f ->
                (,[True])
                  <$> [|$(return f) $(return c) $(return lhs) $(return rhs)|],
              fieldCombineFun =
                \con lst -> return (foldl AppE (ConE con) lst, [False]),
              fieldDifferentExistentialFun = const [|undefined|],
              fieldFunExp =
                defaultFieldFunExp ['mrgIte, 'liftMrgIte, 'liftMrgIte2],
              fieldFunNames = ['mrgIte, 'liftMrgIte, 'liftMrgIte2],
              fieldLMatchResult = [|undefined|],
              fieldRMatchResult = [|undefined|]
            }
        ],
      binaryOpInstanceNames =
        [''SimpleMergeable, ''SimpleMergeable1, ''SimpleMergeable2],
      binaryOpAllowSumType = False,
      binaryOpAllowExistential = True
    }

-- | Derive 'SimpleMergeable' instance for a GADT.
deriveGADTSimpleMergeable :: DeriveConfig -> Name -> Q [Dec]
deriveGADTSimpleMergeable deriveConfig =
  genBinaryOpClass deriveConfig simpleMergeableConfig 0

-- | Derive 'SimpleMergeable1' instance for a GADT.
deriveGADTSimpleMergeable1 :: DeriveConfig -> Name -> Q [Dec]
deriveGADTSimpleMergeable1 deriveConfig =
  genBinaryOpClass deriveConfig simpleMergeableConfig 1

-- | Derive 'SimpleMergeable2' instance for a GADT.
deriveGADTSimpleMergeable2 :: DeriveConfig -> Name -> Q [Dec]
deriveGADTSimpleMergeable2 deriveConfig =
  genBinaryOpClass deriveConfig simpleMergeableConfig 2
