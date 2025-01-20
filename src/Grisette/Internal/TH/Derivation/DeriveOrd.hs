{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      :   Grisette.Internal.TH.Derivation.DeriveOrd
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.Derivation.DeriveOrd
  ( deriveOrd,
    deriveOrd1,
    deriveOrd2,
  )
where

import Data.Functor.Classes (Ord1 (liftCompare), Ord2 (liftCompare2))
import Grisette.Internal.TH.Derivation.BinaryOpCommon
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
import Grisette.Internal.TH.Derivation.Common (DeriveConfig)
import Language.Haskell.TH (Dec, Exp (ListE), Name, Q)

ordConfig :: BinaryOpClassConfig
ordConfig =
  BinaryOpClassConfig
    { binaryOpFieldConfigs =
        [ BinaryOpFieldConfig
            { extraPatNames = [],
              fieldResFun = \_ (lhs, rhs) f ->
                (,[]) <$> [|$(return f) $(return lhs) $(return rhs)|],
              fieldCombineFun = \_ lst ->
                (,[]) <$> [|mconcat $(return $ ListE lst)|],
              fieldDifferentExistentialFun = return,
              fieldFunExp =
                defaultFieldFunExp ['compare, 'liftCompare, 'liftCompare2],
              fieldFunNames = ['compare, 'liftCompare, 'liftCompare2],
              fieldLMatchResult = [|LT|],
              fieldRMatchResult = [|GT|]
            }
        ],
      binaryOpInstanceNames = [''Ord, ''Ord1, ''Ord2],
      binaryOpAllowSumType = True,
      binaryOpAllowExistential = True
    }

-- | Derive 'Ord' instance for a data type.
deriveOrd :: DeriveConfig -> Name -> Q [Dec]
deriveOrd deriveConfig = genBinaryOpClass deriveConfig ordConfig 0

-- | Derive 'Ord1' instance for a data type.
deriveOrd1 :: DeriveConfig -> Name -> Q [Dec]
deriveOrd1 deriveConfig = genBinaryOpClass deriveConfig ordConfig 1

-- | Derive 'Ord2' instance for a data type.
deriveOrd2 :: DeriveConfig -> Name -> Q [Dec]
deriveOrd2 deriveConfig = genBinaryOpClass deriveConfig ordConfig 2
