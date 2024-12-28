{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      :   Grisette.Internal.TH.GADT.DeriveSymOrd
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.GADT.DeriveSymOrd
  ( deriveGADTSymOrd,
    deriveGADTSymOrd1,
    deriveGADTSymOrd2,
  )
where

import Grisette.Internal.Internal.Decl.Core.Data.Class.SymOrd
  ( SymOrd (symCompare),
    SymOrd1 (liftSymCompare),
    SymOrd2 (liftSymCompare2),
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.TryMerge
  ( mrgSingle,
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
import Language.Haskell.TH (Dec, Name, Q)

symOrdConfig :: BinaryOpClassConfig
symOrdConfig =
  BinaryOpClassConfig
    { binaryOpFieldConfigs =
        [ BinaryOpFieldConfig
            { extraPatNames = [],
              fieldResFun =
                \_ (lhs, rhs) f ->
                  (,[]) <$> [|$(return f) $(return lhs) $(return rhs)|],
              fieldCombineFun =
                \_ lst -> do
                  let go [] = [|mrgSingle EQ|]
                      go [x] = [|$(return x)|]
                      go (x : xs) =
                        [|
                          do
                            a <- $(return x)
                            case a of
                              EQ -> $(go xs)
                              _ -> mrgSingle a
                          |]
                  (,[]) <$> go lst,
              fieldDifferentExistentialFun =
                \exp -> [|mrgSingle $(return exp)|],
              fieldFunExp =
                defaultFieldFunExp
                  ['symCompare, 'liftSymCompare, 'liftSymCompare2],
              fieldFunNames = ['symCompare, 'liftSymCompare, 'liftSymCompare2],
              fieldLMatchResult = [|mrgSingle LT|],
              fieldRMatchResult = [|mrgSingle GT|]
            }
        ],
      binaryOpInstanceNames = [''SymOrd, ''SymOrd1, ''SymOrd2],
      binaryOpAllowSumType = True,
      binaryOpAllowExistential = True
    }

-- | Derive 'SymOrd' instance for a GADT.
deriveGADTSymOrd :: DeriveConfig -> Name -> Q [Dec]
deriveGADTSymOrd deriveConfig = genBinaryOpClass deriveConfig symOrdConfig 0

-- | Derive 'SymOrd1' instance for a GADT.
deriveGADTSymOrd1 :: DeriveConfig -> Name -> Q [Dec]
deriveGADTSymOrd1 deriveConfig = genBinaryOpClass deriveConfig symOrdConfig 1

-- | Derive 'SymOrd2' instance for a GADT.
deriveGADTSymOrd2 :: DeriveConfig -> Name -> Q [Dec]
deriveGADTSymOrd2 deriveConfig = genBinaryOpClass deriveConfig symOrdConfig 2